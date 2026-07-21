# Python callback API — runtime user-function registration (W4)

Design for Goal 7 (Python package performs **no** runtime compilation). The engine already
dispatches user functions through a C function-pointer table
([include/auto_f2c.h](auto_f2c.h) `user_function_list`), called from Fortran via
`c_f_procpointer` ([src/user_c.f90](../src/user_c.f90)). Today that table is filled at
**link time** by a compiled `.c`/`.f90` equation file. This design fills it **at runtime
from Python**, so no `gcc`/`gfortran`/`make` is needed.

This does not replace the compiled-C path (Goal 9) — it adds a runtime setter alongside it.

---

## 1. The ABI we must match (existing, do not change)

From [auto_f2c.h:44-71](auto_f2c.h#L44) — `integer`=`int`, `doublereal`=`double`.
Scalars `ndim`, `t`, `ijac`, `nbc`, `nint` are passed **by value**; everything else by pointer.

```c
typedef int user_func_t (int ndim, const double *u,  const int *icp, const double *par,
                         int ijac, double *f, double *dfdu, double *dfdp);
typedef int user_stpnt_t(int ndim, double t, double *u, double *par);
typedef int user_bcnd_t (int ndim, const double *par, const int *icp, int nbc,
                         const double *u0, const double *u1, int ijac,
                         double *fb, double *dbc);
typedef int user_icnd_t (int ndim, const double *par, const int *icp, int nint,
                         const double *u, const double *uold, const double *udot,
                         const double *upold, int ijac, double *fi, double *dint);
typedef int user_fopt_t (int ndim, const double *u, const int *icp, const double *par,
                         int ijac, double *fs, double *dfdu, double *dfdp);
typedef int user_pvls_t (int ndim, const double *u, double *par);

typedef struct {
  user_func_t *func;  user_stpnt_t *stpnt; user_bcnd_t *bcnd;
  user_icnd_t *icnd;  user_fopt_t  *fopt;  user_pvls_t *pvls;
  int uses_fortran;
} user_function_list;
```

### Array shapes (what each pointer spans)
| callback | in | out |
|---|---|---|
| `func` | `u[ndim]`, `par[NPARX]`, `icp[·]` | `f[ndim]`; if `ijac>0`: `dfdu[ndim,ndim]`, `dfdp[ndim,ncol]` |
| `stpnt` | `t` | `u[ndim]`, `par[NPARX]` (initial data) |
| `bcnd` | `u0[ndim]`, `u1[ndim]`, `par`, `icp` | `fb[nbc]`; if `ijac>0`: `dbc[nbc,·]` |
| `icnd` | `u,uold,udot,upold[ndim]`, `par`, `icp` | `fi[nint]`; if `ijac>0`: `dint[nint,·]` |
| `fopt` | `u[ndim]`, `par`, `icp` | `fs` (scalar via `fs[0]`); if `ijac>0`: `dfdu[ndim]`, `dfdp[·]` |
| `pvls` | `u[ndim]` | `par` (write solution measures) |

**All 2-D arrays are Fortran column-major.** `dfdu[i,j]` (∂f_i/∂u_j) lives at `dfdu[i + j*ndim]`.
Wrap with `numpy.ndarray(..., order='F')` — never assume C order.

`par` length: **`NPARX = 36`**, a compile-time `PARAMETER` in
[include/auto.h:7](auto.h#L7) (also in `fcon.h`). Expose it as `auto.NPARX` so the Python
layer can size the `par` view. (Verified; note "do not decrease below 20".)

`ijac` semantics — controlled by the **`JAC` constants-file value**, not by any return code.
The engine stores `AP%JAC` ([main.f90:331](../src/main.f90#L331)) and passes it as `ijac`:
- `JAC=0` → the engine finite-differences the Jacobian and calls the user function **only with
  `ijac=0`**; the callback fills residuals only (`IF(IJAC==0) ... RETURN`, [toolboxae.f90:204](../src/toolboxae.f90#L204)).
- `JAC=1` → the user function is called with `ijac>0` and **must** fill the derivative arrays.

So a Python user who doesn't want to supply Jacobians simply sets `JAC=0`; there is no
per-call fallback. The adapter must handle both `ijac` values but never needs to "signal" back.

**Return value is ignored.** Every callback is bound on the Fortran side as a
`subroutine ... bind(c)` (i.e. `void`) and invoked via `call func_f(...)`
([user_c.f90:20-75, 111-112](../src/user_c.f90#L20)); the C header's `int` return type is
never read. A Python callback may return anything (or `None`) — the value is discarded.
Signal errors by other means (e.g. raise on the Python side before/after the run, or set a
sentinel in `par`), not via the return code.

---

## 2. New runtime entry points (add to libauto)

Add to the C header (guarded so compiled-C users are unaffected):

```c
/* Register user callbacks at runtime. Any NULL slot is left unset.
   uses_fortran must be 0 for Python-supplied callbacks. */
void auto_set_user(const user_function_list *ul);

/* Clear all callbacks (return to unregistered state). */
void auto_reset_user(void);

/* Run one continuation. Returns 0 on success. (From the auto_main_c refactor.) */
int  auto_main_c(void);
```

### Owning the `user` symbol (the key link-time change)
Currently `user` is `extern const user_function_list user;` — **defined by the compiled
equation object**, referenced by the Fortran wrapper (which emits it as a tentative/COMMON
symbol, so the C definition wins at link).

For the Python build there is no equation object, so **libauto must own and define `user`**,
mutable, initialised empty. Implement the setter in Fortran alongside the existing wrapper
([src/user_c.f90](../src/user_c.f90)):

```fortran
subroutine auto_set_user(ul) bind(c, name="auto_set_user")
  use iso_c_binding, only: c_int
  type(user_function_list), intent(in) :: ul
  user = ul                 ! copies the c_funptr fields + uses_fortran
end subroutine auto_set_user

subroutine auto_reset_user() bind(c, name="auto_reset_user")
  use iso_c_binding, only: c_null_funptr
  user%func  = c_null_funptr;  user%stpnt = c_null_funptr
  user%bcnd  = c_null_funptr;  user%icnd  = c_null_funptr
  user%fopt  = c_null_funptr;  user%pvls  = c_null_funptr
  user%uses_fortran = 0
end subroutine auto_reset_user
```

**Compatibility:** keep the C header's `extern const user_function_list user;` for compiled-C
users (Goal 9). For the Python shared library, the Fortran module variable is the definition;
do not also compile a `.c` that defines `user`, or you get a duplicate/const conflict.
Verify the two build variants (compiled-C exe vs. Python shared lib) in CI.

---

## 3. Python layer

Two layers: (a) a low-level ctypes mirror of the ABI, (b) a Pythonic adapter users actually see.

### 3a. Low-level ctypes prototypes (exact ABI)
```python
import ctypes as C
c_int_p, c_dbl_p = C.POINTER(C.c_int), C.POINTER(C.c_double)

FUNC_T  = C.CFUNCTYPE(C.c_int, C.c_int, c_dbl_p, c_int_p, c_dbl_p, C.c_int,
                      c_dbl_p, c_dbl_p, c_dbl_p)
STPNT_T = C.CFUNCTYPE(C.c_int, C.c_int, C.c_double, c_dbl_p, c_dbl_p)
BCND_T  = C.CFUNCTYPE(C.c_int, C.c_int, c_dbl_p, c_int_p, C.c_int,
                      c_dbl_p, c_dbl_p, C.c_int, c_dbl_p, c_dbl_p)
ICND_T  = C.CFUNCTYPE(C.c_int, C.c_int, c_dbl_p, c_int_p, C.c_int,
                      c_dbl_p, c_dbl_p, c_dbl_p, c_dbl_p, C.c_int, c_dbl_p, c_dbl_p)
FOPT_T  = C.CFUNCTYPE(C.c_int, C.c_int, c_dbl_p, c_int_p, c_dbl_p, C.c_int,
                      c_dbl_p, c_dbl_p, c_dbl_p)
PVLS_T  = C.CFUNCTYPE(C.c_int, C.c_int, c_dbl_p, c_dbl_p)

class UserFunctionList(C.Structure):
    _fields_ = [("func", FUNC_T), ("stpnt", STPNT_T), ("bcnd", BCND_T),
                ("icnd", ICND_T), ("fopt", FOPT_T), ("pvls", PVLS_T),
                ("uses_fortran", C.c_int)]
```
The `c_int` return in each prototype mirrors the C header, but the engine calls these as
Fortran `subroutine`s and discards the result — it is ABI-safe and functionally ignored.
Set `uses_fortran = 0` for Python-supplied callbacks.

### 3b. Pointer → NumPy view helpers (no copy)
```python
import numpy as np
def vec(ptr, n):                      # 1-D view, no copy
    return np.ctypeslib.as_array(ptr, shape=(n,))
def mat_F(ptr, rows, cols):           # 2-D Fortran-order view, no copy
    return np.ctypeslib.as_array(ptr, shape=(rows, cols), order='F')
```

### 3c. Adapter — wrap a Pythonic function as a C callback
User writes `def func(u, par): return f[, (dfdu, dfdp)]`. The adapter marshals buffers,
calls it, and writes results back in place:
```python
def _wrap_func(py_func, ndim, ncol):
    def trampoline(ndim_, u, icp, par, ijac, f, dfdu, dfdp):
        U   = vec(u,  ndim_);  PAR = vec(par, NPARX)   # NPARX == 36
        out = py_func(U, PAR)                     # user returns residual (and maybe Jacobian)
        res = out[0] if isinstance(out, tuple) else out
        vec(f, ndim_)[:] = res
        if ijac > 0:                              # JAC=1 run: caller REQUIRES the Jacobian
            du, dp = out[1]                        # user must have supplied it
            mat_F(dfdu, ndim_, ndim_)[:, :] = du   # Fortran column-major
            mat_F(dfdp, ndim_, ncol)[:, :]  = dp
        # return value is ignored by the engine (called as a Fortran subroutine)
    return FUNC_T(trampoline)                     # MUST keep this object alive (see §4)
```
The same pattern wraps `stpnt`/`bcnd`/`icnd`/`fopt`/`pvls` against their shapes in §1.

### 3d. Registration
```python
class _Registry:
    def __init__(self, lib):
        self.lib = lib
        self._keep = []                           # anti-GC anchor (see §4)
    def register(self, ndim, ncol, func=None, stpnt=None, ...):
        ufl = UserFunctionList()
        if func:  cb = _wrap_func(func, ndim, ncol);  self._keep.append(cb); ufl.func = cb
        # ... stpnt/bcnd/icnd/fopt/pvls likewise ...
        ufl.uses_fortran = 0
        self.lib.auto_set_user(C.byref(ufl))
    def clear(self):
        self.lib.auto_reset_user();  self._keep.clear()
```

---

## 4. Hazards to design against (from the plan's risk list)

- **Callback lifetime.** `CFUNCTYPE` objects must be kept referenced for as long as the engine
  may call them (the `_keep` list). If they are GC'd, the engine calls freed memory → crash.
- **Column-major Jacobians.** `dfdu`/`dfdp`/`dbc`/`dint` are Fortran-order; always view with
  `order='F'`. This is the most likely correctness bug.
- **`par` sizing.** Size the `par` view by `NPARX` (= **36**, [auto.h:7](auto.h#L7)), not by `ndim`.
- **Jacobian demand is the `JAC` constant, not a return code.** Set `JAC=0` for FD (callbacks
  see `ijac=0` only); `JAC=1` obliges the callback to fill `dfdu`/`dfdp`. The callback's return
  value is discarded — do not rely on it for control flow.
- **Reentrancy / repeated runs.** The engine uses COMMON blocks + module globals and may call
  `stop`/`exit`. Prove clean re-init before promising repeated in-process `run()`s; convert
  `stop`/`exit` to return codes so the Python/Jupyter host survives. Fallback: subprocess-per-run.
  *Spike result (W4 step 4):* repeated in-process `auto_main_c()` calls on well-formed inputs
  now work and are bit-for-bit reproducible, after fixing the one concrete blocker — the
  constants unit (`fort.2`) was left open across calls, so `AUTO_MAIN` now `CLOSE`s it
  ([main.f90](../src/main.f90)). The module-global allocatables are already cleaned per run
  (`INIT`/`CLEANUP`). **Still open:** error paths call `AUTOSTOP` → `STOP`, which kills the
  host process; a malformed run is therefore not recoverable in-process yet. Convert those to
  return codes (or use subprocess-per-run) before exposing untrusted/edited inputs.
- **Callback overhead.** One Python call per residual evaluation, many per continuation.
  Micro-benchmark early; offer a `numba`/compiled fast path later if needed.
- **GIL.** Callbacks run under the GIL; fine for single-threaded runs. Revisit if the engine
  is built with OpenMP/MPI and calls user functions from multiple threads.

## 5. Acceptance (matches W4 "Done when")
A Python/Jupyter example defines `func`/`stpnt` as Python functions, calls `register(...)`
then `run(...)`, and reproduces a demo's bifurcation diagram — with **no** `make`/`gcc`
invoked — and repeated in-process runs give identical results (or the limitation is documented).
