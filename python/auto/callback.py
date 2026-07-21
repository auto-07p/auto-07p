"""Runtime Python-callback layer for the AUTO engine (refactor-plan.md W4).

The engine dispatches user equations through a C function-pointer table
(`user_function_list` in include/auto_f2c.h), invoked from Fortran via
`c_f_procpointer` (src/user_c.f90).  Normally that table is filled at link
time by a compiled equation file.  This module fills it **at runtime from
Python**, so no gcc/gfortran/make is needed: you write `func`/`stpnt`/... as
Python functions, `register()` them, and the engine calls back into Python.

Layers (see doc/python-callback-api.md):
  * a low-level ctypes mirror of the C ABI (`UserFunctionList`, the `*_T`
    prototypes) -- exact, do not change;
  * a Pythonic adapter (`Registry.register`) that marshals the raw `double*`
    arguments as NumPy views **without copying**, calls your function, and
    writes results back in place.

Requires the PIC shared engine (build with `-DAUTO_BUILD_SHARED=ON`, producing
lib/libauto.so) and NumPy.  Reentrancy / actually running a continuation is
wired up in a later W4 step; this module provides the registration ABI and is
independently verifiable via `python -m auto.callback` (`_selftest`).
"""

import contextlib
import ctypes
import os
import sys
import tempfile
from ctypes import (CFUNCTYPE, POINTER, Structure, byref, c_double, c_int)

# NPARX: compile-time PARAMETER in include/auto.h (= 36).  `par` arrays are
# sized by THIS, never by ndim.  Keep in sync if include/auto.h ever changes
# (it documents "Do not decrease NPARX below 20").
NPARX = 36

try:
    import numpy as _np
    _NUMPY_ERR = None
except ImportError as _e:          # numpy is required, but defer the error so
    _np = None                     # importing this module never hard-fails.
    _NUMPY_ERR = _e

c_int_p = POINTER(c_int)
c_dbl_p = POINTER(c_double)

# ---- 1. Low-level ctypes mirror of the C ABI -------------------------------
# Argument order and by-value scalars match the Fortran bind(c) interfaces the
# engine actually calls (src/user_c.f90) and the C typedefs (auto_f2c.h).
# The `c_int` return mirrors the header; the engine invokes these as Fortran
# subroutines and discards the result, so returning 0 is ABI-safe.
FUNC_T = CFUNCTYPE(c_int, c_int, c_dbl_p, c_int_p, c_dbl_p, c_int,
                   c_dbl_p, c_dbl_p, c_dbl_p)
STPNT_T = CFUNCTYPE(c_int, c_int, c_double, c_dbl_p, c_dbl_p)
BCND_T = CFUNCTYPE(c_int, c_int, c_dbl_p, c_int_p, c_int,
                   c_dbl_p, c_dbl_p, c_int, c_dbl_p, c_dbl_p)
ICND_T = CFUNCTYPE(c_int, c_int, c_dbl_p, c_int_p, c_int,
                   c_dbl_p, c_dbl_p, c_dbl_p, c_dbl_p, c_int, c_dbl_p, c_dbl_p)
FOPT_T = CFUNCTYPE(c_int, c_int, c_dbl_p, c_int_p, c_dbl_p, c_int,
                   c_dbl_p, c_dbl_p, c_dbl_p)
PVLS_T = CFUNCTYPE(c_int, c_int, c_dbl_p, c_dbl_p)


class UserFunctionList(Structure):
    """Mirror of `user_function_list` (auto_f2c.h / src/user_c.f90)."""
    _fields_ = [("func", FUNC_T), ("stpnt", STPNT_T), ("bcnd", BCND_T),
                ("icnd", ICND_T), ("fopt", FOPT_T), ("pvls", PVLS_T),
                ("uses_fortran", c_int)]


# ---- 2. Pointer -> NumPy view helpers (no copy) ----------------------------
def _vec(ptr, n):
    """1-D NumPy view over `ptr[0:n]` -- shares memory, no copy."""
    return _np.ctypeslib.as_array(ptr, shape=(n,))


def _mat_F(ptr, rows, cols):
    """2-D **Fortran-order** (column-major) view over `ptr` -- no copy.

    All AUTO Jacobians (dfdu/dfdp/dbc/dint) are column-major: element (i, j)
    lives at ptr[i + j*rows].  Assigning `view[:, :] = A` writes A in that
    layout regardless of A's own memory order.
    """
    flat = _np.ctypeslib.as_array(ptr, shape=(rows * cols,))
    return flat.reshape((rows, cols), order="F")


def _residual_and_jac(out):
    """Split a callback's return into (residual, jacobian-or-None)."""
    if isinstance(out, tuple):
        return out[0], out[1]
    return out, None


# ---- 3. Adapters: wrap a Pythonic function as a C callback ------------------
# Contract for each callback is documented on Registry.register.  Every
# trampoline returns 0 (ignored by the engine) and must be kept alive by the
# caller (Registry._keep) or the engine will call freed memory.

def _wrap_func(py):
    def trampoline(ndim, u, icp, par, ijac, f, dfdu, dfdp):
        res, jac = _residual_and_jac(py(_vec(u, ndim), _vec(par, NPARX)))
        _vec(f, ndim)[:] = res
        if ijac > 0:                                   # JAC=1: must supply J
            du, dp = jac
            _mat_F(dfdu, ndim, ndim)[:, :] = du
            dp = _np.asarray(dp).reshape(ndim, -1, order="F")
            _mat_F(dfdp, ndim, dp.shape[1])[:, :] = dp
        return 0
    return FUNC_T(trampoline)


def _wrap_stpnt(py):
    def trampoline(ndim, t, u, par):
        out = py(t)                                    # -> u  or  (u, par)
        u_out, par_out = out if isinstance(out, tuple) else (out, None)
        _vec(u, ndim)[:] = u_out
        if par_out is not None:
            P = _vec(par, NPARX)
            if isinstance(par_out, dict):
                for k, v in par_out.items():
                    P[k] = v
            else:
                par_out = _np.asarray(par_out)
                P[:par_out.size] = par_out
        return 0
    return STPNT_T(trampoline)


def _wrap_bcnd(py):
    def trampoline(ndim, par, icp, nbc, u0, u1, ijac, fb, dbc):
        res, jac = _residual_and_jac(
            py(_vec(par, NPARX), _vec(u0, ndim), _vec(u1, ndim)))
        _vec(fb, nbc)[:] = res
        if ijac > 0 and jac is not None:
            jac = _np.asarray(jac)
            _mat_F(dbc, nbc, jac.shape[1])[:, :] = jac
        return 0
    return BCND_T(trampoline)


def _wrap_icnd(py):
    def trampoline(ndim, par, icp, nint, u, uold, udot, upold, ijac, fi, dint):
        res, jac = _residual_and_jac(
            py(_vec(par, NPARX), _vec(u, ndim), _vec(uold, ndim),
               _vec(udot, ndim), _vec(upold, ndim)))
        _vec(fi, nint)[:] = res
        if ijac > 0 and jac is not None:
            jac = _np.asarray(jac)
            _mat_F(dint, nint, jac.shape[1])[:, :] = jac
        return 0
    return ICND_T(trampoline)


def _wrap_fopt(py):
    def trampoline(ndim, u, icp, par, ijac, fs, dfdu, dfdp):
        res, jac = _residual_and_jac(py(_vec(u, ndim), _vec(par, NPARX)))
        _vec(fs, 1)[0] = res                           # fs is a scalar
        if ijac > 0 and jac is not None:
            du, dp = jac
            _vec(dfdu, ndim)[:] = du
            dp = _np.asarray(dp)
            _vec(dfdp, dp.size)[:] = dp
        return 0
    return FOPT_T(trampoline)


def _wrap_pvls(py):
    def trampoline(ndim, u, par):
        py(_vec(u, ndim), _vec(par, NPARX))            # writes par in place
        return 0
    return PVLS_T(trampoline)


_WRAPPERS = {"func": _wrap_func, "stpnt": _wrap_stpnt, "bcnd": _wrap_bcnd,
             "icnd": _wrap_icnd, "fopt": _wrap_fopt, "pvls": _wrap_pvls}


# ---- 4. Locating and loading the shared engine -----------------------------
def find_library():
    """Return the path to libauto.so, or raise OSError listing what was tried."""
    here = os.path.dirname(os.path.abspath(__file__))
    tried = []
    autodir = os.environ.get("AUTO_DIR")
    if autodir:
        tried.append(os.path.join(autodir, "lib", "libauto.so"))
    tried.append(os.path.join(here, "libauto.so"))                 # bundled (wheel)
    tried.append(os.path.join(here, "..", "..", "lib", "libauto.so"))  # source tree
    for p in tried:
        if os.path.exists(p):
            return os.path.abspath(p)
    raise OSError("libauto.so not found (build with -DAUTO_BUILD_SHARED=ON). "
                  "Searched: %r" % tried)


def load_engine(path=None):
    """Load libauto.so and declare the runtime entry-point signatures."""
    if _np is None:
        raise ImportError(
            "numpy is required for the AUTO callback layer") from _NUMPY_ERR
    # Make libgfortran's preconnected units (unit 6 = stdout, unit 0 = stderr)
    # unbuffered so capture_output() sees engine writes immediately.  Read by
    # libgfortran on first Fortran I/O; setdefault lets the user override.
    os.environ.setdefault("GFORTRAN_UNBUFFERED_PRECONNECTED", "y")
    lib = ctypes.CDLL(path or find_library())
    lib.auto_set_user.argtypes = [POINTER(UserFunctionList)]
    lib.auto_set_user.restype = None
    lib.auto_reset_user.argtypes = []
    lib.auto_reset_user.restype = None
    lib.auto_main_c.argtypes = []
    lib.auto_main_c.restype = c_int
    return lib


# ---- 4b. Capturing the engine's Fortran output -----------------------------
class _Capture:
    """Holds text captured by capture_output(); populated when the block exits."""
    __slots__ = ("text",)

    def __init__(self):
        self.text = ""


@contextlib.contextmanager
def capture_output():
    """Capture the engine's C-level stdout(1)/stderr(2) for the block's duration.

    The Fortran engine writes its progress table via `WRITE(6, ...)`, which goes
    to file descriptor 1 and bypasses `sys.stdout` entirely -- so in a notebook
    it leaks to the kernel's terminal (or is lost).  This redirects fds 1 and 2
    to a temporary file with `os.dup2` for the duration, then restores them and
    exposes what was written.

    Yields a `_Capture` whose `.text` holds the output after the block::

        with capture_output() as out:
            reg.run()
        print(out.text)            # the engine's b-diagram summary, etc.

    Pair with load_engine() (which requests unbuffered preconnected units) so
    nothing is left in libgfortran's buffers when the fds are restored.
    """
    cap = _Capture()
    sys.stdout.flush()
    sys.stderr.flush()
    saved1, saved2 = os.dup(1), os.dup(2)
    tf = tempfile.TemporaryFile(mode="w+")
    try:
        os.dup2(tf.fileno(), 1)
        os.dup2(tf.fileno(), 2)
        yield cap
    finally:
        sys.stdout.flush()
        sys.stderr.flush()
        os.dup2(saved1, 1)
        os.close(saved1)
        os.dup2(saved2, 2)
        os.close(saved2)
        tf.flush()
        tf.seek(0)
        cap.text = tf.read()
        tf.close()


# ---- 5. Registry -----------------------------------------------------------
class Registry:
    """Register Python callbacks into the engine's `user` table.

    Callback contracts (all arrays are NumPy views; `par` has length NPARX):
      func(u, par)                     -> f  or  (f, (dfdu, dfdp))
      stpnt(t)                         -> u  or  (u, par)   # initial data
      bcnd(par, u0, u1)                -> fb or (fb, dbc)
      icnd(par, u, uold, udot, upold)  -> fi or (fi, dint)
      fopt(u, par)                     -> fs (scalar) or (fs, (dfdu, dfdp))
      pvls(u, par)                     -> None            # writes par in place
    Supply Jacobians only when the constants-file JAC=1 (then they are
    required); with JAC=0 the engine finite-differences and never asks.
    """

    def __init__(self, lib):
        self.lib = lib
        self._keep = []            # anti-GC anchor for the CFUNCTYPE objects
        self._ufl = None

    def register(self, **callbacks):
        """register(func=..., stpnt=..., bcnd=..., icnd=..., fopt=..., pvls=...)."""
        unknown = set(callbacks) - set(_WRAPPERS)
        if unknown:
            raise TypeError("unknown callback(s): %s" % ", ".join(sorted(unknown)))
        ufl = UserFunctionList()
        keep = []
        for name, py in callbacks.items():
            if py is None:
                continue
            cb = _WRAPPERS[name](py)
            keep.append(cb)
            setattr(ufl, name, cb)
        ufl.uses_fortran = 0
        self._keep = keep          # replace anchors only after building the new set
        self._ufl = ufl
        self.lib.auto_set_user(byref(ufl))
        return self

    def clear(self):
        self.lib.auto_reset_user()
        self._keep = []
        self._ufl = None

    def run(self):
        """Run one continuation from the AUTO constants in ./fort.2.

        Reads the constants file (fort.2) and any restart data from the current
        working directory -- exactly as the executable does -- and writes the
        b/s/d output to fort.7/8/9.  Callbacks must be register()ed first.
        Returns the engine exit code (0 on success).

        Repeated in-process runs of *well-formed* inputs are supported (the
        engine re-reads fort.2 from the start each call).  Caveat (W4): a
        malformed constants file or other engine error path still triggers
        AUTOSTOP, which calls STOP and terminates the host process; converting
        those to return codes is deferred.
        """
        return self.lib.auto_main_c()


# ---- 6. Self-test (no engine run needed) -----------------------------------
def _selftest():
    """Drive the trampolines exactly as the engine would; verify marshaling."""
    lib = load_engine()
    reg = Registry(lib)

    def func(u, par):
        # f0 = u0 + 2 u1 + par0 ; f1 = 3 u0 + 4 u1
        f = _np.array([u[0] + 2 * u[1] + par[0], 3 * u[0] + 4 * u[1]])
        dfdu = _np.array([[1.0, 2.0], [3.0, 4.0]])     # df_i/du_j
        dfdp = _np.zeros((2, NPARX)); dfdp[0, 0] = 1.0
        return f, (dfdu, dfdp)

    def stpnt(t):
        return _np.array([0.5 * t, -t]), {2: 7.0}       # u(t), par[2]=7

    reg.register(func=func, stpnt=stpnt)
    cb_func, cb_stpnt = reg._keep

    ndim = 2
    u = (c_double * ndim)(1.0, 1.0)
    par = (c_double * NPARX)(); par[0] = 10.0
    icp = (c_int * NPARX)()
    f = (c_double * ndim)()
    dfdu = (c_double * (ndim * ndim))()
    dfdp = (c_double * (ndim * NPARX))()
    cb_func(ndim, u, icp, par, 1, f, dfdu, dfdp)

    assert list(f) == [13.0, 7.0], list(f)                       # residual
    assert list(dfdu) == [1.0, 3.0, 2.0, 4.0], list(dfdu)        # column-major!
    assert dfdp[0] == 1.0 and dfdp[1] == 0.0, (dfdp[0], dfdp[1])  # par sizing

    u2 = (c_double * ndim)()
    par2 = (c_double * NPARX)()
    cb_stpnt(ndim, 2.0, u2, par2)
    assert list(u2) == [1.0, -2.0], list(u2)                     # u(t=2)
    assert par2[2] == 7.0, par2[2]                               # par[2] set

    reg.clear()
    print("callback self-test OK: residual, column-major Jacobian, par(NPARX) "
          "sizing, and stpnt initial-data marshaling all verified against "
          "libauto.so")


if __name__ == "__main__":
    _selftest()
