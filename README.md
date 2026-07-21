# AUTO-07p

AUTO-07p is software for **continuation and bifurcation analysis of ODEs** —
computing steady states, periodic orbits, boundary-value problems, and their
bifurcations, and tracing how solutions change as parameters vary.

The numerical **engine is Fortran**; it is driven either through the **Python
CLI / command API** or, new in this version, by supplying your equations as
**Python functions** (no compilation) via a callback layer. The same engine is
also installable as a **C/C++ library** you can link your own code against.

Three things build from one source tree:

| Output | For | Compilation at run time? |
|---|---|---|
| **Python wheel** | write equations as Python functions, run from a script/Jupyter | **No** |
| **Installable `libauto`** | C/C++ users linking their own equation code | at build time only |
| **CLI + demos** | the classic workflow: write a Fortran/C stub, the CLI compiles & links it | Yes (per equation) |

Requirements: `gfortran`, `cmake` ≥ 3.13, Python ≥ 3.8 with `numpy`.
(Optional: `matplotlib` for plotting, an MPI toolchain for `-DWITH_MPI`.)

---

## 1. Introduction

An analysis is defined by two things:

- an **equations file** — the right-hand side `func` and a starting point
  `stpnt` (plus `bcnd`/`icnd`/`fopt`/`pvls` for BVPs/optimization), written in
  **Fortran** (`ab.f90`), **C** (`kdv.c`), or **Python**;
- a **constants file** `c.<name>` — problem size, which parameter to continue,
  step sizes, output labels, etc.

Running an analysis produces the AUTO output files **`b.*`** (bifurcation
diagram), **`s.*`** (solutions), and **`d.*`** (diagnostics). The `demos/`
directory contains ~70 worked examples.

---

## 2. Build and install

### 2a. Python wheel (no-compile Python path)

The wheel bundles the pre-compiled engine (`libauto.so`), so users never need a
compiler. Build it with CMake (`-DBUILD_PYTHON=ON` builds and bundles the
engine), then the standard Python build front-end:

```sh
cmake -S . -B build -DBUILD_PYTHON=ON
cmake --build build -j                 # -> python/auto/libauto.so (bundled)
python -m build --wheel --no-isolation # -> dist/auto_07p-<ver>-<py>-<plat>.whl
```

Install it anywhere (see [§4](#4-venv--wheel--python--jupyter)); no `AUTO_DIR`,
`gfortran`, or `make` needed at run time.

### 2b. CMake build & install (engine + installable C/C++ library)

Builds the engine and installs `libauto` for C/C++ users
([§3a](#3a-traditional-compile--link-a-c-or-fortran-stub)):

```sh
cmake -S . -B build -DAUTO_BUILD_SHARED=ON   # add -DBUILD_DEMOS=ON to build every demo
cmake --build build -j
cmake --install build --prefix /your/prefix
```

Installs into the prefix:

```
lib/libauto.a  lib/libauto_c.a          # static engine + C dispatcher archive
include/auto/{auto_f2c.h,auto.h,fcon.h}  # public headers
lib/cmake/AUTO/AUTOConfig.cmake          # find_package(AUTO)
lib/pkgconfig/auto.pc                    # pkg-config auto
```

Useful options: `-DWITH_MPI=ON`, `-DBUILD_DEMOS=ON`,
`-DAUTO_BUILD_SHARED=ON` (PIC `libauto.so` for the Python path).

### 2c. Local development (source tree)

Work directly from a checkout. Build the engine (either build system) and do an
editable install of the CLI:

```sh
export AUTO_DIR="$PWD"

# engine — pick one:
./configure && make -C src        # autotools: builds lib/libauto.a + python/auto/compilers.py
#   …or…
cmake -S . -B build -DAUTO_BUILD_SHARED=ON && cmake --build build -j

pip install -e .                  # editable install of the `auto` CLI (auto_07p)
```

`AUTO_DIR` tells the CLI where to find the engine archive and headers when it
compiles a stub. The build also generates `python/auto/compilers.py` (the
compiler and flags the CLI uses); it is regenerated on every build.

---

## 3. Running a demo — two ways

The `ab` demo is the A→B reaction (a 2-variable system). Work in a scratch copy
so you don't dirty the source tree:

```sh
export AUTO_DIR="$PWD"
export OMPI_MCA_btl="^openib"      # quiet a harmless OpenMPI warning
```

### 3a. Traditional: compile & link a C or Fortran stub

Here the equations are a **Fortran** stub (`demos/ab/ab.f90`) or a **C** stub
(`demos/kdv/kdv.c`). The CLI compiles the stub and links the engine archive
(`libauto.a`, plus `libauto_c.a` for C) — the successor to the old
`$AUTO_DIR/lib/*.o` link:

```sh
mkdir -p /tmp/ab && cp demos/ab/* /tmp/ab && cd /tmp/ab
python "$AUTO_DIR/python/auto" ab.auto        # or just: auto ab.auto
#   compiling ab.f90 -> ab.o, linking ab.o + libauto.a -> ab.exe, running…
#   writes b.ab, s.ab, d.ab
```

A C-equation demo works the same way (`kdv.c` is compiled with the C compiler,
then linked against `libauto.a libauto_c.a`):

```sh
cp -r "$AUTO_DIR/demos/kdv" /tmp/kdv && cd /tmp/kdv && auto kdv.auto
```

**C/C++ users** can instead link the *installed* engine directly (no CLI). See
[demos/clink/README.md](demos/clink/README.md):

```sh
cc  $(pkg-config --cflags auto) -c ab.c
gfortran ab.o $(pkg-config --libs auto) -fopenmp -o ab.exe   # -lauto -lauto_c
# …or with CMake: find_package(AUTO); target_link_libraries(ab AUTO::auto AUTO::auto_c)
```

### 3b. Python script: equations as Python functions (no compilation)

The same analysis with the right-hand side written in Python and registered
into the engine at run time — **no `gfortran`, no `make`**. See
[demos/pycallback/](demos/pycallback/):

```python
# ab.py
import numpy as np
from auto import callback as cb

def func(u, par):                      # ODE right-hand side
    e = np.exp(u[1])
    return np.array([-u[0] + par[0]*(1-u[0])*e,
                     -u[1] + par[0]*par[1]*(1-u[0])*e - par[2]*u[1]])

def stpnt(t):                          # starting solution + PAR(1..3)
    return np.array([0.0, 0.0]), {0: 0.0, 1: 8.0, 2: 3.0}

reg = cb.Registry(cb.load_engine())
reg.register(func=func, stpnt=stpnt, pvls=lambda u, par: None)
with cb.capture_output() as out:       # capture the engine's Fortran output
    reg.run()                          # reads ./fort.2 (constants), writes fort.7/8/9
print(out.text)
```

```sh
mkdir -p /tmp/pyab && cd /tmp/pyab
python "$AUTO_DIR/demos/pycallback/ab.py"      # runnable as-is; stages c.ab itself
```

This path is verified to reproduce the compiled engine
([test/test_pycallback.py](test/test_pycallback.py)).

---

## 4. venv + wheel + Python / Jupyter

Install the wheel into a virtual environment and drive AUTO entirely from
Python — the recommended way to use the no-compile path:

```sh
python -m venv .venv
source .venv/bin/activate               # Windows: .venv\Scripts\activate
pip install dist/auto_07p-*.whl         # bundles the engine; pulls in numpy
```

Then, from any working directory containing a constants file (`fort.2`):

```python
import numpy as np
from auto import callback as cb

reg = cb.Registry(cb.load_engine())     # loads the bundled libauto.so
reg.register(
    func=lambda u, par: np.array([u[1], -par[0]*np.exp(u[0])]),   # Bratu BVP
    stpnt=lambda t: (np.array([0.0, 0.0]), {0: 0.0}),
    bcnd=lambda par, u0, u1: np.array([u0[0], u1[0]]),
    pvls=lambda u, par: None,
)
rc = reg.run()                          # 0 on success; b/s/d files in the cwd
```

### Jupyter

```sh
pip install jupyterlab matplotlib
jupyter lab
```

In a notebook, wrap runs in `cb.capture_output()` so the engine's Fortran
output is captured cleanly instead of leaking to the kernel log, then plot the
`b.*`/`s.*` files with the `auto` parsing utilities or matplotlib:

```python
with cb.capture_output() as out:
    reg.run()
print(out.text)                         # engine progress table
# repeated in-process reg.run() calls are supported for well-formed inputs
```

> Note: the callback layer runs a full continuation in-process. A malformed
> constants file can still abort the engine (and the host); prefer a
> subprocess per run when feeding untrusted input.

---

## More

- Manual: `doc/auto.tex` (LaTeX; `make -C doc auto.pdf`).
- Migrating from the old `@`-command / `auto.env` workflow:
  [doc/migrating-from-cmds.md](doc/migrating-from-cmds.md).
- Restructuring roadmap and design notes: [refactor-plan.md](refactor-plan.md),
  [doc/python-callback-api.md](doc/python-callback-api.md).
