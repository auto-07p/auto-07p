# Python-callback demos (no compilation)

These are AUTO demos written as **Python functions instead of Fortran**.  The
equations (`func`, `stpnt`, `bcnd`, ...) are registered into the pre-compiled
engine (`libauto.so`) at runtime through `auto.callback`, so running them needs
**no `gfortran`, no `make`, no linking** — the payoff of refactor-plan.md W4.

| demo | source | exercises |
|------|--------|-----------|
| [ab.py](ab.py)   | port of [../ab/ab.f90](../ab/ab.f90)   | `func`, `stpnt` — algebraic (IPS=1) |
| [exp.py](exp.py) | port of [../exp/exp.f90](../exp/exp.f90) | `func`, `stpnt`, `bcnd` — BVP (IPS=4) |

## Prerequisites

Build the shared engine once:

```sh
cmake -S . -B build -DAUTO_BUILD_SHARED=ON
cmake --build build --target auto        # -> lib/libauto.so
```

(Or install the wheel, which bundles it: `pip install dist/auto_07p-*.whl`.)

## Run

```sh
export AUTO_DIR=$PWD                      # only needed from the source tree
cd $(mktemp -d)
python /path/to/demos/pycallback/ab.py    # writes fort.7/8/9 (b/s/d files)
```

Each script reads its constants from the sibling Fortran demo's `c.<name>` file,
so the two paths are directly comparable.

## Verify against the compiled engine

```sh
python test/test_pycallback.py
```

runs each demo both ways (Python callbacks vs. compiled `<name>.f90 + lib/*.o`)
and checks the b-files match — byte-for-byte where the math allows (`ab`), or to
a tight numerical tolerance otherwise (`exp`, whose `np.exp` differs from Fortran
`EXP` by ~1 ULP).  It skips cleanly if `libauto.so` or `gfortran` is absent.
