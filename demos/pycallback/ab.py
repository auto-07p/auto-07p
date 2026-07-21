"""ab -- the A -> B reaction, as Python callbacks (no Fortran compilation).

The Python equivalent of demos/ab/ab.f90: the equations are ordinary Python
functions registered into the engine at runtime.  NumPy arrays are 0-indexed,
so u[0]/u[1] are Fortran U(1)/U(2) and par[0..2] are PAR(1..3).

Run:
    python ab.py        # writes fort.7/8/9 (b/s/d files) in the current dir
"""
import numpy as np


def func(u, par):
    """ODE right-hand side (JAC=0 -> residual only)."""
    e = np.exp(u[1])
    return np.array([
        -u[0] + par[0] * (1.0 - u[0]) * e,
        -u[1] + par[0] * par[1] * (1.0 - u[0]) * e - par[2] * u[1],
    ])


def stpnt(t):
    """Starting solution and parameters PAR(1..3)."""
    return np.array([0.0, 0.0]), {0: 0.0, 1: 8.0, 2: 3.0}


CALLBACKS = dict(func=func, stpnt=stpnt)


if __name__ == "__main__":
    from _engine import run
    rc, out = run("ab", CALLBACKS)
    print(out)
    raise SystemExit(rc)
