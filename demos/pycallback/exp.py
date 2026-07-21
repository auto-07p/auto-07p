"""exp -- Bratu's boundary value problem, as Python callbacks (no compilation).

The Python equivalent of demos/exp/exp.f90 (a BVP, IPS=4).  Exercises a boundary
-condition callback in addition to func/stpnt:

    u'' + lambda * exp(u) = 0,   u(0) = u(1) = 0

written first order as f = [u1, -lambda*exp(u0)] with Dirichlet BCs.

Note: this uses numpy's np.exp, whose result differs from Fortran's EXP by ~1
ULP; the nonlinear solve amplifies that to ~1e-5 in the bifurcation diagram, so
this reproduces demos/exp to a tight tolerance rather than bit-for-bit (see
test/test_pycallback.py).  Swapping np.exp for math.exp makes it byte-identical.

Run:
    python exp.py       # writes fort.7/8/9 (b/s/d files) in the current dir
"""
import numpy as np


def func(u, par):
    return np.array([u[1], -par[0] * np.exp(u[0])])


def stpnt(t):
    return np.array([0.0, 0.0]), {0: 0.0}       # PAR(1)=lambda=0


def bcnd(par, u0, u1):
    """Boundary conditions FB(1)=u(0), FB(2)=u(1) (both zero)."""
    return np.array([u0[0], u1[0]])


CALLBACKS = dict(func=func, stpnt=stpnt, bcnd=bcnd)


if __name__ == "__main__":
    from _engine import run
    rc, out = run("exp", CALLBACKS)
    print(out)
    raise SystemExit(rc)
