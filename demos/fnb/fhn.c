#include "auto_f2c.h"

extern struct {
    integer itwist, istart, iequib, nfixed, npsi, nunstab, nstab, nrev;
} blhom_1;

/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
/*   fhn : Homoclinic bifurcations in Fitz-Hugh Nagumo System */
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */

int func(integer ndim, const doublereal *u, const integer *icp, 
	const doublereal *par, integer ijac, doublereal *f, doublereal *dfdu, 
	doublereal *dfdp)
{
    /* System generated locals */
    integer dfdu_dim1 = ndim, dfdp_dim1 = ndim;

    /* Function Body */
    f[0] = u[1];
    f[1] = par[0] * u[1] + u[0] * (u[0] - par[1]) * (u[0] - 1.) + u[2];
    f[2] = par[2] * u[0] / par[0];

    if (ijac == 0) {
	return 0;
    }

    ARRAY2D(dfdu,0,0) = 0.;
    ARRAY2D(dfdu,0,1) = 1.;
    ARRAY2D(dfdu,0,2) = 0.;

    ARRAY2D(dfdu,1,0) = u[0] * 3 * u[0] - (par[1] + 1) * 2 * u[0] + par[1];
    ARRAY2D(dfdu,1,1) = par[0];
    ARRAY2D(dfdu,1,2) = 1.;

    ARRAY2D(dfdu,2,0) = par[2] / par[0];
    ARRAY2D(dfdu,2,1) = 0.;
    ARRAY2D(dfdu,2,2) = 0.;

    if (ijac == 1) {
	return 0;
    }


    ARRAY2D(dfdp,0,0) = 0.;
    ARRAY2D(dfdp,0,1) = 0.;
    ARRAY2D(dfdp,0,2) = 0.;

    ARRAY2D(dfdp,1,0) = u[1];
    ARRAY2D(dfdp,1,1) = -u[0] * (u[0] - 1.);
    ARRAY2D(dfdp,1,2) = 0.;

    ARRAY2D(dfdp,2,0) = -par[2] * u[0] / (par[0] * par[0]);
    ARRAY2D(dfdp,2,1) = 0.;
    ARRAY2D(dfdp,2,2) = u[0] / par[0];

    return 0;
} /* func */


int stpnt(integer ndim, doublereal t, doublereal *u, doublereal *par)
{
/*     ---------------- */

/* Sets parameter values for homoclinic bifurcation analysis (IPS=9). */


/* COMMON block needed if IPS=9 (homoclinic bifurcations) : */

/* ---------------------------------------------------------------------- */
/* Problem parameters (only PAR(1-9) are available to the user) : */
    /* Function Body */
    par[0] = .21;
/* c */
    par[1] = .2;
/* a */
    par[2] = .0025;

/* b = epsilon */
    par[10] = .1;
/* ---------------------------------------------------------------------- */
/* If IEQUIB=1 then put the equilibrium in PAR(11+i), i=1,...,NDIM : */

/* truncated time interval */
    if (blhom_1.iequib != 0) {
	par[11] = 0.;
	par[12] = 0.;
	par[13] = 0.;
    }
/* ---------------------------------------------------------------------- */
/* Distance along the unstable manifold : */

    if (blhom_1.istart == 3) {
	par[ndim * blhom_1.iequib + 11] = 1e-5;
    }
/* ---------------------------------------------------------------------- */

    return 0;
} /* stpnt */


/* Subroutine */ int pvls(integer ndim, const doublereal *u,
          doublereal *par)
{
    return 0;
} /* pvls */


/* Subroutine */ int bcnd(integer ndim, const doublereal *par, const integer *icp,
          integer nbc, const doublereal *u0, const doublereal *u1, integer ijac,
          doublereal *fb, doublereal *dbc)
{
    return 0;
} /* bcnd */


/* Subroutine */ int icnd(integer ndim, const doublereal *par, const integer *icp,
          integer nint, const doublereal *u, const doublereal *uold,
          const doublereal *udot, const doublereal *upold, integer ijac,
          doublereal *fi, doublereal *dint)
{
    return 0;
} /* icnd */


/* Subroutine */ int fopt(integer ndim, const doublereal *u, const integer *icp,
          const doublereal *par, integer ijac,
          doublereal *fs, doublereal *dfdu, doublereal *dfdp)
{
    return 0;
} /* fopt */










