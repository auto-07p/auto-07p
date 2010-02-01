/* chamgr.f -- translated by f2c (version 19991025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "auto_f2c.h"

/* Common Block Declarations */

extern struct {
    integer itwist, istart, iequib, nfixed, npsi, nunstab, nstab, nrev;
} blhom_1;

/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
/*   chamgr: Champneys and Groves solitary waves. */
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */

int func (integer ndim, const doublereal *u,
                           const integer *icp, 
	const doublereal *par, integer ijac,
                           doublereal *f, doublereal *dfdu, 
	doublereal *dfdp)
{
    /* System generated locals */
    integer dfdu_dim1, dfdp_dim1;

    /* Local variables */
    doublereal dhdp1, dhdq1, dhdq2, dhdp2, a, b, c;

/*     ---------- ---- */


    /* Function Body */
    dfdu_dim1 = dfdp_dim1 = ndim;
    a = par[0];
    b = par[1];
    c = par[2];

/*     The real equation (Hamiltonian) + lambda (PAR(3)) *dH */

    dhdq1 = u[0] * -1.5 * u[0] - a * u[0] + u[2] * .5 * u[2];
    dhdp1 = u[2];
    dhdq2 = u[1] - b * u[2] + u[0] * u[2];
    dhdp2 = u[3] * 7.5;
/*   \dot q_1 = q_2 */
    f[0] = dhdp1 + c * dhdq1;
/*   \dot p_1 = 3/2 q_1^2 + a q_1 - 1/2 q_2^2 */
    f[1] = -dhdq1 + c * dhdp1;
/*   \dot q_2 = 15/2 p_2 */
    f[2] = dhdp2 + c * dhdq2;
/*   \dot p_2 = -p_1+b q_2-q_1 q_2 */
    f[3] = -dhdq2 + c * dhdp2;

    if (ijac == 0) {
	return 0;
    }

    ARRAY2D(dfdu,0,0) = c * (u[0] * -3. - a);
    ARRAY2D(dfdu,0,1) = 0.;
    ARRAY2D(dfdu,0,2) = c * u[2] + 1.;
    ARRAY2D(dfdu,0,3) = 0.;

    ARRAY2D(dfdu,1,0) = u[0] * 3. + a;
    ARRAY2D(dfdu,1,1) = 0.;
    ARRAY2D(dfdu,1,2) = -u[2] + c;
    ARRAY2D(dfdu,1,3) = 0.;

    ARRAY2D(dfdu,2,0) = c * u[2];
    ARRAY2D(dfdu,2,1) = c;
    ARRAY2D(dfdu,2,2) = -c * (b - u[0]);
    ARRAY2D(dfdu,2,3) = 7.5;

    ARRAY2D(dfdu,3,0) = -u[2];
    ARRAY2D(dfdu,3,1) = -1.;
    ARRAY2D(dfdu,3,2) = b - u[0];
    ARRAY2D(dfdu,3,3) = c * 7.5;

    if (ijac == 1) {
	return 0;
    }

    ARRAY2D(dfdp,0,0) = -c * u[0];
    ARRAY2D(dfdp,0,1) = 0;
    ARRAY2D(dfdp,0,2) = dhdq1;

    ARRAY2D(dfdp,1,0) = u[0];
    ARRAY2D(dfdp,1,1) = 0;
    ARRAY2D(dfdp,1,2) = dhdp1;

    ARRAY2D(dfdp,2,0) = 0;
    ARRAY2D(dfdp,2,1) = -c * u[2];
    ARRAY2D(dfdp,2,2) = dhdq2;

    ARRAY2D(dfdp,3,0) = 0;
    ARRAY2D(dfdp,3,1) = u[2];
    ARRAY2D(dfdp,3,2) = dhdp2;

    return 0;
} /* func_ */


int stpnt(integer ndim, const doublereal t,
          doublereal *u, doublereal *par)
{
    /* Local variables */
    doublereal a, b, f, s, f2, sec2, d1;

/*     ---------------- */

/* Sets parameter values for homoclinic bifurcation analysis (IPS=9). */



/* COMMON block needed if IPS=9 (homoclinic bifurcations) : */

/* ---------------------------------------------------------------------- */
/* Problem parameters (only PAR(1-9) are available to the user) : */

    /* Parameter adjustments */

    /* Function Body */
    par[1] = (sqrt(65.) + 3.) / 4.; /* B */
    par[0] = (par[1] * 2. + 1.) * .6 * (par[1] - 2.); /* A */
    par[2] = 0.; /* C or lambda */
    
    par[10] = 20.;
/* ---------------------------------------------------------------------- */
/* If IEQUIB=1 then put initial equilibrium in PAR(11+i), i=0,...,NDIM-1 : */

/* truncated time interval */
    if (blhom_1.iequib != 0) {
	par[11] = 0.;
	par[12] = 0.;
	par[13] = 0.;
	par[14] = 0.;
    }
/* ---------------------------------------------------------------------- */
/* IF ISTART=2 then put analytic homoclinic orbit here with T in the */
/*   interval [0,1] */

/* test example (a=0,b=1) */

    if (blhom_1.istart == 2) {
	if (blhom_1.nrev == 0) {
	    s = (t - .5) * par[10];
	} else {
	    s = (t - 1.) * par[10];
	}
	f = sqrt((par[1] * 2. + 1.) * .75);
/* Computing 2nd power */
	d1 = cosh(f * s);
	sec2 = 1 / (d1 * d1);
	f2 = f * f;
	u[0] = f2 * 2. * sec2;
	u[1] = f2 * -.13333333333333333 * f * tanh(f * s) * sec2 * (f2 * 4 - 
		f2 * 12. * sec2 - 15.);
	u[2] = f2 * -4. * f * tanh(f * s) * sec2;
	u[3] = f2 * .53333333333333333 * f2 * sec2 * (2. - sec2 * 3.);
	a = par[0];
	b = par[1];
/*      H = -0.5D0*(U(1)+A)*U(1)*U(1)+U(2)*U(3)- */
/*     +  0.5D0*(B-U(1))*U(3)*U(3)+15D0/4D0*U(4)*U(4) */
/*      PRINT *, H */
    }
/* ---------------------------------------------------------------------- */
/* Distance along the unstable manifold : */

    if (blhom_1.istart == 3) {
	par[ndim * blhom_1.iequib + 11] = -1.e-5;
    }
/* ---------------------------------------------------------------------- */
/* C */
    return 0;
} /* stpnt_ */


int pvls(integer ndim, const doublereal *u, doublereal *par)
{
    /* Local variables */
    integer i;


/* COMMON block needed if IPS=9 (homoclinic bifurcations) : */
/* If IEQUIB=0 put analytic equilibrium in PAR(11+i), i=0,...,NDIM-1 : */

    /* Parameter adjustments */

    /* Function Body */
    for (i = 0; i < ndim; ++i) {
	par[i + 11] = 0.;
    }
    return 0;
} /* pvls_ */


int bcnd (integer ndim, const doublereal *par, const integer *icp,
          integer nbc, const doublereal *u0, const doublereal *u1, integer ijac,
          doublereal *fb, doublereal *dbc)
{
  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int icnd (integer ndim, const doublereal *par, const integer *icp,
          integer nint, const doublereal *u, const doublereal *uold,
          const doublereal *udot, const doublereal *upold, integer ijac,
          doublereal *fi, doublereal *dint)
{
  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */

int fopt (integer ndim, const doublereal *u, const integer *icp,
          const doublereal *par, integer ijac,
          doublereal *fs, doublereal *dfdu, doublereal *dfdp)
{
  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */





