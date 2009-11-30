#include "auto_f2c.h"

/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
/*   hom : Homoclinic bifurcations in general test example equation */
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int func (integer ndim, const doublereal *u, const integer *icp,
          const doublereal *par, integer ijac,
          doublereal *f, doublereal *dfdu, doublereal *dfdp)
{
  /* System generated locals */
  integer dfdu_dim1 = ndim;
  integer dfdp_dim1 = ndim;

  double a = par[0];
  double b = par[1];
  double c = par[2];
  double alpha = par[3];
  double mu = par[4];
  
  double x = u[0];
  double y = u[1];
  double z = u[2];

  f[0] = a * x + b * y - a * x * x - alpha * z * x * (2. - x * 3.);
  f[1] = b * x + a * y - 1.5 * x * (b * x + a * y) + alpha * z * 2. * y;
  f[2] = c * z + mu * x + 3.0 * x * z + alpha * (x * x * (1. - x) - y * y);

  if (ijac == 0) {
    return 0;
  }

  ARRAY2D(dfdu,0,0) = a - a * 2. * x - alpha * z * (2. - x * 6.);
  ARRAY2D(dfdu,0,1) = b;
  ARRAY2D(dfdu,0,2) = -alpha * x * (2. - x * 3.);

  ARRAY2D(dfdu,1,0) = b - b * 3. * x - a * 1.5 * y;
  ARRAY2D(dfdu,1,1) = a - a * 1.5 * x + alpha * z * 2.;
  ARRAY2D(dfdu,1,2) = alpha * 2. * y;

  ARRAY2D(dfdu,2,0) = mu + 3.0 * z + alpha * x * (2. - x * 3.);
  ARRAY2D(dfdu,2,1) = alpha * -2. * y;
  ARRAY2D(dfdu,2,2) = c + 3.0 * x;

  if (ijac == 1) {
    return 0;
  }

  ARRAY2D(dfdp,0,0) = x - x * x;
  ARRAY2D(dfdp,0,1) = y;
  ARRAY2D(dfdp,0,2) = 0;
  ARRAY2D(dfdp,0,3) = -z * x * (2. - x * 3.);
  ARRAY2D(dfdp,0,4) = 0;

  ARRAY2D(dfdp,1,0) = y - 1.5 * x * y;
  ARRAY2D(dfdp,1,1) = x - 1.5 * x * x;
  ARRAY2D(dfdp,1,2) = 0;
  ARRAY2D(dfdp,1,3) = z * 2. * y;
  ARRAY2D(dfdp,1,4) = 0;

  ARRAY2D(dfdp,2,0) = 0;
  ARRAY2D(dfdp,2,1) = 0;
  ARRAY2D(dfdp,2,2) = z;
  ARRAY2D(dfdp,2,3) = x * x * (1. - x) - y * y;
  ARRAY2D(dfdp,2,4) = x;

  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int stpnt (integer ndim, doublereal t,
           doublereal *u, doublereal *par)
{
  doublereal s;

  /* Sets parameter values for homoclinic bifurcation analysis (IPS=9). */

  /* COMMON block needed if IPS=9 (homoclinic bifurcations) : */

  /* ---------------------------------------------------------------------- 
   */
  /* Problem parameters (only PAR(1-9) are available to the user) : */

  /* a */
  par[0] = 0.;

  /* b */
  par[1] = 0.625;

  /* c */
  par[2] = -2.5;

  /* alpha */
  par[3] = 0;

  /* mu */
  par[4] = 0.;

  /* initial equilibrium */
  
  u[0] = 2./3.;
  u[1] = 0;
  u[2] = 0;
  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int pvls (integer ndim, const doublereal *u,
          doublereal *par)
{

  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
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

