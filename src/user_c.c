/* wrapper functions for user files written in C */

#define WRAPPER

#include "config.h"
#include "auto_f2c.h"

#ifdef FC_FUNC

int FC_FUNC(func,FUNC)(integer *ndim, const doublereal *u, const integer *icp,
		       const doublereal *par, const integer *ijac,
		       doublereal *f, doublereal *dfdu, doublereal *dfdp)
{
  user.func(*ndim, u, icp, par, *ijac, f, dfdu, dfdp);
  return 0;
}

int FC_FUNC(stpnt,STPNT)(integer *ndim, doublereal *u, doublereal *par,
			 doublereal *t)
{
  user.stpnt(*ndim, *t, u, par);
  return 0;
}

int FC_FUNC(bcnd,BCND)(integer *ndim, const doublereal *par,
		       const integer *icp, integer *nbc, const doublereal *u0,
		       const doublereal *u1, doublereal *fb, integer *ijac,
		       doublereal *dbc)
{
  user.bcnd(*ndim, par, icp, *nbc, u0, u1, *ijac, fb, dbc);
  return 0;
}

int FC_FUNC(icnd,ICND)(integer *ndim, const doublereal *par,
		       const integer *icp, integer *nint, const doublereal *u,
		       const doublereal *uold, const doublereal *udot,
		       const doublereal *upold, doublereal *fi, integer *ijac,
		       doublereal *dint)
{
  user.icnd(*ndim, par, icp, *nint, u, uold, udot, upold, *ijac, fi, dint);
  return 0;
}

int FC_FUNC(fopt,FOPT)(integer *ndim, const doublereal *u, const integer *icp,
		       const doublereal *par, integer *ijac, doublereal *fs,
		       doublereal *dfdu, doublereal *dfdp)
{
  user.fopt(*ndim, u, icp, par, *ijac, fs, dfdu, dfdp);
  return 0;
}

int FC_FUNC(pvls,PVLS)(integer *ndim, const void *u, doublereal *par)
{
  user.pvls(*ndim, u, par);
  return 0;
}

doublereal getp(const char *code, integer ic, const doublereal *u)
#undef getp
{
  char ncode[4];
  memcpy(ncode, code, 3);
  ncode[3] = 0;
  return FC_FUNC(getp,GETP)(ncode, &ic, u);

}

#endif
