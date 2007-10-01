/* f2c.h  --  Standard Fortran to C header file */

/**  barf  [ba:rf]  2.  "He suggested using FORTRAN, and everybody barfed."

	- From The Shogakukan DICTIONARY OF NEW ENGLISH (Second edition) */

#ifndef F2C_INCLUDE
#define F2C_INCLUDE

#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

typedef int integer;
typedef float real;
typedef double doublereal;
typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;
typedef integer logical;

#define TRUE_ (1)
#define FALSE_ (0)

/* I/O stuff */

#define VOID void

#ifndef abs
#define abs(x) ((x) >= 0 ? (x) : -(x))
#endif
#ifndef fabs
#define fabs(x) ((x) >= 0 ? (x) : -(x))
#endif

#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (doublereal)min(a,b)
#define dmax(a,b) (doublereal)max(a,b)

#define ARRAY2D(array,i,j) array[(i) + (j) * array ## _dim1]
#define ARRAY3D(array,i,j,k) array[(i) + ((j)  + (k) * array ## _dim2) * array ## _dim1]

#ifdef FC_FUNC
#define blhom_1 FC_FUNC(blhom,BLHOM)
#endif

/* problem defined functions*/
typedef int user_func_t(integer ndim, const doublereal *u, const integer *icp,
	 const doublereal *par, integer ijac, 
	 doublereal *f, doublereal *dfdu, doublereal *dfdp);
typedef int user_stpnt_t(integer ndim, doublereal t, 
	  doublereal *u, doublereal *par);
typedef int user_bcnd_t(integer ndim, const doublereal *par, const integer *icp, integer nbc, 
	 const doublereal *u0, const doublereal *u1, integer ijac,
	 doublereal *f, doublereal *dbc);
typedef int user_icnd_t(integer ndim, const doublereal *par, const integer *icp, integer nint, 
	 const doublereal *u, const doublereal *uold, const doublereal *udot, 
	 const doublereal *upold, integer ijac,
	 doublereal *fi, doublereal *dint);
typedef int user_fopt_t(integer ndim, const doublereal *u, const integer *icp, 
	 const doublereal *par, integer ijac, 
	 doublereal *fs, doublereal *dfdu, doublereal *dfdp);
typedef int user_pvls_t(integer ndim, const doublereal *u, doublereal *par);

typedef struct {
  user_func_t *func;
  user_stpnt_t *stpnt;
  user_bcnd_t *bcnd;
  user_icnd_t *icnd;
  user_fopt_t *fopt;
  user_pvls_t *pvls;
  int uses_fortran;
} user_function_list;
extern const user_function_list user;

#ifdef FC_FUNC
extern doublereal FC_FUNC(getp,GETP)(const char *code, integer *ic, 
				     const doublereal *u);
/* C wrapper for getp; call it getp_ if we have name clashes */
#define name 1
#if FC_FUNC(name,NAME) == 1
#define getp getp_
#endif
#undef name
#endif
extern doublereal getp(const char *code, integer ic, const doublereal *u);

#ifndef WRAPPER
/* user functions */
static int func();
static int stpnt();
static int bcnd();
static int icnd();
static int fopt();
static int pvls();
const user_function_list user = { func, stpnt, bcnd, icnd, fopt, pvls };
#endif

#endif
