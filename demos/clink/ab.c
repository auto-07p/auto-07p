/* ab -- the A -> B reaction, as a C equation linked against the INSTALLED
 * AUTO engine (refactor-plan.md W5, Goal 9).
 *
 * This is the successor to the old workflow of linking $AUTO_DIR/lib/*.o:
 * include the installed header, implement the user functions in C, then
 *
 *     cc  $(pkg-config --cflags auto) -c ab.c
 *     gfortran ab.o $(pkg-config --libs auto) -fopenmp -o ab.exe
 *     cp c.ab fort.2 && ./ab.exe          # writes b/s/d files
 *
 * or, from CMake:  find_package(AUTO); target_link_libraries(ab AUTO::auto AUTO::auto_c)
 *
 * The header declares func/stpnt/... (static) and defines the `user` table that
 * the installed libauto_c dispatchers read at run time; we only fill in the
 * bodies. Indices are 0-based here: u[0]/u[1] are U(1)/U(2), par[0..2] PAR(1..3).
 */
#include <auto_f2c.h>

int func(integer ndim, const doublereal *u, const integer *icp,
         const doublereal *par, integer ijac,
         doublereal *f, doublereal *dfdu, doublereal *dfdp)
{
    doublereal e = exp(u[1]);
    f[0] = -u[0] + par[0] * (1.0 - u[0]) * e;
    f[1] = -u[1] + par[0] * par[1] * (1.0 - u[0]) * e - par[2] * u[1];
    return 0;
}

int stpnt(integer ndim, doublereal t, doublereal *u, doublereal *par)
{
    par[0] = 0.0;   /* PAR(1) */
    par[1] = 8.0;   /* PAR(2) */
    par[2] = 3.0;   /* PAR(3) */
    u[0] = 0.0;
    u[1] = 0.0;
    return 0;
}

/* Unused for this problem, but required by the user-function table. */
int bcnd(integer ndim, const doublereal *par, const integer *icp, integer nbc,
         const doublereal *u0, const doublereal *u1, integer ijac,
         doublereal *fb, doublereal *dbc)
{
    return 0;
}

int icnd(integer ndim, const doublereal *par, const integer *icp, integer nint,
         const doublereal *u, const doublereal *uold, const doublereal *udot,
         const doublereal *upold, integer ijac, doublereal *fi, doublereal *dint)
{
    return 0;
}

int fopt(integer ndim, const doublereal *u, const integer *icp,
         const doublereal *par, integer ijac,
         doublereal *fs, doublereal *dfdu, doublereal *dfdp)
{
    return 0;
}

int pvls(integer ndim, const doublereal *u, doublereal *par)
{
    return 0;
}
