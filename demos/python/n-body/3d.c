/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
/*		nb :	The restricted 3-body problem 				  */
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
#include	"auto_f2c.h"
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int func (integer ndim, const double *u, const integer *icp, 
     const double *par, integer ijac, double *f, double *dfdu, double *dfdp)
{
  double mu,p;
  double x, y, z;
  double xp, yp, zp;
  double rone, rone2, rone3;
  double rtwo, rtwo2, rtwo3;
  double Cx, Cy, Cz, Cxp, Cyp, Czp;
  
  x = u[0];	
  y = u[1];
  z = u[2];
  xp = u[3];	
  yp = u[4];
  zp = u[5];
  
  mu = par[0];
  p  = par[1];
  
  rone = sqrt( (x+mu)*(x+mu) + y*y + z*z ); 		
  rone2 = rone*rone;	
  rone3 = rone2*rone;
  
  rtwo = sqrt( (x-1+mu)*(x-1+mu) + y*y + z*z ); 
  rtwo2 = rtwo*rtwo;	
  rtwo3 = rtwo2*rtwo;
  
  Cx = x - (1-mu)*(x+mu)/rone3 - mu*(x-1+mu)/rtwo3;
  Cy = y - (1-mu)*y/rone3      - mu*y/rtwo3;
  Cz =   - (1-mu)*z/rone3      - mu*z/rtwo3;
  
  Cxp = -2*xp;		 
  Cyp = -2*yp;
  Czp = -2*zp;
  
  f[0] = xp;
  f[1] = yp;
  f[2] = zp;
#ifdef NEGATIVE_U
  f[3] =  2*yp - x + (1-mu)*(x+mu)/rone3 + mu*(x-1+mu)/rtwo3;
  f[4] = -2*xp - y + (1-mu)*y/rone3      + mu*y/rtwo3;
  f[5] =             (1-mu)*z/rone3      + mu*z/rtwo3;
#else
  f[3] =  2*yp + x - (1-mu)*(x+mu)/rone3 - mu*(x-1+mu)/rtwo3;
  f[4] = -2*xp + y - (1-mu)*y/rone3      - mu*y/rtwo3;
  f[5] =           - (1-mu)*z/rone3      - mu*z/rtwo3;
#endif

#ifdef ALL_UNFOLDING
  f[0] += p*Cx;
  f[1] += p*Cy;
  f[2] += p*Cz;
  f[3] += p*Cxp;
  f[4] += p*Cyp;
  f[5] += p*Czp;
#else
  f[3] += p*Cxp;
  f[4] += p*Cyp;
  f[5] += p*Czp;
#endif

  return 0;
} 
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int stpnt (integer ndim, double t, double *u, double *par)
{
  double mu;
  
  mu =0.0;
  par[0] = mu;
  par[1] = 0.;
  
  u[0] = 0.14107; 
  u[1] = 0.99;
  u[2] = 0.0;
  u[3] = 0.0;
  u[4] = 0.0;
  u[5] = 0.0;
  
  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int pvls (integer ndim, const double *u, double *par)
{
  integer tmp;
  extern double getp();
  double x, y, z;
  double xp, yp, zp;
  double mu;
  double rone, rtwo;

  mu = par[0];

  x  = getp("BV0", 1, u);
  y  = getp("BV0", 2, u);
  z  = getp("BV0", 3, u);

  xp = getp("BV0", 4, u);
  yp = getp("BV0", 5, u);
  zp = getp("BV0", 6, u);
  
  rone = sqrt( (x+mu)*(x+mu) + y*y + z*z ); 		
  rtwo = sqrt( (x-1+mu)*(x-1+mu) + y*y + z*z ); 
  par[14]=x*x+y*y+2*(1-mu)/rone+2*mu/rtwo-xp*xp-yp*yp-zp*zp;
  par[15]=y;

  return 0;
}
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int bcnd () { return 0; }
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int icnd () { return 0; } 
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
int fopt() { return 0; }
/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
