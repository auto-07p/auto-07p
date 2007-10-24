!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! r3b : The Restricted 3-Body Problem
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

 SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!---------- ---- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
   DOUBLE PRECISION, INTENT(OUT) :: F(NDIM), DFDU(NDIM,*), DFDP(NDIM,*)

   DOUBLE PRECISION x,y,z,xp,yp,zp
   DOUBLE PRECISION rl,rmu,dE,dM,rmc,dE3,dM3,dE5,dM5
   DOUBLE PRECISION dEx,dEy,dEz,cx,cy,cz,dx,dy,dz,ex,ey,ez,vx,vy,vz
   DOUBLE PRECISION vxp,vyp,vzp,dMx,dMy,dMz

! Define the RHS of the dynamical system
   x  = U(1)
   y  = U(2)
   z  = U(3)
   xp = U(4)
   yp = U(5)
   zp = U(6)

   rl  = PAR(1)
   rmu = PAR(2)

   dE = SQRT((x+rmu)**2 + y**2 + z**2)
   dM = SQRT( (x-1+rmu)**2 + y**2 + z**2 )
   rmc = 1 - rmu
   dE3 = 1/dE**3
   dM3 = 1/dM**3
   dE5 = 1/dE**5
   dM5 = 1/dM**5

   F(1) =  xp
   F(2) =  yp
   F(3) =  zp
   F(4) =  2*yp + x - rmc*dE3*(x+rmu) - rmu*dM3*(x-1+rmu)
   F(5) = -2*xp + y - rmc*dE3*y - rmu*dM3*y
   F(6) = -rmc*dE3*z - rmu*dM3*z

   F(4:6) = F(4:6) + rl*F(1:3)

! Set up the linearized equations
   dEx = -3*(x+rmu)*dE5
   dEy = -3*y*dE5
   dEz = -3*z*dE5

   dMx = -3*(x-1+rmu)*dM5
   dMy = -3*y*dM5
   dMz = -3*z*dM5

   cx = 1 - rmc*dE3 - rmc*(x+rmu)*dEx - rmu*dM3 - rmu*(x-1+rmu)*dMx
   cy = -rmc*(x+rmu)*dEy - rmu*(x-1+rmu)*dMy
   cz = -rmc*(x+rmu)*dEz - rmu*(x-1+rmu)*dMz

   dx = -rmc*y*dEx - rmu*y*dMx
   dy = 1 - rmc*dE3 - rmc*y*dEy - rmu*dM3 - rmu*y*dMy
   dz = -rmc*y*dEz - rmu*y*dMz

   ex = -rmc*z*dEx - rmu*z*dMx
   ey = -rmc*z*dEy - rmu*z*dMy
   ez = -rmc*dE3 - rmc*z*dEz - rmu*dM3 - rmu*z*dMz

   vx = U(7)       
   vy = U(8)       
   vz = U(9)
   vxp= U(10)       
   vyp= U(11)      
   vzp= U(12)

   F(7) = vxp
   F(8) = vyp
   F(9) = vzp
   F(10)= cx*vx + cy*vy + cz*vz + 2*vyp
   F(11)= dx*vx + dy*vy + dz*vz - 2*vxp
   F(12)= ex*vx + ey*vy + ez*vz

! Scale
   F = F * PAR(11)

 END SUBROUTINE FUNC
!---------------------------------------------------------------------- 

 SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!---------- ----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
   DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
   DOUBLE PRECISION, INTENT(OUT) :: FB(NBC), DBC(NBC,*)

   DOUBLE PRECISION fm

! Periodicity boundary conditions
   FB(1:6) = U0(1:6) - U1(1:6)

! Floquet eigenvalue/vector boundary relation
   fm = PAR(4)
   FB(7:12) = U1(7:12) - fm*U0(7:12)

 END SUBROUTINE BCND
!---------------------------------------------------------------------- 

 SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!---------- ----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
   DOUBLE PRECISION, INTENT(IN) :: PAR(*)
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
   DOUBLE PRECISION, INTENT(OUT) :: FI(NINT), DINT(NINT,*)

! Integral phase condition
   FI(1) = DOT_PRODUCT(U(1:6),UPOLD(1:6))

! Integral Floquet eigenfunction normalization
   FI(2) = -PAR(5)+DOT_PRODUCT(U(7:12),U(7:12))

 END SUBROUTINE ICND
!---------------------------------------------------------------------- 

 SUBROUTINE PVLS(NDIM,U,PAR)
!---------- ----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
   DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

   DOUBLE PRECISION GETP,rmu,x,y,z,xp,yp,zp,d1,d2,PE,En

   rmu = PAR(2)

   x = GETP("BV0", 1, U)
   y = GETP("BV0", 2, U)
   z = GETP("BV0", 3, U)
   xp= GETP("BV0", 4, U)
   yp= GETP("BV0", 5, U)
   zp= GETP("BV0", 6, U)

   d1 = SQRT((x+rmu)**2 + y**2 + z**2)
   d2 = SQRT( (x-1+rmu)**2 + y**2 + z**2 )

   PE = (x**2 + y**2)/2 + (1-rmu)/d1 + rmu/d2
   En = (xp**2 + yp**2 + zp**2)/2 - PE - rmu*(1-rmu)/2
   PAR(3) = En 

 END SUBROUTINE PVLS
!---------------------------------------------------------------------- 
 SUBROUTINE STPNT
 END SUBROUTINE STPNT
!---------------------------------------------------------------------- 
 SUBROUTINE FOPT 
 END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
