!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   r3b :    The Restricted 3-Body Problem
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

 SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!---------- ---- 

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
   DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
   DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,*), DFDP(NDIM,*)

   DOUBLE PRECISION x,y,z,xp,yp,zp,lambda,mu,dE,dM,mc,dE3,dM3,dE5,dM5
   DOUBLE PRECISION dEx,dEy,dEz,cx,cy,cz,dx,dy,dz,ex,ey,ez,vx,vy,vz
   DOUBLE PRECISION vxp,vyp,vzp,dMx,dMy,dMz

   x  = U(1)        
   y  = U(2) 
   z  = U(3) 
   xp = U(4) 
   yp = U(5) 
   zp = U(6) 

   lambda  = PAR(1)
   mu = PAR(2)

   dE  = SQRT((x+mu)**2 + y**2 + z**2)
   dM  = SQRT( (x-1+mu)**2 + y**2 + z**2 )
   mc = 1 - mu
   dE3 = 1/dE**3
   dM3 = 1/dM**3

   F(1)= xp
   F(2)= yp
   F(3)= zp
   F(4)= 2*yp + x - mc*dE3*(x+mu) - mu*dM3*(x-1+mu) + lambda*xp
   F(5)=-2*xp + y - mc*dE3*y       - mu*dM3*y         + lambda*yp
   F(6)=          - mc*dE3*z       - mu*dM3*z         + lambda*zp

   IF(NDIM==6)RETURN

! Set up the linearized equations
   dE5 = 1/dE**5
   dM5 = 1/dM**5

   dEx = -3*(x+mu)*dE5
   dEy = -3*y*dE5
   dEz = -3*z*dE5

   dMx = -3*(x-1+mu)*dM5
   dMy = -3*y*dM5
   dMz = -3*z*dM5

   cx = 1 - mc*dE3 - mc*(x+mu)*dEx - mu*dM3 - mu*(x-1+mu)*dMx
   cy = -mc*(x+mu)*dEy - mu*(x-1+mu)*dMy
   cz = -mc*(x+mu)*dEz - mu*(x-1+mu)*dMz

   dx = -mc*y*dEx - mu*y*dMx
   dy = 1 - mc*dE3 - mc*y*dEy - mu*dM3 - mu*y*dMy
   dz = -mc*y*dEz - mu*y*dMz

   ex = -mc*z*dEx - mu*z*dMx
   ey = -mc*z*dEy - mu*z*dMy
   ez = -mc*dE3 - mc*z*dEz - mu*dM3 - mu*z*dMz

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
 SUBROUTINE STPNT(NDIM,U,PAR,T)
!---------- -----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM), PAR(*)
   DOUBLE PRECISION, INTENT(IN) :: T

   DOUBLE PRECISION lambda,mu

   lambda  =  0.
   mu =  0.

   PAR(:2) = (/ lambda, mu /)

   U = (/ 0.14107D0, 0.99D0, 0D0, 0D0, 0D0, 0D0 /)

 END SUBROUTINE STPNT

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

   DOUBLE PRECISION GETP,mu,x,y,z,xp,yp,zp,d1,d2,EU,E,realfm,imagfm,ncmplx
   DOUBLE PRECISION routh,pi
   INTEGER i,j

   mu = PAR(2)

   x  = U(1)
   y  = U(2)
   z  = U(3)
   xp = U(4)
   yp = U(5)
   zp = U(6)

   d1 = SQRT((x+mu)**2 + y**2 + z**2)
   ! PVLS is called for initialization of MPI workers, but results are not
   ! used. In that case all variables are 0.
   IF(d1==0)RETURN
   d2 = SQRT( (x-1+mu)**2 + y**2 + z**2 )

   EU = (x**2 + y**2)/2 + (1-mu)/d1 + mu/d2
   E  = (xp**2 + yp**2 + zp**2)/2 - EU - mu*(1-mu)/2
   PAR(3) = E
   IF(NDIM==12)RETURN
   PAR(16)=y

! Maximum real Floquet multiplier: PAR(4)
! If there are two real multipliers with absolute value > 1
! then PAR(4)=0.

   PAR(4) = 0
   ncmplx = 0
   DO i=1,NDIM
      imagfm = GETP('EIG',I*2,U)
      IF (imagfm == 0) THEN
         realfm = GETP('EIG',I*2-1,U)
         IF (ABS(realfm) > ABS(PAR(4))) THEN
            PAR(4) = realfm
         ENDIF
      ELSE
         ncmplx = ncmplx + 1
      ENDIF
   ENDDO
   IF (ncmplx == 0) THEN
      ! no complex multipliers mean 6 real multipliers: 1,1, two with
      ! absolute value greater than 1 and two less than one
      PAR(4) = 0
   ELSEIF (ncmplx == 4) THEN
      ! all non-trivial multipliers complex: must be 1 without rounding
      ! errors
      PAR(4) = 1
   ENDIF

!  Put purely imaginary eigenvalues in PAR(5), PAR(6) and PAR(7)

   j=1
   PAR(5) = 0
   PAR(6) = 0
   PAR(7) = 0
   pi = 4*ATAN(1d0)
   DO i=1,NDIM
      imagfm = GETP('EIG',I*2,U)
      IF (imagfm > 1d-5) THEN
         realfm = GETP('EIG',I*2-1,U)
         routh = 0.5d0*(1d0-sqrt(69d0)/9d0)
         ! above Routh's ratio we have one period for L4/L5, otherwise 3.
         IF (ABS(realfm) < EPSILON(1d0) .OR. mu < routh .OR. ABS(y) < 0.1) THEN
            PAR(4+j) = 2*pi/imagfm            
            j=j+1
         ENDIF
      ENDIF
   ENDDO

 END SUBROUTINE PVLS
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
 SUBROUTINE FOPT 
 END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
