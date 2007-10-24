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
   DOUBLE PRECISION, INTENT(OUT) :: F(NDIM), DFDU(NDIM,*), DFDP(NDIM,*)

   DOUBLE PRECISION x,y,z,xp,yp,zp,rl,rmu,dE,dM,rmc,dE3,dM3

   x  = U(1)        
   y  = U(2) 
   z  = U(3) 
   xp = U(4) 
   yp = U(5) 
   zp = U(6) 

   rl  = PAR(1)
   rmu = PAR(2)

   dE  = SQRT((x+rmu)**2 + y**2 + z**2)
   dM  = SQRT( (x-1+rmu)**2 + y**2 + z**2 )
   rmc = 1 - rmu
   dE3 = 1./dE**3
   dM3 = 1./dM**3

   F(1)= xp
   F(2)= yp
   F(3)= zp
   F(4)= 2*yp + x - rmc*dE3*(x+rmu) - rmu*dM3*(x-1+rmu) + rl*xp
   F(5)=-2*xp + y - rmc*dE3*y       - rmu*dM3*y         + rl*yp
   F(6)=          - rmc*dE3*z       - rmu*dM3*z         + rl*zp

 END SUBROUTINE FUNC
!---------------------------------------------------------------------- 
 SUBROUTINE STPNT(NDIM,U,PAR,T)
!---------- -----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: T
   DOUBLE PRECISION, INTENT(OUT) :: U(NDIM), PAR(*)

   DOUBLE PRECISION rl,rmu

   rl  =  0.
   rmu =  0.

   PAR(:2) = (/ rl, rmu /)

   U = (/ 0.14107D0, 0.99D0, 0D0, 0D0, 0D0, 0D0 /)

 END SUBROUTINE STPNT
!---------------------------------------------------------------------- 
 SUBROUTINE PVLS(NDIM,U,PAR)
!---------- ----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
   DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

   DOUBLE PRECISION GETP,rmu,x,y,z,xp,yp,zp,d1,d2,EU,E

   rmu = PAR(2)

   x  = U(1)
   y  = U(2)
   z  = U(3)
   xp = U(4)
   yp = U(5)
   zp = U(6)

   d1 = SQRT((x+rmu)**2 + y**2 + z**2)
   d2 = SQRT( (x-1+rmu)**2 + y**2 + z**2 )

   EU = (x**2 + y**2)/2 + (1-rmu)/d1 + rmu/d2
   E  = (xp**2 + yp**2 + zp**2)/2 - EU - rmu*(1-rmu)/2
   PAR(3) = E
   PAR(16)=y

 END SUBROUTINE PVLS
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
 SUBROUTINE BCND 
 END SUBROUTINE BCND

 SUBROUTINE ICND 
 END SUBROUTINE ICND

 SUBROUTINE FOPT 
 END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
