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

   DOUBLE PRECISION x,y,z,xp,yp,zp,rmu,T,dE,dM,rmc,dE3,dM3

   x  = U(1)        
   y  = U(2) 
   z  = U(3) 
   xp = U(4) 
   yp = U(5) 
   zp = U(6) 

   rmu = PAR(2)
   T   = PAR(11)

   dE  = SQRT((x+rmu)**2 + y**2 + z**2)
   dM  = SQRT( (x-1+rmu)**2 + y**2 + z**2 )
   rmc = 1 - rmu
   dE3 = 1./dE**3
   dM3 = 1./dM**3

   F(1)= xp
   F(2)= yp
   F(3)= zp
   F(4)= 2*yp + x - rmc*dE3*(x+rmu) - rmu*dM3*(x-1+rmu) 
   F(5)=-2*xp + y - rmc*dE3*y       - rmu*dM3*y        
   F(6)=          - rmc*dE3*z       - rmu*dM3*z       

   F = T*F

 END SUBROUTINE FUNC
!---------------------------------------------------------------------- 
 SUBROUTINE STPNT
 END SUBROUTINE STPNT
!---------------------------------------------------------------------- 
 SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!---------- ----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
   DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
   DOUBLE PRECISION, INTENT(OUT) :: FB(NBC), DBC(NBC,*)

   DOUBLE PRECISION eps,x,y,z,xp,yp,zp,rmu,dE,dM,U,E
   INTEGER m

   eps=PAR(6)
!  PAR(25:30) are USTART and PAR(31:36) are VSTART
   m=MIN(NBC,NDIM)
   FB(1:m) =  U0(1:m) - ( PAR(25:25+m-1) + eps*PAR(31:31+m-1) )

   FB(NDIM+1:NDIM+3) = U1(1:3) - PAR(21:23)

   x  = U1(1)
   y  = U1(2)
   z  = U1(3)
   xp = U1(4)
   yp = U1(5)
   zp = U1(6)

   rmu = PAR(2)

   dE = SQRT((x+rmu)**2 + y**2 + z**2)
   dM = SQRT( (x-1+rmu)**2 + y**2 + z**2 )

   U = (x**2 + y**2)/2 + (1-rmu)/dE + rmu/dM
   E = (xp**2 + yp**2 + zp**2)/2 - U - rmu*(1-rmu)/2
   FB(NDIM+4) = PAR(3) - E

 END SUBROUTINE BCND
!---------------------------------------------------------------------- 
 SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!---------- ----

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
   DOUBLE PRECISION, INTENT(IN) :: PAR(*)
   DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
   DOUBLE PRECISION, INTENT(OUT) :: FI(NINT), DINT(NINT,*)

   DOUBLE PRECISION FF(NDIM),DFDU(1),DFDP(1)

   CALL FUNC(NDIM,U,ICP,PAR,0,FF,DFDU,DFDP)
   FI(1)=SQRT(FF(1)**2 + FF(2)**2 + FF(3)**2 ) - PAR(12)

 END SUBROUTINE ICND
!---------------------------------------------------------------------- 
 SUBROUTINE FOPT 
 END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
 SUBROUTINE PVLS
 END SUBROUTINE PVLS
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
