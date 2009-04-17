!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   man :  Stable manifold of the origen in the Lorenz model 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION Period

       Period=PAR(11)

       F(1)= Period * (PAR(3) * (U(2)- U(1)))
       F(2)= Period * (PAR(1)*U(1) - U(2) - U(1)*U(3))
       F(3)= Period * (U(1)*U(2) -  PAR(2)*U(3))

      END SUBROUTINE FUNC
!---------------------------------------------------------------------- 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION rad,h,Period,RLength,pi,theta,cs,sn,V1(3),V2(3)

       PAR(1)=28.
       PAR(2)=8.d0/3.d0 
       PAR(3)=10. 

       rad=5.0
       h=0.1
       Period=1e-5
       RLength=0.

       PAR(4)=rad
       PAR(5)=h

       CALL EIGV(NDIM,PAR,V1,V2)

! Set initial approximate solution (for small Period)
       pi=4*ATAN(1.d0)
       theta=2*pi*h
       cs=COS(theta)
       sn=SIN(theta)

       U(1)= rad * ( cs*V1(1) + sn*V2(1) )
       U(2)= rad * ( cs*V1(2) + sn*V2(2) )
       U(3)= rad * ( cs*V1(3) + sn*V2(3) )

       PAR(6) = U(1)
       PAR(7) = U(2)
       PAR(8) = U(3)
       PAR(9) = SQRT(U(1)**2 + U(2)**2 + U(3)**2)      
       PAR(11)=Period   
       PAR(12)=RLength

      END SUBROUTINE STPNT
!---------------------------------------------------------------------- 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

      DOUBLE PRECISION V1(3),V2(3),pi,rad,h,theta,cs,sn

       CALL EIGV(NDIM,PAR,V1,V2)

       pi=4*ATAN(1.d0)
       rad=PAR(4)
       h=PAR(5)
       theta=2*pi*h
       cs=COS(theta)
       sn=SIN(theta)
! At time=0
       FB(1)= U0(1) - rad * ( cs*V1(1) + sn*V2(1) )
       FB(2)= U0(2) - rad * ( cs*V1(2) + sn*V2(2) )
       FB(3)= U0(3) - rad * ( cs*V1(3) + sn*V2(3) )
! At time=1
       FB(4)= U1(1) - PAR(6)
       FB(5)= U1(2) - PAR(7)
       FB(6)= U1(3) - PAR(8)
       FB(7)= SQRT(U1(1)**2 + U1(2)**2 + U1(3)**2) - PAR(9)

      END SUBROUTINE BCND
!---------------------------------------------------------------------- 
      SUBROUTINE EIGV(NDIM,PAR,V1,V2) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: V1(NDIM),V2(NDIM)
      LOGICAL, SAVE :: ifirst = .TRUE.

      DOUBLE PRECISION e,ss

! Stable eigenvalue/vector 1
       V1(1)= 0.
       V1(2)= 0.
       V1(3)= 1./2.66667

! Stable eigenvalue/vector 2
       e=-(11+SQRT(1201.d0))/2
       V2(1)=1+e
       V2(2)=PAR(1)
       V2(3)= 0.
       ss=SQRT(V2(1)**2+V2(2)**2)
       V2(1)=V2(1)/(ss*22.8277)
       V2(2)=V2(2)/(ss*22.8277)
       IF(ifirst)THEN
         WRITE(10,*)V2(1),V2(2)
         ifirst=.FALSE.
       ENDIF

      END SUBROUTINE EIGV
!---------------------------------------------------------------------- 
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

      DOUBLE PRECISION FF(3),DFDU(1),DFDP(1)

       CALL FUNC(NDIM,U,ICP,PAR,0,FF,DFDU,DFDP) 
       FI(1)=SQRT(FF(1)**2 + FF(2)**2 + FF(3)**2 ) - PAR(12) 

      END SUBROUTINE ICND

      SUBROUTINE PVLS
      END SUBROUTINE PVLS

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT
