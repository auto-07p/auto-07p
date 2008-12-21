!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   ph1 :    A one-cell, two-substrate enzyme model (also plp)
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM,ICP(*),IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,*),DFDP(NDIM,*)

      DOUBLE PRECISION S,A,S0,A0,Al,RH,RK,T,D,R

       S=U(1)
       A=U(2)

       S0=PAR(1)
       A0=PAR(2)
       AL=PAR(3)
       RH=PAR(4)
       RK=PAR(5)
       T =1d0
       IF(ICP(1)==11)THEN
          IF(ICP(2)==9)THEN
             ! only use when doing the phase shift
             T =PAR(11)
          ENDIF
       ENDIF

       D=1+S+RK*S**2
       R=S*A/D

       F(1)=T*(   (S0-S) - RH*R)
       F(2)=T*(AL*(A0-A) - RH*R)

      END SUBROUTINE FUNC

!----------------------------------------------------------------------
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*),U0(NDIM),U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

       FB(1)=U0(1) - U1(1)
       FB(2)=U0(2) - U1(2)

      END SUBROUTINE BCND
!----------------------------------------------------------------------
      SUBROUTINE PVLS(NDIM,U,PAR)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM,*)
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      DOUBLE PRECISION, EXTERNAL :: GETP

       PAR(9)=GETP('BV0',1,U)

      END SUBROUTINE PVLS
!----------------------------------------------------------------------
      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=110.
       PAR(2)=500.
       PAR(3)=0.2
       PAR(4)=2.021628
       PAR(5)=0.1

       U(1)=4.555974E+01
       U(2)=1.777987E+02

      END SUBROUTINE STPNT

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT
!----------------------------------------------------------------------
!----------------------------------------------------------------------
