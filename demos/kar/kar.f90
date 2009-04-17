!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   kar :       The Von Karman swirling flow
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION U1,U2,U3,U4,U5,GAMMA,ZINF

       U1=U(1)
       U2=U(2)
       U3=U(3)
       U4=U(4)
       U5=U(5)

       GAMMA=PAR(1)
       ZINF =PAR(3)

       F(1)=ZINF*U2
       F(2)=ZINF*U3
       F(3)=ZINF*( - 2*GAMMA*U4 + U2*U2 - 2*U1*U3 - U4*U4)
       F(4)=ZINF*U5
       F(5)=ZINF*(2*GAMMA*U2 + 2*U2*U4 - 2*U1*U5)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION GAMMA,FINF,ZINF

       GAMMA=1.
       FINF =0.
       ZINF =500.

       PAR(1)=GAMMA 
       PAR(2)=-FINF
       PAR(3)=ZINF

       U(1)=0.
       U(2)=0.
       U(3)=0.
       U(4)=0.
       U(5)=0.

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

      DOUBLE PRECISION GAMMA,FINF,C,A,B

       GAMMA=PAR(1)
       FINF=-PAR(2)

       C=SQRT(FINF**4 + 4*GAMMA**2)
       A=SQRT(C + FINF**2) / SQRT(2.d0)
       B=SQRT(C - FINF**2) / SQRT(2.d0)

       F(1)=U0(1)
       F(2)=U0(2)
       F(3)=U0(4)-1+GAMMA
       F(4)=(FINF+A)*U1(2) + U1(3) - GAMMA*U1(4)/A
       F(5)=A*B**2*U1(2)/GAMMA + (FINF+A)*U1(4) + U1(5)
       F(6)=U1(1)-FINF

      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
