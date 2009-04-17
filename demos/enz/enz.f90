!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   enz :    A two-cell, one-substrate enzyme model 
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION R,S,S1,S2,S0,RM,RH,RK

       R(S)=S/(1+S+RK*S**2)
       S1=U(1)
       S2=U(2)
       S0=PAR(1)
       RM=PAR(2)
       RH=PAR(3)
       RK=PAR(4)

       F(1)=(S0   -S1) + (S2-S1) - RH * R(S1)
       F(2)=(S0+RM-S2) + (S1-S2) - RH * R(S2)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=0.
       PAR(2)=0.
       PAR(3)=100.
       PAR(4)=1.

       U(1)=0.0
       U(2)=0.0

      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
