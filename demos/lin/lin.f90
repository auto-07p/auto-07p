!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   lin :    A linear ODE eigenvalue problem
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!      ---------- ----

       IMPLICIT NONE
       INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
       DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
       DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
       DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

       DOUBLE PRECISION PI

        PI=4*ATAN(1.0D0)
        F(1) = U(2)
        F(2) = -( PAR(1)*PI )**2 * U(1)

       END SUBROUTINE FUNC

       SUBROUTINE STPNT(NDIM,U,PAR,T)
!      ---------- -----

       IMPLICIT NONE
       INTEGER, INTENT(IN) :: NDIM
       DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
       DOUBLE PRECISION, INTENT(IN) :: T

        PAR(1)=0.
        PAR(2)=0.
        PAR(3)=0.

        U(1)=0.0
        U(2)=0.0

       END SUBROUTINE STPNT

       SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!      ---------- ----

       IMPLICIT NONE
       INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
       DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
       DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
       DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

        FB(1)=U0(1)-PAR(2)
        FB(2)=U1(1)

       END SUBROUTINE BCND

       SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!      ---------- ----

       IMPLICIT NONE
       INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
       DOUBLE PRECISION, INTENT(IN) :: PAR(*)
       DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
       DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
       DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

        FI(1)=U(1)*U(1)-PAR(3)

       END SUBROUTINE ICND

       SUBROUTINE FOPT
       END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
