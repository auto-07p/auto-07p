!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   lin :    A problem with a "vertical" Hopf bifurcation
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!      ---------- ----

       IMPLICIT NONE
       INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
       DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
       DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
       DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

        F(1) = PAR(1)*U(1) - U(2)
        F(2) = U(1)*(1-U(1))

       END SUBROUTINE FUNC

       SUBROUTINE STPNT(NDIM,U,PAR,T)
!      ---------- -----

       IMPLICIT NONE
       INTEGER, INTENT(IN) :: NDIM
       DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
       DOUBLE PRECISION, INTENT(IN) :: T

        PAR(1)=-1.

        U(1)=0.0
        U(2)=0.0

       END SUBROUTINE STPNT

       SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
       RETURN
       END

       SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
       RETURN
       END

       SUBROUTINE FOPT
       END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
