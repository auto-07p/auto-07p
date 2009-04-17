!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   ezp :    Complex bifurcation in a boundary value problem
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      COMPLEX(KIND(1.0D0)), INTENT(IN) :: U(NDIM)
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      COMPLEX(KIND(1.0D0)), INTENT(OUT) :: F(NDIM)
      COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      COMPLEX(KIND(1.0D0)) :: U1, U2, RL, E

       U1=U(1)
       U2=U(2)

       RL=PAR(1)

       E=CDEXP(U1)
       F(1)=U2
       F(2)=-RL*E

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: U(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       U(1)=0
       U(2)=0

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      COMPLEX(KIND(1.0D0)), INTENT(IN) :: U0(NDIM), U1(NDIM)
      COMPLEX(KIND(1.0D0)), INTENT(OUT) :: F(NBC)
      COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: DBC(NBC,*)

       F(1)=U0(1)
       F(2)=U1(1)

      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
