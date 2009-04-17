!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   fhn :        The FitzHugh - Nagumo Equation
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION a,b,c,x,y

       a=PAR(1)
       b=PAR(2)
       c=PAR(3)

       x=U(1)
       y=U(2)

       F(1)= c * ( x - x**3/3 + y )
       F(2)=-( x - a + b*y ) / c

      END SUBROUTINE FUNC
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T
!
! Initialize the equation parameters
       PAR(1)=0.
       PAR(2)=0.8
       PAR(3)=3.0

! Initialize the solution (assuming PAR(1)=0 )
       U(1)=0.
       U(2)=0.

      END SUBROUTINE STPNT
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
!----------------------------------------------------------------------
!----------------------------------------------------------------------
