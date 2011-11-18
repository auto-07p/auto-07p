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

      DOUBLE PRECISION R,s,s1,s2,s0,mu,rho,kappa

       R(s)=s/(1+s+kappa*s**2)
       s1=U(1)
       s2=U(2)
       s0=PAR(1)
       mu=PAR(2)
       rho=PAR(3)
       kappa=PAR(4)

       F(1)=(s0   -s1) + (s2-s1) - rho * R(s1)
       F(2)=(s0+mu-s2) + (s1-s2) - rho * R(s2)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=0
       PAR(2)=0
       PAR(3)=100
       PAR(4)=1

       U(1)=0
       U(2)=0

      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
