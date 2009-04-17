!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   ivp :    Time integration (using Implicit Euler)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION E

       E=EXP(-PAR(3)*U(1)) 
       F(1)=PAR(2)*U(1)*(1-U(1)) - U(1)*U(2) - PAR(1)*(1-E) 
       F(2)=-U(2) + PAR(4)*U(1)*U(2) 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

!      **Set (constant) parameters
       PAR(1)=0. 
       PAR(2)=3.0 
       PAR(3)=5.0 
       PAR(4)=3.0 

!      **Set initial values 
       U(1)=0.3
       U(2)=0.3 

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
