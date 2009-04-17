!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   ppp :    A continuous dynamical system with period doubling
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      F(1)= U(1)*(1-U(1)) - PAR(4)*U(1)*U(2) 
      F(2)=-PAR(2)*U(2)   + PAR(4)*U(1)*U(2) - PAR(5)*U(2)*U(3) &
                          - PAR(1)*(1-EXP(-PAR(6)*U(2))) 
      F(3)=-PAR(3)*U(3)   + PAR(5)*U(2)*U(3) 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      U(1)=1.0 
      U(2)=0.0 
      U(3)=0.0 

      PAR(1)=0.0 
      PAR(2)=0.25 
      PAR(3)=0.5 
      PAR(4)=3.0 
      PAR(5)=3.0 
      PAR(6)=5.0 

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
