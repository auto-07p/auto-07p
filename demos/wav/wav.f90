!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   wav :    Periodic waves in an a nonlinear parabolic PDE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION R

       R= U(2)/(PAR(5)+U(2)) * U(1)/(1+U(1)+PAR(6)*U(1)*U(1)) 
       F(1)=-PAR(1)*( PAR(4)*R - (PAR(2)-U(1)) ) 
       F(2)=-PAR(1)*( PAR(4)*R - PAR(7)*(PAR(3)-U(2)) ) 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1) = 3.0 
       PAR(2) = 145 
       PAR(3) = 500.
       PAR(4) = 210 
       PAR(5) = 3.4 
       PAR(6) = 0.023 
       PAR(7) = 0.2 

       U(1) = 6.27662E+01   
       U(2) = 8.88308E+01

       PAR(10) = 0.05 
       PAR(15) = 1.0 
       PAR(16) = 5.0 

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
