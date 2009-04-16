!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   wav :    Periodic waves in an a nonlinear parabolic PDE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
! 
       R= U(2)/(PAR(5)+U(2)) * U(1)/(1+U(1)+PAR(6)*U(1)*U(1)) 
       F(1)=-PAR(1)*( PAR(4)*R - (PAR(2)-U(1)) ) 
       F(2)=-PAR(1)*( PAR(4)*R - PAR(7)*(PAR(3)-U(2)) ) 
! 
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
! 
       PAR(1) = 3.0 
       PAR(2) = 145 
       PAR(3) = 500.
       PAR(4) = 210 
       PAR(5) = 3.4 
       PAR(6) = 0.023 
       PAR(7) = 0.2 
! 
       U(1) = 6.27662E+01   
       U(2) = 8.88308E+01
! 
       PAR(10) = 0.05 
       PAR(15) = 1.0 
       PAR(16) = 5.0 
! 
      RETURN 
      END 
! 
      SUBROUTINE BCND 
      RETURN 
      END 
! 
      SUBROUTINE ICND 
      RETURN 
      END 
! 
      SUBROUTINE FOPT 
      RETURN 
      END 
! 
      SUBROUTINE PVLS
      RETURN 
      END 
