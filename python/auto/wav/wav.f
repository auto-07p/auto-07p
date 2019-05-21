C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   wav :    Periodic waves in an a nonlinear parabolic PDE
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
       R= U(2)/(PAR(5)+U(2)) * U(1)/(1+U(1)+PAR(6)*U(1)*U(1)) 
       F(1)=-PAR(1)*( PAR(4)*R - (PAR(2)-U(1)) ) 
       F(2)=-PAR(1)*( PAR(4)*R - PAR(7)*(PAR(3)-U(2)) ) 
C 
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
C 
       PAR(1) = 3.0 
       PAR(2) = 145 
       PAR(3) = 539.23 
       PAR(4) = 210 
       PAR(5) = 3.4 
       PAR(6) = 0.023 
       PAR(7) = 0.2 
C 
       U(1) = 59.702 
       U(2) = 112.752 
C 
       PAR(10) = 0.05 
       PAR(15) = 1.0 
       PAR(16) = 5.0 
C 
      RETURN 
      END 
C 
      SUBROUTINE BCND 
      RETURN 
      END 
C 
      SUBROUTINE ICND 
      RETURN 
      END 
C 
      SUBROUTINE FOPT 
      RETURN 
      END 
C 
      SUBROUTINE PVLS
      RETURN 
      END 
