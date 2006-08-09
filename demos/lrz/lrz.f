C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   lor :     The Lorenz Equations
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C      
       F(1)= PAR(3) * (U(2)- U(1))
       F(2)= PAR(1)*U(1) - U(2) - U(1)*U(3)
       F(3)= U(1)*U(2) -  PAR(2)*U(3)
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
       PAR(1)=0.
       PAR(2)= 8.d0/3.d0 
       PAR(3)=10. 
C
       U(1)=0.
       U(2)=0.
       U(3)=0.
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
