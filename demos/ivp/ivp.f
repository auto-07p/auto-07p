C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   ivp :    Time integration (using Implicit Euler)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
       E=DEXP(-PAR(3)*U(1)) 
       F(1)=PAR(2)*U(1)*(1-U(1)) - U(1)*U(2) - PAR(1)*(1-E) 
       F(2)=-U(2) + PAR(4)*U(1)*U(2) 
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
C      **Set (constant) parameters
       PAR(1)=0. 
       PAR(2)=3.0 
       PAR(3)=5.0 
       PAR(4)=3.0 
C
C      **Set initial values 
       U(1)=0.3
       U(2)=0.3 
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
