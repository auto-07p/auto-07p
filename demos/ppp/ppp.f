C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   ppp :    A continuous dynamical system with period doubling
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
      F(1)= U(1)*(1-U(1)) - PAR(4)*U(1)*U(2) 
      F(2)=-PAR(2)*U(2)   + PAR(4)*U(1)*U(2) - PAR(5)*U(2)*U(3) 
     *                    - PAR(1)*(1-EXP(-PAR(6)*U(2))) 
      F(3)=-PAR(3)*U(3)   + PAR(5)*U(2)*U(3) 
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
      U(1)=1.0 
      U(2)=0.0 
      U(3)=0.0 
C 
      PAR(1)=0.0 
      PAR(2)=0.25 
      PAR(3)=0.5 
      PAR(4)=3.0 
      PAR(5)=3.0 
      PAR(6)=5.0 
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
