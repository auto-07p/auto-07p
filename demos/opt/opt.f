C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   opt :    A model algebraic optimization problem
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),ICP(*),PAR(*) 
      DIMENSION F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*) 
C 
       X1=U(1) 
       X2=PAR(1) 
       X3=PAR(2) 
       X4=PAR(3) 
       X5=PAR(4) 
C 
       F(1)=X1*X1 + X2*X2 + X3*X3 + X4*X4 + X5*X5 - 1 
C 
      IF(IJAC.EQ.0)RETURN 
C 
       DFDU(1,1)=2*X1 
C 
      IF(IJAC.EQ.1)RETURN 
C
C      *Parameter derivatives
       DFDP(1,1)=2*X2 
       DFDP(1,2)=2*X3 
       DFDP(1,3)=2*X4 
       DFDP(1,4)=2*X5 
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
       X1=1.0 
       X2=0.0 
       X3=0.0 
       X4=0.0 
       X5=0.0 
C 
       U(1)=X1 
C 
       PAR(1)=X2 
       PAR(2)=X3 
       PAR(3)=X4 
       PAR(4)=X5 
C 
      RETURN 
      END 
C 
      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),ICP(*),PAR(*),DFDU(NDIM),DFDP(*) 
C 
       X1=U(1) 
       X2=PAR(1) 
       X3=PAR(2) 
       X4=PAR(3) 
       X5=PAR(4) 
C 
       FS=X1 + X2 + X3 + X4 + X5 
C 
      IF(IJAC.EQ.0)RETURN 
C 
       DFDU(1)=1.0 
C 
      IF(IJAC.EQ.1)RETURN 
C
C      *Parameter derivatives
       DFDP(1)=1.0 
       DFDP(2)=1.0 
       DFDP(3)=1.0 
       DFDP(4)=1.0 
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
      SUBROUTINE PVLS
      RETURN 
      END 
