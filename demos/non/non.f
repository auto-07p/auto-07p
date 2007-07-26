C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   non :    A non-autonomous boundary value problem 
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       U1=U(1)
       U2=U(2)
       X =U(3)
       P=PAR(1)
C 
       F(1)=U2 
       F(2)=-P*DEXP(X**3*U1) 
       F(3)=1.d0
C 
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR,X) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
C
       PAR(1)=0.d0 
       U(1)=0.d0
       U(2)=0.d0 
       U(3)=X
C 
      RETURN 
      END 
C 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
C 
       FB(1)=U0(1) 
       FB(2)=U1(1) 
       FB(3)=U0(3) 
C 
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
