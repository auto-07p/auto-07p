C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   int :    An ODE with boundary and integral constraints
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*) 
C 
       E=EXP(U(1)) 
       F(1)=U(2) 
       F(2)=-PAR(1)*E 
C 
      IF(IJAC.EQ.0)RETURN 
C 
       DFDU(1,1)=0.0 
       DFDU(1,2)=1 
       DFDU(2,1)=-PAR(1)*E 
       DFDU(2,2)=0.0 
C 
      IF(IJAC.EQ.1)RETURN 
C 
C      *Parameter derivatives
       DFDP(1,1)=0.0 
       DFDP(2,1)=-E 
C 
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
C 
       PAR(1)=0
       PAR(2)=0
       PAR(3)=0
C
       U(1)=0.0 
       U(2)=0.0 
C 
      RETURN 
      END 
C 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC),DBC(NBC,*) 
C 
       FB(1)=U0(1)-U1(1)-PAR(2) 
C 
      IF(IJAC.EQ.0)RETURN 
C 
       DBC(1,1)=1.0 
       DBC(1,2)=0.0 
C 
       DBC(1,3)=-1.0 
       DBC(1,4)=0.0 
C 
      IF(IJAC.EQ.1)RETURN 
C 
C      *Parameter derivatives
       DBC(1,5)=0.0 
       DBC(1,6)=-1.0 
       DBC(1,7)=0.0 
C 
      RETURN 
      END 
C 
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM) 
      DIMENSION FI(NINT),DINT(NINT,*),ICP(*),PAR(*) 
C 
       FI(1)=U(1)-PAR(3) 
C 
      IF(IJAC.EQ.0)RETURN 
C 
       DINT(1,1)=1.0 
       DINT(1,2)=0.0 
C 
      IF(IJAC.EQ.1)RETURN 
C 
C      *Parameter derivatives
       DINT(1,3)=0.0 
       DINT(1,4)=0.0 
       DINT(1,5)=-1.0 
C 
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
