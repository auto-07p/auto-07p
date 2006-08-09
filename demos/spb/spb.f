C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   spb :    A singularly perturbed BVP
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
       EPS=PAR(2)
       RL =PAR(3)
C 
       F(1)=U2 
       F(2)=RL * ( U1*(U1**2-1)*U2 + U1 ) / EPS 
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
       GAMMA=1.2
       EPS=0.1
       RL=0.
C
       PAR(1)=GAMMA
       PAR(2)=EPS
       PAR(3)=RL
C
       S=GAMMA-1.5
       U(1)=1.5 + S*X
       U(2)=S
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
       FB(1)=U0(1)-1.5 
       FB(2)=U1(1)-PAR(1) 
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
