C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   nep :    A nonlinear ODE eigenvalue problem
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
       F(1) =  U(2) 
       F(2) = -PAR(2)*U(1) 
       F(3) = -U(2)**2/2 - PAR(2)
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
C
       U(1)=0.0 
       U(2)=0.0 
       U(3)=0.0 
C 
      RETURN 
      END 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
C 
       FB(1)=U0(1) 
       FB(2)=U1(1) 
       FB(3)=U0(3) - PAR(1)
       FB(4)=U1(3) + PAR(1)
C 
      RETURN 
      END 
C---------------------------------------------------------------------- 
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
C---------------------------------------------------------------------- 
