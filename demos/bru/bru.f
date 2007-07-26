C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   bru :    Time integration of a scalar nonlinear parabolic PDE
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
        X=U(1)
        Y=U(2)
        A=PAR(1)
        B=PAR(2)
C
C      *Set the nonlinear term
        F(1)= X**2*Y - (B+1)*X + A
        F(2)=-X**2*Y + B*X
C 
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR,Z) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
      pi=4*DATAN(1.d0)
C
C      *Set the (constant) parameters
       A  = 2.
       B  = 5.45
       Dx = 0.008
       Dy = 0.004
       RL = 0.75
C
       PAR(1)=A
       PAR(2)=B
       PAR(3)=RL
C
C      *Set the actual width of the space interval [0,PAR(11)]
       PAR(11) = 1.
C
C      *Set the initial data in the (scaled) interval [0,1]
       U(1) = A   - 0.5*DSIN(pi*Z)
       U(2) = B/A + 0.7*DSIN(pi*Z)
C
C      *Also set the space-derivative of the initial data
C      *Note the scaling by PAR(11)
       U(3) = - 0.5*pi*DCOS(pi*Z)/PAR(11)
       U(4) =   0.7*pi*DCOS(pi*Z)/PAR(11)
C
C      *Set the diffusion constants
       PAR(15) = Dx/RL**2
       PAR(16) = Dy/RL**2
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
C      *Define the boundary conditions (Dirichlet, in this example).
       FB(1)=U0(1)-PAR(1) 
       FB(2)=U0(2)-PAR(2)/PAR(1) 
       FB(3)=U1(1)-PAR(1) 
       FB(4)=U1(2)-PAR(2)/PAR(1) 
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
