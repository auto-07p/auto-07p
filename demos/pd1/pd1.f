C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   pd1 :    Time integration of a scalar nonlinear parabolic PDE
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
C      *Set the nonlinear term
       F(1)= PAR(1) * U(1) * ( 1. - U(1) )
C 
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR,X) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
      pi=4*DATAN(1.d0)
C
C      *Set the (constant) parameter 
       PAR(1) = 1.
C
C      *Set the actual width of the space interval [0,PAR(11)]
       PAR(11) = 10.
C
C      *Set the initial data in the (scaled) interval [0,1]
       U(1) = DSIN(pi*X) 
C
C      *Also set the space-derivative of the initial data
C      *Note the scaling by 1/PAR(11) !
       U(2) = pi * DCOS(pi*X)/PAR(11)
C
C      *Set the diffusion constant
       PAR(15) = 0.1
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
C      *Define the boundary conditions (Dirichlet, in this demo).
       FB(1)=U0(1) 
       FB(2)=U1(1) 
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
