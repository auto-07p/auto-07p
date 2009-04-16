!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   pd1 :    Time integration of a scalar nonlinear parabolic PDE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
! 
!      *Set the nonlinear term
       F(1)= PAR(1)*U(1)*( 1. - U(1) ) - U(1)*U(2)
       F(2)= -U(2) + U(1)*U(2)
! 
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR,X) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
      pi=4*DATAN(1.d0)
!
!      *Set the (constant) parameter 
       PAR(1) = 12.
!
!      *Set the actual width of the space interval [0,PAR(11)]
       PAR(11) = 1.
!
!      *Set the initial data in the (scaled) interval [0,1]
       U(1) = DSIN(pi*X) 
       U(2) = 1. 
!
!      *Also set the space-derivative of the initial data
!      *Note the scaling by 1/PAR(11) !
       U(3) = pi * DCOS(pi*X) /PAR(11)
       U(4) = 0. / PAR(11)
!
!      *Set the diffusion constants
       PAR(15) = 1.
       PAR(16) = 1.
! 
      RETURN 
      END 
! 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
! 
!      *Define the boundary conditions.
       FB(1)=U0(1) 
       FB(2)=U0(2)-1. 
       FB(3)=U1(1) 
       FB(4)=U1(2)-1. 
! 
      RETURN 
      END 
! 
      SUBROUTINE ICND 
      RETURN 
      END 
! 
      SUBROUTINE FOPT 
      RETURN 
      END 
! 
      SUBROUTINE PVLS
      RETURN 
      END 
