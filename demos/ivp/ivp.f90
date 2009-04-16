!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   ivp :    Time integration (using Implicit Euler)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
! 
       E=DEXP(-PAR(3)*U(1)) 
       F(1)=PAR(2)*U(1)*(1-U(1)) - U(1)*U(2) - PAR(1)*(1-E) 
       F(2)=-U(2) + PAR(4)*U(1)*U(2) 
! 
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
! 
!      **Set (constant) parameters
       PAR(1)=0. 
       PAR(2)=3.0 
       PAR(3)=5.0 
       PAR(4)=3.0 
!
!      **Set initial values 
       U(1)=0.3
       U(2)=0.3 
! 
      RETURN 
      END 
! 
      SUBROUTINE BCND 
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
