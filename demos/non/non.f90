!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   non :    A non-autonomous boundary value problem 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
       U1=U(1)
       U2=U(2)
       X =U(3)
       P=PAR(1)
! 
       F(1)=U2 
       F(2)=-P*DEXP(X**3*U1) 
       F(3)=1.d0
! 
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR,X) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
!
       PAR(1)=0.d0 
       U(1)=0.d0
       U(2)=0.d0 
       U(3)=X
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
       FB(1)=U0(1) 
       FB(2)=U1(1) 
       FB(3)=U0(3) 
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
