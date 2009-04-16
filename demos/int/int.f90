!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   int :    An ODE with boundary and integral constraints
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*) 
! 
       E=EXP(U(1)) 
       F(1)=U(2) 
       F(2)=-PAR(1)*E 
! 
      IF(IJAC.EQ.0)RETURN 
! 
       DFDU(1,1)=0.0 
       DFDU(1,2)=1 
       DFDU(2,1)=-PAR(1)*E 
       DFDU(2,2)=0.0 
! 
      IF(IJAC.EQ.1)RETURN 
! 
!      *Parameter derivatives
       DFDP(1,1)=0.0 
       DFDP(2,1)=-E 
! 
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
! 
       PAR(1)=0
       PAR(2)=0
       PAR(3)=0
!
       U(1)=0.0 
       U(2)=0.0 
! 
      RETURN 
      END 
! 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC),DBC(NBC,*) 
! 
       FB(1)=U0(1)-U1(1)-PAR(2) 
! 
      IF(IJAC.EQ.0)RETURN 
! 
       DBC(1,1)=1.0 
       DBC(1,2)=0.0 
! 
       DBC(1,3)=-1.0 
       DBC(1,4)=0.0 
! 
      IF(IJAC.EQ.1)RETURN 
! 
!      *Parameter derivatives
       DBC(1,5)=0.0 
       DBC(1,6)=-1.0 
       DBC(1,7)=0.0 
! 
      RETURN 
      END 
! 
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM) 
      DIMENSION FI(NINT),DINT(NINT,*),ICP(*),PAR(*) 
! 
       FI(1)=U(1)-PAR(3) 
! 
      IF(IJAC.EQ.0)RETURN 
! 
       DINT(1,1)=1.0 
       DINT(1,2)=0.0 
! 
      IF(IJAC.EQ.1)RETURN 
! 
!      *Parameter derivatives
       DINT(1,3)=0.0 
       DINT(1,4)=0.0 
       DINT(1,5)=-1.0 
! 
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
