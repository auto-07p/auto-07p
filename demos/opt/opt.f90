!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   opt :    A model algebraic optimization problem
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),ICP(*),PAR(*) 
      DIMENSION F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*) 
! 
       X1=U(1) 
       X2=PAR(1) 
       X3=PAR(2) 
       X4=PAR(3) 
       X5=PAR(4) 
! 
       F(1)=X1*X1 + X2*X2 + X3*X3 + X4*X4 + X5*X5 - 1 
! 
      IF(IJAC.EQ.0)RETURN 
! 
       DFDU(1,1)=2*X1 
! 
      IF(IJAC.EQ.1)RETURN 
!
!      *Parameter derivatives
       DFDP(1,1)=2*X2 
       DFDP(1,2)=2*X3 
       DFDP(1,3)=2*X4 
       DFDP(1,4)=2*X5 
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
       X1=1.0 
       X2=0.0 
       X3=0.0 
       X4=0.0 
       X5=0.0 
! 
       U(1)=X1 
! 
       PAR(1)=X2 
       PAR(2)=X3 
       PAR(3)=X4 
       PAR(4)=X5 
! 
      RETURN 
      END 
! 
      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),ICP(*),PAR(*),DFDU(NDIM),DFDP(*) 
! 
       X1=U(1) 
       X2=PAR(1) 
       X3=PAR(2) 
       X4=PAR(3) 
       X5=PAR(4) 
! 
       FS=X1 + X2 + X3 + X4 + X5 
! 
      IF(IJAC.EQ.0)RETURN 
! 
       DFDU(1)=1.0 
! 
      IF(IJAC.EQ.1)RETURN 
!
!      *Parameter derivatives
       DFDP(1)=1.0 
       DFDP(2)=1.0 
       DFDP(3)=1.0 
       DFDP(4)=1.0 
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
      SUBROUTINE PVLS
      RETURN 
      END 
