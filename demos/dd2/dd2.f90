!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   dd2 :    Basic computations for discrete dynamical systems
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
      DIMENSION F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*) 
! 
       F(1)=PAR(1)*U(1)*(1-U(1)) - PAR(2)*U(1)*U(2) 
       F(2)=(1-PAR(3))*U(2) + PAR(2)*U(1)*U(2) 
! 
      IF(IJAC.EQ.0)RETURN 
! 
       DFDU(1,1)=PAR(1)*(1-2*U(1))-PAR(2)*U(2) 
       DFDU(1,2)=-PAR(2)*U(1) 
       DFDU(2,1)=PAR(2)*U(2) 
       DFDU(2,2)=1-PAR(3) + PAR(2)*U(1) 
! 
      IF(IJAC.EQ.1)RETURN 
! 
       DFDP(1,1)=U(1)*(1-U(1)) 
       DFDP(2,1)=0.0 
       DFDP(1,2)=-U(1)*U(2) 
       DFDP(2,2)= U(1)*U(2) 
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
       PAR(1)=0.0 
       PAR(2)=0.2 
       PAR(3)=0.1 
! 
       U(1)=0.0 
       U(2)=0.0 
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
