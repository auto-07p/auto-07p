!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   abc :            The A --> B --> C reaction
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
! 
       X1=U(1)
       X2=U(2)
       X3=U(3)
!
       D=PAR(1)
       ALPHA=PAR(2)
       BETA=PAR(3)
       B=PAR(4)
       S=PAR(5)
!
       E=DEXP(X3)
       X1C=1-X1
!
       F(1)=-X1 + D*X1C*E
       F(2)=-X2 + D*E*(X1C - S*X2)
       F(3)=-X3 - BETA*X3 + D*B*E*(X1C + ALPHA*S*X2)
! 
      RETURN 
      END 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
!
       PAR(1)=0.0
       PAR(2)=1.0
       PAR(3)=1.55
       PAR(4)=8.
       PAR(5)=0.04
!
       U(1)=0.
       U(2)=0.
       U(3)=0.
! 
      RETURN 
      END 
!---------------------------------------------------------------------- 
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
