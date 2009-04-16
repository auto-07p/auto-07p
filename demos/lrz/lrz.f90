!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   lor :     The Lorenz Equations
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!      
       F(1)= PAR(3) * (U(2)- U(1))
       F(2)= PAR(1)*U(1) - U(2) - U(1)*U(3)
       F(3)= U(1)*U(2) -  PAR(2)*U(3)
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
       PAR(1)=0.
       PAR(2)= 8.d0/3.d0 
       PAR(3)=10. 
!
       U(1)=0.
       U(2)=0.
       U(3)=0.
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
