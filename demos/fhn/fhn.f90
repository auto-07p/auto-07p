!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   fhn :        The FitzHugh - Nagumo Equation
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*), F(NDIM), ICP(*)
!
       a=PAR(1)
       b=PAR(2)
       c=PAR(3)
!
       x=U(1)
       y=U(2)
!
       F(1)= c * ( x - x**3/3 + y )
       F(2)=-( x - a + b*y ) / c
!
      RETURN
      END
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE STPNT(NDIM,U,PAR)
!     ---------- -----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*)
!
! Initialize the equation parameters
       PAR(1)=0.
       PAR(2)=0.8
       PAR(3)=3.0
!
! Initialize the solution (assuming PAR(1)=0 )
       U(1)=0.
       U(2)=0.
!
      RETURN
      END
!----------------------------------------------------------------------
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!----------------------------------------------------------------------
