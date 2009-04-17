!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   pd1 :    Time integration of a scalar nonlinear parabolic PDE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)
! 
!      *Set the nonlinear term
       F(1)= PAR(1) * U(1) * ( 1. - U(1) )

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,X)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: X

      DOUBLE PRECISION pi

      pi=4*ATAN(1.d0)

!      *Set the (constant) parameter 
       PAR(1) = 1.

!      *Set the actual width of the space interval [0,PAR(11)]
       PAR(11) = 10.

!      *Set the initial data in the (scaled) interval [0,1]
       U(1) = SIN(pi*X) 

!      *Also set the space-derivative of the initial data
!      *Note the scaling by 1/PAR(11) !
       U(2) = pi * COS(pi*X)/PAR(11)

!      *Set the diffusion constant
       PAR(15) = 0.1

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

!      *Define the boundary conditions (Dirichlet, in this demo).
       FB(1)=U0(1) 
       FB(2)=U1(1) 

      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
