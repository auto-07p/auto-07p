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
       F(1)= PAR(1)*U(1)*( 1. - U(1) ) - U(1)*U(2)
       F(2)= -U(2) + U(1)*U(2)

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
       PAR(1) = 12.

!      *Set the actual width of the space interval [0,PAR(11)]
       PAR(11) = 1.

!      *Set the initial data in the (scaled) interval [0,1]
       U(1) = SIN(pi*X) 
       U(2) = 1. 

!      *Also set the space-derivative of the initial data
!      *Note the scaling by 1/PAR(11) !
       U(3) = pi * COS(pi*X) /PAR(11)
       U(4) = 0. / PAR(11)

!      *Set the diffusion constants
       PAR(15) = 1.
       PAR(16) = 1.

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

!      *Define the boundary conditions.
       FB(1)=U0(1) 
       FB(2)=U0(2)-1. 
       FB(3)=U1(1) 
       FB(4)=U1(2)-1. 

      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
