!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   nep :    A nonlinear ODE eigenvalue problem
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

       F(1) =  U(2) 
       F(2) = -PAR(2)*U(1) 
       F(3) = -U(2)**2/2 - PAR(2)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=0
       PAR(2)=0

       U(1)=0.0 
       U(2)=0.0 
       U(3)=0.0 

      END SUBROUTINE STPNT
!---------------------------------------------------------------------- 

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

       FB(1)=U0(1) 
       FB(2)=U1(1) 
       FB(3)=U0(3) - PAR(1)
       FB(4)=U1(3) + PAR(1)

      END SUBROUTINE BCND
!---------------------------------------------------------------------- 

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
!---------------------------------------------------------------------- 
