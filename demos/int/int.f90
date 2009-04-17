!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   int :    An ODE with boundary and integral constraints
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION E

       E=EXP(U(1)) 
       F(1)=U(2) 
       F(2)=-PAR(1)*E 

      IF(IJAC.EQ.0)RETURN 

       DFDU(1,1)=0.0 
       DFDU(1,2)=1 
       DFDU(2,1)=-PAR(1)*E 
       DFDU(2,2)=0.0 

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives
       DFDP(1,1)=0.0 
       DFDP(2,1)=-E 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=0
       PAR(2)=0
       PAR(3)=0

       U(1)=0.0 
       U(2)=0.0 

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

       FB(1)=U0(1)-U1(1)-PAR(2) 

      IF(IJAC.EQ.0)RETURN 

       DBC(1,1)=1.0 
       DBC(1,2)=0.0 

       DBC(1,3)=-1.0 
       DBC(1,4)=0.0 

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives
       DBC(1,5)=0.0 
       DBC(1,6)=-1.0 
       DBC(1,7)=0.0 

      END SUBROUTINE BCND

      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

       FI(1)=U(1)-PAR(3) 

      IF(IJAC.EQ.0)RETURN 

       DINT(1,1)=1.0 
       DINT(1,2)=0.0 

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives
       DINT(1,3)=0.0 
       DINT(1,4)=0.0 
       DINT(1,5)=-1.0 

      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
