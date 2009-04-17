!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   spb :    A singularly perturbed BVP
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION U1,U2,EPS,RL

       U1=U(1)
       U2=U(2)
       EPS=PAR(2)
       RL =PAR(3)

       F(1)=U2 
       F(2)=RL * ( U1*(U1**2-1)*U2 + U1 ) / EPS 

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,X)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: X

      DOUBLE PRECISION GAMMA,EPS,RL,S

       GAMMA=1.2
       EPS=0.1
       RL=0.

       PAR(1)=GAMMA
       PAR(2)=EPS
       PAR(3)=RL

       S=GAMMA-1.5
       U(1)=1.5 + S*X
       U(2)=S

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

       FB(1)=U0(1)-1.5
       FB(2)=U1(1)-PAR(1) 

      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
