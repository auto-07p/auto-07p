!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   Henon :    The Henon map
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION x, y, alpha, beta

      x=U(1)
      y=U(2)
      alpha=PAR(1)
      beta=PAR(2)

       F(1)=y
       F(2)=alpha-beta*x-y**2

      IF(IJAC.EQ.0)RETURN 

       DFDU(1,1)=0
       DFDU(1,2)=1
       DFDU(2,1)=-beta
       DFDU(2,2)=-2*y

      IF(IJAC.EQ.1)RETURN 

       DFDP(1,1)=0
       DFDP(1,2)=0
       DFDP(2,1)=1 
       DFDP(2,2)=-x

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=2 ! alpha
       PAR(2)=0 ! beta

       U(1)=1
       U(2)=1

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
