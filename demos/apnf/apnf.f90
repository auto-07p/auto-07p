!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   Example 1: The AP BP normal form
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
  DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
  DOUBLE PRECISION, INTENT(OUT) :: F(NDIM), DFDU(NDIM,*), DFDP(NDIM,*)

  F(1) = (U(1)-PAR(1))*(U(1)-PAR(2))+PAR(3)

  IF(IJAC==0)RETURN
  
  DFDU(1,1) = U(1)-PAR(1)+U(1)-PAR(2)

  IF(IJAC==1)RETURN

  DFDP(1,1) = -(U(1)-PAR(2))
  DFDP(1,2) = -(U(1)-PAR(1))
  DFDP(1,3) = 1.0d0

END SUBROUTINE FUNC

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE STPNT(NDIM,U,PAR)
!--------- -----
  
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM
  DOUBLE PRECISION, INTENT(OUT) :: U(NDIM), PAR(*)

  PAR(1:3) = (/ 1.0d0, 2.0d0, 0.0d0 /)

  U(1) = 1.0 
   
END SUBROUTINE STPNT

!----------------------------------------------------------------------
!----------------------------------------------------------------------

SUBROUTINE BCND
END SUBROUTINE BCND

SUBROUTINE ICND
END SUBROUTINE ICND

SUBROUTINE FOPT
END SUBROUTINE FOPT

SUBROUTINE PVLS
END SUBROUTINE PVLS

!----------------------------------------------------------------------
!----------------------------------------------------------------------
