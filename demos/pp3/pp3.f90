!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   pp3 :    A continuous dynamical system with period doubling
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!--------- ---- 

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
  DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
  DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
  DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,*), DFDP(NDIM,*)
 
  F(1)= U(1)*(1-U(1)) - PAR(4)*U(1)*U(2) 
  F(2)=-PAR(2)*U(2)   + PAR(4)*U(1)*U(2) - PAR(5)*U(2)*U(3) &
       - PAR(1)*(1-EXP(-PAR(6)*U(2))) 
  F(3)=-PAR(3)*U(3)   + PAR(5)*U(2)*U(3) 

END SUBROUTINE FUNC
!---------------------------------------------------------------------- 

SUBROUTINE STPNT(NDIM,U,PAR,T) 
!--------- ----- 

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM
  DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
  DOUBLE PRECISION, INTENT(IN) :: T

  U = (/ 1.0, 0.0, 0.0 /)

  PAR(:6) = (/ 0.0, 0.25, 0.5, 4.0, 3.0, 5.0 /)

END SUBROUTINE STPNT
!---------------------------------------------------------------------- 

SUBROUTINE PVLS(NDIM,U,PAR)
!--------- ----

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM
  DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
  DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
  DOUBLE PRECISION GETP

  DOUBLE PRECISION r1, r2, r3

! Set PAR(9) equal to the L2-norm of U
  r1=GETP('NRM',1,U)
  r2=GETP('NRM',2,U)
  r3=GETP('NRM',3,U)

  PAR(9)=SQRT(r1**2 + r2**2 + r3**2)

END SUBROUTINE PVLS

SUBROUTINE BCND 
END SUBROUTINE BCND

SUBROUTINE ICND 
END SUBROUTINE ICND

SUBROUTINE FOPT 
END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
