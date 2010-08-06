!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Time Integration of ODEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE TIMEINT

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE AE
  USE TOOLBOXAE
  USE INTERFACES

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: AUTOTI

CONTAINS

! ---------- ------
  SUBROUTINE AUTOTI(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    ! ** Time integration (IPS==-2)
    AP%NFPR=1
    AP%ISP=0
    AP%ILP=0
    ICP(1)=14
    CALL AUTOAE(AP,ICP,ICU,FNTI,STPNAE,FNCSAE)
  END SUBROUTINE AUTOTI

! ---------- ----
  SUBROUTINE FNTI(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generate the equations for time integration.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    INTEGER I,J
    DOUBLE PRECISION TOLD,DT

    CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    TOLD=UOLD(NDIM+1)
    DT=PAR(ICP(1))-TOLD

    DO I=1,NDIM
       DFDP(I,ICP(1))=F(I)
       F(I)= DT*F(I) - U(I) + UOLD(I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    DO I=1,NDIM
       DO J=1,NDIM
          DFDU(I,J)= DT*DFDU(I,J)
       ENDDO
       DFDU(I,I)= DFDU(I,I) - 1.d0
    ENDDO
  END SUBROUTINE FNTI

END MODULE TIMEINT
