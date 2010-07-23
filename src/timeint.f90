!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Time Integration of ODEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE TIMEINT

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE AE
  USE INTERFACES

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: AUTOTI

CONTAINS

! ---------- ------
  SUBROUTINE AUTOTI(AP,PAR,ICP,ICU,THL,THU,IUZ,VUZ)

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),ICU(*),IUZ(*)
    DOUBLE PRECISION PAR(*),THL(*),THU(*),VUZ(*)

    ! ** Time integration (IPS==-2)
    CALL AUTOAE(AP,PAR,ICP,ICU,FNTI,STPNAE,THL,THU,IUZ,VUZ)
  END SUBROUTINE AUTOTI

! ---------- ----
  SUBROUTINE FNTI(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generate the equations for continuing fixed points.

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
