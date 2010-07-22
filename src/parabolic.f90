!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Parabolic PDEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE PARABOLIC

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE INTERFACES

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: FNWS        ! Spatially uniform sols (parabolic PDEs)
  PUBLIC :: FNWP        ! Travelling waves (parabolic PDEs)
  PUBLIC :: FNSP        ! Stationary states (parabolic PDEs)
  PUBLIC :: FNPE,ICPE,PVLSPE   ! Time evolution (parabolic PDEs)

CONTAINS


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Travelling Wave Solutions to Parabolic PDEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNWS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Sets up equations for the continuation of spatially homogeneous
    ! solutions to parabolic systems, for the purpose of finding
    ! bifurcations to travelling wave solutions.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:)
    INTEGER NDM,I,J
    DOUBLE PRECISION C,DUMDFU(1),DUMDFP(1)

    ! Generate the function.

    NDM=AP%NDM/2

    IF(IJAC==0)THEN
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F,DUMDFU,DUMDFP)
    ELSE
       ALLOCATE(DFU(NDM,NDM))
       IF(IJAC==1)THEN
          CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFU,DUMDFP)
       ELSE
          ALLOCATE(DFP(NDM,AP%NPAR))
          CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFU,DFP)
       ENDIF
    ENDIF

    C=PAR(10)
    DO I=1,NDM
       F(NDM+I)=-( C*U(NDM+I) + F(I) )/PAR(14+I)
       F(I)=U(NDM+I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    DO I=1,NDM
       DO J=1,NDM
          DFDU(I,J)        =0.d0
          DFDU(I,J+NDM)    =0.d0
          DFDU(I+NDM,J)    =-DFU(I,J)/PAR(14+I)
          DFDU(I+NDM,J+NDM)=0.d0
       ENDDO
       DFDU(I,I+NDM)     =1
       DFDU(I+NDM,I+NDM)=-C/PAR(14+I)
    ENDDO

    DEALLOCATE(DFU)
    IF(IJAC.EQ.1)RETURN

    DO I=1,NDM
       IF(ICP(1)/=10)THEN
          DFDP(I,ICP(1))    =0.d0
          DFDP(I+NDM,ICP(1))=-DFP(I,ICP(1))/PAR(14+I)
       ENDIF
       IF(AP%NFPR>1.AND.ICP(2)/=10)THEN
          DFDP(I,ICP(2))    =0.d0
          DFDP(I+NDM,ICP(2))=-DFP(I,ICP(2))/PAR(14+I)
       ENDIF
    ENDDO

    ! Derivative with respect to the wave speed.

    DO I=1,NDM
       DFDP(I,10)    =0.d0
       DFDP(I+NDM,10)=-U(NDM+I)/PAR(14+I)
    ENDDO

    ! Derivatives with respect to the diffusion coefficients.

    DO J=1,NDM
       DO I=1,NDM
          DFDP(I,14+J)    =0.d0
          DFDP(I+NDM,14+J)=0.d0
       ENDDO
       DFDP(J+NDM,14+J)=-F(J+NDM)/PAR(14+J)
    ENDDO

    DEALLOCATE(DFP)
  END SUBROUTINE FNWS

! ---------- ----
  SUBROUTINE FNWP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Equations for the continuation of traveling waves.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    INTEGER NFPX,J
    DOUBLE PRECISION PERIOD

    ! Generate the function and Jacobian.

    CALL FNWS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
    PERIOD=PAR(11)
    IF(ICP(2).EQ.11.AND.(IJAC.EQ.2.OR.IJAC.EQ.-1))THEN
       !        **Variable wave length
       DFDP(:,11)=F(:)
    ENDIF
    F(:)=PERIOD*F(:)
    IF(IJAC.EQ.0)RETURN
    DFDU(:,:)=PERIOD*DFDU(:,:)
    IF(ABS(IJAC).EQ.1)RETURN
    NFPX=1
    IF(ICP(2).NE.11)THEN
       !        **Fixed wave length
       NFPX=2
    ENDIF
    DO J=1,NFPX
       DFDP(:,ICP(J))=PERIOD*DFDP(:,ICP(J))
    ENDDO

  END SUBROUTINE FNWP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!             Parabolic PDEs : Stationary States
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNSP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for taking one time step (Implicit Euler).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:)
    INTEGER NDM,NPAR,I,J
    DOUBLE PRECISION PERIOD,DUMDFU(1),DUMDFP(1)

    NDM=AP%NDM
    NPAR=AP%NPAR

    ! Generate the function and Jacobian.

    IF(IJAC==0)THEN
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DUMDFU,DUMDFP)
    ELSE
       ALLOCATE(DFU(NDM,NDM))
       IF(IJAC==1)THEN
          CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DUMDFU,DFP)
       ELSE
          ALLOCATE(DFP(NDM,NPAR))
          CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DFU,DFP)
       ENDIF
    ENDIF

    PERIOD=PAR(11)
    DO I=1,NDM
       F(I)    = PERIOD*U(NDM+I)
       F(NDM+I)=-PERIOD*F(NDM+I)/PAR(14+I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    DO I=1,NDM
       DO J=1,NDM
          DFDU(I,J)        =0.d0
          DFDU(I,J+NDM)    =0.d0
          DFDU(I+NDM,J)    =-PERIOD*DFU(I,J)/PAR(14+I)
          DFDU(I+NDM,J+NDM)=0.d0
       ENDDO
       DFDU(I,I+NDM)     =PERIOD
    ENDDO
    DEALLOCATE(DFU)
    IF(IJAC.EQ.1)RETURN
    DO I=1,NDM
       IF(ICP(1).EQ.11)THEN
          DFDP(I,ICP(1))    = F(I)/PERIOD
          DFDP(NDM+I,ICP(1))= F(NDM+I)/PERIOD
       ELSEIF(ICP(1).EQ.14+I)THEN
          DFDP(I,ICP(1))    = 0.d0
          DFDP(NDM+I,ICP(1))=-F(NDM+I)/PAR(14+I)
       ELSEIF(ICP(1).NE.11 .AND. &
            .NOT. (ICP(1).GT.14 .AND. ICP(1).LE.14+NDM) )THEN
          DFDP(I,ICP(1))    =0.d0
          DFDP(I+NDM,ICP(1))=-PERIOD*DFP(I,ICP(1))/PAR(14+I)
       ENDIF
    ENDDO

    DEALLOCATE(DFP)
  END SUBROUTINE FNSP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!            Time Evolution of Parabolic PDEs
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNPE(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for taking one time step (Implicit Euler).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:)
    INTEGER NDM,IIJAC,I,J
    DOUBLE PRECISION DUMDFU(1),DUMDFP(1),T,DT,PERIOD,RLOLD

    NDM=AP%NDM

    ! Generate the function and Jacobian.

    IF(IJAC==0)THEN
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F(NDM+1),DUMDFU,DUMDFP)
    ELSE
       ALLOCATE(DFU(NDM,NDM))
       IIJAC=IJAC
       IF(IJAC.GT.1)IIJAC=1
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IIJAC,F(NDM+1),DFU,DUMDFP)
    ENDIF

    PERIOD=PAR(11)
    T=PAR(ICP(1))
    RLOLD=PAR(12)
    IF(T==RLOLD)THEN
       ! Two reasons:
       ! 1. at the start, then UOLD==U, so we can set DT to 1,
       !    because it is irrelevant.
       ! 2. from STUPBV, to calculate UPOLDP, which is not needed
       !    because NINT is forced to 0.
       DT=1.d0
    ELSE
       DT=T-RLOLD
    ENDIF

    DO I=1,NDM
       F(I)=PERIOD*U(NDM+I)
       F(NDM+I)=PERIOD*( (U(I)-UOLD(I))/DT - F(NDM+I) )/PAR(14+I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    DO I=1,NDM
       DO J=1,NDM
          DFDU(I,J)        =0.d0
          DFDU(I,J+NDM)    =0.d0
          DFDU(I+NDM,J)    =-PERIOD*DFU(I,J)/PAR(14+I)
          DFDU(I+NDM,J+NDM)=0.d0
       ENDDO
       DFDU(I,I+NDM)     =PERIOD
       DFDU(I+NDM,I)     =DFDU(I+NDM,I) + PERIOD/(DT*PAR(14+I))
    ENDDO
    DEALLOCATE(DFU)
    IF(IJAC.EQ.1)RETURN

    DO I=1,NDM
       DFDP(I,ICP(1))    =0.d0
       DFDP(I+NDM,ICP(1))=-PERIOD*(U(I)-UOLD(I))/(DT**2*PAR(14+I))
    ENDDO

  END SUBROUTINE FNPE

! ---------- ----
  SUBROUTINE ICPE(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Dummy integral condition subroutine for parabolic systems.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    F(:)=0d0
  END SUBROUTINE ICPE

! ---------- ------
  SUBROUTINE PVLSPE(AP,ICP,UPS,NDIM,PAR)

    USE BVP
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

    !     save old time

    PAR(12)=PAR(ICP(1))
    CALL PVLSBV(AP,ICP,UPS,NDIM,PAR)

  END SUBROUTINE PVLSPE

END MODULE PARABOLIC