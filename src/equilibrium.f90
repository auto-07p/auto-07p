MODULE EQUILIBRIUM

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE AE
  USE INTERFACES

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: AUTOEQ,INITEQ
  PUBLIC :: FNLPF,STPNLPF ! Folds (Algebraic Problems)
  PUBLIC :: FNHBF,STPNHBF ! Hopf bifs

  DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

CONTAINS

! ---------- ------
  SUBROUTINE INITEQ(AP)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP

    INTEGER ISW, ITP, NDIM, NFPR
    ISW = AP%ISW
    ITP = AP%ITP
    NDIM = AP%NDIM
    NFPR = 1

    IF(ABS(ISW)>=2)THEN
       ! ** Continuation of singular points
       NFPR=2
       IF(ITP==2.OR.ABS(ITP)/10==2.OR.ITP==7.OR.ABS(ITP)/10==7)THEN
          ! ** Fold/PD continuation (Algebraic Problems)
          NDIM=2*NDIM+1
       ELSE
          ! Hopf/Neimark-Sacker or BP cont
          NDIM=2*NDIM+2
          IF(ITP==1.OR.ABS(ITP)/10==1)THEN
             ! ** BP cont (Algebraic Problems) (by F. Dercole)
             NFPR=ABS(ISW)
          ENDIF
       ENDIF
    ENDIF
    AP%NDIM = NDIM
    AP%NFPR = NFPR
  END SUBROUTINE INITEQ

! ---------- ------
  SUBROUTINE AUTOEQ(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    INTEGER IPS, ISW, ITP

    CALL INITEQ(AP)
    IPS = AP%IPS
    ISW = AP%ISW
    ITP = AP%ITP

    IF(ABS(ISW)<=1)THEN
       IF(IPS==-1) THEN
          ! ** Discrete dynamical systems : fixed points.
          CALL AUTOAE(AP,ICP,ICU,FNDS,STPNAE)
       ELSE
          ! Algebraic systems.
          CALL AUTOAE(AP,ICP,ICU,FUNI,STPNAE)
       ENDIF
    ELSE
       IF(ABS(ISW)==2)THEN
          IF(ITP==2.OR.ITP==7.OR.ABS(ITP)/10==2.OR.ABS(ITP)/10==7)THEN
             ! ** Fold/PD continuation (algebraic problems).
             CALL AUTOAE(AP,ICP,ICU,FNLP,STPNLP)
          ELSEIF(ITP==3.OR.ITP==8.OR.ABS(ITP)/10==3.OR.ABS(ITP)/10==8)THEN
             ! Hopf/Neimark-Sacker bifurcation continuation (ODE/maps).
             CALL AUTOAE(AP,ICP,ICU,FNHB,STPNHB)
          ENDIF
       ENDIF
       IF(ITP==1.OR.ABS(ITP)/10==1)THEN
          ! ** BP cont (algebraic problems) (by F. Dercole).
          CALL AUTOAE(AP,ICP,ICU,FNBP,STPNBP)
       ENDIF
    ENDIF
  END SUBROUTINE AUTOEQ

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Subroutines for the Continuation of Folds (Algebraic Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNLP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-par continuation of folds.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNLPF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

  END SUBROUTINE FNLP

! ---------- -----
  SUBROUTINE FNLPF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

    ! Generates the equations for the 2-par continuation of folds.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:),FF1(:),FF2(:)
    INTEGER NDM,NPAR,I,II,J
    DOUBLE PRECISION UMX,EP,P,UU

    NDM=AP%NDM
    NPAR=AP%NPAR

    IF(NDIM==NDM)THEN ! reduced function for Cusp detection
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,0,F,DFDU,DFDP)
       RETURN
    ENDIF

    ! Generate the function.

    ALLOCATE(DFU(NDM,NDM),DFP(NDM,NPAR))
    CALL FFLP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,NDM,DFU,DFP,FUNI)

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(DFU,DFP)
       RETURN
    ENDIF
    ALLOCATE(FF1(NDIM),FF2(NDIM))

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DFDU(1:NDM,1:NDM)=DFU(:,:)
    DFDU(1:NDM,NDM+1:2*NDM)=0d0
    DFDU(1:NDM,NDIM)=DFP(:,ICP(2))

    DFDU(NDM+1:2*NDM,NDM+1:2*NDM)=DFU(:,:)

    DFDU(NDIM,1:NDM)=0d0
    DFDU(NDIM,NDM+1:2*NDM)=2*U(NDM+1:NDM*2)
    DFDU(NDIM,NDIM)=0d0

    DO II=1,NDM+1
       I=II
       IF(I==NDM+1)I=NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FFLP(AP,NDIM,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DFP,FUNI)
       U(I)=UU+EP
       CALL FFLP(AP,NDIM,U,UOLD,ICP,PAR,0,FF2,NDM,DFU,DFP,FUNI)
       U(I)=UU
       DO J=NDM+1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(FF1,DFU,DFP)
       RETURN
    ENDIF
    P=PAR(ICP(1))
    PAR(ICP(1))=P+EP

    DFDP(1:NDM,ICP(1))=DFP(:,ICP(1))
    CALL FFLP(AP,NDIM,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DFP,FUNI)

    DO J=NDM+1,NDIM
       DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
    ENDDO

    PAR(ICP(1))=P
    DEALLOCATE(FF1,DFU,DFP)

  END SUBROUTINE FNLPF

! ---------- ----
  SUBROUTINE FFLP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP,FUNI)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NDM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    include 'interfaces.h'

    INTEGER IPS,IJC,I,J

    IPS=AP%IPS

    PAR(ICP(2))=U(NDIM)
    IJC=MAX(IJAC,1)
    IF(IPS.EQ.-1) THEN
       CALL FNDS(AP,NDM,U,UOLD,ICP,PAR,IJC,F,DFDU,DFDP)
       IF(AP%ITPST==7)THEN ! PD bif for maps
          DO I=1,NDM
             DFDU(I,I)=DFDU(I,I)+2
          ENDDO
       ENDIF
    ELSE
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJC,F,DFDU,DFDP)
    ENDIF

    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
       ENDDO
    ENDDO

    F(NDIM)=-1

    DO I=1,NDM
       F(NDIM)=F(NDIM)+U(NDM+I)*U(NDM+I)
    ENDDO

  END SUBROUTINE FFLP

! ---------- -------
  SUBROUTINE STPNLP(AP,PAR,ICP,U,UDOT,NODIR)

    USE IO
    USE SUPPORT
    USE AE, ONLY: STPNAE

    ! Generates starting data for the continuation of folds.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)

    CALL STPNLPF(AP,PAR,ICP,U,UDOT,NODIR,FUNI)

  END SUBROUTINE STPNLP

! ---------- -------
  SUBROUTINE STPNLPF(AP,PAR,ICP,U,UDOT,NODIR,FUNI)

    USE IO
    USE SUPPORT
    USE AE, ONLY: STPNAE

    ! Generates starting data for the continuation of folds.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),V(:),F(:)
    DOUBLE PRECISION DUMDFP(1)
    INTEGER ICPRS(2),NDIM,IPS,NDM,I

    NDIM=AP%NDIM
    IPS=AP%IPS
    NDM=AP%NDM

    IF(ABS(AP%ITP)/10==2 .OR. ABS(AP%ITP)/10==7)THEN
       ! restart
       CALL STPNAE(AP,PAR,ICP,U,UDOT,NODIR)
       U(NDIM)=PAR(ICP(2))
       RETURN
    ENDIF

    CALL READLB(AP,ICPRS,U,UDOT,PAR)

    ALLOCATE(DFU(NDM,NDM),V(NDM),F(NDM))
    IF(IPS.EQ.-1)THEN
       CALL FNDS(AP,NDM,U,U,ICP,PAR,1,F,DFU,DUMDFP)
       IF(AP%ITPST==7)THEN ! PD bif for maps
          DO I=1,NDM
             DFU(I,I)=DFU(I,I)+2
          ENDDO
       ENDIF
    ELSE
       CALL FUNI(AP,NDM,U,U,ICP,PAR,1,F,DFU,DUMDFP)
    ENDIF
    CALL NLVC(NDM,NDM,1,DFU,V)
    CALL NRMLZ(NDM,V)
    DO I=1,NDM
       U(NDM+I)=V(I)
    ENDDO
    DEALLOCATE(DFU,V,F)
    U(NDIM)=PAR(ICP(2))
    NODIR=1

  END SUBROUTINE STPNLPF

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for BP cont (Algebraic Problems) (by F. Dercole)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNBP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-par continuation of BP.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:),DFP(:),FF1(:),FF2(:)
    INTEGER NDM,NPAR,I,J
    DOUBLE PRECISION UMX,EP,P,UU

    NDM=AP%NDM
    NPAR=AP%NPAR

    ! Generate the function.

    ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPAR))
    CALL FFBP(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(DFU,DFP)
       RETURN
    ENDIF
    ALLOCATE(FF1(NDIM),FF2(NDIM))

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FFBP(AP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
       U(I)=UU+EP
       CALL FFBP(AP,NDIM,U,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
       U(I)=UU
       DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(FF1,DFU,DFP)
       RETURN
    ENDIF
    P=PAR(ICP(1))
    PAR(ICP(1))=P+EP

    CALL FFBP(AP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)

    DO J=1,NDIM
       DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
    ENDDO

    PAR(ICP(1))=P
    DEALLOCATE(FF1,DFU,DFP)

  END SUBROUTINE FNBP

  !     ---------- ----
  SUBROUTINE FFBP(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)

    INTEGER IPS,ISW,I,J

    IPS=AP%IPS
    ISW=AP%ISW

    IF(ISW.EQ.3) THEN
       !        ** Generic case
       PAR(ICP(3))=U(NDIM)
    ENDIF
    PAR(ICP(2))=U(NDIM-1)

    IF(IPS.EQ.-1) THEN
       CALL FNDS(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
    ELSE
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
    ENDIF

    IF(ISW.EQ.2) THEN
       !        ** Non-generic case
       DO I=1,NDM
          F(I)=F(I)+U(NDIM)*U(NDM+I)
       ENDDO
    ENDIF

    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(J,I)*U(NDM+J)
       ENDDO
    ENDDO

    F(NDIM-1)=0.d0
    DO I=1,NDM
       F(NDIM-1)=F(NDIM-1)+DFDP(I,ICP(1))*U(NDM+I)
    ENDDO

    F(NDIM)=-1
    DO I=1,NDM
       F(NDIM)=F(NDIM)+U(NDM+I)*U(NDM+I)
    ENDDO

  END SUBROUTINE FFBP

! ---------- ------
  SUBROUTINE STPNBP(AP,PAR,ICP,U,UDOT,NODIR)

    USE IO
    USE SUPPORT
    USE AE, ONLY: STPNAE

    ! Generates starting data for the continuation of BP.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE ::DFU(:,:),DFP(:,:),A(:,:),V(:),F(:)
    INTEGER :: ICPRS(3),NDIM,IPS,ISW,NDM,NPAR,I,J

    NDIM=AP%NDIM
    IPS=AP%IPS
    ISW=AP%ISW
    NDM=AP%NDM
    NPAR=AP%NPAR

    IF(ABS(AP%ITP)/10==1)THEN
       ! restart
       CALL STPNAE(AP,PAR,ICP,U,UDOT,NODIR)
       U(NDIM-1)=PAR(ICP(2))
       IF(ISW==3) THEN 
          U(NDIM)=PAR(ICP(3)) ! Generic case
       ELSE
          U(NDIM)=0.d0        ! Non-generic case
       ENDIF
       RETURN
    ENDIF

    CALL READLB(AP,ICPRS,U,UDOT,PAR)

    ALLOCATE(DFU(NDM,NDM),DFP(NDM,NPAR),A(NDM,NDM+1))
    ALLOCATE(V(NDM+1),F(NDM))
    IF(IPS.EQ.-1)THEN
       CALL FNDS(AP,NDM,U,U,ICP,PAR,2,F,DFU,DFP)
    ELSE
       CALL FUNI(AP,NDM,U,U,ICP,PAR,2,F,DFU,DFP)
    ENDIF
    A(:,1:NDM)=DFU(:,:)
    A(:,NDM+1)=DFP(:,ICP(1))
    CALL NLVC(NDM,NDM+1,2,A,V)
    DEALLOCATE(A)
    ALLOCATE(A(NDM+1,NDM+1))
    DO I=1,NDM
       DO J=1,NDM
          A(I,J)=DFU(J,I)
       ENDDO
       A(NDM+1,I)=DFP(I,ICP(1))
    ENDDO
    DO I=1,NDM+1
       A(I,NDM+1)=V(I)
    ENDDO
    CALL NLVC(NDM+1,NDM+1,1,A,V)
    CALL NRMLZ(NDM,V)
    DO I=1,NDM
       U(NDM+I)=V(I)
    ENDDO
    DEALLOCATE(DFU,DFP,A,V,F)
    U(NDIM-1)=PAR(ICP(2))
    IF(ISW.EQ.3) THEN
       !        ** Generic case
       U(NDIM)=PAR(ICP(3))
    ELSE
       !        ** Non-generic case
       U(NDIM)=0.d0
    ENDIF

    NODIR=1
  END SUBROUTINE STPNBP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Discrete Dynamical Systems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNDS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generate the equations for continuing fixed points.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    INTEGER I, J, ITDS, NPAR, NFPR
    DOUBLE PRECISION, ALLOCATABLE :: FN(:),DFDU1(:,:),DFDP1(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: DFDU2(:,:)


    ITDS=AP%ITDS
    CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
    IF(ITDS>=2)THEN
       NFPR=AP%NFPR
       NPAR=AP%NPAR
       ALLOCATE(FN(NDIM),DFDU1(NDIM,NDIM),DFDP1(NDIM,NPAR), &
            DFDU2(NDIM,NDIM))
       DO I=2,ITDS
          ! with iterations use the chain rule
          FN(:)=F(:)
          CALL FUNI(AP,NDIM,FN,UOLD,ICP,PAR,IJAC,F,DFDU1,DFDP1)
          IF(IJAC>0)THEN
             ! DFDU=DFDU1*DFDU
             CALL DGEMM('n','n',NDIM,NDIM,NDIM,1.d0,DFDU1, &
                  NDIM,DFDU,NDIM,0.d0,DFDU2,NDIM)
             DFDU(:,:)=DFDU2(:,:)
             IF(IJAC>1)THEN
                ! DFDP=DFDU1*DFDP+DFDP1
                DO J=1,NFPR
                   CALL DGEMV('n',NDIM,NDIM,1.d0,DFDU1,NDIM, &
                        DFDP(1,ICP(J)),1,1d0,DFDP1(1,ICP(J)),1)
                   DFDP(:,ICP(J))=DFDP1(:,ICP(J))
                ENDDO
             ENDIF
          ENDIF
       ENDDO
       DEALLOCATE(FN,DFDU1,DFDU2,DFDP1)
    ENDIF

    F(:)=F(:)-U(:)

    IF(IJAC.EQ.0)RETURN

    DO I=1,NDIM
       DFDU(I,I)=DFDU(I,I)-1
    ENDDO

  END SUBROUTINE FNDS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Subroutines for the Continuation of Hopf Bifurcation Points
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNHB(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-parameter continuation of Hopf
    ! bifurcation points in ODE/map.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNHBF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

  END SUBROUTINE FNHB

! ---------- -----
  SUBROUTINE FNHBF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

    USE SUPPORT, ONLY: PI

    ! Generates the equations for the 2-parameter continuation of Hopf
    ! bifurcation points in ODE/wave/map.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE ::DFU(:),UU1(:),UU2(:),FF1(:),FF2(:)
    INTEGER NDM,I,J
    DOUBLE PRECISION UMX,EP,P,KAPPA

    NDM=AP%NDM

    IF(NDIM==NDM)THEN ! reduced function for Generalized-Hopf detection
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,0,F,DFDU,DFDP)
       RETURN
    ENDIF

    ! Generate the function.

    ALLOCATE(DFU(NDIM*NDIM))
    CALL FFHB(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,FUNI)
    IF(AP%IPS/=-1)THEN
       KAPPA=U(NDIM-1)
       IF(KAPPA>0)THEN
          PAR(11)=PI(2.d0)/SQRT(KAPPA)
       ENDIF
    ENDIF

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(DFU)
       RETURN
    ENDIF

    ! Generate the Jacobian.

    ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDIM
       DO J=1,NDIM
          UU1(J)=U(J)
          UU2(J)=U(J)
       ENDDO
       UU1(I)=UU1(I)-EP
       UU2(I)=UU2(I)+EP
       CALL FFHB(AP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,FUNI)
       CALL FFHB(AP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,FUNI)
       DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(UU1,UU2,FF2)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(FF1,DFU)
       RETURN
    ENDIF

    P=PAR(ICP(1))
    PAR(ICP(1))=P+EP

    CALL FFHB(AP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,FUNI)

    DO J=1,NDIM
       DFDP(J,ICP(1))=(FF1(J)-F(J))/EP
    ENDDO

    PAR(ICP(1))=P
    DEALLOCATE(FF1,DFU)

  END SUBROUTINE FNHBF

! ---------- ----
  SUBROUTINE FFHB(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,FUNI)

    !     See Kuznetsov, 3rd ed., (10.80) and (10.83)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM)
    include 'interfaces.h'
    ! Local
    INTEGER IPS,I,J,K
    DOUBLE PRECISION DUMDP(1),THTA,C1,D2,KAPPA

    IPS=AP%IPS

    PAR(ICP(2))=U(NDIM)
    IF(IPS==-1)THEN
       THTA=U(NDIM-1)
       C1=-2*COS(THTA)
       KAPPA=1.d0
    ELSE
       C1=0.d0
       KAPPA=U(NDIM-1)
    ENDIF
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,1,F,DFDU,DUMDP)

    IF(IPS==-1)THEN
       DO I=1,NDM
          F(I)=F(I)-U(I)
       ENDDO
    ENDIF

    DO I=1,NDM
       F(NDM+I)=KAPPA*U(NDM+I)
       DO J=1,NDM
          D2=0
          DO K=1,NDM
             D2=D2+DFDU(K,J)*DFDU(I,K)
          ENDDO
          F(NDM+I)=F(NDM+I)+(D2+C1*DFDU(I,J))*U(NDM+J)
       ENDDO
    ENDDO

    F(NDIM-1)=-1
    DO I=1,NDM
       F(NDIM-1)=F(NDIM-1)+U(NDM+I)*U(NDM+I)
    ENDDO

    ! This expression approximates the previously used phase
    ! condition
    ! <eta,xi_0> - <eta_0,xi>
    ! disregarding the factor T/(2*pi).
    ! where \eta=U(NDM+1:NDM*2), \eta_0=UOLD(NDM+1:NDM*2), and
    ! \xi=-T/(2*pi)*DFDU \eta
    F(NDIM)=0.d0
    DO I=1,NDM
       DO J=1,NDM
          F(NDIM)=F(NDIM)+DFDU(I,J)* &
                       (U(NDM+J)*UOLD(NDM+I)-U(NDM+I)*UOLD(NDM+J))
       ENDDO
    ENDDO

  END SUBROUTINE FFHB

! ---------- ------
  SUBROUTINE STPNHB(AP,PAR,ICP,U,UDOT,NODIR)

    ! Generates starting data for the 2-parameter continuation of
    ! Hopf bifurcation point (ODE/wave/map).

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)

    CALL STPNHBF(AP,PAR,ICP,U,UDOT,NODIR,FUNI)

  END SUBROUTINE STPNHB

! ---------- -------
  SUBROUTINE STPNHBF(AP,PAR,ICP,U,UDOT,NODIR,FUNI)

    USE IO
    USE SUPPORT
    USE AE, ONLY: STPNAE

    ! Generates starting data for the 2-parameter continuation of
    ! Hopf bifurcation point (ODE/wave/map).

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),SMAT(:,:),V(:),F(:)
    INTEGER :: ICPRS(2),NDIM,IPS,NDM,I,J
    DOUBLE PRECISION DFP(1),THTA,C1,KAPPA,PERIOD

    NDIM=AP%NDIM
    IPS=AP%IPS
    NDM=AP%NDM

    IF(ABS(AP%ITP)/10==3 .OR. ABS(AP%ITP)/10==8)THEN
       ! restart
       CALL STPNAE(AP,PAR,ICP,U,UDOT,NODIR)
       U(NDIM)=PAR(ICP(2))
       RETURN
    ENDIF

    ALLOCATE(DFU(NDM,NDM),F(NDM),V(NDM),SMAT(NDM,NDM))

    CALL READLB(AP,ICPRS,U,UDOT,PAR)

    IF(IPS==-1)THEN
       THTA=PI(2.d0)/PAR(11)
       C1=-2*COS(THTA)
       KAPPA=1d0
       U(NDIM-1)=THTA
    ELSE
       PERIOD=PAR(11)
       KAPPA=(PI(2.d0)/PERIOD)**2
       C1=0d0
       U(NDIM-1)=KAPPA
    ENDIF
    CALL FUNI(AP,NDM,U,U,ICP,PAR,1,F,DFU,DFP)

    CALL DGEMM('n','n',NDM,NDM,NDM,1.d0,DFU, &
         NDM,DFU,NDM,0.d0,SMAT,NDM)

    DO I=1,NDM 
       DO J=1,NDM
          SMAT(I,J)=SMAT(I,J)+C1*DFU(I,J)
       ENDDO
       SMAT(I,I)=SMAT(I,I)+KAPPA
    ENDDO
    CALL NLVC(NDM,NDM,2,SMAT,V)
    CALL NRMLZ(NDM,V)

    DO I=1,NDM
       U(NDM+I)=V(I)
    ENDDO

    U(NDIM)=PAR(ICP(2))

    DEALLOCATE(DFU,F,V,SMAT)
    NODIR=1
  END SUBROUTINE STPNHBF

END MODULE EQUILIBRIUM
