MODULE EQUILIBRIUM

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE AE
  USE INTERFACES

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: AUTOEQ,INITEQ
  PUBLIC :: FNLPF,STPNLPF,FNCSEQF ! Folds (Algebraic Problems)
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
          CALL AUTOAE(AP,ICP,ICU,FNDS,STPNAE,FNCSEQ)
       ELSE
          ! Algebraic systems.
          CALL AUTOAE(AP,ICP,ICU,FUNI,STPNAE,FNCSEQ)
       ENDIF
    ELSE
       IF(ABS(ISW)==2)THEN
          IF(ITP==2.OR.ITP==7.OR.ABS(ITP)/10==2.OR.ABS(ITP)/10==7)THEN
             ! ** Fold/PD continuation (algebraic problems).
             CALL AUTOAE(AP,ICP,ICU,FNLP,STPNLP,FNCSEQ)
          ELSEIF(ITP==3.OR.ITP==8.OR.ABS(ITP)/10==3.OR.ABS(ITP)/10==8)THEN
             ! Hopf/Neimark-Sacker bifurcation continuation (ODE/maps).
             CALL AUTOAE(AP,ICP,ICU,FNHB,STPNHB,FNCSEQ)
          ENDIF
       ENDIF
       IF(ITP==1.OR.ABS(ITP)/10==1)THEN
          ! ** BP cont (algebraic problems) (by F. Dercole).
          CALL AUTOAE(AP,ICP,ICU,FNBP,STPNBP,FNCSEQ)
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

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNCSEQ(AP,ICP,U,NDIM,PAR,ITEST,ITP) RESULT(Q)

    USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(IN) :: ITEST
    INTEGER, INTENT(OUT) :: ITP

    Q=FNCSEQF(AP,ICP,U,NDIM,PAR,ITEST,ITP,FUNI)

  END FUNCTION FNCSEQ

! ------ --------- -------- -------
  DOUBLE PRECISION FUNCTION FNCSEQF(AP,ICP,U,NDIM,PAR,ITEST,ITP,FUNI) RESULT(Q)

    USE SUPPORT, ONLY: AA=>P0V
    USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(IN) :: ITEST
    INTEGER, INTENT(OUT) :: ITP

    Q=0.d0
    ITP=0
    SELECT CASE(ITEST)
    CASE(0,1,2)
       Q=FNCSAE(AP,ICP,U,NDIM,PAR,ITEST,ITP)
    CASE(3) ! Check for cusp on fold
       Q=FNCPAE(AP,PAR,ICP,ITP,FUNI,U,AA)
    CASE(4) ! Check for Bogdanov-Takens bifurcation
       Q=FNBTAE(AP,ITP,U,AA)
    CASE(5) ! Check for generalized Hopf (Bautin)
       Q=FNGHAE(AP,PAR,ICP,ITP,FUNI,U,AA)
    CASE(6) ! Check for Hopf or Zero-Hopf
       Q=FNHBAE(AP,PAR,ITP,AA)
       CALL PRINTEIG(AP)
    END SELECT

  END FUNCTION FNCSEQF

! ---------- --------
  SUBROUTINE PRINTEIG(AP)

    USE SUPPORT, ONLY : EVV
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP

    INTEGER i,NTOP

    IF(AP%IID>0)THEN
       NTOP=MOD(AP%NTOT-1,9999)+1
       WRITE(9,102)ABS(AP%IBR),NTOP+1,AP%NINS
       DO i=1,AP%NDM
          WRITE(9,103)ABS(AP%IBR),NTOP+1,I,EVV(i)
       ENDDO
    ENDIF

102 FORMAT(/,I4,I6,9X,'Eigenvalues  :   Stable:',I4)
103 FORMAT(I4,I6,9X,'Eigenvalue',I3,":",2ES14.5)

  END SUBROUTINE PRINTEIG

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNHBAE(AP,PAR,ITP,AA)

    USE SUPPORT, ONLY: PI, EVV, EIG, CHECKSP, LBTYPE

    DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7,RLARGE=1.0d+30

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: AA(AP%NDIM+1,AP%NDIM+1)
! Local
    COMPLEX(KIND(1.0D0)) ZTMP
    COMPLEX(KIND(1.0D0)), ALLOCATABLE :: EV(:)
    DOUBLE PRECISION, ALLOCATABLE :: AAA(:,:)
    INTEGER NDM,IPS,ISP,ISW,IID,IBR,NTOT,NTOP,NINS,NINS1,I,j,LOC,ITPST
    DOUBLE PRECISION a,AR,AREV,RIMHB,tol,trace,REV

    NDM=AP%NDM
    IPS=AP%IPS
    ISP=AP%ISP
    ISW=AP%ISW
    IID=AP%IID
    IBR=AP%IBR
    ITPST=AP%ITPST
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1
    ALLOCATE(EV(NDM))

! INITIALIZE

    ITP=0
    IF(IPS==0)RETURN

! Set tolerance for deciding if an eigenvalue is in the positive
! half-plane. Use, for example, tol=1d-3 for conservative systems.

! Try to guess whether the system is probably conservative or definitely not:
! the dimension is even and the trace 0 if it is conservative.
! In that case we use a tolerance to avoid detecting spurious
! Hopf bifurcations.

    tol=0.d0
    IF(MOD(NDM,2)==0)THEN
       trace=0.d0
       DO I=1,NDM
          trace=trace+AA(i,i)
       ENDDO
       a=0.d0
       DO i=1,NDM
          DO j=1,NDM
             IF(ABS(AA(i,j))>a)THEN
                a=ABS(AA(i,j))
             ENDIF
          ENDDO
       ENDDO
       IF(ABS(trace)<HMACH*a)THEN
          tol=1.d-5
       ENDIF
    ENDIF

! Compute the eigenvalues of the Jacobian

    ALLOCATE(AAA(NDM,NDM))
    AAA(:,:)=AA(1:NDM,1:NDM)
    CALL EIG(AP,NDM,NDM,AAA,EV)
    DEALLOCATE(AAA)
    IF(IPS.EQ.-1)THEN
       DO I=1,NDM
          IF(REAL(EV(I)).NE.-1.d0 .OR. &
               AIMAG(EV(I)).NE. 0.d0)THEN
             EV(I)=LOG(1.d0+EV(I))
          ELSE
             EV(I)= CMPLX(-RLARGE,0.d0,KIND(1.0D0))
          ENDIF
       ENDDO
    ENDIF

! Order the eigenvalues by real part.

    DO I=1,NDM-1
       LOC=I
       DO J=I+1,NDM
          IF(REAL(EV(J)).GE.REAL(EV(LOC)))THEN
             LOC=J
          ENDIF
       ENDDO
       IF(LOC>I) THEN
          ZTMP=EV(LOC)
          EV(LOC)=EV(I)
          EV(I)=ZTMP
       ENDIF
    ENDDO

! Compute the smallest real part.

    AREV=HUGE(AREV)
    REV=0.d0
    LOC=0
    DO I=1,NDM
       IF(AIMAG(EV(I)).NE.0.d0.OR.(ISW==2.AND.ITPST==2.AND.IPS/=-1))THEN
          AR=ABS(REAL(EV(I)))
          IF(AR.LE.AREV)THEN
             AREV=AR
             LOC=I
          ENDIF
       ENDIF
    ENDDO
    IF(ISW==2.AND.IPS/=-1)THEN
       IF(ITPST==2)THEN
          ! for Zero-Hopf on LP curves compute one-but-smallest real part
          IF(AP%ITP/=-23)THEN ! No Bogdanov-Takens
             AREV=HUGE(AREV)
             DO I=1,NDM
                AR=ABS(REAL(EV(I)))
                IF(AR.LE.AREV.AND.I/=LOC)THEN
                   AREV=AR
                   REV=REAL(EV(I))
                ENDIF
             ENDDO
          ENDIF
       ELSE
          ! Evaluate determinant on Hopf/BP bifurcations
          ZTMP=1
          DO I=1,NDM
             ZTMP=ZTMP*EV(I)
          ENDDO
          REV=REAL(ZTMP)
       ENDIF
    ELSEIF(LOC>0)THEN
       REV=REAL(EV(LOC))
       RIMHB=ABS(AIMAG(EV(LOC)))
       IF(RIMHB.NE.0.d0.AND.ABS(ISW).LE.1)PAR(11)=PI(2.d0)/RIMHB
    ENDIF

! Count the number of eigenvalues with negative real part.

    NINS1=0
    DO I=1,NDM
       IF(REAL(EV(I)).LE.tol)NINS1=NINS1+1
    ENDDO

    IF(IPS==-1)THEN
       ITP=TPSPAE(AP%EPSS,ITPST,PAR(11))
    ELSEIF(ITPST==3)THEN
       ITP=-32 ! Check for Zero-Hopf on HB
    ELSE
       ITP=3+10*ITPST
    ENDIF
    IF((ISW==2.AND.IPS==-1).OR..NOT.CHECKSP(LBTYPE(ITP),IPS,AP%ILP,ISP))THEN
       FNHBAE=0d0
       ITP=0
    ELSE
       FNHBAE=REV
       IF(IID>=2)WRITE(9,101)ABS(IBR),NTOP+1,FNHBAE
    ENDIF
    AP%HBFF=FNHBAE
    NINS=AP%NINS
    IF(IPS/=-1.AND.ISW/=2)THEN
       IF(ABS(NINS1-NINS)<2)ITP=0
    ELSE
       IF(NINS1==NINS)ITP=0
    ENDIF
    NINS=NINS1
    AP%NINS=NINS

    IF(IPS.EQ.-1)THEN
       EVV(:)=EXP(EV(:))
    ELSE
       EVV(:)=EV(:)
    ENDIF

101 FORMAT(I4,I6,9X,'Hopf Function:',ES14.5)

  END FUNCTION FNHBAE

! ---------- -------
  SUBROUTINE RNULLVC(AP,AA,V)

    ! get null vector for the transposed Jacobian for BT/CP detection

    USE SUPPORT, ONLY: NLVC, NRMLZ

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    DOUBLE PRECISION, INTENT(IN) :: AA(AP%NDIM+1,AP%NDIM+1)
    DOUBLE PRECISION, INTENT(INOUT) :: V(AP%NDM)

    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:)
    INTEGER NDM,I

    IF(AP%ISW==2.AND.AP%ITPST==2.AND.AP%IPS/=-1.AND.AP%ISP/=0.AND.&
         AP%ISP/=3)THEN
       NDM=AP%NDM
       ALLOCATE(DFU(NDM,NDM))
       DO I=1,NDM
          DFU(1:NDM,I)=AA(I,1:NDM)
       ENDDO
       CALL NLVC(NDM,NDM,1,DFU,V)
       CALL NRMLZ(NDM,V)
       DEALLOCATE(DFU)
    ENDIF
  END SUBROUTINE RNULLVC

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNBTAE(AP,ITP,U,AA)

    USE SUPPORT, ONLY: CHECKSP

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: U(AP%NDIM), AA(AP%NDIM+1,AP%NDIM+1)
! Local
    INTEGER NDM,NTOP
    DOUBLE PRECISION, ALLOCATABLE :: V(:)

    FNBTAE = 0
    ITP=0
    IF(AP%ISW/=2.OR.AP%ITPST==1.OR..NOT.CHECKSP('BT',AP%IPS,AP%ILP,AP%ISP))THEN
       RETURN
    ENDIF

    IF(AP%ITPST==2)THEN
       NDM=AP%NDM

       ! take the inner product with the null vector for the Jacobian
       ALLOCATE(V(NDM))
       CALL RNULLVC(AP,AA,V)
       FNBTAE = DOT_PRODUCT(U(NDM+1:2*NDM),V(1:NDM))
       DEALLOCATE(V)

    ELSE
       ! BT on Hopf curve
       FNBTAE = U(AP%NDIM-1)
    ENDIF
    ITP=-3-10*AP%ITPST

    NTOP=MOD(AP%NTOT-1,9999)+1
    IF(AP%IID.GE.2)WRITE(9,101)ABS(AP%IBR),NTOP+1,FNBTAE
101 FORMAT(I4,I6,9X,'BT   Function:',ES14.5)

  END FUNCTION FNBTAE

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNCPAE(AP,PAR,ICP,ITP,FUNI,U,AA)

    USE SUPPORT, ONLY: CHECKSP

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: U(AP%NDIM),AA(AP%NDM)
    INTEGER, INTENT(IN) :: ICP(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
! Local
    DOUBLE PRECISION, ALLOCATABLE :: F(:),UU(:),V(:)
    DOUBLE PRECISION DUM(1),H
    INTEGER NDM,NTOP,I

    FNCPAE = 0
    ITP=0
    IF(AP%ISW/=2.OR.AP%ITPST/=2.OR..NOT.CHECKSP('CP',AP%IPS,AP%ILP,AP%ISP))THEN
       RETURN
    ENDIF

    NDM=AP%NDM
    ALLOCATE(UU(NDM),F(NDM),V(NDM))

    CALL RNULLVC(AP,AA,V)

    ! Evaluate cusp function:
    H=0.d0
    DO I=1,NDM
       IF(ABS(U(I))>H)H=ABS(U(I))
    ENDDO
    H=(EPSILON(H)**(1d0/3))*(1+H)

    UU(:)=U(:NDM)+U(NDM+1:2*NDM)*H
    CALL FUNI(AP,NDM,UU,UU,ICP,PAR,0,F,DUM,DUM)
    FNCPAE=DOT_PRODUCT(V(:),F(:))
    UU(:)=U(:NDM)-U(NDM+1:2*NDM)*H
    CALL FUNI(AP,NDM,UU,UU,ICP,PAR,0,F,DUM,DUM)
    FNCPAE=(FNCPAE+DOT_PRODUCT(V(:),F(:)))/H**2

    DEALLOCATE(UU,F,V)
    ITP=-22

    NTOP=MOD(AP%NTOT-1,9999)+1
    IF(AP%IID.GE.2)WRITE(9,101)ABS(AP%IBR),NTOP+1,FNCPAE
101 FORMAT(I4,I6,9X,'Cusp Function:',ES14.5)

  END FUNCTION FNCPAE

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNGHAE(AP,PAR,ICP,ITP,FUNI,U,AA)

    ! Evaluate first Lyapunov coefficient for Bautin (GH) bifurcations
    ! and to determine if the Hopf is subcritical or supercritical.

    USE SUPPORT, ONLY: NRMLZ, NLVC, GEL, CHECKSP

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: U(AP%NDIM),AA(AP%NDIM+1,AP%NDIM+1)
    INTEGER, INTENT(IN) :: ICP(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
! Local
    DOUBLE PRECISION, ALLOCATABLE :: pR(:),qR(:),pI(:),qI(:),sR(:),sI(:),r(:)
    DOUBLE PRECISION, ALLOCATABLE :: a(:),b(:),c(:),abc(:),tmp(:)
    DOUBLE PRECISION, ALLOCATABLE :: f1(:),f2(:),x(:),SMAT(:,:),A1(:,:)
    DOUBLE PRECISION alpha,beta,DUM(1),h,omega,DET,phi
    DOUBLE PRECISION delta1,delta2,delta3,delta4,Delta
    DOUBLE PRECISION gamma1,gamma2,gamma3,gamma4,Gamma
    DOUBLE PRECISION sigma1,sigma2,Sigma
    INTEGER n,i,NTOP

    FNGHAE = 0
    ITP = 0
    IF(AP%ISW/=2.OR.AP%ITPST/=3.OR..NOT.CHECKSP('GH',AP%IPS,AP%ILP,AP%ISP))THEN
       RETURN
    ENDIF
    IF((U(AP%NDIM-1))<=0)RETURN
    omega = SQRT(U(AP%NDIM-1))

    n = AP%NDM
    ALLOCATE(pI(n),pR(n),qR(n),qI(n),sR(n),sI(n),f1(n),f2(n),x(n),r(n))
    ALLOCATE(a(n),b(n),c(n),abc(2*n),tmp(2*n),SMAT(2*n,2*n),A1(n,n))

    h = 0.d0
    DO i = 1,n
       IF(ABS(U(i))>h) h = ABS(U(i))
    ENDDO
    h = (EPSILON(h)**(1d0/3))*(1+h)

    ! Following Kuznetsov, Elements of Applied Bif. Theory, 3rd Ed., 10.2
    ! Step 1
    qR = U(n+1:2*n)
    ! qI=-AA.qR/omega
    CALL DGEMV('N',n,n,-1/omega,AA,AP%NDIM+1,qR,1,0d0,qI,1)

    ! normalize so that <qR,qI>=0
    phi = ATAN2(-2*DOT_PRODUCT(qR,qI),1-DOT_PRODUCT(qI,qI))/2
    alpha = COS(phi)
    beta = SIN(phi)
    tmp(:n) = alpha*qR-beta*qI
    tmp(n+1:) = alpha*qI+beta*qR

    ! normalize so that <qR,qR>+<qI,qI>=1
    CALL NRMLZ(2*n,tmp)
    qR = tmp(:n)
    qI = tmp(n+1:)

    ! compute pR,pI
    SMAT(:,:) = 0.d0
    DO i = 1,n
       SMAT(I,n+i) = -OMEGA
       SMAT(n+I,I) = OMEGA
    ENDDO
    DO i = 1,n
       SMAT(1:n,i) = AA(i,1:n)
    ENDDO
    SMAT(n+1:2*n,n+1:2*n) = SMAT(1:n,1:n)
    CALL NLVC(2*n,2*n,2,SMAT,tmp)
    pR = tmp(:n)
    pI = tmp(n+1:)

    ! normalize so that <pR,qI>-<pI,qR>=0
    alpha = DOT_PRODUCT(pR,qR)+DOT_PRODUCT(pI,qI)
    beta = DOT_PRODUCT(pI,qR)-DOT_PRODUCT(pR,qI)
    tmp(:n) = alpha*pR+beta*pI
    tmp(n+1:) = alpha*pI-beta*pR

    ! normalize so that <pR,qR>+<pI,qI>=1
    tmp(:) = tmp(:)/(DOT_PRODUCT(tmp(:n),qR(:))+DOT_PRODUCT(tmp(n+1:),qI(:)))
    pR = tmp(:n)
    pI = tmp(n+1:)

    ! Step 2
    a = DERIV2a(qR)
    b = DERIV2a(qI)
    c = DERIV2b(qR, qI)
    c = c/4

    ! Step 3
    A1(:,:) = AA(1:n,1:n)
    f1(:) =  a+b
    CALL GEL(n,A1,1,r,f1,DET)
    SMAT(:,:) = 0d0
    DO i = 1,n
       SMAT(i,n+i) = -2*omega
       SMAT(n+i,i) = 2*omega
    ENDDO
    SMAT(1:n,1:n) = -AA(1:n,1:n)
    SMAT(n+1:2*n,n+1:2*n) = SMAT(1:n,1:n)
    abc(:n) = a-b
    abc(n+1:) = 2*c
    CALL GEL(2*n,SMAT,1,tmp,abc,DET)
    sR = tmp(:n)
    sI = tmp(n+1:)

    ! Step 4
    sigma1 = DERIV2c(pR, qR, r)/4
    sigma2 = DERIV2c(pI, qI, r)/4
    Sigma = sigma1+sigma2

    ! Step 5
    delta1 = DERIV2c(pR, qR, sR)/4
    delta2 = DERIV2c(pR, qI, sI)/4
    delta3 = DERIV2c(pI, qR, sI)/4
    delta4 = DERIV2c(pI, qI, sR)/4
    Delta = delta1+delta2+delta3-delta4

    ! Step 6
    ! adjust h for third order derivatives
    h = h * EPSILON(h)**(-1d0/12)
    gamma1 = DERIV3(pR, qR)
    gamma2 = DERIV3(pI, qI)
    sR = pR + pI
    sI = qR + qI
    gamma3 = DERIV3(sR, sI)
    sR = pR - pI
    sI = qR - qI
    gamma4 = DERIV3(sR, sI)
    Gamma = ((gamma1+gamma2)*2)/3 + (gamma3+gamma4)/6

    ! Step 7
    FNGHAE = (Gamma-2*Sigma+Delta)/(2*omega)

    DEALLOCATE(pI,pR,qR,qI,sR,sI,f1,f2,x,r,a,b,c,abc,tmp,SMAT,A1)
    ITP=35

    IF(AP%IID>=2)THEN
       NTOP=MOD(AP%NTOT-1,9999)+1
       WRITE(9,"(I4,I6,9X,A,ES14.5)",ADVANCE="no")&
            ABS(AP%IBR),NTOP+1,'GH   Function:',FNGHAE
       IF(FNGHAE>0)THEN
          WRITE(9,'(A)')' (subcritical)'
       ELSEIF(FNGHAE<0)THEN
          WRITE(9,'(A)')' (supercritical)'
       ELSE
          WRITE(9,*)
       ENDIF
    ENDIF

    CONTAINS
      
      FUNCTION FN(x) RESULT(f)
        DOUBLE PRECISION, INTENT(INOUT) :: x(n)
        DOUBLE PRECISION f(size(x))

        CALL FUNI(AP,n,x,x,ICP,PAR,0,f,dum,dum)
      END FUNCTION FN

      FUNCTION DERIV2a(p) RESULT(d)
        DOUBLE PRECISION, INTENT(IN) :: p(n)
        DOUBLE PRECISION d(size(p))

        x(:) = U(:n) + h*p
        f1 = FN(x)
        x(:) = U(:n) - h*p
        f2 = FN(x)
        d = (f1+f2)/h**2
      END FUNCTION DERIV2a

      FUNCTION DERIV2b(p, q) RESULT(d)
        DOUBLE PRECISION, INTENT(IN) :: p(n), q(n)
        DOUBLE PRECISION d(size(p))

        x(:) = U(:n) + h*(p+q)
        f1 = FN(x)
        x(:) = U(:n) + h*(p-q)
        f2 = FN(x)
        d = f1-f2

        x(:) = U(:n) - h*(p+q)
        f1 = FN(x)
        x(:) = U(:n) - h*(p-q)
        f2 = FN(x)
        d = (d + f1-f2)/h**2
      END FUNCTION DERIV2b

      FUNCTION DERIV2c(p, q, r) RESULT(d)
        DOUBLE PRECISION, INTENT(IN) :: p(n), q(n), r(n)
        DOUBLE PRECISION d

        x(:) = U(:n) + h*(q+r)
        f1 = FN(x)
        x(:) = U(:n) + h*(q-r)
        f2 = FN(x)
        d = DOT_PRODUCT(p, f1-f2)

        x(:) = U(:n) - h*(q+r)
        f1 = FN(x)
        x(:) = U(:n) - h*(q-r)
        f2 = FN(x)
        d = (d + DOT_PRODUCT(p, f1-f2))/h**2
      END FUNCTION DERIV2c

      FUNCTION DERIV3(p, q) RESULT(d)
        DOUBLE PRECISION, INTENT(IN) :: p(n), q(n)
        DOUBLE PRECISION d

        x(:) = U(:n) + 3*h*q
        f1 = FN(x)
        d = DOT_PRODUCT(p,f1)

        x(:) = U(:n) + h*q
        f1 = FN(x)
        d = d - 3*DOT_PRODUCT(p, f1)

        x(:) = U(:n) - h*q
        f1 = FN(x)
        d = d + 3*DOT_PRODUCT(p, f1)

        x(:) = U(:n) - 3*h*q
        f1 = FN(x)
        d = (d-DOT_PRODUCT(p, f1))/(8*h**3)
      END FUNCTION DERIV3

  END FUNCTION FNGHAE

! ------- -------- ------
  INTEGER FUNCTION TPSPAE(EPSS,ITPST,PERIOD)

! Determines type of secondary bifurcation of maps.
    
    USE SUPPORT, ONLY: PI

    INTEGER, INTENT(IN) :: ITPST
    DOUBLE PRECISION, INTENT(IN) :: EPSS, PERIOD

    IF(PERIOD-2 <= PERIOD/PI(1d0)*SQRT(EPSS))THEN
!       ** period doubling
       TPSPAE=7+10*ITPST
    ELSEIF(PERIOD /= 0 .AND. PERIOD < PI(2d0)/SQRT(EPSS))THEN
!       ** torus (Neimark-Sacker) bifurcation
       TPSPAE=8+10*ITPST
    ELSE
!       ** something else... (very large PERIOD: close to fold)
       TPSPAE=0
    ENDIF

  END FUNCTION TPSPAE

END MODULE EQUILIBRIUM
