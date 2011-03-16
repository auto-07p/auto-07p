!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Optimization
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE OPTIMIZATION

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE AE
  USE BVP
  USE INTERFACES
  USE TOOLBOXAE
  USE TOOLBOXBV
  USE PERIODIC
  USE SUPPORT

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: AUTOOP

  DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

  INTERFACE
     SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP)
       INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
       DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
       DOUBLE PRECISION, INTENT(OUT) :: FS
       DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM),DFDP(*)
     END SUBROUTINE FOPT
  END INTERFACE

CONTAINS

! ---------- ------
  SUBROUTINE AUTOOP(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    INTEGER IPS, ISW, ITP, NDIM, NFPR, NFPRPREV, I, IC, JC, NNEG
    IPS = AP%IPS
    ISW = AP%ISW
    ITP = AP%ITP
    NDIM = AP%NDIM
    NFPR = AP%NFPR

    IF(IPS==15.AND.ABS(ISW)==1) THEN
       ! ** Optimization of periodic solutions.
       NFPRPREV=NFPR
       NFPR=0
       DO I=1,AP%NICP
          IF(ICU(I)>0)THEN
             NFPR=NFPR+1
             ICP(NFPR)=ICU(I)
          ENDIF
       ENDDO
       ICP(NFPR+1)=10
       ICP(NFPR+2)=13
       ICP(NFPR+3)=14
       NFPR=NFPR+3
       AP%NFPR=NFPR
       AP%NDIM=2*NDIM
       AP%NBC=AP%NDIM
       AP%NINT=NFPR-1
       ! overload to define optimality integrals
       NNEG=0
       DO I=1,AP%NICP
          IC=ICU(I)
          JC=ABS(IC)-20        
          IF(IC<0.AND.JC>0.AND.JC<=11)THEN
             NNEG=NNEG+1
             ICP(NFPR+NNEG)=JC
          ENDIF
       ENDDO
       AP%NICP=NFPR-3
       IF(NFPRPREV<6)THEN
          CALL AUTOBV(AP,ICP,ICU,FNPO,BCPS,ICPO,STPNPO,FNCSBV)
       ELSE
          CALL AUTOBV(AP,ICP,ICU,FNPO,BCPS,ICPO,STPNBV,FNCSBV)
       ENDIF
    ELSE IF(IPS==5) THEN
       ! ** Algebraic optimization problems.
       IF(MOD(ITP,10)==2.OR.AP%IRS==0)NFPR=NFPR+1
       AP%NFPR=NFPR
       ICP(1)=10
       IF(NFPR==2) THEN
          AP%NDIM=NDIM+1
          IF(AP%IRS>0) THEN
             CALL AUTOAE(AP,ICP,ICU,FNC1,STPNAE,FNCSAE)
          ELSE
             CALL AUTOAE(AP,ICP,ICU,FNC1,STPNC1,FNCSAE)
          ENDIF
       ELSE
          AP%NDIM=2*NDIM+NFPR
          IF(MOD(ITP,10)/=2) THEN
             CALL AUTOAE(AP,ICP,ICU,FNC2,STPNAE,FNCSAE)
          ELSE
             CALL AUTOAE(AP,ICP,ICU,FNC2,STPNC2,FNCSAE)
          ENDIF
       ENDIF
    ENDIF
  END SUBROUTINE AUTOOP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Subroutines for the Optimization of Algebraic Systems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNC1(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generate the equations for the continuation scheme used for
    ! the optimization of algebraic systems (one parameter).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DDU(:),DDP(:)
    INTEGER JAC,NDM,NFPR,NPAR,I

    JAC=AP%JAC
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR
    ALLOCATE(DDU(NDM),DDP(NPAR))

    PAR(ICP(2))=U(NDIM)
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Rearrange (Since dimensions in FNC1 and FUNI differ).

    IF(IJAC.NE.0)THEN
       CALL EXPANDJAC(DFDU,NDM,NDM,NDIM)
       CALL EXPANDJAC(DFDP,NPAR,NDM,NDIM)
    ENDIF

    CALL FOPI(JAC,NFPR,NDM,U,ICP,PAR,IJAC,F(NDIM),DDU,DDP)
    F(NDIM)=PAR(ICP(1))-F(NDIM)

    IF(IJAC.NE.0)THEN
       DO I=1,NDM
          DFDU(NDIM,I)=-DDU(I)
          DFDU(I,NDIM)=DFDP(I,ICP(2))
          DFDP(I,ICP(1))=0
       ENDDO
       DFDU(NDIM,NDIM)=-DDP(ICP(2))
       DFDP(NDIM,ICP(1))=1
    ENDIF

    DEALLOCATE(DDU,DDP)
  END SUBROUTINE FNC1

! ---------- ------
  SUBROUTINE STPNC1(AP,PAR,ICP,U,UDOT,NODIR)

    ! Generate starting data for optimization problems (one parameter).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)
    ! Local
    INTEGER NDIM,JAC,NDM,NFPR
    DOUBLE PRECISION DUM(1),T,FOP

    NDIM=AP%NDIM
    JAC=AP%JAC
    NDM=AP%NDM
    NFPR=AP%NFPR

    T=0.d0
    U(:NDIM)=0.d0
    CALL STPNT(NDIM,U,PAR,T)
    CALL FOPI(JAC,NFPR,NDM,U,ICP,PAR,0,FOP,DUM,DUM)
    PAR(ICP(1))=FOP
    U(NDIM)=PAR(ICP(2))
    NODIR=1
    UDOT(:NDIM)=0d0

  END SUBROUTINE STPNC1

! ---------- ----
  SUBROUTINE FNC2(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generate the equations for the continuation scheme used for the
    ! optimization of algebraic systems (more than one parameter).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DDP(:),DFU(:),DFP(:),FF1(:),FF2(:)
    INTEGER NDM,NPAR,I,J
    DOUBLE PRECISION UMX,EP,UU

    NDM=AP%NDM
    NPAR=AP%NPAR

    ! Generate the function.

    ALLOCATE(DDP(NPAR))
    CALL FFC2(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP,DDP)

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(DDP)
       RETURN
    ENDIF

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPAR),FF1(NDIM),FF2(NDIM))
    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FFC2(AP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP,DDP)
       U(I)=UU+EP
       CALL FFC2(AP,NDIM,U,UOLD,ICP,PAR,FF2,NDM,DFU,DFP,DDP)
       U(I)=UU
       DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO
    DEALLOCATE(DFU,DFP,FF1,FF2,DDP)

    IF (IJAC.EQ.1)RETURN

    DO I=1,NDIM
       DFDP(I,ICP(1))=0.d0
    ENDDO
    DFDP(NDIM,ICP(1))=1.d0

  END SUBROUTINE FNC2

! ---------- ----
  SUBROUTINE FFC2(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP,DDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*),DDP(AP%NPAR)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    ! Local
    INTEGER JAC,NFPR,NPAR,NDM2,ICPM,I,J
    DOUBLE PRECISION FOP

    JAC=AP%JAC
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    DO I=2,NFPR
       PAR(ICP(I))=U(2*NDM+I)
    ENDDO
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
    CALL FOPI(JAC,NFPR,NDM,U,ICP,PAR,2,FOP,F(NDM+1),DDP)

    DO I=1,NDM
       F(NDM+I)=F(NDM+I)*U(2*NDM+1)
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(J,I)*U(NDM+J)
       ENDDO
    ENDDO

    NDM2=2*NDM
    ICPM=NFPR-2
    DO I=1,ICPM
       F(NDM2+I)=DDP(ICP(I+1))*U(NDM2+1)
    ENDDO

    DO I=1,ICPM
       DO J=1,NDM
          F(NDM2+I)=F(NDM2+I)+U(NDM+J)*DFDP(J,ICP(I+1))
       ENDDO
    ENDDO

    F(NDIM-1)=U(NDM2+1)*U(NDM2+1)-1
    DO J=1,NDM
       F(NDIM-1)=F(NDIM-1)+U(NDM+J)*U(NDM+J)
    ENDDO
    F(NDIM)=PAR(ICP(1))-FOP

  END SUBROUTINE FFC2

! ---------- ------
  SUBROUTINE STPNC2(AP,PAR,ICP,U,UDOT,NODIR)

    USE IO
    USE SUPPORT

    ! Generates starting data for the continuation equations for
    ! optimization of algebraic systems (More than one parameter).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:),DFP(:),DD(:,:),DU(:),V(:),F(:),DP(:)
    INTEGER, ALLOCATABLE :: ICPRS(:)
    INTEGER NDIM,JAC,NDM,NFPR,NPAR,I,J
    DOUBLE PRECISION FOP

    NDIM=AP%NDIM
    JAC=AP%JAC
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ALLOCATE(ICPRS(NFPR))
    CALL READLB(AP,ICPRS,U,UDOT,PAR)
    DEALLOCATE(ICPRS)

    IF(NFPR.EQ.3)THEN
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPAR),F(NDM),V(NDM+1))
       ALLOCATE(DD(NDM+1,NDM+1),DU(NDM),DP(NPAR))
       CALL FUNI(AP,NDM,U,U,ICP,PAR,2,F,DFU,DFP)
       CALL FOPI(JAC,NFPR,NDM,U,ICP,PAR,2,FOP,DU,DP)
       !       TRANSPOSE
       DO I=1,NDM
          DO J=1,NDM
             DD(I,J)=DFU((I-1)*NDM+J)
          ENDDO
       ENDDO
       DO I=1,NDM
          DD(I,NDM+1)=DU(I)
          DD(NDM+1,I)=DFP((ICP(2)-1)*NDM+I)
       ENDDO
       DD(NDM+1,NDM+1)=DP(ICP(2))
       CALL NLVC(NDM+1,NDM+1,1,DD,V)
       CALL NRMLZ(NDM+1,V)
       DO I=1,NDM+1
          U(NDM+I)=V(I)
       ENDDO
       PAR(ICP(1))=FOP
       DEALLOCATE(DFU,DFP,F,V,DD,DU,DP)
    ENDIF

    DO I=1,NFPR-1
       U(NDIM-NFPR+1+I)=PAR(ICP(I+1))
    ENDDO

    NODIR=1
  END SUBROUTINE STPNC2

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Optimization of Periodic Solutions
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNPO(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for periodic optimization problems.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:),FF1(:),FF2(:),UPOLD(:)
    INTEGER NDM,NFPR,I,J
    DOUBLE PRECISION UMX,EP,P,PERIOD,UU,DUMDU(1),DUMDP(1)

    NDM=AP%NDM
    NFPR=AP%NFPR

    ! Generate F(UOLD)

    ALLOCATE(UPOLD(NDIM))
    CALL FUNC(NDM,UOLD,ICP,PAR,0,UPOLD,DUMDU,DUMDP)
    PERIOD=PAR(11)
    DO I=1,NDM
       UPOLD(I)=PERIOD*UPOLD(I)
    ENDDO

    ! Generate the function.

    CALL FFPO(AP,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFDU)

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(UPOLD)
       RETURN
    ENDIF

    ALLOCATE(DFU(NDIM*NDIM),FF1(NDIM),FF2(NDIM))

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FFPO(AP,U,UOLD,UPOLD,ICP,PAR,FF1,NDM,DFU)
       U(I)=UU+EP
       CALL FFPO(AP,U,UOLD,UPOLD,ICP,PAR,FF2,NDM,DFU)
       U(I)=UU
       DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(UPOLD,DFU,FF1)
       RETURN
    ENDIF

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FFPO(AP,U,UOLD,UPOLD,ICP,PAR,FF1,NDM,DFU)
       DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(UPOLD,DFU,FF1)
  END SUBROUTINE FNPO

! ---------- ----
  SUBROUTINE FFPO(AP,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFDU)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*),UPOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(*),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDM*2)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,*)
    ! Local
    INTEGER JAC,NFPR,I,J
    DOUBLE PRECISION DUMDP(1),PERIOD,RKAPPA,GAMMA,DFU,FOP

    JAC=AP%JAC
    NFPR=AP%NFPR
    PERIOD=PAR(11)
    RKAPPA=PAR(13)
    GAMMA =PAR(14)

    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,1,F(NDM+1),DFDU,DUMDP)
    CALL FOPI(JAC,NFPR,NDM,U,ICP,PAR,1,FOP,F,DUMDP)

    DO I=1,NDM
       DFU=F(I)
       F(I)=F(NDM+I)
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
       ENDDO
       F(I)=PERIOD*F(I)
       F(NDM+I)=PERIOD*F(NDM+I)+ RKAPPA*UPOLD(I) + GAMMA*DFU
    ENDDO

  END SUBROUTINE FFPO

! ---------- ----
  SUBROUTINE ICPO(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Generates integral conditions for periodic optimization problems.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:),DFP(:),F1(:),F2(:)
    INTEGER NPAR,NDM,NFPR,I,J
    DOUBLE PRECISION UMX,EP,P,UU

    NPAR=AP%NPAR
    ALLOCATE(DFU(NDIM*NDIM),DFP(NDIM*NPAR))

    NDM=AP%NDM
    NFPR=AP%NFPR

    ! Generate the function.

    CALL FIPO(AP,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,NDM,DFU,DFP)

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(DFU,DFP)
       RETURN
    ENDIF

    ! Generate the Jacobian.

    ALLOCATE(F1(NINT),F2(NINT))
    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FIPO(AP,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F1,NDM,DFU,DFP)
       U(I)=UU+EP
       CALL FIPO(AP,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F2,NDM,DFU,DFP)
       U(I)=UU
       DO J=1,NINT
          DINT(J,I)=(F2(J)-F1(J))/(2*EP)
       ENDDO
    ENDDO

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FIPO(AP,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F1,NDM,DFU,DFP)
       DO J=1,NINT
          DINT(J,NDIM+ICP(I))=(F1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(F1,F2,DFU,DFP)
  END SUBROUTINE ICPO

! ---------- ----
  SUBROUTINE FIPO(AP,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,NDM,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDM,NINT
    DOUBLE PRECISION, INTENT(IN)::UOLD(2*NDM),UDOT(2*NDM),UPOLD(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(2*NDM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)

    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFP(:),F(:)
    INTEGER JAC,NFPR,NPAR,INDX,I,J,L
    DOUBLE PRECISION FOP

    JAC=AP%JAC
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    FI(1)=0.d0
    DO I=1,NDM
       FI(1)=FI(1)+(U(I)-UOLD(I))*UPOLD(I)
    ENDDO

    ALLOCATE(F(NDM),DFP(NPAR))
    DO I=1,NPAR
       DFP(I)=0.d0
    ENDDO
    CALL FOPI(JAC,NFPR,NDM,U,ICP,PAR,2,FOP,F,DFP)
    FI(2)=PAR(10)-FOP

    FI(3)=PAR(13)**2+PAR(14)**2-PAR(12)
    DO I=1,NDM
       FI(3)=FI(3)+U(NDM+I)**2
    ENDDO

    DO I=1,NDM
       DO J=1,NPAR
          DFDP(I,J)=0.d0
       ENDDO
    ENDDO
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)

    DO L=4,NINT
       INDX=ICP(NFPR+L-3)
       IF(INDX.EQ.11)THEN
          FI(L)=-PAR(14)*DFP(INDX) - PAR(20+INDX)
          DO I=1,NDM
             FI(L)=FI(L)+F(I)*U(NDM+I)
          ENDDO
       ELSE
          FI(L)=-PAR(14)*DFP(INDX) - PAR(20+INDX)
          DO I=1,NDM
             FI(L)=FI(L)+PAR(11)*DFDP(I,INDX)*U(NDM+I)
          ENDDO
       ENDIF
    ENDDO

    DEALLOCATE(DFP,F)
  END SUBROUTINE FIPO

! ---------- ------
  SUBROUTINE STPNPO(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE BVP
    USE IO
    USE MESH

    ! Generates starting data for optimization of periodic solutions.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    INTEGER, ALLOCATABLE :: ICPRS(:)
    DOUBLE PRECISION, ALLOCATABLE :: U(:),TEMP(:),DTMTEMP(:)
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER NDIM,NTST,NCOL,NDM,NFPR,NPAR,NDIMRD,ITPRS,I,J,K
    DOUBLE PRECISION FS,DUMU(1),DUMP(1)

    NDIM=AP%NDIM
    NTST=AP%NTST
    NCOL=AP%NCOL
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ALLOCATE(ICPRS(NFPR))
    ALLOCATE(UPSR(NDM,0:NCOLRS*NTSR),UDOTPSR(NDM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOT,UPSR, &
         UDOTPSR,TMR,ITPRS,NDM)
    DEALLOCATE(ICPRS)
    ALLOCATE(U(NDM),TEMP(0:NTSR*NCOLRS),DTMTEMP(NTSR))
    DO J=1,NTSR
       DTMTEMP(J)=TMR(J)-TMR(J-1)
    ENDDO

    ! Compute the starting value of the objective functional
    DO J=0,NTSR*NCOLRS
       DO K=1,NDM
          U(K)=UPSR(K,J)
       ENDDO
       CALL FOPT(NDM,U,ICP,PAR,0,FS,DUMU,DUMP)
       TEMP(J)=FS
    ENDDO
    PAR(10)=RINTG(NTSR,NCOLRS,1,1,TEMP,DTMTEMP)
    DEALLOCATE(U,TEMP,DTMTEMP)

    ! Complement starting data

    DO I=12,NPAR
       PAR(I)=0.d0
    ENDDO

    NODIR=1
    CALL ADAPT2(NTSR,NCOLRS,NDM,NTST,NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNPO

! ---------- ----
  SUBROUTINE FOPI(JAC,NFPR,NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Interface subroutine to user supplied FOPT.

    INTEGER, INTENT(IN) :: JAC,NFPR,NDIM,ICP(*),IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U(*),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM),DFDP(*)
    ! Local
    INTEGER IJC,I
    DOUBLE PRECISION UU,UMX,EP,P,F1,F2


    ! Generate the objective function.

    IF(JAC.EQ.0)THEN
       IJC=0
    ELSE
       IJC=IJAC
    ENDIF
    CALL FOPT(NDIM,U,ICP,PAR,IJC,F,DFDU,DFDP)

    IF(JAC.EQ.1 .OR. IJAC.EQ.0)RETURN

    ! Generate the Jacobian by differencing.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FOPT(NDIM,U,ICP,PAR,0,F1,DFDU,DFDP)
       U(I)=UU+EP
       CALL FOPT(NDIM,U,ICP,PAR,0,F2,DFDU,DFDP)
       U(I)=UU
       DFDU(I)=(F2-F1)/(2*EP)
    ENDDO

    IF(IJAC.EQ.1)RETURN

    DO I=1,NFPR
       P=PAR(ICP(I))
       EP=HMACH*( 1 +ABS(P) )
       PAR(ICP(I))=P+EP
       CALL FOPT(NDIM,U,ICP,PAR,0,F1,DFDU,DFDP)
       DFDP(ICP(I))=(F1-F)/EP
       PAR(ICP(I))=P
    ENDDO

  END SUBROUTINE FOPI

END MODULE OPTIMIZATION
