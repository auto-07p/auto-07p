!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Periodic Solutions and Fixed Period Orbits
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE PERIODIC

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE INTERFACES
  USE TOOLBOXBV
  USE BVP
  USE SUPPORT

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: AUTOPS,INITPS
  PUBLIC :: BCPS,ICPS,STPNPS,FNCSPS ! Periodic solutions
  PUBLIC :: FNPLF,BCPL,ICPL,STPNPL ! Fold cont of periodic sol

  DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

CONTAINS

! ---------- ------
  SUBROUTINE INITPS(AP,ICP)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(*)

    INTEGER IPS, ITP, ISW, NICP, NDIM, NBC, NINT, NPAR, NPARI

    IPS = AP%IPS
    ITP = AP%ITP
    ISW = AP%ISW
    NICP = AP%NICP
    NDIM = AP%NDIM
    NBC = AP%NBC
    NINT = AP%NINT
    NPAR = AP%NPAR
    NPARI = AP%NPARI

    IF(ABS(ISW)<=1)THEN
       IF(IPS/=7)THEN
          ! ** Periodic Solutions
          NBC=NDIM
          NINT=1
          ! **ISW=1 when starting from a HB
          IF(ITP==3.OR.(ABS(ITP)/10)==3)ISW=1
          IF(NICP==1)THEN
             ! **Variable period
             ICP(2)=11
          ENDIF
       ENDIF
    ELSE
       IF(ABS(ITP)/10==5 .OR. ITP==5)THEN
          NDIM=2*NDIM
          NBC=NDIM
          NINT=3
          NPARI=2
          IF(ICP(3)==11 .OR. NICP==2)THEN
             ! ** Variable period
             IF( ITP/=5 )ICP(3)=ICP(2)
             ICP(2)=11
          ENDIF
          IF( ITP==5 )THEN
             ! ** Fold continuation (Periodic solutions); start
             ISW=-2
             ICP(3)=NPAR+2
          ENDIF
          ICP(4)=NPAR+1
       ELSEIF(ITP==6.OR.ABS(ITP)/10==6)THEN
          ! ** BP cont (Periodic solutions); start/restart (by F. Dercole)
          IF(ABS(ISW)==2)THEN
             ! ** Non-generic case
             IF(ICP(3)==11 .OR. NICP==2)THEN
                ! ** Variable period
                ICP(3)=ICP(2)
                ICP(2)=11 ! T
             ENDIF
             ICP(4)=NPAR+3 ! b
             NPARI=3
          ELSE
             ! ** Generic case
             IF(ICP(4)==11 .OR. NICP==3)THEN
                ! ** Variable period
                ICP(4)=ICP(3)
                ICP(3)=ICP(2)
                ICP(2)=11 ! T
             ENDIF
             NPARI=2
          ENDIF
          ICP(5)=NPAR+1     ! phi^*_3
          NDIM=2*NDIM
          NINT=4
          IF(ITP==6)THEN
             ! start
             NDIM=2*NDIM
             NINT=10
             ICP(3)=NPAR+2   ! a
             ICP(4)=NPAR+3   ! b
             ICP(6)=NPAR+4   ! q1
             ICP(7)=NPAR+5   ! q2/beta1
             ICP(8)=NPAR+6   ! r1
             ICP(9)=NPAR+7   ! r2/beta2
             ICP(10)=NPAR+8  ! c1
             ICP(11)=NPAR+9  ! c2
             ISW=-ABS(ISW)
             NPARI=9
          ENDIF
          NBC=NDIM
       ELSEIF(ITP==7.OR.ABS(ITP)/10==7)THEN
          ! ** Continuation of period doubling bifurcations; start/restart
          NDIM=2*NDIM
          NBC=NDIM
          NINT=2
          IF(ITP==7)THEN
             ! start
             IF(ICP(3)==11 .OR. NICP==2)THEN
                ! ** Variable period
                ICP(2)=11
             ENDIF
             ICP(3)=NPAR+1
             ISW=-2
          ELSEIF(NICP==2)THEN
             ! ** restart with variable period
             ICP(3)=11
          ENDIF
          NPARI=1
       ELSE IF(ITP==8.OR.ABS(ITP)/10==8)THEN
          ! ** Continuation of torus bifurcations; start/restart
          NDIM=3*NDIM
          NBC=NDIM
          NINT=3
          IF(ITP==8)THEN
             ! start
             ICP(2)=11
             ICP(3)=12
             ICP(4)=NPAR+1
             ISW=-2
          ELSEIF(NICP<4)THEN
             ! **restart if not specified by user
             ICP(3)=11
             ICP(4)=12
          ENDIF
          NPARI=1
       ENDIF
    ENDIF
    AP%NDIM=NDIM
    AP%NBC=NBC
    AP%NINT=NINT
    AP%NFPR=NBC+NINT-NDIM+1
    AP%NPARI=NPARI
    AP%ISW=ISW
    AP%NTEST=3

  END SUBROUTINE INITPS

! ---------- ------
  SUBROUTINE AUTOPS(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    INTEGER IPS, ISW, ITP

    CALL INITPS(AP,ICP)
    IPS = AP%IPS
    ISW = AP%ISW
    ITP = AP%ITP

    IF(ABS(ISW)<=1)THEN
       IF(IPS==2)THEN
          ! ** Periodic solutions
          CALL AUTOBV(AP,ICP,ICU,FNPS,BCPS,ICPS,STPNPS,FNCSPS)
       ELSE 
          ! Boundary value problems with Floquet multipliers. (IPS=7)
          CALL AUTOBV(AP,ICP,ICU,FUNI,BCNI,ICNI,STPNPS,FNCSPS)
       ENDIF
    ELSE ! here IPS=2
       IF(ABS(ISW)==2)THEN
          IF(ITP==5) THEN 
             ! ** Fold continuation (Periodic solutions, start).
             CALL AUTOBV(AP,ICP,ICU,FNPL,BCPL,ICPL,STPNPL,FNCSPS)
          ELSE IF((ABS(ITP)/10)==5)THEN
             ! ** Fold continuation (Periodic solutions, restart).
             CALL AUTOBV(AP,ICP,ICU,FNPL,BCPL,ICPL,STPNBV,FNCSPS)
          ELSE IF(ITP==7) THEN
             ! ** Continuation of period doubling bifurcations (start).
             CALL AUTOBV(AP,ICP,ICU,FNPD,BCPD,ICPD,STPNPD,FNCSPS)
          ELSE IF(ABS(ITP)/10==7)THEN
             ! ** Continuation of period doubling bifurcations (restart).
             CALL AUTOBV(AP,ICP,ICU,FNPD,BCPD,ICPD,STPNBV,FNCSPS)
          ELSE IF(ITP==8)THEN
             ! ** Continuation of torus bifurcations (start).
             CALL AUTOBV(AP,ICP,ICU,FNTR,BCTR,ICTR,STPNTR,FNCSPS)
          ELSE IF(ABS(ITP)/10==8)THEN
             ! ** Continuation of torus bifurcations (restart).
             CALL AUTOBV(AP,ICP,ICU,FNTR,BCTR,ICTR,STPNBV,FNCSPS)
          ENDIF
       ENDIF
       IF(ITP==6.OR.(ABS(ITP)/10)==6) THEN
          ! ** BP cont (Periodic sol., start and restart) (by F. Dercole).
          CALL AUTOBV(AP,ICP,ICU,FNPBP,BCPBP,ICPBP,STPNPBP,FNCSPS)
       ENDIF
    ENDIF
  END SUBROUTINE AUTOPS

! ---------- ----
  SUBROUTINE FNPS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the continuation of periodic orbits.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    INTEGER J
    DOUBLE PRECISION PERIOD

    ! Generate the function.

    CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,ABS(IJAC),F,DFDU,DFDP)
    PERIOD=PAR(11)
    IF(IJAC==2.OR.IJAC==-1)THEN
       DO J=1,AP%NFPR
          IF(ICP(J)==11)THEN
             !          **Variable period continuation
             DFDP(:,11)=F(:)
          ELSEIF(IJAC==2.AND.ICP(J)<=AP%NPAR-AP%NPARI)THEN
             DFDP(:,ICP(J))=PERIOD*DFDP(:,ICP(J))
          ENDIF
       ENDDO
    ENDIF
    F(:)=PERIOD*F(:)
    IF(IJAC.EQ.0)RETURN
    !      **Generate the Jacobian.
    DFDU(:,:)=PERIOD*DFDU(:,:)

  END SUBROUTINE FNPS

! ---------- ----
  SUBROUTINE BCPS(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    USE BVP, ONLY: IRTN, NRTN
    USE SUPPORT, ONLY : PI

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    INTEGER NFPR,I,J

    NFPR=AP%NFPR

    DO I=1,NBC
       F(I)=U0(I)-U1(I)
    ENDDO

    ! Rotations
    IF(IRTN.NE.0)THEN
       DO I=1,NBC/2
          IF(NRTN(I).NE.0)F(I)=F(I) + PI(2.d0)*NRTN(I)
       ENDDO
    ENDIF

    IF(IJAC.EQ.0)RETURN

    DO I=1,NBC
       DO J=1,2*NDIM
          DBC(I,J)=0.d0
       ENDDO
       DBC(I,I)=1.D0
       DBC(I,NDIM+I)=-1.d0
       DO J=1,NFPR
          DBC(I,2*NDIM+ICP(J))=0.d0
       ENDDO
    ENDDO

  END SUBROUTINE BCPS

! ---------- ----
  SUBROUTINE ICPS(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    INTEGER NPAR,NN,I

    F(1)=0.d0
    DO I=1,NDIM
       F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    NPAR=AP%NPAR
    NN=NDIM+NPAR
    DO I=1,NN
       DINT(1,I)=0.d0
    ENDDO

    DO I=1,NDIM
       DINT(1,I)=UPOLD(I)
    ENDDO

  END SUBROUTINE ICPS

! ---------- -----
  SUBROUTINE PDBLE(NDIM,NTST,NCOL,UPS,UDOTPS,TM,PAR)

    ! Preprocesses restart data for switching branches at a period doubling


    INTEGER, INTENT(IN) :: NDIM,NCOL
    INTEGER, INTENT(INOUT) :: NTST
    DOUBLE PRECISION,INTENT(INOUT) :: TM(0:*),UPS(NDIM,0:*),&
         UDOTPS(NDIM,0:*),PAR(*)

    INTEGER NTC,I,J

    PAR(11)=2.d0*PAR(11)

    DO I=0,NTST-1
       TM(I)=.5d0*TM(I)
       TM(NTST+I)=.5d0+TM(I)
    ENDDO
    TM(2*NTST)=1

    NTC=NTST*NCOL
    DO J=0,NTC
       UPS(:,NTC+J)=   UPS(:,NTC)+   UPS(:,J)-   UPS(:,0)
       UDOTPS(:,NTC+J)=UDOTPS(:,NTC)+UDOTPS(:,J)-UDOTPS(:,0)
    ENDDO

    NTST=2*NTST

  END SUBROUTINE PDBLE

! ---------- ------
  SUBROUTINE STPNPS(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE BVP
    USE MESH

    ! Generates starting data for the continuation of a branch of periodic
    ! solutions.
    ! If IPS is not equal to 2 then the user must have supplied
    ! BCND, ICND, and period-scaled F in FUNC, and the user period-scaling of F
    ! must be taken into account.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)

    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER NDIM,IRS,ISW,ITP,NDIMRD,NTSR2

    NDIM=AP%NDIM
    IRS=AP%IRS
    ISW=AP%ISW
    ITP=AP%ITP

    IF(IRS==0)THEN
       CALL STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)
       RETURN
    ENDIF

    IF(ISW==-1.AND.ITP==7) THEN
       !               period doubling
       NTSR2=NTSR*2
    ELSE
       NTSR2=NTSR
    ENDIF
    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR2), &
         UDOTPSR(NDIM,0:NCOLRS*NTSR2),TMR(0:NTSR2))
    CALL STPNBV1(AP,PAR,ICP,NDIM,NTSR,NDIMRD,NCOLRS, &
         RLDOT,UPSR,UDOTPSR,TMR,NODIR)
    IF(ISW==-1.AND.ITP==7) THEN

       ! Special case : Preprocess restart data in case of branch switching
       ! at a period doubling bifurcation.

       CALL PDBLE(NDIM,NTSR,NCOLRS,UPSR,UDOTPSR,TMR,PAR)
    ENDIF
    CALL ADAPT2(NTSR,NCOLRS,NDIM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.TRUE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNPS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Subroutines for the Continuation of Folds for Periodic Solution
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNPL(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNPLF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FNPS)

  END SUBROUTINE FNPL

! ---------- -----
  SUBROUTINE FNPLF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:),FF1(:),FF2(:)
    INTEGER NDM,NFPR,NPAR,I,J
    DOUBLE PRECISION UMX,UU,EP,P

    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Generate the function.

    CALL FFPL(AP,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP,FUNI)

    IF(IJAC.EQ.0)RETURN

    ! Generate the Jacobian.

    CALL EXPANDJAC(DFDU,NDM,NDM,NDIM)
    CALL EXPANDJAC(DFDP,NPAR,NDM,NDIM)
    DFDU(1:NDM,NDM+1:NDIM)=0d0
    DFDU(NDM+1:NDIM,NDM+1:NDIM)=DFDU(1:NDM,1:NDM)
    IF(IJAC==2)THEN
       IF(ICP(2)==11)THEN
          DFDP(NDM+1:NDIM,11)= &
               (F(NDM+1:NDIM)-PAR(NPAR-1)*DFDP(1:NDM,11))/PAR(11)
       ENDIF
       DFDP(1:NDM,NPAR-1)=0d0
       DFDP(NDM+1:NDIM,NPAR-1)=DFDP(1:NDM,ICP(2))
       IF(ICP(3)==NPAR)THEN
          DFDP(1:NDM,NPAR)=0d0
       ENDIF
    ENDIF

    UMX=0.d0
    DO I=1,NDM
       IF(ABS(U(I))>UMX)UMX=ABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    ALLOCATE(DFU(NDM,NDM),DFP(NDM,NPAR),FF1(NDIM),FF2(NDIM))
    DO I=1,NDM
       UU=U(I)
       U(I)=UU-EP
       CALL FFPL(AP,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DFP,FUNI)
       U(I)=UU+EP
       CALL FFPL(AP,U,UOLD,ICP,PAR,0,FF2,NDM,DFU,DFP,FUNI)
       U(I)=UU
       DO J=NDM+1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF (IJAC.EQ.1)THEN
       DEALLOCATE(DFU,DFP,FF1)
       RETURN
    ENDIF

    DO I=1,NFPR-1
       IF(ICP(I)==11)CYCLE
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FFPL(AP,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DFP,FUNI)
       DO J=NDM+1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(DFU,DFP,FF1)
  END SUBROUTINE FNPLF

! ---------- ----
  SUBROUTINE FFPL(AP,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP,FUNI)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(2*NDM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    include 'interfaces.h'

    INTEGER IJC,I,J,NPAR

    IJC=IJAC
    IF(IJC==0)IJC=-1
    IF(ICP(2)/=11)IJC=2
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJC,F,DFDU,DFDP)

    NPAR=AP%NPAR
    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
       ENDDO
       F(NDM+I)=F(NDM+I)+PAR(NPAR-1)*DFDP(I,ICP(2))
    ENDDO

  END SUBROUTINE FFPL

! ---------- ----
  SUBROUTINE BCPL(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    ! Boundary conditions for continuing folds (Periodic solutions)

    USE BVP, ONLY: IRTN, NRTN
    USE SUPPORT, ONLY : PI

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    INTEGER NDM,NPAR,NN,I,J

    DO I=1,NDIM
       F(I)=U0(I)-U1(I)
    ENDDO

    ! Rotations
    IF(IRTN.NE.0)THEN
       NDM=AP%NDM
       DO I=1,NDM
          IF(NRTN(I).NE.0)F(I)=F(I) + PI(2.d0)*NRTN(I)
       ENDDO
    ENDIF

    IF(IJAC.EQ.0)RETURN

    NPAR=AP%NPAR
    NN=2*NDIM+NPAR
    DO I=1,NBC
       DO J=1,NN
          DBC(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDIM
       DBC(I,I)=1
       DBC(I,NDIM+I)=-1
    ENDDO

  END SUBROUTINE BCPL

! ---------- ----
  SUBROUTINE ICPL(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Integral conditions for continuing folds (Periodic solutions)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    INTEGER NDM,NPAR,NN,I,J

    NDM=AP%NDM
    NPAR=AP%NPAR

    F(1)=0.d0
    F(2)=0.d0
    F(3)=PAR(NPAR-1)**2 - PAR(NPAR)

    DO I=1,NDM
       F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
       F(2)=F(2)+U(NDM+I)*UPOLD(I)
       F(3)=F(3)+U(NDM+I)*U(NDM+I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    NN=NDIM+NPAR
    DO I=1,NINT
       DO J=1,NN
          DINT(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDM
       DINT(1,I)=UPOLD(I)
       DINT(2,NDM+I)=UPOLD(I)
       DINT(3,NDM+I)=2.d0*U(NDM+I)
    ENDDO

    DINT(3,NDIM+NPAR-1)=2.d0*PAR(NPAR-1)
    DINT(3,NDIM+NPAR)=-1.d0

  END SUBROUTINE ICPL

! ---------- ------
  SUBROUTINE STPNPL(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE BVP
    USE IO
    USE MESH

    ! Generates starting data for the 2-parameter continuation of folds
    ! on a branch of periodic solutions.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    INTEGER ICPRS(4),NDIM,NDM,ITPRS,NDIMRD,NPAR
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)

    NDIM=AP%NDIM
    NDM=AP%NDM
    NPAR=AP%NPAR

    ALLOCATE(UPSR(NDM,0:NCOLRS*NTSR),UDOTPSR(NDM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOT,UPSR, &
         UDOTPSR,TMR,ITPRS,NDM)

    ! Complement starting data
    PAR(NPAR-1)=0.d0
    PAR(NPAR)=0.d0

    NODIR=0
    CALL ADAPT2(NTSR,NCOLRS,NDM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNPL

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for BP cont (Periodic Solutions) (by F. Dercole)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- -----
  SUBROUTINE FNPBP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:),DFP(:),FF1(:),FF2(:)
    INTEGER NDM,NFPR,NPAR,I,J,ISW,IP,NPARU
    DOUBLE PRECISION UMX,EP,P,UU

    ISW=AP%ISW
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Generate the function.

    CALL FFPBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,IJAC,F,NDM,DFDU,DFDP)

    IF(IJAC.EQ.0)RETURN

    CALL EXPANDJAC(DFDU,NDM,NDM,NDIM)
    CALL EXPANDJAC(DFDP,NPAR,NDM,NDIM)

    DFDU(1:NDM,NDM+1:NDIM)=0
    NPARU=NPAR-AP%NPARI
    IF(ISW==2.OR.ISW<0)THEN
       !        ** Non-generic and/or start, derivatives of -b phi_1^* in (18)
       DO I=1,NDM
          DFDU(I,NDM+I)=-PAR(NPARU+3) ! b
       ENDDO
    ENDIF
    DO I=1,NDM ! d(13a)/d phi_1^* = -f_x(x,p)^T
       DFDU(NDM+1:2*NDM,NDM+I)=-DFDU(I,1:NDM)
    ENDDO
    IF(ISW<0)THEN ! start
       DFDU(NDM+1:2*NDM,2*NDM+1:4*NDM)=0
       DO I=1,NDM
          ! d(15a)/dv = c1, d(15a)/dw = c2
          DFDU(NDM+I,2*NDM+I)=PAR(NPARU+8)
          DFDU(NDM+I,3*NDM+I)=PAR(NPARU+9)
       ENDDO
       DFDU(2*NDM+1:4*NDM,1:2*NDM)=0
       ! d(9a)/dv = d(9b)/dw = f_x(x,p)
       DFDU(2*NDM+1:3*NDM,2*NDM+1:3*NDM)=DFDU(1:NDM,1:NDM)
       DFDU(2*NDM+1:3*NDM,3*NDM+1:4*NDM)=0
       DFDU(3*NDM+1:4*NDM,2*NDM+1:3*NDM)=0
       DFDU(3*NDM+1:4*NDM,3*NDM+1:4*NDM)=DFDU(1:NDM,1:NDM)
    ENDIF

    ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPAR))
    ALLOCATE(FF1(NDIM),FF2(NDIM))

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDM
       UU=U(I)
       U(I)=UU-EP
       CALL FFPBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,0,FF1,NDM,DFU,DFP)
       U(I)=UU+EP
       CALL FFPBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,0,FF2,NDM,DFU,DFP)
       U(I)=UU
       DO J=NDM+1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF (IJAC.EQ.1)THEN
       DEALLOCATE(DFU,DFP,FF1)
       RETURN
    ENDIF

    DO I=1,NFPR
       ! note: restart uses only PAR(NPARU+1) and perhaps PAR(NPARU+3)
       IP=ICP(I)
       IF(IP==11)THEN
          DFDP(NDM+1:2*NDM,11)=F(NDM+1:2*NDM)/PAR(11)
          IF(ISW<0)THEN
             DFDP(2*NDM+1:3*NDM,11)=&
                  (F(2*NDM+1:3*NDM)-DFDP(1:NDM,11)*PAR(NPARU+5))/PAR(11)
             DFDP(3*NDM+1:4*NDM,11)=&
                  (F(3*NDM+1:4*NDM)-DFDP(1:NDM,11)*PAR(NPARU+7))/PAR(11)
          ENDIF
       ELSEIF(IP<=NPARU)THEN
          P=PAR(IP)
          PAR(IP)=P+EP
          CALL FFPBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,0,FF1,NDM,DFU,DFP)
          PAR(IP)=P
          DO J=NDM+1,NDIM
             DFDP(J,IP)=(FF1(J)-F(J))/EP
          ENDDO
       ENDIF
    ENDDO
    DEALLOCATE(DFU,DFP,FF1)

    IP=NPARU+1 ! phi^*_3
    DFDP(1:NDM,IP)=0
    DFDP(NDM+1:2*NDM,IP)=UOLD(NDIM+NFPR+1:NDIM+NFPR+NDM) ! =UPOLD(1:NDM)=h_x
    DFDP(2*NDM+1:NDIM,IP)=0
    DFDP(:,NPARU+2)=0 ! a

    IF(ISW<0) THEN
       ! start
       DO J=2,3
          DO I=1,2
             ! derivatives to q1, q2, r1, r2
             IP=NPARU+2*J+I-1
             DFDP(1:2*NDM,IP)=0
             ! d(9a)/dq = d(9b)/dr = f_p(x,p)
             DFDP(J*NDM+1:(J+1)*NDM,IP)=DFDP(1:NDM,ICP(I))
             DFDP((5-J)*NDM+1:(6-J)*NDM,IP)=0
          ENDDO
          ! derivatives to c1, c2
          IP=NPARU+6+J
          DFDP(1:NDM,IP)=0
          DFDP(NDM+1:2*NDM,IP)=U(J*NDM+1:(J+1)*NDM)
          DFDP(2*NDM+1:4*NDM,IP)=0
       ENDDO
    ENDIF

    IF(ISW<0.OR.ISW==2)THEN ! non-generic and/or start
       IP=NPARU+3 ! b
       DFDP(1:NDM,IP)=-U(NDM+1:2*NDM)
       DFDP(NDM+1:NDIM,IP)=0
    ENDIF

  END SUBROUTINE FNPBP

! ---------- -----
  SUBROUTINE FFPBP(AP,NDIM,U,UOLD,UPOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC,NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UPOLD(NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    ! Local
    INTEGER ISW,I,J,NPARU,IJC

    ISW=AP%ISW

    IF(ISW<0) THEN
       IJC=MAX(IJAC,2)
    ELSE
       IJC=MAX(IJAC,1)
    ENDIF
    CALL FNPS(AP,NDM,U,UOLD,ICP,PAR,IJC,F,DFDU,DFDP)

    NPARU=AP%NPAR-AP%NPARI

    ! (13a) \dot phi_1^* = -f_x(x,p)^T phi_1^* + h_x(x,p)^T phi_3^*
    !       where h_x=DINT
    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
       ENDDO
       F(NDM+I)=F(NDM+I)+UPOLD(I)*PAR(NPARU+1)
    ENDDO
    IF(ISW<0) THEN
       !        ** start
       ! (9a) \dot v = f_x(x,p)v + f_p(x,p)q
       ! (9b) \dot w = f_x(x,p)w + f_p(x,p)r
       ! (15a) \dot phi_1^* = -f_x(x,p)^T phi_1^*+h_x(x,p)^T phi_3^*+c1 v+c2 w
       DO I=1,NDM
          F(2*NDM+I)=0.d0
          F(3*NDM+I)=0.d0
          DO J=1,NDM
             F(2*NDM+I)=F(2*NDM+I)+DFDU(I,J)*U(2*NDM+J)
             F(3*NDM+I)=F(3*NDM+I)+DFDU(I,J)*U(3*NDM+J)
          ENDDO
          F(NDM+I)=F(NDM+I)+PAR(NPARU+8)*U(2*NDM+I)+PAR(NPARU+9)*U(3*NDM+I)
          DO J=1,2
             F(2*NDM+I)=F(2*NDM+I)+DFDP(I,ICP(J))*PAR(NPARU+3+J)
             F(3*NDM+I)=F(3*NDM+I)+DFDP(I,ICP(J))*PAR(NPARU+5+J)
          ENDDO
       ENDDO
    ENDIF

    IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
       !        ** Non-generic and/or start
       ! (18) \dot x = f(x,p) - b phi_1^*
       DO I=1,NDM
          F(I)=F(I)-PAR(NPARU+3)*U(NDM+I)
       ENDDO
    ENDIF

  END SUBROUTINE FFPBP

! ---------- -----
  SUBROUTINE BCPBP(AP,NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)

    ! Boundary conditions for continuing BP (Periodic solutions)

    USE BVP, ONLY: IRTN, NRTN
    USE SUPPORT, ONLY : PI

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    INTEGER ISW,NDM,NPAR,NN,I,J,NPARU

    ISW=AP%ISW
    NDM=AP%NDM
    NPAR=AP%NPAR

    DO I=1,NDIM
       FB(I)=U0(I)-U1(I)
    ENDDO

    NPARU=NPAR-AP%NPARI
    IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
       !        ** Non-generic and/or start
       DO I=1,NDM
          FB(I)=FB(I)+PAR(NPARU+3)*U0(NDM+I)
       ENDDO
    ENDIF

    ! Rotations
    IF(IRTN.NE.0)THEN
       DO I=1,NDM
          IF(NRTN(I).NE.0)FB(I)=FB(I)+PI(2.d0)*NRTN(I)
       ENDDO
    ENDIF

    IF(IJAC.EQ.0)RETURN

    NN=2*NDIM+NPAR
    DO I=1,NBC
       DO J=1,NN
          DBC(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDIM
       DBC(I,I)=1
       DBC(I,NDIM+I)=-1
    ENDDO

    IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
       !        ** Non-generic and/or start
       DO I=1,NDM
          DBC(I,NDM+I)=PAR(NPARU+3)
       ENDDO
    ENDIF

    IF(IJAC.EQ.1)RETURN

    IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
       !        ** Non-generic and/or start
       DO I=1,NDM
          DBC(I,2*NDIM+NPARU+3)=U0(NDM+I)
       ENDDO
    ENDIF

  END SUBROUTINE BCPBP

! ---------- -----
  SUBROUTINE ICPBP(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)

    ! Integral conditions for continuing BP (Periodic solutions)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: F(:),DFU(:,:),DFP(:,:),DFP0(:,:),DFP1(:,:)
    INTEGER NFPR,NDM,NPAR,NPARU,ISW,I,J,K
    DOUBLE PRECISION UMX,EP,P,UU

    ISW=AP%ISW
    NDM=AP%NDM
    NPAR=AP%NPAR
    NFPR=AP%NFPR
    NPARU=NPAR-AP%NPARI

    ! Generate the function.

    ALLOCATE(F(NDM),DFU(NDM,NDM),DFP(NDM,NPAR))
    CALL FNPS(AP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)

    ! (5) int_0^1 h(x,p) dt = 0
    ! (13c) int_0^1 ||phi_1^*||^2 dt + ||phi_3^*||^2 - a = 0
    !       (phi_2^*=0 because of periodic BCs)
    FI(1)=0.d0
    FI(NINT)=PAR(NPARU+1)**2-PAR(NPARU+2)
    DO I=1,NDM
       FI(1)=FI(1)+(U(I)-UOLD(I))*UPOLD(I)
       FI(NINT)=FI(NINT)+U(NDM+I)**2
    ENDDO

    IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
       !        ** Non-generic and/or start
       ! (18) int_0^1 h(x,p) dt + b phi_3^* = 0
       FI(1)=FI(1)+PAR(NPARU+3)*PAR(NPARU+1)
    ENDIF

    ! (13b) int_0^1 -f_p(x,p)^T phi_1^* dt = 0 (h_p=0)
    DO I=1,2
       FI(1+I)=0.d0
       DO J=1,NDM
          FI(1+I)=FI(1+I)-DFP(J,ICP(I))*U(NDM+J)
       ENDDO
    ENDDO

    IF(ISW<0) THEN
       !        ** start
       ! (15b) int_0^1 -f(x,p)^T phi_1^* dt + c_1 q + c_2 r = 0
       DO I=1,2
          FI(1+I)=FI(1+I)+PAR(NPARU+8)*PAR(NPARU+3+I)+&
               PAR(NPARU+9)*PAR(NPARU+5+I)
       ENDDO
       ! (9a) int_0^1 h_x(x,p)v dt = 0
       ! (9b) int_0^1 h_x(x,p)w dt = 0
       ! (10a) int_0^1 <v, v_old> dt + <q, q_old> - 1 = 0
       ! (10b) int_0^1 <w, w_old> dt + <r, r_old> - 1 = 0
       ! (11a) int_0^1 <v, w_old> dt + <q, r_old> = 0
       ! (11b) int_0^1 <w, v_old> dt + <r, q_old> = 0
       FI(4)=0.d0
       FI(5)=0.d0
       FI(6)=PAR(NPARU+4)*UOLD(NDIM+6)+PAR(NPARU+5)*UOLD(NDIM+7)-1.d0
       FI(7)=PAR(NPARU+6)*UOLD(NDIM+8)+PAR(NPARU+7)*UOLD(NDIM+9)-1.d0
       FI(8)=PAR(NPARU+4)*UOLD(NDIM+8)+PAR(NPARU+5)*UOLD(NDIM+9)
       FI(9)=PAR(NPARU+6)*UOLD(NDIM+6)+PAR(NPARU+7)*UOLD(NDIM+7)
       DO I=1,NDM
          FI(4)=FI(4)+U(2*NDM+I)*UPOLD(I)
          FI(5)=FI(5)+U(3*NDM+I)*UPOLD(I)
          FI(6)=FI(6)+U(2*NDM+I)*UOLD(2*NDM+I)
          FI(7)=FI(7)+U(3*NDM+I)*UOLD(3*NDM+I)
          FI(8)=FI(8)+U(2*NDM+I)*UOLD(3*NDM+I)
          FI(9)=FI(9)+U(3*NDM+I)*UOLD(2*NDM+I)
       ENDDO
    ENDIF

    IF(IJAC.EQ.0)THEN
       DEALLOCATE(F,DFU,DFP)
       RETURN
    ENDIF

    ! Generate the Jacobian.

    ! derivatives for (5) int_0^1 h(x,p) dt = 0
    DINT(1,1:NDM)=UPOLD(1:NDM)
    DINT(1,NDM+1:NDIM)=0

    ! derivatives for (13c) int_0^1 ||phi_1^*||^2 dt + ||phi_3^*||^2 - a = 0
    DINT(NINT,1:NDM)=0d0
    DINT(NINT,NDM+1:2*NDM)=2*U(NDM+1:2*NDM)
    DINT(NINT,2*NDM+1:NDIM)=0

    ! derivatives for (13b) int_0^1 -f_p(x,p)^T phi_1^* dt = 0
    DO I=1,2
       ! DINT(1+I,1:NDM) determined by finite differences
       DINT(1+I,NDM+1:2*NDM)=-DFP(1:NDM,ICP(I))
       DINT(1+I,2*NDM+1:NDIM)=0
    ENDDO

    IF(ISW<0)THEN
       ! Derivatives for (9a)-(11b) (see below)
       DINT(4:9,1:NDIM)=0
       DINT(4,2*NDM+1:3*NDM)=UPOLD(1:NDM)
       DINT(5,3*NDM+1:4*NDM)=UPOLD(1:NDM)
       DINT(6,2*NDM+1:3*NDM)=UOLD(2*NDM+1:3*NDM)
       DINT(7,3*NDM+1:4*NDM)=UOLD(3*NDM+1:4*NDM)
       DINT(8,2*NDM+1:3*NDM)=UOLD(3*NDM+1:4*NDM)
       DINT(9,3*NDM+1:4*NDM)=UOLD(2*NDM+1:3*NDM)
    ENDIF

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    ALLOCATE(DFP0(NDM,2),DFP1(NDM,2))
    IF(IJAC/=1)THEN
       DFP0(:,1)=DFP(:,ICP(1))
       DFP0(:,2)=DFP(:,ICP(2))
    ENDIF

    DO I=1,NDM
       ! derivatives for (13b) int_0^1 -f_p(x,p)^T phi_1^* dt = 0 (h_p=0)
       UU=U(I)
       U(I)=UU-EP
       CALL FNPS(AP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       DFP1(:,1)=DFP(:,ICP(1))
       DFP1(:,2)=DFP(:,ICP(2))
       U(I)=UU+EP
       CALL FNPS(AP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       U(I)=UU
       DO J=1,2
          DINT(J+1,I)=0
          DO K=1,NDM
             DINT(J+1,I)=DINT(J+1,I)-U(NDM+K)*(DFP(K,ICP(J))-DFP1(K,J))/(2*EP)
          ENDDO
       ENDDO
    ENDDO

    DEALLOCATE(DFP1)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(F,DFU,DFP,DFP0)
       RETURN
    ENDIF

    DINT(1:NINT,NDIM+NPARU+1:NDIM+NPAR)=0d0
    IF(ISW==2.OR.ISW<0)THEN !        ** Non-generic and/or start
       DINT(1,NDIM+NPARU+3)=PAR(NPARU+1) ! d(18)/db
       DINT(1,NDIM+NPARU+1)=PAR(NPARU+3) ! d(18)/dphi_3^*
    ENDIF

    ! derivatives for (13c) int_0^1 ||phi_1^*||^2 dt + ||phi_3^*||^2 - a = 0
    DINT(NINT,NDIM+NPARU+1)=2*PAR(NPARU+1)
    DINT(NINT,NDIM+NPARU+2)=-1

    IF(ISW<0) THEN
       !        ** start
       ! derivatives for (15b) int_0^1 -f(x,p)^T phi_1^* dt + c_1 q + c_2 r = 0
       DO I=1,2
          DINT(1+I,NDIM+NPARU+3+I)=PAR(NPARU+8)
          DINT(1+I,NDIM+NPARU+8)=PAR(NPARU+3+I)
          DINT(1+I,NDIM+NPARU+5+I)=PAR(NPARU+9)
          DINT(1+I,NDIM+NPARU+9)=PAR(NPARU+5+I)
       ENDDO
       ! derivatives for (10a)-(11b)
       DINT(6,NDIM+NPARU+4)=UOLD(NDIM+6)
       DINT(6,NDIM+NPARU+5)=UOLD(NDIM+7)
       DINT(7,NDIM+NPARU+6)=UOLD(NDIM+8)
       DINT(7,NDIM+NPARU+7)=UOLD(NDIM+9)
       DINT(8,NDIM+NPARU+4)=UOLD(NDIM+8)
       DINT(8,NDIM+NPARU+5)=UOLD(NDIM+9)
       DINT(9,NDIM+NPARU+6)=UOLD(NDIM+6)
       DINT(9,NDIM+NPARU+7)=UOLD(NDIM+7)
    ENDIF

    DO I=1,NFPR
       IF(ICP(I)>=NPARU)CYCLE
       ! derivatives for (13b) int_0^1 -f_p(x,p)^T phi_1^* dt = 0 (h_p=0)
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FNPS(AP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       DO J=1,2
          DINT(J+1,NDIM+ICP(I))=0d0
          DO K=1,NDM
             DINT(J+1,NDIM+ICP(I))=DINT(J+1,NDIM+ICP(I))-U(NDM+K)* &
                  (DFP(K,ICP(J))-DFP0(K,J))/EP
          ENDDO
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(F,DFU,DFP,DFP0)

  END SUBROUTINE ICPBP

! ---------- -------
  SUBROUTINE STPNPBP(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE BVP
    USE IO
    USE MESH
    USE SOLVEBV

    ! Generates starting data for the 2-parameter continuation of BP
    ! on a branch of periodic solutions.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: THU1(:)
    DOUBLE PRECISION, ALLOCATABLE :: P0(:,:),P1(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: U(:),DTM(:)
    INTEGER ICPRS(11)
    DOUBLE PRECISION DUM(1),RLDOTRS(11),RLCUR(2),RVDOT(2),THL1(2)
    DOUBLE PRECISION, ALLOCATABLE :: UPST(:,:),UDOTPST(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: VDOTPST(:,:),UPOLDPT(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER NDIM,ISW,NDM,NFPR,IFST,NLLV,ITPRS
    INTEGER NDIMRD,I,J,NPAR
    DOUBLE PRECISION DET,RDSZ
    TYPE(AUTOPARAMETERS) AP2

    NDIM=AP%NDIM
    ISW=AP%ISW
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    IF(ISW>0) THEN
       !        ** restart
       CALL STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)
       RETURN
    ENDIF

    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),UDOTPSR(NDIM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))

    ! Start
    !        ** allocation
    ALLOCATE(UPST(NDM,0:NTSR*NCOLRS),UDOTPST(NDM,0:NTSR*NCOLRS))
    ALLOCATE(UPOLDPT(NDM,0:NTSR*NCOLRS))
    ALLOCATE(VDOTPST(NDM,0:NTSR*NCOLRS))
    ALLOCATE(THU1(NDM))
    ALLOCATE(P0(NDM,NDM),P1(NDM,NDM))
    ALLOCATE(U(NDM),DTM(NTSR))

    !        ** read the std branch
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPST, &
         UDOTPST,TMR,ITPRS,NDM)

    DO I=1,NTSR
       DTM(I)=TMR(I)-TMR(I-1)
    ENDDO

    RLCUR(1)=PAR(ICPRS(1))
    RLCUR(2)=PAR(ICPRS(2))

    ! Compute the second null vector

    !        ** redefine IAP
    AP2=AP
    AP2%NDIM=NDM
    AP2%NTST=NTSR
    AP2%NCOL=NCOLRS
    AP2%NBC=NDM
    AP2%NINT=1
    AP2%NFPR=2

    !        ** compute UPOLDP
    DO J=0,NTSR*NCOLRS
       U(:)=UPST(:,J)
       CALL FNPS(AP2,NDM,U,U,ICPRS,PAR,0,UPOLDPT(1,J),DUM,DUM)
    ENDDO

    !        ** unit weights
    THL1(:)=1.d0
    THU1(1:NDM)=1.d0

    !        ** call SOLVBV
    RDSZ=0.d0
    NLLV=1
    IFST=1
    CALL SOLVBV(IFST,AP2,DET,PAR,ICPRS,FNPS,BCPS,ICPS,RDSZ,NLLV, &
         RLCUR,RLCUR,RLDOTRS,NDM,UPST,UPST,UDOTPST,UPOLDPT, &
         DTM,VDOTPST,RVDOT,P0,P1,THL1,THU1)

    !        ** normalization
    CALL SCALEB(NTSR,NCOLRS,NDM,2,UDOTPST,RLDOTRS,DTM,THL1,THU1)
    CALL SCALEB(NTSR,NCOLRS,NDM,2,VDOTPST,RVDOT,DTM,THL1,THU1)

    !        ** init UPS,PAR
    UPSR(1:NDM,:)=UPST(:,:)
    UPSR(NDM+1:2*NDM,:)=0.d0
    UPSR(2*NDM+1:3*NDM,:)=UDOTPST(:,:)
    UPSR(3*NDM+1:4*NDM,:)=VDOTPST(:,:)
    UDOTPSR(:,:)=0.d0

    !        ** init phi^*_3,a,b,q,r,c1,c2
    PAR(NPAR-9+1:NPAR-9+3)=0.d0
    PAR(NPAR-9+4:NPAR-9+5)=RLDOTRS(1:2)
    PAR(NPAR-9+6:NPAR-9+7)=RVDOT(1:2)
    PAR(NPAR-9+8:NPAR-9+9)=0.d0
    RLDOT(1:3)=0.d0
    RLDOT(4)=1.d0
    RLDOT(5:11)=0.d0

    DEALLOCATE(UPST,UPOLDPT,UDOTPST,VDOTPST)
    DEALLOCATE(THU1)
    DEALLOCATE(P0,P1)
    DEALLOCATE(U,DTM)

    NODIR=0

    CALL ADAPT2(NTSR,NCOLRS,NDIM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)
  END SUBROUTINE STPNPBP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for the Continuation of Period Doubling Bifurcations
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNPD(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE ::DFU(:,:),FF1(:),FF2(:)
    INTEGER NDM,NFPR,I,J,NPAR
    DOUBLE PRECISION UMX,EP,P,UU,DUMDP(1)

    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Generate the function.

    CALL FFPD(AP,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP)

    IF(IJAC.EQ.0)RETURN

    CALL EXPANDJAC(DFDU,NDM,NDM,NDIM)
    CALL EXPANDJAC(DFDP,NPAR,NDM,NDIM)

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DFDU(1:NDM,1:NDM)=PAR(11)*DFDU(1:NDM,1:NDM)
    DFDU(1:NDM,NDM+1:2*NDM)=0
    DFDU(NDM+1:2*NDM,NDM+1:2*NDM)=DFDU(1:NDM,1:NDM)

    ALLOCATE(DFU(NDM,NDM),FF1(NDIM),FF2(NDIM))
    DO I=1,NDM
       UU=U(I)
       U(I)=UU-EP
       CALL FFPD(AP,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DUMDP)
       U(I)=UU+EP
       CALL FFPD(AP,U,UOLD,ICP,PAR,0,FF2,NDM,DFU,DUMDP)
       U(I)=UU
       DO J=NDM+1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(FF1,DFU)
       RETURN
    ENDIF

    DO I=1,NFPR
       IF(ICP(I)==NPAR)THEN
          DFDP(:,ICP(I))=0
       ELSEIF(ICP(I)==11)THEN
          DFDP(1:NDIM,11)=F(1:NDIM)/PAR(11)
       ELSE
          DFDP(1:NDM,ICP(I))=PAR(11)*DFDP(1:NDM,ICP(I))
          P=PAR(ICP(I))
          PAR(ICP(I))=P+EP
          CALL FFPD(AP,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DUMDP)
          DO J=NDM+1,NDIM
             DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
          ENDDO
          PAR(ICP(I))=P
       ENDIF
    ENDDO

    DEALLOCATE(FF1,DFU)
  END SUBROUTINE FNPD

! ---------- ----
  SUBROUTINE FFPD(AP,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(2*NDM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    ! Local
    INTEGER I,J
    DOUBLE PRECISION PERIOD

    PERIOD=PAR(11)
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,MAX(IJAC,1),F,DFDU,DFDP)

    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
       ENDDO
       F(I)=PERIOD*F(I)
       F(NDM+I)=PERIOD*F(NDM+I)
    ENDDO

  END SUBROUTINE FFPD

! ---------- ----
  SUBROUTINE BCPD(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    USE BVP, ONLY: IRTN, NRTN
    USE SUPPORT, ONLY : PI

    ! Generate boundary conditions for the 2-parameter continuation
    ! of period doubling bifurcations.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    INTEGER NDM,NPAR,NN,I,J
    NDM=AP%NDM

    DO I=1,NDM
       F(I)=U0(I)-U1(I)
       F(NDM+I)=U0(NDM+I)+U1(NDM+I)
    ENDDO

    ! Rotations
    IF(IRTN.NE.0)THEN
       DO I=1,NDM
          IF(NRTN(I).NE.0)F(I)=F(I) + PI(2.d0)*NRTN(I)
       ENDDO
    ENDIF

    IF(IJAC.EQ.0)RETURN

    NPAR=AP%NPAR
    NN=2*NDIM+NPAR
    DO I=1,NBC
       DO J=1,NN
          DBC(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDIM
       DBC(I,I)=1
       IF(I.LE.NDM) THEN
          DBC(I,NDIM+I)=-1
       ELSE
          DBC(I,NDIM+I)=1
       ENDIF
    ENDDO

  END SUBROUTINE BCPD

! ---------- ----
  SUBROUTINE ICPD(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    INTEGER NDM,NPAR,NN,I,J

    NDM=AP%NDM
    NPAR=AP%NPAR

    F(1)=0.d0
    F(2)=-PAR(NPAR)

    DO I=1,NDM
       F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
       F(2)=F(2)+U(NDM+I)*U(NDM+I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    NN=NDIM+NPAR
    DO I=1,NINT
       DO J=1,NN
          DINT(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDM
       DINT(1,I)=UPOLD(I)
       DINT(2,NDM+I)=2.d0*U(NDM+I)
    ENDDO

    DINT(2,NDIM+NPAR)=-1.d0

  END SUBROUTINE ICPD

! ---------- ------
  SUBROUTINE STPNPD(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE BVP
    USE IO
    USE MESH

    ! Generates starting data for the 2-parameter continuation of
    ! period-doubling bifurcations on a branch of periodic solutions.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER ICPRS(4),NDIM,NDM,NDIMRD,ITPRS,NPAR

    NDIM=AP%NDIM
    NDM=AP%NDM
    NPAR=AP%NPAR

    ALLOCATE(UPSR(NDM,0:NCOLRS*NTSR),UDOTPSR(NDM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOT,UPSR, &
         UDOTPSR,TMR,ITPRS,NDM)

    ! Complement starting data 
    PAR(NPAR)=0.d0

    NODIR=0
    CALL ADAPT2(NTSR,NCOLRS,NDM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNPD

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!       Subroutines for the Continuation of Torus Bifurcations
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNTR(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-parameter continuation of
    ! torus bifurcations.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE ::DFU(:,:),FF1(:),FF2(:)
    INTEGER NDM,NFPR,NPAR,I,J
    DOUBLE PRECISION EP,UMX,P,UU,DUMDP(1)

    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Generate the function.

    CALL FFTR(AP,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP)

    IF(IJAC.EQ.0)RETURN

    CALL EXPANDJAC(DFDU,NDM,NDM,NDIM)
    CALL EXPANDJAC(DFDP,NPAR,NDM,NDIM)

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DFDU(1:NDM,1:NDM)=PAR(11)*DFDU(1:NDM,1:NDM)
    DFDU(1:NDM,NDM+1:3*NDM)=0
    DFDU(NDM+1:2*NDM,NDM+1:2*NDM)=DFDU(1:NDM,1:NDM)
    DFDU(NDM+1:2*NDM,2*NDM+1:3*NDM)=0
    DFDU(2*NDM+1:3*NDM,NDM+1:2*NDM)=0
    DFDU(2*NDM+1:3*NDM,2*NDM+1:3*NDM)=DFDU(1:NDM,1:NDM)

    ALLOCATE(FF1(NDIM),FF2(NDIM),DFU(NDM,NDM))
    DO I=1,NDM
       UU=U(I)
       U(I)=UU-EP
       CALL FFTR(AP,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DUMDP)
       U(I)=UU+EP
       CALL FFTR(AP,U,UOLD,ICP,PAR,0,FF2,NDM,DFU,DUMDP)
       U(I)=UU
       DO J=NDM+1,3*NDM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC.EQ.1)THEN
       DEALLOCATE(FF1,DFU)
       RETURN
    ENDIF

    DO I=1,NFPR
       IF(ICP(I)==12.OR.ICP(I)==NPAR)THEN
          DFDP(:,ICP(I))=0
       ELSEIF(ICP(I)==11)THEN
          DFDP(1:NDIM,11)=F(1:NDIM)/PAR(11)
       ELSE
          DFDP(1:NDM,ICP(I))=PAR(11)*DFDP(1:NDM,ICP(I))
          P=PAR(ICP(I))
          PAR(ICP(I))=P+EP
          CALL FFTR(AP,U,UOLD,ICP,PAR,0,FF1,NDM,DFU,DUMDP)
          DO J=NDM+1,3*NDM
             DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
          ENDDO
          PAR(ICP(I))=P
       ENDIF
    ENDDO

    DEALLOCATE(FF1,DFU)
  END SUBROUTINE FNTR

! ---------- ----
  SUBROUTINE FFTR(AP,U,UOLD,ICP,PAR,IJAC,F,NDM,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(*),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDM*3)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,*),DFDP(NDM,*)
    ! Local
    INTEGER NDM2,I,J
    DOUBLE PRECISION PERIOD

    PERIOD=PAR(11)
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,MAX(IJAC,1),F,DFDU,DFDP)

    NDM2=2*NDM
    DO I=1,NDM
       F(NDM+I)=0.d0
       F(NDM2+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
          F(NDM2+I)=F(NDM2+I)+DFDU(I,J)*U(NDM2+J)
       ENDDO
       F(NDM+I)=PERIOD*F(NDM+I)
       F(NDM2+I)=PERIOD*F(NDM2+I)
       F(I)=PERIOD*F(I)
    ENDDO

  END SUBROUTINE FFTR

! ---------- ----
  SUBROUTINE BCTR(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    USE BVP, ONLY: IRTN, NRTN
    USE SUPPORT, ONLY : PI

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    INTEGER NDM,NDM2,NN,NPAR,I,J
    DOUBLE PRECISION THETA,SS,CS

    NDM=AP%NDM
    NPAR=AP%NPAR

    NDM2=2*NDM
    THETA=PAR(12)

    SS=SIN(THETA)
    CS=COS(THETA)

    DO I=1,NDM
       F(I)=U0(I)-U1(I)
       F(NDM+I)= U1(NDM+I) -CS*U0(NDM+I) +SS*U0(NDM2+I)
       F(NDM2+I)=U1(NDM2+I)-CS*U0(NDM2+I)-SS*U0(NDM+I)
    ENDDO

    ! Rotations
    IF(IRTN.NE.0)THEN
       DO I=1,NDM
          IF(NRTN(I).NE.0)F(I)=F(I) + PI(2.d0)*NRTN(I)
       ENDDO
    ENDIF

    IF(IJAC.EQ.0)RETURN

    NN=2*NDIM+NPAR
    DO I=1,NBC
       DO J=1,NN
          DBC(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDM
       DBC(I,I)=1
       DBC(I,NDIM+I)=-1
       DBC(NDM+I,NDM+I)=-CS
       DBC(NDM+I,NDM2+I)=SS
       DBC(NDM+I,NDIM+NDM+I)=1
       DBC(NDM+I,2*NDIM+12)=CS*U0(NDM2+I)+SS*U0(NDM+I)
       DBC(NDM2+I,NDM+I)=-SS
       DBC(NDM2+I,NDM2+I)=-CS
       DBC(NDM2+I,NDIM+NDM2+I)=1
       DBC(NDM2+I,2*NDIM+12)=SS*U0(NDM2+I)-CS*U0(NDM+I)
    ENDDO

  END SUBROUTINE BCTR

! ---------- ----
  SUBROUTINE ICTR(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    INTEGER NDM,NDM2,NN,NPAR,I,J

    NDM=AP%NDM
    NPAR=AP%NPAR
    NDM2=2*NDM

    F(1)=0.d0
    F(2)=0.d0
    F(3)=-PAR(NPAR)

    DO I=1,NDM
       F(1)=F(1)+(U(I)-UOLD(I))*UPOLD(I)
       F(2)=F(2)+U(NDM+I)*UOLD(NDM2+I)-U(NDM2+I)*UOLD(NDM+I)
       F(3)=F(3)+U(NDM+I)*U(NDM+I) +U(NDM2+I)*U(NDM2+I)
    ENDDO

    IF(IJAC.EQ.0)RETURN

    NN=NDIM+NPAR
    DO I=1,NINT
       DO J=1,NN
          DINT(I,J)=0.d0
       ENDDO
    ENDDO

    DO I=1,NDM
       DINT(1,I)=UPOLD(I)
       DINT(2,NDM+I)=UOLD(NDM2+I)
       DINT(2,NDM2+I)=-UOLD(NDM+I)
       DINT(3,NDM+I)=2*U(NDM+I)
       DINT(3,NDM2+I)=2*U(NDM2+I)
    ENDDO

    DINT(3,NDIM+NPAR)=-1

  END SUBROUTINE ICTR

! ---------- ------
  SUBROUTINE STPNTR(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE BVP
    USE IO
    USE MESH
    USE AUTO_CONSTANTS, ONLY: TY

    ! Generates starting data for the 2-parameter continuation of torus
    ! bifurcations.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    DOUBLE PRECISION T,DT,THETA
    INTEGER ICPRS(4),NDIM,NDM,NPAR,NDIMRD,ITPRS,I,J,K
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)

    NDIM=AP%NDIM
    NDM=AP%NDM
    NPAR=AP%NPAR

    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),UDOTPSR(NDIM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOT,UPSR, &
         UDOTPSR,TMR,ITPRS,NDIM)

    T=0.d0
    DT=0.d0
    IF(LEN_TRIM(TY)>2)THEN
       READ(TY(3:),'(I5)')I
       PAR(12)=PAR(I)
    ENDIF
    THETA=PAR(12)
    DO I=0,NTSR*NCOLRS
       IF(MOD(I,NCOLRS)==0)THEN
          J=I/NCOLRS
          T=TMR(J)
          IF(J<NTSR)DT=(TMR(J+1)-T)/NCOLRS
       ENDIF
       DO K=NDM+1,2*NDM
          UPSR(K,I)        = 0.0001d0*COS(T*THETA)
          UPSR(K+NDM,I)    = 0.0001d0*SIN(T*THETA)
          UDOTPSR(K,I)     = 0.d0
          UDOTPSR(K+NDM,I) = 0.d0
       ENDDO
       T=T+DT
    ENDDO

    PAR(NPAR)=0.d0

    NODIR=0
    CALL ADAPT2(NTSR,NCOLRS,NDIM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNTR

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNCSPS(AP,ICP,UPS,NDIM,PAR,ITEST,ATYPE) RESULT(Q)
    USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
    USE SUPPORT, ONLY: P0=>P0V, P1=>P1V, EV=>EVV

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(IN) :: ITEST
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE

    Q=FNCSBV(AP,ICP,UPS,NDIM,PAR,ITEST,ATYPE)
    IF(ITEST==3)THEN
        Q=FNSPBV(AP,PAR,ATYPE,P0,P1,EV)
    ENDIF
  END FUNCTION FNCSPS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNSPBV(AP,PAR,ATYPE,P0,P1,EV)

    USE FLOQUET
    USE SUPPORT, ONLY: PI, LBTYPE, CHECKSP, NULLVC

! This function returns a quantity that changes sign when a complex
! pair of eigenvalues of the linearized Poincare map moves in or out
! of the unit circle or when a real eigenvalues passes through -1.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    DOUBLE PRECISION, INTENT(IN) :: P0(AP%NDIM,*),P1(AP%NDIM,*)
    COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: EV(*)

! Local
    COMPLEX(KIND(1.0D0)) ZTMP
    INTEGER ISP,ISW,IID,IBR,NTOT,NTOP,I,J,L,LOC,NINS,NINS1,NDIM,NDM,ITMP
    DOUBLE PRECISION D,AMIN,AZM1,tol,V,THETA
    DOUBLE PRECISION, ALLOCATABLE :: Q0(:,:),Q1(:,:),U(:)
    INTEGER, ALLOCATABLE :: IC(:),IR(:),IRPIV(:)

    NDIM=AP%NDIM
    NDM=AP%NDM
    ISP=AP%ISP
    ISW=AP%ISW
    IID=AP%IID
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

! Initialize.

    FNSPBV=0.d0
    AP%SPBF=FNSPBV
    D=0.d0
    ATYPE=''

    IF(ISP==0)RETURN

    IF(IID.GE.4)THEN
       CALL EVECS(NDIM,P0,P1,.FALSE.)
    ENDIF
    IF(NDIM>NDM)THEN
       ! on loci we can take the eigenvalues of the lower right hand
       ! block; however, the rows of P0 and P1 are a linear combination wrt
       ! those corresponding to the original coordinates
       ! (Q 0)
       ! (* Q) (for PD and LP continuations)
       ! or
       ! (Q    0)
       ! (* -Q^T) (for BP continuations)
       ! or
       ! (Q 0 0)
       ! (* Q 0) (for TR continuations)
       ! (* 0 Q)
       ! any combination of rows from the right hand block that is invertible
       ! suffices though,
       ! so we swap rows according to the echelon form obtained from NULLVC
       ALLOCATE(Q1(NDIM,NDM),IC(NDM),IRPIV(NDM),U(NDM))
       Q1(:,:)=P1(:,NDIM-NDM+1:NDIM)
       CALL NULLVC(NDIM,NDM,0,Q1,U,IC,IRPIV)
       DEALLOCATE(Q1,IC,U)

       ALLOCATE(Q0(NDM,NDM),Q1(NDM,NDM),IR(NDIM))       
       ! swap rows in IR according to IRPIV
       DO I=1,NDIM
          IR(I)=I
       ENDDO
       DO I=1,NDM
          J=IRPIV(I)
          IF(J/=I)THEN
             ITMP=IR(J)
             IR(J)=IR(I)
             IR(I)=ITMP
          ENDIF
       ENDDO
       IF(AP%ITPST==6)THEN
          ! BP cont: took negative of transpose (adjoint variational
          ! equation) in flow: swap P0/P1
          ! EVECS then needs the left eigenvector
          DO I=1,NDM
             Q0(I,:)=P1(IR(I),NDIM-NDM+1:NDIM)
             Q1(I,:)=P0(IR(I),NDIM-NDM+1:NDIM)
          ENDDO
       ELSE
          DO I=1,NDM
             Q0(I,:)=P0(IR(I),NDIM-NDM+1:NDIM)
             Q1(I,:)=P1(IR(I),NDIM-NDM+1:NDIM)
          ENDDO
       ENDIF
       IF(IID.GE.4)THEN
          CALL EVECS(NDM,Q0,Q1,AP%ITPST==6)
       ENDIF

!  Compute the Floquet multipliers
       CALL FLOWKM(NDM, Q0, Q1, IID, EV)
       DEALLOCATE(Q0,Q1,IR,IRPIV)
    ELSE
       IF(IID.GE.4)THEN
          CALL EVECS(NDIM,P0,P1,.FALSE.)
       ENDIF
!  Compute the Floquet multipliers
       CALL FLOWKM(NDM, P0, P1, IID, EV)
    ENDIF

! The algorithm in FLOWKM causes the first multiplier to be closest to z=1.

    L=1
    IF(AP%ITPST==5.OR.AP%ITPST==7)THEN
       ! Find the remaining multiplier closest to z=1/-1 on PD/LP curves
       ! to be able to detect 1:1 and 1:2 resonances.
       L=2
       IF(AP%ITPST==7)THEN
          ! For PD: Find the multiplier closest to z=-1.
          V=-1.d0
       ELSE
          ! For LP: Find the multiplier closest to z=-1.
          V=1.d0
       ENDIF
       AMIN=HUGE(1.d0)
       LOC=2
       DO J=2,NDM
          IF(EV(J)==CMPLX( HUGE(1.0D0), HUGE(1.0D0), KIND(1.0D0) ))CYCLE
          AZM1= ABS( EV(J) - V )
          IF(AZM1<AMIN)THEN
             AMIN=AZM1
             LOC=J
          ENDIF
       ENDDO
       IF(LOC.NE.2) THEN
          ZTMP=EV(LOC)
          EV(LOC)=EV(2)
          EV(2)=ZTMP
       ENDIF
    ENDIF

! Order the remaining Floquet multipliers by distance from |z|=1.

    IF(NDM.GE.3)THEN
       DO I=L+1,NDM-1
          ! complex conjugate pairs from FLOWKM are ordered a+bi, a-bi
          ! keep them ordered this way
          IF(AIMAG(EV(I))<0)CYCLE
          AMIN=HUGE(1.d0)
          LOC=I
          DO J=I,NDM
             IF(AIMAG(EV(J))<0)CYCLE
             IF(EV(J)==CMPLX( HUGE(1.0D0), HUGE(1.0D0), KIND(1.0D0) ))CYCLE
             AZM1=ABS(ABS(EV(J)) - 1.d0)
             IF(AZM1<AMIN)THEN
                AMIN=AZM1
                LOC=J
             ENDIF
          ENDDO
          IF(LOC.NE.I) THEN
             ZTMP=EV(LOC)
             EV(LOC)=EV(I)
             EV(I)=ZTMP
             IF(AIMAG(EV(LOC))>0.AND.LOC<NDM)THEN
                ! swap other complex conjugate too
                ZTMP=EV(LOC+1)
                EV(LOC+1)=EV(I+1)
                EV(I+1)=ZTMP
             ENDIF
          ENDIF
       ENDDO
    ENDIF

    ATYPE=TPSPBV(NDM,AP%EPSS,AP%ITPST,PAR,AP%NPAR,EV,THETA)
    IF(.NOT.CHECKSP(ATYPE,AP%IPS,AP%ILP,ISP)) ATYPE=''
    IF(ATYPE=='TR') PAR(12)=THETA !try to find TR bif

! Print error message if the Floquet multiplier at z=1 is inaccurate.
! (ISP is set to negative and detection of bifurations is discontinued)

    AMIN= ABS( EV(1) - 1.d0 )
    IF(AMIN>5.0D-2 .AND. LEN_TRIM(ATYPE)>0) THEN
       NINS=0
       AP%NINS=NINS
       ISP=-ISP
       AP%ISP=ISP
       IF(IID>0)THEN
          IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1
          DO I=1,NDM
             WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I)
          ENDDO
          WRITE(9,104)ABS(IBR),NTOP+1,NINS
       ENDIF
       RETURN
    ENDIF

! Restart automatic detection if the Floquet multiplier at z=1 is
! sufficiently accurate again.

    IF(ISP.LT.0)THEN
       IF(AMIN.LT.1.0E-2)THEN
          IF(IID>0)WRITE(9,102)ABS(IBR),NTOP+1
          ISP=-ISP
          AP%ISP=ISP
       ELSE
          IF(IID>0)THEN
             DO I=1,NDM
                WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I),ABS(EV(I))
             ENDDO
          ENDIF
          RETURN
       ENDIF
    ENDIF

! Count the number of Floquet multipliers inside the unit circle.
!
! Set tolerance for deciding if a multiplier is outside |z=1|.
! Use, for example, tol=1d-3 for conservative systems.
    tol=1.d-5

    NINS1=1
    IF(NDM>1) THEN
       DO I=2,NDM
          IF(EV(I)==CMPLX( HUGE(1.0D0), HUGE(1.0D0), KIND(1.0D0) ))CYCLE
          IF( ABS(EV(I)).LE.(1.d0+tol))NINS1=NINS1+1
       ENDDO
       IF(LEN_TRIM(ATYPE)>0)THEN
          IF(ISW.EQ.2)THEN
             IF(AP%ITPST==8)THEN
                ! check the angle for resonances on Torus bifurcations
                THETA=PAR(12)
                D=THETA*(THETA-PI(.5d0))*(THETA-PI(2d0/3))*(THETA-PI(1d0))
             ELSEIF(NDM>2)THEN
                IF(ABS(AIMAG(EV(3)))<SQRT(SQRT(AP%EPSS)).AND. &
                     ((AP%ITPST==5.AND.REAL(EV(3))>0.AND. &
                     ABS( EV(2)-1.d0 )<5.0d-2).OR. &
                     ((AP%ITPST==7.AND.REAL(EV(3))<0.AND. &
                     ABS( EV(2)+1.d0 )<5.0d-2))))THEN
                   ! On LP curve: look for 1:1 resonance
                   ! On PD curve: look for 1:2 resonance
                   D= ABS(EV(3)) - 1.d0
                ENDIF
             ENDIF
          ELSE
             IF(AIMAG(EV(2))/=0.d0 .OR. REAL(EV(2))<0.d0)THEN
!               *Ignore if second multiplier is real positive
                D= ABS(EV(2)) - 1.d0
             ENDIF
          ENDIF
       ENDIF
    ENDIF
    IF( LEN_TRIM(ATYPE)>0 .AND. IID>=2 ) WRITE(9,103)ABS(IBR),NTOP+1,D
    FNSPBV=D
    AP%SPBF=FNSPBV

    NINS=AP%NINS
    IF(LEN_TRIM(ATYPE)>0)THEN
       IF(NINS1==NINS.AND.AP%ITPST/=8)ATYPE=ATYPE//'0'
    ENDIF
    NINS=NINS1
    AP%NINS=NINS

    IF(IID>0)THEN
! Print the Floquet multipliers.

       WRITE(9,104)ABS(IBR),NTOP+1,NINS
       DO I=1,NDM
          IF(EV(I)==CMPLX( HUGE(1.0D0), HUGE(1.0D0), KIND(1.0D0) ))THEN
             WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I),REAL(EV(I))
          ELSE
             WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I),ABS(EV(I))
          ENDIF
       ENDDO
    ENDIF

101 FORMAT(I4,I6,' NOTE:Multiplier inaccurate')
102 FORMAT(I4,I6,' NOTE:Multiplier accurate again')
103 FORMAT(I4,I6,9X,'SPB  Function ',ES14.5)
104 FORMAT(I4,I6,9X,'Multipliers:     Stable:',I4)
105 FORMAT(I4,I6,9X,'Multiplier',I3,1X,2ES14.5, &
         '  Abs. Val.',ES14.5)

  END FUNCTION FNSPBV

! ------------ -------- ------
  CHARACTER(2) FUNCTION TPSPBV(NDM,EPSS,ITPST,PAR,NPAR,EV,THETA)

! Determines type of secondary periodic bifurcation.

    USE SUPPORT, ONLY: PI

    INTEGER, INTENT(IN) :: NDM,ITPST,NPAR
    DOUBLE PRECISION, INTENT(IN) :: EPSS
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(NPAR)
    DOUBLE PRECISION, INTENT(OUT) :: THETA
    COMPLEX(KIND(1.0D0)), INTENT(IN) :: EV(NDM)

    INTEGER LOC,LOC1,I
    DOUBLE PRECISION AMIN,AZM1,D,AD

    THETA=0
    IF(NDM<=2)THEN
       ! None of these bifurcations occur for flat systems.
       TPSPBV=''
       RETURN
    ENDIF
    IF(ITPST==5)THEN
       ! 1:1 resonance
       TPSPBV='R1'
       RETURN
    ELSEIF(ITPST==7)THEN
       ! 1:2 resonance
       TPSPBV='R2'
       RETURN
    ELSEIF(ITPST==8)THEN
       TPSPBV=''
       SELECT CASE(NINT(PAR(12)*6/PI(1d0)))
       CASE(0) ! 1:1 res
          TPSPBV='R1'
       CASE(3) ! 1:4 res
          TPSPBV='R4'
       CASE(4) ! 1:3 res
          TPSPBV='R3'
       CASE(6) ! 1:2 res
          TPSPBV='R2'
       END SELECT
       RETURN
    ENDIF

! Find the eigenvalue closest to z=1.

    LOC=1
    AMIN=HUGE(1.d0)
    DO I=1,NDM
       AZM1= ABS( EV(I) - 1.d0 )
       IF(AZM1.LE.AMIN)THEN
          AMIN=AZM1
          LOC=I
       ENDIF
    ENDDO

! Find the eigenvalue closest to the unit circle
! (excluding the eigenvalue at z=1).

    LOC1=1
    AMIN=HUGE(1.d0)
    DO I=1,NDM
       IF(I.NE.LOC)THEN
          D= ABS(EV(I)) - 1.d0
          AD=ABS(D)
          IF(AD.LE.AMIN)THEN
             AMIN=AD
             LOC1=I
          ENDIF
       ENDIF
    ENDDO

    IF(ABS(AIMAG(EV(LOC1))).GT.SQRT(EPSS))THEN
!       ** torus bifurcation
       TPSPBV='TR'
       THETA=ABS(ATAN2(AIMAG(EV(LOC1)),REAL(EV(LOC1))))
    ELSE IF(REAL(EV(LOC1)).LT.-.5d0)THEN
!       ** period doubling
       TPSPBV='PD'
    ELSE
!       ** something else...
       TPSPBV=''
    ENDIF

  END FUNCTION TPSPBV

! ---------- -----
  SUBROUTINE EVECS(NDIM,P0,P1,TRANS)

    USE SUPPORT

    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(IN) :: P0(NDIM,*),P1(NDIM,*)
    LOGICAL, INTENT(IN) :: TRANS

! Local
    DOUBLE PRECISION, ALLOCATABLE :: Q0(:,:),Q1(:,:),P(:,:),Z(:,:),WR(:),WI(:)
    DOUBLE PRECISION, ALLOCATABLE :: WORK(:)
    INTEGER I,J,IERR,LWORK
    DOUBLE PRECISION DET,WORKINFO(1)
    CHARACTER(1) JOBVL,JOBVR

    ALLOCATE(Q0(NDIM,NDIM), Q1(NDIM,NDIM), P(NDIM,NDIM))
    ALLOCATE(Z(NDIM,NDIM), WR(NDIM), WI(NDIM))

    DO I=1,NDIM
       DO J=1,NDIM
          Q0(I,J)=-P0(I,J)
          Q1(I,J)= P1(I,J)
       ENDDO
    ENDDO

    CALL GEL(NDIM,Q1,NDIM,P,Q0,DET)
    IF(TRANS)THEN
       JOBVL='V'
       JOBVR='N'
    ELSE
       JOBVL='N'
       JOBVR='V'
    ENDIF
    CALL DGEEV(JOBVL,JOBVR,NDIM,P,NDIM,WR,WI,Z,NDIM,Z,NDIM,WORKINFO,-1,IERR)
    LWORK=NINT(WORKINFO(1))
    ALLOCATE(WORK(LWORK))
    CALL DGEEV(JOBVL,JOBVR,NDIM,P,NDIM,WR,WI,Z,NDIM,Z,NDIM,WORK,LWORK,IERR)
    DEALLOCATE(WORK)

    WRITE(9,100)
    WRITE(9,101)
    DO I=1,NDIM
       WRITE(9,102)WR(I),WI(I),(Z(I,J),J=1,NDIM)
    ENDDO
    WRITE(9,101)
100 FORMAT(" Multipliers + eigenvectors obtained from - P1^-1 P0 :")
101 FORMAT(" ")
102 FORMAT(2ES14.5," | ",8ES14.5)

    DEALLOCATE(Q0,Q1,P,Z,WR,WI)
  END SUBROUTINE EVECS

END MODULE PERIODIC
