MODULE MAPS

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
  USE AE
  USE TOOLBOXAE
  USE EQUILIBRIUM
  USE INTERFACES

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: AUTODS

CONTAINS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Discrete Dynamical Systems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE INITDS(AP)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP

    CALL INITEQ(AP)
    ! ** Continuation of singular points
    SELECT CASE(AP%ITPST)
    CASE(7) ! ** PD continuation
       AP%NFPR=2
       AP%NDIM=2*AP%NDIM+1
    CASE(8) ! Neimark-Sacker
       AP%NFPR=2
       AP%NDIM=2*AP%NDIM+2
    END SELECT

  END SUBROUTINE INITDS

! ---------- ------
  SUBROUTINE AUTODS(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    CALL INITDS(AP)
    SELECT CASE(AP%ITPST)
    CASE(0)   ! ** Discrete dynamical systems : fixed points.
       CALL AUTOAE(AP,ICP,ICU,FNDS,STPNDS,FNCSDS)
    CASE(1)   ! BP continuation (maps)
       CALL AUTOAE(AP,ICP,ICU,FNBPDS,STPNBPDS,FNCSDS)
    CASE(2,7) ! ** PD or fold continuation (maps)
       CALL AUTOAE(AP,ICP,ICU,FNLPDS,STPNLPDS,FNCSDS)
    CASE(8)   ! Neimark-Sacker bifurcation continuation (maps)
       CALL AUTOAE(AP,ICP,ICU,FNNS,STPNNS,FNCSDS)
    END SELECT

  END SUBROUTINE AUTODS

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
             DFDU(:,:NDIM)=DFDU2(:,:)
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

! ---------- --------
  SUBROUTINE STPNDS(AP,PAR,ICP,U,UDOT,NODIR)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)

    CALL STPNAE(AP,PAR,ICP,U,UDOT,NODIR)
    IF(AP%ISW==-1.AND.AP%ITP==7)THEN
       ! period doubling for maps: set iteration count
       AP%ITDS=NINT(AINT(PAR(11)))
    ENDIF

  END SUBROUTINE STPNDS

! ---------- ------
  SUBROUTINE FNLPDS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-par continuation of folds (maps)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNLPF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FNDS)

  END SUBROUTINE FNLPDS

! ---------- --------
  SUBROUTINE STPNLPDS(AP,PAR,ICP,U,UDOT,NODIR)

    ! Generates starting data for the 2-parameter continuation of
    ! Neimark-Sacker bifurcation point (map).

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)

    CALL STPNLPF(AP,PAR,ICP,U,UDOT,NODIR,FNDS)

  END SUBROUTINE STPNLPDS

! ---------- ------
  SUBROUTINE FNBPDS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-par continuation of folds (maps)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNBPF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FNDS)

  END SUBROUTINE FNBPDS

! ---------- --------
  SUBROUTINE STPNBPDS(AP,PAR,ICP,U,UDOT,NODIR)

    ! Generates starting data for the 2-parameter continuation of
    ! Neimark-Sacker bifurcation point (map).

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)

    CALL STPNBPF(AP,PAR,ICP,U,UDOT,NODIR,FNDS)

  END SUBROUTINE STPNBPDS

! ---------- ----
  SUBROUTINE FNNS(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-parameter continuation of Neimark-
    ! Sacker bifurcation points in maps.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNHBF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FNDS,FFNSX)

  END SUBROUTINE FNNS

! ---------- -----
  SUBROUTINE FFNSX(AP,U,PAR,DFDU,DFDV)

    USE SUPPORT, ONLY: PI

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: U(AP%NDM*2+2)
    DOUBLE PRECISION, INTENT(IN) :: PAR(*), DFDU(AP%NDM,AP%NDM)
    DOUBLE PRECISION, INTENT(OUT) :: DFDV(AP%NDM,AP%NDM+1)

    INTEGER I,NDM
    DOUBLE PRECISION THTA

    NDM=AP%NDM
    IF(AP%ITP==8)THEN
       ! initialization
       THTA=PI(2.d0)/PAR(11)
       U(2*NDM+1)=THTA
    ENDIF

    ! construct matrix for extended Neimark-Sacker system
    ! Kuznetsov, 3rd ed., (10.83)
    ! A^2-2cA+I, and the derivative to theta: 2sA
    ! where A=DFDU, c=cos(theta), and s=sin(theta)
    CALL DGEMM('n','n',NDM,NDM,NDM,1.d0,DFDU,NDM,DFDU,NDM,0.d0,DFDV,NDM)
    THTA=U(NDM*2+1)
    DFDV(:,:)=DFDV(:,:)-2*COS(THTA)*DFDU(:,:)
    DO I=1,NDM
       DFDV(I,I)=DFDV(I,I)+1
    ENDDO
    CALL DGEMV('n',NDM,NDM,2*SIN(THTA),DFDU,NDM,U(NDM+1),1,0d0,&
         DFDV(1,NDM+1),1)

  END SUBROUTINE FFNSX

! ---------- ------
  SUBROUTINE STPNNS(AP,PAR,ICP,U,UDOT,NODIR)

    USE SUPPORT, ONLY: PI

    ! Generates starting data for the 2-parameter continuation of
    ! Neimark-Sacker bifurcation point (map).

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),U(*),UDOT(*)

    CALL STPNHBF(AP,PAR,ICP,U,UDOT,NODIR,FNDS,FFNSX)

  END SUBROUTINE STPNNS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNCSDS(AP,ICP,U,NDIM,PAR,ITEST,ITP) RESULT(Q)

    USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
    USE SUPPORT, ONLY: AA=>P0V

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(IN) :: ITEST
    INTEGER, INTENT(OUT) :: ITP

    SELECT CASE(ITEST)
    CASE(4)
       Q=FNRNDS(AP,ITP,U,AA)
    CASE(6)
       Q=FNHBDS(AP,PAR,ITP,AA)
    CASE DEFAULT
       Q=FNCSEQF(AP,ICP,U,NDIM,PAR,ITEST,ITP,FNDS)
    END SELECT

  END FUNCTION FNCSDS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNRNDS(AP,ITP,U,AA)

    USE SUPPORT, ONLY: CHECKSP, PI, LBTYPE

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: U(AP%NDIM), AA(AP%NDIM+1,AP%NDIM+1)
! Local
    INTEGER NTOP,ITPST
    DOUBLE PRECISION THETA

    FNRNDS = 0
    ITP=0
    ITPST=AP%ITPST

    IF(ITPST==2.OR.ITPST==7)THEN
       ! Rn on Fold/PD curve
       IF(AP%ITPST==2)THEN
          IF(.NOT.CHECKSP('R1',AP%IPS,AP%ILP,AP%ISP))RETURN
          ITP=-5
       ELSE
          IF(.NOT.CHECKSP('R2',AP%IPS,AP%ILP,AP%ISP))RETURN
          ITP=-6
       ENDIF
       ITP=ITP-10*ITPST
       FNRNDS=FNBTAE(AP,U,AA)
    ELSEIF(ITPST==8)THEN
       ! check the angle for resonances on Torus bifurcations
       THETA=U(AP%NDIM-1)
       SELECT CASE(NINT(THETA*6/PI(1d0)))
       CASE(3) ! 1:4 res
          ITP=-8
       CASE(4) ! 1:3 res
          ITP=-7
       CASE(6) ! 1:2 res
          ITP=-6
       CASE DEFAULT ! 1:1 res
          ITP=-5
       END SELECT
       ITP=ITP-10*ITPST
       IF(.NOT.CHECKSP(LBTYPE(ITP),AP%IPS,AP%ILP,AP%ISP))ITP=0
       FNRNDS=THETA*(THETA-PI(.5d0))*(THETA-PI(2d0/3))*(THETA-PI(1d0))
    ELSE
       RETURN
    ENDIF

    NTOP=MOD(AP%NTOT-1,9999)+1
    IF(ITP/=0.AND.AP%IID>=2)WRITE(9,101)ABS(AP%IBR),NTOP+1,FNRNDS
101 FORMAT(I4,I6,9X,'Rn   Function:',ES14.5)

  END FUNCTION FNRNDS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNHBDS(AP,PAR,ITP,AA)

    USE SUPPORT, ONLY: PI, EVV, EIG, CHECKSP, LBTYPE

    DOUBLE PRECISION, PARAMETER :: RLARGE=1.0d+30

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: AA(AP%NDIM+1,AP%NDIM+1)
! Local
    COMPLEX(KIND(1.0D0)), ALLOCATABLE :: EV(:)
    DOUBLE PRECISION, ALLOCATABLE :: AAA(:,:)
    INTEGER NDM,ISP,IID,IBR,NTOT,NTOP,NINS,I,LOC,ITPST
    DOUBLE PRECISION RIMHB,REV

    NDM=AP%NDM
    ISP=AP%ISP
    IID=AP%IID
    IBR=AP%IBR
    ITPST=AP%ITPST
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1
    ALLOCATE(EV(NDM))

! Compute the eigenvalues of the Jacobian

    ALLOCATE(AAA(NDM,NDM))
    AAA(:,:)=AA(1:NDM,1:NDM)
    CALL EIG(AP,NDM,NDM,AAA,EV)
    DEALLOCATE(AAA)
    EV(:)=EV(:)+1d0
    DO I=1,NDM
       IF(EV(I)==CMPLX(0d0,0d0,KIND(1d0)))THEN
          EV(I)=CMPLX(-RLARGE,0.d0,KIND(1.0D0))
       ELSE
          EV(I)=LOG(EV(I))
       ENDIF
    ENDDO

    CALL STABEQ(AP,AA,EV,NINS,LOC)
    EVV(:)=EXP(EV(:))

    REV=0.d0
    IF(LOC>0)THEN
       REV=REAL(EV(LOC))
       RIMHB=ABS(AIMAG(EV(LOC)))
       IF(RIMHB.NE.0.d0.AND.ITPST==0)PAR(11)=PI(2.d0)/RIMHB
    ENDIF

    ITP=TPSPAE(AP%EPSS,ITPST,PAR(11))
    IF(ITPST/=0.OR..NOT.CHECKSP(LBTYPE(ITP),AP%IPS,AP%ILP,ISP))THEN
       FNHBDS=0d0
       ITP=0
    ELSE
       FNHBDS=REV
       IF(IID>=2)WRITE(9,101)ABS(IBR),NTOP+1,FNHBDS
    ENDIF
    AP%HBFF=FNHBDS
    IF(NINS==AP%NINS.AND.AP%ITPST/=8)ITP=0
    AP%NINS=NINS
    CALL PRINTEIG(AP)

101 FORMAT(I4,I6,9X,'Hopf Function:',ES14.5)

  END FUNCTION FNHBDS

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

END MODULE MAPS
