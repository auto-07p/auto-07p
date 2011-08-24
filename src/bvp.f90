!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    General Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE BVP

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: AUTOBV,SETRTN,IRTN,NRTN

  INTEGER, ALLOCATABLE :: NRTN(:)
  INTEGER IRTN

CONTAINS

! ---------- ------
  SUBROUTINE AUTOBV(AP,ICP,ICU,FUNI,BCNI,ICNI,STPNBVI,FNCI)

    USE AUTOMPI

! THIS IS THE ENTRY ROUTINE FOR GENERAL BOUNDARY VALUE PROBLEMS.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ICU(*)

    include 'interfaces.h'

    IF(MPIIAM()>0)THEN
!        This is a little trick to tell MPI workers what FUNI and ICNI
!        are.
       DO WHILE(MPIWFI(.TRUE.))
          CALL mpi_setubv_worker(FUNI,ICNI,BCNI)
       ENDDO
       RETURN
    ENDIF
    CALL CNRLBV(AP,ICP,ICU,FUNI,BCNI,ICNI,STPNBVI,FNCI)

  END SUBROUTINE AUTOBV

! ---------- -----------------
  subroutine mpi_setubv_worker(funi,icni,bcni)
    use autompi
    use solvebv

    integer iam,kwt
    include 'interfaces.h'

    integer :: ndim, ifst, nllv, na, ncol, nint, ntst, nfpr
    integer :: npar
    type(autoparameters) ap

    double precision, allocatable :: ups(:,:), uoldps(:,:), rlold(:)
    double precision, allocatable :: udotps(:,:), upoldp(:,:), thu(:)
    double precision, allocatable :: dups(:,:), dtm(:), par(:)
    integer, allocatable :: np(:),icp(:)
    double precision :: dum,dum1(1),det

    call mpibcastap(ap)
    iam=mpiiam()
    kwt=mpikwt()

    ndim=ap%ndim
    ntst=ap%ntst
    ncol=ap%ncol
    nint=ap%nint
    nfpr=ap%nfpr
    npar=ap%npar

    allocate(np(kwt))
    call partition(ntst,kwt,np)
    na=np(iam+1)
    deallocate(np)

    allocate(icp(nfpr+nint),thu(ndim),dtm(na),par(npar),rlold(nfpr))
    allocate(ups(ndim,0:na*ncol),uoldps(ndim,0:na*ncol))
    allocate(udotps(ndim,0:na*ncol),upoldp(ndim,0:na*ncol))
    allocate(dups(ndim,0:na*ncol-1))

    call mpisbv(ap,par,icp,ndim,ups,uoldps,rlold,udotps,upoldp, &
         dtm,thu,ifst,nllv)
    dum=0
    call solvbv(ifst,ap,det,par,icp,funi,bcni,icni,dum, &
         nllv,dum1,dum1,dum1,ndim,ups,uoldps,udotps,upoldp,dtm, &
         dups,dum1,dum1,dum1,dum1,thu)

    ! free input arrays
    deallocate(ups,uoldps,dtm,udotps,upoldp,dups,thu,icp,par)

  end subroutine mpi_setubv_worker

! ---------- ------
  SUBROUTINE CNRLBV(AP,ICP,ICU,FUNI,BCNI,ICNI,STPNBVI,FNCI)

    USE IO
    USE MESH
    USE SUPPORT, ONLY: DTM=>DTV, P0=>P0V, P1=>P1V, EV=>EVV, CHECKSP, STOPPED, &
         INITSTOPCNTS, INIT2, INIT3, FNCS, PVLI, LBITP
    USE AUTO_CONSTANTS, ONLY: NPARX

! Controls the computation of solution branches.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ICU(*)
! Local
    INTEGER, ALLOCATABLE :: IUZ(:)
    DOUBLE PRECISION, ALLOCATABLE :: PAR(:),VUZ(:),THU(:),THL(:)
    DOUBLE PRECISION, ALLOCATABLE :: RLCUR(:),RLOLD(:),RLDOT(:)
    DOUBLE PRECISION, ALLOCATABLE :: UPS(:,:),UOLDPS(:,:),UPOLDP(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: UDOTPS(:,:),TM(:),TEST(:)
    INTEGER NDIM,IPS,IRS,ILP,NTST,NCOL,IAD,IADS,ISP,ISW,NUZR,ITP,ITPST,NFPR
    INTEGER IBR,IPERP,ISTOP,ITNW,ITEST,I,NITPS,NODIR,NTOP,NTOT,NPAR,NINS
    INTEGER ITPDUM,IFOUND,ISTEPPED
    INTEGER STOPCNTS(-9:14)
    DOUBLE PRECISION DS,DSMAX,DSOLD,DSTEST,RDS,SP1
    LOGICAL STEPPED
    CHARACTER(4) ATYPE,ATYPEDUM

! INITIALIZE COMPUTATION OF BRANCH

    IF(AP%ISW==-2.OR.AP%ISW==-3)THEN
       AP%ILP=0
       AP%ISP=0
       AP%NMX=5
       WRITE(6,"(/,A,A)")' Generating starting data :', &
            ' Restart at EP label below :'
    ENDIF
    CALL INIT2(AP,ICP,ICU)

    NDIM=AP%NDIM
    IPS=AP%IPS
    IRS=AP%IRS
    ILP=AP%ILP
    NTST=AP%NTST
    NCOL=AP%NCOL
    IAD=AP%IAD
    IADS=AP%IADS
    ISP=AP%ISP
    ISW=AP%ISW
    NUZR=AP%NUZR
    ITP=AP%ITP
    ITPST=AP%ITPST
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! allocate a minimum of NPARX so we can detect overflows 
    ! past NPAR gracefully
    ALLOCATE(PAR(MAX(NPAR,NPARX)),THL(NFPR),THU(NDIM),IUZ(NUZR),VUZ(NUZR))
    CALL INIT3(AP,ICP,PAR,THL,THU,IUZ,VUZ)

    ALLOCATE(RLCUR(NFPR),RLDOT(NFPR),RLOLD(NFPR))
    ALLOCATE(UPS(NDIM,0:NTST*NCOL),UOLDPS(NDIM,0:NTST*NCOL))
    ALLOCATE(UPOLDP(NDIM,0:NTST*NCOL),UDOTPS(NDIM,0:NTST*NCOL))
    ALLOCATE(TM(0:NTST),DTM(NTST))
    ALLOCATE(P0(NDIM,NDIM),P1(NDIM,NDIM),TEST(AP%NTEST),EV(AP%NDM))

    DS=AP%DS

    RDS=DS
    DSOLD=RDS
    CALL INITSTOPCNTS(ISP,ILP,ITPST,STOPCNTS)
    IF(ISP.LT.0)THEN
       ISP=-ISP
       AP%ISP=ISP
    ENDIF
    DO I=1,AP%NTEST
       TEST(I)=0.d0
    ENDDO
    NITPS=0
    NTOT=0
    AP%NTOT=NTOT
    ISTOP=0

    DO I=1,NFPR
       RLCUR(I)=0.d0
       RLOLD(I)=0.d0
       RLDOT(I)=0.d0
    ENDDO

    UPS(:,:)=0.d0
    UOLDPS(:,:)=0.d0
    UPOLDP(:,:)=0.d0
    UDOTPS(:,:)=0.d0

    NODIR=0
    CALL RSPTBV(AP,PAR,ICP,FUNI,STPNBVI,FNCI,RLCUR,RLOLD,RLDOT, &
         NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THU)

!     don't set global rotations here for homoclinics, but in autlib5.f
    IF(IPS.NE.9)CALL SETRTN(AP%NDM,NTST*NCOL,NDIM,UPS)

    IPERP=2
    IF(NODIR.EQ.1 .AND. ISW.GT.0)THEN
       ! no direction given or valid; no branch switch
       IPERP=0
    ELSEIF(IRS.NE.0 .AND. ISW.LT.0)THEN
       ! branch switch
       IPERP=1
    ELSEIF( ISP/=0 .AND. (IPS==2.OR.IPS==7.OR.IPS==12) .AND. &
         ITP/=3 .AND. ABS(ITP/10)/=3) THEN
       ! periodic orbit with detection of special points: compute FMs
       ! (but not at a Hopf bifurcation)
       IPERP=-1
    ENDIF
    IF(IPERP/=2)THEN
       CALL STDRBV(AP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,IPERP,P0,P1,THL,THU)
       IF(ISP/=0 .AND. (IPS==2.OR.IPS==7.OR.IPS==12) )THEN
          ! determine and print Floquet multipliers and stability
          SP1 = FNCI(AP,ICP,UPS,NDIM,PAR,3,ATYPEDUM)
       ENDIF
    ENDIF

! Store plotting data for restart point :

    CALL STHD(AP,ICP)
    IF(IRS.EQ.0) THEN
       ITP=9+10*ITPST
    ELSE
       ITP=0
    ENDIF
    AP%ITP=ITP
    CALL PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)
    CALL STPLBV(AP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THU,ISTOP)
    DO WHILE(ISTOP==0)
       ITP=0
       AP%ITP=ITP
       NINS=AP%NINS
       CALL STEPBV(AP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,FNCI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)

       IFOUND=0
       DSTEST=DSOLD
       IF(ISTOP/=0)THEN
          ISTEPPED=AP%NTEST+1
       ELSE
          ISTEPPED=0
          DO ITEST=1,AP%NTEST
             ! Check for special points
             CALL LCSPBV(AP,DSOLD,DSTEST,PAR,ICP,ITEST,FUNI,BCNI,ICNI,FNCI, &
                  TEST(ITEST),RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS, &
                  UPOLDP,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ATYPE,STEPPED)
             IF(STEPPED)ISTEPPED=ITEST
             IF(LEN_TRIM(ATYPE)>0)THEN
                IFOUND=ITEST
                AP%ITP=LBITP(ATYPE,.TRUE.)
                AP%ITP=AP%ITP+SIGN(10,AP%ITP)*ITPST
             ENDIF
          ENDDO
       ENDIF

       DO ITEST=1,ISTEPPED-1
          ! evaluate the test functions for the next step
          TEST(ITEST)=FNCS(AP,ICP,UPS,PAR,ATYPEDUM,IUZ,VUZ,ITEST,FNCI)
       ENDDO

       ITP=AP%ITP
       IF(ITP/=0)THEN
          IF(STOPPED(IUZ,IFOUND,NUZR,ITP,STOPCNTS))THEN
             ISTOP=-1 ! *Stop at the first found bifurcation
          ENDIF
          IF(MOD(ITP,10)/=-4)THEN
             ! for plotter: use stability of previous point
             ! for bifurcation points
             AP%NINS=NINS
          ENDIF
       ENDIF

! Store plotting data.

       CALL PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)
       CALL STPLBV(AP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THU,ISTOP)

       IF(ISTOP/=0)EXIT
       NTOT=AP%NTOT

! Adapt the mesh to the solution.

       IF(IAD.NE.0)THEN
          IF(MOD(NTOT,IAD).EQ.0)THEN
             CALL ADAPT(NTST,NCOL,NDIM,TM,DTM,UPS,UOLDPS, &
                  ((IPS==2.OR.IPS==12) .AND. ABS(ISW)<=1))
          ENDIF
       ENDIF

! Adapt the stepsize along the branch.

       IF(IADS.NE.0)THEN
          IF(MOD(NTOT,IADS).EQ.0)THEN
             ITNW=AP%ITNW
             IBR=AP%IBR
             NTOP=MOD(NTOT-1,9999)+1
             DSMAX=AP%DSMAX
             CALL ADPTDS(NITPS,ITNW,IBR,NTOP,AP%IID,DSMAX,RDS)
             AP%RDS=RDS
          ENDIF
       ENDIF

! Update UOLDPS, UDOTPS, UPOLDP, RLOLD, and RLDOT.

       CALL CONTBV(AP,DSOLD,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)

    ENDDO
    DEALLOCATE(PAR,THL,THU,IUZ,VUZ)
    DEALLOCATE(EV,UPS,UOLDPS,UPOLDP,UDOTPS,TM,DTM,P0,P1)
    DEALLOCATE(TEST,RLCUR,RLOLD,RLDOT)

  END SUBROUTINE CNRLBV

! ---------- ------
  SUBROUTINE CONTBV(AP,DSOLD,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)

    USE MESH

! Computes new rate of change (UDOTPS,RLDOT) and time derivative (UPOLDP)
! arrays depending on (UOLDPS,RLOLD) and (UPS,RLCUR), and then replaces
! (UOLDPS,RLOLD) with the current solution in (UPS,RLCUR).
! The stepsize used in the preceding step has been stored in DSOLD.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    DOUBLE PRECISION, INTENT(IN) :: DSOLD
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*), UPS(NDIM,0:*), UOLDPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(OUT) :: UDOTPS(NDIM,0:*), UPOLDP(NDIM,0:*)
    DOUBLE PRECISION, INTENT(IN) :: RLCUR(AP%NFPR), DTM(*), THL(*), THU(*)
    DOUBLE PRECISION, INTENT(INOUT) :: RLOLD(AP%NFPR)
    DOUBLE PRECISION, INTENT(OUT) :: RLDOT(AP%NFPR)

    INTEGER NTST,NCOL

    NTST=AP%NTST
    NCOL=AP%NCOL

! Compute rate of change (along branch) of PAR(ICP(1)) and U :

    UDOTPS(:,0:NCOL*NTST)=(UPS(:,0:NCOL*NTST)-UOLDPS(:,0:NCOL*NTST))/DSOLD
    RLDOT(:)=(RLCUR(:)-RLOLD(:))/DSOLD
! Rescale, to set the norm of (UDOTPS,RLDOT) equal to 1.
    CALL SCALEB(NTST,NCOL,NDIM,AP%NFPR,UDOTPS,RLDOT,DTM,THL,THU)

! Store time-derivative.

    CALL STUPBV(AP,PAR,ICP,FUNI,NDIM,UPS,UPOLDP)

    RLOLD(:)=RLCUR(:)
    UOLDPS(:,0:NCOL*NTST)=UPS(:,0:NCOL*NTST)

  END SUBROUTINE CONTBV

! ---------- ------
  SUBROUTINE STUPBV(AP,PAR,ICP,FUNI,NDIM,UPS,UPOLDP)

! Stores U-prime (derivative with respect to T) in UPOLDP.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: UPS(NDIM,0:*), PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: UPOLDP(NDIM,0:*)
! Local
    INTEGER NTST,NCOL,NPAR,J
    DOUBLE PRECISION, ALLOCATABLE :: UOLD(:),DFDU(:,:),DFDP(:,:)

    NTST=AP%NTST
    NCOL=AP%NCOL
    NPAR=AP%NPAR

    ALLOCATE(UOLD(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,NPAR))
    DFDU(:,:)=0.d0
    DFDP(:,:)=0.d0

    DO J=0,NTST*NCOL
       UOLD(:)=UPS(:,J)
       CALL FUNI(AP,NDIM,UPS(1,J),UOLD,ICP,PAR,0,UPOLDP(1,J),&
            DFDU,DFDP)
    ENDDO

    DEALLOCATE(UOLD,DFDU,DFDP)
  END SUBROUTINE STUPBV

! ---------- ------
  SUBROUTINE STEPBV(AP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,FNCI,RDS, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)

    USE MESH

! Controls the solution of the nonlinear equations (by Newton's method)
! for the next solution (PAR(ICP(*)) , U) on a branch of solutions.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*), NDIM
    INTEGER, INTENT(OUT) :: ISTOP, NITPS
    DOUBLE PRECISION, INTENT(OUT) :: UPS(NDIM,0:*), RLCUR(AP%NFPR)
    DOUBLE PRECISION, INTENT(IN) :: UOLDPS(NDIM,0:*), RLOLD(AP%NFPR)
    DOUBLE PRECISION, INTENT(IN) :: UDOTPS(NDIM,0:*), RLDOT(AP%NFPR), UPOLDP(NDIM,0:*)
    DOUBLE PRECISION, INTENT(IN) :: TM(*), DTM(*), THL(*), THU(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*), RDS
    DOUBLE PRECISION, INTENT(OUT) :: DSOLD, P0(*),P1(*)

    INTEGER NTST,NCOL,IADS,IID,ITNW,NFPR,IBR,NTOT,NTOP,I
    DOUBLE PRECISION DSMIN,DSMAX
    LOGICAL CONVERGED

    NTST=AP%NTST
    NCOL=AP%NCOL
    IADS=AP%IADS
    IID=AP%IID
    ITNW=AP%ITNW
    NFPR=AP%NFPR
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    DSMIN=AP%DSMIN

    DO
       DSOLD=RDS

! Perform Newton iterations

       CALL NEWTONBV(AP,PAR,ICP,FUNI,BCNI,ICNI,FNCI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,CONVERGED)
       IF(CONVERGED)THEN
          ISTOP=0
          RETURN
       ENDIF

! Maximum number of iterations reached.

       IF(IADS.EQ.0)THEN
          IF(IID>0)WRITE(9,101)IBR,NTOP
          EXIT
       ENDIF

! Reduce stepsize and try again.

       DSMAX=AP%DSMAX
       NITPS=ITNW
       CALL ADPTDS(NITPS,ITNW,IBR,NTOP,IID,DSMAX,RDS)
       AP%RDS=RDS
       IF(ABS(RDS).LT.DSMIN)THEN
          ! Minimum stepsize reached.
          IF(IID>0)WRITE(9,103)IBR,NTOP
          EXIT
       ENDIF
       IF(IID.GE.2)WRITE(9,102)IBR,NTOP
    ENDDO

! Minimum stepsize reached.

    DO I=1,NFPR
       RLCUR(I)=RLOLD(I)
       PAR(ICP(I))=RLCUR(I)
    ENDDO
    UPS(:,0:NCOL*NTST)=UOLDPS(:,0:NCOL*NTST)
    ISTOP=1

101 FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
102 FORMAT(I4,I6,' NOTE:Retrying step')
103 FORMAT(I4,I6,' NOTE:No convergence using minimum step size')

  END SUBROUTINE STEPBV

! ---------- --------
  SUBROUTINE NEWTONBV(AP,PAR,ICP,FUNI,BCNI,ICNI,FNCI,RDS, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,THL,THU,NITPS,CONVERGED)

! This subroutine contains the main predictor-corrector loop

    USE MESH, ONLY: SCALEB
    USE SOLVEBV, ONLY: SOLVBV
    USE SUPPORT, ONLY: CHECKSP, PVLI

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*), NDIM
    INTEGER, INTENT(OUT) :: NITPS
    DOUBLE PRECISION, INTENT(OUT) :: UPS(NDIM,0:*), RLCUR(AP%NFPR)
    DOUBLE PRECISION, INTENT(IN) :: UOLDPS(NDIM,0:*), RLOLD(AP%NFPR)
    DOUBLE PRECISION, INTENT(IN) :: UDOTPS(NDIM,0:*), RLDOT(AP%NFPR), UPOLDP(NDIM,0:*)
    DOUBLE PRECISION, INTENT(IN) :: TM(*), DTM(*), THL(*), THU(*), RDS
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: P0(*),P1(*)
    LOGICAL, INTENT(OUT) :: CONVERGED

    INTEGER NTST,NCOL,IID,ITNW,NWTN,NFPR,IFST,NLLV,I,J,NIT1,IPS,ILP,ISP
    DOUBLE PRECISION EPSL,EPSU,DELREF,DELMAX,ADRL,ADU,AU,DET
    DOUBLE PRECISION DUMX,RDRL,RDUMX,UMX,RDSZ
    DOUBLE PRECISION, ALLOCATABLE :: DUPS(:,:),DRL(:),P0T(:),P1T(:)
    LOGICAL DONE

    IPS=AP%IPS
    ILP=AP%ILP
    NTST=AP%NTST
    NCOL=AP%NCOL
    ISP=AP%ISP
    IID=AP%IID
    ITNW=AP%ITNW
    NWTN=AP%NWTN
    NFPR=AP%NFPR

    EPSL=AP%EPSL
    EPSU=AP%EPSU

    ! Extrapolate to get the approximation to the next solution point.

    RLCUR(:)=RLOLD(:)+RDS*RLDOT(:)
    UPS(:,0:NCOL*NTST)=UOLDPS(:,0:NCOL*NTST)+RDS*UDOTPS(:,0:NCOL*NTST)

! Write additional output on unit 9 if requested.

    NITPS=0
    CALL WRTBV9(AP,RLCUR,NDIM,UPS,TM,DTM,THU,NITPS)

! Generate the Jacobian matrix and the right hand side.

    ALLOCATE(DUPS(NDIM,0:NTST*NCOL),DRL(NFPR))
    CONVERGED=.FALSE.
    DELREF=0
    DO NIT1=1,ITNW

       NITPS=NIT1
       NLLV=0

       IFST=0
       IF(NITPS.LE.NWTN)IFST=1

       CALL SOLVBV(IFST,AP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDS,NLLV, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,DUPS,DRL, &
            P0,P1,THL,THU)
       AP%DET=DET

! Add Newton increments.

       DO I=1,NFPR
          RLCUR(I)=RLCUR(I)+DRL(I)
          PAR(ICP(I))=RLCUR(I)
       ENDDO

       DUMX=0.d0
       UMX=0.d0
       DO J=0,NTST*NCOL
          DO I=1,NDIM
             ADU=ABS(DUPS(I,J))
             IF(ADU.GT.DUMX)DUMX=ADU
             AU=ABS(UPS(I,J))
             IF(AU.GT.UMX)UMX=AU
             UPS(I,J)=UPS(I,J)+DUPS(I,J)
          ENDDO
       ENDDO

       CALL WRTBV9(AP,RLCUR,NDIM,UPS,TM,DTM,THU,NITPS)

! Check whether user-supplied error tolerances have been met :

       DONE=.TRUE.
       RDRL=0.d0
       DO I=1,NFPR
          ADRL=ABS(DRL(I))/(1.d0+ABS(RLCUR(I)))
          IF(ADRL.GT.EPSL)DONE=.FALSE.
          IF(ADRL.GT.RDRL)RDRL=ADRL
       ENDDO
       RDUMX=DUMX/(1.d0+UMX)
       IF(DONE.AND.RDUMX.LT.EPSU)THEN
          IF(CHECKSP('LP',IPS,ILP,ISP))THEN
             ! Find the direction vector (for test functions)
             ALLOCATE(P0T(NDIM*NDIM),P1T(NDIM*NDIM))
             NLLV=-1
             IFST=0
             RDSZ=0.d0

             CALL SOLVBV(IFST,AP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
                  RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,DUPS,&
                  DRL,P0T,P1T,THL,THU)

             ! Scale the direction vector.
             CALL SCALEB(NTST,NCOL,NDIM,NFPR,DUPS,DRL,DTM,THL,THU)
             AP%FLDF=DRL(1)
             DEALLOCATE(P0T,P1T)
          ENDIF

          CALL PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)
          IF(IID.GE.2)WRITE(9,*)
          DEALLOCATE(DUPS,DRL)
          CONVERGED=.TRUE.
          RETURN
       ENDIF

       IF(NITPS.EQ.1)THEN
          DELREF=20*DMAX1(RDRL,RDUMX)
       ELSE
          DELMAX=DMAX1(RDRL,RDUMX)
          IF(DELMAX.GT.DELREF)EXIT
       ENDIF

    ENDDO
    DEALLOCATE(DUPS,DRL)

  END SUBROUTINE NEWTONBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!      Restart of Solution Branches ( Differential Equations )
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE RSPTBV(AP,PAR,ICP,FUNI,STPNBVI,FNCI,RLCUR,RLOLD, &
       RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THU)

    USE IO
    USE MESH
    USE SUPPORT, ONLY: PVLI
    USE AUTO_CONSTANTS, ONLY: TY

! Restarts computation of a branch of solutions at point labelled IRS.
! The output written on unit 8 by a previous run is now expected as
! input on unit 3. The label IRS, where computation is to resume, must
! be specified in the user-supplied subroutine INIT.
! If IRS=0 then the starting point must be provided analytically in the
! user-supplied subroutine STPNT.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER ICP(*),NDIM,NODIR
    DOUBLE PRECISION UPS(NDIM,0:*),UOLDPS(NDIM,0:*),UPOLDP(NDIM,0:*)
    DOUBLE PRECISION UDOTPS(NDIM,0:*),TM(0:*),DTM(*),PAR(*)
    DOUBLE PRECISION RLCUR(AP%NFPR),RLOLD(AP%NFPR),RLDOT(AP%NFPR)
    DOUBLE PRECISION THU(*)

    DOUBLE PRECISION, ALLOCATABLE :: U(:),UDOT(:)
    INTEGER :: ICPRS(2),IRS,NTST,NCOL,ITP,NFPR,NCOLRS,NTSRS,I,J

    IRS=AP%IRS
    NTST=AP%NTST
    NCOL=AP%NCOL
    ITP=AP%ITP
    NFPR=AP%NFPR

! Get restart data :

    IF(IRS>0)THEN
       NTSRS=GETNTST3()
       NCOLRS=GETNCOL3()
    ELSE
       NTSRS=NTST
       NCOLRS=NCOL
    ENDIF
    IF(NCOLRS*NTSRS==0)THEN
       ALLOCATE(U(NDIM),UDOT(NDIM+1))
       CALL READLB(AP,ICPRS,U,UDOT,PAR)

! Generate the (initially uniform) mesh.

       CALL MSH(NTST,TM)

       DO J=0,NTST*NCOL
          UPS(:,J)=U(:)
       ENDDO

       IF(ITP==3 .OR. ABS(ITP/10)==3) THEN
          IF(LEN_TRIM(TY)>2)THEN
             READ(TY(3:),'(I5)')I
             PAR(11)=PAR(I)
          ENDIF
          ! call PVLS here the first time so the parameters can be initialized
          CALL PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)
          ! Hopf bifurcation
          CALL STHOPF(AP,U,PAR,ICP,NTST,NCOL,NFPR,RLDOT, &
               NDIM,UDOTPS,UPOLDP,NODIR,THU,FUNI)
       ELSE
          ! else we just use the uniform mesh with no direction given
          NODIR=1
       ENDIF
       DEALLOCATE(U,UDOT)
    ELSE
       CALL STPNBVI(AP,PAR,ICP,NTSRS,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)
    ENDIF

! Determine a suitable starting label and branch number.

    CALL NEWLAB(AP)

    DTM(1:NTST)=TM(1:NTST)-TM(0:NTST-1)

! Set UOLDPS, RLOLD.

    IF(NODIR.NE.-1)THEN
       ! call PVLS here the first time so the parameters can be initialized
       CALL PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)
    ENDIF
    DO I=1,NFPR
       RLCUR(I)=PAR(ICP(I))
       RLOLD(I)=RLCUR(I)
    ENDDO

    UOLDPS(:,0:NCOL*NTST)=UPS(:,0:NCOL*NTST)

! Store U-prime (derivative with respect to time or space variable).

    IF(NODIR.EQ.-1)THEN
!      ** Restart from a Hopf bifurcation.
       NODIR=0
       AP%ISW=1
    ELSE
!      ** Restart from orbit.
       CALL STUPBV(AP,PAR,ICP,FUNI,NDIM,UPS,UPOLDP)
    ENDIF

  END SUBROUTINE RSPTBV

! ---------- ------
  SUBROUTINE STHOPF(AP,U,PAR,ICP,NTST,NCOL, &
       NFPR,RLDOT,NDIM,UDOTPS,UPOLDP,NODIR,THU,FUNI)

    USE IO
    USE MESH
    USE SUPPORT
    USE AUTO_CONSTANTS, ONLY: TY

!  Generates starting data for a periodic orbit from a Hopf
!  bifurcation point (for waves or periodic orbits)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER ICP(*),NDIM,NTST,NCOL,NFPR,NODIR
    DOUBLE PRECISION U(NDIM),PAR(*),RLDOT(AP%NFPR),THU(*)
    DOUBLE PRECISION UDOTPS(NDIM,0:*),UPOLDP(NDIM,0:*)
    include 'interfaces.h'
! Local
    INTEGER I,J
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),SMAT(:,:),RNLLV(:),F(:),DTM(:)
    DOUBLE PRECISION DUMDFP(1)
    DOUBLE PRECISION THL(2)
    DOUBLE PRECISION PERIOD,TPI,RIMHB,T,C,S

    ALLOCATE(DFU(NDIM,NDIM),F(NDIM),RNLLV(2*NDIM),SMAT(2*NDIM,2*NDIM))

    PERIOD=PAR(11)
    TPI=PI(2.d0)
    RIMHB=TPI/PERIOD

    SMAT(:,:)=0.d0

    DO I=1,NDIM
       SMAT(I,I)=-RIMHB
       SMAT(NDIM+I,NDIM+I)=RIMHB
    ENDDO

    CALL FUNI(AP,NDIM,U,U,ICP,PAR,1,F,DFU,DUMDFP)

! Note that the period-scaling in FUNC is taken into account:
    SMAT(1:NDIM,NDIM+1:2*NDIM)=DFU(:,:)/PAR(11)
    SMAT(NDIM+1:2*NDIM,1:NDIM)=DFU(:,:)/PAR(11)

    CALL NLVC(2*NDIM,2*NDIM,2,SMAT,RNLLV)
    CALL NRMLZ(2*NDIM,RNLLV)

    DO J=0,NTST*NCOL
       T=J*TPI/(NTST*NCOL)
       S=SIN(T)
       C=COS(T)
       UDOTPS(:,J)=S*RNLLV(1:NDIM)+C*RNLLV(NDIM+1:2*NDIM)
       UPOLDP(:,J)=C*RNLLV(1:NDIM)-S*RNLLV(NDIM+1:2*NDIM)
    ENDDO

    THL(1:2)=0.d0

    ALLOCATE(DTM(NTST))
    DTM(:)=1.d0/NTST

    CALL SCALEB(NTST,NCOL,NDIM,NFPR,UDOTPS,RLDOT,DTM,THL,THU)

    NODIR=-1

    DEALLOCATE(DFU,F,RNLLV,SMAT,DTM)
  END SUBROUTINE STHOPF

! ---------- ------
  SUBROUTINE SETRTN(NDM,NTNC,NDIM,UPS)

    USE SUPPORT

! Initialization for rotations
    
    INTEGER, INTENT(IN) :: NDM, NTNC, NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTNC)

    INTEGER I

    ALLOCATE(NRTN(NDM))
    IRTN=0
    DO I=1,NDM
       NRTN(I)=NINT( (UPS(I,NTNC)-UPS(I,0)) / PI(2.d0) )
       IF(NRTN(I).NE.0)THEN
          IRTN=1
       ENDIF
    ENDDO
    IF(IRTN.EQ.0)DEALLOCATE(NRTN)

  END SUBROUTINE SETRTN

! ---------- ------
  SUBROUTINE STDRBV(AP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD, &
       RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,IPERP, &
       P0,P1,THL,THU)

    USE MESH
    USE SOLVEBV

! Generates a direction vector (UDOTPS,RLDOT) that is needed to start
! the computation of a branch when no direction vector is given.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER ICP(*),NDIM,IPERP
    DOUBLE PRECISION UDOTPS(NDIM,0:AP%NTST*AP%NCOL),DTM(*)
    DOUBLE PRECISION PAR(*),RLCUR(AP%NFPR),RLOLD(AP%NFPR),RLDOT(AP%NFPR)
    DOUBLE PRECISION THL(*),THU(*),UPS(NDIM,0:AP%NTST*AP%NCOL)
    DOUBLE PRECISION UOLDPS(*),UPOLDP(*),P0(*),P1(*)

    INTEGER NTST,NCOL,IID,NFPR,NLLV,IFST,I
    DOUBLE PRECISION RDSZ,DET
    DOUBLE PRECISION, ALLOCATABLE :: DUPS(:,:),DRL(:)

! Generate the Jacobian matrix with zero direction vector.
! (Then the last row of the Jacobian will be zero)
! in case the starting direction is to be determined.

    NTST=AP%NTST
    NCOL=AP%NCOL
    IID=AP%IID
    NFPR=AP%NFPR

    IF(IPERP.EQ.0)THEN
       UDOTPS(:,:)=0.d0
       RLDOT(:)=0.d0
    ENDIF

    RDSZ=0.d0
    NLLV=1
    IFST=1
    ALLOCATE(DUPS(NDIM,0:NCOL*NTST),DRL(NFPR))
    CALL SOLVBV(IFST,AP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
         RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,DUPS,DRL, &
         P0,P1,THL,THU)

    IF(IPERP==-1)THEN
       DEALLOCATE(DUPS,DRL)
       RETURN
    ENDIF
    
! Compute the starting direction.

    DO I=1,NFPR
       RLDOT(I)=DRL(I)
       PAR(ICP(I))=RLCUR(I)
    ENDDO

    UDOTPS(:,:)=DUPS(:,:)
    DEALLOCATE(DUPS,DRL)

! Scale the starting direction.

    CALL SCALEB(NTST,NCOL,NDIM,NFPR,UDOTPS,RLDOT,DTM,THL,THU)

! Make sure that RLDOT(1) is positive (unless practically zero: then look
! at other variables).

    DO I=1,NFPR
       IF(ABS(RLDOT(I))/(1.d0+ABS(RLCUR(I)))>AP%EPSL)THEN
          IF(RLDOT(I)<0.d0)THEN
             RLDOT(:)=-RLDOT(:)
             UDOTPS(:,:)=-UDOTPS(:,:)
          ENDIF
          EXIT
       ENDIF
    ENDDO
    IF(I>NFPR)THEN
       DO I=1,NDIM
          IF(ABS(UDOTPS(I,NTST*NCOL))/(1.d0+ABS(UPS(I,NTST*NCOL)))>AP%EPSU)THEN
             IF(UDOTPS(I,NTST*NCOL)<0.d0)THEN
                RLDOT(:)=-RLDOT(:)
                UDOTPS(:,:)=-UDOTPS(:,:)
             ENDIF
             EXIT
          ENDIF
       ENDDO
    ENDIF

    IF(IID.GE.2)THEN
       WRITE(9,101)
       DO I=1,NFPR 
          WRITE(9,102)ICP(I),RLDOT(I)
       ENDDO
    ENDIF

101 FORMAT(/,' Starting direction of the free parameter(s) : ')
102 FORMAT(' PAR(',I3,') :',E20.12)

  END SUBROUTINE STDRBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Detection and Location of Branch Points in Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE LCSPBV(AP,DSOLD,DSTEST,PAR,ICP,ITEST,FUNI,BCNI,ICNI,FNCI,Q, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ATYPE,STEPPED)

    USE SUPPORT

    DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

! This subroutine uses the Secant method to accurately locate folds
! branch points, and zero(es) of user parameter values.
! Such points are located as points on a solution branch where the
! function FNCS changes sign.
! It involves calling the basic solution subroutines CONTBV and STEP
! with decreasing values of RDS (stepsize along branch).
! The point is assumed to have been found with sufficient accuracy if
! the ratio between RDS and the user supplied value of DS is less than
! the user-supplied tolerance EPSS.
! This subroutine is called from CNRLB, which controls the computation
! of branches of solutions to general boundary value problems.

    include 'interfaces.h'
    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ITEST,IUZ(*),NDIM
    INTEGER, INTENT(INOUT) :: NITPS
    COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: EV(AP%NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*), UPS(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(INOUT) :: UOLDPS(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(INOUT) :: UDOTPS(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(INOUT) :: UPOLDP(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(IN) :: TM(*),DTM(*),THL(*),THU(*),VUZ(*)
    DOUBLE PRECISION, INTENT(INOUT) :: DSOLD,DSTEST,Q
    DOUBLE PRECISION, INTENT(INOUT) :: RLCUR(AP%NFPR),RLOLD(AP%NFPR)
    DOUBLE PRECISION, INTENT(INOUT) :: RLDOT(AP%NFPR)
    DOUBLE PRECISION, INTENT(INOUT) :: P0(NDIM,NDIM),P1(NDIM,NDIM)
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    LOGICAL, INTENT(OUT) :: STEPPED

    INTEGER I,IID,ITMX,IBR,NTOT,NTOP,NITSP1,ISTOP,NCOL,NFPR,NTST
    DOUBLE PRECISION DS,DSMAX,EPSS,Q0,Q1,DQ,RDS,RRDS,S0,S1
    DOUBLE PRECISION DETS,FLDFS,DSOLDS,NITPSS,DSTESTS
    CHARACTER(4) :: ATYPEDUM

    DOUBLE PRECISION, ALLOCATABLE :: RLOLDS(:),UOLDPSS(:,:),P0S(:,:),P1S(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: RLCURS(:),UPSS(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: RLDOTS(:),UDOTPSS(:,:)
    COMPLEX(KIND(1.0D0)), ALLOCATABLE :: EVS(:)

    IID=AP%IID
    ITMX=AP%ITMX
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1
    NCOL=AP%NCOL
    NTST=AP%NTST
    NFPR=AP%NFPR

    DS=AP%DS
    DSMAX=AP%DSMAX
    EPSS=AP%EPSS

    STEPPED=.FALSE.

! Check for zero.

    Q0=Q
    Q1=FNCS(AP,ICP,UPS,PAR,ATYPE,IUZ,VUZ,ITEST,FNCI)
    ! disable detected potential bifurcations without stability changes
    I=LEN_TRIM(ATYPE)
    IF(ATYPE(I:I)=='0')ATYPE=''

    IF(AP%ITP/=0.AND.ABS((1.d0+HMACH)*Q1*DSTEST) < &
         EPSS*(1+SQRT(ABS(DS*DSMAX)))*ABS(Q0-Q1))THEN
       ! there could be multiple test functions going through zero
       ! at a point. In general, use "first come, first served", but
       ! In general, use "first come, first served", but
       ! bifurcations override UZ.
       IF(MOD(AP%ITP,10)==-4.AND.LEN_TRIM(ATYPE)>0.AND.TRIM(ATYPE)/='UZ')THEN
          Q=0.d0
          IF(IID>0)WRITE(9,102)RDS
          RETURN
       ENDIF
       Q1=0.d0
    ENDIF

    ! do not test via Q0*Q1 to avoid overflow.
    IF((Q0>=0.AND.Q1>=0) .OR. (Q0<=0.AND.Q1<=0) .OR. LEN_TRIM(ATYPE)==0)THEN
       ATYPE=''
       Q=Q1
       RETURN
    ENDIF

! Use the secant method for the first step:

    S0=0.d0
    S1=DSTEST
    DQ=Q0-Q1
    RDS=Q1/DQ*(S1-S0)
    RDS=(1.d0+HMACH)*RDS

! Return if tolerance has been met :

    RRDS=ABS(RDS)/(1+SQRT(ABS(DS*DSMAX)))
    IF(RRDS.LT.EPSS) THEN
       Q=0.d0
       IF(IID>0)WRITE(9,102)RDS
       RETURN
    ENDIF

    ALLOCATE(UOLDPSS(NDIM,0:NTST*NCOL),RLOLDS(NFPR))
    ALLOCATE(UDOTPSS(NDIM,0:NTST*NCOL),RLDOTS(NFPR))
    ALLOCATE(UPSS(NDIM,0:NTST*NCOL),RLCURS(NFPR))
    ALLOCATE(P0S(NDIM,NDIM),P1S(NDIM,NDIM),EVS(AP%NDM))
    ! save state to restore in case of non-convergence or
    ! "possible special point"
    UPSS(:,:)=UPS(:,:)
    UDOTPSS(:,:)=UDOTPS(:,:)
    UOLDPSS(:,:)=UOLDPS(:,:)
    RLCURS(:)=RLCUR(:)
    RLDOTS(:)=RLDOT(:)
    RLOLDS(:)=RLOLD(:)
    P0S(:,:)=P0(:,:)
    P1S(:,:)=P1(:,:)
    EVS(:)=EV(:)
    DETS=AP%DET
    FLDFS=AP%FLDF
    DSOLDS=DSOLD
    NITPSS=NITPS
    DSTESTS=DSTEST

    DO NITSP1=0,ITMX

! If requested write additional output on unit 9 :

       IF(IID.GE.2)THEN
          WRITE(9,101)NITSP1,RDS
       ENDIF

       CALL CONTBV(AP,DSOLD,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
       CALL STEPBV(AP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,FNCI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)
       IF(ISTOP.NE.0)EXIT

! Check for zero.

       Q=FNCS(AP,ICP,UPS,PAR,ATYPE,IUZ,VUZ,ITEST,FNCI)
       ! ignore stability changes
       I=LEN_TRIM(ATYPE)
       IF(ATYPE(I:I)=='0')ATYPE=ATYPE(1:I-1)

!        Use Mueller's method with bracketing for subsequent steps
       DSTEST=S1+RDS
       CALL MUELLER(Q0,Q1,Q,S0,S1,DSTEST,RDS)

       RDS=(1.d0+HMACH)*RDS

! Return if tolerance has been met :

       RRDS=ABS(RDS)/(1+SQRT(ABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS) THEN
          Q=0.d0
          IF(IID>0)WRITE(9,102)RDS
          DEALLOCATE(UPSS,RLCURS,UOLDPSS,RLOLDS,UDOTPSS,RLDOTS,P0S,P1S)
          STEPPED=.TRUE.
          RETURN
       ENDIF

    ENDDO

    IF(IID>0)WRITE(9,103)IBR,NTOP+1
    ATYPE=''
    ! set back to previous (converged) state
    UPS(:,:)=UPSS(:,:)
    UDOTPS(:,:)=UDOTPSS(:,:)
    UOLDPS(:,:)=UOLDPSS(:,:)
    RLCUR(:)=RLCURS(:)
    DO I=1,NFPR
       PAR(ICP(I))=RLCUR(I)
    ENDDO
    RLDOT(:)=RLDOTS(:)
    RLOLD(:)=RLOLDS(:)
    P0(:,:)=P0S(:,:)
    P1(:,:)=P1S(:,:)
    EV(:)=EVS(:)
    AP%DET=DETS
    AP%FLDF=FLDFS
    DSOLD=DSOLDS
    DSTEST=DSTESTS
    NITPS=NITPSS
    CALL PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)
    Q=FNCS(AP,ICP,UPS,PAR,ATYPEDUM,IUZ,VUZ,ITEST,FNCI)
    DEALLOCATE(UPSS,RLCURS,UOLDPSS,RLOLDS,UDOTPSS,RLDOTS,P0S,P1S)

101 FORMAT(' ==> Location of special point :  Iteration ',I3, &
         '  Step size = ',ES13.5)
102 FORMAT(' ==> Location of special point : ', &
         ' Convergence.   Step size = ',ES13.5)
103 FORMAT(I4,I6,' NOTE:Possible special point')

  END SUBROUTINE LCSPBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Boundary Value Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE STPLBV(AP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THU,ISTOP)

    USE IO
    USE MESH

! Writes the bifurcation diagram on unit 7 (Differential Equations)
! (Also controls the writing of complete solutions on unit 8).
! Every line written contains, in order, the following:
!
!  IBR    : The label of the branch.
!  NTOT   : The index of the point on the branch.
!           (Points are numbered consecutively along a branch).
!           If IPS=2 or 3, then the sign of NTOT indicates stability :
!            - = stable , + = unstable, or unknown.
!  ITP    : An integer indicating the type of point :
!
!             4  (  )  :   Output point (Every NPR steps along branch).
!            -4  (UZ)  :   Output point (Zero of user function).
!             5  (LP)  :   Fold (fold).
!             6  (BP)  :   Branch point.
!             7  (PD)  :   Period doubling bifurcation.
!             8  (TR)  :   Bifurcation to an invariant torus.
!             9  (EP)  :   End point of branch, normal termination.
!            -9  (MX)  :   End point of branch, abnormal termination.
!
!  LAB        : The label of a special point.
!  PAR(ICP(1)): The principal parameter.
!  A          : The L2-norm of the solution vector, or other measure of
!               the solution (see the user-supplied parameter IPLT).
!  MAX U(*)   : The maxima of the first few solution components.
!  PAR(ICP(*)): Further free parameters (if any).

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER ICP(*),ICU(*),NDIM,ISTOP
    DOUBLE PRECISION PAR(*),TM(*),DTM(*),UPS(*),THU(*)
    DOUBLE PRECISION RLDOT(AP%NFPR),UDOTPS(*)
! Local
    DOUBLE PRECISION UMX(7)
    INTEGER IPS,NTST,NCOL,ISW,IPLT,NMX,NPR,NDM,ITP,ITPST,IBR,I,IAB,IBRS,ITMP
    INTEGER LABW,N2,NINS,NTOT,NTOTS
    DOUBLE PRECISION RL0,RL1,A0,A1,AMP

    IPS=AP%IPS
    NTST=AP%NTST
    NCOL=AP%NCOL
    ISW=AP%ISW
    IPLT=AP%IPLT
    NMX=AP%NMX
    NPR=AP%NPR
    NDM=AP%NDM
    ITP=AP%ITP
    ITPST=AP%ITPST
    IBR=AP%IBR

    RL0=AP%RL0
    RL1=AP%RL1
    A0=AP%A0
    A1=AP%A1

    NTOT=AP%NTOT
    NTOT=NTOT+1
    AP%NTOT=NTOT

! ITP is set to 4 every NPR steps along a branch of solns and the entire
! solution is written on unit 8.

    IF(NPR.NE.0)THEN
       IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
       AP%ITP=ITP
    ENDIF

! Check whether limits of the bifurcation diagram have been reached :

    IAB=ABS(IPLT)
    IF(IAB.EQ.0.OR.IAB.GT.3*NDM) &
         AMP=SQRT(RNRMSQ(NTST,NCOL,NDIM,NDM,UPS,DTM,THU))
    IF(IPLT.GT.0.AND.IAB.LE.NDM)AMP=RMXUPS(NTST,NCOL,NDIM,IAB,UPS)
    IF(IPLT.GT.NDM.AND.IAB.LE.2*NDM) &
         AMP=RINTG(NTST,NCOL,NDIM,IAB-NDM,UPS,DTM)
    IF(IPLT.GT.2*NDM.AND.IAB.LE.3*NDM) &
         AMP=RNRM2(NTST,NCOL,NDIM,IAB-2*NDM,UPS,DTM)
    IF(IPLT.LT.0.AND.IAB.LE.NDM)AMP=RMNUPS(NTST,NCOL,NDIM,IAB,UPS)

    IF(ISTOP.EQ.1)THEN
!      ** Maximum number of iterations reached somewhere.
       ITP=-9-10*ITPST
       AP%ITP=ITP
    ELSE
       IF(PAR(ICP(1)).LT.RL0.OR.PAR(ICP(1)).GT.RL1 &
            .OR. AMP.LT.A0.OR.AMP.GT.A1 .OR. NTOT.EQ.NMX)THEN
          ISTOP=1
          ITP=9+10*ITPST
          AP%ITP=ITP
       ENDIF
    ENDIF

! All special points receive label:

    LABW=0
    IF(MOD(ITP,10).NE.0) THEN
       LABW=AP%LAB
    ENDIF

! Compute maxima of solution components.

    N2=NDM
    IF(N2.GT.7)N2=7
    DO I=1,N2
       ITMP=I
       UMX(I)=RMXUPS(NTST,NCOL,NDIM,ITMP,UPS)
    ENDDO

! Branch number is negative for periodic solutions

    IF(IPS.EQ.2.OR.IPS.EQ.12)THEN
       IBRS=-IBR
    ELSE
       IBRS=IBR
    ENDIF

! Determine stability, and write output on units 7 and 8.

    NTOTS=NTOT
    IF(ABS(ISW).LE.1 .AND. (IPS.EQ.2.OR.IPS.EQ.7.OR.IPS.EQ.12))THEN
       NINS=AP%NINS
       IF(NINS.EQ.NDIM)NTOTS=-NTOT
    ENDIF
    CALL WRLINE(AP,PAR,ICU,IBRS,NTOTS,LABW,AMP,UMX)

! Write plotting and restart data on unit 8.

    IF(MOD(ITP,10).NE.0)THEN
       CALL WRTBV8(AP,PAR,ICP,RLDOT,NDIM,UPS,UDOTPS,TM,DTM)
       AP%LAB=AP%LAB+1
    ENDIF

  END SUBROUTINE STPLBV

! ---------- ------
  SUBROUTINE WRTBV8(AP,PAR,ICP,RLDOT,NDIM,UPS,UDOTPS,TM,DTM)

    USE COMPAT
    USE SUPPORT, ONLY: DIRECTION
    USE AUTO_CONSTANTS, ONLY: IPS, NDIMU => NDIM

! Writes plotting and restart data on unit 8, viz.:
! (1) data identifying the corresponding point on unit 7,
! (2) the complete solution,
! (3) the direction of the branch.
!
! Specifically the following is written:
!
!  IBR   : The index of the branch.
!  NTOT  : The index of the point.
!  ITP   : The type of point (see STPLBV above).
!  LAB   : The label of the point.
!  NFPR : The number of free parameters used in the computation.
!  ISW   : The value of ISW used in the computation.
!  NTPL  : The number of points in the time interval [0,1] for which
!          solution values are written.
!  NAR   : The number of values written per point.
!          (NAR=NDIM+1, since T and U(i), i=1,..,NDIM are written).
!  NROWPR: The number of lines printed following the identifying line
!          and before the next data set or the end of the file.
!          (Used for quickly skipping a data set when searching).
!  NTST  : The number of time intervals used in the discretization.
!  NCOL  : The number of collocation points used.
!  NPAR  : The dimension of the array PAR.
!  NPARI : Number of internal parameters, at the end of the array PAR.
!  NDIMU : The user-specified dimension.
!  IPS   : The problem type. 
!  IPRIV : Private field for use by toolboxes.
!
!  Following the above described identifying line there are NTPL lines
! containing :
!     T , U-1(T) , U-2(T) , ... , U-NDIM(T),
! where NDIM is the dimension of the system of differential equations.
!
! Following this is a line containing the indices of the free parameters
!    ICP(I),I=1,NFPR,
!
! followed by a line containing the values
!    RL-dot(i) , i=1,NFPR,
!
! and following this are NTPL lines each containing
!    U-dot-1(T), U-dot-2(T), ... , U-dot-NDIM(T).
!
! Finally the parameter values PAR(i) , i=1,NPAR, are written.
!
!  Above, RL-dot(.) and U-dot(.) specify the direction of the branch.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER ICP(*),NDIM
    DOUBLE PRECISION UPS(NDIM,0:*),UDOTPS(NDIM,0:*),TM(0:*),DTM(*)
    DOUBLE PRECISION PAR(*),RLDOT(AP%NFPR)

    INTEGER NTST,NCOL,ISW,ITP,NFPR,IBR,NPAR,NTOT,LAB,NTPL,NAR,NROWPR
    INTEGER MTOT,I,J,NPARI
    DOUBLE PRECISION T
    LOGICAL DIR
!xxx====================================================================
!xxx Test problem: compute the error
!    err(x,t)=x - 2*DATAN(1.d0)*PAR(2)*DSIN(4*DATAN(1.d0)*t)
!xxx====================================================================

    NTST=AP%NTST
    NCOL=AP%NCOL
    ISW=AP%ISW
    NPARI=AP%NPARI
    ITP=AP%ITP
    NFPR=AP%NFPR
    IBR=AP%IBR
    NPAR=AP%NPAR
    NTOT=AP%NTOT
    LAB=AP%LAB

! Write information identifying the solution :
! skip direction info based on IIS and ITP
    DIR=DIRECTION(AP%IIS,ITP)

    NTPL=NCOL*NTST+1
    NAR=NDIM+1
    NROWPR=(NDIM/7+1)*NTPL + (NPAR+6)/7
    IF(DIR)THEN
       NROWPR=NROWPR + (NFPR+19)/20 + (NFPR+6)/7 + ((NDIM+6)/7)*NTPL
    ENDIF
    MTOT=MOD(NTOT-1,9999)+1
    WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR, &
         NPARI,NDIMU,IPS,0

! Write the entire solution on unit 8 :

!xxx====================================================================
!xxx Test problem
    !xxx eg=0.d0
    !xxx em=0.d0
!xxx====================================================================
    DO J=0,NTST*NCOL-1
       T=TM(J/NCOL)+MOD(J,NCOL)*DTM(J/NCOL+1)/NCOL
       WRITE(8,102)T,UPS(:,J)
!xxx====================================================================
!xxx Test problem
       !xxx er = err(ups(1,j),T)
       !xxx if(dabs(er).gt.eg)eg=dabs(er)
       !xxx if(i.eq.1 .and. dabs(er).gt.em)em=dabs(er)
!xxx====================================================================
    ENDDO
    WRITE(8,102)1.0d0,UPS(:,NTST*NCOL)
!xxx====================================================================
!xxx Test problem
! Write global error and mesh error
!xxx       write(10,100)ncol,ntst,eg,em
!xxx 100   FORMAT(4X,I2,I4,7ES11.3)
!xxx====================================================================

    IF(DIR)THEN
! Write the free parameter indices:

       WRITE(8,103)(ICP(I),I=1,NFPR)

! Write the direction of the branch:

       WRITE(8,102)(RLDOT(I),I=1,NFPR)
       DO J=0,NTST*NCOL
          WRITE(8,102)UDOTPS(:,J)
       ENDDO
    ENDIF

! Write the parameter values.

    WRITE(8,102)(PAR(I),I=1,NPAR)

101 FORMAT(6I6,I8,I6,I8,7I5)
102 FORMAT(4X,7ES19.10)
103 FORMAT(20I5)

    CALL AUTOFLUSH(8)
  END SUBROUTINE WRTBV8

! ---------- ------
  SUBROUTINE WRTBV9(AP,RLCUR,NDIM,UPS,TM,DTM,THU,NITPS)

    USE IO
    USE MESH

! Writes additional output on unit 9.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,NITPS
    DOUBLE PRECISION, INTENT(IN) :: RLCUR(AP%NFPR),DTM(*),UPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(IN) :: TM(0:*),THU(*)

    INTEGER IAB,NTST,NCOL,IPLT,IID,NDM,IBR,NTOT,J,MTOT
    DOUBLE PRECISION T,AMP

    NTST=AP%NTST
    NCOL=AP%NCOL
    IPLT=AP%IPLT
    IID=AP%IID
    NDM=AP%NDM
    IBR=AP%IBR
    NTOT=AP%NTOT

    IAB=ABS(IPLT)
    IF(IAB.EQ.0.OR.IAB.GT.NDIM)AMP=SQRT(RNRMSQ(NTST,NCOL,NDIM,NDM,UPS,DTM,THU))
    IF(IPLT.GT.0.AND.IAB.LE.NDIM)AMP=RMXUPS(NTST,NCOL,NDIM,IAB,UPS)
    IF(IPLT.LT.0.AND.IAB.LE.NDIM)AMP=RMNUPS(NTST,NCOL,NDIM,IAB,UPS)
    IF(IID.GE.2)THEN
       IF(NITPS.EQ.0)CALL WRBAR("=",47)
       IF(NITPS.EQ.0 .OR. IID.GE.3)THEN
          WRITE(9,102)
       ENDIF
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(9,103)IBR,MTOT+1,NITPS,RLCUR(1),AMP
    ENDIF

    IF(IID.GE.5)THEN
       WRITE(9,104)
       DO J=0,NTST*NCOL-1
          T=TM(J/NCOL)+MOD(J,NCOL)*DTM(J/NCOL+1)/NCOL
          WRITE(9,105)T,UPS(:,J)
       ENDDO
       WRITE(9,105)1.0d0,UPS(:,NTST*NCOL)
    ENDIF
102 FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
103 FORMAT(I4,I6,I4,5X,6ES14.5)
104 FORMAT(' UPS :')
105 FORMAT(1X,7ES14.5)

  END SUBROUTINE WRTBV9

END MODULE BVP
