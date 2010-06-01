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
  PUBLIC :: AUTOBV,STPNUB,STPNBV,STPNBV1,PVLSBV,SETRTN,IRTN,NRTN

  INTEGER, ALLOCATABLE :: NRTN(:)
  INTEGER IRTN

CONTAINS

! ---------- ------
  SUBROUTINE AUTOBV(AP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNBVI, &
       PVLI,THL,THU,IUZ,VUZ)

    USE AUTOMPI

! THIS IS THE ENTRY ROUTINE FOR GENERAL BOUNDARY VALUE PROBLEMS.

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),ICU(*),IUZ(*)
    DOUBLE PRECISION PAR(*),THL(*),THU(*),VUZ(*)

    include 'interfaces.h'

    IF(MPIIAM()>0)THEN
!        This is a little trick to tell MPI workers what FUNI and ICNI
!        are.
       DO WHILE(MPIWFI(.TRUE.))
          CALL mpi_setubv_worker(FUNI,ICNI,BCNI)
       ENDDO
       RETURN
    ENDIF
    CALL CNRLBV(AP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNBVI, &
         PVLI,THL,THU,IUZ,VUZ)

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

    double precision, allocatable :: ups(:,:), uoldps(:,:)
    double precision, allocatable :: udotps(:,:), upoldp(:,:), thu(:)
    double precision, allocatable :: dtm(:),par(:)
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

    allocate(icp(nfpr+nint),thu(ndim*8),dtm(na),par(npar))
    allocate(ups(ndim,0:na*ncol),uoldps(ndim,0:na*ncol))
    allocate(udotps(ndim,0:na*ncol),upoldp(ndim,0:na*ncol))

    call mpisbv(ap,par,icp,ndim,ups,uoldps,udotps,upoldp, &
         dtm,thu,ifst,nllv)
    dum=0
    call solvbv(ifst,ap,det,par,icp,funi,bcni,icni,dum, &
         nllv,dum1,dum1,dum1,ndim,ups,uoldps,udotps,upoldp,dtm, &
         dum1,dum1,dum1,dum1,dum1,thu)

    ! free input arrays
    deallocate(ups,uoldps,dtm,udotps,upoldp,thu,icp,par)

  end subroutine mpi_setubv_worker

! ---------- ------
  SUBROUTINE CNRLBV(AP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNBVI, &
       PVLI,THL,THU,IUZ,VUZ)

    USE IO
    USE MESH
    USE SUPPORT, ONLY: DTM=>DTV, P0=>P0V, P1=>P1V, EV=>EVV, CHECKSP, STOPPED, &
         INITSTOPCNTS

! Controls the computation of solution branches.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),ICU(*),IUZ(*)
    DOUBLE PRECISION PAR(*),VUZ(*),THL(*),THU(*)
! Local
    DOUBLE PRECISION, ALLOCATABLE :: RLCUR(:),RLOLD(:),RLDOT(:)
    DOUBLE PRECISION, ALLOCATABLE :: UPS(:,:),UOLDPS(:,:),UPOLDP(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: UDOTPS(:,:),TM(:),TEST(:)
    INTEGER NDIM,IPS,IRS,ILP,NTST,NCOL,IAD,IADS,ISP,ISW,NUZR,ITP,ITPST,NFPR
    INTEGER IBR,IPERP,ISTOP,ITNW,ITEST,JTEST,I,NITPS,NODIR,NTOP,NTOT,NPAR,NINS
    INTEGER ITPDUM
    INTEGER STOPCNTS(-9:13)
    DOUBLE PRECISION DS,DSMAX,DSOLD,RDS,SP1

! INITIALIZE COMPUTATION OF BRANCH

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

    ALLOCATE(RLCUR(NFPR),RLDOT(NFPR),RLOLD(NFPR))
    ALLOCATE(UPS(NDIM,0:NTST*NCOL),UOLDPS(NDIM,0:NTST*NCOL))
    ALLOCATE(UPOLDP(NDIM,0:NTST*NCOL),UDOTPS(NDIM,0:NTST*NCOL))
    ALLOCATE(TM(0:NTST),DTM(NTST))
    ALLOCATE(P0(NDIM,NDIM),P1(NDIM,NDIM),TEST(NUZR+5),EV(NDIM))

    DS=AP%DS

    RDS=DS
    DSOLD=RDS
    CALL INITSTOPCNTS(ISP,ILP,ITPST,STOPCNTS)
    IF(ISP.LT.0)THEN
       ISP=-ISP
       AP%ISP=ISP
    ENDIF
    DO I=1,NUZR+5
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
    CALL RSPTBV(AP,PAR,ICP,FUNI,STPNBVI,PVLI,RLCUR,RLOLD,RLDOT, &
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
          SP1 = FNSPBV(AP,PAR,ITPDUM,P0,P1,EV)
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
    CALL PVLI(AP,ICP,UPS,NDIM,PAR)
    CALL STPLBV(AP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THU,ISTOP)
    DO WHILE(ISTOP==0)
       ITP=0
       AP%ITP=ITP
       NINS=AP%NINS
       CALL STEPBV(AP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)

       DO ITEST=1,NUZR+5
          IF(ISTOP.NE.0)EXIT
          CALL LCSPBV(AP,DSOLD,PAR,ICP,ITEST,FUNI,BCNI,ICNI,PVLI, &
               TEST(ITEST),RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS, &
               UPOLDP,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP,ITP)
          IF(ITP/=0)THEN
             AP%ITP=ITP
             IF(STOPPED(IUZ,ITEST,NUZR,ITP,STOPCNTS))THEN
                ISTOP=-1 ! *Stop at the first found bifurcation
                DO JTEST=ITEST+1,NUZR+5
                   ! just evaluate test functions to get stability info etc.
                   TEST(JTEST)=FNCS(AP,PAR,ITPDUM,P0,P1,EV,IUZ,VUZ,JTEST)
                ENDDO
             ELSEIF(MOD(ITP,10)==-4)THEN
                TEST(1:NUZR)=0.d0
             ELSE
                TEST(NUZR+1:NUZR+5)=0.d0
             ENDIF
          ENDIF
       ENDDO

       ITP=AP%ITP
       IF(ITP/=0.AND.MOD(ITP,10)/=-4)THEN
          ! for plotter: use stability of previous point
          ! for bifurcation points
          AP%NINS=NINS
       ENDIF

! Store plotting data.

       CALL PVLI(AP,ICP,UPS,NDIM,PAR)
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
    DEALLOCATE(EV,UPS,UOLDPS,UPOLDP,UDOTPS,TM,DTM,P0,P1)
    DEALLOCATE(TEST,RLCUR,RLOLD,RLDOT)

  END SUBROUTINE CNRLBV

! ---------- ------
  SUBROUTINE CONTBV(AP,DSOLD,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)

    USE MESH

! Computes new rate of change (UDOTPS,RLDOT) and time derivative (UPOLDP)
! arrays depending on (UOLDPS,RLDOT) and (UPS,RLCUR), and then replaces
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
    DOUBLE PRECISION, ALLOCATABLE :: DFDU(:,:),DFDP(:,:)

    NTST=AP%NTST
    NCOL=AP%NCOL
    NPAR=AP%NPAR

    ALLOCATE(DFDU(NDIM,NDIM),DFDP(NDIM,NPAR))
    DFDU(:,:)=0.d0
    DFDP(:,:)=0.d0

    DO J=0,NTST*NCOL
       CALL FUNI(AP,NDIM,UPS(1,J),UPS(1,J),ICP,PAR,0,UPOLDP(1,J),&
            DFDU,DFDP)
    ENDDO

    DEALLOCATE(DFDU,DFDP)
  END SUBROUTINE STUPBV

! ---------- ------
  SUBROUTINE STEPBV(AP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
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

       CALL NEWTONBV(AP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
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
  SUBROUTINE NEWTONBV(AP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,THL,THU,NITPS,CONVERGED)

! This subroutine contains the main predictor-corrector loop

    USE MESH, ONLY: SCALEB
    USE SOLVEBV, ONLY: SOLVBV
    USE SUPPORT, ONLY: CHECKSP

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

          CALL PVLI(AP,ICP,UPS,NDIM,PAR)
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
  SUBROUTINE RSPTBV(AP,PAR,ICP,FUNI,STPNBVI,PVLI,RLCUR,RLOLD, &
       RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THU)

    USE IO
    USE MESH

! Restarts computation of a branch of solutions at point labelled IRS.
! The output written on unit 8 by a previous run is now expected as
! input on unit 3. The label IRS, where computation is to resume, must
! be specified in the user-supplied subroutine INIT.
! If IRS=0 then the starting point must be provided analytically in the
! user-supplied subroutine STPNT.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),NDIM,NODIR
    DOUBLE PRECISION UPS(NDIM,0:*),UOLDPS(NDIM,0:*),UPOLDP(NDIM,0:*)
    DOUBLE PRECISION UDOTPS(NDIM,0:*),TM(0:*),DTM(*),PAR(*)
    DOUBLE PRECISION RLCUR(AP%NFPR),RLOLD(AP%NFPR),RLDOT(AP%NFPR)
    DOUBLE PRECISION THU(*)

    DOUBLE PRECISION, ALLOCATABLE :: U(:),UDOT(:)
    INTEGER :: ICPRS(2),IRS,NTST,NCOL,ITP,NFPR,ISW,NCOLRS,NTSRS,I,J

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
          ! call PVLS here the first time so the parameters can be initialized
          CALL PVLI(AP,ICP,UPS,NDIM,PAR)
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
       CALL PVLI(AP,ICP,UPS,NDIM,PAR)
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
       ISW=1
       AP%ISW=ISW
    ELSE
!      ** Restart from orbit.
       CALL STUPBV(AP,PAR,ICP,FUNI,NDIM,UPS,UPOLDP)
    ENDIF

  END SUBROUTINE RSPTBV

! ---------- ------
  SUBROUTINE STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE MESH

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR),TM(0:*)
    DOUBLE PRECISION, INTENT(OUT) :: UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*)

    INTEGER NDIM,IPS,ISW,NTST,NCOL,NDIMRD,NTSRS
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    NDIM=AP%NDIM
    IPS=AP%IPS
    ISW=AP%ISW
    NTST=AP%NTST
    NCOL=AP%NCOL

    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),UDOTPSR(NDIM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL STPNBV1(AP,PAR,ICP,NDIM,NTSRS,NDIMRD,NCOLRS,RLDOT, &
         UPSR,UDOTPSR,TMR,NODIR)
    CALL ADAPT2(NTSR,NCOLRS,NDIM,NTST,NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,(IPS==2.OR.IPS==12) .AND. ABS(ISW)<=1)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNBV

! ---------- -------
  SUBROUTINE STPNBV1(AP,PAR,ICP,NDIM,NTSRS,NDIMRD,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE IO

! This subroutine locates and retrieves the information required to
! restart computation at the point with label IRS.
! This information is expected on unit 3.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    INTEGER, INTENT(OUT) :: NTSRS,NCOLRS,NDIMRD,NODIR
    DOUBLE PRECISION, INTENT(OUT) :: UPS(*),UDOTPS(*),TM(*)
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR)
! Local
    INTEGER NFPR,NFPRS,ITPRS,I
    INTEGER, ALLOCATABLE :: ICPRS(:)

    NFPR=AP%NFPR

    ALLOCATE(ICPRS(NFPR))
    CALL READBV(AP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOT,UPS, &
         UDOTPS,TM,ITPRS,NDIM)

! Take care of the case where the free parameters have been changed at
! the restart point.

    NODIR=0
    NFPRS=GETNFPR3()
    IF(NFPRS.NE.NFPR)THEN
       NODIR=1
    ELSE
       DO I=1,NFPR
          IF(ICPRS(I).NE.ICP(I)) THEN
             NODIR=1
             EXIT
          ENDIF
       ENDDO
    ENDIF
    DEALLOCATE(ICPRS)

  END SUBROUTINE STPNBV1

! ---------- ------
  SUBROUTINE STPNUB(AP,PAR,ICP,NTSRS,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE MESH
    USE AUTO_CONSTANTS, ONLY : DATFILE, PARVALS, parnames
    USE IO, ONLY: NAMEIDX

! Generates a starting point for the continuation of a branch of
! of solutions to general boundary value problems by calling the user
! supplied subroutine STPNT where an analytical solution is given.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSRS,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR)
    DOUBLE PRECISION, INTENT(OUT) :: TM(0:*),UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*)

    INTEGER NDIM,IPS,NTST,NCOL,ISW,IBR,LAB,NTSR,ios,I,J
    DOUBLE PRECISION TEMP,PERIOD
    DOUBLE PRECISION, ALLOCATABLE :: TMR(:),UPSR(:,:),UDOTPSR(:,:),U(:)

    NDIM=AP%NDIM
    IPS=AP%IPS
    NTST=AP%NTST
    NCOL=AP%NCOL
    ISW=AP%ISW

! Generate the (initially uniform) mesh.

    CALL MSH(NTST,TM)

    IF(DATFILE/='')THEN
       OPEN(3,FILE=TRIM(DATFILE),STATUS='old',ACCESS='sequential',&
            IOSTAT=ios)
       IF(ios/=0)THEN
          OPEN(3,FILE=TRIM(DATFILE)//'.dat',STATUS='old',&
               ACCESS='sequential',IOSTAT=ios)
       ENDIF
       IF(ios/=0)THEN
          WRITE(6,"(A,A,A)")'Datafile ',TRIM(DATFILE),' not found.'
          STOP
       ENDIF
       NTSR=-1
       DO
          READ(3,*,END=2)TEMP,(TEMP,I=1,NDIM)
          NTSR=NTSR+1
       ENDDO
2      CONTINUE
       ALLOCATE(TMR(0:NTSR),UPSR(NDIM,0:NTSR),UDOTPSR(NDIM,0:NTSR),U(NDIM))
       REWIND 3
       DO J=0,NTSR
          READ(3,*)TMR(J),UPSR(:,J)
       ENDDO
       CLOSE(3)
       UDOTPSR(:,:)=0.d0
       PERIOD=TMR(NTSR)-TMR(0)
       DO I=NTSR,0,-1
          TMR(I)=(TMR(I)-TMR(0))/PERIOD
       ENDDO
       CALL ADAPT2(NTSR,1,NDIM,NTST,NCOL,NDIM, &
            TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,(IPS==2.OR.IPS==12).AND.ABS(ISW)<=1)
       IF(AP%NPAR>10)THEN
          PAR(11)=PERIOD
       ENDIF
       CALL STPNT(NDIM,U,PAR,0d0)
       RLDOT(:)=0.d0
       DEALLOCATE(TMR,UPSR,UDOTPSR,U)
    ELSE
       DO J=0,NTST*NCOL
          UPS(:,J)=0.d0
          CALL STPNT(NDIM,UPS(1,J),PAR,DBLE(J)/(NTST*NCOL))
       ENDDO
    ENDIF

! override parameter values with values from constants file

    DO I=1,SIZE(PARVALS)
       PAR(NAMEIDX(PARVALS(I)%INDEX,parnames))=PARVALS(I)%VAR
    ENDDO

    IBR=1
    AP%IBR=IBR
    LAB=0
    AP%LAB=LAB

    NODIR=1

  END SUBROUTINE STPNUB

! ---------- ------
  SUBROUTINE STHOPF(AP,U,PAR,ICP,NTST,NCOL, &
       NFPR,RLDOT,NDIM,UDOTPS,UPOLDP,NODIR,THU,FUNI)

    USE IO
    USE MESH
    USE SUPPORT

!  Generates starting data for a periodic orbit from a Hopf
!  bifurcation point (for waves or periodic orbits)

    TYPE(AUTOPARAMETERS) AP
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

    RLDOT(1:2)=0.d0
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

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),NDIM,IPERP
    DOUBLE PRECISION UDOTPS(NDIM,0:AP%NTST*AP%NCOL),DTM(*)
    DOUBLE PRECISION PAR(*),RLCUR(AP%NFPR),RLOLD(AP%NFPR),RLDOT(AP%NFPR)
    DOUBLE PRECISION THL(*),THU(*),UPS(*),UOLDPS(*),UPOLDP(*),P0(*),P1(*)

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

! Make sure that RLDOT(1) is positive (unless zero).

    IF(RLDOT(1).LT.0.d0)THEN
       RLDOT(:)=-RLDOT(:)
       UDOTPS(:,:)=-UDOTPS(:,:)
    ENDIF

    IF(IID.GE.2)THEN
       WRITE(9,101)
       DO I=1,NFPR 
          WRITE(9,102)ICP(I),RLDOT(I)
       ENDDO
    ENDIF

101 FORMAT(/,' Starting direction of the free parameter(s) : ')
102 FORMAT(' PAR(',I3,') :',E11.3)

  END SUBROUTINE STDRBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Detection and Location of Branch Points in Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE LCSPBV(AP,DSOLD,PAR,ICP,ITEST,FUNI,BCNI,ICNI,PVLI,Q, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP,ITP)

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
    COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: EV(*)
    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ITEST,IUZ(*),NDIM
    INTEGER, INTENT(OUT) :: NITPS,ISTOP,ITP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*), UPS(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(INOUT) :: UOLDPS(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(INOUT) :: UDOTPS(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(INOUT) :: UPOLDP(NDIM,0:AP%NCOL*AP%NTST)
    DOUBLE PRECISION, INTENT(IN) :: TM(*),DTM(*),THL(*),THU(*),VUZ(*)
    DOUBLE PRECISION, INTENT(INOUT) :: DSOLD,Q
    DOUBLE PRECISION, INTENT(INOUT) :: RLCUR(AP%NFPR),RLOLD(AP%NFPR)
    DOUBLE PRECISION, INTENT(INOUT) :: RLDOT(AP%NFPR),P0(*),P1(*)

    INTEGER IID,ITMX,IBR,NTOT,NTOP,NITSP1,ITPDUM
    DOUBLE PRECISION DS,DSMAX,EPSS,Q0,Q1,DQ,RDS,RRDS,S,S0,S1

    IID=AP%IID
    ITMX=AP%ITMX
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    DS=AP%DS
    DSMAX=AP%DSMAX
    EPSS=AP%EPSS

! Check for zero.

    Q0=Q
    Q1=FNCS(AP,PAR,ITP,P0,P1,EV,IUZ,VUZ,ITEST)

    ! do not test via Q0*Q1 to avoid overflow.
    IF((Q0>=0.AND.Q1>=0) .OR. (Q0<=0.AND.Q1<=0) .OR. ITP==0)THEN
       ITP=0
       Q=Q1
       RETURN
    ENDIF

! Use the secant method for the first step:

    S0=0.d0
    S1=DSOLD
    DQ=Q0-Q1
    RDS=Q1/DQ*(S1-S0)
    DO NITSP1=0,ITMX
       RDS=(1.d0+HMACH)*RDS
       S=S1+RDS

! Return if tolerance has been met :

       RRDS=ABS(RDS)/(1+SQRT(ABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS) THEN
!xx???   Q=0.d0
          IF(IID>0)WRITE(9,102)RDS
          RETURN
       ENDIF

! If requested write additional output on unit 9 :

       IF(IID.GE.2)THEN
          WRITE(9,101)NITSP1,RDS
       ENDIF

       CALL CONTBV(AP,DSOLD,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
       CALL STEPBV(AP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)
       IF(ISTOP.NE.0)THEN
          ITP=0
          Q=0.d0
          RETURN
       ENDIF

! Check for zero.

       Q=FNCS(AP,PAR,ITPDUM,P0,P1,EV,IUZ,VUZ,ITEST)

!        Use Mueller's method with bracketing for subsequent steps
       CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
    ENDDO

    IF(IID>0)WRITE(9,103)IBR,NTOP+1
    ITP=0
    Q=0.d0
101 FORMAT(' ==> Location of special point :  Iteration ',I3, &
         '  Step size = ',ES13.5)
102 FORMAT(' ==> Location of special point : ', &
         ' Convergence.   Step size = ',ES13.5)
103 FORMAT(I4,I6,' NOTE:Possible special point')

  END SUBROUTINE LCSPBV

! ------ --------- -------- ----
  DOUBLE PRECISION FUNCTION FNCS(AP,PAR,ITP,P0,P1,EV,IUZ,VUZ,ITEST)

    USE SUPPORT, ONLY: FNUZ

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: P0(*),P1(*)
    COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: EV(*)
    INTEGER, INTENT(IN) :: IUZ(*),ITEST
    DOUBLE PRECISION, INTENT(IN) :: VUZ(*)

    INTEGER NUZR

    NUZR=AP%NUZR

    FNCS=0.d0
    IF(ITEST==NUZR+2)THEN
       FNCS=FNLPBV(AP,ITP)
    ELSEIF(ITEST==NUZR+3)THEN
       FNCS=FNBPBV(AP,ITP,P1)
    ELSEIF(ITEST==NUZR+5)THEN
       FNCS=FNSPBV(AP,PAR,ITP,P0,P1,EV)
    ELSEIF(ITEST<=NUZR)THEN
       FNCS=FNUZ(AP,PAR,ITP,IUZ,VUZ,ITEST)
    ENDIF

  END FUNCTION FNCS
       
! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNLPBV(AP,ITP)

    USE MESH
    USE SOLVEBV
    USE SUPPORT, ONLY: CHECKSP

! RETURNS A QUANTITY THAT CHANGES SIGN AT A LIMIT POINT (BVP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: ITP

    INTEGER IID,IBR,NTOT,NTOP

    ITP=0
    FNLPBV=0d0
    IF(.NOT.CHECKSP('LP',AP%IPS,AP%ILP,AP%ISP))RETURN

    IID=AP%IID
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    FNLPBV=AP%FLDF
    IF(IID.GE.2)THEN
       WRITE(9,101)ABS(IBR),NTOP+1,FNLPBV
    ENDIF

! Set the quantity to be returned.

    ITP=5+10*AP%ITPST

101 FORMAT(I4,I6,9X,'Fold Function ',ES14.5)

  END FUNCTION FNLPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNBPBV(AP,ITP,P1)

    USE SUPPORT

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: P1(*)

! Local
    DOUBLE PRECISION, ALLOCATABLE :: PP(:)
    INTEGER IID,I,IBR,NTOP,NTOT,NDIM
    DOUBLE PRECISION U(1),F(1),DET

    ITP=0
    FNBPBV=0d0
    AP%BIFF=FNBPBV
    IF(.NOT.CHECKSP('BP',AP%IPS,AP%ILP,AP%ISP))RETURN

    NDIM=AP%NDIM
    IID=AP%IID

    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

! Compute the determinant of P1.

    ALLOCATE(PP(NDIM**2))
    DO I=1,NDIM**2
       PP(I)=P1(I)
    ENDDO
    CALL GEL(NDIM,PP,0,U,F,DET)
    DEALLOCATE(PP)

! AP%DET contains the determinant of the reduced system.
! Set the determinant of the normalized reduced system.

    IF(ABS(AP%DET)/HUGE(DET).LT.ABS(DET))THEN
       FNBPBV=AP%DET/DET
       ITP=6+10*AP%ITPST
    ELSE
       FNBPBV=0.d0
       ITP=0
    ENDIF
    AP%BIFF=FNBPBV

    IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1,FNBPBV
101 FORMAT(I4,I6,9X,'BP   Function ',ES14.5)

  END FUNCTION FNBPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNSPBV(AP,PAR,ITP,P0,P1,EV)

    USE FLOQUET
    USE SUPPORT, ONLY: PI, LBTYPE, CHECKSP

! This function returns a quantity that changes sign when a complex
! pair of eigenvalues of the linearized Poincare map moves in or out
! of the unit circle or when a real eigenvalues passes through -1.

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(OUT) :: ITP
    DOUBLE PRECISION, INTENT(IN) :: P0(*),P1(*)
    COMPLEX(KIND(1.0D0)), INTENT(INOUT) :: EV(*)

! Local
    COMPLEX(KIND(1.0D0)) ZTMP
    INTEGER ISP,ISW,IID,IBR,NTOT,NTOP,I,J,L,LOC,NINS,NINS1,NDIM
    DOUBLE PRECISION D,AMIN,AZM1,tol,V,THETA

    NDIM=AP%NDIM
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
    ITP=0

    IF(ISP==0 .OR. (AP%IPS/=2.AND.AP%IPS/=7.AND.AP%IPS/=12) )RETURN

    IF(IID.GE.4)THEN
       CALL EVECS(NDIM,P0,P1)
    ENDIF

!  Compute the Floquet multipliers
    CALL FLOWKM(NDIM, P0, P1, IID, EV)

! Find the multipliers closest to z=1.

    IF(ISW==2)THEN
       IF(AP%ITPST==5.OR.AP%ITPST==7)THEN
          L=4
       ELSE ! Torus/BP
          L=NDIM/AP%NDM
       ENDIF
    ELSE
       L=1
    ENDIF
    LOC=1
    V=1.d0
    DO I=1,L
       AMIN=HUGE(1.d0)
       LOC=I
       DO J=I,NDIM
          AZM1= ABS( EV(J) - V )
          IF(AZM1<=AMIN)THEN
             ! try to keep complex conjugates together
             IF(MOD(I,2)==0)THEN
                IF(AIMAG(EV(I-1))==0.AND.AIMAG(EV(J))/=0)CYCLE
                IF(AIMAG(EV(I-1))/=0.AND.AIMAG(EV(J))==0)CYCLE
             ENDIF
             AMIN=AZM1
             LOC=J
          ENDIF
       ENDDO
       IF(LOC.NE.I) THEN
          ZTMP=EV(LOC)
          EV(LOC)=EV(I)
          EV(I)=ZTMP
       ENDIF
       ! For PD: Find the multipliers closest to z=-1.
       IF(I*2==L.AND.AP%ITPST==7)THEN
          V=-1.d0
       ENDIF
    ENDDO

! Order the remaining Floquet multipliers by distance from |z|=1.

    IF(NDIM.GE.3)THEN
       DO I=L+1,NDIM-1
          AMIN=HUGE(1.d0)
          DO J=I,NDIM
             AZM1= ABS(EV(J)) - 1.d0 
             AZM1=ABS(AZM1)
             IF(AZM1.LE.AMIN)THEN
                AMIN=AZM1
                LOC=J
             ENDIF
          ENDDO
          IF(LOC.NE.I) THEN
             ZTMP=EV(LOC)
             EV(LOC)=EV(I)
             EV(I)=ZTMP
          ENDIF
       ENDDO
    ENDIF

    ITP=TPSPBV(NDIM,AP%EPSS,AP%ITPST,PAR,AP%NPAR,EV,THETA)
    IF(.NOT.CHECKSP(LBTYPE(ITP),AP%IPS,AP%ILP,ISP)) ITP=0
    IF(MOD(ITP,10)==8) PAR(12)=THETA !try to find TR bif

! Print error message if the Floquet multiplier at z=1 is inaccurate.
! (ISP is set to negative and detection of bifurations is discontinued)

    AMIN= ABS( EV(1) - 1.d0 )
    IF(AMIN>5.0D-2 .AND. ITP/=0) THEN
       NINS=0
       AP%NINS=NINS
       ISP=-ISP
       AP%ISP=ISP
       IF(IID>0)THEN
          IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1
          DO I=1,NDIM
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
             DO I=1,NDIM
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
    IF(NDIM>1) THEN
       DO I=2,NDIM
          IF( ABS(EV(I)).LE.(1.d0+tol))NINS1=NINS1+1
       ENDDO
       IF(ITP/=0)THEN
          IF(ISW.EQ.2)THEN
             IF(AP%ITPST==8)THEN
                ! check the angle for resonances on Torus bifurcations
                THETA=PAR(12)
                D=THETA*(THETA-PI(.5d0))*(THETA-PI(2d0/3))*(THETA-PI(1d0))
             ELSEIF(NDIM>2.AND.ABS( EV(2) - 1.d0 )<5.0d-2.AND. &
                  (NDIM==3.OR.ABS(AIMAG(EV(5)))<SQRT(SQRT(AP%EPSS))).AND. &
                  ((AP%ITPST==5.AND.REAL(EV(5))>0.AND. &
                  ABS( EV(3)-1.d0 )<5.0d-2.AND.ABS( EV(4)-1.d0 )<5.0d-2).OR. &
                  ((AP%ITPST==7.AND.REAL(EV(5))<0.AND. &
                  ABS( EV(3)+1.d0 )<5.0d-2.AND.ABS( EV(4)+1.d0 )<5.0d-2))))THEN
                ! On LP curve: look for 1:1 resonance
                ! On PD curve: look for 1:2 resonance
                D= ABS(EV(5)) - 1.d0
             ENDIF
          ELSE
             IF(AIMAG(EV(2))/=0.d0 .OR. REAL(EV(2))<0.d0)THEN
!               *Ignore if second multiplier is real positive
                D= ABS(EV(2)) - 1.d0
             ENDIF
          ENDIF
       ENDIF
    ENDIF
    IF( ITP/=0 .AND. IID>=2 ) WRITE(9,103)ABS(IBR),NTOP+1,D
    FNSPBV=D
    AP%SPBF=FNSPBV

    NINS=AP%NINS
    IF(NINS1==NINS)ITP=0
    NINS=NINS1
    AP%NINS=NINS

    IF(IID>0)THEN
! Print the Floquet multipliers.

       WRITE(9,104)ABS(IBR),NTOP+1,NINS
       DO I=1,NDIM
          WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I),ABS(EV(I))
       ENDDO
    ENDIF

101 FORMAT(I4,I6,' NOTE:Multiplier inaccurate')
102 FORMAT(I4,I6,' NOTE:Multiplier accurate again')
103 FORMAT(I4,I6,9X,'SPB  Function ',ES14.5)
104 FORMAT(I4,I6,9X,'Multipliers:     Stable:',I4)
105 FORMAT(I4,I6,9X,'Multiplier',I3,1X,2ES14.5, &
         '  Abs. Val.',ES14.5)

  END FUNCTION FNSPBV

! ------- -------- ------
  INTEGER FUNCTION TPSPBV(NDIM,EPSS,ITPST,PAR,NPAR,EV,THETA)

! Determines type of secondary periodic bifurcation.

    USE SUPPORT, ONLY: PI

    INTEGER, INTENT(IN) :: NDIM,ITPST,NPAR
    DOUBLE PRECISION, INTENT(IN) :: EPSS
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(NPAR)
    DOUBLE PRECISION, INTENT(OUT) :: THETA
    COMPLEX(KIND(1.0D0)), INTENT(IN) :: EV(NDIM)

    INTEGER LOC,LOC1,I
    DOUBLE PRECISION AMIN,AZM1,D,AD

    THETA=0
    IF(ITPST==5.OR.ITPST==7)THEN
       ! 1:1 and 1:2 resonances
       TPSPBV=8+10*ITPST
       RETURN
    ELSEIF(ITPST==8)THEN
       TPSPBV=0
       SELECT CASE(NINT(PAR(12)*6/PI(1d0)))
       CASE(0) ! 1:1 res
          TPSPBV=-5-10*ITPST
       CASE(3) ! 1:4 res
          TPSPBV=-8-10*ITPST
       CASE(4) ! 1:3 res
          TPSPBV=8+10*ITPST
       CASE(6) ! 1:2 res
          TPSPBV=7+10*ITPST
       END SELECT
       RETURN
    ENDIF

! Find the eigenvalue closest to z=1.

    LOC=1
    AMIN=HUGE(1.d0)
    DO I=1,NDIM
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
    DO I=1,NDIM
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
       TPSPBV=8+10*ITPST
       THETA=ABS(ATAN2(AIMAG(EV(LOC1)),REAL(EV(LOC1))))
    ELSE IF(REAL(EV(LOC1)).LT.-.5d0)THEN
!       ** period doubling
       TPSPBV=7+10*ITPST
    ELSE
!       ** something else...
       TPSPBV=0
    ENDIF

  END FUNCTION TPSPBV

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

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),ICU(*),NDIM,ISTOP
    DOUBLE PRECISION PAR(*),TM(*),DTM(*),UPS(*),THU(*)
    DOUBLE PRECISION RLDOT(AP%NFPR),UDOTPS(*)
! Local
    DOUBLE PRECISION UMX(7)
    INTEGER IPS,NTST,NCOL,ISW,IPLT,NMX,NPR,NDM,ITP,ITPST,IBR,I,IAB,IBRS,ITMP
    INTEGER LAB,LABW,N2,NINS,NTOT,NTOTS
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
            .OR. AMP.LT.A0.OR.AMP.GT.A1 .OR. NTOT.GE.NMX)THEN
          ISTOP=1
          ITP=9+10*ITPST
          AP%ITP=ITP
       ENDIF
    ENDIF

! All special points receive label:

    LABW=0
    IF(MOD(ITP,10).NE.0) THEN
       LAB=AP%LAB
       LAB=LAB+1
       AP%LAB=LAB
       LABW=LAB
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
    ENDIF

  END SUBROUTINE STPLBV

! ---------- ------
  SUBROUTINE WRTBV8(AP,PAR,ICP,RLDOT,NDIM,UPS,UDOTPS,TM,DTM)

    USE COMPAT
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

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),NDIM
    DOUBLE PRECISION UPS(NDIM,0:*),UDOTPS(NDIM,0:*),TM(0:*),DTM(*)
    DOUBLE PRECISION PAR(*),RLDOT(AP%NFPR)

    INTEGER NTST,NCOL,ISW,ITP,NFPR,IBR,NPAR,NTOT,LAB,NTPL,NAR,NRD,NROWPR
    INTEGER MTOT,I,J,NPARI
    DOUBLE PRECISION T
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

    NTPL=NCOL*NTST+1
    NAR=NDIM+1
    NRD=(NDIM+7)/7+(NDIM+6)/7
    NROWPR=NRD*(NCOL*NTST+1) + (NFPR+6)/7 + (NPAR+6)/7 + (NFPR+19)/20
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

! Write the free parameter indices:

    WRITE(8,103)(ICP(I),I=1,NFPR)

! Write the direction of the branch:

    WRITE(8,102)(RLDOT(I),I=1,NFPR)
    DO J=0,NTST*NCOL
       WRITE(8,102)UDOTPS(:,J)
    ENDDO

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

! ---------- ------
  SUBROUTINE PVLSBV(AP,ICP,UPS,NDIM,PAR)

    USE AUTO_CONSTANTS, ONLY : NPARX

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

    INTEGER NDM,i

    NDM=AP%NDM
    CALL PVLS(NDM,UPS,PAR)

    DO i=NPARX,AP%NPAR+1,-1
       IF(PAR(i)/=0)THEN
          WRITE(6,"(A,I4)"),'NPAR should be at least ',i
          STOP
       ENDIF
    ENDDO

  END SUBROUTINE PVLSBV

! ---------- -----
  SUBROUTINE EVECS(NDIM,P0,P1)

    USE SUPPORT

    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(IN) :: P0(NDIM,*),P1(NDIM,*)

! Local
    DOUBLE PRECISION, ALLOCATABLE :: Q0(:,:),Q1(:,:),P(:,:),Z(:,:),WR(:),WI(:)
    INTEGER IV1(1),I,J,IERR
    DOUBLE PRECISION FV1(1),DET

    ALLOCATE(Q0(NDIM,NDIM), Q1(NDIM,NDIM), P(NDIM,NDIM))
    ALLOCATE(Z(NDIM,NDIM), WR(NDIM), WI(NDIM))

    DO I=1,NDIM
       DO J=1,NDIM
          Q0(I,J)=-P0(I,J)
          Q1(I,J)= P1(I,J)
       ENDDO
    ENDDO

    CALL GEL(NDIM,Q1,NDIM,P,Q0,DET)
    CALL RG(NDIM,NDIM,P,WR,WI,1,Z,IV1,FV1,IERR)

    WRITE(9,100)
    WRITE(9,101)
    DO I=1,NDIM
       WRITE(9,102)WR(I),WI(I),(Z(I,J),J=1,NDIM)
    ENDDO
    WRITE(9,101)
100 FORMAT(" Multipliers + eigenvectors obtained from - P0^-1 P1 :")
!xx
    write(9,112)WR(1)*WR(2)
112 format(" Product = ",ES16.7)       
!xx
101 FORMAT(" ")
102 FORMAT(2ES14.5," | ",8ES14.5)

    DEALLOCATE(Q0,Q1,P,Z,WR,WI)
  END SUBROUTINE EVECS

END MODULE BVP
