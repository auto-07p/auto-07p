!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    General Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE BVP

  PRIVATE
  PUBLIC :: AUTOBV,STPNUB,STPNBV,STPNBV1,PVLSBV,SETRTN
  INTEGER NPARX,NIAP,NRAP
  INCLUDE 'auto.h'

CONTAINS

! ---------- ------
  SUBROUTINE AUTOBV(IAP,RAP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNT, &
       PVLI,THL,THU,IUZ,VUZ)

    USE AUTOMPI
    IMPLICIT NONE

! THIS IS THE ENTRY ROUTINE FOR GENERAL BOUNDARY VALUE PROBLEMS.

    INTEGER IAP(*),ICP(*),ICU(*),IUZ(*)
    DOUBLE PRECISION RAP(*),PAR(*),THL(*),THU(*),VUZ(*)

    EXTERNAL FUNI,BCNI,ICNI,STPNT,PVLI

    IF(IAP(38).GT.0)THEN
!        This is a little trick to tell MPI workers what FUNI and ICNI
!        are.
       DO WHILE(MPIWFI(.TRUE.))
          CALL mpi_setubv_worker(IAP(38),IAP(39),FUNI,ICNI,BCNI)
       ENDDO
       RETURN
    ENDIF
    CALL CNRLBV(IAP,RAP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNT, &
         PVLI,THL,THU,IUZ,VUZ)

  END SUBROUTINE AUTOBV

! ---------- -----------------
  subroutine mpi_setubv_worker(iam,kwt,funi,icni,bcni)
    use autompi
    use solvebv
    implicit none
    integer NIAP,NRAP,NPARX
    include 'auto.h'

    integer iam,kwt
    external funi,icni,bcni

    integer :: ndim, ifst, nllv, na, ncol, nint, ntst, nfpr
    integer :: npar, iap(NIAP)

    double precision, allocatable :: ups(:,:), uoldps(:,:)
    double precision, allocatable :: udotps(:,:), upoldp(:,:), thu(:)
    double precision, allocatable :: dtm(:),par(:)
    integer, allocatable :: np(:),icp(:)
    double precision :: dum,dum1(1),det

    call mpibcasti(iap,NIAP)
    iap(38)=iam
    iap(39)=kwt

    ndim=iap(1)
    ntst=iap(5)
    ncol=iap(6)
    nint=iap(13)
    nfpr=iap(29)
    npar=iap(31)

    allocate(np(kwt))
    call partition(ntst,kwt,np)
    na=np(iam+1)
    deallocate(np)

    allocate(icp(nfpr+nint),thu(ndim*8),dtm(na),par(npar))
    allocate(ups(ndim,0:na*ncol),uoldps(ndim,0:na*ncol))
    allocate(udotps(ndim,0:na*ncol),upoldp(ndim,0:na*ncol))

    call mpisbv(iap,par,icp,ndim,ups,uoldps,udotps,upoldp, &
         dtm,thu,ifst,nllv)
    call solvbv(ifst,iap,det,par,icp,funi,bcni,icni,dum, &
         nllv,dum1,dum1,dum1,ndim,ups,uoldps,udotps,upoldp,dtm, &
         dum1,dum1,dum1,dum1,dum1,thu)

    ! free input arrays
    deallocate(ups,uoldps,dtm,udotps,upoldp,thu,icp,par)

  end subroutine mpi_setubv_worker

! ---------- ------
  SUBROUTINE CNRLBV(IAP,RAP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNT, &
       PVLI,THL,THU,IUZ,VUZ)

    USE IO
    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Controls the computation of solution branches.

    EXTERNAL FUNI,BCNI,ICNI,STPNT,PVLI

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),ICU(*),IUZ(*),VUZ(*),THL(*),THU(*)
! Local
    COMPLEX(KIND(1.0D0)) EV
    ALLOCATABLE RLCUR(:),RLOLD(:),RLDOT(:)
    ALLOCATABLE EV(:),UPS(:,:),UOLDPS(:,:),UPOLDP(:,:)
    ALLOCATABLE UDOTPS(:,:),TM(:),DTM(:)
    ALLOCATABLE P0(:,:),P1(:,:),UZR(:)
    LOGICAL CHNG

! INITIALIZE COMPUTATION OF BRANCH

    NDIM=IAP(1)
    IPS=IAP(2)
    IRS=IAP(3)
    ILP=IAP(4)
    NTST=IAP(5)
    NCOL=IAP(6)
    IAD=IAP(7)
    IADS=IAP(8)
    ISP=IAP(9)
    ISW=IAP(10)
    NUZR=IAP(15)
    ITP=IAP(27)
    ITPST=IAP(28)
    NFPR=IAP(29)

    ALLOCATE(RLCUR(NFPR),RLDOT(NFPR),RLOLD(NFPR))
    ALLOCATE(UPS(NDIM,0:NTST*NCOL),UOLDPS(NDIM,0:NTST*NCOL))
    ALLOCATE(UPOLDP(NDIM,0:NTST*NCOL),UDOTPS(NDIM,0:NTST*NCOL))
    ALLOCATE(TM(0:NTST),DTM(NTST))
    ALLOCATE(P0(NDIM,NDIM),P1(NDIM,NDIM),UZR(NUZR),EV(NDIM))

    CALL SETPBV(IAP,RAP,DTM,NDIM,P0,P1,EV)
    DS=RAP(1)

    RDS=DS
    DSOLD=RDS
    IF(ISP.LT.0)THEN
       ISP=-ISP
       IAP(9)=ISP
    ENDIF
    SP1=0.d0
    BP1=0.d0
    RLP=0.d0
    IF(NUZR.GT.0)THEN
       DO I=1,NUZR
          UZR(I)=0.d0
       ENDDO
    ENDIF
    NITPS=0
    NTOT=0
    IAP(32)=NTOT
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
    CALL RSPTBV(IAP,PAR,ICP,FUNI,STPNT,RLCUR,RLOLD,RLDOT, &
         NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THU)
    CALL PVLI(IAP,ICP,UPS,NDIM,PAR)

!     don't set global rotations here for homoclinics, but in autlib5.f
    IF(IPS.NE.9)CALL SETRTN(IAP(23),NTST*NCOL,NDIM,UPS,PAR)

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
       ! ** Time evolution computations (parabolic systems),
       !    to avoid zero time for direction
       IF(IPS.EQ.14 .OR. IPS.EQ.16)PAR(12)=PAR(12)-DS
       CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,IPERP,P0,P1,THL,THU)
       IF(ISP/=0 .AND. (IPS==2.OR.IPS==7.OR.IPS==12) )THEN
          ! determine and print Floquet multipliers and stability
          SP1 = FNSPBV(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
               RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM, &
               DTM,THL,THU,IUZ,VUZ)
          SP1 = 0.0d0
       ENDIF
    ENDIF

! Store plotting data for restart point :

    CALL STHD(IAP,RAP,ICP,ICU)
    IF(IRS.EQ.0) THEN
       ITP=9+10*ITPST
    ELSE
       ITP=0
    ENDIF
    IAP(27)=ITP
    CALL PVLI(IAP,ICP,UPS,NDIM,PAR)
    CALL STPLBV(IAP,RAP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THL,THU,ISTOP)
    IF(ISTOP==0)THEN
       CALL EXTRBV(NDIM,NTST,NCOL,NFPR,RDS,RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS)
    ENDIF
    DO WHILE(ISTOP==0)
       ITP=0
       IAP(27)=ITP
       CALL STEPBV(IAP,RAP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)

! Check for user supplied parameter output parameter-values.

       IF(ISTOP.EQ.0.AND.NUZR.GT.0)THEN
          DO IUZR=1,NUZR
             IAP(26)=IUZR 
             CALL LCSPBV(IAP,RAP,DSOLD,PAR,ICP,FNUZBV,FUNI,BCNI,ICNI,PVLI, &
                  UZR(IUZR),RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS, &
                  UPOLDP,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP)
             ITP=IAP(27)
             IF(ISTOP.EQ.0.AND.ITP.EQ.-1)THEN
                ITP=-4-10*ITPST
                IAP(27)=ITP
                IF(IUZ(IUZR).GT.0)THEN
                   DO K=1,NUZR
                      UZR(K)=0.d0
                   ENDDO
                ELSE
                   ISTOP=-1
                ENDIF
             ENDIF
          ENDDO
       ENDIF

! Check for fold.

       IF(ISTOP.EQ.0.AND.ABS(ILP).GT.0)THEN
          CALL LCSPBV(IAP,RAP,DSOLD,PAR,ICP,FNLPBV,FUNI,BCNI,ICNI,PVLI,RLP, &
               RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
               TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP)
          ITP=IAP(27)
          IF(ISTOP.EQ.0.AND.ITP.EQ.-1)THEN
             ITP=5+10*ITPST
             IAP(27)=ITP
             IF(ILP.GT.0)THEN
                RLP=0.d0
                BP1=0.d0
                SP1=0.d0
             ELSE
!            *Stop at the first found fold
                ISTOP=-1
             ENDIF
          ENDIF
       ENDIF

! Check for branch point.

       IF(ABS(ISP)>=2.AND.ABS(ISP)/=4)THEN
          CALL LCSPBV(IAP,RAP,DSOLD,PAR,ICP,FNBPBV,FUNI,BCNI,ICNI,PVLI,BP1, &
               RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
               TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP)
          ITP=IAP(27)
          IF(ISTOP.EQ.0.AND.ITP.EQ.-1)THEN
             ITP=6+10*ITPST
             IAP(27)=ITP
             IF(ISP.GT.0)THEN
                RLP=0.d0
                BP1=0.d0
                SP1=0.d0
             ELSE
!            *Stop at the first found BP
                ISTOP=-1
             ENDIF
          ENDIF
       ENDIF

! Check for period-doubling and torus bifurcation.

       IF(ISTOP.EQ.0 .AND. ABS(ISP).GT.0 .AND. &
            (IPS.EQ.2.OR.IPS.EQ.7.OR.IPS.EQ.12) )THEN
          CALL LCSPBV(IAP,RAP,DSOLD,PAR,ICP,FNSPBV,FUNI,BCNI,ICNI,PVLI,SP1, &
               RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
               TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP)
          ITP=IAP(27)
          IF(ISTOP.EQ.0 .AND. ITP.EQ.-1)THEN
!            **Secondary periodic bifurcation: determine type
             CALL TPSPBV(IAP,RAP,PAR,EV)
             IF(ISP.GT.0)THEN
                RLP=0.d0
                BP1=0.d0
                SP1=0.d0
             ELSE
!            *Stop at the first found SPB
                ISTOP=-1
             ENDIF
          ENDIF
       ELSEIF(ABS(ISP).GT.0 .AND. &
            (IPS.EQ.2.OR.IPS.EQ.7.OR.IPS.EQ.12) )THEN
! Still determine and print Floquet multipliers
          SP1 = FNSPBV(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
               RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM, &
               DTM,THL,THU,IUZ,VUZ)
       ENDIF

! Store plotting data.

       CALL PVLI(IAP,ICP,UPS,NDIM,PAR)
       CALL STPLBV(IAP,RAP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THL,THU, &
            ISTOP)

       IF(ISTOP/=0)EXIT
       NTOT=IAP(32)

! Adapt the mesh to the solution.

       IF(IAD.NE.0)THEN
          IF(MOD(NTOT,IAD).EQ.0)THEN
             CALL ADAPT(NTST,NCOL,NDIM,TM,DTM,UPS,UOLDPS, &
                  (IPS.EQ.2 .AND. ABS(ISW).LE.1))
          ENDIF
       ENDIF

! Adapt the stepsize along the branch.

       IF(IADS.NE.0)THEN
          IF(MOD(NTOT,IADS).EQ.0)THEN
             ITNW=IAP(20)
             IBR=IAP(30)
             NTOP=MOD(NTOT-1,9999)+1
             DSMAX=RAP(3)
             CALL ADPTDS(NITPS,ITNW,IBR,NTOP,DSMAX,RDS)
          ENDIF
       ENDIF

! Provide initial approximation and determine next point.

       CALL CONTBV(IAP,DSOLD,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)

    ENDDO
    DEALLOCATE(EV,UPS,UOLDPS,UPOLDP,UDOTPS,TM,DTM,P0,P1)
    DEALLOCATE(UZR,RLCUR,RLOLD,RLDOT)

  END SUBROUTINE CNRLBV

! ---------- ------
  SUBROUTINE CONTBV(IAP,DSOLD,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)

    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Determines an initial approximation to the next solution point,
! by extrapolating from the two preceding points.
! The stepsize used in the preceding step has been stored in DSOLD.

    EXTERNAL FUNI

    DIMENSION IAP(*),PAR(*),ICP(*)
    DIMENSION UPS(NDIM,0:*),UDOTPS(NDIM,0:*),UOLDPS(NDIM,0:*),UPOLDP(*),DTM(*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)

    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)

! Compute rate of change (along branch) of PAR(ICP(1)) and U :

    UDOTPS(:,0:NCOL*NTST)=(UPS(:,0:NCOL*NTST)-UOLDPS(:,0:NCOL*NTST))/DSOLD
    RLDOT(:NFPR)=(RLCUR(:NFPR)-RLOLD(:NFPR))/DSOLD
! Rescale, to set the norm of (UDOTPS,RLDOT) equal to 1.
    CALL SCALEB(NTST,NCOL,NDIM,NFPR,UDOTPS,RLDOT,DTM,THL,THU)

! Extrapolate to get initial approximation to next solution point.

    CALL EXTRBV(NDIM,NTST,NCOL,NFPR,RDS,RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS)

! Store time-derivative.

    CALL STUPBV(IAP,PAR,ICP,FUNI,RLCUR,RLOLD,NDIM,UPS,UOLDPS,UPOLDP)

  END SUBROUTINE CONTBV

! ---------- ------
  SUBROUTINE EXTRBV(NDIM,NTST,NCOL,NFPR,RDS,RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS)

! Determines an initial approximation to the next solution by
! extrapolating from the two preceding points.

    INTEGER, INTENT(IN) :: NDIM,NTST,NCOL,NFPR
    DOUBLE PRECISION, INTENT(IN) :: UDOTPS(NDIM,0:NCOL*NTST), RLDOT(NFPR), RDS
    DOUBLE PRECISION, INTENT(INOUT) :: UPS(NDIM,0:NCOL*NTST), RLCUR(NFPR)
    DOUBLE PRECISION, INTENT(OUT) :: UOLDPS(NDIM,0:NCOL*NTST), RLOLD(NFPR)

    RLOLD(:)=RLCUR(:)
    RLCUR(:)=RLCUR(:)+RDS*RLDOT(:)
    UOLDPS(:,:)=UPS(:,:)
    UPS(:,:)=UPS(:,:)+RDS*UDOTPS(:,:)

  END SUBROUTINE EXTRBV

! ---------- ------
  SUBROUTINE STUPBV(IAP,PAR,ICP,FUNI,RLCUR,RLOLD, &
       NDIM,UPS,UOLDPS,UPOLDP)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Stores U-prime (derivative with respect to T) in UPOLDP.

    EXTERNAL FUNI

    DIMENSION UPS(NDIM,0:*),UOLDPS(NDIM,0:*),UPOLDP(NDIM,0:*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),IAP(*)
! Local
    ALLOCATABLE UOLD(:),DFDU(:,:),DFDP(:,:)

    IPS=IAP(2)
    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)
    NPAR=IAP(31)

    DO I=1,NFPR
       PAR(ICP(I))=RLOLD(I)
    ENDDO

    ALLOCATE(UOLD(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,NPAR))

    DO J=0,NTST*NCOL
       IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
          UOLD(:)=2*UOLDPS(:,J)-UPS(:,J)
       ELSE
          UOLD(:)=UOLDPS(:,J)
       ENDIF
       CALL FUNI(IAP,NDIM,UOLDPS(:,J),UOLD,ICP,PAR,0,UPOLDP(:,J),DFDU,DFDP)
    ENDDO

    DO I=1,NFPR
       PAR(ICP(I))=RLCUR(I)
    ENDDO

    DEALLOCATE(UOLD,DFDU,DFDP)
  END SUBROUTINE STUPBV

! ---------- ------
  SUBROUTINE STEPBV(IAP,RAP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)

    USE MESH
    USE SOLVEBV
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Controls the solution of the nonlinear equations (by Newton's method)
! for the next solution (PAR(ICP(*)) , U) on a branch of solutions.

    EXTERNAL FUNI,BCNI,ICNI,PVLI

    DIMENSION IAP(*),RAP(*),UPS(NDIM,0:*),UOLDPS(NDIM,0:*),UDOTPS(NDIM,0:*)
    DIMENSION UPOLDP(NDIM,0:*),TM(*),DTM(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
    DIMENSION P0(*),P1(*)
    DOUBLE PRECISION, ALLOCATABLE :: DUPS(:,:),DRL(:)
    LOGICAL DONE

    NTST=IAP(5)
    NCOL=IAP(6)
    IADS=IAP(8)
    IID=IAP(18)
    ITNW=IAP(20)
    NWTN=IAP(21)
    NFPR=IAP(29)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    DSMIN=RAP(2)
    EPSL=RAP(11)
    EPSU=RAP(12)

    ALLOCATE(DUPS(NDIM,0:NTST*NCOL),DRL(NFPR))
    DELREF=0
    DO
       DSOLD=RDS
       NITPS=0

! Write additional output on unit 9 if requested.

       CALL WRTBV9(IAP,RAP,RLCUR,NDIM,UPS,TM,DTM,THU,NITPS)

! Generate the Jacobian matrix and the right hand side.

       DO NIT1=1,ITNW

          NITPS=NIT1
          NLLV=0

          IFST=0
          IF(NITPS.LE.NWTN)IFST=1

          CALL SOLVBV(IFST,IAP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDS,NLLV, &
               RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,DUPS,DRL, &
               P0,P1,THL,THU)
          RAP(14)=DET

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

          CALL WRTBV9(IAP,RAP,RLCUR,NDIM,UPS,TM,DTM,THU,NITPS)

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
             CALL PVLI(IAP,ICP,UPS,NDIM,PAR)
             IF(IID.GE.2)WRITE(9,*)
             DEALLOCATE(DUPS,DRL)
             RETURN
          ENDIF

          IF(NITPS.EQ.1)THEN
             DELREF=20*DMAX1(RDRL,RDUMX)
          ELSE
             DELMAX=DMAX1(RDRL,RDUMX)
             IF(DELMAX.GT.DELREF)EXIT
          ENDIF

       ENDDO

! Maximum number of iterations reached.

       IF(IADS.EQ.0)THEN
          WRITE(9,101)IBR,NTOP
          EXIT
       ENDIF

! Reduce stepsize and try again.

       DSMAX=RAP(3)
       NITPS=ITNW
       CALL ADPTDS(NITPS,ITNW,IBR,NTOP,DSMAX,RDS)
       IF(ABS(RDS).LT.DSMIN)THEN
          ! Minimum stepsize reached.
          WRITE(9,103)IBR,NTOP
          EXIT
       ENDIF
       RLCUR(:NFPR)=RLOLD(:NFPR)+RDS*RLDOT(:NFPR)
       UPS(:,0:NCOL*NTST)=UOLDPS(:,0:NCOL*NTST)+RDS*UDOTPS(:,0:NCOL*NTST)
       IF(IID.GE.2)WRITE(9,102)IBR,NTOP
    ENDDO

! Minimum stepsize reached.

    DO I=1,NFPR
       RLCUR(I)=RLOLD(I)
       PAR(ICP(I))=RLCUR(I)
    ENDDO
    UPS(:,0:NCOL*NTST)=UOLDPS(:,0:NCOL*NTST)
    ISTOP=1
    DEALLOCATE(DUPS,DRL)

101 FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
102 FORMAT(I4,I6,' NOTE:Retrying step')
103 FORMAT(I4,I6,' NOTE:No convergence using minimum step size')

  END SUBROUTINE STEPBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!      Restart of Solution Branches ( Differential Equations )
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE RSPTBV(IAP,PAR,ICP,FUNI,STPNT,RLCUR,RLOLD, &
       RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THU)

    USE IO
    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Restarts computation of a branch of solutions at point labelled IRS.
! The output written on unit 8 by a previous run is now expected as
! input on unit 3. The label IRS, where computation is to resume, must
! be specified in the user-supplied subroutine INIT.
! If IRS=0 then the starting point must be provided analytically in the
! user-supplied subroutine STPNT.

    EXTERNAL FUNI, STPNT

    DIMENSION IAP(*)
    DIMENSION UPS(NDIM,0:*),UOLDPS(NDIM,0:*),UPOLDP(NDIM,0:*),UDOTPS(NDIM,0:*)
    DIMENSION TM(0:*),DTM(*),PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
    DIMENSION THU(*)

    IRS=IAP(3)
    NTST=IAP(5)
    NCOL=IAP(6)
    ITP=IAP(27)
    NFPR=IAP(29)

! Get restart data :

    IF(IRS>0)THEN
       NTSRS=GETNTST3()
       NCOLRS=GETNCOL3()
    ELSE
       NTSRS=NTST
       NCOLRS=NCOL
    ENDIF
    IF(NCOLRS*NTSRS==0)THEN
       IF(ITP==3 .OR. ABS(ITP/10)==3) THEN
          ! Hopf bifurcation
          CALL STHOPF(IAP,PAR,ICP,NTST,NCOL,NFPR,RLDOT, &
               NDIM,UPS,UDOTPS,UPOLDP,TM,NODIR,THU,FUNI)
       ELSE
          WRITE(6,"(A)")"The restart label is not a Hopf bifurcation."
          STOP
       ENDIF
    ELSE
       CALL STPNT(IAP,PAR,ICP,NTSRS,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)
    ENDIF

! Determine a suitable starting label and branch number.

    CALL NEWLAB(IAP)

    DTM(1:NTST)=TM(1:NTST)-TM(0:NTST-1)

! Set UOLDPS, RLOLD.

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
       IAP(10)=ISW
    ELSE
!      ** Restart from orbit.
       CALL STUPBV(IAP,PAR,ICP,FUNI,RLCUR,RLOLD,NDIM,UPS,UOLDPS,UPOLDP)
    ENDIF

  END SUBROUTINE RSPTBV

! ---------- ------
  SUBROUTINE STPNBV(IAP,PAR,ICP,NTSR,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE MESH

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    DIMENSION IAP(*),UPS(*),UDOTPS(*)
    DIMENSION PAR(*),ICP(*),RLDOT(*),TM(*)
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),UDOTPSR(NDIM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL STPNBV1(IAP,PAR,ICP,NDIM,NTSRS,NDIMRD,NCOLRS,RLDOT, &
         UPSR,UDOTPSR,TMR,NODIR)
    CALL ADAPT2(NTSR,NCOLRS,NDIM,NTST,NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNBV

! ---------- -------
  SUBROUTINE STPNBV1(IAP,PAR,ICP,NDIM,NTSRS,NDIMRD,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE IO
    IMPLICIT NONE

! This subroutine locates and retrieves the information required to
! restart computation at the point with label IRS.
! This information is expected on unit 3.

    INTEGER, INTENT(IN) :: ICP(*),NDIM
    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(OUT) :: NTSRS,NDIMRD,NCOLRS,NODIR
    DOUBLE PRECISION, INTENT(OUT) :: UPS(*),UDOTPS(*),TM(*)
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(*)
! Local
    INTEGER NFPR,NFPRS,ITPRS,I
    INTEGER, ALLOCATABLE :: ICPRS(:)

    NFPR=IAP(29)

    ALLOCATE(ICPRS(NFPR))
    CALL READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOT,UPS, &
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
  SUBROUTINE STPNUB(IAP,PAR,ICP,NTSRS,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Generates a starting point for the continuation of a branch of
! of solutions to general boundary value problems by calling the user
! supplied subroutine STPNT where an analytical solution is given.

    DIMENSION IAP(*),UPS(IAP(1),0:*),UDOTPS(IAP(1),0:*),TM(0:*)
    DIMENSION PAR(*),ICP(*),RLDOT(*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

! Generate the (initially uniform) mesh.

    CALL MSH(NTST,TM)

    DO J=0,NTST*NCOL
       CALL STPNT(NDIM,UPS(:,J),PAR,DBLE(J)/(NTST*NCOL))
    ENDDO

    IBR=1
    IAP(30)=IBR
    LAB=0
    IAP(37)=LAB

    NODIR=1

  END SUBROUTINE STPNUB

! ---------- ------
  SUBROUTINE STHOPF(IAP,PAR,ICP,NTST,NCOL, &
       NFPR,RLDOT,NDIM,UPS,UDOTPS,UPOLDP,TM,NODIR,THU,FUNI)

    USE IO
    USE MESH
    USE SUPPORT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!  Generates starting data for a periodic orbit from a Hopf
!  bifurcation point (for waves or periodic orbits)

    DIMENSION PAR(*),ICP(*),IAP(*),RLDOT(*),THU(*)
    DIMENSION UPS(NDIM,0:*),UDOTPS(NDIM,0:*),UPOLDP(NDIM,0:*),TM(*)
    EXTERNAL FUNI
! Local
    ALLOCATABLE DFU(:,:),SMAT(:,:),RNLLV(:),F(:),U(:),UDOT(:), DTM(:)
    DOUBLE PRECISION DUMDFP(1),UOLD(1)
    INTEGER :: ICPRS(2)
    DOUBLE PRECISION THL(2)

    ALLOCATE(DFU(NDIM,NDIM),F(NDIM),U(NDIM),UDOT(NDIM+1))
    ALLOCATE(RNLLV(2*NDIM),SMAT(2*NDIM,2*NDIM))

    CALL READLB(IAP,ICPRS,U,UDOT,PAR)

    PERIOD=PAR(11)
    TPI=PI(2.d0)
    RIMHB=TPI/PERIOD

    SMAT(:,:)=0.d0

    DO I=1,NDIM
       SMAT(I,I)=-RIMHB
       SMAT(NDIM+I,NDIM+I)=RIMHB
    ENDDO

    CALL FUNI(IAP,NDIM,U,UOLD,ICP,PAR,1,F,DFU,DUMDFP)

! Note that the period-scaling in FUNC is taken into account:
    SMAT(1:NDIM,NDIM+1:2*NDIM)=DFU(:,:)/PAR(11)
    SMAT(NDIM+1:2*NDIM,1:NDIM)=DFU(:,:)/PAR(11)

    CALL NLVC(2*NDIM,2*NDIM,2,SMAT,RNLLV)
    CALL NRMLZ(2*NDIM,RNLLV)

! Generate the (initially uniform) mesh.

    CALL MSH(NTST,TM)

    DO J=0,NTST*NCOL
       T=J*TPI/(NTST*NCOL)
       S=SIN(T)
       C=COS(T)
       UDOTPS(:,J)=S*RNLLV(1:NDIM)+C*RNLLV(NDIM+1:2*NDIM)
       UPOLDP(:,J)=C*RNLLV(1:NDIM)-S*RNLLV(NDIM+1:2*NDIM)
       UPS(:,J)=U(:)
    ENDDO

    RLDOT(1:2)=0.d0
    THL(1:2)=0.d0

    ALLOCATE(DTM(NTST))
    DTM(:)=1.d0/NTST

    CALL SCALEB(NTST,NCOL,NDIM,NFPR,UDOTPS,RLDOT,DTM,THL,THU)

    NODIR=-1

    DEALLOCATE(DFU,F,U,UDOT,RNLLV,SMAT,DTM)
  END SUBROUTINE STHOPF

! ---------- ------
  SUBROUTINE SETRTN(NDM,NTNC,NDIM,UPS,PAR)

    USE SUPPORT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Initialization for rotations
    
    POINTER NRTN(:)
    COMMON /BLRTN/ NRTN,IRTN
    DIMENSION UPS(NDIM,0:NTNC),PAR(*)

    ALLOCATE(NRTN(NDM))
    IRTN=0
    DO I=1,NDM
       NRTN(I)=NINT( (UPS(I,NTNC)-UPS(I,0)) / PI(2.d0) )
       IF(NRTN(I).NE.0)THEN
          PAR(19)=PI(2.d0)
          IRTN=1
       ENDIF
    ENDDO
    IF(IRTN.EQ.0)DEALLOCATE(NRTN)

  END SUBROUTINE SETRTN

! ---------- ------
  SUBROUTINE STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD, &
       RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,IPERP, &
       P0,P1,THL,THU)

    USE MESH
    USE SOLVEBV
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Generates a direction vector (UDOTPS,RLDOT) that is needed to start
! the computation of a branch when no direction vector is given.

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),RAP(*),UDOTPS(NDIM,0:IAP(5)*IAP(6)),DTM(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(IAP(29)),THL(*),THU(*)
    DOUBLE PRECISION UPS(*),UOLDPS(*),UPOLDP(*),P0(*),P1(*)
    ALLOCATABLE DUPS(:,:),DRL(:)

! Generate the Jacobian matrix with zero direction vector.
! (Then the last row of the Jacobian will be zero)
! in case the starting direction is to be determined.

    NTST=IAP(5)
    NCOL=IAP(6)
    IID=IAP(18)
    NFPR=IAP(29)

    IF(IPERP.EQ.0)THEN
       UDOTPS(:,:)=0.d0
       RLDOT(:)=0.d0
    ENDIF

    RDSZ=0.d0
    NLLV=1
    IFST=1
    ALLOCATE(DUPS(NDIM,0:NCOL*NTST),DRL(NFPR))
    CALL SOLVBV(IFST,IAP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
         RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,DUPS,DRL, &
         P0,P1,THL,THU)
    RAP(14)=DET

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
  SUBROUTINE LCSPBV(IAP,RAP,DSOLD,PAR,ICP,FNCS,FUNI,BCNI,ICNI,PVLI,Q, &
       RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
       TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS,ISTOP)

    USE SUPPORT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

! This subroutine uses the Secant method to accurately locate folds
! branch points, and zero(es) of user parameter values.
! Such points are located as points on a solution branch where the
! passed function FNCS changes sign.
! It involves calling the basic solution subroutines CONTBV and STEP
! with decreasing values of RDS (stepsize along branch).
! The point is assumed to have been found with sufficient accuracy if
! the ratio between RDS and the user supplied value of DS is less than
! the user-supplied tolerance EPSS.
! This subroutine is called from CNRLB, which controls the computation
! of branches of solutions to general boundary value problems.

    EXTERNAL FUNI,BCNI,ICNI,PVLI

    COMPLEX(KIND(1.0D0)) EV(*)

    LOGICAL CHNG

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),TM(*),DTM(*)
    DIMENSION UPS(*),UDOTPS(*),UOLDPS(*),UPOLDP(*),IUZ(*),VUZ(*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*),P0(*),P1(*)

    IID=IAP(18)
    ITMX=IAP(19)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    DS=RAP(1)
    DSMAX=RAP(3)
    EPSS=RAP(13)

! Check for zero.

    Q0=Q
    Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
         RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
         TM,DTM,THL,THU,IUZ,VUZ)

    PQ=Q0*Q1
    IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
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
          ITP=-1
          IAP(27)=ITP
!xx???   Q=0.d0
          WRITE(9,102)RDS
          RETURN
       ENDIF

! If requested write additional output on unit 9 :

       IF(IID.GE.2)THEN
          WRITE(9,101)NITSP1,RDS
       ENDIF

       CALL CONTBV(IAP,DSOLD,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
            NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
       CALL STEPBV(IAP,RAP,DSOLD,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,P0,P1,THL,THU,NITPS,ISTOP)
       IF(ISTOP.NE.0)THEN
          Q=0.d0
          RETURN
       ENDIF

! Check for zero.

       Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
            RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP, &
            TM,DTM,THL,THU,IUZ,VUZ)

!        Use Mueller's method with bracketing for subsequent steps
       CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
    ENDDO

    WRITE(9,103)IBR,NTOP+1
    Q=0.d0
101 FORMAT(' ==> Location of special point :  Iteration ',I3, &
         '  Step size = ',ES13.5)
102 FORMAT(' ==> Location of special point : ', &
         ' Convergence.   Step size = ',ES13.5)
103 FORMAT(I4,I6,' NOTE:Possible special point')

  END SUBROUTINE LCSPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNLPBV &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,THL,THU,IUZ,VUZ)

    USE MESH
    USE SOLVEBV
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! RETURNS A QUANTITY THAT CHANGES SIGN AT A LIMIT POINT (BVP)

    COMPLEX(KIND(1.0D0)) EV(*)

    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),UDOTPS(NDIM,0:IAP(5)*IAP(6))
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(IAP(29)),TM(*),DTM(*),THL(*),THU(*)
    DOUBLE PRECISION UPS(*),UOLDPS(*),UPOLDP(*),P0(*),P1(*)

    DOUBLE PRECISION, ALLOCATABLE :: DUPS(:,:),DRL(:)

    NTST=IAP(5)
    NCOL=IAP(6)
    IID=IAP(18)
    NFPR=IAP(29)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

! Find the direction vector.

    NLLV=-1
    IFST=0
    RDSZ=0.d0

    ALLOCATE(DUPS(NDIM,0:NTST*NCOL),DRL(NFPR))
    CALL SOLVBV(IFST,IAP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
         RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,DUPS,DRL, &
         P0,P1,THL,THU)
    RAP(14)=DET

    RLDOT(:)=DRL(:)
    UDOTPS(:,:)=DUPS(:,:)
    DEALLOCATE(DUPS,DRL)

! Scale the direction vector.

    CALL SCALEB(NTST,NCOL,NDIM,NFPR,UDOTPS,RLDOT,DTM,THL,THU)
    IF(IID.GE.2)THEN
       WRITE(9,101)ABS(IBR),NTOP+1,RLDOT(1)
    ENDIF

! Set the quantity to be returned.

    FNLPBV=RLDOT(1)
    CHNG=.TRUE.
    RAP(16)=FNLPBV

101 FORMAT(I4,I6,9X,'Fold Function ',ES14.5)

  END FUNCTION FNLPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNBPBV &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,THL,THU,IUZ,VUZ)

    USE SUPPORT

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    COMPLEX(KIND(1.0D0)) EV(*)

    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),RAP(*),P1(*)
! Local
    ALLOCATABLE PP(:)
    DOUBLE PRECISION U(1),F(1)

    IID=IAP(18)

! Save the determinant of the reduced system.

    DET=RAP(14)
    DET0=DET
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

! Compute the determinant of P1.

    ALLOCATE(PP(NDIM**2))
    DO I=1,NDIM**2
       PP(I)=P1(I)
    ENDDO
    CALL GEL(NDIM,PP,0,U,F,DET)
    DEALLOCATE(PP)
    RAP(14)=DET

! Set the determinant of the normalized reduced system.

    IF(ABS(DET0)/HUGE(DET).LT.ABS(DET))THEN
       FNBPBV=DET0/DET
       CHNG=.TRUE.
    ELSE
       FNBPBV=0.d0
       CHNG=.FALSE.
    ENDIF
    RAP(18)=FNBPBV

    IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1,FNBPBV
101 FORMAT(I4,I6,9X,'BP   Function ',ES14.5)

  END FUNCTION FNBPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNSPBV &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,THL,THU,IUZ,VUZ)

    USE FLOQUET

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

! This function returns a quantity that changes sign when a complex
! pair of eigenvalues of the linearized Poincare map moves in or out
! of the unit circle or when a real eigenvalues passes through -1.

    COMPLEX(KIND(1.0D0)) EV(*),ZTMP
    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),P0(*),P1(*)
    DOUBLE PRECISION RLCUR(*),RLOLD(*),RLDOT(*)
    DOUBLE PRECISION UPS(*),UDOTPS(*),UOLDPS(*),UPOLDP(*)
    DOUBLE PRECISION TM(*),DTM(*),THL(*),THU(*),VUZ(*)
    INTEGER IUZ(*)
! Local
    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    ISP=IAP(9)
    ISW=IAP(10)
    IID=IAP(18)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

! Initialize.

    FNSPBV=0.d0
    RAP(19)=FNSPBV
    D=0.d0
    CHNG=.FALSE.

    IF(IID.GE.4)THEN
       CALL EVECS(NDIM,P0,P1)
    ENDIF

!  Compute the Floquet multipliers
    CALL FLOWKM(NDIM, P0, P1, IID, EV)

! Find the multiplier closest to z=1.

    AMIN=RLARGE
    LOC=1
    DO J=1,NDIM
       AZM1= ABS( EV(J) - 1.d0 )
       IF(AZM1.LE.AMIN)THEN
          AMIN=AZM1
          LOC=J
       ENDIF
    ENDDO
    IF(LOC.NE.1) THEN
       ZTMP=EV(LOC)
       EV(LOC)=EV(1)
       EV(1)=ZTMP
    ENDIF

! Order the remaining Floquet multipliers by distance from |z|=1.

    IF(NDIM.GE.3)THEN
       DO I=2,NDIM-1
          AMIN=RLARGE
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

! Print error message if the Floquet multiplier at z=1 is inaccurate.
! (ISP is set to negative and detection of bifurations is discontinued)

    AMIN= ABS( EV(1) - 1.d0 )
    IF(AMIN>5.0D-2 .AND. (ISP==2 .OR. ISP==4)) THEN
       IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1
       DO I=1,NDIM
          WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I)
       ENDDO
       NINS=0
       IAP(33)=NINS
       WRITE(9,104)ABS(IBR),NTOP+1,NINS
       ISP=-ISP
       IAP(9)=ISP
       RETURN
    ENDIF

! Restart automatic detection if the Floquet multiplier at z=1 is
! sufficiently accurate again.

    IF(ISP.LT.0)THEN
       IF(AMIN.LT.1.0E-2)THEN
          WRITE(9,102)ABS(IBR),NTOP+1
          ISP=-ISP
          IAP(9)=ISP
       ELSE
          DO I=1,NDIM
             WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I)
          ENDDO
          RETURN
       ENDIF
    ENDIF

! Count the number of Floquet multipliers inside the unit circle.
!
! Set tolerance for deciding if a multiplier is outside |z=1|.
! Use, for example, tol=1d-3 for conservative systems.
    tol=1.d-5

    NINS1=1
    IF(NDIM.EQ.1) THEN
       D=0.d0
       FNSPBV=D
       RAP(19)=FNSPBV
    ELSE
       DO I=2,NDIM
          IF( ABS(EV(I)).LE.(1.d0+tol))NINS1=NINS1+1
       ENDDO
       IF(ISP==2.OR.ISP==4) THEN
          IF(AIMAG(EV(2)).EQ.0.d0 .AND. REAL(EV(2)).GT.0.d0)THEN
!            *Ignore if second multiplier is real positive
             D=0.d0
          ELSE
             D= ABS(EV(2)) - 1.d0
          ENDIF
          IF(ISW.EQ.2)THEN
             FNSPBV=0.d0
          ELSE
             FNSPBV=D
          ENDIF
          RAP(19)=FNSPBV
          NINS=IAP(33)
          IF(NINS1.NE.NINS)CHNG=.TRUE.
       ENDIF
    ENDIF

    NINS=NINS1
    IAP(33)=NINS
    IF( IID>=2 .AND. (ISP==1 .OR. ISP==2 .OR. ISP==4))THEN
       WRITE(9,103)ABS(IBR),NTOP+1,D
    ENDIF

! Print the Floquet multipliers.

    NINS=IAP(33)
    WRITE(9,104)ABS(IBR),NTOP+1,NINS
    DO I=1,NDIM
       WRITE(9,105)ABS(IBR),NTOP+1,I,EV(I),ABS(EV(I))
    ENDDO

101 FORMAT(I4,I6,' NOTE:Multiplier inaccurate')
102 FORMAT(I4,I6,' NOTE:Multiplier accurate again')
103 FORMAT(I4,I6,9X,'SPB  Function ',ES14.5)
104 FORMAT(I4,I6,9X,'Multipliers:     Stable:',I4)
105 FORMAT(I4,I6,9X,'Multiplier',I3,1X,2ES14.5, &
         '  Abs. Val.',ES14.5)

  END FUNCTION FNSPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNUZBV &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT, &
       NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,THL,THU,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    COMPLEX(KIND(1.0D0)) EV(*)

    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),PAR(*),IUZ(*),VUZ(*)

    IID=IAP(18)
    IUZR=IAP(26)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    FNUZBV=PAR(ABS(IUZ(IUZR)))-VUZ(IUZR)
    CHNG=.TRUE.

    IF(IID.GE.3)WRITE(9,101)ABS(IBR),NTOP+1,IUZR,FNUZBV
101 FORMAT(I4,I6,9X,'User Func.',I3,1X,ES14.5)

  END FUNCTION FNUZBV

! ---------- ------
  SUBROUTINE TPSPBV(IAP,RAP,PAR,EV)

! Determines type of secondary periodic bifurcation.

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

    COMPLEX(KIND(1.0D0)) EV(*)

    DIMENSION PAR(*),IAP(*),RAP(*)

    NDIM=IAP(1)

    EPSS=RAP(13)
    ITPST=IAP(28)

! Find the eigenvalue closest to z=1.

    LOC=1
    AMIN=RLARGE
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
    AMIN=RLARGE
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
       ITP=8+10*ITPST
       IAP(27)=ITP
       PAR(12)=ASIN(AIMAG(EV(LOC1)))
    ELSE IF(REAL(EV(LOC1)).LT.-.5d0)THEN
!       ** period doubling
       ITP=7+10*ITPST
       IAP(27)=ITP
    ELSE
!       ** something else...
       ITP=0
       IAP(27)=ITP
    ENDIF

  END SUBROUTINE TPSPBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Output (Boundary Value Problems)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE STPLBV(IAP,RAP,PAR,ICP,ICU,RLDOT,NDIM,UPS,UDOTPS,TM,DTM,THL,THU,ISTOP)

    USE IO
    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

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

    DIMENSION PAR(*),ICP(*),ICU(*),IAP(*),RAP(*),TM(*),DTM(*),UPS(*),THL(*),THU(*)
    DIMENSION RLDOT(*),UDOTPS(*)
! Local
    DIMENSION UMX(7)

    IPS=IAP(2)
    NTST=IAP(5)
    NCOL=IAP(6)
    ISW=IAP(10)
    IPLT=IAP(11)
    NMX=IAP(14)
    NPR=IAP(16)
    NDM=IAP(23)
    ITP=IAP(27)
    ITPST=IAP(28)
    IBR=IAP(30)

    RL0=RAP(6)
    RL1=RAP(7)
    A0=RAP(8)
    A1=RAP(9)

    NTOT=IAP(32)
    NTOT=NTOT+1
    IAP(32)=NTOT

! ITP is set to 4 every NPR steps along a branch of solns and the entire
! solution is written on unit 8.

    IF(NPR.NE.0)THEN
       IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
       IAP(27)=ITP
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

    RAP(10)=AMP

    IF(ISTOP.EQ.1)THEN
!      ** Maximum number of iterations reached somewhere.
       ITP=-9-10*ITPST
       IAP(27)=ITP
    ELSE
       IF(PAR(ICP(1)).LT.RL0.OR.PAR(ICP(1)).GT.RL1 &
            .OR. AMP.LT.A0.OR.AMP.GT.A1 .OR. NTOT.GE.NMX)THEN
          ISTOP=1
          ITP=9+10*ITPST
          IAP(27)=ITP
       ENDIF
    ENDIF

! All special points receive label:

    LABW=0
    IF(MOD(ITP,10).NE.0) THEN
       LAB=IAP(37)
       LAB=LAB+1
       IAP(37)=LAB
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

    IF(IPS.EQ.2)THEN
       IBRS=-IBR
    ELSE
       IBRS=IBR
    ENDIF

! Determine stability, and write output on units 7 and 8.

    NTOTS=NTOT
    IF(ABS(ISW).LE.1 .AND. (IPS.EQ.2.OR.IPS.EQ.7))THEN
       NINS=IAP(33)
       IF(NINS.EQ.NDIM)NTOTS=-NTOT
    ENDIF
    CALL WRLINE(IAP,PAR,ICU,IBRS,NTOTS,LABW,AMP,UMX)

! Write plotting and restart data on unit 8.

    IF(MOD(ITP,10).NE.0)THEN
       CALL WRTBV8(IAP,PAR,ICP,RLDOT,NDIM,UPS,UDOTPS,TM,DTM)
    ENDIF

  END SUBROUTINE STPLBV

! ---------- ------
  SUBROUTINE WRTBV8(IAP,PAR,ICP,RLDOT,NDIM,UPS,UDOTPS,TM,DTM)

    USE COMPAT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

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

    DIMENSION IAP(*),UPS(NDIM,0:*),UDOTPS(NDIM,0:*),TM(0:*),DTM(*)
    DIMENSION PAR(*),ICP(*),RLDOT(*)
!xxx====================================================================
!xxx Test problem: compute the error
!    err(x,t)=x - 2*DATAN(1.d0)*PAR(2)*DSIN(4*DATAN(1.d0)*t)
!xxx====================================================================

    NTST=IAP(5)
    NCOL=IAP(6)
    ISW=IAP(10)
    ITP=IAP(27)
    NFPR=IAP(29)
    IBR=IAP(30)
    NPAR=IAP(31)
    NTOT=IAP(32)
    LAB=IAP(37)

! Write information identifying the solution :

    NTPL=NCOL*NTST+1
    NAR=NDIM+1
    NRD=(NDIM+7)/7+(NDIM+6)/7
    NROWPR=NRD*(NCOL*NTST+1) + (NFPR+6)/7 + (NPAR+6)/7 + (NFPR+19)/20
    MTOT=MOD(NTOT-1,9999)+1
    WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR

! Write the entire solution on unit 8 :

!xxx====================================================================
!xxx Test problem
    !xxx eg=0.d0
    !xxx em=0.d0
!xxx====================================================================
    DO J=0,NTST*NCOL
       T=TM(J/NCOL)+MOD(J,NCOL)*DTM(J/NCOL+1)/NCOL
       WRITE(8,102)T,UPS(:,J)
!xxx====================================================================
!xxx Test problem
       !xxx er = err(ups(1,j),T)
       !xxx if(dabs(er).gt.eg)eg=dabs(er)
       !xxx if(i.eq.1 .and. dabs(er).gt.em)em=dabs(er)
!xxx====================================================================
    ENDDO
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

101 FORMAT(6I6,I8,I6,I8,3I5)
102 FORMAT(4X,7ES19.10)
103 FORMAT(20I5)

    CALL AUTOFLUSH(8)
  END SUBROUTINE WRTBV8

! ---------- ------
  SUBROUTINE WRTBV9(IAP,RAP,RLCUR,NDIM,UPS,TM,DTM,THU,NITPS)

    USE IO
    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Writes additional output on unit 9.

    DIMENSION IAP(*),RAP(*)
    DIMENSION DTM(*),UPS(NDIM,0:*),TM(*),RLCUR(*),THU(*)

    NTST=IAP(5)
    NCOL=IAP(6)
    IPLT=IAP(11)
    IID=IAP(18)
    NDM=IAP(23)
    IBR=IAP(30)
    NTOT=IAP(32)

    IAB=ABS(IPLT)
    IF(IAB.EQ.0.OR.IAB.GT.NDIM)AMP=SQRT(RNRMSQ(NTST,NCOL,NDIM,NDM,UPS,DTM,THU))
    IF(IPLT.GT.0.AND.IAB.LE.NDIM)AMP=RMXUPS(NTST,NCOL,NDIM,IAB,UPS)
    IF(IPLT.LT.0.AND.IAB.LE.NDIM)AMP=RMNUPS(NTST,NCOL,NDIM,IAB,UPS)
    RAP(10)=AMP
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
       DO J=0,NTST*NCOL
          T=TM(J/NCOL)+MOD(J,NCOL)*DTM(J/NCOL+1)/NCOL
          WRITE(9,105)T,UPS(:,J)
       ENDDO
    ENDIF
102 FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
103 FORMAT(I4,I6,I4,5X,6ES14.5)
104 FORMAT(' UPS :')
105 FORMAT(1X,7ES14.5)

  END SUBROUTINE WRTBV9

! ---------- ------
  SUBROUTINE PVLSBV(IAP,ICP,UPS,NDIM,PAR)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION IAP(*),ICP(*),UPS(NDIM,0:*),PAR(*)

    NDM=IAP(23)
    CALL PVLS(NDM,UPS,PAR)

  END SUBROUTINE PVLSBV

! ---------- -----
  SUBROUTINE EVECS(NDIM,P0,P1)

    USE SUPPORT

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION P0(NDIM,*),P1(NDIM,*)

! Local
    ALLOCATABLE Q0(:,:), Q1(:,:), P(:,:), Z(:,:), WR(:), WI(:)
    ALLOCATABLE IV1(:), FV1(:)

    ALLOCATE(Q0(NDIM,NDIM), Q1(NDIM,NDIM), P(NDIM,NDIM))
    ALLOCATE(Z(NDIM,NDIM), WR(NDIM), WI(NDIM))
    ALLOCATE(IV1(NDIM), FV1(NDIM))

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

    DEALLOCATE(Q0,Q1,P,Z,WR,WI,IV1,FV1)
  END SUBROUTINE EVECS

! ---------- ------
  SUBROUTINE SETPBV(IAP,RAP,DTM,NDIM,P0,P1,EV)

    USE SUPPORT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    TARGET IAP(NIAP),RAP(NRAP),DTM(IAP(5))
    TARGET P0(NDIM,NDIM),P1(NDIM,NDIM)
    COMPLEX(KIND(1.0D0)), TARGET :: EV(NDIM)

    IAV=>IAP
    RAV=>RAP
    DTV=>DTM
    P0V=>P0
    P1V=>P1
    EVV=>EV

  END SUBROUTINE SETPBV

END MODULE BVP
