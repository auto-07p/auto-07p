!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    General Boundary Value Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE BVP

  PRIVATE
  PUBLIC :: AUTOBV,STPNUB,STPNBV,STPNBV1,PVLSBV,READBV,SETRTN
  INTEGER NPARX,NIAP,NRAP
  INCLUDE 'auto.h'

CONTAINS

! ---------- ------
  SUBROUTINE AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT, &
       PVLI,THL,THU,IUZ,VUZ)

    USE AUTOMPI
    IMPLICIT NONE

! THIS IS THE ENTRY ROUTINE FOR GENERAL BOUNDARY VALUE PROBLEMS.

    INTEGER IAP(*),ICP(*),IUZ(*)
    DOUBLE PRECISION RAP(*),PAR(*),THL(*),THU(*),VUZ(*)

    EXTERNAL FUNI,BCNI,ICNI,STPNT,PVLI

    IF(IAP(38).GT.0)THEN
!        This is a little trick to tell MPI workers what FUNI and ICNI
!        are.
       DO WHILE(MPIWFI(.TRUE.))
          CALL mpi_setubv_worker(IAP,RAP,PAR,ICP,FUNI,ICNI,BCNI)
       ENDDO
       RETURN
    ENDIF
    CALL CNRLBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT, &
         PVLI,THL,THU,IUZ,VUZ)

  END SUBROUTINE AUTOBV

! ---------- -----------------
  subroutine mpi_setubv_worker(iap,rap,par,icp,funi,icni,bcni)
    use autompi
    use solvebv
    implicit none
    integer NIAP,NRAP,NPARX
    include 'auto.h'

    integer :: iap(*), icp(*)
    double precision :: rap(*), par(*)
    external funi,icni,bcni

    integer :: ndim, nra, nfc, ifst, nllv, na, iam, kwt, nbc, ncol, nint, ntst

    double precision :: rldot(NPARX)
    double precision, allocatable :: ups(:,:), uoldps(:,:)
    double precision, allocatable :: udotps(:,:), upoldp(:,:), thu(:)
    double precision, allocatable :: dtm(:),fa(:,:), fc(:)
    integer, allocatable :: np(:)
    double precision :: dum,dum1(1)

    iam=iap(38)
    kwt=iap(39)
    call mpibcasti(iap,NIAP)
    iap(38)=iam
    iap(39)=kwt

    ndim=iap(1)
    ntst=iap(5)
    ncol=iap(6)
    nbc=iap(12)
    nint=iap(13)

    allocate(np(kwt))
    call partition(ntst,kwt,np)
    na=np(iam+1)
    deallocate(np)
    nra=ndim*ncol
    nfc=nbc+nint+1

    allocate(thu(ndim*8),dtm(na))
    allocate(ups(nra,na+1),uoldps(nra,na+1),udotps(nra,na+1),upoldp(nra,na+1))
    ! output arrays
    allocate(fa(nra,na),fc(nfc))

    call mpisbv(iap,rap,par,icp,rldot,nra,ups,uoldps,udotps,upoldp, &
         dtm,thu,ifst,nllv)
    call solvbv(ifst,iap,rap,par,icp,funi,bcni,icni,dum, &
         nllv,dum1,dum1,rldot,nra,ups,dum1,uoldps,udotps,upoldp,dtm, &
         fa,fc,dum1,dum1,dum1,thu)

    ! free input arrays
    deallocate(ups,uoldps,dtm,udotps,upoldp,thu)

    deallocate(fa,fc)

  end subroutine mpi_setubv_worker

! ---------- ------
  SUBROUTINE CNRLBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT, &
       PVLI,THL,THU,IUZ,VUZ)

    USE IO
    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Controls the computation of solution branches.

    EXTERNAL FUNI,BCNI,ICNI,STPNT,PVLI

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),IUZ(*),VUZ(*),THL(*),THU(*)
! Local
    DIMENSION RLCUR(NPARX),RLOLD(NPARX),RLDOT(NPARX) 
    COMPLEX(KIND(1.0D0)) EV
    ALLOCATABLE EV(:),UPS(:,:),UOLDPS(:,:),UPOLDP(:,:)
    ALLOCATABLE DUPS(:,:),UDOTPS(:,:),FA(:,:),FC(:),TM(:),DTM(:)
    ALLOCATABLE P0(:,:),P1(:,:),UZR(:)

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
    NBC=IAP(12)
    NINT=IAP(13)
    NUZR=IAP(15)
    ITPST=IAP(28)
    NDX=NDIM*NCOL

    ALLOCATE(UPS(NDX,NTST+1),UOLDPS(NDX,NTST+1))
    ALLOCATE(UPOLDP(NDX,NTST+1),DUPS(NDX,NTST+1))
    ALLOCATE(UDOTPS(NDX,NTST+1),FA(NDX,NTST+1))
    ALLOCATE(FC(NBC+NINT+1),TM(NTST+1),DTM(NTST+1))
    ALLOCATE(P0(NDIM,NDIM),P1(NDIM,NDIM),UZR(NUZR),EV(NDIM))

    CALL SETPBV(IAP,RAP,DTM,NDIM,P0,P1,EV)
    DS=RAP(1)

    RDS=DS
    DSOLD=RDS
    RAP(5)=DSOLD
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
    IAP(34)=ISTOP

    DO I=1,NPARX
       RLCUR(I)=0.d0
       RLOLD(I)=0.d0
       RLDOT(I)=0.d0
    ENDDO

    DO J=1,NTST+1
       DO I=1,NDX
          UPS(I,J)=0.d0
          UOLDPS(I,J)=0.d0
          UPOLDP(I,J)=0.d0
          DUPS(I,J)=0.d0
          UDOTPS(I,J)=0.d0
          FA(I,J)=0.d0
       ENDDO
    ENDDO

    NODIR=0
    CALL RSPTBV(IAP,RAP,PAR,ICP,FUNI,STPNT,RLCUR,RLOLD,RLDOT, &
         NDX,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
    CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)

!     don't set global rotations here for homoclinics, but in autlib5.c
    IF(IPS.NE.9)CALL SETRTN(IAP(23),NTST,NDX,UPS,PAR)

    IF(NODIR.EQ.1 .AND. ISW.GT.0)THEN
       CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT, &
            NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,0,P0,P1,THL,THU)
    ELSEIF(IRS.NE.0 .AND. ISW.LT.0)THEN
       CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT, &
            NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,1,P0,P1,THL,THU)
    ENDIF

! Store plotting data for restart point :

    CALL STHD(IAP,RAP,ICP)
    IF(IRS.EQ.0) THEN
       ITP=9+10*ITPST
    ELSE
       ITP=0
    ENDIF
    IAP(27)=ITP
    ISTOP=0
    IAP(34)=ISTOP
    CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
    CALL STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,THL,THU)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)RETURN

    CALL EXTRBV(IAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS)

    ITP=0
    IAP(27)=ITP
    GOTO 2

1   ITP=0
    IAP(27)=ITP
    NTOT=IAP(32)

! Adapt the mesh to the solution.

    IF(IAD.NE.0)THEN
       IF(MOD(NTOT,IAD).EQ.0)THEN
          CALL ADAPT(IAP,NTST,NCOL,NTST,NCOL,TM,DTM,NDX,UPS,UOLDPS)
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

    CALL CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
         NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
2   CALL STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
         RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC, &
         TM,DTM,P0,P1,THL,THU,NITPS)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 3

! Check for user supplied parameter output parameter-values.

    IF(NUZR.GT.0)THEN
       DO IUZR=1,NUZR
          IAP(26)=IUZR 
          CALL LCSPBV(IAP,RAP,PAR,ICP,FNUZBV,FUNI,BCNI,ICNI,PVLI, &
               UZR(IUZR),RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS, &
               UDOTPS,UPOLDP,FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS)
          ISTOP=IAP(34)
          IF(ISTOP.EQ.1)GOTO 3
          ITP=IAP(27)
          IF(ITP.EQ.-1)THEN
             IF(IUZ(IUZR).GT.0)THEN
                ITP=-4-10*ITPST
                IAP(27)=ITP
                DO K=1,NUZR
                   UZR(K)=0.d0
                ENDDO
             ELSE
                ISTOP=-1
                IAP(34)=ISTOP
             ENDIF
          ENDIF
       ENDDO
    ENDIF

! Check for fold.

    IF(ABS(ILP).GT.0)THEN
       CALL LCSPBV(IAP,RAP,PAR,ICP,FNLPBV,FUNI,BCNI,ICNI,PVLI,RLP, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
            FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 3
       ITP=IAP(27)
       IF(ITP.EQ.-1)THEN
          IF(ILP.GT.0)THEN
             ITP=5+10*ITPST
             IAP(27)=ITP
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
          ELSE
!            *Stop at the first found fold
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
          ENDIF
       ENDIF
    ENDIF

! Check for branch point.

    IF(ABS(ISP)>=2.AND.ABS(ISP)/=4)THEN
       CALL LCSPBV(IAP,RAP,PAR,ICP,FNBPBV,FUNI,BCNI,ICNI,PVLI,BP1, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
            FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 3
       ITP=IAP(27)
       IF(ITP.EQ.-1)THEN
          IF(ISP.GT.0)THEN
             ITP=6+10*ITPST
             IAP(27)=ITP
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
          ELSE
!            *Stop at the first found BP
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
          ENDIF
       ENDIF
    ENDIF

! Check for period-doubling and torus bifurcation.

    IF(ABS(ISP).GT.0 .AND. &
         (IPS.EQ.2.OR.IPS.EQ.7.OR.IPS.EQ.12) )THEN
       CALL LCSPBV(IAP,RAP,PAR,ICP,FNSPBV,FUNI,BCNI,ICNI,PVLI,SP1, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
            FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 3
       ITP=IAP(27)
       IF(ITP.EQ.-1)THEN
          IF(ISP.GT.0)THEN
!            **Secondary periodic bifurcation: determine type
             CALL TPSPBV(IAP,RAP,PAR,EV)
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
          ELSE
!            *Stop at the first found SPB
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
          ENDIF
       ENDIF
    ENDIF

! Store plotting data.

3   CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
    CALL STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,THL,THU)

    ISTOP=IAP(34)
    IF(ISTOP.EQ.0)THEN
       GOTO 1
    ENDIF
    DEALLOCATE(EV,UPS,UOLDPS,UPOLDP,DUPS,UDOTPS,FA,FC,TM,DTM,P0,P1)
    DEALLOCATE(UZR)

  END SUBROUTINE CNRLBV

! ---------- ------
  SUBROUTINE CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
       NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)

    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Determines an initial approximation to the next solution point,
! by extrapolating from the two preceding points.
! The stepsize used in the preceding step has been stored in DSOLD.

    EXTERNAL FUNI

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*)
    DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*),UPOLDP(*),DTM(*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)

    DSOLD=RAP(5)

! Compute rate of change (along branch) of PAR(ICP(1)) and U :

    DDS=1.d0/DSOLD
    NROW=NDIM*NCOL
    DO J=1,NTST+1
       DO I=1,NROW
          UDOTPS(I,J)=(UPS(I,J)-UOLDPS(I,J))*DDS
       ENDDO
    ENDDO
    DO I=1,NFPR
       RLDOT(I)=(RLCUR(I)-RLOLD(I))*DDS
    ENDDO
! Rescale, to set the norm of (UDOTPS,RLDOT) equal to 1.
    CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)

! Extrapolate to get initial approximation to next solution point.

    CALL EXTRBV(IAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS)

! Store time-derivative.

    CALL STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD,NDX,UPS,UOLDPS,UPOLDP)

  END SUBROUTINE CONTBV

! ---------- ------
  SUBROUTINE EXTRBV(IAP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
       NDX,UPS,UOLDPS,UDOTPS)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Determines an initial approximation to the next solution by
! extrapolating from the two preceding points.
! The stepsize used in the preceding step has been stored in DSOLD.
!
    EXTERNAL FUNI

    DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)

    NROW=NDIM*NCOL
    DO I=1,NFPR
       RLOLD(I)=RLCUR(I)
       RLCUR(I)=RLCUR(I)+RDS*RLDOT(I)
    ENDDO
    DO J=1,NTST+1
       DO I=1,NROW
          UOLDPS(I,J)=UPS(I,J)
          UPS(I,J)=UPS(I,J)+RDS*UDOTPS(I,J)
       ENDDO
    ENDDO

  END SUBROUTINE EXTRBV

! ---------- ------
  SUBROUTINE STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD, &
       NDX,UPS,UOLDPS,UPOLDP)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Stores U-prime (derivative with respect to T) in UPOLDP.

    EXTERNAL FUNI

    DIMENSION UPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),IAP(*),RAP(*)
! Local
    ALLOCATABLE U(:),UOLD(:),F(:),DFDU(:),DFDP(:)

    NDIM=IAP(1)
    IPS=IAP(2)
    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)

    DO I=1,NFPR
       PAR(ICP(I))=RLOLD(I)
    ENDDO

    ALLOCATE(U(NDIM),UOLD(NDIM),F(NDIM))
    ALLOCATE(DFDU(NDIM**2),DFDP(NDIM*NPARX))

    DO J=1,NTST+1
       DO I=1,NDIM
          U(I)=UOLDPS(I,J)
          IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
             UOLD(I)=2*UOLDPS(I,J)-UPS(I,J)
          ELSE
             UOLD(I)=UOLDPS(I,J)
          ENDIF
       ENDDO
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,0,F,DFDU,DFDP)
       DO I=1,NDIM
          UPOLDP(I,J)=F(I)
       ENDDO
    ENDDO

    NC1=NCOL-1
    DO K=1,NC1
       N1=K*NDIM
       DO J=1,NTST
          DO I=1,NDIM
             U(I)=UOLDPS(N1+I,J)
             IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
                UOLD(I)=2*UOLDPS(N1+I,J)-UPS(N1+I,J)
             ELSE
                UOLD(I)=UOLDPS(N1+I,J)
             ENDIF
          ENDDO
          CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,0,F,DFDU,DFDP)
          DO I=1,NDIM
             UPOLDP(N1+I,J)=F(I)
          ENDDO
       ENDDO
    ENDDO

    DO I=1,NFPR
       PAR(ICP(I))=RLCUR(I)
    ENDDO

    DEALLOCATE(U,UOLD,F,DFDU,DFDP)
  END SUBROUTINE STUPBV

! ---------- ------
  SUBROUTINE STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
       RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC, &
       TM,DTM,P0,P1,THL,THU,NITPS)

    USE MESH
    USE SOLVEBV
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Controls the solution of the nonlinear equations (by Newton's method)
! for the next solution (PAR(ICP(*)) , U) on a branch of solutions.

    EXTERNAL FUNI,BCNI,ICNI,PVLI

    DIMENSION IAP(*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*),UDOTPS(NDX,*)
    DIMENSION UPOLDP(NDX,*),DUPS(NDX,*),FA(NDX,*),FC(*),TM(*),DTM(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
    DIMENSION P0(*),P1(*)
    LOGICAL DONE

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    NROW=NDIM*NCOL
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

    DELREF=0
1   DSOLD=RDS
    RAP(5)=DSOLD
    NITPS=0

! Write additional output on unit 9 if requested.

    CALL WRTBV9(IAP,RAP,RLCUR,NDX,UPS,TM,DTM,THU,NITPS)

! Generate the Jacobian matrix and the right hand side.

    DO NIT1=1,ITNW

       NITPS=NIT1
       NLLV=0

       IFST=0
       IF(NITPS.LE.NWTN)IFST=1

       CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDS,NLLV, &
            RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC, &
            P0,P1,THL,THU)

! Add Newton increments.

       DO I=1,NDIM
          UPS(I,NTST+1)=UPS(I,NTST+1)+FC(I)
       ENDDO
       DO I=1,NFPR
          RLCUR(I)=RLCUR(I)+FC(NDIM+I)
          PAR(ICP(I))=RLCUR(I)
       ENDDO

       DUMX=0.d0
       UMX=0.d0
       DO J=1,NTST
          DO I=1,NROW
             ADU=ABS(FA(I,J))
             IF(ADU.GT.DUMX)DUMX=ADU
             AU=ABS(UPS(I,J))
             IF(AU.GT.UMX)UMX=AU
             UPS(I,J)=UPS(I,J)+FA(I,J)
          ENDDO
       ENDDO

       CALL WRTBV9(IAP,RAP,RLCUR,NDX,UPS,TM,DTM,THU,NITPS)

! Check whether user-supplied error tolerances have been met :

       DONE=.TRUE.
       RDRL=0.d0
       DO I=1,NFPR
          ADRL=ABS(FC(NDIM+I))/(1.d0+ABS(RLCUR(I)))
          IF(ADRL.GT.EPSL)DONE=.FALSE.
          IF(ADRL.GT.RDRL)RDRL=ADRL
       ENDDO
       RDUMX=DUMX/(1.d0+UMX)
       IF(DONE.AND.RDUMX.LT.EPSU)THEN
          CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
          IF(IID.GE.2)WRITE(9,*)  
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

    IF(IADS.EQ.0)WRITE(9,101)IBR,NTOP
    IF(IADS.EQ.0)GOTO 13

! Reduce stepsize and try again.

    DSMAX=RAP(3)
    NITPS=ITNW
    CALL ADPTDS(NITPS,ITNW,IBR,NTOP,DSMAX,RDS)
    IF(ABS(RDS).LT.DSMIN)GOTO 12
    DO I=1,NFPR
       RLCUR(I)=RLOLD(I)+RDS*RLDOT(I)
    ENDDO
    DO J=1,NTST+1
       DO I=1,NROW
          UPS(I,J)=UOLDPS(I,J)+RDS*UDOTPS(I,J)
       ENDDO
    ENDDO
    IF(IID.GE.2)WRITE(9,102)IBR,NTOP
    GOTO 1

! Minimum stepsize reached.

12  WRITE(9,103)IBR,NTOP
13  DO I=1,NFPR
       RLCUR(I)=RLOLD(I)
       PAR(ICP(I))=RLCUR(I)
    ENDDO
    DO J=1,NTST+1
       DO I=1,NROW
          UPS(I,J)=UOLDPS(I,J)
       ENDDO
    ENDDO
    ISTOP=1
    IAP(34)=ISTOP

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
  SUBROUTINE RSPTBV(IAP,RAP,PAR,ICP,FUNI,STPNT,RLCUR,RLOLD, &
       RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)

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

    DIMENSION IAP(*),RAP(*)
    DIMENSION UPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*),UDOTPS(NDX,*)
    DIMENSION TM(*),DTM(*),PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
    DIMENSION THL(*),THU(*)

    LOGICAL FOUND
    ALLOCATABLE UPSN(:,:),UPOLDN(:,:),UDOTPN(:,:),TMN(:),DTMN(:)

    NDIM=IAP(1)
    IPS=IAP(2)
    IRS=IAP(3)
    NTST=IAP(5)
    NCOL=IAP(6)
    NDM=IAP(23)
    NFPR=IAP(29)

! Get restart data :
!
!     First take a peek at the file to see if ntst, ndim and
!     ncol are different then the values found in
!     the parameter file fort.2.
!
    IF(IRS.GT.0)THEN
       CALL FINDLB(IAP,IRS,NFPRS,FOUND)
       READ(3,*)IBR,NTOTRS,ITPRS,LAB,NFPRS,ISWRS,NTPLRS,NARS,NSKIP, &
            NTSRS,NCOLRS,NPARR
       NTST3=NTSRS
       NCOL3=NCOLRS
       NDIM3=NARS-1
    ELSE
       NTST3=NTST
       NCOL3=NCOL
       NDIM3=NDIM
    ENDIF
       
! use the bigger of the size defined in fort.2 and the one defined in fort.8
    NTSTU=MAX(NTST,NTST3)
    NCOLU=MAX(NCOL,NCOL3)
    NDIMU=NDIM
    NDXLOC=NDIMU*NCOLU
    NTSTCU=(NTSTU+1)*NCOLU

! Autodetect special case when homoclinic branch switching is
! completed and the orbit's representation has to be
! changed.

    IF(IPS.EQ.9.AND.NDIM3.GT.(NDM*2).AND.NDIM3.GT.NDIM)THEN
       NTSTCU=(NTSTU+1)*(NDIM3/NDM)
       NDIMU=NDIM3
       NDXLOC=NDIMU*NCOLU
       IAP(1)=NDIMU
    ENDIF
    ALLOCATE(UPSN(NDXLOC,NTSTCU),UPOLDN(NDXLOC,NTSTCU))
    ALLOCATE(UDOTPN(NDXLOC,NTSTCU),TMN(NTSTCU),DTMN(NTSTCU))
! initialize arrays
    DO I=1,NTSTCU
       DO J=1,NDXLOC
          UPSN(J,I)=0.0d0
          UPOLDN(J,I)=0.0d0
          UDOTPN(J,I)=0.0d0
       ENDDO
    ENDDO

    CALL STPNT(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT, &
         NDXLOC,UPSN,UDOTPN,UPOLDN,TMN,DTMN,NODIR,THL,THU)
    IAP(1)=NDIM

! Determine a suitable starting label and branch number.

    CALL NEWLAB(IAP)

    DO J=1,NTSRS
       DTMN(J)=TMN(J+1)-TMN(J)
    ENDDO

! Adapt mesh if necessary :

    IF( NTST.NE.NTSRS .OR. NCOL.NE.NCOLRS)THEN
       CALL ADAPT(IAP,NTSRS,NCOLRS,NTST,NCOL,TMN,DTMN,NDXLOC, &
            UPSN,UDOTPN)
    ENDIF
! Copy from the temporary large arrays into the normal arrays.
    DO I=1,NTST+1
       DTM(I)=DTMN(I)
       TM(I)=TMN(I)
       DO J=1,NDIM*NCOL
          UPS(J,I)=UPSN(J,I)
          UPOLDP(J,I)=UPOLDN(J,I)
          UDOTPS(J,I)=UDOTPN(J,I)
       ENDDO
    ENDDO
    DEALLOCATE(DTMN,TMN,UPSN,UPOLDN,UDOTPN)

! Set UOLDPS, RLOLD.

    DO I=1,NFPR
       RLCUR(I)=PAR(ICP(I))
       RLOLD(I)=RLCUR(I)
    ENDDO

    NROW=NDIM*NCOL
    DO J=1,NTST+1
       DO I=1,NROW
          UOLDPS(I,J)=UPS(I,J)
       ENDDO
    ENDDO

! Store U-prime (derivative with respect to time or space variable).

    IF(NODIR.EQ.-1)THEN
!      ** Restart from a Hopf bifurcation.
       NODIR=0
       ISW=1
       IAP(10)=ISW
    ELSE
!      ** Restart from orbit.
       CALL STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD,NDX,UPS,UOLDPS,UPOLDP)
    ENDIF

  END SUBROUTINE RSPTBV

! ---------- ------
  SUBROUTINE READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOTRS,UPS, &
       UDOTPS,TM,ITPRS,NDX)

    USE IO
    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(IN) :: NDX
    INTEGER, INTENT(OUT) :: ICPRS(*),NTSRS,NCOLRS,NDIMRD,ITPRS
    DOUBLE PRECISION, INTENT(OUT) :: RLDOTRS(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*)
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*)
! Local
    DOUBLE PRECISION TEMP(7)
    INTEGER NDIM,NRSP1,NDIMRS,NARS,NSKIP1,NSKIP2,I,J,K,K1,K2
    INTEGER IBR,NTOT,LAB,NFPR,ISW,NTPL,NSKIP,NPARR
    LOGICAL EOF3

    NDIM=IAP(1)
    READ(3,*)IBR,NTOT,ITPRS,LAB,NFPR,ISW,NTPL,NARS,NSKIP,NTSRS,NCOLRS,NPARR
    IAP(30)=IBR
    IAP(37)=LAB
    NRSP1=NTSRS+1

    NDIMRS=NARS-1
    NSKIP1=(NDIMRS+1)/8 - NDIM/7
    NSKIP2=(NDIMRS+1)/9 - NDIM/8
    IF(NDIM.LE.NDIMRS)THEN
       NDIMRD=NDIM
    ELSE
       NDIMRD=NDIMRS
    ENDIF

    DO J=1,NTSRS
       DO I=1,NCOLRS
          K1=(I-1)*NDIM+1
          K2=K1+NDIMRD-1
          READ(3,*)TEMP(I),(UPS(K,J),K=K1,K2)
          IF(NSKIP1.GT.0)CALL SKIP3(NSKIP1,EOF3)
       ENDDO
       TM(J)=TEMP(1)
    ENDDO
    READ(3,*)TM(NRSP1),(UPS(K,NRSP1),K=1,NDIMRD)
    IF(NSKIP1.GT.0)CALL SKIP3(NSKIP1,EOF3)

    READ(3,*)(ICPRS(K),K=1,NFPR)
    READ(3,*)(RLDOTRS(K),K=1,NFPR)

! Read U-dot (derivative with respect to arclength).

    DO J=1,NTSRS
       DO I=1,NCOLRS
          K1=(I-1)*NDIM+1
          K2=K1+NDIMRD-1
          READ(3,*)(UDOTPS(K,J),K=K1,K2)
          IF(NSKIP2.GT.0)CALL SKIP3(NSKIP2,EOF3)
       ENDDO
    ENDDO
    READ(3,*)(UDOTPS(K,NRSP1),K=1,NDIMRD)
    IF(NSKIP2.GT.0)CALL SKIP3(NSKIP2,EOF3)

! Read the parameter values.

    IF(NPARR.GT.NPARX)THEN
       NPARR=NPARX
       WRITE(6,100)NPARR
100    FORMAT(' Warning : NPARX too small for restart data : ',/, &
            ' PAR(i) set to zero, for i > ',I3)
    ENDIF
    READ(3,*)(PAR(I),I=1,NPARR)

  END SUBROUTINE READBV

! ---------- ------
  SUBROUTINE STPNBV(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT, &
       NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    DIMENSION IAP(*),RAP(*),UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*),TM(*),DTM(*),THL(*),THU(*)

    CALL STPNBV1(IAP,RAP,PAR,ICP,NTSRS,NDIMRD,NCOLRS,RLCUR,RLDOT, &
         NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)

  END SUBROUTINE STPNBV

! ---------- -------
  SUBROUTINE STPNBV1(IAP,RAP,PAR,ICP,NTSRS,NDIMRD,NCOLRS,RLCUR,RLDOT, &
       NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)

    USE IO
    IMPLICIT NONE

! This subroutine locates and retrieves the information required to
! restart computation at the point with label IRS.
! This information is expected on unit 3.

    INTEGER, INTENT(IN) :: NDX,ICP(*)
    INTEGER, INTENT(INOUT) :: IAP(*)
    INTEGER, INTENT(OUT) :: NTSRS,NDIMRD,NCOLRS,NODIR
    DOUBLE PRECISION, INTENT(INOUT) :: RAP(*)
    DOUBLE PRECISION, INTENT(OUT) :: UPS(NDX,*),UDOTPS(NDX,*),TM(*)
    DOUBLE PRECISION, INTENT(IN) :: UPOLDP(NDX,*),DTM(*),THL(*),THU(*)
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLCUR(*),RLDOT(*)
! Local
    INTEGER ICPRS(NPARX),IRS,NFPR,NFPRS,ITPRS,I

    LOGICAL FOUND

    IRS=IAP(3)
    NFPR=IAP(29)

    CALL FINDLB(IAP,IRS,NFPRS,FOUND)
    CALL READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOT,UPS, &
         UDOTPS,TM,ITPRS,NDX)

    DO I=1,NFPR
       RLCUR(I)=PAR(ICP(I))
    ENDDO

! Take care of the case where the free parameters have been changed at
! the restart point.

    NODIR=0
    IF(NFPRS.NE.NFPR)THEN
       NODIR=1
       RETURN
    ENDIF
    DO I=1,NFPR
       IF(ICPRS(I).NE.ICP(I)) THEN
          NODIR=1
          RETURN
       ENDIF
    ENDDO

  END SUBROUTINE STPNBV1

! ---------- ------
  SUBROUTINE STPNUB(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT, &
       NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)

    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Generates a starting point for the continuation of a branch of
! of solutions to general boundary value problems by calling the user
! supplied subroutine STPNT where an analytical solution is given.

    DIMENSION IAP(*),RAP(*),UPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*),TM(*),DTM(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*),THL(*),THU(*)
! Local
    ALLOCATABLE U(:)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)
    ALLOCATE(U(NDIM))

! Generate the (initially uniform) mesh.

    CALL MSH(NTST,TM)
    DT=1.d0/(NTST*NCOL)

    DO J=1,NTST+1
       IF(J.EQ.(NTST+1)) THEN
          NCOL1=1
       ELSE
          NCOL1=NCOL
       ENDIF
       DO I=1,NCOL1
          T=TM(J)+(I-1)*DT
          K1=(I-1)*NDIM+1
          K2=I*NDIM
          CALL STPNT(NDIM,U,PAR,T)
          DO K=K1,K2
             UPS(K,J)=U(K-K1+1)
          ENDDO
       ENDDO
    ENDDO

    NTSRS=NTST
    NCOLRS=NCOL
    IBR=1
    IAP(30)=IBR
    LAB=0
    IAP(37)=LAB

    DO I=1,NFPR
       RLCUR(I)=PAR(ICP(I))
    ENDDO

    NODIR=1

    DEALLOCATE(U)
  END SUBROUTINE STPNUB

! ---------- ------
  SUBROUTINE SETRTN(NDM,NTST,NDX,UPS,PAR)

    USE SUPPORT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Initialization for rotations
    
    POINTER NRTN(:)
    COMMON /BLRTN/ NRTN,IRTN
    DIMENSION UPS(NDX,*),PAR(*)

    ALLOCATE(NRTN(NDM))
    IRTN=0
    DO I=1,NDM
       NRTN(I)=NINT( (UPS(I,NTST+1)-UPS(I,1)) / PI(2.d0) )
       IF(NRTN(I).NE.0)THEN
          PAR(19)=PI(2.d0)
          IRTN=1
       ENDIF
    ENDDO
    IF(IRTN.EQ.0)DEALLOCATE(NRTN)

  END SUBROUTINE SETRTN

! ---------- ------
  SUBROUTINE STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD, &
       RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,IPERP, &
       P0,P1,THL,THU)

    USE MESH
    USE SOLVEBV
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Generates a direction vector (UDOTPS,RLDOT) that is needed to start
! the computation of a branch when no direction vector is given.

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),RAP(*),UDOTPS(NDX,*),FA(NDX,*),FC(*),DTM(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
    DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
    DOUBLE PRECISION UPOLDP(NDX,*),P0(*),P1(*)

! Generate the Jacobian matrix with zero direction vector.
! (Then the last row of the Jacobian will be zero)
! in case the starting direction is to be determined.

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    IID=IAP(18)
    NFPR=IAP(29)

    NROW=NDIM*NCOL
    IF(IPERP.EQ.0)THEN
       DO J=1,NTST+1
          DO I=1,NROW
             UDOTPS(I,J)=0.d0
          ENDDO
       ENDDO
       DO I=1,NFPR
          RLDOT(I)=0.d0
       ENDDO
    ENDIF

    RDSZ=0.d0
    NLLV=1
    IFST=1
    CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
         RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC, &
         P0,P1,THL,THU)

! Compute the starting direction.

    DO I=1,NDIM
       UDOTPS(I,NTST+1)=FC(I)
    ENDDO
    DO I=1,NFPR
       RLDOT(I)=FC(NDIM+I)
       PAR(ICP(I))=RLCUR(I)
    ENDDO

    DO J=1,NTST
       DO I=1,NROW
          UDOTPS(I,J)=FA(I,J)
       ENDDO
    ENDDO

! Scale the starting direction.

    CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)

! Make sure that RLDOT(1) is positive (unless zero).

    IF(RLDOT(1).LT.0.d0)THEN
       DO I=1,NFPR
          RLDOT(I)=-RLDOT(I)
       ENDDO
       DO J=1,NTST+1
          DO I=1,NROW
             UDOTPS(I,J)=-UDOTPS(I,J)
          ENDDO
       ENDDO
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
  SUBROUTINE LCSPBV(IAP,RAP,PAR,ICP,FNCS,FUNI,BCNI,ICNI,PVLI,Q, &
       RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC, &
       TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ,NITPS)

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

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),TM(*),DTM(*),FA(*),FC(*)
    DIMENSION UPS(*),UDOTPS(*),UOLDPS(*),UPOLDP(*),DUPS(*),IUZ(*),VUZ(*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*),P0(*),P1(*)

    IID=IAP(18)
    ITMX=IAP(19)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    DS=RAP(1)
    DSMAX=RAP(3)
    DSOLD=RAP(5)
    EPSS=RAP(13)

! Check for zero.

    Q0=Q
    Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
         RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS, &
         TM,DTM,THL,THU,IUZ,VUZ)

    PQ=Q0*Q1
    IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
       Q=Q1
       RETURN
    ENDIF

! Use the secant method for the first step:

    S0=0.d0
    S1=DSOLD
    NITSP1=0
    DQ=Q0-Q1
    RDS=Q1/DQ*(S1-S0)
1   RDS=(1.d0+HMACH)*RDS
    S=S1+RDS

! Return if tolerance has been met :

    RRDS=ABS(RDS)/(1+DSQRT(ABS(DS*DSMAX)))
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

    CALL CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT, &
         NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
    CALL STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS, &
         RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP, &
         FA,FC,TM,DTM,P0,P1,THL,THU,NITPS)
    ISTOP=IAP(34)
    IF(ISTOP.NE.0)THEN
       Q=0.d0
       RETURN
    ENDIF

! Check for zero.

    Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, &
         RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS, &
         TM,DTM,THL,THU,IUZ,VUZ)

    NITSP1=NITSP1+1
    IF(NITSP1.LE.ITMX)THEN
!        Use Mueller's method with bracketing for subsequent steps
       CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
       GOTO 1
    ENDIF

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
       NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)

    USE MESH
    USE SOLVEBV
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! RETURNS A QUANTITY THAT CHANGES SIGN AT A LIMIT POINT (BVP)

    COMPLEX(KIND(1.0D0)) EV(*)

    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),UDOTPS(NDX,*),FA(NDX,*)
    DIMENSION FC(*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),TM(*),DTM(*),THL(*),THU(*)
    DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
    DOUBLE PRECISION UPOLDP(NDX,*),P0(*),P1(*)

    NDIM=IAP(1)
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

    CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV, &
         RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC, &
         P0,P1,THL,THU)

    DO I=1,NDIM
       UDOTPS(I,NTST+1)=FC(I)
    ENDDO

    DO I=1,NFPR
       RLDOT(I)=FC(NDIM+I)
    ENDDO

    NROW=NDIM*NCOL
    DO J=1,NTST
       DO I=1,NROW
          UDOTPS(I,J)=FA(I,J)
       ENDDO
    ENDDO

! Scale the direction vector.

    CALL SCALEB(IAP,NDIM,NDX,UDOTPS,RLDOT,DTM,THL,THU)
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
       NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)

    USE SUPPORT

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    COMPLEX(KIND(1.0D0)) EV(*)

    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    DIMENSION IAP(*),RAP(*),P1(*)
! Local
    ALLOCATABLE IR(:),IC(:),PP(:)
    DOUBLE PRECISION U(1),F(1)

    NDIM=IAP(1)
    IID=IAP(18)

! Save the determinant of the reduced system.

    DET=RAP(14)
    DET0=DET
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

! Compute the determinant of P1.

    ALLOCATE(IR(NDIM),IC(NDIM),PP(NDIM**2))
    DO I=1,NDIM**2
       PP(I)=P1(I)
    ENDDO
    CALL GE(0,NDIM,NDIM,PP,0,1,U,1,F,IR,IC,DET)
    DEALLOCATE(IR,IC,PP)
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
       NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)

    USE FLOQUET

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

! This function returns a quantity that changes sign when a complex
! pair of eigenvalues of the linearized Poincare map moves in or out
! of the unit circle or when a real eigenvalues passes through -1.

    COMPLEX(KIND(1.0D0)) EV(*),ZTMP
    DIMENSION IAP(*),RAP(*),P0(*),P1(*)
! Local
    LOGICAL CHNG

    EXTERNAL FUNI,BCNI,ICNI

    NDIM=IAP(1)
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
    IF(AMIN>5.0E-2 .AND. (ISP==2 .OR. ISP==4)) THEN
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
       NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)

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
  SUBROUTINE STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,THL,THU)

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

    DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),TM(*),DTM(*),UPS(*),THL(*),THU(*)
    DIMENSION RLDOT(*),UDOTPS(*)
! Local
    DIMENSION UMX(7)

    NDIM=IAP(1)
    IPS=IAP(2)
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
         AMP=DSQRT(RNRMSQ(IAP,NDM,NDX,UPS,DTM,THU))
    IF(IPLT.GT.0.AND.IAB.LE.NDM)AMP=RMXUPS(IAP,NDX,IAB,UPS)
    IF(IPLT.GT.NDM.AND.IAB.LE.2*NDM) &
         AMP=RINTG(IAP,NDX,IAB-NDM,UPS,DTM)
    IF(IPLT.GT.2*NDM.AND.IAB.LE.3*NDM) &
         AMP=RNRM2(IAP,NDX,IAB-2*NDM,UPS,DTM)
    IF(IPLT.LT.0.AND.IAB.LE.NDM)AMP=RMNUPS(IAP,NDX,IAB,UPS)

    RAP(10)=AMP

    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)THEN
!      ** Maximum number of iterations reached somewhere.
       ITP=-9-10*ITPST
       IAP(27)=ITP
    ELSEIF(ISTOP.EQ.-1)THEN
!      ** UZR endpoint
       ITP=9+10*ITPST
       IAP(27)=ITP
    ELSE
       IF(PAR(ICP(1)).LT.RL0.OR.PAR(ICP(1)).GT.RL1 &
            .OR. AMP.LT.A0.OR.AMP.GT.A1 .OR. NTOT.GE.NMX)THEN
          ISTOP=1
          IAP(34)=ISTOP
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
       UMX(I)=RMXUPS(IAP,NDX,ITMP,UPS)
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
    CALL WRLINE(IAP,PAR,ICP(NPARX+1),IBRS,NTOTS,LABW,AMP,UMX)

! Write plotting and restart data on unit 8.

    IF(MOD(ITP,10).NE.0)THEN
       CALL WRTBV8(IAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM)
    ENDIF

  END SUBROUTINE STPLBV

! ---------- ------
  SUBROUTINE WRTBV8(IAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM)

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
!  NPARX : The dimension of the array PAR.
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
! Finally the parameter values PAR(i) , i=1,NPARX, are written.
!
!  Above, RL-dot(.) and U-dot(.) specify the direction of the branch.

    DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
    DIMENSION PAR(*),ICP(*),RLDOT(*)
!xxx====================================================================
!xxx Test problem: compute the error
    err(x,t)=x - 2*DATAN(1.d0)*PAR(2)*DSIN(4*DATAN(1.d0)*t)
!xxx====================================================================

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    ISW=IAP(10)
    ITP=IAP(27)
    NFPR=IAP(29)
    IBR=IAP(30)
    NTOT=IAP(32)
    LAB=IAP(37)

! Write information identifying the solution :

    NTPL=NCOL*NTST+1
    NAR=NDIM+1
    NRD=2+NDIM/7+(NDIM-1)/7
    NROWPR=NRD*(NCOL*NTST+1) + (NFPR-1)/7+1 + (NPARX-1)/7+1 &
                             + (NFPR-1)/20+1

    MTOT=MOD(NTOT-1,9999)+1
    WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPARX

! Write the entire solution on unit 8 :

!xxx====================================================================
!xxx Test problem
    eg=0.d0
    em=0.d0
!xxx====================================================================
    DO J=1,NTST
       RN=1.d0/NCOL
       DO I=1,NCOL
          K1=(I-1)*NDIM+1
          K2=I*NDIM
          T=TM(J)+(I-1)*RN*DTM(J)
          WRITE(8,102)T,(UPS(K,J),K=K1,K2)
!xxx====================================================================
!xxx Test problem
          er = err(ups(k1,j),T)
          if(dabs(er).gt.eg)eg=dabs(er)
          if(i.eq.1 .and. dabs(er).gt.em)em=dabs(er)
!xxx====================================================================
       ENDDO
    ENDDO
!xxx====================================================================
!xxx Test problem
! Write global error and mesh error
!xxx       write(10,100)ncol,ntst,eg,em
!xxx 100   FORMAT(4X,I2,I4,7ES11.3)
!xxx====================================================================
    WRITE(8,102)TM(NTST+1),(UPS(I,NTST+1),I=1,NDIM)

! Write the free parameter indices:

    WRITE(8,103)(ICP(I),I=1,NFPR)

! Write the direction of the branch:

    WRITE(8,102)(RLDOT(I),I=1,NFPR)
    DO J=1,NTST
       DO I=1,NCOL
          K1=(I-1)*NDIM+1
          K2=I*NDIM
          WRITE(8,102)(UDOTPS(K,J),K=K1,K2)
       ENDDO
    ENDDO
    WRITE(8,102)(UDOTPS(K,NTST+1),K=1,NDIM)

! Write the parameter values.

    WRITE(8,102)(PAR(I),I=1,NPARX)

101 FORMAT(6I6,I8,I6,I8,3I5)
102 FORMAT(4X,7ES19.10)
103 FORMAT(20I5)

    CALL FLUSH(8)
  END SUBROUTINE WRTBV8

! ---------- ------
  SUBROUTINE WRTBV9(IAP,RAP,RLCUR,NDX,UPS,TM,DTM,THU,NITPS)

    USE IO
    USE MESH
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Writes additional output on unit 9.

    DIMENSION IAP(*),RAP(*)
    DIMENSION DTM(*),UPS(NDX,*),TM(*),RLCUR(*),THU(*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    IPLT=IAP(11)
    IID=IAP(18)
    NDM=IAP(23)
    IBR=IAP(30)
    NTOT=IAP(32)

    IAB=ABS(IPLT)
    IF(IAB.EQ.0.OR.IAB.GT.NDIM)AMP=DSQRT(RNRMSQ(IAP,NDM,NDX,UPS,DTM,THU))
    IF(IPLT.GT.0.AND.IAB.LE.NDIM)AMP=RMXUPS(IAP,NDX,IAB,UPS)
    IF(IPLT.LT.0.AND.IAB.LE.NDIM)AMP=RMNUPS(IAP,NDX,IAB,UPS)
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
       DO J=1,NTST
          RN=1.d0/NCOL
          DO I=1,NCOL
             T=TM(J)+(I-1)*RN*DTM(J)
             K1=(I-1)*NDIM+1
             K2=I*NDIM
             WRITE(9,105)T,(UPS(K,J),K=K1,K2)
          ENDDO
       ENDDO
       WRITE(9,105)TM(NTST+1),(UPS(I,NTST+1),I=1,NDIM)
    ENDIF
102 FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
103 FORMAT(I4,I6,I4,5X,6ES14.5)
104 FORMAT(' UPS :')
105 FORMAT(1X,7ES14.5)

  END SUBROUTINE WRTBV9

! ---------- ------
  SUBROUTINE PVLSBV(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION IAP(*),RAP(*),ICP(*),DTM(*),UPS(NDX,*),PAR(*)
    DIMENSION P0(NDIM,*),P1(NDIM,*)

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
    ALLOCATABLE IR(:), IC(:), IV1(:), FV1(:)

    ALLOCATE(Q0(NDIM,NDIM), Q1(NDIM,NDIM), P(NDIM,NDIM))
    ALLOCATE(Z(NDIM,NDIM), WR(NDIM), WI(NDIM))
    ALLOCATE(IR(NDIM), IC(NDIM))
    ALLOCATE(IV1(NDIM), FV1(NDIM))

    DO I=1,NDIM
       DO J=1,NDIM
          Q0(I,J)=-P0(I,J)
          Q1(I,J)= P1(I,J)
       ENDDO
    ENDDO

    CALL GE(0,NDIM,NDIM,Q1,NDIM,NDIM,P,NDIM,Q0,IR,IC,DET)
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

    DEALLOCATE(Q0,Q1,P,Z,WR,WI,IR,IC,IV1,FV1)
  END SUBROUTINE EVECS

! ---------- ------
  SUBROUTINE SETPBV(IAP,RAP,DTM,NDIM,P0,P1,EV)

    USE SUPPORT
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    TARGET IAP(NIAP),RAP(NRAP),DTM(IAP(5)+1)
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
