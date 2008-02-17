!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Algebraic Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE AE

  PRIVATE
  PUBLIC :: AUTOAE,STPNUS,STPNAE
  INTEGER NPARX,NBIFX,NIAP,NRAP
  INCLUDE 'auto.h'

CONTAINS

! ---------- ------
  SUBROUTINE AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)

! This is the entry subroutine for algebraic systems.

    IMPLICIT NONE

    INTEGER IAP(*),ICP(*),IUZ(*)
    DOUBLE PRECISION RAP(*),PAR(*),THL(*),THU(*),VUZ(*)

    EXTERNAL FUNI,STPNT

    IF(IAP(38)>0)THEN
       CALL MPIWFI(.FALSE.,FUNI,STPNT)
       RETURN
    ENDIF
    CALL CNRLAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)

  END SUBROUTINE AUTOAE

! ---------- ------
  SUBROUTINE CNRLAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Controls the bifurcation analysis of algebraic problems

    EXTERNAL FUNI,STPNT

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),IUZ(*),VUZ(*),THL(*),THU(*)
! Local
    DIMENSION RLCUR(NPARX),RLOLD(NPARX),RLDOT(NPARX)
    ALLOCATABLE AA(:,:),RHS(:),U(:),DU(:),UDOT(:),UOLD(:),STUD(:,:)
    ALLOCATABLE STU(:,:),STLA(:),STLD(:),F(:),DFDU(:),DFDP(:),UZR(:)

    NDIM=IAP(1)
    IPS=IAP(2)
    IRS=IAP(3)
    ILP=IAP(4)
    IADS=IAP(8)
    ISP=IAP(9)
    NUZR=IAP(15)
    MXBF=IAP(17)
    ITPST=IAP(28)
    IBR=IAP(30)

    DS=RAP(1)

    ALLOCATE(AA(NDIM+1,NDIM+1),RHS(NDIM+1),U(NDIM),DU(NDIM+1))
    ALLOCATE(UDOT(NDIM),UOLD(NDIM),STUD(NBIFX,NDIM),STU(NBIFX,NDIM))
    ALLOCATE(STLA(NBIFX),STLD(NBIFX),F(NDIM),DFDU(NDIM**2))
    ALLOCATE(DFDP(NDIM*NPARX),UZR(NUZR))

    NINS=0
    IAP(33)=NINS
    RBP=0.d0
    REV=0.d0
    RLP=0.d0
    IF(NUZR.GT.0)THEN
       DO I=1,NUZR
          UZR(I)=0.d0
       ENDDO
    ENDIF
    RDS=DS
    DSOLD=DS
    RAP(5)=DSOLD
    NIT=0
    IAP(31)=NIT
    NBIF=0
    IAP(35)=NBIF
    NBFC=0
    IPOS=1
    IAP(36)=IPOS
    NTOT=0
    IAP(32)=NTOT
    LAB=0
    IAP(37)=LAB

    DO I=1,NDIM
       U(I)=0.d0
       DU(I)=0.d0
       UDOT(I)=0.d0
       UOLD(I)=0.d0
       F(I)=0.d0
    ENDDO

! Generate the starting point

    CALL STPNT(IAP,RAP,PAR,ICP,U)
    CALL PVLSAE(IAP,RAP,U,PAR)

! Determine a suitable starting label and branch number

    CALL NEWLAB(IAP)

! Write constants

    CALL STHD(IAP,RAP,ICP)

! Write plotting data for the starting point

    ISTOP=0
    IAP(34)=ISTOP
    IF(IRS.EQ.0) THEN
       ITP=9+10*ITPST
    ELSE
       ITP=0
    ENDIF
    IAP(27)=ITP
    RLCUR(1)=PAR(ICP(1))
    CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 6

! Starting procedure  (to get second point on first branch) :

    CALL STPRAE(IAP,RAP,PAR,ICP,FUNI,RDS,NDIM+1,AA,RHS, &
         RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 5
    ITP=0
    IAP(27)=ITP
    GOTO 3

! Initialize computation of the next bifurcating branch.

 2  CALL SWPNT(IAP,RAP,PAR,ICP,RDS,NBIFX,STUD,STU,STLA,STLD, &
         RLCUR,RLDOT,U,UDOT)

    IPOS=IAP(36)
    IF(IPOS.EQ.1)THEN
       NBIF=NBIF-1
       IAP(35)=NBIF
       NBFC=NBFC+1
    ENDIF

    RBP=0.d0
    REV=0.d0
    RLP=0.d0
    IF(NUZR.GT.0)THEN
       DO I=1,NUZR
          UZR(I)=0.d0
       ENDDO
    ENDIF
    IF(IPOS.EQ.0 .OR. MXBF.LT.0 )IBR=IBR+1
    IAP(30)=IBR

    NTOT=0
    IAP(32)=NTOT
    ISTOP=0
    IAP(34)=ISTOP
    ITP=0
    IAP(27)=ITP
    NIT=0
    IAP(31)=NIT
    DSOLD=RDS
    RAP(5)=DSOLD

! Store plotting data for first point on the bifurcating branch

    CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 6

! Determine the second point on the bifurcating branch

    CALL SWPRC(IAP,RAP,PAR,ICP,FUNI,NDIM+1,AA,RHS,RLCUR,RLOLD,RLDOT, &
         U,DU,UOLD,UDOT,F,DFDU,DFDP,RDS,THL,THU)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 5

! Store plotting data for second point :

    CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 6
    RBP=0.d0
    REV=0.d0
    RLP=0.d0

! Provide initial approximation to the next point on the branch

3   CALL CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)

! Find the next solution point on the branch

    CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,NDIM+1,AA,RHS, &
         RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)GOTO 5

! Check for user supplied parameter output parameter-values.

    IF(NUZR.GT.0)THEN
       DO IUZR=1,NUZR
          IAP(26)=IUZR
          CALL LCSPAE(IAP,RAP,PAR,ICP,FNUZAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
               RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,UZR(IUZR),THL,THU, &
               IUZ,VUZ)
          ISTOP=IAP(34)
          IF(ISTOP.EQ.1)GOTO 5
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
                ! NOTE: Fix (February 2005)
                GOTO 5
             ENDIF
          ENDIF
       ENDDO
    ENDIF

! Check for fold

    IF(ABS(ILP).GT.0)THEN
       CALL LCSPAE(IAP,RAP,PAR,ICP,FNLPAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
            RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RLP,THL,THU,IUZ,VUZ)
       ITP=IAP(27)
       IF(ITP.EQ.-1) THEN
          IF(ILP.GT.0)THEN
             ITP=2+10*ITPST
             IAP(27)=ITP
             RLP=0.d0
             RBP=0.d0
             REV=0.d0
          ELSE
!            *Stop at the first found fold
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 5
          ENDIF
       ENDIF
    ENDIF
!
! Check for branch point, and if so store data :
!
    IF(ABS(ISP).GT.0)THEN
       CALL LCSPAE(IAP,RAP,PAR,ICP,FNBPAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
            RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RBP,THL,THU,IUZ,VUZ)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 5
       ITP=IAP(27)
       IF(ITP.EQ.-1)THEN
          IF(ISP.GT.0)THEN
             ITP=1+10*ITPST
             IAP(27)=ITP
             NBIF=NBIF+1
             IAP(35)=NBIF
             CALL STBIF(IAP,ICP,NDIM+1,AA,NBIFX,STUD,STU,STLA, &
                  STLD,RLCUR,RLDOT,U,DU,UDOT,DFDU,DFDP,THL,THU)
             RLP=0.d0
             RBP=0.d0
             REV=0.d0
          ELSE
!            *Stop at the first found BP
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 5
          ENDIF
       ENDIF
    ENDIF

! Check for Hopf bifurcation

    IF(ABS(IPS).EQ.1)THEN
       CALL LCSPAE(IAP,RAP,PAR,ICP,FNHBAE,FUNI,NDIM+1,AA,RHS,RLCUR, &
            RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,REV,THL,THU,IUZ,VUZ)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 5
       ITP=IAP(27)
       IF(ITP.EQ.-1)THEN
          ITP=3+10*ITPST
          IAP(27)=ITP
          REV=0.d0
       ENDIF
    ENDIF

! Store plotting data on unit 7 :

5   CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)

! Adapt the stepsize along the branch

    ITP=IAP(27)
    NTOT=IAP(32)
    IF(IADS.NE.0 .AND. MOD(NTOT,IADS).EQ.0 &
         .AND. ( MOD(ITP,10).EQ.0 .OR. MOD(ITP,10).EQ.4) )THEN
       CALL ADPTDS(IAP,RAP,RDS)
    ENDIF

6   ITP=0
    IAP(27)=ITP
    ISTOP=IAP(34)
    IF(ISTOP.EQ.0)GOTO 3

    NBIF=IAP(35)
    IF(NBIF.NE.0 .AND. NBFC.LT.ABS(MXBF))GOTO 2

    DEALLOCATE(AA,RHS,U,DU,UDOT,UOLD,STUD,STU,STLA,STLD,F,DFDU,DFDP)
    DEALLOCATE(UZR)
  END SUBROUTINE CNRLAE

! ---------- ------
  SUBROUTINE STPNUS(IAP,RAP,PAR,ICP,U)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Gets the starting data from user supplied STPNT

    DIMENSION IAP(*)

    NDIM=IAP(1)

    CALL STPNT(NDIM,U,PAR,T)
    
  END SUBROUTINE STPNUS

! ---------- ------
  SUBROUTINE STPNAE(IAP,RAP,PAR,ICP,U)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    LOGICAL FOUND

! Gets the starting data from unit 3
    DIMENSION IAP(*)

    IRS=IAP(3)
    CALL FINDLB(IAP,IRS,NFPRS,FOUND)
    CALL READLB(IAP,U,PAR)

  END SUBROUTINE STPNAE

! ---------- ------
  SUBROUTINE STPRAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
     RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Finds the second point on the initial solution branch.

    EXTERNAL FUNI

    DIMENSION IAP(*),RAP(*),AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DU(*)
    DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)

! Local
    ALLOCATABLE IR(:),IC(:)

    NDIM=IAP(1)
    IID=IAP(18)

    RLOLD(1)=PAR(ICP(1))
    DO I=1,NDIM
       UOLD(I)=U(I)
    ENDDO

! Determine the direction of the branch at the starting point

    CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
    DO I=1,NDIM
       RHS(I)=F(I)
       AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
       AA(NDIM+1,I)=0.d0
       DO K=1,NDIM
          AA(I,K)=DFDU((K-1)*NDIM+I)
       ENDDO
    ENDDO
    RHS(NDIM+1)=0.d0
    AA(NDIM+1,NDIM+1)=0.d0

    IF(IID.GE.3)CALL WRJAC(NDIM+1,M1AA,AA,RHS)
    ALLOCATE(IR(NDIM+1),IC(NDIM+1))
    CALL NLVC(NDIM+1,M1AA,1,AA,DU,IR,IC)
    DEALLOCATE(IR,IC)

! Scale and make sure that the PAR(ICP(1))-dot is positive.

    SS=0.d0
    DO I=1,NDIM
       SS=SS+THU(I)*DU(I)**2
    ENDDO
    SS=SS+THL(1)*DU(NDIM+1)**2

    SIGN=1.d0
    IF(DU(NDIM+1).LT.0.d0)SIGN=-1.d0
    SC=SIGN/DSQRT(SS)
    DO I=1,NDIM+1
       DU(I)=SC*DU(I)
    ENDDO

    DO I=1,NDIM
       UDOT(I)=DU(I)
    ENDDO
    RLDOT(1)=DU(NDIM+1)

! Set initial approximations to the second point on the branch

    DO I=1,NDIM
       U(I)=UOLD(I)+RDS*UDOT(I)
    ENDDO
    RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)

    CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
         RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)

  END SUBROUTINE STPRAE

! ---------- ------
  SUBROUTINE CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! This subroutine determines an initial approximation to the next
! solution on a branch by extrapolating from the two preceding points.
! The step used in the preceding step has been stored in DSOLD.

    DIMENSION IAP(*),RAP(*),UOLD(*),U(*),UDOT(*)
    DIMENSION RLCUR(*),RLOLD(*),RLDOT(*)

    NDIM=IAP(1)
    IPS=IAP(2)

    DSOLD=RAP(5)

    RLDOT(1)=(RLCUR(1)-RLOLD(1))/DSOLD
    DO I=1,NDIM
       UDOT(I)=(U(I)-UOLD(I))/DSOLD
    ENDDO

    RLOLD(1)=RLCUR(1)
    RLCUR(1)=RLCUR(1)+RDS*RLDOT(1)
    DO I=1,NDIM
       UOLD(I)=U(I)
       U(I)=U(I)+UDOT(I)*RDS
    ENDDO
!      Save old time for time integration
    IF(IPS.EQ.-2)RAP(15)=RLOLD(1)

  END SUBROUTINE CONTAE

! ---------- -----
  SUBROUTINE SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
       RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! This is the subroutine for computing solution branches. It solves
! the equations for finding the next point on the branch at distance DS
! from the current point. An initial approximation to the new point
! ( i.e. to PAR(ICP(1)) and U ) has been supplied by CONT.

    EXTERNAL FUNI

    DIMENSION IAP(*),RAP(*)
    DIMENSION AA(M1AA,*),RHS(*),U(*),DU(*),UOLD(*),UDOT(*) 
    DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
! Local
    ALLOCATABLE IR(:),IC(:)

    NDIM=IAP(1)
    IADS=IAP(8)
    IID=IAP(18)
    ITNW=IAP(20)
    NDM=IAP(23)
    IBR=IAP(30)

    DSMIN=RAP(2)
    EPSL=RAP(11)
    EPSU=RAP(12)

    DELREF=0
1   DSOLD=RDS
    RAP(5)=DSOLD
    DDS=1.d0/RDS
    NIT=0
    IAP(31)=NIT
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1
    IF(IID.GE.2)THEN
       IF(NIT.EQ.0)THEN
          CALL WRBAR("=",47)
          WRITE(9,100)
       ENDIF
       WRITE(9,101)IBR,NTOP+1,NIT,RLCUR(1),RNRMV(NDM,U)
    ENDIF
100 FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
101 FORMAT(I4,I6,I4,5X,2ES14.5)

! Call user-supplied FUNC to evaluate the right hand side of the
! differential equation and its derivatives :

    DO NIT1=1,ITNW

       NIT=NIT1
       IAP(31)=NIT
       PAR(ICP(1))=RLCUR(1)
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)

! Set up the Jacobian matrix and the right hand side :

       DO I=1,NDIM
          AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
          RHS(I)=-F(I)
          DO K=1,NDIM
             AA(I,K)=DFDU((K-1)*NDIM+I)
          ENDDO
       ENDDO
       DO K=1,NDIM
          AA(NDIM+1,K)=2.d0*THU(K)*(U(K)-UOLD(K))*DDS
       ENDDO
       AA(NDIM+1,NDIM+1)=2.d0*THL(1)*(RLCUR(1)-RLOLD(1))*DDS
       SS=0.d0
       DO I=1,NDIM
          SS=SS+THU(I)*(U(I)-UOLD(I))**2
       ENDDO
       RHS(NDIM+1)=RDS-DDS*SS-THL(1)*DDS*(RLCUR(1)-RLOLD(1))**2

! Use Gauss elimination with pivoting to solve the linearized system :

       IF(IID.GE.5)CALL WRJAC(NDIM+1,M1AA,AA,RHS)
       ALLOCATE(IR(NDIM+1),IC(NDIM+1))
       CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,DU,NDIM+1, &
            RHS,IR,IC,DET)
       DEALLOCATE(IR,IC)
       RAP(14)=DET
       DRLM=DU(NDIM+1)

! Add the Newton increments :

       DO I=1,NDIM
          U(I)=U(I)+DU(I)
       ENDDO
       RLCUR(1)=RLCUR(1)+DRLM
       DUMX=0.d0
       UMX=0.d0
       DO I=1,NDIM
          ADU=ABS(DU(I))
          AU=ABS(U(I))
          IF(AU.GT.UMX)UMX=AU
          IF(ADU.GT.DUMX)DUMX=ADU
       ENDDO

       IF(IID.GE.2)THEN
          WRITE(9,101)IBR,NTOP+1,NIT,RLCUR(1),RNRMV(NDM,U)
       ENDIF

       RDRLM= ABS(DRLM)/(1.d0+ ABS(RLCUR(1)))
       RDUMX=DUMX/(1.d0+UMX)
       IF(RDRLM.LE.EPSL.AND.RDUMX.LE.EPSU)THEN
          CALL PVLSAE(IAP,RAP,U,PAR)
          IF(IID.GE.2)WRITE(9,*)
          RETURN
       ENDIF

! Check whether relative error has reached user-supplied tolerance :

       IF(NIT.EQ.1)THEN
          DELREF=20*DMAX1(RDRLM,RDUMX)
       ELSE
          DELMAX=DMAX1(RDRLM,RDUMX)
          IF(DELMAX.GT.DELREF)EXIT
       ENDIF

    ENDDO

! Maximum number of iterations has been reached

    IF(IADS.EQ.0)WRITE(9,102)IBR,NTOP
102 FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
    IF(IADS.EQ.0)GOTO 5

! Reduce stepsize and try again

    MXT=ITNW
    IAP(31)=MXT
    CALL ADPTDS(IAP,RAP,RDS)
    IF(ABS(RDS).LT.DSMIN)GOTO 4
    RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
    DO I=1,NDIM
       U(I)=UOLD(I)+RDS*UDOT(I)
    ENDDO
    IF(IID.GE.2)WRITE(9,103)
103 FORMAT(I4,I6,' NOTE:Retrying step')
    GOTO 1

! Minimum stepsize reached

4   WRITE(9,104)IBR,NTOP
104 FORMAT(I4,I6,' NOTE:No convergence using minimum step size')
5   RLCUR(1)=RLOLD(1)
    PAR(ICP(1))=RLCUR(1)
    DO I=1,NDIM
       U(I)=UOLD(I)
    ENDDO
    ISTOP=1
    IAP(34)=ISTOP
  END SUBROUTINE SOLVAE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!               Detection of Singular Points
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! ---------- ------
  SUBROUTINE LCSPAE(IAP,RAP,PAR,ICP,FNCS,FUNI,M1AA,AA,RHS,RLCUR, &
       RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,Q,THL,THU,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

! This subroutine uses the secant method to accurately locate special
! points (branch points, folds, Hopf bifurcations, user zeroes).
! These are characterized as zeroes of the function FNCS supplied in the
! call.
! This subroutine calls CONT and SOLVAE with varying stepsize RDS.
! The special point is assumed to have been found with sufficient
! accuracy if the ratio between RDS and the user supplied value of
! DS is less than the user-supplied toler EPSS.

    EXTERNAL FUNI

    DIMENSION IAP(*),RAP(*),RLCUR(*),RLOLD(*),RLDOT(*),PAR(*),ICP(*)
    DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*),IUZ(*),VUZ(*)
    DIMENSION AA(M1AA,*),RHS(*),U(*),DU(*),UDOT(*),UOLD(*)

    LOGICAL CHNG

    IID=IAP(18)
    ITMX=IAP(19)
    IBR=IAP(30)

    DS=RAP(1)
    DSMAX=RAP(3)
    DSOLD=RAP(5)
    EPSS=RAP(13)

! Check whether FNCS has changed sign (FNCS is EXTERNAL).

    Q0=Q
    Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA, &
         RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
    PQ=Q0*Q1
    NTOT=IAP(32)
    IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
       Q=Q1
       RETURN
    ENDIF

! Use the secant method for the first step:

    S0=0.d0
    S1=DSOLD
    ITLCSP=0
    DQ=Q0-Q1
    RDS=Q1/DQ*(S1-S0)
1   RDS=(1.d0+HMACH)*RDS
    S=S1+RDS

! Return if relative tolerance has been met :

    RRDS=ABS(RDS)/(1+DSQRT(ABS(DS*DSMAX)))
    IF(RRDS.LT.EPSS)THEN
       ITP=-1
       IAP(27)=ITP
       Q=0.d0
       WRITE(9,102)RDS
       RETURN
    ENDIF

! If requested write additional output on unit 9 :

    IF(IID.GE.2)THEN
       WRITE(9,101)ITLCSP,RDS
    ENDIF

    CALL CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
    CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS, &
         RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)THEN
       Q=0.d0
       RETURN
    ENDIF

    Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA, &
         RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
    ITLCSP=ITLCSP+1
    IF(ITLCSP.LE.ITMX)THEN
!        Use Mueller's method with bracketing for subsequent steps
       CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
       GOTO 1
    ELSE
       WRITE(9,103)IBR,MOD(NTOT-1,9999)+1
       Q=0.d0
       RETURN
    ENDIF

101 FORMAT(' ==> Location of special point :  Iteration ',I3, &
         '  Step size = ',ES13.5)
102 FORMAT(' ==> Location of special point : ', &
         ' Convergence.   Step size = ',ES13.5)
103 FORMAT(I4,I6,' NOTE:Possible special point')
  END SUBROUTINE LCSPAE

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNBPAE &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD, &
       UDOT,RHS,DFDU,DFDP,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    LOGICAL CHNG

    DIMENSION IAP(*),RAP(*)

    IID=IAP(18)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    DET=RAP(14)
    FNBPAE=DET
    CHNG=.TRUE.

! If requested write additional output on unit 9 :

    IF(IID.GE.2)WRITE(9,101)IBR,NTOP+1,FNBPAE
101 FORMAT(I4,I6,9X,'BP   Function:',ES14.5)

  END FUNCTION FNBPAE

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNLPAE &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT, &
       RHS,DFDU,DFDP,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    EXTERNAL FUNI

    DIMENSION IAP(*),RAP(*)
    DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DFDU(*),DFDP(*) 
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
! Local
    ALLOCATABLE UD(:),IR(:),IC(:)

    LOGICAL CHNG

    NDIM=IAP(1)
    IID=IAP(18)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    PAR(ICP(1))=RLCUR(1)
    CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,RHS,DFDU,DFDP)
    DO I=1,NDIM
       AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
       DO K=1,NDIM
          AA(I,K)=DFDU((K-1)*NDIM+I)
       ENDDO
    ENDDO
    DO K=1,NDIM
       AA(NDIM+1,K)=UDOT(K)
       RHS(K)=0.d0
    ENDDO
    AA(NDIM+1,NDIM+1)=RLDOT(1)
    RHS(NDIM+1)=1.d0

    ALLOCATE(UD(NDIM+1),IR(NDIM+1),IC(NDIM+1))
    CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,UD,NDIM+1,RHS,IR,IC,DET)
    RAP(14)=DET
    CALL NRMLZ(NDIM+1,UD)
    FNLPAE=UD(NDIM+1)
    DEALLOCATE(UD,IR,IC)
    RAP(16)=FNLPAE
    CHNG=.TRUE.

! If requested write additional output on unit 9 :

    IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1,FNLPAE
101 FORMAT(I4,I6,9X,'Fold Function:',ES14.5)

  END FUNCTION FNLPAE

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNHBAE &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT, &
       RHS,DFDU,DFDP,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

    EXTERNAL FUNI

    DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*)
    DIMENSION PAR(*),ICP(*),IAP(*),RAP(*)
! Local
    COMPLEX(KIND(1.0D0)) EV, ZTMP
    ALLOCATABLE EV(:)
    LOGICAL CHNG

    NDIM=IAP(1)
    NDM=IAP(23)
    IPS=IAP(2)
    ISP=IAP(9)
    ISW=IAP(10)
    IID=IAP(18)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1
    ALLOCATE(EV(NDIM))

! INITIALIZE

    CHNG=.FALSE.

! Compute the eigenvalues of the Jacobian

    CALL EIG(IAP,NDM,NDIM,DFDU,EV,IER)
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
       RMAX=-RLARGE
       LOC=I
       DO J=I,NDM
          RP=REAL(EV(J))
          IF(RP.GE.RMAX)THEN
             RMAX=RP
             LOC=J
          ENDIF
       ENDDO
       IF(LOC.NE.I) THEN
          ZTMP=EV(LOC)
          EV(LOC)=EV(I)
          EV(I)=ZTMP
       ENDIF
    ENDDO

! Compute the smallest real part.

    RIMHB=0.d0
    AREV=RLARGE
    REV=0.d0
    DO I=1,NDM
       IF(AIMAG(EV(I)).NE.0.d0)THEN
          AR=ABS(REAL(EV(I)))
          IF(AR.LE.AREV)THEN
             AREV=AR
             REV=REAL(EV(I))
             RIMHB=ABS(AIMAG(EV(I)))
             IF(RIMHB.NE.0.d0.AND.ABS(ISW).LE.1)PAR(11)=PI(2.d0)/RIMHB
          ENDIF
       ENDIF
    ENDDO

! Count the number of eigenvalues with negative real part.

! Set tolerance for deciding if an eigenvalue is in the positive
! half-plane. Use, for example, tol=1d-3 for conservative systems.

    tol=1.d-5
    NINS1=0
    DO I=1,NDM
       IF(REAL(EV(I)).LE.tol)NINS1=NINS1+1
    ENDDO

    IF(ISW.EQ.2 .OR. ISP.EQ.0 .OR. ISP.EQ.3)THEN
       FNHBAE=0.d0
    ELSE
       FNHBAE=REV
    ENDIF
    RAP(17)=FNHBAE
    NINS=IAP(33)
    IF(NINS1.NE.NINS)CHNG=.TRUE.
    NINS=NINS1
    IAP(33)=NINS

    NTOT=IAP(32)
    NTOTP1=NTOT+1
    IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1,FNHBAE
    IF(NINS1.EQ.NDM)NTOTP1=-NTOTP1

    WRITE(9,102)ABS(IBR),NTOP+1,NINS
    IF(IPS.EQ.-1)THEN
       DO I=1,NDM
          WRITE(9,103)ABS(IBR),NTOP+1,I,EXP(EV(I))
       ENDDO
    ELSE
       DO I=1,NDM
          WRITE(9,103)ABS(IBR),NTOP+1,I,EV(I)
       ENDDO
    ENDIF

101 FORMAT(I4,I6,9X,'Hopf Function:',ES14.5)
102 FORMAT(/,I4,I6,9X,'Eigenvalues  :   Stable:',I4)
103 FORMAT(I4,I6,9X,'Eigenvalue',I3,":",2ES14.5)

    DEALLOCATE(EV)
  END FUNCTION FNHBAE

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNUZAE &
       (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT, &
       RHS,DFDU,DFDP,IUZ,VUZ)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION IAP(*),PAR(*),IUZ(*),VUZ(*)

    LOGICAL CHNG

    IID=IAP(18)
    IUZR=IAP(26)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    FNUZAE=PAR(ABS(IUZ(IUZR)))-VUZ(IUZR)
    CHNG=.TRUE.

    IF(IID.GE.3)WRITE(9,101)ABS(IBR),NTOP+1,IUZR,FNUZAE
101 FORMAT(I4,I6,9X,'User Func.',I3,1X,ES14.5)

  END FUNCTION FNUZAE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                   Branch Switching for Algebraic Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! ---------- -----
  SUBROUTINE STBIF(IAP,ICP,M1AA,AA,M1SB,STUD,STU,STLA, &
       STLD,RLCUR,RLDOT,U,DU,UDOT,DFDU,DFDP,THL,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Stores branching data in the following arrays :
!        STU    ( the solution vector U )
!        STUD   ( U-dot )
!        STLA   ( PAR(ICP(1)) )
!        STLD  ( PAR(ICP(1))-dot )
! Here the vector ( PAR(ICP(1))-dot , U-dot ) lies in the 2-d nullspace
! at branch point and is perpendicular to the direction vector of
! known branch at this point.

    DIMENSION IAP(*),AA(M1AA,*),U(*),DU(*),UDOT(*),DFDU(*),DFDP(*)
    DIMENSION STUD(M1SB,*),STU(M1SB,*),STLA(*),STLD(*)
    DIMENSION ICP(*),RLCUR(*),RLDOT(*),THL(*),THU(*)
    ALLOCATABLE IR(:),IC(:)

    NDIM=IAP(1)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1
    NBIF=IAP(35)

! Keep track of the number of branch points stored.

    IF(NBIF.EQ.NBIFX)WRITE(9,101)IBR,NTOP
    IF(NBIF.GT.NBIFX)THEN
       NBIF=NBIFX
       IAP(35)=NBIF
       RETURN
    ENDIF

    DO I=1,NDIM
       DO J=1,NDIM
          AA(I,J)=DFDU((J-1)*NDIM+I)
       ENDDO
    ENDDO

    ND1=NDIM+1
    DO I=1,NDIM
       AA(I,ND1)=DFDP((ICP(1)-1)*NDIM+I)
       AA(ND1,I)=UDOT(I)
    ENDDO
    AA(ND1,ND1)=RLDOT(1)

    ALLOCATE(IR(NDIM+1),IC(NDIM+1))
    CALL NLVC(ND1,M1AA,1,AA,DU,IR,IC)
    DEALLOCATE(IR,IC)

    SS=0.d0
    DO I=1,NDIM
       SS=SS+THU(I)*DU(I)**2
    ENDDO
    SS=SS+THL(1)*DU(ND1)**2
    SC=1.d0/DSQRT(SS)

    DO I=1,ND1
       DU(I)=SC*DU(I)
    ENDDO

    NBIF=IAP(35)
    STLD(NBIF)=DU(ND1)
    DO I=1,NDIM
       STU(NBIF,I)=U(I)
       STUD(NBIF,I)=DU(I)
    ENDDO
    STLA(NBIF)=RLCUR(1)

101 FORMAT(I4,I6,' NOTE:No more branch points can be stored')
  END SUBROUTINE STBIF

! ---------- -----
  SUBROUTINE SWPNT(IAP,RAP,PAR,ICP,RDS,M1SB,STUD,STU,STLA,STLD, &
       RLCUR,RLDOT,U,UDOT)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! This subroutine retrieves the branching data U, U-dot, PAR(ICP(1)),
! PAR(ICP(1))-dot. If this initialization corresponds to the computation
! of the bifurcating branch in opposite direction, then only the sign of
!  the stepsize ( DS ) along the branch is reversed.

    DIMENSION IAP(*),RAP(*)
    DIMENSION U(*),UDOT(*),STUD(M1SB,*),STU(M1SB,*),STLA(*),STLD(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)

    NDIM=IAP(1)
    ISW=IAP(10)
    MXBF=IAP(17)
    NBIF=IAP(35)
    IPOS=IAP(36)

    DS=RAP(1)

    RDS=DS
    IF(IPOS.EQ.0)RDS=-DS
    RLCUR(1)=STLA(1)
    PAR(ICP(1))=RLCUR(1)
    RLDOT(1)=STLD(1)
    DO I=1,NDIM
       U(I)=STU(1,I)
       UDOT(I)=STUD(1,I)
    ENDDO
    IF(ABS(ISW).EQ.2)PAR(ICP(2))=U(NDIM)

    IF(MXBF.GE.0)THEN
       IPOS=1-IPOS
       IAP(36)=IPOS
    ENDIF
    IF(IPOS.EQ.0)RETURN

    DO I=1,NBIF
       STLA(I)=STLA(I+1)
       STLD(I)=STLD(I+1)
       DO I1=1,NDIM
          STU(I,I1)=STU(I+1,I1)
          STUD(I,I1)=STUD(I+1,I1)
       ENDDO
    ENDDO

  END SUBROUTINE SWPNT

! ---------- -----
  SUBROUTINE SWPRC(IAP,RAP,PAR,ICP,FUNI,M1AA,AA,RHS, &
       RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RDS,THL,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Controls the computation of the second point on a bifurcating branch.
! This point is required to lie in a hyper-plane at distance DS from the
! branch point. This hyper-plane is parallel to the tangent of the
! known branch at the branch point.

    EXTERNAL FUNI

    DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DU(*)
    DIMENSION IAP(*),RAP(*),F(*),DFDU(*),DFDP(*),THL(*),THU(*)
    DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
! Local
    ALLOCATABLE IR(:),IC(:),U1(:)

    NDIM=IAP(1)
    IADS=IAP(8)
    IID=IAP(18)
    ITNW=IAP(20)
    IBR=IAP(30)
    NTOT=IAP(32)
    NTOP=MOD(NTOT-1,9999)+1

    DSMIN=RAP(2)
    EPSL=RAP(11)
    EPSU=RAP(12)

! Initialize and provide initial guess :

    ALLOCATE(IR(NDIM+1),IC(NDIM+1),U1(NDIM+1))
    RLOLD(1)=RLCUR(1)
    RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
    DO I=1,NDIM
       UOLD(I)=U(I)
       U(I)=UOLD(I)+RDS*UDOT(I)
    ENDDO

2   DSOLD=RDS
    RAP(5)=DSOLD
    NIT=0
    IAP(31)=NIT

! Write additional output on unit 9 if requested :

    NDMR=NDIM
    IF(NDMR.GT.6)NDMR=6
    IF(IID.GE.2)WRITE(9,101)IBR,NTOP,NIT,ICP(1), &
         RLCUR(1),(U(I),I=1,NDMR)

    RLM1=RLCUR(1)
    DO I=1,NDIM
       U1(I)=U(I)
    ENDDO

    DO NIT1=1,ITNW

       NIT=NIT1
       IAP(31)=NIT
       PAR(ICP(1))=RLCUR(1)
       CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       DO I=1,NDIM
          AA(I,NDIM+1)=DFDP((ICP(1)-1)*NDIM+I)
          RHS(I)=-F(I)
          DO K=1,NDIM
             AA(I,K)=DFDU((K-1)*NDIM+I)
          ENDDO
       ENDDO
       DO K=1,NDIM
          AA(NDIM+1,K)=THU(K)*UDOT(K)
       ENDDO
       AA(NDIM+1,NDIM+1)=THL(1)*RLDOT(1)
       SS=0.d0
       DO I=1,NDIM
          SS=SS+THU(I)*(U(I)-U1(I))*UDOT(I)
       ENDDO
       RHS(NDIM+1)=-SS-THL(1)*(RLCUR(1)-RLM1)*RLDOT(1)

! Use Gauss elimination with pivoting to solve the linearized system :

       IF(IID.GE.5)CALL WRJAC(NDIM+1,M1AA,AA,RHS)
       CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,DU,NDIM+1,RHS,IR,IC,DET)
       RAP(14)=DET
       DRLM=DU(NDIM+1)

! Add the Newton increments :

       DO I=1,NDIM
          U(I)=U(I)+DU(I)
       ENDDO
       RLCUR(1)=RLCUR(1)+DRLM
       DUMX=0.d0
       UMX=0.d0
       DO I=1,NDIM
          ADU=ABS(DU(I))
          IF(ADU.GT.DUMX)DUMX=ADU
          AU=ABS(U(I))
          IF(AU.GT.UMX)UMX=AU
       ENDDO

       IF(IID.GE.2)THEN
          WRITE(9,101)IBR,NTOP,NIT,ICP(1),RLCUR(1),(U(I),I=1,NDMR)
       ENDIF

! Check whether relative error has reached user-supplied tolerance :

       RDRLM=ABS(DRLM)/(1.d0+ABS(RLCUR(1)))
       RDUMX=DUMX/(1.d0+UMX)
       IF(RDRLM.LT.EPSL.AND.RDUMX.LT.EPSU)THEN
          DEALLOCATE(IR,IC,U1)
          RETURN
       ENDIF
    ENDDO

! Maximum number of iterations reached. Reduce stepsize and try again.

    IF(IADS.EQ.0)WRITE(9,102)IBR,NTOP
    IF(IADS.EQ.0)GOTO 5

    MXT=ITNW
    IAP(31)=MXT
    CALL ADPTDS(IAP,RAP,RDS)
    IF(ABS(RDS).LT.DSMIN)GOTO 4
    RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
    DO I=1,NDIM
       U(I)=UOLD(I)+RDS*UDOT(I)
    ENDDO
    IF(IID.GE.2)WRITE(9,103)IBR,NTOP
    GOTO 2

! Minimum stepsize reached.

4   WRITE(9,104)IBR,NTOP
5   RLCUR(1)=RLOLD(1)
    PAR(ICP(1))=RLCUR(1)
    DO I=1,NDIM
       U(I)=UOLD(I)
    ENDDO
    ISTOP=1
    IAP(34)=ISTOP

    DEALLOCATE(IR,IC,U1)

101 FORMAT(' Branch ',I2,' N=',I5,1X,'IT=',I2,1X,'PAR(',I2,')=', &
         ES11.3,1X,'U=',7ES11.3)
102 FORMAT(I4,I6,' NOTE:No convergence when switching branches', &
         ' with fixed step size')
103 FORMAT(I4,I6,' NOTE:Retrying step')
104 FORMAT(I4,I6,' NOTE:No convergence when switching branches', &
         ' with minimum step size')
  END SUBROUTINE SWPRC
  
! ---------- ------
  SUBROUTINE STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Stores the bifurcation diagram on unit 7 (Algebraic Problems).
! Every line written contains, in order, the following:
!
!  IBR    : The label of the branch.
!  NTOT   : The index of the point on the branch.
!           (Points are numbered consecutively along a branch).
!           If IPS=1 or -1, then the sign of NTOT indicates stability :
!            - = stable , + = unstable, unknown, or not relevant.
!  ITP    : An integer indicating the type of point :
!
!             1  (BP)  :   Branch point.
!             2  (LP)  :   Fold.
!             3  (HB)  :   Hopf bifurcation point.
!             4  (  )  :   Output point (Every NPR steps along branch).
!            -4  (UZ)  :   Output point (Zero of user function).
!             9  (EP)  :   End point of branch, normal termination.
!            -9  (MX)  :   End point of branch, abnormal termination.
!
!  LAB        : The label of a special point.
!  PAR(ICP(1)): The principal parameter.
!  A          : The L2-norm of the solution vector, or other measure of
!               the solution (see the user-supplied parameter IPLT).
!  U          : The first few components of the solution vector.
!  PAR(ICP(*)): Further free parameters (if any).
!
    DIMENSION IAP(*),ICP(*),RAP(*),PAR(*),RLCUR(*),U(*)

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

    CALL PVLSAE(IAP,RAP,U,PAR)

! ITP is set to 4 every NPR steps along a branch, and the entire
! solution is written on unit 8.

    IF(NPR.NE.0)THEN
       IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
       IAP(27)=ITP
    ENDIF

! CHECK WHETHER LIMITS OF THE BIFURCATION DIAGRAM HAVE BEEN REACHED :

    IAB=ABS(IPLT)

    IF(IAB.LE.NDIM .AND. IAB.GT.0)THEN
       AMP=U(IAB)
    ELSE IF(IPLT.GT.NDIM.AND.IPLT.LE.2*NDIM)THEN
       AMP=U(IPLT-NDIM)
    ELSE IF(IPLT.GT.2*NDIM.AND.IPLT.LE.3*NDIM)THEN
       AMP=U(IPLT-2*NDIM)
    ELSE
       AMP=RNRMV(NDM,U)
    ENDIF
    RAP(10)=AMP

    ISTOP=IAP(34)
    IF(ISTOP.EQ.1)THEN
!        Maximum number of iterations reached somewhere.
       ITP=-9-10*ITPST
       IAP(27)=ITP
    ELSEIF(ISTOP.EQ.-1)THEN
!        ** UZR endpoint
       ITP=9+10*ITPST
       IAP(27)=ITP
    ELSE
       IF(RLCUR(1).LT.RL0.OR.RLCUR(1).GT.RL1 &
            .OR. AMP.LT.A0.OR.AMP.GT.A1 &
            .OR. NTOT.EQ.NMX) THEN
          ISTOP=1
          IAP(34)=ISTOP
          ITP=9+10*ITPST
          IAP(27)=ITP
       ENDIF
    ENDIF

    LABW=0
    IF(MOD(ITP,10).NE.0)THEN
       LAB=IAP(37)
       LAB=LAB+1
       IAP(37)=LAB
       LABW=LAB
    ENDIF

! Determine stability and print output on units 6 and 7.

    NTOTS=NTOT
    NINS=IAP(33)
    IF(ABS(IPS).EQ.1 .AND. ABS(ISW).LE.1 .AND. NTOT.GT.1)THEN
       IF(NINS.EQ.NDIM)NTOTS=-NTOT
    ENDIF
    CALL WRLINE(IAP,PAR,ICP(NPARX+1),IBR,NTOTS,LABW,AMP,U)

! Write restart information for multi-parameter analysis :

    IF(LABW.NE.0)CALL WRTSP8(IAP,RAP,PAR,ICP,LABW,RLCUR,U)
!
  END SUBROUTINE STPLAE

! ---------- ------
  SUBROUTINE WRTSP8(IAP,RAP,PAR,ICP,LAB,RLCUR,U)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Write restart information on singular points, plotting points, etc.,
! on unit 8.

    DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),RLCUR(*),U(*)

    NDIM=IAP(1)
    ISW=IAP(10)
    ITP=IAP(27)
    IBR=IAP(30)
    NFPR=IAP(29)
    NTOT=IAP(32)

    NTPL=1
    NAR=NDIM+1
    NROWPR=NDIM/7+1 + (NPARX-1)/7+1
    PAR(ICP(1))=RLCUR(1)
    T=0.d0
    AMP=0.d0
    RAP(10)=AMP

    MTOT=MOD(NTOT-1,9999)+1
    WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,0,0,NPARX
    WRITE(8,102)T,(U(I),I=1,NDIM)
    WRITE(8,102)(PAR(I),I=1,NPARX)

101 FORMAT(6I6,I8,I6,I8,3I5)
102 FORMAT(4X,7ES19.10)

    CALL FLUSH(8)
  END SUBROUTINE WRTSP8

! ---------- ------
  SUBROUTINE WRJAC(N,M1AA,AA,RHS)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION AA(M1AA,*),RHS(*)

    WRITE(9,101)
    WRITE(9,100)(RHS(I),I=1,N)
    WRITE(9,102)
    DO I=1,N
       WRITE(9,100)(AA(I,J),J=1,N)
    ENDDO
100 FORMAT(1X,12E10.3)
101 FORMAT(/,' Residual vector :')
102 FORMAT(/,' Jacobian matrix :')

  END SUBROUTINE WRJAC

! ---------- ------
  SUBROUTINE PVLSAE(IAP,RAP,U,PAR)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION IAP(*),RAP(*),U(*),PAR(*)

    CALL SETPAE(IAP,RAP)
    NDM=IAP(23)
    CALL PVLS(NDM,U,PAR)

  END SUBROUTINE PVLSAE

! ---------- ------
  SUBROUTINE SETPAE(IAP,RAP)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    POINTER DTV(:),RAV(:),IAV(:),P0V(:,:),P1V(:,:)
    COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V
    TARGET IAP(NIAP),RAP(NRAP)

    IAV=>IAP
    RAV=>RAP

  END SUBROUTINE SETPAE

END MODULE AE
