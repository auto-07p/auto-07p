C     ------ --------------
      MODULE AUTO_CONSTANTS
C
      IMPLICIT NONE
      INTEGER NPARX,NIAP,NRAP
      INCLUDE 'auto.h'
C
      INTEGER NDIM,IPS,IRS,ILP
      INTEGER NICP
      INTEGER,ALLOCATABLE :: ICU(:)
      INTEGER NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
      INTEGER NMX
      DOUBLE PRECISION RL0,RL1,A0,A1
      INTEGER NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
      DOUBLE PRECISION EPSL,EPSU,EPSS
      DOUBLE PRECISION DS,DSMIN,DSMAX
      INTEGER IADS
      INTEGER NTHL
      INTEGER,ALLOCATABLE :: ITHL(:)
      DOUBLE PRECISION,ALLOCATABLE :: THL(:),VTHL(:)
      INTEGER NTHU
      DOUBLE PRECISION,ALLOCATABLE :: THU(:)
      INTEGER NUZR
      INTEGER, ALLOCATABLE :: IUZ(:)
      DOUBLE PRECISION,ALLOCATABLE :: VUZ(:)
C
      END MODULE AUTO_CONSTANTS
C
C     ------- ----
      PROGRAM AUTO
C
      USE AUTOMPI
      USE IO
      USE SUPPORT
      USE AUTO_CONSTANTS
C$    USE OMP_LIB
      USE COMPAT
C
      IMPLICIT NONE
C
      LOGICAL EOF
C Local
      INTEGER IAP(NIAP)
      DOUBLE PRECISION RAP(NRAP),TIME0,TIME1,TOTTIM
      INTEGER IAM,LINE
      LOGICAL FIRST
      CHARACTER(256) :: SFILE, SOLFILE, BIFFILE, DIAFILE
C
C Initialization :
C
       CALL MPIINI(IAP)
       IAM=IAP(38)
       IF(IAM/=0)THEN
         CALL MPIWORKER(IAP)
         STOP
       ENDIF
C
       FIRST=.TRUE.
       SFILE='fort.3'
       BIFFILE='fort.7'
       SOLFILE='fort.8'
       DIAFILE='fort.9'
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential')
C
       EOF=.FALSE.
       LINE=0
 1     IF(IAP(39).GT.1)THEN
         CALL MPITIM(TIME0)
       ELSE
         TIME0=AUTIM()
C$       TIME0=omp_get_wtime()
       ENDIF
       CALL INIT(IAP,RAP,EOF,LINE,SFILE,BIFFILE)
       IF(FIRST)THEN
          IF(BIFFILE/='fort.7')THEN
             BIFFILE(1:2)='b.'
             SOLFILE=BIFFILE
             SOLFILE(1:2)='s.'
             DIAFILE=BIFFILE
             DIAFILE(1:2)='d.'
          ENDIF
          OPEN(7,FILE=BIFFILE,STATUS='unknown',ACCESS='sequential')
          OPEN(8,FILE=SOLFILE,STATUS='unknown',ACCESS='sequential')
          OPEN(9,FILE=DIAFILE,STATUS='unknown',ACCESS='sequential')
          FIRST=.FALSE.
       ENDIF
       IF(EOF)THEN
         CALL MPIEND()
         STOP
       ENDIF
       CALL FINDLB_OR_STOP(SFILE,IAP)
       CALL MPIIAP(IAP)
       CALL AUTOI(IAP,RAP)
C-----------------------------------------------------------------------
C
      IF(IAP(39).GT.1)THEN
        CALL MPITIM(TIME1)
      ELSE
        TIME1=AUTIM()
C$      TIME1=omp_get_wtime()
      ENDIF
      TOTTIM=TIME1-TIME0
      CALL WRBAR("=",47)
      WRITE(9,301)TOTTIM
      WRITE(6,301)TOTTIM
      CALL CLEANUP()
      GOTO 1
C
 301  FORMAT(/,' Total Time ',E12.3)
C
      CONTAINS
C
C     ---------- ---------
      SUBROUTINE MPIWORKER(IAP)
      
      USE AUTO_CONSTANTS
      USE AUTOMPI
      IMPLICIT NONE

      INTEGER IAP(*)
      DOUBLE PRECISION RAP(1)

      INTEGER FUNI_ICNI_PARAMS(5)

      DO WHILE(.TRUE.)
         CALL MPIBCASTI(FUNI_ICNI_PARAMS,5)
         ! figure out what funi and icni are from
         ! the iap array. We do it here, since I
         ! don't know how to pass function pointers
         ! through MPI in a possibly heterogeneous 
         ! environment :-)
         IPS     = FUNI_ICNI_PARAMS(1)
         IAP(2)  = IPS
         IRS     = FUNI_ICNI_PARAMS(2)
         IAP(3)  = IRS
         ISW     = FUNI_ICNI_PARAMS(3)
         IAP(10) = ISW
         IAP(27) = FUNI_ICNI_PARAMS(4) ! itp
         IAP(29) = FUNI_ICNI_PARAMS(5) ! nfpr
         CALL AUTOI(IAP,RAP)
         ! autoi calls autobv which eventually calls solvbv;
         ! a return means another init message
      ENDDO
      END SUBROUTINE MPIWORKER
C
C     ---------- --------------
      SUBROUTINE FINDLB_OR_STOP(SFILE,IAP)
C
C Find restart label and determine type of restart point.
C or stop otherwise
C
      USE AUTO_CONSTANTS
      IMPLICIT NONE
      INTEGER IAP(*)
      CHARACTER(*) SFILE

      INTEGER NFPR,NPARR
      LOGICAL FOUND

      NFPR=IAP(29)

      FOUND=.FALSE.
      IF(IRS.GT.0) THEN
         CALL FINDLB(SFILE,IAP,IRS,NFPR,NPARR,FOUND)
         IAP(29)=NFPR
         IF(.NOT.FOUND) THEN
            WRITE(6,"(' Restart label ',I4,' not found')")IRS
            STOP
         ENDIF
         IAP(31)=MAX(NPARR,IAP(31))
      ENDIF
      END SUBROUTINE FINDLB_OR_STOP
C
C     ---------- -----
      SUBROUTINE AUTOI(IAP,RAP)
C
      USE INTERFACES
      USE AUTO_CONSTANTS
      USE AE
      USE BVP
      USE HOMCONT, ONLY:FNHO,BCHO,ICHO,PVLSHO,STPNHO
C
      INTEGER IAP(*)
      DOUBLE PRECISION RAP(*)

      INTEGER ITP,NFPR,NNICP,NPAR
      INTEGER, ALLOCATABLE :: ICP(:)
      DOUBLE PRECISION, ALLOCATABLE :: PAR(:)

      ITP=IAP(27)
      NFPR=IAP(29)
C
      IF(IAP(38)==0)THEN
        NPAR=IAP(31)
        NNICP=MAX(5*(NBC+NINT-NDIM+1)+NDIM+NINT+3,5*NICP+NDIM+3)
        ALLOCATE(ICP(NNICP))
        ICP(1:NICP)=ICU(1:NICP)
        ICP(NICP+1:NNICP)=0
        ALLOCATE(PAR(NPAR))
        PAR(:)=0.d0
        CALL INIT1(IAP,RAP,ICP,PAR)
      ENDIF
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  One-parameter continuations
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
       IF((IPS.EQ.0.OR.IPS.EQ.1) .AND. ABS(ISW).LE.1 ) THEN
C        ** Algebraic systems.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FUNI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FUNI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.11 .AND. ABS(ISW).LE.1 ) THEN
C        ** Waves : Spatially homogeneous solutions,
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNWS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNWS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF((IPS.EQ.-1) .AND. ABS(ISW).LE.1 ) THEN
C        ** Discrete dynamical systems : fixed points.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNDS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNDS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.-2) THEN
C        ** Time integration.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNTI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNTI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).LE.1 ) THEN
C        ** Periodic solutions
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPS,BCPS,ICPS,STPNPS,
     *     PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.12 .AND. ABS(ISW).LE.1 ) THEN
C        ** Wave train solutions to parabolic systems.
          IF(IRS.GT.0)THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNWP,BCPS,ICPS,STPNBV,
     *        PVLSBV,THL,THU,IUZ,VUZ)
          ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNWP,BCPS,ICPS,STPNUB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
          ENDIF
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)<=1) THEN
C        ** Boundary value problems. (4)
C        ** Boundary value problems with Floquet multipliers. (7)
          CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNPS,
     *         PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.9 .AND. ABS(ISW).LE.1) THEN
C        ** Homoclinic bifurcation analysis.
          CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNHO,BCHO,ICHO,STPNHO,
     *         PVLSHO,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.14) THEN
C        ** Evolution calculations for parabolic systems.
C           (Periodic boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPE,BCPS,ICPE,STPNBV,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPE,BCPS,ICPE,STPNUB,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.15.AND.ABS(ISW).EQ.1) THEN
C        ** Optimization of periodic solutions.
         IF(NFPR.LT.6)THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPO,BCPO,ICPO,STPNPO,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPO,BCPO,ICPO,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.16) THEN
C        ** Evolution calculations for parabolic systems.
C           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPE,BCNI,ICPE,STPNBV,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPE,BCNI,ICPE,STPNUB,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.17) THEN
C        ** Continuation of stationary states of parabolic systems.
C           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNSP,BCNI,ICPE,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNSP,BCNI,ICPE,STPNUB,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.5) THEN
C        ** Algebraic optimization problems.
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPR=NFPR+1
         IF(NFPR.EQ.2) THEN
           IF(IRS.GT.0) THEN
            CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNC1,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
            CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNC1,STPNC1,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           IF(MOD(ITP,10).NE.2) THEN
            CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNC2,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
            CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNC2,STPNC2,THL,THU,IUZ,VUZ)
           ENDIF
         ENDIF
C
       ELSE 
C        **None of the above cases
         GOTO 2
       ENDIF
C
       GOTO 3
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  Two-Parameter Continuation.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
 2     IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 .AND. (ITP.EQ.2) )
     * THEN
C        ** Fold continuation (algebraic problems).
         CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNLP,STPNLP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 
     *         .AND. ( (ABS(ITP)/10).EQ.2 ) )
     * THEN
C        ** Fold continuation (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNLP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 .AND. (ITP.EQ.1) )
     * THEN
C        ** BP cont (algebraic problems) (by F. Dercole).
         CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNBP,STPNBP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 
     *         .AND. ( (ABS(ITP)/10).EQ.1 ) )
     * THEN
C        ** BP cont (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNBP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF((ABS(IPS)<=1.OR.IPS==11).AND.ABS(ISW)==2.AND.ITP==3 )
     * THEN
C        ** Hopf bifurcation continuation (ODE/waves/maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNHB,STPNHB,THL,THU,IUZ,VUZ)
C
       ELSE IF((ABS(IPS)<=1.OR.IPS==11)
     *        .AND.ABS(ISW)==2.AND.(ABS(ITP)/10)==3 ) THEN
C        ** Hopf bifurcation continuation (ODE/waves/maps, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,ICU,FNHB,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS==2 .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN 
C        ** Fold continuation (Periodic solutions, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPL,BCPL,ICPL,STPNPL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS==2 .AND. ABS(ISW)==2 .AND. (ABS(ITP)/10)==5 )
     * THEN
C        ** Fold continuation (Periodic solutions, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPL,BCPL,ICPL,STPNBV,
     *   PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS==2 .AND. ABS(ISW)>=2 .AND. 
     *         (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
C        ** BP cont (Periodic sol., start and restart) (by F. Dercole).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPBP,BCPBP,ICPBP,STPNPBP,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.2 .OR. IPS.EQ.7)
     *      .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.7 ) THEN
C        ** Continuation of period doubling bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPD,BCPD,ICPD,STPNPD,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.2 .OR. IPS .EQ.7)
     *      .AND. ABS(ISW).EQ.2 .AND. (ABS(ITP)/10).EQ.7)
     * THEN
C        ** Continuation of period doubling bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNPD,BCPD,ICPD,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.8 ) THEN
C        ** Continuation of torus bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNTR,BCTR,ICTR,STPNTR,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.2 .AND. (ABS(ITP)/10).EQ.8)
     * THEN
C        ** Continuation of torus bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNTR,BCTR,ICTR,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN
C        ** Continuation of folds (BVP, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNBL,BCBL,ICBL,STPNBL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. 
     *         (ABS(ITP)/10)==5 ) THEN
C        ** Continuation of folds (BVP, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNBL,BCBL,ICBL,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)>=2 .AND.
     *          (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
C        ** BP cont (BVP, start and restart) (by F. Dercole).
         CALL AUTOBV(IAP,RAP,PAR,ICP,ICU,FNBBP,BCBBP,ICBBP,STPNBBP,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE
C        ** Error in INIT.
         WRITE(6,500)
         STOP
       ENDIF
 3     CONTINUE
C
C Error Message.
 500  FORMAT(' Initialization Error')
C
      IF(IAP(38)==0)THEN
         DEALLOCATE(ICP,PAR)
      ENDIF

      END SUBROUTINE AUTOI
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Initialization
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE INIT(IAP,RAP,EOF,LINE,SFILE,SVFILE)
C
      USE AUTO_CONSTANTS
C
      IMPLICIT NONE
C
C Reads the file of continuation constants
C
      INTEGER, INTENT(OUT) :: IAP(*)
      DOUBLE PRECISION, INTENT(OUT) :: RAP(*)
      LOGICAL, INTENT(OUT) :: EOF
      INTEGER, INTENT(INOUT) :: LINE
      CHARACTER(LEN=*), INTENT(INOUT) :: SFILE, SVFILE
C
      INTEGER IBR,I,IUZR,NFPR,NDM,NNT0,NBC0
      INTEGER NINS,LAB,NTOT,ITP,ITPST,NPAR
      DOUBLE PRECISION AMP,BIFF,DET,SPBF,HBFF,FLDF
      CHARACTER(LEN=256) :: STR
      INTEGER KEYEND,POS,LISTLEN,NPOS
      LOGICAL KEYS
      CHARACTER(LEN=*), PARAMETER :: ICONSTANTS(23) = (/
     * "NDIM", "IPS ", "IRS ", "ILP ", "NTST", "NCOL", "IAD ", "IADS",
     * "ISP ", "ISW ", "IPLT", "NBC ", "NINT", "NMX ", "    ", "NPR ",
     * "MXBF", "IID ", "ITMX", "ITNW", "NWTN", "JAC ", "NPAR" /)
      INTEGER, PARAMETER :: IDEFAULTS(23) = (/
     * 2, 1, 0, 1, 20, 4, 3, 1,
     * 2, 1, 0, 0, 0, 99999, 0, 9999,
     * 10, 2, 9, 5, 3, 0, NPARX /)
      CHARACTER(LEN=*), PARAMETER :: RCONSTANTS(13) = (/
     * "DS   ", "DSMIN", "DSMAX", "     ", "     ", "RL0  ", "RL1  ",
     * "A0   ", "A1   ", "     ", "EPSL ", "EPSU ", "EPSS " /)
      DOUBLE PRECISION, PARAMETER :: RDEFAULTS(13) = (/
     * 0.01d0, 0.005d0, 0.1d0, 0d0, 0d0, -1d300, 1d300, -1d300, 1d300,
     * 0d0, 1d-7, 1d-7, 1d-5 /)
      INTEGER, ALLOCATABLE :: ITH(:)
      DOUBLE PRECISION, ALLOCATABLE :: VTHU(:)
C
      IAP(1:23)=IDEFAULTS(:)
      RAP(1:13)=RDEFAULTS(:)
      RAP(6)=-HUGE(1d0)
      RAP(7)=HUGE(1d0)
      RAP(8)=-HUGE(1d0)
      RAP(9)=HUGE(1d0)
      NICP=1
      ALLOCATE(ICU(1),IUZ(1),VUZ(1),ITH(1),VTHU(1),ITHL(1),VTHL(1))
      ICU(1)=1
      NTHU=0
      NTHL=1
      ITHL(1)=11
      VTHL(1)=0d0
      NUZR=0
      NPAR=NPARX

      NPOS=1
      KEYS=.FALSE.
      scanloop: DO
         IF(NPOS==1)THEN
            LINE=LINE+1
            EOF=.TRUE.
            READ(2,'(A)',END=1) STR
            EOF=.FALSE.
         ELSE
            STR=STR(NPOS:)
         ENDIF
         STR=ADJUSTL(STR)
         IF(LEN_TRIM(STR)==0)CYCLE
         DO I=1,LEN_TRIM(STR)
            ! comment on line
            IF(STR(I:I)=='#'.OR.STR(I:I)=='!')THEN
               NPOS=1
               CYCLE scanloop
            ENDIF
            ! keyword detected
            IF((LGE(STR(I:I),'A').AND.LLE(STR(I:I),'Z')).OR.
     &         (LGE(STR(I:I),'a').AND.LLE(STR(I:I),'z')))THEN
               STR=STR(I:)
               KEYS=.TRUE.
               EXIT
            ELSE
               EXIT scanloop
            ENDIF
            IF(I==LEN_TRIM(STR))THEN
               NPOS=1
               CYCLE scanloop
            ENDIF
         ENDDO
         EOF=.FALSE.
         ! look for = after keyword
         KEYEND=SCAN(STR,'= ')-1
         IF(KEYEND==-1)THEN
            LINE=LINE-1
            EXIT scanloop
         ENDIF
         POS=SCAN(STR,'=')+1
         STR(POS:)=ADJUSTL(STR(POS:))
         CALL SCANVALUE(STR,POS,NPOS,LISTLEN)
         DO I=1,23
            IF(STR(1:KEYEND)==TRIM(ICONSTANTS(I)))THEN
               READ(STR(POS:),*,ERR=3)IAP(I)
               CYCLE scanloop
            ENDIF
         ENDDO
         DO I=1,13
            IF(STR(1:KEYEND)==TRIM(RCONSTANTS(I)))THEN
               READ(STR(POS:),*,ERR=3)RAP(I)
               CYCLE scanloop
            ENDIF
         ENDDO
         IF(STR(1:KEYEND)=='ICP')THEN
            NICP=LISTLEN
            DEALLOCATE(ICU)
            ALLOCATE(ICU(NICP))
            READ(STR(POS:),*,ERR=3)ICU            
         ELSEIF(STR(1:KEYEND)=='UZR')THEN
            NUZR=LISTLEN
            DEALLOCATE(IUZ,VUZ)
            ALLOCATE(IUZ(NUZR),VUZ(NUZR))
            READ(STR(POS:),*,ERR=3)(IUZ(I),VUZ(I),I=1,NUZR)
         ELSEIF(STR(1:KEYEND)=='THL')THEN
            NTHL=LISTLEN
            DEALLOCATE(ITHL,VTHL)
            ALLOCATE(ITHL(NTHL),VTHL(NTHL))
            READ(STR(POS:),*,ERR=3)(ITHL(I),VTHL(I),I=1,NTHL)
         ELSEIF(STR(1:KEYEND)=='THU')THEN
            NTHU=LISTLEN
            DEALLOCATE(ITH,VTHU)
            ALLOCATE(ITH(NTHU),VTHU(NTHU))
            READ(STR(POS:),*,ERR=3)(ITH(I),VTHU(I),I=1,NTHU)
         ELSEIF(STR(1:KEYEND)=='s'.OR.STR(1:KEYEND)=='sv')THEN
            IF(STR(1:KEYEND)=='s')THEN
               SFILE(1:2)='s.'
               READ(STR(POS:),*)SFILE(3:)
            ELSEIF(STR(1:KEYEND)=='sv')THEN
               READ(STR(POS:),*)SVFILE(3:)
            ENDIF
         ELSE
            WRITE(6,'(A,A,A,I2)')"Unknown AUTO constant ",STR(1:KEYEND),
     &           " on line ",LINE
         ENDIF
      ENDDO scanloop

 1    IF(EOF.AND..NOT.KEYS)GOTO 5
      BACKSPACE 2
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
      IPLT=IAP(11)
      NBC=IAP(12)
      NINT=IAP(13)
      NMX=IAP(14)
      NPR=IAP(16)
      MXBF=IAP(17)
      IID=IAP(18)
      ITMX=IAP(19)
      ITNW=IAP(20)
      NWTN=IAP(21)
      JAC=IAP(22)
      NPAR=IAP(23)
C
      DS=RAP(1)
      DSMIN=RAP(2)
      DSMAX=RAP(3)
      RL0=RAP(6)
      RL1=RAP(7)
      A0=RAP(8)
      A1=RAP(9)
      EPSL=RAP(11)
      EPSU=RAP(12)
      EPSS=RAP(13)

      READ(2,*,ERR=3,END=5) NDIM,IPS,IRS,ILP
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NICP
      IF(NICP.GT.0)THEN
        DEALLOCATE(ICU)
        ALLOCATE(ICU(NICP))
        BACKSPACE 2
        READ(2,*,ERR=3,END=4) NICP,(ICU(I),I=1,NICP)
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NMX,RL0,RL1,A0,A1
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) EPSL,EPSU,EPSS
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) DS,DSMIN,DSMAX,IADS
      DSMIN=ABS(DSMIN)
      DSMAX=ABS(DSMAX)
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NTHL
      IF(NTHL.GT.0)THEN
        DEALLOCATE(ITHL,VTHL)
        ALLOCATE(ITHL(NTHL),VTHL(NTHL))
        DO I=1,NTHL
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)ITHL(I),VTHL(I)
        ENDDO
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NTHU
      IF(NTHU.GT.0)THEN
        DEALLOCATE(ITH,VTHU)
        ALLOCATE(ITH(NTHU),VTHU(NTHU))
        DO I=1,NTHU
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)ITH(I),VTHU(I)
        ENDDO
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4)NUZR
      IF(NUZR.GT.0)THEN
        DEALLOCATE(IUZ,VUZ)
        ALLOCATE(IUZ(NUZR),VUZ(NUZR))
        DO I=1,NUZR
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)IUZ(I),VUZ(I)
        ENDDO
      ENDIF
C
C     we allocate THU (a pointer to the THU array in the
C     main program) here since this is the place where we 
C     know the size.  It is 8 times bigger then ndim since
C     INIT can modify THU based on the problem type,
C     but only up to making it 8 times larger.
C
 2    ALLOCATE(THU(8*NDIM+1))
      DO I=1,NDIM*8+1
        THU(I)=1.d0
      ENDDO
      IF(NTHU.GT.0)THEN
        DO I=1,NTHU
          THU(ITH(I))=VTHU(I)
        ENDDO
      ENDIF
      DEALLOCATE(ITH,VTHU)
C
      IAP(1)=NDIM
      IAP(2)=IPS
      IAP(3)=IRS
      IAP(4)=ILP
      IAP(5)=NTST
      IAP(6)=NCOL
      IAP(7)=IAD
      IAP(8)=IADS
      IAP(9)=ISP
      IAP(10)=ISW
      IAP(11)=IPLT
      IAP(12)=NBC
      IAP(13)=NINT
      IAP(14)=NMX
      IAP(15)=NUZR
      IAP(16)=NPR
      IAP(17)=MXBF
      IAP(18)=IID
      IAP(19)=ITMX
      IAP(20)=ITNW
      IAP(21)=NWTN      
      IAP(22)=JAC
C
      NDM=NDIM
      IF(NBC.NE.0) THEN
        NBC0=NBC
      ELSE
        NBC0=1
      ENDIF
      IF(NINT.NE.0)THEN
        NNT0=NINT
      ELSE
        NNT0=1
      ENDIF
      IUZR=1
      ITP=0
      ITPST=0
      NFPR=1
      IBR=1
      NTOT=0
      NINS=0
      LAB=0
C
      IAP(23)=NDM
      IAP(24)=NBC0
      IAP(25)=NNT0
      IAP(26)=IUZR
      IAP(27)=ITP
      IAP(28)=ITPST
      IAP(29)=NFPR
      IAP(30)=IBR
      IAP(31)=NPAR
      IAP(32)=NTOT
      IAP(33)=NINS
      IAP(37)=LAB
      IAP(41)=NICP
C
      RAP(1)=DS
      RAP(2)=DSMIN
      RAP(3)=DSMAX
      RAP(6)=RL0
      RAP(7)=RL1
      RAP(8)=A0
      RAP(9)=A1
C
      AMP=0.d0
      DET=0.d0
      FLDF=0.d0
      HBFF=0.d0
      BIFF=0.d0
      SPBF=0.d0
C
      RAP(10)=AMP
      RAP(11)=EPSL
      RAP(12)=EPSU
      RAP(13)=EPSS
      RAP(14)=DET
      RAP(16)=FLDF
      RAP(17)=HBFF
      RAP(18)=BIFF
      RAP(19)=SPBF
C
      EOF=.FALSE.
      RETURN
 3    WRITE(6,"(A,I2,A)")
     *     " Error in fort.2 or c. file: bad value on line ",
     *     LINE,"."
      STOP
 4    WRITE(6,"(A,I2,A)")
     *     " Error in fort.2 or c. file: ends prematurely on line ",
     *     LINE,"."
 5    BACKSPACE 2
      IF(.NOT.EOF.OR.KEYS)GOTO 2
      RETURN
      END SUBROUTINE INIT

C     ---------- ---------
      SUBROUTINE SCANVALUE(STR,POS,NPOS,LISTLEN)
      IMPLICIT NONE
C
C     Scans STR(POS:) for a value
C     NPOS points to the next keyword on the same line,
C       or is set to 1 if there is none
C     LISTLEN gives the number of items in lists delimited by []
C     [] characters are removed
C
      CHARACTER(*), INTENT(INOUT) :: STR
      INTEGER, INTENT(IN) :: POS
      INTEGER, INTENT(OUT) :: NPOS,LISTLEN

      INTEGER I,LEVEL
      CHARACTER(1) C,PREV,QUOTE
      LOGICAL QUOTEESC
      LISTLEN=1
      LEVEL=0
      QUOTE=' '
      QUOTEESC=.FALSE.
      PREV=' '

      NPOS=POS
      DO I=POS,LEN_TRIM(STR)
         NPOS=I
         C=STR(I:I)
         IF(QUOTE==' ')THEN
            SELECT CASE(C)
            CASE('"',"'")
               QUOTE=C
            CASE(',',' ')
               IF(LEVEL==0)EXIT
            CASE(']')
               STR(I:I)=' '
               LEVEL=LEVEL-1
            CASE DEFAULT
               IF((PREV==','.OR.PREV==' ').AND.LEVEL==1)THEN
                  LISTLEN=LISTLEN+1
               ENDIF
               IF(C=='[')THEN
                  STR(I:I)=' '
                  LEVEL=LEVEL+1
               ENDIF
            END SELECT
         ELSEIF(C==QUOTE)THEN
            ! ignore "" and ''
            IF(STR(I+1:I+1)==C.OR.QUOTEESC)THEN
               QUOTEESC=.NOT.QUOTEESC
            ELSE
               QUOTE=' '
            ENDIF
         ENDIF
         PREV=C
      ENDDO
      NPOS=NPOS+VERIFY(STR(NPOS:)," ,")-1
      IF(NPOS>=LEN_TRIM(STR))NPOS=1
      END SUBROUTINE SCANVALUE

C     ---------- -------
      SUBROUTINE CLEANUP()
C
C     Deallocate some globally allocated arrays.
C
      USE AUTO_CONSTANTS

      IMPLICIT NONE

      DEALLOCATE(THU,IUZ,VUZ,THL,ICU)
      END SUBROUTINE CLEANUP
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C               The leading subroutines of AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- -----
      SUBROUTINE INIT1(IAP,RAP,ICP,PAR)
C
      USE HOMCONT, ONLY:INHO
      USE AUTO_CONSTANTS, ONLY:ICU,ITHL,VTHL,THL,NTHL
C
      DOUBLE PRECISION, PARAMETER ::
     *     HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30
C
C General initialization. Redefinition of constants.
C
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION RAP(*),PAR(*)
C
C Local
      INTEGER NDIM,IPS,IRS,ILP,NCOL,ISP,ISW,NBC,NINT,NMX,NUZR
      INTEGER JAC,ITP,NFPR,NICP,NDM,NXP,I,J,NNEG,IC,JC
      DOUBLE PRECISION DS,DSMIN,DSMAX,FC
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       NCOL=IAP(6)
       ISP=IAP(9)
       ISW=IAP(10)
       NBC=IAP(12)
       NINT=IAP(13)
       NMX=IAP(14)
       NUZR=IAP(15)
       JAC=IAP(22)
       ITP=IAP(27)
       NFPR=IAP(29)
       NICP=IAP(41)
C
       DS=RAP(1)
       DSMIN=RAP(2)
       DSMAX=RAP(3)
C
       IF(ISW.EQ.0)ISW=1
C
C Check and perturb pseudo arclength stepsize and steplimits.
C (Perturbed to avoid exact computation of certain singular points).
C
       IF(DS.EQ.0.d0)DS=0.1
       IF(DSMIN.EQ.0.d0)DSMIN=1.0D-4*ABS(DS)
       FC=1.d0+HMACH
       DS=FC*DS
       DSMIN=DSMIN/FC
       DSMAX=FC*DSMAX
C
C Redefinition for waves
       IF(IPS.EQ.11)THEN
         IPS=1
         IAP(2)=IPS
         NDIM=2*NDIM
         NDM=NDIM
         IAP(23)=NDM
       ELSEIF(IPS.EQ.12)THEN
         IPS=2
         IAP(2)=IPS
         NDIM=2*NDIM
         NDM=NDIM
         IAP(23)=NDM
       ENDIF
C
C General Redefinition.
C
       IF(ABS(IPS).LE.1 .AND. ISW.EQ.1 )THEN
C        ** Algebraic Systems
         NFPR=1
C
       ELSE IF(IPS.EQ.-2)THEN
C        ** Time integration
         NFPR=1
         ISP=0
         ILP=0
         ICP(1)=14
C 
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.1 )THEN
C        ** Periodic Solutions
         NBC=NDIM
         NINT=1
         NFPR=NBC+NINT-NDIM+1
C        **ISW=1 when starting from a HB
         IF(ITP.EQ.3.OR.(ABS(ITP)/10).EQ.3)ISW=1
         IF(NICP.EQ.1)THEN
C          **Variable period
           ICP(2)=11
         ENDIF
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==1  ) THEN
C        ** Boundary value problems
         NFPR=NBC+NINT-NDIM+1
C
       ELSE IF( IPS.EQ.9 .AND. ABS(ISW).EQ.1  ) THEN
C        ** Homoclinic bifurcation analysis
C        Redefine AUTO constants for homoclinic orbits
         CALL INHO(IAP,ICP,PAR)
         NDIM=IAP(1)
         NBC=IAP(12)
         NINT=IAP(13)
         NUZR=IAP(15)
         NFPR=NBC+NINT-NDIM+1
C
       ELSE IF(IPS.EQ.14 .OR. IPS.EQ.16)THEN
C        **Evolution calculations for Parabolic Systems
         NDIM=2*NDIM
         NBC=NDIM
         NINT=0
         NFPR=1
         ILP=0
         ISP=0
         ICP(1)=14
C
       ELSE IF(IPS.EQ.17)THEN
C        **Stationary calculations for Parabolic Systems
         NDIM=2*NDIM
         NBC=NDIM
         NINT=0
         NFPR=1
C
         ELSE IF(IPS.EQ.15)THEN
C          ** Optimization of periodic solutions 
           NFPR=0
           DO I=1,NICP
             IF(ICU(I).GT.0)THEN
               NFPR=NFPR+1
               ICP(NFPR)=ICU(I)
             ENDIF
           ENDDO
           ICP(NFPR+1)=10
           ICP(NFPR+2)=13
           ICP(NFPR+3)=14
           NFPR=NFPR+3
           NDIM=2*NDIM
           NBC=NDIM
           NINT=NFPR-1
C overload to define optimality integrals
           NNEG=0
           DO I=1,NICP
             IC=ICU(I)
             JC=ABS(IC)-20        
             IF(IC.LT.0.AND.JC.GT.0.AND.JC.LE.11)THEN
               NNEG=NNEG+1
               ICP(NFPR+NNEG)=JC
             ENDIF
           ENDDO
C Set indices of output parameters
           NICP=NFPR-3
           DO I=1,NICP
             ICU(I)=ICP(I)
           ENDDO
C
       ELSE IF(IPS.EQ.5)THEN
C        ** Algebraic optimization Problems
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPR=NFPR+1
         IF(NFPR.EQ.2)THEN
           NDIM=NDIM+1
           ICP(1)=10
         ELSE
           NDIM=2*NDIM+NFPR
           ICP(1)=10
         ENDIF
C
       ELSE IF(IRS.GT.0 .AND. ABS(ISW).GE.2 )THEN
C        ** Continuation of singular points
C
         IF( ( ITP.EQ.2.OR.(ABS(ITP)/10).EQ.2 )
     *        .AND. ABS(IPS).LE.1)THEN
C          ** Fold continuation (Algebraic Problems)
           NDIM=2*NDIM+1
           NFPR=2
C
         ELSE IF( ( ITP.EQ.1.OR.(ABS(ITP)/10).EQ.1 )
     *        .AND. ABS(IPS).LE.1)THEN
C          ** BP cont (Algebraic Problems) (by F. Dercole)
           NDIM=2*NDIM+2
           NFPR=3
C
         ELSE IF((ITP.EQ.3.OR.(ABS(ITP)/10).EQ.3)
     *               .AND. ABS(IPS).LE.1 )THEN
C          ** Hopf bifurcation continuation (Maps, ODE, Waves)
           NDIM=3*NDIM+2
           NFPR=2
C
         ELSE IF( ITP==5 .AND. IPS==2 )THEN
C          ** Fold continuation (Periodic solutions); start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(2)=11
           ENDIF
           ICP(3)=13
           ICP(4)=12
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (ABS(ITP)/10)==5 .AND. IPS==2 )THEN
C          ** Fold continuation (Periodic solutions); restart
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(3)=ICP(2)
             ICP(2)=11
           ENDIF
           ICP(4)=12
C
         ELSE IF( (ITP==6) .AND. IPS==2)THEN
C          ** BP cont (Periodic solutions); start (by F. Dercole)
           NDIM=4*NDIM
           NBC=NDIM
           NINT=10
           NFPR=NBC+NINT-NDIM+1
           IF(((ABS(ISW)==2).AND.(ICP(3)==11 .OR. NICP==2)).OR.
     *        ((ABS(ISW)==3).AND.(ICP(4)==11 .OR. NICP==3)))THEN
C            ** Variable period
             ICP(2)=17 ! a
             ICP(3)=18 ! b
             ICP(4)=11 ! T
           ELSE
C            ** Fixed period
             ICP(3)=17 ! a
             ICP(4)=18 ! b
           ENDIF
           ICP(5)=12   ! q1
           ICP(6)=13   ! q2/beta1
           ICP(7)=14   ! r1
           ICP(8)=15   ! r2/beta2
           ICP(9)=16   ! psi^*_3
           ICP(10)=20  ! c1
           ICP(11)=21  ! c2
C
           ILP=0
           ISW=-ABS(ISW)
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (ABS(ITP)/10==6) .AND. IPS==2)THEN
C          ** BP cont (Periodic solutions); restart 1 or 2
           NDIM=2*NDIM
           NBC=NDIM
           NINT=4
           NFPR=NBC+NINT-NDIM+1
           IF(ABS(ISW)==2)THEN
C            ** Non-generic case
             IF(ICP(3)==11 .OR. NICP==2)THEN
C              ** Variable period
               ICP(3)=18 ! b
               ICP(4)=11 ! T
             ELSE
C              ** Fixed period
               ICP(4)=18 ! b
             ENDIF
           ELSE
C            ** Generic case
             IF(ICP(4)==11 .OR. NICP==3)THEN
C              ** Variable period
               ICP(4)=11 ! T
             ENDIF
           ENDIF
           ICP(5)=16     ! psi^*_3
C
         ELSE IF(ITP.EQ.7 .AND. (IPS.EQ.2 .OR. IPS.EQ.7))THEN
C          ** Continuation of period doubling bifurcations; start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=2
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(2)=11
             ICP(3)=13
           ELSE
C            ** Fixed period
             ICP(3)=13
           ENDIF
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF(ABS(ITP)/10.EQ.7 .AND. (IPS.EQ.2 .OR. IPS.EQ.7))THEN
C          ** Continuation of period doubling bifurcations; restart
           NDIM=2*NDIM
           NBC=NDIM
           NINT=2
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(3)=11
           ENDIF
C
         ELSE IF(ITP.EQ.8 .AND. IPS.EQ.2)THEN
C          ** Continuation of torus bifurcations; start
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           ICP(2)=11
           ICP(3)=12
           ICP(4)=13
           ILP=0
           ISP=0
           ISW=-2
           NMX=5
           WRITE(6,101)
C
         ELSE IF(ABS(ITP)/10.EQ.8 .AND. IPS.EQ.2)THEN
C          ** Continuation of torus bifurcations; restart
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           ICP(3)=11
           ICP(4)=12
C
         ELSE IF( (ITP==5) .AND. (IPS==4.OR.IPS==7) )
     *   THEN
C          ** Continuation of folds (BVP; start)
           NDIM=2*NDIM
           NBC=2*NBC
           NINT=2*NINT+1
           NFPR=NBC+NINT-NDIM+1
           NXP=NFPR/2-1
           IF(NXP.GT.0)THEN
             DO I=1,NXP
               ICP(NFPR/2+I+1)=11+I
             ENDDO
           ENDIF
           ICP(NFPR/2+1)=11+NFPR/2
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (ABS(ITP)/10)==5 .AND. (IPS==4.OR.IPS==7))THEN
C          ** Continuation of folds (BVP; restart)
           NDIM=2*NDIM
           NBC=2*NBC
           NINT=2*NINT+1
           NFPR=NBC+NINT-NDIM+1
           NXP=NFPR/2-1
           IF(NXP.GT.0)THEN
             DO I=1,NXP
               ICP(NFPR/2+I+1)=11+I
             ENDDO
           ENDIF
C
         ELSE IF( ITP==6 .AND. (IPS==4.OR.IPS==7) )THEN
C          ** BP cont (BVP; start) (by F. Dercole)
           NXP=NBC+NINT-NDIM+1
           NDIM=4*NDIM
           NBC=3*NBC+NDIM/2+NXP
           NINT=3*NINT+NXP+5
           NFPR=NBC+NINT-NDIM+1
           ICP(NXP+1)=11+3*NXP+NDIM/4   ! a
           ICP(NXP+2)=11+3*NXP+NDIM/4+1 ! b
           DO I=1,NXP
             ICP(NXP+I+2)=11+I          ! q
             ICP(2*NXP+I+2)=11+NXP+I    ! r
             ICP(4*NXP+NDIM/4+I+3)=11+3*NXP+NDIM/4+3+I ! d
           ENDDO
           DO I=1,NXP+NDIM/4-1
             ICP(3*NXP+I+2)=11+2*NXP+I  ! psi^*_2,psi^*_3
           ENDDO
           ICP(4*NXP+NDIM/4+2)=11+3*NXP+NDIM/4+2 ! c1
           ICP(4*NXP+NDIM/4+3)=11+3*NXP+NDIM/4+3 ! c2
C
           ILP=0
           ISW=-ABS(ISW)
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (ABS(ITP)/10)==6 .AND. (IPS==4.OR.IPS==7))THEN
C          ** BP cont (BVP; restart 1 or 2)
           NXP=NBC+NINT-NDIM+1
           NDIM=2*NDIM
           NBC=NBC+NDIM+NXP
           NINT=NINT+NXP+1
           NFPR=NBC+NINT-NDIM+1
           IF(ABS(ISW)==2)THEN
C            ** Non-generic case
             ICP(NXP+2)=11+3*NXP+NDIM/2+1 ! b
           ENDIF
           DO I=1,NXP+NDIM/2-1
             ICP(NXP+I+2)=11+2*NXP+I      ! psi^*_2,psi^*_3
           ENDDO
           DO I=1,NXP
             ICP(2*NXP+NDIM/2+I+1)=11+3*NXP+NDIM/2+3+I ! d
           ENDDO
C
         ENDIF
C
       ENDIF

C     redefine nthl to be nfpr sized and indexed
       ALLOCATE(THL(NFPR))
       DO I=1,NFPR
         THL(I)=1.0D0
         DO J=1,NTHL
           IF(ICP(I)==ITHL(J))THEN
             THL(I)=VTHL(J)
           ENDIF
         ENDDO
       ENDDO
       DEALLOCATE(ITHL,VTHL)
C
       IAP(1)=NDIM
       IAP(2)=IPS
       IAP(3)=IRS
       IAP(4)=ILP
       IAP(6)=NCOL
       IAP(9)=ISP
       IAP(10)=ISW
       IAP(12)=NBC
       IAP(13)=NINT
       IAP(14)=NMX
       IAP(15)=NUZR
       IAP(22)=JAC
       IAP(29)=NFPR
       IAP(41)=NICP
C
       RAP(1)=DS
       RAP(2)=DSMIN
       RAP(3)=DSMAX
C
 101   FORMAT(/,' Generating starting data :',
     *          ' Restart at EP label below :')

      RETURN
      END SUBROUTINE INIT1

      END PROGRAM AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
