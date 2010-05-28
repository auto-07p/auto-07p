C     ------- ----
      PROGRAM AUTO
C
      USE AUTOMPI
      USE IO
      USE SUPPORT, ONLY:AP=>AV, CHECKSP
      USE AUTO_CONSTANTS,ONLY:SVFILE,SFILE,DATFILE,EFILE,
     *     ICU,parnames,AUTOPARAMETERS
C$    USE OMP_LIB
      USE COMPAT
C
      IMPLICIT NONE
C
      LOGICAL EOF,KEYS
C Local
      DOUBLE PRECISION TIME0,TIME1,TOTTIM
      INTEGER I,LINE,ios
      INTEGER,ALLOCATABLE :: IICU(:)
      LOGICAL FIRST
      CHARACTER(258) :: SOLFILE, BIFFILE, DIAFILE
C
C Initialization :
C
       CALL MPIINI()
       IF(MPIIAM()/=0)THEN
         CALL MPIWORKER(AP)
         STOP
       ENDIF
C
       FIRST=.TRUE.
       EFILE=''
       SFILE=''
       SVFILE=''
       DATFILE=''
       BIFFILE='fort.7'
       SOLFILE='fort.8'
       DIAFILE='fort.9'
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential',IOSTAT=ios)
       IF(ios/=0)THEN
          WRITE(6,'(A,A)')'The constants file (fort.2 or c. file) ',
     *         'could not be found.'
          STOP
       ENDIF
C
       KEYS=.FALSE.
       LINE=0
 1     IF(MPIKWT()>1)THEN
         CALL MPITIM(TIME0)
       ELSE
         TIME0=AUTIM()
C$       TIME0=omp_get_wtime()
       ENDIF
       CALL INIT(AP,EOF,KEYS,LINE)
       IF(EOF)THEN
         CALL MPIEND()
         STOP
       ENDIF
       CALL FINDLB_OR_STOP(AP)
       CALL MPIIAP(AP)
       ALLOCATE(IICU(SIZE(ICU)))
       DO I=1,SIZE(ICU)
          IICU(I)=NAMEIDX(ICU(I),parnames)
       ENDDO
       CALL AUTOI(AP,IICU,SIZE(IICU),.FALSE.)
       DEALLOCATE(IICU)
C-----------------------------------------------------------------------
C
      IF(MPIKWT()>1)THEN
        CALL MPITIM(TIME1)
      ELSE
        TIME1=AUTIM()
C$      TIME1=omp_get_wtime()
      ENDIF
      TOTTIM=TIME1-TIME0
      IF(AP%IID>0)THEN
         CALL WRBAR("=",47)
         WRITE(9,301)TOTTIM
      ENDIF
      WRITE(6,301)TOTTIM
      CALL CLEANUP()
      GOTO 1
C
 301  FORMAT(/,' Total Time ',E12.3)
C
      CONTAINS
C
C     ---------- ---------
      SUBROUTINE MPIWORKER(AP)
      
      USE AUTOMPI
      IMPLICIT NONE

      TYPE(AUTOPARAMETERS) AP
      INTEGER ICU(1),IPS,IRS,ISW

      INTEGER FUNI_ICNI_PARAMS(5)

      DO WHILE(.TRUE.)
         CALL MPIBCASTI(FUNI_ICNI_PARAMS,5)
         ! figure out what funi and icni are from
         ! the iap array. We do it here, since I
         ! don't know how to pass function pointers
         ! through MPI in a possibly heterogeneous 
         ! environment :-)
         IPS     = FUNI_ICNI_PARAMS(1)
         AP%IPS  = IPS
         IRS     = FUNI_ICNI_PARAMS(2)
         AP%IRS  = IRS
         ISW     = FUNI_ICNI_PARAMS(3)
         AP%ISW = ISW
         AP%ITP = FUNI_ICNI_PARAMS(4) ! itp
         AP%NFPR = FUNI_ICNI_PARAMS(5) ! nfpr
         CALL AUTOI(AP,ICU,SIZE(ICU),.TRUE.)
         ! autoi calls autobv which eventually calls solvbv;
         ! a return means another init message
      ENDDO
      END SUBROUTINE MPIWORKER
C
C     ---------- --------------
      SUBROUTINE FINDLB_OR_STOP(AP)
C
C Find restart label and determine type of restart point.
C or stop otherwise
C
      USE AUTO_CONSTANTS, ONLY: SIRS
      IMPLICIT NONE
      TYPE(AUTOPARAMETERS) AP
      CHARACTER(258) FILE

      INTEGER NFPR,NPARR,IRS
      LOGICAL FOUND

      IRS=AP%IRS

      FOUND=.FALSE.
      IF(IRS/=0) THEN
         IF(LEN_TRIM(SFILE)==0)THEN
            FILE='fort.3'
         ELSE
            FILE='s.'//SFILE
         ENDIF
         CALL FINDLB(FILE,AP,IRS,NFPR,NPARR,FOUND)
         AP%IRS=IRS
         AP%NFPR=NFPR
         IF(.NOT.FOUND) THEN
            WRITE(6,"(' Restart label ',A,' not found')")TRIM(SIRS)
            STOP
         ENDIF
         AP%NPAR=MAX(NPARR,AP%NPAR)
      ENDIF
      END SUBROUTINE FINDLB_OR_STOP
C
C     ---------- -----
      SUBROUTINE AUTOI(AP,ICU,NICU,WORKER)
C
      USE INTERFACES
      USE AUTO_CONSTANTS, ONLY: IVTHL,IVTHU,IVUZR,NBC,NINT,NDIM,unames,
     &     parnames,NPARX
      USE IO, ONLY: NAMEIDX
      USE AE
      USE BVP
      USE HOMCONT, ONLY:FNHO,BCHO,ICHO,PVLSHO,STPNHO
C
      IMPLICIT NONE
      TYPE(AUTOPARAMETERS) AP
      INTEGER NICU,ICU(NICU)
      LOGICAL WORKER

      INTEGER IPS,IRS,ISW,ITP,NFPRPREV,NFPR,NNICP,NPAR,NDIMA,IND,I,J,K
      INTEGER ILP,ISP
      INTEGER NUZR,NPARI,NICP
      INTEGER, ALLOCATABLE :: ICP(:),IUZ(:)
      DOUBLE PRECISION, ALLOCATABLE :: PAR(:),THL(:),THU(:),VUZ(:)

      IPS=AP%IPS
      IRS=AP%IRS
      ILP=AP%ILP
      ISP=AP%ISP
      ISW=AP%ISW
      NUZR=AP%NUZR
      ITP=AP%ITP
      NFPRPREV=AP%NFPR
C
      IF(.NOT.WORKER)THEN
        NNICP=MAX(5*(NBC+NINT-NDIM+1)+NDIM+NINT+3,5*SIZE(ICU)+NDIM+3)
        ALLOCATE(ICP(NNICP))
        ICP(:SIZE(ICU))=ICU(:)
        ICP(SIZE(ICU)+1:)=0
        NPAR=AP%NPAR
        NPAR=MAX(MAXVAL(ABS(ICU)),NPAR)
        AP%NPAR=NPAR
        CALL INIT1(AP,ICP,ICU)
        ! check output (user-specified) parameters
        NICP=AP%NICP
        DO I=1,NICP
           IF(ICU(I)<=0)THEN
              WRITE(6,'(A,I5,A,I5)')
     &             "Invalid parameter index ",ICP(I),
     &             " specified in ICP index ",I
              STOP
           ENDIF
        ENDDO
        ! check active continuation parameters
        NFPR=AP%NFPR
        DO I=SIZE(ICU)+1,NFPR
           IF(ICP(I)==0)THEN
              WRITE(6,'(A/A,I5,A,I5,A)')
     &             "Insufficient number of parameters in ICP.",
     &             "You specified ",SIZE(ICU)," but need at least ",
     &             NFPR-I+1+SIZE(ICU), " continuation parameters."
              STOP
           ENDIF
        ENDDO
        NPARI=AP%NPARI
        NPAR=AP%NPAR
        NPAR=MAX(MAXVAL(ICP(:NFPR)),NPAR+NPARI)
        IF(ABS(IPS)==1.OR.IPS==2.OR.IPS>=7)THEN
           !HB period and period for periodic orbits stored in PAR(11)
           NPAR=MAX(11,NPAR)
           IF(CHECKSP(8,IPS,ILP,ISP))THEN
              ! the torus angle in stored in PAR(12)
              NPAR=MAX(12,NPAR)
           ENDIF
        ENDIF
        AP%NPAR=NPAR
        ! allocate a minimum of NPARX so we can detect overflows 
        ! past NPAR gracefully
        ALLOCATE(PAR(MAX(NPAR,NPARX)))
        PAR(:)=0.d0
C     redefine thl to be nfpr sized and indexed
        ALLOCATE(THL(NFPR))
        DO I=1,NFPR
           THL(I)=1.0D0
           DO J=1,SIZE(IVTHL)
              IF(ICP(I)==NAMEIDX(IVTHL(J)%INDEX,parnames))THEN
                 THL(I)=IVTHL(J)%VAR
              ENDIF
           ENDDO
        ENDDO
C     set thu to 1 higher than NDIM for (u,par) representation in ae.f90
        NDIMA=AP%NDIM ! active NDIM from INIT1
        ALLOCATE(THU(NDIMA+1))
        DO I=1,NDIMA
           THU(I)=1.d0
        ENDDO
        DO I=1,SIZE(IVTHU)
           THU(NAMEIDX(IVTHU(I)%INDEX,unames))=IVTHU(I)%VAR
        ENDDO
C     set IUZ/VUZ
        ALLOCATE(IUZ(NUZR),VUZ(NUZR))
        K=0
        DO I=1,SIZE(IVUZR)
           IND=NAMEIDX(IVUZR(I)%INDEX,parnames)
           DO J=1,SIZE(IVUZR(I)%VAR)
              K=K+1
              IUZ(K)=IND
              VUZ(K)=IVUZR(I)%VAR(J)
           ENDDO
        ENDDO

        ! only now open the output files
        IF(FIRST)THEN
           IF(SVFILE/='')THEN
              BIFFILE='b.'//SVFILE
              SOLFILE='s.'//SVFILE
              DIAFILE='d.'//SVFILE
           ENDIF
           OPEN(7,FILE=BIFFILE,STATUS='unknown',ACCESS='sequential')
           OPEN(8,FILE=SOLFILE,STATUS='unknown',ACCESS='sequential')
           OPEN(9,FILE=DIAFILE,STATUS='unknown',ACCESS='sequential')
           FIRST=.FALSE.
        ENDIF
      ELSE
        ! ignored for MPI workers
        ALLOCATE(ICP(1),PAR(1),THU(1),THL(1),IUZ(1),VUZ(1))
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
           CALL AUTOAE(AP,PAR,ICP,ICU,FUNI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(AP,PAR,ICP,ICU,FUNI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.11 .AND. ABS(ISW).LE.1 ) THEN
C        ** Waves : Spatially homogeneous solutions,
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(AP,PAR,ICP,ICU,FNWS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(AP,PAR,ICP,ICU,FNWS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF((IPS.EQ.-1) .AND. ABS(ISW).LE.1 ) THEN
C        ** Discrete dynamical systems : fixed points.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(AP,PAR,ICP,ICU,FNDS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(AP,PAR,ICP,ICU,FNDS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.-2) THEN
C        ** Time integration.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(AP,PAR,ICP,ICU,FNTI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(AP,PAR,ICP,ICU,FNTI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).LE.1 ) THEN
C        ** Periodic solutions
         CALL AUTOBV(AP,PAR,ICP,ICU,FNPS,BCPS,ICPS,STPNPS,
     *     PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.12 .AND. ABS(ISW).LE.1 ) THEN
C        ** Wave train solutions to parabolic systems.
         CALL AUTOBV(AP,PAR,ICP,ICU,FNWP,BCPS,ICPS,STPNPS,
     *     PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)<=1) THEN
C        ** Boundary value problems. (4)
C        ** Boundary value problems with Floquet multipliers. (7)
          CALL AUTOBV(AP,PAR,ICP,ICU,FUNI,BCNI,ICNI,STPNPS,
     *         PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.9 .AND. ABS(ISW).LE.1) THEN
C        ** Homoclinic bifurcation analysis.
          CALL AUTOBV(AP,PAR,ICP,ICU,FNHO,BCHO,ICHO,STPNHO,
     *         PVLSHO,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.14) THEN
C        ** Evolution calculations for parabolic systems.
C           (Periodic boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(AP,PAR,ICP,ICU,FNPE,BCPS,ICPE,STPNBV,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(AP,PAR,ICP,ICU,FNPE,BCPS,ICPE,STPNUB,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.15.AND.ABS(ISW).EQ.1) THEN
C        ** Optimization of periodic solutions.
         IF(NFPRPREV.LT.6)THEN
           CALL AUTOBV(AP,PAR,ICP,ICU,FNPO,BCPO,ICPO,STPNPO,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(AP,PAR,ICP,ICU,FNPO,BCPO,ICPO,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.16) THEN
C        ** Evolution calculations for parabolic systems.
C           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(AP,PAR,ICP,ICU,FNPE,BCNI,ICPE,STPNBV,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(AP,PAR,ICP,ICU,FNPE,BCNI,ICPE,STPNUB,
     *      PVLSPE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.17) THEN
C        ** Continuation of stationary states of parabolic systems.
C           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(AP,PAR,ICP,ICU,FNSP,BCNI,ICPE,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(AP,PAR,ICP,ICU,FNSP,BCNI,ICPE,STPNUB,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.5) THEN
C        ** Algebraic optimization problems.
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPRPREV=NFPRPREV+1
         IF(NFPRPREV.EQ.2) THEN
           IF(IRS.GT.0) THEN
            CALL AUTOAE(AP,PAR,ICP,ICU,FNC1,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
            CALL AUTOAE(AP,PAR,ICP,ICU,FNC1,STPNC1,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           IF(MOD(ITP,10).NE.2) THEN
            CALL AUTOAE(AP,PAR,ICP,ICU,FNC2,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
            CALL AUTOAE(AP,PAR,ICP,ICU,FNC2,STPNC2,THL,THU,IUZ,VUZ)
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
 2     IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 .AND. (ITP.EQ.2.OR.ITP.EQ.7) )
     * THEN
C        ** Fold/PD continuation (algebraic problems).
         CALL AUTOAE(AP,PAR,ICP,ICU,FNLP,STPNLP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 
     *         .AND. (ABS(ITP)/10.EQ.2 .OR. ABS(ITP)/10.EQ.7))
     * THEN
C        ** Fold/PD continuation (algebraic problems, restart).
         CALL AUTOAE(AP,PAR,ICP,ICU,FNLP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 .AND. (ITP.EQ.1) )
     * THEN
C        ** BP cont (algebraic problems) (by F. Dercole).
         CALL AUTOAE(AP,PAR,ICP,ICU,FNBP,STPNBP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 
     *         .AND. ( (ABS(ITP)/10).EQ.1 ) )
     * THEN
C        ** BP cont (algebraic problems, restart).
         CALL AUTOAE(AP,PAR,ICP,ICU,FNBP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF((ABS(IPS)<=1.OR.IPS==11).AND.ABS(ISW)==2.AND.
     *        (ITP==3.OR.ITP==8) )
     * THEN
C        ** Hopf/Neimark-Sacker bifurcation continuation (ODE/waves/maps).
         CALL AUTOAE(AP,PAR,ICP,ICU,FNHB,STPNHB,THL,THU,IUZ,VUZ)
C
       ELSE IF((ABS(IPS)<=1.OR.IPS==11).AND.ABS(ISW)==2.AND.
     *        (ABS(ITP)/10==3.OR.ABS(ITP)/10==8)) THEN
C        ** Hopf/NS bifurcation continuation (ODE/waves/maps, restart).
         CALL AUTOAE(AP,PAR,ICP,ICU,FNHB,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2.OR.IPS==12).AND.ABS(ISW)==2.AND.ITP==5) THEN 
C        ** Fold continuation (Periodic solutions, start).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNPL,BCPL,ICPL,STPNPL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2.OR.IPS==12).AND.ABS(ISW)==2.AND.(ABS(ITP)/10)==5)
     * THEN
C        ** Fold continuation (Periodic solutions, restart).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNPL,BCPL,ICPL,STPNBV,
     *   PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2.OR.IPS==12) .AND. ABS(ISW)>=2 .AND. 
     *         (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
C        ** BP cont (Periodic sol., start and restart) (by F. Dercole).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNPBP,BCPBP,ICPBP,STPNPBP,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2.OR.IPS==7.OR.IPS==12)
     *      .AND. ABS(ISW)==2 .AND. ITP==7 ) THEN
C        ** Continuation of period doubling bifurcations (start).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNPD,BCPD,ICPD,STPNPD,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2 .OR. IPS==7 .OR. IPS==12)
     *      .AND. ABS(ISW)==2 .AND. (ABS(ITP)/10)==7)
     * THEN
C        ** Continuation of period doubling bifurcations (restart).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNPD,BCPD,ICPD,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2.OR.IPS==12).AND.ABS(ISW)==2.AND.ITP==8) THEN
C        ** Continuation of torus bifurcations (start).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNTR,BCTR,ICTR,STPNTR,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==2.OR.IPS==12).AND.ABS(ISW)==2.AND.(ABS(ITP)/10)==8)
     * THEN
C        ** Continuation of torus bifurcations (restart).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNTR,BCTR,ICTR,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN
C        ** Continuation of folds (BVP, start).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNBL,BCBL,ICBL,STPNBL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. 
     *         (ABS(ITP)/10)==5 ) THEN
C        ** Continuation of folds (BVP, restart).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNBL,BCBL,ICBL,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)>=2 .AND.
     *          (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
C        ** BP cont (BVP, start and restart) (by F. Dercole).
         CALL AUTOBV(AP,PAR,ICP,ICU,FNBBP,BCBBP,ICBBP,STPNBBP,
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
      DEALLOCATE(ICP,PAR,THL,THU,IUZ,VUZ)

      END SUBROUTINE AUTOI
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Initialization
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE INIT(AP,EOF,KEYS,LINE)
C
      USE AUTO_CONSTANTS
      USE HOMCONT, ONLY : INSTRHO
C
      IMPLICIT NONE
C
C Reads the file of continuation constants
C
      TYPE(AUTOPARAMETERS), INTENT(OUT) :: AP
      LOGICAL, INTENT(OUT) :: EOF
      LOGICAL, INTENT(INOUT) :: KEYS
      INTEGER, INTENT(INOUT) :: LINE
C
      INTEGER IAP(23)
      DOUBLE PRECISION RAP(13)
      INTEGER IBR,I,J,NFPR,NDM
      INTEGER NINS,LAB,NTOT,ITP,ITPST,NUZR,NICP,NPARI,ITDS
      DOUBLE PRECISION BIFF,DET,SPBF,HBFF,FLDF
      CHARACTER(LEN=2048) :: STR
      CHARACTER(LEN=1) :: C,QUOTE,PREV
      LOGICAL QUOTEESC
      INTEGER KEYEND,POS,LISTLEN,NPOS,LISTLEN2,IERR,ios

      TYPE INDEXSTRL
         CHARACTER(13) INDEX
         CHARACTER(2048) STRL
      END TYPE INDEXSTRL
      TYPE(INDEXSTRL),ALLOCATABLE :: IVUZRS(:)

      CHARACTER(LEN=*), PARAMETER :: ICONSTANTS(23) = (/
     * "NDIM", "IPS ", "    ", "ILP ", "NTST", "NCOL", "IAD ", "IADS",
     * "ISP ", "ISW ", "IPLT", "NBC ", "NINT", "NMX ", "    ", "NPR ",
     * "MXBF", "IID ", "ITMX", "ITNW", "NWTN", "JAC ", "NPAR" /)
      INTEGER, PARAMETER :: IDEFAULTS(23) = (/
     * 2, 1, 0, 1, 20, 4, 3, 1,
     * 2, 1, 0, 0, 0, 99999, 0, 99999,
     * 10, 2, 9, 5, 3, 0, NPARX /)
      CHARACTER(LEN=*), PARAMETER :: RCONSTANTS(13) = (/
     * "DS   ", "DSMIN", "DSMAX", "     ", "     ", "RL0  ", "RL1  ",
     * "A0   ", "A1   ", "     ", "EPSL ", "EPSU ", "EPSS " /)
      DOUBLE PRECISION, PARAMETER :: RDEFAULTS(13) = (/
     * 0.01d0, 0.005d0, 0.1d0, 0d0, 0d0, -1d300, 1d300, -1d300, 1d300,
     * 0d0, 1d-7, 1d-7, 1d-5 /)
C
      IF(KEYS)THEN
         EOF=.TRUE.
         RETURN
      ENDIF
      IAP(:)=IDEFAULTS(:)
      RAP(:)=RDEFAULTS(:)
      RAP(6)=-HUGE(1d0)*0.99995d0 !avoid rounding up in sthd
      RAP(7)=HUGE(1d0)*0.99995d0
      RAP(8)=-HUGE(1d0)*0.99995d0
      RAP(9)=HUGE(1d0)*0.99995d0
      NICP=1
      ALLOCATE(ICU(1),IVUZR(0),IVTHU(0),parnames(0),unames(0),SP(0))
      ALLOCATE(STOPS(0),UVALS(0),PARVALS(0))
      ICU(1)='1'
      NUZR=0

      NPOS=1
      KEYS=.FALSE.
      EOF=.FALSE.
      scanloop: DO
         IF(NPOS==1)THEN
            LINE=LINE+1
            READ(2,'(A)',END=5) STR
            QUOTE=' '
            QUOTEESC=.FALSE.
            DO I=1,LEN_TRIM(STR)
               C=STR(I:I)
               IF(QUOTE==' ')THEN
                  ! replace a tab with a spaces if not in a string
                  IF(IACHAR(C)==9)THEN
                     STR(I:I)=' '
                  ELSEIF(C=="'".OR.C=='"')THEN
                     QUOTE=STR(I:I)
                  ENDIF
               ELSEIF(C==QUOTE)THEN
                  ! ignore "" and ''
                  IF(STR(I+1:I+1)==C.OR.QUOTEESC)THEN
                     QUOTEESC=.NOT.QUOTEESC
                  ELSE
                     QUOTE=' '
                  ENDIF
               ENDIF
            ENDDO
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
               NEWCFILE=.TRUE.
               EXIT
            ELSE
               EXIT scanloop
            ENDIF
            IF(I==LEN_TRIM(STR))THEN
               NPOS=1
               CYCLE scanloop
            ENDIF
         ENDDO
         ! look for = after keyword
         KEYEND=SCAN(STR,'= ')-1
         IF(KEYEND==-1)THEN
            LINE=LINE-1
            EXIT scanloop
         ENDIF
         POS=SCAN(STR,'=')+1
         STR(POS:)=ADJUSTL(STR(POS:))
         CALL SCANVALUE(STR(POS:),NPOS,LISTLEN)
         IF(NPOS/=1)THEN
            NPOS=NPOS+POS-1
         ENDIF
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
         SELECT CASE(STR(1:KEYEND))
         CASE('IRS')
            READ(STR(POS:),*,ERR=3)SIRS
            READ(SIRS,*,IOSTAT=ios)IAP(3)
            IF(ios/=0)IAP(3)=1
         CASE('ICP')
            NICP=LISTLEN
            DEALLOCATE(ICU)
            ALLOCATE(ICU(NICP))
            READ(STR(POS:),*,ERR=3)ICU            
         CASE('UZR')
            ALLOCATE(IVUZRS(LISTLEN))
            READ(STR(POS:),*,ERR=3)IVUZRS
            DO I=1,SIZE(IVUZR)
               DEALLOCATE(IVUZR(I)%VAR)
            ENDDO
            DEALLOCATE(IVUZR)
            ALLOCATE(IVUZR(LISTLEN))
            NUZR=0
            DO I=1,LISTLEN
               PREV=' '
               LISTLEN2=0
               DO J=1,LEN_TRIM(IVUZRS(I)%STRL)
                  C=IVUZRS(I)%STRL(J:J)
                  IF(C/=' '.AND.C/=','.AND.(PREV==' '.OR.PREV==','))THEN
                     LISTLEN2=LISTLEN2+1
                  ENDIF
                  PREV=C
               ENDDO
               ALLOCATE(IVUZR(I)%VAR(LISTLEN2))
               IVUZR(I)%INDEX=IVUZRS(I)%INDEX
               READ(IVUZRS(I)%STRL,*,ERR=3)IVUZR(I)%VAR
               NUZR=NUZR+LISTLEN2
            ENDDO
            DEALLOCATE(IVUZRS)
         CASE('THL')
            IF(ALLOCATED(IVTHL))DEALLOCATE(IVTHL)
            ALLOCATE(IVTHL(LISTLEN))
            READ(STR(POS:),*,ERR=3)IVTHL
         CASE('THU')
            DEALLOCATE(IVTHU)
            ALLOCATE(IVTHU(LISTLEN))
            READ(STR(POS:),*,ERR=3)IVTHU
         CASE('SP')
            IF(ALLOCATED(SP))DEALLOCATE(SP)
            ALLOCATE(SP(LISTLEN))
            READ(STR(POS:),*,ERR=3)SP
         CASE('STOP')
            IF(ALLOCATED(STOPS))DEALLOCATE(STOPS)
            ALLOCATE(STOPS(LISTLEN))
            READ(STR(POS:),*,ERR=3)STOPS
         CASE('PAR')
            IF(ALLOCATED(PARVALS))DEALLOCATE(PARVALS)
            ALLOCATE(PARVALS(LISTLEN))
            READ(STR(POS:),*,ERR=3)PARVALS
         CASE('U')
            IF(ALLOCATED(UVALS))DEALLOCATE(UVALS)
            ALLOCATE(UVALS(LISTLEN))
            READ(STR(POS:),*,ERR=3)UVALS
         CASE('parnames')
            IF(ALLOCATED(parnames))DEALLOCATE(parnames)
            ALLOCATE(parnames(LISTLEN))
            READ(STR(POS:),*,ERR=3)parnames
         CASE('unames')
            IF(ALLOCATED(unames))DEALLOCATE(unames)
            ALLOCATE(unames(LISTLEN))
            READ(STR(POS:),*,ERR=3)unames
         CASE('s')
            READ(STR(POS:),*)SFILE
         CASE('dat')
            READ(STR(POS:),*)DATFILE
         CASE('sv')
            READ(STR(POS:),*)SVFILE
         CASE('e')
            READ(STR(POS:),*)EFILE
         CASE DEFAULT
            CALL INSTRHO(STR(1:KEYEND),STR(POS:),LISTLEN,IERR)
            IF(IERR==3)GOTO 3
            IF(IERR==1)THEN
               WRITE(6,'(A,A,A,I2)')"Unknown AUTO constant ",
     &              STR(1:KEYEND)," on line ",LINE
            ENDIF
         END SELECT
      ENDDO scanloop

 1    NDIM=IAP(1)
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

      IF(EOF)GOTO 2 ! completely new-style, just keys
      BACKSPACE 2
      READ(2,*,ERR=3,END=4) NDIM,IPS,SIRS,ILP
      READ(SIRS,*,IOSTAT=ios)IRS
      IF(ios/=0)IRS=1
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
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) LISTLEN
      !allocate: no THL vs. non-allocated:default THL (in SUB. INIT1)
      IF(ALLOCATED(IVTHL))DEALLOCATE(IVTHL)
      ALLOCATE(IVTHL(LISTLEN))
      IF(LISTLEN>0)THEN
        DO I=1,LISTLEN
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)IVTHL(I)
        ENDDO
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) LISTLEN
      IF(LISTLEN>0)THEN
        DEALLOCATE(IVTHU)
        ALLOCATE(IVTHU(LISTLEN))
        DO I=1,LISTLEN
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)IVTHU(I)
        ENDDO
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4)NUZR
      IF(NUZR>0)THEN
        DO I=1,SIZE(IVUZR)
           DEALLOCATE(IVUZR(I)%VAR)
        ENDDO
        DEALLOCATE(IVUZR)
        ALLOCATE(IVUZR(NUZR))
        DO I=1,NUZR
          LINE=LINE+1
          ALLOCATE(IVUZR(I)%VAR(1))
          READ(2,*,ERR=3,END=4)IVUZR(I)%INDEX,IVUZR(I)%VAR(1)
        ENDDO
      ENDIF
      KEYS=.FALSE.
C
 2    AP%NDIM=NDIM
      AP%IPS=IPS
      AP%IRS=IRS
      AP%ILP=ILP
      AP%NTST=NTST
      AP%NCOL=NCOL
      AP%IAD=IAD
      AP%IADS=IADS
      AP%ISP=ISP
      AP%ISW=ISW
      AP%IPLT=IPLT
      AP%NBC=NBC
      AP%NINT=NINT
      AP%NMX=NMX
      AP%NUZR=NUZR
      AP%NPR=NPR
      AP%MXBF=MXBF
      AP%IID=IID
      AP%ITMX=ITMX
      AP%ITNW=ITNW
      AP%NWTN=NWTN      
      AP%JAC=JAC
C
      NDM=NDIM
      NPARI=0
      ITDS=1
      ITP=0
      ITPST=0
      NFPR=1
      IBR=1
      NTOT=0
      NINS=0
      LAB=0
C
      AP%NDM=NDM
      AP%NPARI=NPARI
      AP%ITDS=ITDS
      AP%ITP=ITP
      AP%ITPST=ITPST
      AP%NFPR=NFPR
      AP%IBR=IBR
      AP%NPAR=NPAR
      AP%NTOT=NTOT
      AP%NINS=NINS
      AP%LAB=LAB
      AP%NICP=NICP
C
      AP%DS=DS
      AP%DSMIN=ABS(DSMIN)
      AP%DSMAX=ABS(DSMAX)
      AP%RDS=DS
      AP%RL0=RL0
      AP%RL1=RL1
      AP%A0=A0
      AP%A1=A1
C
      DET=0.d0
      FLDF=0.d0
      HBFF=0.d0
      BIFF=0.d0
      SPBF=0.d0
C
      AP%EPSL=EPSL
      AP%EPSU=EPSU
      AP%EPSS=EPSS
      AP%DET=DET
      AP%FLDF=FLDF
      AP%HBFF=HBFF
      AP%BIFF=BIFF
      AP%SPBF=SPBF
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
      EOF=.TRUE.
      RETURN
 5    EOF=.TRUE.
      IF(KEYS)GOTO 1
      END SUBROUTINE INIT

C     ---------- ---------
      SUBROUTINE SCANVALUE(STR,NPOS,LISTLEN)
      IMPLICIT NONE
C
C     Scans STR(:) for a value
C     NPOS points to the next keyword on the same line,
C       or is set to 1 if there is none
C     LISTLEN gives the number of items in lists delimited by []
C     [] characters are removed
C
      CHARACTER(*), INTENT(INOUT) :: STR
      INTEGER, INTENT(OUT) :: NPOS,LISTLEN

      INTEGER I,LEVEL,LENSTR,ios
      CHARACTER(1) C,PREV,QUOTE
      LOGICAL QUOTEESC,ISDICT
      LISTLEN=1
      LEVEL=0
      QUOTE=' '
      QUOTEESC=.FALSE.
      PREV=' '

      NPOS=1
      ISDICT=.FALSE.
      LENSTR=LEN_TRIM(STR)
      I=1
      DO
         IF(I>LENSTR)THEN
            IF(LEVEL==0)EXIT
            LENSTR=LEN_TRIM(STR)
            READ(2,'(A)',IOSTAT=ios) STR(LENSTR+1:)
            IF(ios/=0)EXIT
            LENSTR=LEN_TRIM(STR)
         ENDIF
         NPOS=I
         C=STR(I:I)
         IF(QUOTE==' ')THEN
            SELECT CASE(C)
            CASE(',',' ')
               IF(LEVEL==0)EXIT
               IF(PREV==':')C=PREV !eat ',' and ' ' after ':'
            CASE(':')
               STR(I:I)=','
            CASE(']','}')
               IF(C=='}') ISDICT=.FALSE.
               STR(I:I)=' '
               IF(LEVEL==1.AND.(PREV=='['.OR.PREV=='{'))LISTLEN=0
               LEVEL=LEVEL-1
               IF(C==']'.AND.ISDICT) STR(I:I)="'"
            CASE DEFAULT
               IF((PREV==','.OR.PREV==' ').AND.LEVEL==1)THEN
                  LISTLEN=LISTLEN+1
               ENDIF
               SELECT CASE(C)
               CASE('[','{')
                  STR(I:I)=' '
                  LEVEL=LEVEL+1
                  IF(C=='{')THEN
                     ISDICT=.TRUE.
                  ELSEIF(ISDICT)THEN
                     STR(I:I)="'"
                  ENDIF
               CASE('"',"'")
                  QUOTE=C
               END SELECT
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
         I=I+1
      ENDDO
      I=VERIFY(STR(NPOS:)," ,")
      IF(I==0)THEN
         NPOS=1
      ELSE
         NPOS=NPOS+I-1
         IF(NPOS>=LEN_TRIM(STR))NPOS=1
      ENDIF
      END SUBROUTINE SCANVALUE

C     ---------- -------
      SUBROUTINE CLEANUP()
C
C     Deallocate some globally allocated arrays.
C
      USE AUTO_CONSTANTS, ONLY : IVTHU,IVUZR,IVTHL,ICU,parnames,unames,
     *     SP,STOPS,PARVALS,UVALS

      IMPLICIT NONE

      DO I=1,SIZE(IVUZR)
         DEALLOCATE(IVUZR(I)%VAR)
      ENDDO
      DEALLOCATE(IVTHU,IVUZR,IVTHL,ICU,parnames,unames,SP,STOPS,
     *     PARVALS,UVALS)
      END SUBROUTINE CLEANUP
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C               The leading subroutines of AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- -----
      SUBROUTINE INIT1(AP,ICP,ICU)
C
      USE HOMCONT, ONLY:INHO
      USE AUTO_CONSTANTS, ONLY:IVTHL
C
      DOUBLE PRECISION, PARAMETER ::
     *     HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30
C
C General initialization. Redefinition of constants.
C The following constants are redefined, ie. they are different than in
C fort.2 or c.*:

C   DS: if DS is set to 0 it'll be set to 0.1
C   DS: if DSMIN is set to 0 it'll be set to 1.0d-4 * |DS|
C   DSMIN is divided by 1+HMACH
C   DS and DSMAX are multiplied by 1+HMACH

C   NDIM: set to the dimension of the extended system
C   ILP: set to 0 dependent on problem type
C   ISP: set to 0 dependent on problem type
C   ISW: set to 1 if equal to 0, to -|ISW| for starts of ext systems
C   NBC: set by problem type
C   NINT: set by problem type
C   NMX: set to 5 for starts of extended systems

      TYPE(AUTOPARAMETERS) AP
      INTEGER ICP(*),ICU(*)
C
C Local
      INTEGER NDIM,IPS,IRS,ILP,ISP,ISW,NBC,NINT,NMX,NPAR,NPARI
      INTEGER ITP,NFPR,NICP,NDM,NXP,I,NNEG,IC,JC
      DOUBLE PRECISION DS,DSMIN,DSMAX,FC
C
       NDIM=AP%NDIM
       IPS=AP%IPS
       IRS=AP%IRS
       ILP=AP%ILP
       ISP=AP%ISP
       ISW=AP%ISW
       NBC=AP%NBC
       NINT=AP%NINT
       NMX=AP%NMX
       ITP=AP%ITP
       NFPR=AP%NFPR
       NPAR=AP%NPAR
       NICP=AP%NICP
C
       DS=AP%DS
       DSMIN=AP%DSMIN
       DSMAX=AP%DSMAX
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
       NPARI=0
C
C Redefinition for waves
       IF(IPS==11.OR.IPS==12)THEN
         NDIM=2*NDIM
         NDM=NDIM
         AP%NDM=NDM
       ENDIF
C
C General Redefinition.
C
       IF((ABS(IPS)<=1.OR.IPS==11) .AND. ISW==1 )THEN
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
       ELSE IF((IPS==2.OR.IPS==12) .AND. ABS(ISW)==1 )THEN
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
         CALL INHO(AP,ICP)
         NDIM=AP%NDIM
         NBC=AP%NBC
         NINT=AP%NINT
         NPARI=AP%NPARI
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
           NICP=NFPR-3
C
       ELSE IF(IPS.EQ.5)THEN
C        ** Algebraic optimization Problems
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPR=NFPR+1
         ICP(1)=10
         IF(NFPR.EQ.2)THEN
           NDIM=NDIM+1
         ELSE
           NDIM=2*NDIM+NFPR
         ENDIF
C
       ELSE IF(IRS.GT.0 .AND. ABS(ISW).GE.2 )THEN
C        ** Continuation of singular points
C
         IF( ( ITP==2.OR.(ABS(ITP)/10)==2.OR.
     *         ITP==7.OR.(ABS(ITP)/10)==7 )
     *        .AND. (ABS(IPS)<=1.OR.IPS==11))THEN
C          ** Fold/PD continuation (Algebraic Problems)
           NDIM=2*NDIM+1
           NFPR=2
C
         ELSE IF( ( ITP.EQ.1.OR.(ABS(ITP)/10).EQ.1 )
     *        .AND. (ABS(IPS)<=1.OR.IPS==11))THEN
C          ** BP cont (Algebraic Problems) (by F. Dercole)
           NDIM=2*NDIM+2
           NFPR=ABS(ISW)
C
         ELSE IF((ITP==3.OR.(ABS(ITP)/10)==3.OR.
     *            ITP==8.OR.(ABS(ITP)/10)==8)
     *               .AND. (ABS(IPS)<=1.OR.IPS==11))THEN
C          ** Hopf/Neimark-Sacker bifurcation continuation (Maps, ODE, Waves)
           NDIM=2*NDIM+2
           NFPR=2
C
         ELSE IF( ITP==5 .AND. (IPS==2.OR.IPS==12) )THEN
C          ** Fold continuation (Periodic solutions); start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(2)=11
           ENDIF
           ICP(3)=NPAR+2
           ICP(4)=NPAR+1
           NPARI=2
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (ABS(ITP)/10)==5 .AND. (IPS==2.OR.IPS==12) )THEN
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
           ICP(4)=NPAR+1
           NPARI=2
C
         ELSE IF( (ITP==6) .AND.  (IPS==2.OR.IPS==12) )THEN
C          ** BP cont (Periodic solutions); start (by F. Dercole)
           NDIM=4*NDIM
           NBC=NDIM
           NINT=10
           NFPR=NBC+NINT-NDIM+1
           IF(((ABS(ISW)==2).AND.(ICP(3)==11 .OR. NICP==2)).OR.
     *        ((ABS(ISW)==3).AND.(ICP(4)==11 .OR. NICP==3)))THEN
C            ** Variable period
             ICP(2)=NPAR+6 ! a
             ICP(3)=NPAR+7 ! b
             ICP(4)=11 ! T
           ELSE
C            ** Fixed period
             ICP(3)=NPAR+6 ! a
             ICP(4)=NPAR+7 ! b
           ENDIF
           ICP(5)=NPAR+1   ! q1
           ICP(6)=NPAR+2   ! q2/beta1
           ICP(7)=NPAR+3   ! r1
           ICP(8)=NPAR+4   ! r2/beta2
           ICP(9)=NPAR+5   ! psi^*_3
           ICP(10)=NPAR+8  ! c1
           ICP(11)=NPAR+9  ! c2
           NPARI=9
C
           ILP=0
           ISW=-ABS(ISW)
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (ABS(ITP)/10==6) .AND. (IPS==2.OR.IPS==12))THEN
C          ** BP cont (Periodic solutions); restart 1 or 2
           NDIM=2*NDIM
           NBC=NDIM
           NINT=4
           NFPR=NBC+NINT-NDIM+1
           IF(ABS(ISW)==2)THEN
C            ** Non-generic case
             IF(ICP(3)==11 .OR. NICP==2)THEN
C              ** Variable period
               ICP(3)=NPAR+7 ! b
               ICP(4)=11 ! T
             ELSE
C              ** Fixed period
               ICP(4)=NPAR+7 ! b
             ENDIF
           ELSE
C            ** Generic case
             IF(ICP(4)==11 .OR. NICP==3)THEN
C              ** Variable period
               ICP(4)=11 ! T
             ENDIF
           ENDIF
           ICP(5)=NPAR+5     ! psi^*_3
           NPARI=9
C
         ELSE IF(ITP==7 .AND. (IPS==2.OR.IPS==7.OR.IPS==12))THEN
C          ** Continuation of period doubling bifurcations; start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=2
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(2)=11
           ENDIF
           ICP(3)=NPAR+1
           NPARI=1
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF(ABS(ITP)/10==7 .AND. (IPS==2.OR.IPS==7.OR.IPS==12))THEN
C          ** Continuation of period doubling bifurcations; restart
           NDIM=2*NDIM
           NBC=NDIM
           NINT=2
           NFPR=NBC+NINT-NDIM+1
           IF(NICP.EQ.2)THEN
C            ** Variable period
             ICP(3)=11
           ENDIF
           NPARI=1
C
         ELSE IF(ITP==8 .AND. (IPS==2.OR.IPS==12))THEN
C          ** Continuation of torus bifurcations; start
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           ICP(2)=11
           ICP(3)=12
           ICP(4)=NPAR+1
           NPARI=1
           ILP=0
           ISP=0
           ISW=-2
           NMX=5
           WRITE(6,101)
C
         ELSE IF(ABS(ITP)/10==8 .AND. (IPS==2.OR.IPS==12))THEN
C          ** Continuation of torus bifurcations; restart
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(NICP.LT.4)THEN
C            **If not specified by user
             ICP(3)=11
             ICP(4)=12
           ENDIF
           NPARI=1
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
               ICP(NFPR/2+I+1)=NPAR+I
             ENDDO
           ENDIF
           ICP(NFPR/2+1)=NPAR+NFPR/2
           NPARI=NFPR/2
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
               ICP(NFPR/2+I+1)=NPAR+I
             ENDDO
           ENDIF
           ! PAR(NPAR+NFPR/2) contains a norm
           NPARI=NFPR/2
C
         ELSE IF( ITP==6 .AND. (IPS==4.OR.IPS==7) )THEN
C          ** BP cont (BVP; start) (by F. Dercole)
           NXP=NBC+NINT-NDIM+1
           NDIM=4*NDIM
           NBC=3*NBC+NDIM/2+NXP
           NINT=3*NINT+NXP+5
           NFPR=NBC+NINT-NDIM+1
           ICP(NXP+1)=NPAR+3*NXP+NDIM/4   ! a
           ICP(NXP+2)=NPAR+3*NXP+NDIM/4+1 ! b
           DO I=1,NXP
             ICP(NXP+I+2)=NPAR+I          ! q
             ICP(2*NXP+I+2)=NPAR+NXP+I    ! r
             ICP(4*NXP+NDIM/4+I+3)=NPAR+3*NXP+NDIM/4+3+I ! d
           ENDDO
           DO I=1,NXP+NDIM/4-1
             ICP(3*NXP+I+2)=NPAR+2*NXP+I  ! psi^*_2,psi^*_3
           ENDDO
           ICP(4*NXP+NDIM/4+2)=NPAR+3*NXP+NDIM/4+2 ! c1
           ICP(4*NXP+NDIM/4+3)=NPAR+3*NXP+NDIM/4+3 ! c2
           NPARI=4*NXP+NDIM/4+3
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
             ICP(NXP+2)=NPAR+3*NXP+NDIM/2+1 ! b
           ENDIF
           DO I=1,NXP+NDIM/2-1
             ICP(NXP+I+2)=NPAR+2*NXP+I      ! psi^*_2,psi^*_3
           ENDDO
           DO I=1,NXP
             ICP(2*NXP+NDIM/2+I+1)=NPAR+3*NXP+NDIM/2+3+I ! d
           ENDDO
           NPARI=4*NXP+NDIM/2+3
C
         ENDIF
C
       ENDIF
C
       IF(.NOT.ALLOCATED(IVTHL))THEN
          ! set default for *THL
          IF(IPS==2)THEN
             ALLOCATE(IVTHL(1))
             IVTHL(1)%INDEX='11'
             IVTHL(1)%VAR=0d0
          ELSE
             ALLOCATE(IVTHL(0))
          ENDIF
       ENDIF

       AP%NDIM=NDIM
       AP%ILP=ILP
       AP%ISP=ISP
       AP%ISW=ISW
       AP%NBC=NBC
       AP%NINT=NINT
       AP%NMX=NMX
       AP%NPARI=NPARI
       AP%NFPR=NFPR
       AP%NICP=NICP
C
       AP%DS=DS
       AP%DSMIN=DSMIN
       AP%DSMAX=DSMAX
C
 101   FORMAT(/,' Generating starting data :',
     *          ' Restart at EP label below :')

      RETURN
      END SUBROUTINE INIT1

      END PROGRAM AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
