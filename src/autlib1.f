C     ------- ----
      PROGRAM AUTO
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTERFACE
        SUBROUTINE SETPAE(IAP,RAP)
          INCLUDE 'auto.h'
          IMPLICIT DOUBLE PRECISION (A-H,O-Z)
          TARGET IAP(NIAP),RAP(NRAP)
        END SUBROUTINE SETPAE

        SUBROUTINE SETPBV(IAP,RAP,DTM)
          INCLUDE 'auto.h'
          IMPLICIT DOUBLE PRECISION (A-H,O-Z)
          TARGET IAP(NIAP),RAP(NRAP),DTM(IAP(5)+1)
        END SUBROUTINE SETPBV
      END INTERFACE
C
      LOGICAL FOUND,EOF
C Local
      DIMENSION IAP(NIAP),RAP(NRAP)
      DIMENSION PAR(2*NPARX)
C
C Initialization :
C
      CALL MPIINI(IAP)
C
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential')
       OPEN(3,FILE='fort.3',STATUS='unknown',ACCESS='sequential')
       OPEN(7,FILE='fort.7',STATUS='unknown',ACCESS='sequential')
       OPEN(8,FILE='fort.8',STATUS='unknown',ACCESS='sequential')
       OPEN(9,FILE='fort.9',STATUS='unknown',ACCESS='sequential')
C
 1     IF(IAP(39).GT.1)THEN
         CALL MPITIM(TIME0)
       ELSE
         CALL AUTIM0(TIME0)
       ENDIF
       FOUND=.FALSE.
       CALL INIT(IAP,RAP,PAR,EOF)
       IF(EOF)THEN
         CALL MPIEND()
         STOP
       ENDIF
C
C Find restart label and determine type of restart point.
C
       IRS=IAP(3)
       NFPR=IAP(29)
C
       IF(IRS.GT.0) THEN
         CALL FINDLB(IAP,RAP,IRS,NFPR,FOUND)
         IAP(29)=NFPR
         IF(.NOT.FOUND) THEN
           WRITE(6,400)IRS
           STOP
         ENDIF
       ENDIF
C
       CALL MPIIAP(IAP)
       CALL AUTOI(IAP,RAP,PAR)
C-----------------------------------------------------------------------
C
      IF(IAP(39).GT.1)THEN
        CALL MPITIM(TIME1)
      ELSE
        CALL AUTIM1(TIME1)
      ENDIF
      TOTTIM=TIME1-TIME0
      CALL WRBAR("=",47)
      WRITE(9,301)TOTTIM
      WRITE(6,301)TOTTIM
      GOTO 1
C
 301  FORMAT(/,' Total Time ',E12.3)
C
C Error Message.
 400  FORMAT(' Restart label ',I4,' not found')
C
      END
C
C
      MODULE AUTO_CONSTANTS
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'auto.h'
C
      INTEGER NDIM,IPS,IRS,ILP
      INTEGER NICP
      INTEGER ICP(2*NPARX)
      INTEGER NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
      INTEGER NMX
      DOUBLE PRECISION RL0,RL1,A0,A1
      INTEGER NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
      DOUBLE PRECISION EPSL,EPSU,EPSS
      DOUBLE PRECISION DS,DSMIN,DSMAX
      INTEGER IADS
      INTEGER NTHL
      DOUBLE PRECISION THL(NPARX)
      INTEGER NTHU
      DOUBLE PRECISION,ALLOCATABLE :: THU(:)
      INTEGER NUZR
      INTEGER, ALLOCATABLE :: IUZ(:)
      DOUBLE PRECISION,ALLOCATABLE :: VUZ(:)
C
      END MODULE AUTO_CONSTANTS
C
C
C     ---------- ----
      SUBROUTINE AUTOI(IAP,RAP,PAR)
C
      USE INTERFACES
      USE AUTO_CONSTANTS
      USE HOMCONT, ONLY:FNHO,BCHO,ICHO,PVLSHO,STPNHO
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      EXTERNAL STPNUS,STPNAE,STPNBV,STPNUB
      EXTERNAL PVLSBV
C
      INTEGER IAP(*)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  One-parameter continuations
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
       ITP=IAP(27)
       NFPR=IAP(29)
C
       IF((IPS.EQ.0.OR.IPS.EQ.1) .AND. IABS(ISW).LE.1 ) THEN
C        ** Algebraic systems.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.11 .AND. IABS(ISW).LE.1 ) THEN
C        ** Waves : Spatially homogeneous solutions,
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNWS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNWS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF((IPS.EQ.-1) .AND. IABS(ISW).LE.1 ) THEN
C        ** Discrete dynamical systems : fixed points.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNDS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNDS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.-2) THEN
C        ** Time integration.
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNTI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNTI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.2 .AND. IABS(ISW).LE.1 ) THEN
C        ** Periodic solutions
         IF(ITP.NE.3 .AND. IABS(ITP/10).NE.3) THEN
           IF(IRS.GT.0)THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNBV,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNUB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNPS,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.12 .AND. IABS(ISW).LE.1 ) THEN
C        ** Wave train solutions to parabolic systems.
         IF(ITP.NE.3) THEN
           IF(IRS.GT.0)THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNWP,BCPS,ICPS,STPNBV,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FNWP,BCPS,ICPS,STPNUB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNWP,BCPS,ICPS,STPNWP,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.4 .AND. IABS(ISW).LE.1) THEN
C        ** Boundary value problems.
         IF(ITP.NE.3 .AND. IABS(ITP/10).NE.3) THEN
           IF(IRS.GT.0) THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNBV,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNUB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNPB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.7 .AND. IABS(ISW).LE.1) THEN
C        ** Boundary value problems with Floquet multipliers.
         IF(ITP.NE.3 .AND. IABS(ITP/10).NE.3) THEN
           IF(IRS.GT.0) THEN
             CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNBV,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNUB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNPB,
     *        PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF

       ELSE IF(IPS.EQ.9 .AND. IABS(ISW).LE.1) THEN
C        ** Homoclinic bifurcation analysis.
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNHO,BCHO,ICHO,STPNBV,
     *      PVLSHO,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNHO,BCHO,ICHO,STPNHO,
     *      PVLSHO,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.14) THEN
C        ** Evolution calculations for parabolic systems.
C           (Periodic boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCPS,ICPE,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCPS,ICPE,STPNUB,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.15.AND.IABS(ISW).EQ.1) THEN
C        ** Optimization of periodic solutions.
         IF(NFPR.LT.6)THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPO,BCPO,ICPO,STPNPO,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPO,BCPO,ICPO,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.16) THEN
C        ** Evolution calculations for parabolic systems.
C           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCNI,ICPE,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNPE,BCNI,ICPE,STPNUB,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.17) THEN
C        ** Continuation of stationary states of parabolic systems.
C           (User supplied boundary conditions.)
         IF(IRS.GT.0) THEN
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNSP,BCNI,ICPE,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOBV(IAP,RAP,PAR,ICP,FNSP,BCNI,ICPE,STPNUB,
     *      PVLSBV,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.5) THEN
C        ** Algebraic optimization problems.
         IF(MOD(ITP,10).EQ.2.OR.IRS.EQ.0)NFPR=NFPR+1
         IAP(29)=NFPR
         IF(NFPR.EQ.2) THEN
           IF(IRS.GT.0) THEN
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC1,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC1,STPNC1,THL,THU,IUZ,VUZ)
           ENDIF
         ELSE
           IF(MOD(ITP,10).NE.2) THEN
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC2,STPNAE,THL,THU,IUZ,VUZ)
           ELSE
             CALL AUTOAE(IAP,RAP,PAR,ICP,FNC2,STPNC2,THL,THU,IUZ,VUZ)
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
 2     IF(IPS.LE.1 .AND. IABS(ISW).EQ.2 .AND. (ITP.EQ.2) )
     * THEN
C        ** Fold continuation (algebraic problems).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNLP,STPNLP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. IABS(ISW).EQ.2 
     *         .AND. ( (IABS(ITP)/10).EQ.2 ) )
     * THEN
C        ** Fold continuation (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNLP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. IABS(ISW).GE.2 .AND. (ITP.EQ.1) )
     * THEN
C        ** BP cont (algebraic problems) (by F. Dercole).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNBP,STPNBP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. IABS(ISW).GE.2 
     *         .AND. ( (IABS(ITP)/10).EQ.1 ) )
     * THEN
C        ** BP cont (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNBP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.0.OR.IPS.EQ.1).AND.IABS(ISW).EQ.2.AND.ITP.EQ.3 )
     * THEN
C        ** Hopf bifurcation continuation (ODE).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHB,STPNHB,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.0.OR.IPS.EQ.1).AND.IABS(ISW).EQ.2.AND.
     * (IABS(ITP)/10).EQ.3 ) THEN
C        ** Hopf bifurcation continuation (ODE, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHB,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.11.AND.IABS(ISW).EQ.2.AND.ITP.EQ.3 )
     * THEN
C        ** Hopf bifurcation continuation (Waves).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHW,STPNHW,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.11.AND.IABS(ISW).EQ.2.AND.
     * (IABS(ITP)/10).EQ.3 ) THEN
C        ** Hopf bifurcation continuation (Waves, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHW,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.-1 .AND. IABS(ISW).EQ.2 .AND. ITP.EQ.3 ) THEN
C        ** Hopf bifurcation continuation (Maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHD,STPNHD,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.-1 .AND. IABS(ISW).EQ.2 .AND.(IABS(ITP)/10).EQ.3)
     * THEN
C        ** Hopf bifurcation continuation (Maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHD,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. IABS(ISW).EQ.2 .AND. 
     *         (ITP.EQ.5.OR.ITP.EQ.6) ) THEN 
C        ** Fold continuation (Periodic solutions, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPL,BCPL,ICPL,STPNPL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
      ELSE IF(IPS.EQ.2 .AND. IABS(ISW).EQ.2 .AND. 
     *        ( (IABS(ITP)/10).EQ.5 .OR. (IABS(ITP)/10).EQ.6 ) )
     * THEN
C        ** Fold continuation (Periodic solutions, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPL,BCPL,ICPL,STPNBV,
     *   PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.2 .OR. IPS.EQ.7)
     *      .AND. IABS(ISW).EQ.2 .AND. ITP.EQ.7 ) THEN
C        ** Continuation of period doubling bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPD,BCPD,ICPD,STPNPD,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.2 .OR. IPS .EQ.7)
     *      .AND. IABS(ISW).EQ.2 .AND. (IABS(ITP)/10).EQ.7)
     * THEN
C        ** Continuation of period doubling bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPD,BCPD,ICPD,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. IABS(ISW).EQ.2 .AND. ITP.EQ.8 ) THEN
C        ** Continuation of torus bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNTR,BCTR,ICTR,STPNTR,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. IABS(ISW).EQ.2 .AND. (IABS(ITP)/10).EQ.8)
     * THEN
C        ** Continuation of torus bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNTR,BCTR,ICTR,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.4.OR.IPS.EQ.7) .AND. IABS(ISW).EQ.2 .AND.
     *          (ITP.EQ.5.OR.ITP.EQ.6) ) THEN
C        ** Continuation of folds (BVP, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBL,BCBL,ICBL,STPNBL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.4.OR.IPS.EQ.7) .AND. IABS(ISW).EQ.2 .AND. 
     *         ( (IABS(ITP)/10).EQ.5 .OR. (IABS(ITP)/10).EQ.6 ) ) THEN
C        ** Continuation of folds (BVP, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBL,BCBL,ICBL,STPNBV,
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
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Initialization
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE INIT(IAP,RAP,PAR,EOF)
C
      USE AUTO_CONSTANTS
C
      IMPLICIT NONE
C
C Reads the file of continuation constants
C
      INTEGER IAP(*)
      DOUBLE PRECISION RAP(*),PAR(*)
      LOGICAL EOF
C
      INTEGER IBR,I,IUZR,ITHL,ITHU,NBIF,NFPR,NDM,NNT0,NBC0
      INTEGER NINS,NIT,LAB,NTOT,ITP,IPOS,ISTOP,ITPST
      DOUBLE PRECISION AMP,BIFF,DET,DSOLD,SPBF,TIVP,HBFF,FLDF
C
      DO I=1,NPARX
        ICP(I)=I
        ICP(NPARX+I)=0
        PAR(I)=0.d0
        PAR(NPARX+I)=0.d0
        THL(ICP(I))=1.d0
      ENDDO
C
      READ(2,*,END=5) NDIM,IPS,IRS,ILP
C
C     we allocate THU (a pointer to the THU arrau in the
C     main program) here since this is the place where we 
C     know the size.  It is 8 times bigger then ndim since
C     INIT can modify THU based on the problem type,
C     but only up to making it 8 times larger.
C
      ALLOCATE(THU(8*NDIM))
      DO I=1,NDIM*8
        THU(I)=1.d0
      ENDDO
C
      READ(2,*) NICP,(ICP(NPARX+I),I=1,NICP)
      IF(NICP.GT.0)THEN
        DO I=1,NICP
          ICP(I)=ICP(NPARX+I)
        ENDDO
      ELSE
        NICP=1
        ICP(NPARX+1)=ICP(1)
      ENDIF
      READ(2,*) NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
      READ(2,*) NMX,RL0,RL1,A0,A1
      READ(2,*) NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
      READ(2,*) EPSL,EPSU,EPSS
      READ(2,*) DS,DSMIN,DSMAX,IADS
      DSMIN=DABS(DSMIN)
      DSMAX=DABS(DSMAX)
      READ(2,*) NTHL
      IF(NTHL.GT.0)THEN
        DO I=1,NTHL
          READ(2,*)ITHL,THL(ITHL)
        ENDDO
      ENDIF
      READ(2,*) NTHU
      IF(NTHU.GT.0)THEN
        DO I=1,NTHU
          READ(2,*)ITHU,THU(ITHU)
        ENDDO
      ENDIF
      READ(2,*)NUZR
      IF(NUZR.GT.0)THEN
        ALLOCATE(IUZ(NUZR),VUZ(NUZR))
        DO I=1,NUZR
          READ(2,*)IUZ(I),VUZ(I)
        ENDDO
      ELSE
C       Avoid uninitialized pointers
        ALLOCATE(IUZ(1),VUZ(1))
      ENDIF
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
      NIT=0
      NTOT=0
      NINS=0
      ISTOP=0
      NBIF=0
      IPOS=1
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
      IAP(31)=NIT
      IAP(32)=NTOT
      IAP(33)=NINS
      IAP(34)=ISTOP
      IAP(35)=NBIF
      IAP(36)=IPOS
      IAP(37)=LAB
      IAP(41)=NICP
C
      RAP(1)=DS
      RAP(2)=DSMIN
      RAP(3)=DSMAX
      DSOLD=DS
      RAP(5)=DSOLD
      RAP(6)=RL0
      RAP(7)=RL1
      RAP(8)=A0
      RAP(9)=A1
C
      AMP=0.d0
      DET=0.d0
      TIVP=0.d0
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
      RAP(15)=TIVP
      RAP(16)=FLDF
      RAP(17)=HBFF
      RAP(18)=BIFF
      RAP(19)=SPBF
C
      EOF=.FALSE.
      RETURN
 5    EOF=.TRUE.
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE CHDIM(IAP)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Check dimensions.
C
      DIMENSION IAP(*)
C
       NPAR=IAP(29)
C
       IF(NPAR.GT.NPARX)THEN
         WRITE(6,101)NPAR,NPARX
         STOP
       ENDIF
C
 101   FORMAT(' Dimension exceeded : NPAR=',I5,'  maximum=',I5,/,
     *        ' (Increase NPARX in auto.h and recompile AUTO)')
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C               The leading subroutines of AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C This is the entry subroutine for algebraic systems.
C
      INTEGER IAP(*)
C
      EXTERNAL FUNI,STPNT
C
       IF(IAP(38).GT.0)THEN
         CALL MPIWFI(.FALSE.,FUNI,STPNT)
         RETURN
       ENDIF
       CALL INIT1(IAP,RAP,ICP,PAR)
       CALL CHDIM(IAP)
       CALL CNRLAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT,
     * PVLI,THL,THU,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C THIS IS THE ENTRY ROUTINE FOR GENERAL BOUNDARY VALUE PROBLEMS.
C
      INTEGER IAP(*)
C
      EXTERNAL FUNI,BCNI,ICNI,STPNT,PVLI
C
       IF(IAP(38).GT.0)THEN
C        This is a little trick to tell MPI workers what FUNI and ICNI
C        are.
         CALL MPIWFI(.TRUE.,FUNI,ICNI)
         RETURN
       ENDIF
       CALL INIT1(IAP,RAP,ICP,PAR)
       CALL CHDIM(IAP)
       CALL CNRLBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT,
     *  PVLI,THL,THU,IUZ,VUZ)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE INIT1(IAP,RAP,ICP,PAR)
C
      USE HOMCONT, ONLY:INHO
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C General initialization. Redefinition of constants.
C
      DIMENSION IAP(*),RAP(*),ICP(*),PAR(*)
C
C Local
      DIMENSION ICT(NPARX)
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
       IF(DSMIN.EQ.0.d0)DSMIN=1.0D-4*DABS(DS)
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
       IF(IABS(IPS).LE.1 .AND. ISW.EQ.1 )THEN
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
       ELSE IF(IPS.EQ.2 .AND. IABS(ISW).EQ.1 )THEN
C        ** Periodic Solutions
         NBC=NDIM
         NINT=1
         NFPR=NBC+NINT-NDIM+1
C        **ISW=1 when starting from a HB
         IF(ITP.EQ.3.OR.(IABS(ITP)/10).EQ.3)ISW=1
         IF(NICP.EQ.1)THEN
C          **Variable period
           ICP(2)=11
         ENDIF
C
       ELSE IF(IPS.EQ.4 .AND. IABS(ISW).EQ.1  ) THEN
C        ** Boundary value problems
         NFPR=NBC+NINT-NDIM+1
C
       ELSE IF(IPS.EQ.7 .AND. IABS(ISW).EQ.1  ) THEN
C        ** Boundary value problems
         NFPR=NBC+NINT-NDIM+1
C
       ELSE IF( IPS.EQ.9 .AND. IABS(ISW).EQ.1  ) THEN
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
           DO I=1,NICP
             ICT(I)=ICP(I)
           ENDDO
           NFPR=0
           DO I=1,NICP
             IF(ICT(I).GT.0)THEN
               NFPR=NFPR+1
               ICP(NFPR)=ICT(I)
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
             IC=ICT(I)
             JC=IABS(IC)-20        
             IF(IC.LT.0.AND.JC.GT.0.AND.JC.LE.11)THEN
               NNEG=NNEG+1
               ICP(NFPR+NNEG)=JC
             ENDIF
           ENDDO
C Set indices of output parameters
           NICP=NFPR-3
           DO I=1,NICP
             ICP(NPARX+I)=ICP(I)
           ENDDO
C
       ELSE IF(IPS.EQ.5)THEN
C        ** Algebraic optimization Problems
         IF(NFPR.EQ.2)THEN
           NDIM=NDIM+1
           ICP(1)=10
         ELSE
           NDIM=2*NDIM+NFPR
           ICP(1)=10
         ENDIF
C
       ELSE IF(IRS.GT.0 .AND. IABS(ISW).GE.2 )THEN
C        ** Continuation of singular points
C
         IF( ( ITP.EQ.2.OR.(IABS(ITP)/10).EQ.2 )
     *        .AND. IABS(IPS).LE.1)THEN
C          ** Fold continuation (Algebraic Problems)
           NDIM=2*NDIM+1
           NFPR=2
C
         ELSE IF( ( ITP.EQ.1.OR.(IABS(ITP)/10).EQ.1 )
     *        .AND. IABS(IPS).LE.1)THEN
C          ** BP cont (Algebraic Problems) (by F. Dercole)
           NDIM=2*NDIM+2
           NFPR=3
C
         ELSE IF((ITP.EQ.3.OR.(IABS(ITP)/10).EQ.3)
     *               .AND. IABS(IPS).LE.1 )THEN
C          ** Hopf bifurcation continuation (Maps, ODE, Waves)
           NDIM=3*NDIM+2
           NFPR=2
C
         ELSE IF( (ITP.EQ.5.OR.ITP.EQ.6) .AND. IPS.EQ.2)THEN
C          ** Fold continuation (Periodic solutions); start
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(2)=13
             ICP(3)=11
             ICP(4)=12
           ELSE
C            ** Fixed period
             ICP(3)=13
             ICP(4)=12
           ENDIF
           ILP=0
           ISW=-2
           ISP=0
           NMX=5
           WRITE(6,101)
C
         ELSE IF( (IABS(ITP)/10.EQ.5 .OR. IABS(ITP)/10.EQ.6) 
     *           .AND. IPS.EQ.2)THEN
C          ** Fold continuation (Periodic solutions); restart
           NDIM=2*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           IF(ICP(3).EQ.11 .OR. NICP.EQ.2)THEN
C            ** Variable period
             ICP(3)=11
           ENDIF
           ICP(4)=12
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
         ELSE IF(IABS(ITP)/10.EQ.7 .AND. (IPS.EQ.2 .OR. IPS.EQ.7))THEN
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
         ELSE IF(IABS(ITP)/10.EQ.8 .AND. IPS.EQ.2)THEN
C          ** Continuation of torus bifurcations; restart
           NDIM=3*NDIM
           NBC=NDIM
           NINT=3
           NFPR=NBC+NINT-NDIM+1
           ICP(3)=11
           ICP(4)=12
C
         ELSE IF( (ITP.EQ.5.OR.ITP.EQ.6) .AND. (IPS.EQ.4.OR.IPS.EQ.7) )
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
         ELSE IF( ( (IABS(ITP)/10).EQ.5 .OR. IABS(ITP)/10.EQ.6)
     *           .AND. (IPS.EQ.4.OR.IPS.EQ.7))THEN
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
         ENDIF
C
       ENDIF
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
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Algebraic Problems
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE CNRLAE(IAP,RAP,PAR,ICP,FUNI,STPNT,THL,THU,IUZ,VUZ)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Controls the bifurcation analysis of algebraic problems
C
      EXTERNAL FUNI,STPNT,FNLPAE,FNHBAE,FNBPAE,FNUZAE
C
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),IUZ(*)
C Local
      DIMENSION RLCUR(NPARX),RLOLD(NPARX),RLDOT(NPARX)
      ALLOCATABLE AA(:,:),RHS(:),U(:),DU(:),UDOT(:),UOLD(:),STUD(:,:)
      ALLOCATABLE STU(:,:),STLA(:),STLD(:),F(:),DFDU(:),DFDP(:),UZR(:)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       IADS=IAP(8)
       ISP=IAP(9)
       ISW=IAP(10)
       NUZR=IAP(15)
       MXBF=IAP(17)
       NDM=IAP(23)
       ITPST=IAP(28)
       IBR=IAP(30)
C
       DS=RAP(1)
C
       ALLOCATE(AA(NDIM+1,NDIM+1),RHS(NDIM+1),U(NDIM),DU(NDIM+1))
       ALLOCATE(UDOT(NDIM),UOLD(NDIM),STUD(NBIFX,NDIM),STU(NBIFX,NDIM))
       ALLOCATE(STLA(NBIFX),STLD(NBIFX),F(NDIM),DFDU(NDIM**2))
       ALLOCATE(DFDP(NDIM*NPARX),UZR(NUZR))
C
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
C
       DO I=1,NDIM
         U(I)=0.d0
         DU(I)=0.d0
         UDOT(I)=0.d0
         UOLD(I)=0.d0
         F(I)=0.d0
       ENDDO
C
C Generate the starting point
C
       CALL STPNT(IAP,RAP,PAR,ICP,U)
       CALL PVLSAE(IAP,RAP,U,PAR)
C
C Determine a suitable starting label and branch number
C
       CALL NEWLAB(IAP,RAP)
C
C Write constants
C
       CALL STHD(IAP,RAP,PAR,ICP,THL,THU)
C
C Write plotting data for the starting point
C
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
C
C Starting procedure  (to get second point on first branch) :
C
       CALL STPRAE(IAP,RAP,PAR,ICP,FUNI,RDS,NDIM+1,AA,RHS,
     *  RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 5
       ITP=0
       IAP(27)=ITP
       GOTO 3
C
C Initialize computation of the next bifurcating branch.
C
 2     CALL SWPNT(IAP,RAP,PAR,ICP,RDS,NBIFX,STUD,STU,STLA,STLD,
     *  RLCUR,RLOLD,RLDOT,U,UDOT)
C
       IPOS=IAP(36)
       IF(IPOS.EQ.1)THEN
         NBIF=NBIF-1
         IAP(35)=NBIF
         NBFC=NBFC+1
       ENDIF
C
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
C
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
C
C Store plotting data for first point on the bifurcating branch
C
       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 6
C
C Determine the second point on the bifurcating branch
C
       CALL SWPRC(IAP,RAP,PAR,ICP,FUNI,NDIM+1,AA,RHS,RLCUR,RLOLD,RLDOT,
     *  U,DU,UOLD,UDOT,F,DFDU,DFDP,RDS,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 5
C
C Store plotting data for second point :
C
       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 6
       RBP=0.d0
       REV=0.d0
       RLP=0.d0
C
C Provide initial approximation to the next point on the branch
C
 3     CALL CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
C
C Find the next solution point on the branch
C
        CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,NDIM+1,AA,RHS,
     *   RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
        ISTOP=IAP(34)
        IF(ISTOP.EQ.1)GOTO 5
C
C Check for user supplied parameter output parameter-values.
C
       IF(NUZR.GT.0)THEN
         DO IUZR=1,NUZR
           IAP(26)=IUZR
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNUZAE,FUNI,NDIM+1,AA,RHS,RLCUR,
     *      RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,UZR(IUZR),THL,THU,
     *      IUZ,VUZ)
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
C NOTE: Fix (February 2005)
               GOTO 5
             ENDIF
           ENDIF
         ENDDO
       ENDIF
C
C Check for fold
C
         IF(IABS(ILP).GT.0)THEN
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNLPAE,FUNI,NDIM+1,AA,RHS,RLCUR,
     *      RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RLP,THL,THU,IUZ,VUZ)
           ITP=IAP(27)
           IF(ITP.EQ.-1) THEN
             IF(ILP.GT.0)THEN
               ITP=2+10*ITPST
               IAP(27)=ITP
               RLP=0.d0
               RBP=0.d0
               REV=0.d0
             ELSE
C            *Stop at the first found fold
               ISTOP=-1
               IAP(34)=ISTOP
               GOTO 5
             ENDIF
           ENDIF
         ENDIF
C
C Check for branch point, and if so store data :
C
         IF(IABS(ISP).GT.0)THEN
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNBPAE,FUNI,NDIM+1,AA,RHS,RLCUR,
     *      RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RBP,THL,THU,IUZ,VUZ)
           ISTOP=IAP(34)
           IF(ISTOP.EQ.1)GOTO 5
           ITP=IAP(27)
           IF(ITP.EQ.-1)THEN
             IF(ISP.GT.0)THEN
               ITP=1+10*ITPST
               IAP(27)=ITP
               NBIF=NBIF+1
               IAP(35)=NBIF
               CALL STBIF(IAP,RAP,PAR,ICP,NDIM+1,AA,NBIFX,STUD,STU,STLA,
     *          STLD,RLCUR,RLOLD,RLDOT,U,DU,UDOT,DFDU,DFDP,THL,THU)
               RLP=0.d0
               RBP=0.d0
               REV=0.d0
             ELSE
C            *Stop at the first found BP
               ISTOP=-1
               IAP(34)=ISTOP
               GOTO 5
             ENDIF
           ENDIF
         ENDIF
C
C Check for Hopf bifurcation
C
         IF(IABS(IPS).EQ.1)THEN
           CALL LCSPAE(IAP,RAP,PAR,ICP,FNHBAE,FUNI,NDIM+1,AA,RHS,RLCUR,
     *      RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,REV,THL,THU,IUZ,VUZ)
           ISTOP=IAP(34)
           IF(ISTOP.EQ.1)GOTO 5
           ITP=IAP(27)
           IF(ITP.EQ.-1)THEN
             ITP=3+10*ITPST
             IAP(27)=ITP
             REV=0.d0
           ENDIF
         ENDIF
C
C Store plotting data on unit 7 :
C
 5       CALL STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
C
C Adapt the stepsize along the branch
C
       ITP=IAP(27)
       NTOT=IAP(32)
       IF(IADS.NE.0 .AND. MOD(NTOT,IADS).EQ.0 
     *   .AND. ( MOD(ITP,10).EQ.0 .OR. MOD(ITP,10).EQ.4) )THEN
         CALL ADPTDS(IAP,RAP,RDS)
       ENDIF
C
 6     ITP=0
       IAP(27)=ITP
       ISTOP=IAP(34)
       IF(ISTOP.EQ.0)GOTO 3
C
       NBIF=IAP(35)
       IF(NBIF.NE.0 .AND. NBFC.LT.IABS(MXBF))GOTO 2
C
      DEALLOCATE(AA,RHS,U,DU,UDOT,UOLD,STUD,STU,STLA,STLD,F,DFDU,DFDP)
      DEALLOCATE(UZR)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNUS(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Gets the starting data from user supplied STPNT
C
      DIMENSION IAP(*)
C
       NDIM=IAP(1)
C
       CALL STPNT(NDIM,U,PAR,T)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNAE(IAP,RAP,PAR,ICP,U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL FOUND
C
C Gets the starting data from unit 3
C
      DIMENSION IAP(*)
C
       IRS=IAP(3)
       CALL FINDLB(IAP,RAP,IRS,NFPRS,FOUND)
       CALL READLB(IAP,RAP,U,PAR)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPRAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS,
     * RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Finds the second point on the initial solution branch.
C
      EXTERNAL FUNI
C
      DIMENSION IAP(*),AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DU(*)
      DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
C
C Local
      ALLOCATABLE IR(:),IC(:)
C
       NDIM=IAP(1)
       IID=IAP(18)
C
       RLOLD(1)=PAR(ICP(1))
       DO I=1,NDIM
         UOLD(I)=U(I)
       ENDDO
C
C Determine the direction of the branch at the starting point
C
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
C
       IF(IID.GE.3)CALL WRJAC(IAP,NDIM+1,M1AA,AA,RHS)
       ALLOCATE(IR(NDIM+1),IC(NDIM+1))
       CALL NLVC(NDIM+1,M1AA,1,AA,DU,IR,IC)
       DEALLOCATE(IR,IC)
C
C Scale and make sure that the PAR(ICP(1))-dot is positive.
C
       SS=0.d0
       DO I=1,NDIM
         SS=SS+THU(I)*DU(I)**2
       ENDDO
       SS=SS+THL(ICP(1))*DU(NDIM+1)**2
C
       SIGN=1.d0
       IF(DU(NDIM+1).LT.0.d0)SIGN=-1.d0
       SC=SIGN/DSQRT(SS)
       DO I=1,NDIM+1
         DU(I)=SC*DU(I)
       ENDDO
C
       DO I=1,NDIM
         UDOT(I)=DU(I)
       ENDDO
       RLDOT(1)=DU(NDIM+1)
C
C Set initial approximations to the second point on the branch
C
       DO I=1,NDIM
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
C
       CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS,
     *  RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C This subroutine determines an initial approximation to the next
C solution on a branch by extrapolating from the two preceding points.
C The step used in the preceding step has been stored in DSOLD.
C
      DIMENSION IAP(*),RAP(*),UOLD(*),U(*),UDOT(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*)
C
       NDIM=IAP(1)
       IPS=IAP(2)
C
       DSOLD=RAP(5)
C
       RLDOT(1)=(RLCUR(1)-RLOLD(1))/DSOLD
       DO I=1,NDIM
         UDOT(I)=(U(I)-UOLD(I))/DSOLD
       ENDDO
C
       RLOLD(1)=RLCUR(1)
       RLCUR(1)=RLCUR(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         UOLD(I)=U(I)
         U(I)=U(I)+UDOT(I)*RDS
       ENDDO
C      Save old time for time integration
       IF(IPS.EQ.-2)RAP(15)=RLOLD(1)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS,
     * RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C This is the subroutine for computing solution branches. It solves
C the equations for finding the next point on the branch at distance DS
C from the current point. An initial approximation to the new point
C ( i.e. to PAR(ICP(1)) and U ) has been supplied by CONT.
C
      EXTERNAL FUNI
C
      DIMENSION IAP(*),RAP(*)
      DIMENSION AA(M1AA,*),RHS(*),U(*),DU(*),UOLD(*),UDOT(*) 
      DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
C Local
      ALLOCATABLE IR(:),IC(:)
C
       NDIM=IAP(1)
       IADS=IAP(8)
       IID=IAP(18)
       ITNW=IAP(20)
       NDM=IAP(23)
       IBR=IAP(30)
C
       DSMIN=RAP(2)
       DSMAX=RAP(3)
       EPSL=RAP(11)
       EPSU=RAP(12)
C
 1     DSOLD=RDS
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
 100   FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
 101   FORMAT(I4,I6,I4,5X,1P2E14.5)
C
C Call user-supplied FUNC to evaluate the right hand side of the
C differential equation and its derivatives :
C
       DO 2 NIT1=1,ITNW
C
         NIT=NIT1
         IAP(31)=NIT
         PAR(ICP(1))=RLCUR(1)
         CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
C
C Set up the Jacobian matrix and the right hand side :
C
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
         AA(NDIM+1,NDIM+1)=2.d0*THL(ICP(1))*(RLCUR(1)-RLOLD(1))*DDS
         SS=0.d0
         DO I=1,NDIM
           SS=SS+THU(I)*(U(I)-UOLD(I))**2
         ENDDO
         RHS(NDIM+1)=RDS-DDS*SS-THL(ICP(1))*DDS*(RLCUR(1)-RLOLD(1))**2
C
C Use Gauss elimination with pivoting to solve the linearized system :
C
         IF(IID.GE.5)CALL WRJAC(IAP,NDIM+1,M1AA,AA,RHS)
         ALLOCATE(IR(NDIM+1),IC(NDIM+1))
         CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,DU,NDIM+1,
     *           RHS,IR,IC,DET)
         DEALLOCATE(IR,IC)
         RAP(14)=DET
         DRLM=DU(NDIM+1)
C
C Add the Newton increments :
C
         DO I=1,NDIM
           U(I)=U(I)+DU(I)
         ENDDO
         RLCUR(1)=RLCUR(1)+DRLM
         DUMX=0.d0
         UMX=0.d0
         DO I=1,NDIM
           ADU=DABS(DU(I))
           AU=DABS(U(I))
           IF(AU.GT.UMX)UMX=AU
           IF(ADU.GT.DUMX)DUMX=ADU
         ENDDO
C
         IF(IID.GE.2)THEN
            WRITE(9,101)IBR,NTOP+1,
     *    NIT,RLCUR(1),RNRMV(NDM,U)
         ENDIF
C
         RDRLM= ABS(DRLM)/(1.d0+ ABS(RLCUR(1)))
         RDUMX=DUMX/(1.d0+UMX)
         IF(RDRLM.LE.EPSL.AND.RDUMX.LE.EPSU)THEN
           CALL PVLSAE(IAP,RAP,U,PAR)
           IF(IID.GE.2)WRITE(9,*)
           RETURN
         ENDIF
C
C Check whether relative error has reached user-supplied tolerance :
C
         IF(NIT.EQ.1)THEN
           DELREF=20*DMAX1(RDRLM,RDUMX)
         ELSE
           DELMAX=DMAX1(RDRLM,RDUMX)
           IF(DELMAX.GT.DELREF)GOTO 3
         ENDIF
C
 2     CONTINUE
C
C Maximum number of iterations has been reached
C
3      IF(IADS.EQ.0)WRITE(9,102)IBR,NTOP
 102   FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
       IF(IADS.EQ.0)GOTO 5
C
C Reduce stepsize and try again
C
       MXT=ITNW
       IAP(31)=MXT
       CALL ADPTDS(IAP,RAP,RDS)
       IF(DABS(RDS).LT.DSMIN)GOTO 4
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
       IF(IID.GE.2)WRITE(9,103)
 103   FORMAT(I4,I6,' NOTE:Retrying step')
       GOTO 1
C
C Minimum stepsize reached
C
 4     WRITE(9,104)IBR,NTOP
 104   FORMAT(I4,I6,' NOTE:No convergence using minimum step size')
 5     RLCUR(1)=RLOLD(1)
       PAR(ICP(1))=RLCUR(1)
       DO I=1,NDIM
         U(I)=UOLD(I)
       ENDDO
       ISTOP=1
       IAP(34)=ISTOP
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C               Detection of Singular Points
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE LCSPAE(IAP,RAP,PAR,ICP,FNCS,FUNI,M1AA,AA,RHS,RLCUR,
     * RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,Q,THL,THU,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C This subroutine uses the secant method to accurately locate special
C points (branch points, folds, Hopf bifurcations, user zeroes).
C These are characterized as zeroes of the function FNCS supplied in the
C call.
C This subroutine calls CONT and SOLVAE with varying stepsize RDS.
C The special point is assumed to have been found with sufficient
C accuracy if the ratio between RDS and the user supplied value of
C DS is less than the user-supplied toler EPSS.
C
      EXTERNAL FUNI,FNCS
C
      DIMENSION IAP(*),RAP(*),RLCUR(*),RLOLD(*),RLDOT(*),PAR(*),ICP(*)
      DIMENSION F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION AA(M1AA,*),RHS(*),U(*),DU(*),UDOT(*),UOLD(*)
C
      LOGICAL CHNG
C
       IID=IAP(18)
       ITMX=IAP(19)
       IBR=IAP(30)
C
       DS=RAP(1)
       DSMAX=RAP(3)
       DSOLD=RAP(5)
       EPSS=RAP(13)
C
C Check whether FNCS has changed sign (FNCS is EXTERNAL).
C
       Q0=Q
       Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,
     *  RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
       PQ=Q0*Q1
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
       IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
         Q=Q1
         RETURN
       ENDIF
C
C Use the secant method for the first step:
C
       S0=0.d0
       S1=DSOLD
       ITLCSP=0
       DQ=Q0-Q1
       RDS=Q1/DQ*(S1-S0)
 1     RDS=(1.d0+HMACH)*RDS
       S=S1+RDS
C
C Return if relative tolerance has been met :
C
       RRDS=DABS(RDS)/(1+DSQRT(DABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS)THEN
         ITP=-1
         IAP(27)=ITP
         Q=0.d0
         WRITE(9,102)RDS
         RETURN
       ENDIF
C
C If requested write additional output on unit 9 :
C
       IF(IID.GE.2)THEN
          WRITE(9,101)ITLCSP,RDS
       ENDIF
C
       CALL CONTAE(IAP,RAP,RDS,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT)
       CALL SOLVAE(IAP,RAP,PAR,ICP,FUNI,RDS,M1AA,AA,RHS,
     *  RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)THEN
         Q=0.d0
         RETURN
       ENDIF
C
       Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,
     *  RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
       ITLCSP=ITLCSP+1
       IF(ITLCSP.LE.ITMX)THEN
C        Use Mueller's method with bracketing for subsequent steps
         CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
         GOTO 1
       ELSE
         WRITE(9,103)IBR,MOD(NTOT-1,9999)+1
         Q=0.d0
         RETURN
       ENDIF
C
 101   FORMAT(' ==> Location of special point :  Iteration ',I3,
     *  '  Step size = ',1PE13.5)
 102   FORMAT(' ==> Location of special point : ',
     *        ' Convergence.   Step size = ',1PE13.5)
 103   FORMAT(I4,I6,' NOTE:Possible special point')
      END
C
C     ---------- -------
      SUBROUTINE MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
C
C Mueller's method with bracketing
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
        H0=S0-S
        H1=S1-S
        D=H0*H1*(H1-H0)
        A=( H1**2*(Q0-Q) - H0**2*(Q1-Q) ) / D
        B=(-H1*(Q0-Q)    + H0*(Q1-Q)    ) / D
        IF(DABS(B).LE.RSMALL)THEN
          RDS=-Q/A
        ELSE
          C=A/(2*B)
          R=DSQRT(C**2-Q/B)
          IF(C.LT.0.d0)THEN
            RDS=-C - R
          ELSE
            RDS=-C + R
          ENDIF
        ENDIF
C
        DQ=Q1*Q
        IF(DQ.LT.0.d0)THEN
          Q0=Q1
          S0=S1
        ENDIF
        Q1=Q
        S1=S
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNBPAE
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,
     *  UDOT,RHS,DFDU,DFDP,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL CHNG
C
      DIMENSION IAP(*),RAP(*)
C
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       DET=RAP(14)
       FNBPAE=DET
       CHNG=.TRUE.
C
C If requested write additional output on unit 9 :
C
       IF(IID.GE.2)WRITE(9,101)IBR,NTOP+1,FNBPAE
 101   FORMAT(I4,I6,9X,'BP   Function:',1PE14.5)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNLPAE
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,
     * RHS,DFDU,DFDP,IUZ,VUZ)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      EXTERNAL FUNI
C
      DIMENSION IAP(*),RAP(*)
      DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DFDU(*),DFDP(*) 
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
C Local
      ALLOCATABLE UD(:),IR(:),IC(:)
C
      LOGICAL CHNG
C
       NDIM=IAP(1)
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
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
C
       ALLOCATE(UD(NDIM+1),IR(NDIM+1),IC(NDIM+1))
       CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,UD,NDIM+1,RHS,IR,IC,DET)
       RAP(14)=DET
       CALL NRMLZ(NDIM+1,UD)
       FNLPAE=UD(NDIM+1)
       DEALLOCATE(UD,IR,IC)
       RAP(16)=FNLPAE
       CHNG=.TRUE.
C
C If requested write additional output on unit 9 :
C
       IF(IID.GE.2)WRITE(9,101)IABS(IBR),NTOP+1,FNLPAE
 101   FORMAT(I4,I6,9X,'Fold Function:',1PE14.5)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNHBAE
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,
     * RHS,DFDU,DFDP,IUZ,VUZ)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      EXTERNAL FUNI
C
      DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*)
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*)
C Local
      COMPLEX*16 EV, ZTMP
      ALLOCATABLE EV(:)
      LOGICAL CHNG
C
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
C
C INITIALIZE
C
       CHNG=.FALSE.
C
C Compute the eigenvalues of the Jacobian
C
       CALL EIG(IAP,NDM,NDIM,DFDU,EV,IER)
       IF(IPS.EQ.-1)THEN
         DO I=1,NDM
           IF(DREAL(EV(I)).NE.-1.d0 .OR.
     *        DIMAG(EV(I)).NE. 0.d0)THEN
             EV(I)=LOG(1.d0+EV(I))
           ELSE
             EV(I)= DCMPLX(-RLARGE,0.d0)
           ENDIF
         ENDDO
       ENDIF
C
C Order the eigenvalues by real part.
C
       DO I=1,NDM-1
         RMAX=-RLARGE
         DO J=I,NDM
           RP=DREAL(EV(J))
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
C
C Compute the smallest real part.
C
       RIMHB=0.d0
       AREV=RLARGE
       REV=0.d0
       DO I=1,NDM
         IF(DIMAG(EV(I)).NE.0.d0)THEN
           AR=DABS(DREAL(EV(I)))
           IF(AR.LE.AREV)THEN
             AREV=AR
             REV=DREAL(EV(I))
             RIMHB=DABS(DIMAG(EV(I)))
             IF(RIMHB.NE.0.d0.AND.IABS(ISW).LE.1)PAR(11)=PI(2.d0)/RIMHB
           ENDIF
         ENDIF
       ENDDO
C
C Count the number of eigenvalues with negative real part.
C
C Set tolerance for deciding if an eigenvalue is in the positive
C half-plane. Use, for example, tol=1d-3 for conservative systems.
C
       tol=1.d-5
       NINS1=0
       DO I=1,NDM
         IF(DREAL(EV(I)).LE.tol)NINS1=NINS1+1
       ENDDO
C
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
C
       NTOT=IAP(32)
       NTOTP1=NTOT+1
       IF(IID.GE.2)WRITE(9,101)IABS(IBR),NTOP+1,FNHBAE
       IF(NINS1.EQ.NDM)NTOTP1=-NTOTP1
C
       WRITE(9,102)IABS(IBR),NTOP+1,NINS
       IF(IPS.EQ.-1)THEN
          DO I=1,NDM
             WRITE(9,103)IABS(IBR),NTOP+1,I,EXP(EV(I))
          ENDDO
       ELSE
          DO I=1,NDM
             WRITE(9,103)IABS(IBR),NTOP+1,I,EV(I)
          ENDDO
       ENDIF
C
 101   FORMAT(I4,I6,9X,'Hopf Function:',1PE14.5)
 102   FORMAT(/,I4,I6,9X,'Eigenvalues  :   Stable:',I4)
 103   FORMAT(I4,I6,9X,'Eigenvalue',I3,":",1P2E14.5)
C
      DEALLOCATE(EV)
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNUZAE
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,M1AA,AA,RLCUR,RLOLD,RLDOT,U,UOLD,UDOT,
     * RHS,DFDU,DFDP,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),PAR(*),IUZ(*),VUZ(*)
C
      LOGICAL CHNG
C
       IID=IAP(18)
       IUZR=IAP(26)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       FNUZAE=PAR(IABS(IUZ(IUZR)))-VUZ(IUZR)
       CHNG=.TRUE.
C
       IF(IID.GE.3)WRITE(9,101)IABS(IBR),NTOP+1,IUZR,FNUZAE
 101   FORMAT(I4,I6,9X,'User Func.',I3,1X,1PE14.5)
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                   Branch Switching for Algebraic Problems
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- -----
      SUBROUTINE STBIF(IAP,RAP,PAR,ICP,M1AA,AA,M1SB,STUD,STU,STLA,
     * STLD,RLCUR,RLOLD,RLDOT,U,DU,UDOT,DFDU,DFDP,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Stores branching data in the following arrays :
C        STU    ( the solution vector U )
C        STUD   ( U-dot )
C        STLA   ( PAR(ICP(1)) )
C        STLD  ( PAR(ICP(1))-dot )
C Here the vector ( PAR(ICP(1))-dot , U-dot ) lies in the 2-d nullspace
C at branch point and is perpendicular to the direction vector of
C known branch at this point.
C
      DIMENSION IAP(*),AA(M1AA,*),U(*),DU(*),UDOT(*),DFDU(*),DFDP(*)
      DIMENSION STUD(M1SB,*),STU(M1SB,*),STLA(*),STLD(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
      ALLOCATABLE IR(:),IC(:)
C
       NDIM=IAP(1)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
       NBIF=IAP(35)
C
C Keep track of the number of branch points stored.
C
       IF(NBIF.EQ.NBIFX)WRITE(9,101)IBR,NTOP
       IF(NBIF.GT.NBIFX)THEN
         NBIF=NBIFX
         IAP(35)=NBIF
         RETURN
       ENDIF
C
       DO I=1,NDIM
         DO J=1,NDIM
           AA(I,J)=DFDU((J-1)*NDIM+I)
         ENDDO
       ENDDO
C
       ND1=NDIM+1
       DO I=1,NDIM
         AA(I,ND1)=DFDP((ICP(1)-1)*NDIM+I)
         AA(ND1,I)=UDOT(I)
       ENDDO
       AA(ND1,ND1)=RLDOT(1)
C
       ALLOCATE(IR(NDIM+1),IC(NDIM+1))
       CALL NLVC(ND1,M1AA,1,AA,DU,IR,IC)
       DEALLOCATE(IR,IC)
C
       SS=0.d0
       DO I=1,NDIM
         SS=SS+THU(I)*DU(I)**2
       ENDDO
       SS=SS+THL(ICP(1))*DU(ND1)**2
       SC=1.d0/DSQRT(SS)
C
       DO I=1,ND1
         DU(I)=SC*DU(I)
       ENDDO
C
       NBIF=IAP(35)
       STLD(NBIF)=DU(ND1)
       DO I=1,NDIM
         STU(NBIF,I)=U(I)
         STUD(NBIF,I)=DU(I)
       ENDDO
       STLA(NBIF)=RLCUR(1)
C
      RETURN
 101   FORMAT(I4,I6,' NOTE:No more branch points can be stored')
      END
C
C     ---------- -----
      SUBROUTINE SWPNT(IAP,RAP,PAR,ICP,RDS,M1SB,STUD,STU,STLA,STLD,
     * RLCUR,RLOLD,RLDOT,U,UDOT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C This subroutine retrieves the branching data U, U-dot, PAR(ICP(1)),
C PAR(ICP(1))-dot. If this initialization corresponds to the computation
C of the bifurcating branch in opposite direction, then only the sign of
C  the stepsize ( DS ) along the branch is reversed.
C
      DIMENSION IAP(*),RAP(*)
      DIMENSION U(*),UDOT(*),STUD(M1SB,*),STU(M1SB,*),STLA(*),STLD(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
C
       NDIM=IAP(1)
       ISW=IAP(10)
       MXBF=IAP(17)
       NBIF=IAP(35)
       IPOS=IAP(36)
C
       DS=RAP(1)
C
       RDS=DS
       IF(IPOS.EQ.0)RDS=-DS
       RLCUR(1)=STLA(1)
       PAR(ICP(1))=RLCUR(1)
       RLDOT(1)=STLD(1)
       DO I=1,NDIM
         U(I)=STU(1,I)
         UDOT(I)=STUD(1,I)
       ENDDO
       IF(IABS(ISW).EQ.2)PAR(ICP(2))=U(NDIM)
C
       IF(MXBF.GE.0)THEN
         IPOS=1-IPOS
         IAP(36)=IPOS
       ENDIF
       IF(IPOS.EQ.0)RETURN
C
       DO I=1,NBIF
         STLA(I)=STLA(I+1)
         STLD(I)=STLD(I+1)
         DO I1=1,NDIM
           STU(I,I1)=STU(I+1,I1)
           STUD(I,I1)=STUD(I+1,I1)
         ENDDO
       ENDDO
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE SWPRC(IAP,RAP,PAR,ICP,FUNI,M1AA,AA,RHS,
     * RLCUR,RLOLD,RLDOT,U,DU,UOLD,UDOT,F,DFDU,DFDP,RDS,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Controls the computation of the second point on a bifurcating branch.
C This point is required to lie in a hyper-plane at distance DS from the
C branch point. This hyper-plane is parallel to the tangent of the
C known branch at the branch point.
C
      EXTERNAL FUNI
C
      DIMENSION AA(M1AA,*),RHS(*),U(*),UOLD(*),UDOT(*),DU(*)
      DIMENSION IAP(*),RAP(*),F(*),DFDU(*),DFDP(*),THL(*),THU(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
C Local
      ALLOCATABLE IR(:),IC(:),U1(:)
C
       NDIM=IAP(1)
       IADS=IAP(8)
       IID=IAP(18)
       ITNW=IAP(20)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       DSMIN=RAP(2)
       DSMAX=RAP(3)
       EPSL=RAP(11)
       EPSU=RAP(12)
C
C Initialize and provide initial guess :
C
       ALLOCATE(IR(NDIM+1),IC(NDIM+1),U1(NDIM+1))
       RLOLD(1)=RLCUR(1)
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         UOLD(I)=U(I)
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
C
 2     DSOLD=RDS
       RAP(5)=DSOLD
       NIT=0
       IAP(31)=NIT
C
C Write additional output on unit 9 if requested :
C
       NDMR=NDIM
       IF(NDMR.GT.6)NDMR=6
       IF(IID.GE.2)WRITE(9,101)IBR,NTOP,NIT,ICP(1),
     *   RLCUR(1),(U(I),I=1,NDMR)
C
       RLM1=RLCUR(1)
       DO I=1,NDIM
         U1(I)=U(I)
       ENDDO
C
       DO 3 NIT1=1,ITNW
C
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
         AA(NDIM+1,NDIM+1)=THL(ICP(1))*RLDOT(1)
         SS=0.d0
         DO I=1,NDIM
           SS=SS+THU(I)*(U(I)-U1(I))*UDOT(I)
         ENDDO
         RHS(NDIM+1)=-SS-THL(ICP(1))*(RLCUR(1)-RLM1)*RLDOT(1)
C
C Use Gauss elimination with pivoting to solve the linearized system :
C
         IF(IID.GE.5)CALL WRJAC(IAP,NDIM+1,M1AA,AA,RHS)
         CALL GE(0,NDIM+1,M1AA,AA,1,NDIM+1,DU,NDIM+1,RHS,IR,IC,DET)
         RAP(14)=DET
         DRLM=DU(NDIM+1)
C
C Add the Newton increments :
C
         DO I=1,NDIM
           U(I)=U(I)+DU(I)
         ENDDO
         RLCUR(1)=RLCUR(1)+DRLM
         DUMX=0.d0
         UMX=0.d0
         DO I=1,NDIM
           ADU=DABS(DU(I))
           IF(ADU.GT.DUMX)DUMX=ADU
           AU=DABS(U(I))
           IF(AU.GT.UMX)UMX=AU
         ENDDO
C
         IF(IID.GE.2)THEN
           WRITE(9,101)IBR,NTOP,NIT,ICP(1),RLCUR(1),(U(I),I=1,NDMR)
         ENDIF
C
C Check whether relative error has reached user-supplied tolerance :
C
         RDRLM=DABS(DRLM)/(1.d0+DABS(RLCUR(1)))
         RDUMX=DUMX/(1.d0+UMX)
         IF(RDRLM.LT.EPSL.AND.RDUMX.LT.EPSU)THEN
           DEALLOCATE(IR,IC,U1)
           RETURN
         ENDIF
 3     CONTINUE
C
C Maximum number of iterations reached. Reduce stepsize and try again.
C
       IF(IADS.EQ.0)WRITE(9,102)IBR,NTOP
       IF(IADS.EQ.0)GOTO 5
C
       MXT=ITNW
       IAP(31)=MXT
       CALL ADPTDS(IAP,RAP,RDS)
       IF(DABS(RDS).LT.DSMIN)GOTO 4
       RLCUR(1)=RLOLD(1)+RDS*RLDOT(1)
       DO I=1,NDIM
         U(I)=UOLD(I)+RDS*UDOT(I)
       ENDDO
       IF(IID.GE.2)WRITE(9,103)IBR,NTOP
       GOTO 2
C
C Minimum stepsize reached.
C
 4     WRITE(9,104)IBR,NTOP
 5     RLCUR(1)=RLOLD(1)
       PAR(ICP(1))=RLCUR(1)
       DO I=1,NDIM
         U(I)=UOLD(I)
       ENDDO
       ISTOP=1
       IAP(34)=ISTOP
C
       DEALLOCATE(IR,IC,U1)
      RETURN
 101   FORMAT(' Branch ',I2,' N=',I5,1X,'IT=',I2,1X,'PAR(',I2,')=',
     * 1PE11.3,1X,'U=',1P7E11.3)
 102   FORMAT(I4,I6,' NOTE:No convergence when switching branches',
     *        ' with fixed step size')
 103   FORMAT(I4,I6,' NOTE:Retrying step')
 104   FORMAT(I4,I6,' NOTE:No convergence when switching branches',
     *        ' with minimum step size')
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Output (Algebraic Problems)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE STHD(IAP,RAP,PAR,ICP,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Write the values of the user defined parameters on unit 7.
C This identifying information is preceded by a '   0' on each line.
C The first line in the file contains the (generally) user-supplied
C limits of the bifurcation diagram, viz. RL0,RL1,A0 and A1.
C These are often convenient for an initial plot of the diagram.
C
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),THL(*),THU(*)
       CHARACTER (LEN=*), PARAMETER :: D3 = "('   0'3(A8,ES11.4))"
       CHARACTER (LEN=*), PARAMETER :: I4 = "('   0'4(A8,I4))"
       CHARACTER (LEN=*), PARAMETER :: I5 = "('   0'3(A8,I4),2(A7,I4))"
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ILP=IAP(4)
       NTST=IAP(5)
       NCOL=IAP(6)
       IAD=IAP(7)
       ISP=IAP(9)
       ISW=IAP(10)
       IPLT=IAP(11)
       NBC=IAP(12)
       NINT=IAP(13)
       NMX=IAP(14)
       NUZR=IAP(15)
       NPR=IAP(16)
       MXBF=IAP(17)
       IID=IAP(18)
       ITMX=IAP(19)
       ITNW=IAP(20)
       NWTN=IAP(21)
       JAC=IAP(22)
       NFPR=IAP(29)
       NICP=IAP(41)
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
C
       WRITE(7,"(I4,' ',4ES12.4)")0,RL0,RL1,A0,A1
       WRITE(7,D3)'EPSL=',EPSL,'EPSU =',EPSU, 'EPSS =',EPSS
       WRITE(7,D3)'DS  =',DS,  'DSMIN=',DSMIN,'DSMAX=',DSMAX
       WRITE(7,I4)'NDIM=',NDIM,'IPS =',IPS, 'IRS =',IRS, 'ILP =',ILP
       WRITE(7,I4)'NTST=',NTST,'NCOL=',NCOL,'IAD =',IAD, 'ISP =',ISP
       WRITE(7,I4)'ISW =',ISW, 'IPLT=',IPLT,'NBC =',NBC, 'NINT=',NINT
       WRITE(7,I4)'NMX= ',NMX, 'NPR =',NPR, 'MXBF=',MXBF,'IID =',IID
       WRITE(7,I5)'ITMX=',ITMX,'ITNW=',ITNW,'NWTN=',NWTN,'JAC=',JAC,
     *  '  NUZR=',NUZR
C
       WRITE(7,"('   0   User-specified parameter')",ADVANCE="NO")
       IF(NICP.EQ.1)THEN
         WRITE(7,"(':       ',  I4)")(ICP(NPARX+I),I=1,NICP)
       ELSE
         WRITE(7,"('s:      ',24I4)")(ICP(NPARX+I),I=1,NICP)
       ENDIF
C
       WRITE(7,"('   0   Active continuation parameter')",ADVANCE="NO")
       IF(NFPR.EQ.1)THEN
         WRITE(7,"(':  ',  I4)")(ICP(I),I=1,NFPR)
       ELSE
         WRITE(7,"('s: ',24I4)")(ICP(I),I=1,NFPR)
       ENDIF
       CALL FLUSH(7)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE HEADNG(IAP,PAR,ICP,IUNIT,N1,N2)
C
      IMPLICIT NONE
C
C Prints headings above columns on unit 6, 7, and 9.
C N1 = number of parameters to print (maximum: 7 for screen output)
C N2 = number of (max) variables to print (maximum: max(0,7-N1,7))
C
      INTEGER IAP(*),ICP(*),IUNIT,N1,N2
      DOUBLE PRECISION PAR(*)
C Local
      INTEGER I,J,IPS,ISW,IPLT,NDM,ITP
C
       IPS=IAP(2)
       IPLT=IAP(11)
       NDM=IAP(23)
C
       IF(IUNIT.EQ.7)THEN
          WRITE(7,"(I4/I4,A)",ADVANCE="NO")0,0,'    PT  TY  LAB '
       ELSE
          WRITE(IUNIT,"(1X/A)",ADVANCE="NO")'  BR    PT  TY  LAB '
       ENDIF
C
       DO J=1,N1+N2+1
          IF(J==1.OR.J>N2+2)THEN
             I=1
             IF(J>1)I=J-N2-1
             IF(ICP(I)==11.AND.IPS>0.AND.IPS/=4)THEN
                CALL WRITECOL(5,'PERIOD')
             ELSEIF(ICP(I)==10.AND.(IPS==5.OR.IPS==15))THEN
                CALL WRITECOL(6,'FOPT')
             ELSEIF(ICP(I)==14.AND.(IPS==14.OR.IPS==16))THEN
                CALL WRITECOL(6,'TIME')
             ELSE
                CALL WRITECOL(4,'PAR',ICP(I))
             ENDIF
          ELSEIF(J==2)THEN
             IF(IPLT>NDM.AND.IPLT<=2*NDM) THEN
                CALL WRITECOL(2,'INTEGRAL U',IPLT-NDM)
             ELSE IF(IPLT>2*NDM.AND.IPLT<=3*NDM) THEN
                CALL WRITECOL(2,'L2-NORM U',IPLT-2*NDM)
             ELSE IF(IPLT>0.AND.IPLT<=NDM) THEN
                IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                   CALL WRITECOL(6,'U',IPLT)
                ELSE
                   CALL WRITECOL(4,'MAX U',IPLT)
                ENDIF
             ELSE IF(IPLT<0.AND.IPLT>=-NDM) THEN
                IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                   CALL WRITECOL(6,'U',-IPLT)
                ELSE
                   CALL WRITECOL(4,'MIN U',-IPLT)
                ENDIF
             ELSE
                CALL WRITECOL(4,'L2-NORM')
             ENDIF
          ELSE !J>2 with N2>0
             IF(ABS(IPS)<=1.OR.IPS==5.OR.IPS==11)THEN
                CALL WRITECOL(6,'U',J-2)
             ELSE
                CALL WRITECOL(4,'MAX U',J-2)
             ENDIF
          ENDIF
C
       ENDDO
C
       WRITE(IUNIT,"()")
       CALL FLUSH(IUNIT)
      RETURN
      CONTAINS

      SUBROUTINE WRITECOL(I,S,N)
      INTEGER, INTENT(IN) :: I
      CHARACTER(*), INTENT(IN) :: S
      INTEGER, INTENT(IN), OPTIONAL :: N
C Local
      CHARACTER(10) SN
      CHARACTER(19) COL
      COL=' '
      IF(PRESENT(N))THEN
         WRITE(SN,"(I10)")N
         WRITE(COL(I:),"(A,A,A,A)") S,'(',TRIM(ADJUSTL(SN)),')'
      ELSE
         WRITE(COL(I:),"(A)") S
      ENDIF
      IF(IUNIT.EQ.7)THEN
         WRITE(IUNIT,"(A19)",ADVANCE="NO")COL
      ELSE
         WRITE(IUNIT,"(A14)",ADVANCE="NO")COL
      ENDIF
      END SUBROUTINE WRITECOL

      END SUBROUTINE HEADNG
C
C     ---------- ------
      SUBROUTINE STPLAE(IAP,RAP,PAR,ICP,RLCUR,U)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Stores the bifurcation diagram on unit 7 (Algebraic Problems).
C Every line written contains, in order, the following:
C
C  IBR    : The label of the branch.
C  NTOT   : The index of the point on the branch.
C           (Points are numbered consecutively along a branch).
C           If IPS=1 or -1, then the sign of NTOT indicates stability :
C            - = stable , + = unstable, unknown, or not relevant.
C  ITP    : An integer indicating the type of point :
C
C             1  (BP)  :   Branch point.
C             2  (LP)  :   Fold.
C             3  (HB)  :   Hopf bifurcation point.
C             4  (  )  :   Output point (Every NPR steps along branch).
C            -4  (UZ)  :   Output point (Zero of user function).
C             9  (EP)  :   End point of branch, normal termination.
C            -9  (MX)  :   End point of branch, abnormal termination.
C
C  LAB        : The label of a special point.
C  PAR(ICP(1)): The principal parameter.
C  A          : The L2-norm of the solution vector, or other measure of
C               the solution (see the user-supplied parameter IPLT).
C  U          : The first few components of the solution vector.
C  PAR(ICP(*)): Further free parameters (if any).
C
      DIMENSION IAP(*),ICP(*),RAP(*),RLCUR(*),U(*)
C
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
C
       RL0=RAP(6)
       RL1=RAP(7)
       A0=RAP(8)
       A1=RAP(9)
C
       NTOT=IAP(32)
       NTOT=NTOT+1
       IAP(32)=NTOT
C
       CALL PVLSAE(IAP,RAP,U,PAR)
C
C ITP is set to 4 every NPR steps along a branch, and the entire
C solution is written on unit 8.
C
       IF(NPR.NE.0)THEN
         IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
         IAP(27)=ITP
       ENDIF
C
C CHECK WHETHER LIMITS OF THE BIFURCATION DIAGRAM HAVE BEEN REACHED :
C
       IAB=IABS(IPLT)
C
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
C
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)THEN
C        Maximum number of iterations reached somewhere.
         ITP=-9-10*ITPST
         IAP(27)=ITP
       ELSEIF(ISTOP.EQ.-1)THEN
C        ** UZR endpoint
         ITP=9+10*ITPST
         IAP(27)=ITP
       ELSE
         IF(RLCUR(1).LT.RL0.OR.RLCUR(1).GT.RL1 
     *     .OR. AMP.LT.A0.OR.AMP.GT.A1
     *     .OR. NTOT.EQ.NMX) THEN
           ISTOP=1
           IAP(34)=ISTOP
           ITP=9+10*ITPST
           IAP(27)=ITP
         ENDIF
       ENDIF
C
       LABW=0
       IF(MOD(ITP,10).NE.0)THEN
         LAB=IAP(37)
         LAB=LAB+1
         IAP(37)=LAB
         LABW=LAB
       ENDIF
C
C Determine stability and print output on units 6 and 7.
C
       NTOTS=NTOT
       NINS=IAP(33)
       IF(IABS(IPS).EQ.1 .AND. IABS(ISW).LE.1 .AND. NTOT.GT.1)THEN
         IF(NINS.EQ.NDIM)NTOTS=-NTOT
       ENDIF
       CALL WRLINE(IAP,PAR,ICP,ICP(NPARX+1),IBR,NTOTS,LABW,AMP,U)
C
C Write restart information for multi-parameter analysis :
C
       IF(LABW.NE.0)CALL WRTSP8(IAP,RAP,PAR,ICP,LABW,RLCUR,U)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE WRLINE(IAP,PAR,ICP,ICU,IBR,NTOT,LAB,VAXIS,U)
C
      IMPLICIT NONE
C
C Write one line of output on unit 6 and 7.
C
      INTEGER IAP(*),ICP(*),ICU(*),IBR,NTOT,LAB
      DOUBLE PRECISION PAR(*),U(*),VAXIS
C Local
      CHARACTER*2 ATYPE
      CHARACTER*2, PARAMETER :: ATYPESP(9) =
     *     (/ 'BP','LP','HB','  ','LP','BP','PD','TR','EP' /)
      CHARACTER*2, PARAMETER :: ATYPESN(9) =
     *     (/ '  ','  ','  ','UZ','  ','  ','  ','  ','MX' /)
      CHARACTER(33) :: F69 ! (I4,I6,2X,A2,I5,**********ES14.5)
      CHARACTER(31) :: F7  ! (I4,I6,I4,I5,**********ES19.10)
      INTEGER MTOT,NDM,ITP,NICP,N1,N2,I
C
       NDM=IAP(23)
       ITP=IAP(27)
       NICP=IAP(41)
C
       N1=NICP
       N2=NDM
C
       IF(N1.GT.7)THEN
         N1=7
         N2=0
       ELSEIF(N1+N2.GT.7)THEN
         N2=7-N1
       ENDIF
C
C Write a heading above the first line.
C
       IF(IABS(NTOT).EQ.1)CALL HEADNG(IAP,PAR,ICU,6,N1,N2)
       IF(IABS(NTOT).EQ.1)CALL HEADNG(IAP,PAR,ICU,7,NICP,N2)
       CALL HEADNG(IAP,PAR,ICU,9,N1,N2)
C
       IF(MOD(ITP,10)>0)THEN
         ATYPE=ATYPESP(MOD(ITP,10))
       ELSEIF(MOD(ITP,10)<0)THEN
         ATYPE=ATYPESN(-MOD(ITP,10))
       ELSE
         ATYPE='  '
       ENDIF
C
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(F69,"(A,I10,A)") '(I4,I6,2X,A2,I5,',N1+N2+1,'ES14.5)'
       WRITE(F7,"(A,I10,A)") '(I4,I6,I4,I5,',NICP+N2+1,'ES19.10)'
       IF(MOD(ITP,10).NE.0)THEN
          WRITE(6,F69)ABS(IBR),ABS(MTOT),ATYPE,LAB,PAR(ICU(1)),VAXIS,
     *         (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
          CALL FLUSH(6)
       ENDIF
       WRITE(7,F7)IBR,MTOT,ITP,LAB,PAR(ICU(1)),VAXIS,
     *      (U(I),I=1,N2),(PAR(ICU(I)),I=2,NICP)
       CALL FLUSH(7)
       WRITE(9,F69)IBR,MTOT,ATYPE,LAB,PAR(ICU(1)),VAXIS,
     *      (U(I),I=1,N2),(PAR(ICU(I)),I=2,N1)
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE WRBAR(C,N)
C
      CHARACTER*1 C
        WRITE(9,101)(C,I=1,N)
 101    FORMAT(80A1)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE WRTSP8(IAP,RAP,PAR,ICP,LAB,RLCUR,U)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Write restart information on singular points, plotting points, etc.,
C on unit 8.
C
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),RLCUR(*),U(*)
C
       NDIM=IAP(1)
       ISW=IAP(10)
       ITP=IAP(27)
       IBR=IAP(30)
       NFPR=IAP(29)
       NTOT=IAP(32)
C
       NTPL=1
       NAR=NDIM+1
       NROWPR=NDIM/7+1 + (NPARX-1)/7+1
       PAR(ICP(1))=RLCUR(1)
       T=0.d0
       AMP=0.d0
       RAP(10)=AMP
C       
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,0,0,NPARX
       WRITE(8,102)T,(U(I),I=1,NDIM)
       WRITE(8,102)(PAR(I),I=1,NPARX)
C
 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,1P7E19.10)
C
       CALL FLUSH(8)
       RETURN
       END
C
C     ---------- ------
      SUBROUTINE WRJAC(IAP,N,M1AA,AA,RHS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION AA(M1AA,*),RHS(*),IAP(*)
C
       WRITE(9,101)
       WRITE(9,100)(RHS(I),I=1,N)
       WRITE(9,102)
       DO I=1,N
         WRITE(9,100)(AA(I,J),J=1,N)
       ENDDO
C
 100   FORMAT(1X,12E10.3)
 101   FORMAT(/,' Residual vector :')
 102   FORMAT(/,' Jacobian matrix :')
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Mesh and Weight Generation
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ---
      SUBROUTINE MSH(IAP,RAP,TM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates a uniform mesh on [0,1].
C
      DIMENSION IAP(*),TM(*)
C
       NTST=IAP(5)
C
       TM(1)=0.d0
       DT=1.d0/NTST
       DO J=1,NTST
         TM(J+1)=J*DT
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE GENWTS(NCOL,N1,WT,WP)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates weights of the collocation method. The user selected
C number of collocation points (ncol) must be one of { 2,...,7 }.
C
C The following weights are generated :
C
C         WT : for the function value,
C         WP : for the first derivative,
C
      DIMENSION WT(N1,*),WP(N1,*)   
C Local
      DIMENSION ZM(NCOL),XM(NCOL+1)
C
C Generate the collocation points :
       CALL CPNTS(NCOL,ZM)
C
       NCP1=NCOL+1
       D=1.d0/NCOL
       DO I=1,NCP1
         XM(I)=(I-1)*D
       ENDDO
C
C Generate weights :
C
       DO IB=1,NCP1
         DENOM=1.d0
         DO K=1,NCP1
           IF(K.NE.IB)DENOM=DENOM*( XM(IB)-XM(K) )
         ENDDO
         DO IC=1,NCOL
C Weights for the function values :
           P=1.d0
           DO K=1,NCP1
             IF(K.NE.IB)P=P*( ZM(IC)-XM(K) )
           ENDDO
           WT(IB,IC)=P/DENOM
C Weights for derivatives :
           SUM=0.d0
           DO L=1,NCP1
             IF(L.NE.IB)THEN
               P=1.d0
               DO K=1,NCP1
                 IF(K.NE.IB.AND.K.NE.L)P=P*( ZM(IC)-XM(K) )
               ENDDO
               SUM=SUM+P
             ENDIF
           ENDDO
           WP(IB,IC)=SUM/DENOM
         ENDDO
       ENDDO
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE CPNTS(NCOL,ZM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the collocation points with respect to [0,1].
C
      DIMENSION ZM(*)
C
       GOTO (2,3,4,5,6,7)NCOL-1
C
 2     C=.5d0/DSQRT(3.0d0)
       ZM(1)=.5d0-C
       ZM(2)=.5d0+C
      RETURN
C
 3     C=.5d0*DSQRT(0.6d0)
       ZM(1)=.5d0-C
       ZM(2)=.5d0
       ZM(3)=.5d0+C
      RETURN
C
 4     R=6.0d0/7.0d0
       C=.5d0*DSQRT(R**2-12.0d0/35.0d0)
       C1=.5d0*DSQRT(3.0d0/7.0d0+C)
       C2=.5d0*DSQRT(3.0d0/7.0d0-C)
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0+C2
       ZM(4)=.5d0+C1
      RETURN
C
 5     C1=.5d0*0.90617984593866399280d0
       C2=.5d0*0.53846931010568309104d0
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0
       ZM(4)=.5d0+C2
       ZM(5)=.5d0+C1
      RETURN
C
 6     C1=.5d0*0.93246951420315202781d0
       C2=.5d0*0.66120938646626451366d0
       C3=.5d0*0.23861918608319690863d0
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0-C3
       ZM(4)=.5d0+C3
       ZM(5)=.5d0+C2
       ZM(6)=.5d0+C1
      RETURN
C
 7     C1=.5d0*0.949107991234275852452d0
       C2=.5d0*0.74153118559939443986d0
       C3=.5d0*0.40584515137739716690d0
       ZM(1)=.5d0-C1
       ZM(2)=.5d0-C2
       ZM(3)=.5d0-C3
       ZM(4)=.5d0
       ZM(5)=.5d0+C3
       ZM(6)=.5d0+C2
       ZM(7)=.5d0+C1
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE CNTDIF(N,D)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the coefficients of the central difference formula for
C Nth derivative at uniformly spaced points
C              0 = x  < x  < ... < x  = 1.
C                   0    1          N
C
      DIMENSION D(*)
C
       D(1)=1.d0
       IF(N.EQ.0)RETURN
C
       DO I=1,N
         D(I+1)=0.d0
         DO K=1,I
           K1=I+2-K
           D(K1)=D(K1-1)-D(K1)
         ENDDO
         D(1)=-D(1)
       ENDDO
C
C Scale to [0,1]  :
C
       SC=N**N
       NP1=N+1
       DO I=1,NP1
         D(I)=SC*D(I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE WINT(N,WI)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the weights for the integration formula based on polynomial
C interpolation at N equally spaced points in [0,1].
C
      DIMENSION WI(*)
C
       GOTO (3,4,5,6,7,8)N-2
C
 3     C=1.d0/6.0d0
       WI(1)=C
       WI(2)=4.0d0*C
       WI(3)=C
      RETURN
C
 4     C=1.d0/8.0d0
       WI(1)=C
       WI(2)=3.0d0*C
       WI(3)=WI(2)
       WI(4)=C
      RETURN
C
 5     C=1.d0/90.0d0
       WI(1)=7.0d0*C
       WI(2)=32.0d0*C
       WI(3)=12.0d0*C
       WI(4)=WI(2)
       WI(5)=WI(1)
      RETURN
C
 6     WI(1)=19.0d0/288.0d0
       WI(2)=25.0d0/96.0d0
       WI(3)=25.0d0/144.0d0
       WI(4)=WI(3)
       WI(5)=WI(2)
       WI(6)=WI(1)
      RETURN
C
 7     WI(1)=41.0d0/840.0d0
       WI(2)=9.0d0/35.0d0
       WI(3)=9.0d0/280.0d0
       WI(4)=34.0d0/105.0d0
       WI(5)=WI(3)
       WI(6)=WI(2)
       WI(7)=WI(1)
      RETURN
C
 8     WI(1)=751.0d0/17280.0d0
       WI(2)=3577.0d0/17280.0d0
       WI(3)=49.0d0/640.0d0
       WI(4)=2989.0d0/17280.0d0
       WI(5)=WI(4)
       WI(6)=WI(3)
       WI(7)=WI(2)
       WI(8)=WI(1)
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C          Stepsize and Mesh Adaption
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE ADPTDS(IAP,RAP,RDS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C The stepsize along the branch of solutions is adapted depending on the
C number of Newton iterations in the previous step (called if IADS > 0).
C
      DIMENSION IAP(*),RAP(*)
C
       DSMAX=RAP(3)
       IID=IAP(18)
       ITNW=IAP(20)
       IBR=IAP(30)
       NIT=IAP(31)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       IF(ITNW.LE.3) THEN
         ITNW=3
         N1=2
       ELSE
         N1=ITNW/2
       ENDIF
C
       IF(NIT.LE.1) THEN
         RDS= 2.d0*RDS
       ELSE IF(NIT.EQ.2) THEN
         RDS= 1.5*RDS
       ELSE IF(NIT.GT.2 .AND. NIT.LE.N1) THEN
         RDS= 1.1*RDS
       ELSE IF(NIT.GE.ITNW) THEN
         RDS=.5d0*RDS
       ENDIF
C
       ARDS= ABS(RDS)
       IF(ARDS.GT.DSMAX)RDS=RDS*DSMAX/ARDS
C
       WRITE(9,101)IABS(IBR),NTOP,NIT
       WRITE(9,102)IABS(IBR),NTOP,RDS
 101   FORMAT(/,I4,I6,8X,' Iterations   : ',I3)
 102   FORMAT(I4,I6,8X,' Next Step    : ',1PE13.5)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE ADAPT(IAP,RAP,NOLD,NCOLD,NNEW,NCNEW,TM,DTM,NDX,UPS,VPS)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Adapts the distribution of the mesh points so that the increase of the
C monotone function EQDF becomes approximately equidistributed over the
C intervals. The functions UPS and VPS are interpolated on new mesh.
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),VPS(NDX,*),TM(*),DTM(*)
C Local
      ALLOCATABLE TINT(:),UINT(:,:),TM2(:),ITM(:)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       ISW=IAP(10)
C
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
       NRWNEW=NDIM*NCNEW
       ALLOCATE(TINT(NNEWP1),UINT(NRWNEW,NNEWP1))
       ALLOCATE(TM2(NNEWP1),ITM(NNEWP1))
C
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UINT(I,J)=0.d0
         ENDDO
       ENDDO
C
C For periodic boundary conditions extrapolate by periodicity.
C
       IF(IPS.EQ.2 .AND. IABS(ISW).LE.1) THEN
         IPER=1
       ELSE
         IPER=0
       ENDIF
C
C Generate the new mesh :
C
       CALL NEWMSH(IAP,RAP,NDX,UPS,NOLD,NCOLD,TM,DTM,NNEW,TINT,IPER)
C
C Replace UPS by its interpolant on the new mesh :
C
       CALL INTERP(IAP,RAP,NDIM,NOLDP1,NCOLD,TM,NDX,UPS,NNEWP1,NCNEW,
     *  TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UPS(I,J)=UINT(I,J)
         ENDDO
       ENDDO
C
C Replace VPS by its interpolant on the new mesh :
C
       CALL INTERP(IAP,RAP,NDIM,NOLDP1,NCOLD,TM,NDX,VPS,NNEWP1,NCNEW,
     *  TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           VPS(I,J)=UINT(I,J)
         ENDDO
       ENDDO
C
C Replace old mesh :
C
       TM(1)=0.d0
       DO J=1,NNEW
         DTM(J)=TINT(J+1)-TINT(J)
         TM(J+1)=TINT(J+1)
       ENDDO
C
      DEALLOCATE(TINT,UINT,TM2,ITM)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INTERP(IAP,RAP,NDIM,N,NC,TM,NDX,UPS,N1,NC1,TM1,UPS1,
     * TM2,ITM1)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.
C
      DIMENSION TM(*),TM1(*),TM2(*),ITM1(*),UPS(NDX,*),UPS1(NC1*NDIM,*)
C Local
      DIMENSION X(NC+1),W(NC+1)
C
       NCP1=NC+1
       N1M1=N1-1
C
       DO I=1,NC1
         RI=I-1
         D=RI/NC1
         DO J1=1,N1M1
           TM2(J1)=TM1(J1)+D*( TM1(J1+1)-TM1(J1) )
         ENDDO
         CALL ORDR(IAP,RAP,N,TM,N1M1,TM2,ITM1)
         DO J1=1,N1M1
           J=ITM1(J1)
           Z=TM2(J1)
           D=( TM(J+1)-TM(J) )/NC
           DO L=1,NCP1
             X(L)=TM(J)+(L-1)*D
           ENDDO
           CALL INTWTS(NCP1,Z,X,W)
           DO K=1,NDIM
             K1=(I-1)*NDIM+K
             UPS1(K1,J1)=W(NCP1)*UPS(K,J+1)
             DO L=1,NC
               L1=K+(L-1)*NDIM
               UPS1(K1,J1)=UPS1(K1,J1)+W(L)*UPS(L1,J)
             ENDDO
           ENDDO
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         UPS1(I,N1)=UPS(I,N)
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE NEWMSH(IAP,RAP,NDX,UPS,NOLD,NCOLD,TMOLD,DTMOLD,
     * NNEW,TMNEW,IPER)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Redistributes the mesh according to the function EQDF.
C
      DIMENSION IAP(*),TMOLD(*),DTMOLD(*),TMNEW(*)
C Local
      ALLOCATABLE EQF(:),UNEQ(:),IAL(:)
      ALLOCATE(EQF(NOLD+1),UNEQ(NNEW+1),IAL(NNEW+1))
C
       NDIM=IAP(1)
C
C Put the values of the monotonely increasing function EQDF in EQF.
C
       CALL EQDF(IAP,RAP,NOLD,NDIM,NCOLD,DTMOLD,NDX,UPS,EQF,IPER)
C
C Uniformly divide the range of EQDF :
C
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
       DAL=EQF(NOLDP1)/NNEW
       DO J=1,NNEWP1
         UNEQ(J)=(J-1)*DAL
       ENDDO
C
       CALL ORDR(IAP,RAP,NOLDP1,EQF,NNEWP1,UNEQ,IAL)
C
C Generate the new mesh in TMNEW :
C
       DO J1=1,NNEW
         J=IAL(J1)
         X=(UNEQ(J1)-EQF(J))/(EQF(J+1)-EQF(J))
         TMNEW(J1)=(1.d0-X)*TMOLD(J)+X*TMOLD(J+1)
       ENDDO
C
C Assign TMNEW(NNEWP1) explicitly because of loss of precision
C problems when EQF(NOLDP1) and EQF(NOLD) are very close
C
       TMNEW(NNEWP1)=TMOLD(NOLDP1)
C
      DEALLOCATE(EQF,UNEQ,IAL)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ORDR(IAP,RAP,N,TM,N1,TM1,ITM1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C TM and TM1 are two ascending arrays with values in [0,1]. On exit the
C value of ITM1( i ) specifies the index of the TM-interval in which
C TM1(i) lies.
C
      DIMENSION TM(N),TM1(N1),ITM1(N1)
C
       K0=2
       DO J1=1,N1
         K1=K0
         DO J=K0,N
           K1=J
           IF(TM1(J1).LT.TM(J))GOTO 1
         ENDDO
 1       ITM1(J1)=K1-1
         K0=K1
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INTWTS(N,Z,X,WTS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates weights for Lagrange interpolation.
C
      DIMENSION X(*),WTS(*)
C
       DO IB=1,N
         P=1.d0
         DENOM=1.d0
         DO K=1,N
           IF(K.NE.IB)THEN
             P=P*( Z-X(K) )
             DENOM=DENOM*( X(IB)-X(K) )
            ENDIF
         ENDDO
         WTS(IB)=P/DENOM
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE EQDF(IAP,RAP,NTST,NDIM,NCOL,DTM,NDX,UPS,EQF,IPER)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),EQF(*),DTM(*)
      LOGICAL SMALL
C Local
      DIMENSION WH(NCOL+1)
      ALLOCATABLE HD(:,:)
      ALLOCATE(HD(NDIM*NCOL,NTST+1))
C
C Compute approximation to NCOL-th derivative :
       CALL CNTDIF(NCOL,WH)
C
       SMALL=.TRUE.
       DO J=1,NTST
         JP1=J+1
         SC=1.d0/DTM(J)**NCOL
         DO I=1,NDIM
           HD(I,J)=WH(NCOL+1)*UPS(I,JP1)
           DO K=1,NCOL
             K1=I+(K-1)*NDIM
             HD(I,J)=HD(I,J)+WH(K)*UPS(K1,J)
           ENDDO
           HD(I,J)=SC*HD(I,J)
           IF(DABS(HD(I,J)).GT.HMACH)SMALL=.FALSE.
         ENDDO
       ENDDO
C
C Take care of "small derivative" case.
C
       IF(SMALL)THEN
         DO I=1,NTST+1
           EQF(I)=I-1
         ENDDO
         DEALLOCATE(HD)
         RETURN
       ENDIF
C
       IF(IPER.EQ.1)THEN
C        *Extend by periodicity :
         DO I=1,NDIM
           HD(I,NTST+1)=HD(I,1)
         ENDDO
         DTM(NTST+1)=DTM(1)
       ELSE
C        *Extend by extrapolation :
         DO I=1,NDIM
           HD(I,NTST+1)=2*HD(I,NTST)-HD(I,NTST-1)
         ENDDO
         DTM(NTST+1)=DTM(NTST)
       ENDIF
C
C Compute approximation to (NCOL+1)-st derivative :
C
       DO J=1,NTST
         JP1=J+1
         DTAV=.5d0*(DTM(J)+DTM(J+1))
         SC=1.d0/DTAV
         DO I=1,NDIM
           HD(I,J)=SC*( HD(I,JP1)-HD(I,J) )
         ENDDO
       ENDDO
C
C Define the equidistribution function :
C
       PWR=1.d0/(NCOL+1.d0)
       EQF(1)=0.d0
       DO J=1,NTST
         E=0.d0
         DO I=1,NDIM
           E=E+DABS( HD(I,J) )**PWR
         ENDDO
         EQF(J+1)=EQF(J)+DTM(J)*E
       ENDDO
C
C
       DEALLOCATE(HD)
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    General Support Routines
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ---
      SUBROUTINE EIG(IAP,NDIM,M1A,A,EV,IER)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C This subroutine uses the EISPACK subroutine RG to compute the
C eigenvalues of the general real matrix A.
C NDIM is the dimension of A.
C M1A is the first dimension of A as in the DIMENSION statement.
C The eigenvalues are to be returned in the complex vector EV.
C
      DIMENSION A(M1A,*),IAP(*)
C
      COMPLEX*16 EV(*)
C Local
      ALLOCATABLE WR(:),WI(:),Z(:),FV1(:),IV1(:)
      ALLOCATE(WR(NDIM),WI(NDIM),Z(M1A*NDIM),FV1(NDIM),IV1(NDIM))
C
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       IER=0
       IF(IID.GE.4)THEN 
         MATZ=1
       ELSE
         MATZ=0
       ENDIF
C
       CALL RG(M1A,NDIM,A,WR,WI,MATZ,Z,IV1,FV1,IER)
       IF(IER.NE.0)IER=1
       IF(IER.EQ.1)WRITE(9,101)IBR,NTOP
C
       IF(MATZ.NE.0)THEN
         WRITE(9,102)
         DO I=1,NDIM
            WRITE(9,104)WR(I),WI(I)
         ENDDO
         WRITE(9,103)
         DO I=1,NDIM
            WRITE(9,104)(Z((I-1)*M1A+J),J=1,NDIM)
         ENDDO
       ENDIF
C
       DO I=1,NDIM
          EV(I) = DCMPLX(WR(I),WI(I))
       ENDDO
C

 101   FORMAT(I4,I6,' NOTE:Error return from EISPACK routine RG')
 102   FORMAT(/,' Eigenvalues:')
 103   FORMAT(/,' Eigenvectors (by row):')
 104   FORMAT(4X,1P7E19.10)
C
      DEALLOCATE(WR,WI,Z,FV1,IV1)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE NLVC(N,M,K,A,U,IR,IC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Finds a null-vector of a singular matrix A.
C The null space of A is assumed to be K-dimensional.
C
C Parameters :
C
C     N : number of equations,
C     M : first dimension of A from DIMENSION statement,
C     K : dimension of nullspace,
C     A : N * N matrix of coefficients,
C     U : on exit U contains the null vector,
C IR,IC : integer arrays of dimension at least N.
C
C
      DIMENSION IR(*),IC(*),A(M,*),U(*)
C
       DO I=1,N
         IC(I)=I
         IR(I)=I
       ENDDO
C
C   Elimination.
C
       NMK=N-K
C
       DO JJ=1,NMK
         IPIV=JJ
         JPIV=JJ
         PIV=0.d0
         DO I=JJ,N
           DO J=JJ,N
             P=DABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
               PIV=P
               IPIV=I
               JPIV=J
             ENDIF
           ENDDO
         ENDDO
         IF(PIV.LT.RSMALL)WRITE(9,101)JJ,RSMALL
C
         KK=IR(JJ)
         IR(JJ)=IR(IPIV)
         IR(IPIV)=KK
C
         KK=IC(JJ)
         IC(JJ)=IC(JPIV)
         IC(JPIV)=KK
C
         JJP1=JJ+1
         DO L=JJP1,N
           RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
           IF(RM.NE.0.d0)THEN
             DO I=JJP1,N
               A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
           ENDIF
         ENDDO
       ENDDO
C
C   Backsubstitution :
C
       DO I=1,K
         U(IC(N+1-I))=1.d0
       ENDDO
C
       DO I1=1,NMK
         I=NMK+1-I1
         SM=0.d0
         IP1=I+1
         DO J=IP1,N
           SM=SM+A(IR(I),IC(J))*U(IC(J))
         ENDDO
         U(IC(I))=-SM/A(IR(I),IC(I))
       ENDDO
C
 101     FORMAT(8x,' NOTE:Pivot ',I3,' < ',E10.3,' in NLVC : ',
     *        /,'        A null space may be multi-dimensional')
C
      RETURN
      END
C
C     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RNRMV(N,V)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION V(*)
C
C Returns the L2-norm of the vector V.
C
       RNRMV = 0.d0
       DO I=1,N
         RNRMV=RNRMV+V(I)**2
       ENDDO
       RNRMV=DSQRT(RNRMV)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE NRMLZ(NDIM,V)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION V(*)
C
C Scale the vector V so that its discrete L2-norm becomes 1.
C
       SS=0.d0
       DO I=1,NDIM
         SS=SS+V(I)*V(I)
       ENDDO
       C=1.d0/DSQRT(SS)
       DO I=1,NDIM
         V(I)=V(I)*C
       ENDDO
C
      RETURN
      END
C
C     ------ --------- --------
      DOUBLE PRECISION FUNCTION PI(R)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
       PI=R*4.0d0*DATAN(1.d0)
C
      RETURN
      END
C
C     ---------- --
      SUBROUTINE GE(IAM,N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Solves the linear system  A U = F by Gauss elimination
C with complete pivoting.
C
C Parameters :
C
C   N   : number of equations,
C   M1A : first dimension of A from DIMENSION statement,
C   A   : N * N matrix of coefficients,
C   NRHS: 0   if no right hand sides (determinant only),
C         >0   if there are NRHS right hand sides,
C   NDX : first dimension of U from DIMENSION statement,
C   U   : on exit U contains the solution vector(s),
C   M1F : first dimension of F from DIMENSION statement,
C   F   : right hand side vector(s),
C  IR,IC: integer vectors of dimension at least N.
C
C The input matrix A is overwritten.
C
      DIMENSION IR(*),IC(*),A(M1A,*),U(NDX,*),F(M1F,*)
C
       DO I=1,N
         IC(I)=I
         IR(I)=I
       ENDDO
C
C   Elimination.
C
       DET=1.d0
       NM1=N-1
C
       DO JJ=1,NM1
         IPIV=JJ
         JPIV=JJ
         PIV=0.d0
         DO I=JJ,N
           DO J=JJ,N
             P=DABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
               PIV=P
               IPIV=I
               JPIV=J
              ENDIF
           ENDDO
         ENDDO
C
         DET=DET*A(IR(IPIV),IC(JPIV))
         IF(IPIV.NE.JJ)DET=-DET
         IF(JPIV.NE.JJ)DET=-DET
C
         IF(PIV.LT.RSMALL)WRITE(9,101)JJ,RSMALL
C
         K=IR(JJ)
         IR(JJ)=IR(IPIV)
         IR(IPIV)=K
C
         K=IC(JJ)
         IC(JJ)=IC(JPIV)
         IC(JPIV)=K
C
         JJP1=JJ+1
         DO L=JJP1,N
           RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
           IF(RM.NE.0.d0)THEN
             DO I=JJP1,N
               A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
             IF(NRHS.NE.0)THEN
               DO IRH=1,NRHS
                 F(IR(L),IRH)=F(IR(L),IRH)-RM*F(IR(JJ),IRH)
               ENDDO
             ENDIF
           ENDIF
         ENDDO
       ENDDO
       DET=DET*A(IR(N),IC(N))
C
       IF(NRHS.EQ.0)RETURN
C
C   Backsubstitution :
C
       DO IRH=1,NRHS
         U(IC(N),IRH)=F(IR(N),IRH)/A(IR(N),IC(N))
         DO I1=1,NM1
           I=N-I1
           SM=0.d0
           IP1=I+1
           DO J=IP1,N
             SM=SM+A(IR(I),IC(J))*U(IC(J),IRH)
           ENDDO
           U(IC(I),IRH)=(F(IR(I),IRH)-SM)/A(IR(I),IC(I))
         ENDDO
       ENDDO
C
 101   FORMAT(8x,' NOTE:Pivot ',I3,' < ',D10.3,' in GE')
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE GESC(N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C Solves the linear system  A U = F by Gauss elimination
C with complete pivoting. Returns a scaled determinant.
C
C Parameters :
C
C   N   : number of equations,
C   M1A : first dimension of A from DIMENSION statement,
C   A   : N * N matrix of coefficients,
C   NRHS: 0   if no right hand sides (determinant only),
C         >0   if there are NRHS right hand sides,
C   NDX : first dimension of U from DIMENSION statement,
C   U   : on exit U contains the solution vector(s),
C   M1F : first dimension of F from DIMENSION statement,
C   F   : right hand side vector(s),
C  IR,IC: integer vectors of dimension at least N.
C
C The input matrix A is overwritten.
C
      DIMENSION IR(*),IC(*),A(M1A,*),U(NDX,*),F(M1F,*)
C
       DO I=1,N
         IC(I)=I
         IR(I)=I
       ENDDO
C
C   Elimination.
C
       DET=1.d0
       NM1=N-1
C
       DO JJ=1,NM1
         IPIV=JJ
         JPIV=JJ
         PIV=0.d0
         DO I=JJ,N
           DO J=JJ,N
             P=DABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
               PIV=P
               IPIV=I
               JPIV=J
              ENDIF
           ENDDO
         ENDDO
C
         AP=A(IR(IPIV),IC(JPIV)) 
         DET=DET*LOG10(10+ABS(AP)) * atan(AP)
         IF(IPIV.NE.JJ)DET=-DET
         IF(JPIV.NE.JJ)DET=-DET
C
         IF(PIV.LT.RSMALL)WRITE(9,101)JJ,RSMALL
C
         K=IR(JJ)
         IR(JJ)=IR(IPIV)
         IR(IPIV)=K
C
         K=IC(JJ)
         IC(JJ)=IC(JPIV)
         IC(JPIV)=K
C
         JJP1=JJ+1
         DO L=JJP1,N
           RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
           IF(RM.NE.0.d0)THEN
             DO I=JJP1,N
               A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
             IF(NRHS.NE.0)THEN
               DO IRH=1,NRHS
                 F(IR(L),IRH)=F(IR(L),IRH)-RM*F(IR(JJ),IRH)
               ENDDO
             ENDIF
           ENDIF
         ENDDO
       ENDDO
       AP=A(IR(N),IC(N)) 
       DET=DET*LOG10(10+ABS(AP)) * atan(AP)
C
       IF(NRHS.EQ.0)RETURN
C
C   Backsubstitution :
C
       DO IRH=1,NRHS
         U(IC(N),IRH)=F(IR(N),IRH)/A(IR(N),IC(N))
         DO I1=1,NM1
           I=N-I1
           SM=0.d0
           IP1=I+1
           DO J=IP1,N
             SM=SM+A(IR(I),IC(J))*U(IC(J),IRH)
           ENDDO
           U(IC(I),IRH)=(F(IR(I),IRH)-SM)/A(IR(I),IC(I))
         ENDDO
       ENDDO
C
 101   FORMAT(8x,' NOTE:Pivot ',I3,' < ',D10.3,' in GE')
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE NEWLAB(IAP,RAP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Determine a suitable label when restarting.
C
      LOGICAL EOF3
      DIMENSION IAP(*),RAP(*)
C
       IPS=IAP(2)
       IRS=IAP(3)
       ISW=IAP(10)
       ITP=IAP(27)
C
       REWIND 3
       MBR=0
       MLAB=0
C
       IF(IRS>0)THEN
          DO
             READ(3,*,END=2)IBRS,NTOTRS,ITPRS,LABRS,NFPRS,ISWRS,NTPLRS,
     *            NARS,NSKIP
             IF(IBRS>MBR)MBR=IBRS
             IF(LABRS>MLAB)MLAB=LABRS
             CALL SKIP3(NSKIP,EOF3)
             IF(EOF3)EXIT
          ENDDO
       ENDIF
C
 2     LAB=MLAB
       IAP(37)=LAB
       IF(ISW.LT.0.OR.IRS.EQ.0)THEN
         IBR=MBR+1
         IAP(30)=IBR
       ELSEIF( (IABS(ITP).LT.10.AND.IABS(ISW).EQ.2)
     *    .OR. (IPS.EQ.2.AND.ITP.EQ.3)
     *    .OR. (IPS.EQ.4.AND.ISW.EQ.2.AND.IABS(ITP).LT.10)
     *    .OR. (IPS.EQ.5.AND.MOD(ITP,10).EQ.2) )THEN
         IBR=IRS
         IAP(30)=IBR
       ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE FINDLB(IAP,RAP,IRS,NFPR,FOUND)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL FOUND,EOF3
C
      DIMENSION IAP(*),RAP(*)
C
C Locates restart point with label IRS and determines type.
C If the label can not be located on unit 3 then FOUND will be .FALSE.
C
       FOUND=.FALSE.
       REWIND 3
       ISW=IAP(10)
C
 1       READ(3,*,END=2)IBR,NTOTRS,ITP,LABRS,NFPR,ISWRS,NTPLRS,
     *   NARS,NSKIP
         IAP(27)=ITP
         IAP(30)=IBR
         IF(LABRS.EQ.IRS)THEN
           FOUND=.TRUE.
           IF(IABS(ISW).EQ.2)THEN
             IF(IABS(ITP).LT.10)THEN
               ITPST=IABS(ITP)
               IAP(28)=ITPST
             ELSE
               ITPST=IABS(ITP/10)
               IAP(28)=ITPST
             ENDIF
           ELSE
             ITPST=0
             IAP(28)=ITPST
           ENDIF
           BACKSPACE 3
           RETURN
         ELSE
           CALL SKIP3(NSKIP,EOF3)
           IF(EOF3)GOTO 2
         ENDIF
       GOTO 1
C
 2    I=1
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE READLB(IAP,RAP,U,PAR)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),U(*),PAR(*)
C
C Reads the restart data for algebraic problems.
C
       READ(3,*)IBRR,NTOTR,ITPR,LABR,NFPRR,ISWR,NTPLRS,NAR,
     *          NSKIPR,N1,N2,NPARR
       NDIM=NAR-1
       IF(NDIM.LE.IAP(1))THEN
         READ(3,*)T,(U(I),I=1,NDIM)
       ELSE
         READ(3,*)T,(U(I),I=1,IAP(1)),(DUM,I=1,NDIM-IAP(1))
       ENDIF
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data :',
     *          ' restart PAR(i) skipped for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE SKIP3(NSKIP,EOF3)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Skips the specified number of lines on unit 3.
C
      LOGICAL EOF3
C
       EOF3=.FALSE.
       DO I=1,NSKIP
         READ(3,*,END=2)
       ENDDO
       RETURN
 2     EOF3=.TRUE.
       RETURN
      END
C
C     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RINPR(IAP,NDIM1,NDX,UPS,VPS,DTM,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Computes the L2 inner product of UPS and VPS.
C (Using the first NDIM1 components only.)
C
      DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),DTM(*),THU(*)
C Local
      DIMENSION WI(IAP(6)+1)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
C
C Weights for the integration formulae :
       CALL WINT(NCOL+1,WI)
C
       S=0.d0
       DO J=1,NTST
         JP1=J+1
         SJ=0.d0
         DO I=1,NDIM1
           DO K=1,NCOL
             K1=(K-1)*NDIM+I
             SJ=SJ+WI(K)*THU(I)*UPS(K1,J)*VPS(K1,J)
           ENDDO
           SJ=SJ+WI(NCOL+1)*THU(I)*UPS(I,JP1)*VPS(I,JP1)
         ENDDO
         S=S+DTM(J)*SJ
       ENDDO
C
       RINPR=S
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION RNRMSQ(IAP,NDIM1,NDX,UPS,DTM,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION THU(*),IAP(*),UPS(*),DTM(*)
C
C Finds the norm-squared of UPS (first NDIM1 components are included only).
C
       RNRMSQ=RINPR(IAP,NDIM1,NDX,UPS,UPS,DTM,THU)
C
      RETURN
      END
C
C     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RINTG(IAP,NDX,IC,UPS,DTM)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Computes the integral of the IC'th component of UPS.
C
      DIMENSION IAP(*),UPS(NDX,*),DTM(*)
C Local
      DIMENSION WI(IAP(6)+1)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
C
C Weights for the integration formulae :
       CALL WINT(NCOL+1,WI)
C
       S=0.d0
       DO J=1,NTST
         JP1=J+1
         SJ=0.d0
           DO K=1,NCOL
             K1=(K-1)*NDIM+IC
             SJ=SJ+WI(K)*UPS(K1,J)
           ENDDO
           SJ=SJ+WI(NCOL+1)*UPS(IC,JP1)
         S=S+DTM(J)*SJ
       ENDDO
C
       RINTG=S
C
      RETURN
      END
C
C     ------ --------- -------- -----
      DOUBLE PRECISION FUNCTION RNRM2(IAP,NDX,IC,UPS,DTM)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Computes the L2-norm of the IC'th component of UPS.
C 
      DIMENSION IAP(*),UPS(NDX,*),DTM(*)
C Local
      DIMENSION WI(IAP(6)+1)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
C
C Weights for the integration formulae :
       CALL WINT(NCOL+1,WI)
C
       S=0.d0
       DO J=1,NTST
         JP1=J+1
         SJ=0.d0
           DO K=1,NCOL
             K1=(K-1)*NDIM+IC
             SJ=SJ+WI(K)*UPS(K1,J)**2
           ENDDO
           SJ=SJ+WI(NCOL+1)*UPS(IC,JP1)**2
         S=S+DTM(J)*SJ
       ENDDO
C
       RNRM2=DSQRT(S)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION RMXUPS(IAP,NDX,I,UPS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Computes the maximum of the I'th component of UPS.
C
      DIMENSION IAP(*),UPS(NDX,*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
C
       RMXUPS=UPS(I,1)
C
       DO J=1,NTST
         DO K=1,NCOL
           K1=(K-1)*NDIM+I
           IF(UPS(K1,J).GT.RMXUPS)RMXUPS=UPS(K1,J)
         ENDDO
       ENDDO
       IF(UPS(I,NTST+1).GT.RMXUPS)RMXUPS=UPS(I,NTST+1)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION RMNUPS(IAP,NDX,I,UPS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Computes the minimum of the I'th component of UPS.
C
      DIMENSION IAP(*),UPS(NDX,*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
C
       RMNUPS=UPS(I,1)
C
       DO J=1,NTST
         DO K=1,NCOL
           K1=(K-1)*NDIM+I
           IF(UPS(K1,J).LT.RMNUPS)RMNUPS=UPS(K1,J)
         ENDDO
       ENDDO
       IF(UPS(I,NTST+1).LT.RMNUPS)RMNUPS=UPS(I,NTST+1)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE SCALEB(IAP,ICP,NDX,DVPS,RLD,DTM,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Scales the vector (DVPS,RLD) so its norm becomes 1.
C
      DIMENSION IAP(*),ICP(*),DVPS(NDX,*),DTM(*),RLD(*),THL(*),THU(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       SS=RNRMSQ(IAP,NDIM,NDX,DVPS,DTM,THU)
C
       DO I=1,NFPR
         SS=SS+THL(ICP(I))*RLD(I)**2
       ENDDO
C
       SC=1.d0/DSQRT(SS)
C
       NROW=NDIM*NCOL
       DO J=1,NTST
         DO I=1,NROW
           DVPS(I,J)=DVPS(I,J)*SC
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         DVPS(I,NTST+1)=DVPS(I,NTST+1)*SC
       ENDDO
C
       DO I=1,NFPR
         RLD(I)=SC*RLD(I)
       ENDDO
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    General Boundary Value Problems
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE CNRLBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNT,
     * PVLI,THL,THU,IUZ,VUZ)
C
      INCLUDE 'auto.h'
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Controls the computation of solution branches.
C
      EXTERNAL FUNI,BCNI,ICNI,STPNT,FNLPBV,FNUZBV,PVLI,FNBPBV,FNSPBV
C
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),IUZ(*),THL(*),THU(*)
C Local
      DIMENSION RLCUR(NPARX),RLOLD(NPARX),RLDOT(NPARX) 
      COMPLEX*16 EV
      ALLOCATABLE EV(:),UPS(:,:),UOLDPS(:,:),UPOLDP(:,:)
      ALLOCATABLE DUPS(:,:),UDOTPS(:,:),FA(:,:),FC(:),TM(:),DTM(:)
      ALLOCATABLE P0(:,:),P1(:,:),UZR(:)
C
C INITIALIZE COMPUTATION OF BRANCH
C
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
C
       ALLOCATE(UPS(NDX,NTST+1),UOLDPS(NDX,NTST+1))
       ALLOCATE(UPOLDP(NDX,NTST+1),DUPS(NDX,NTST+1))
       ALLOCATE(UDOTPS(NDX,NTST+1),FA(NDX,NTST+1))
       ALLOCATE(FC(NBC+NINT+1),TM(NTST+1),DTM(NTST+1))
       ALLOCATE(P0(NDIM,NDIM),P1(NDIM,NDIM),UZR(NUZR),EV(NDIM))
C
       DS=RAP(1)
C
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
       IAP(31)=NITPS
       NTOT=0
       IAP(32)=NTOT
       ISTOP=0
       IAP(34)=ISTOP
C
      DO I=1,NPARX
        RLCUR(I)=0.d0
        RLOLD(I)=0.d0
        RLDOT(I)=0.d0
      ENDDO
C
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
C
      NODIR=0
       CALL RSPTBV(IAP,RAP,PAR,ICP,FUNI,STPNT,RDS,RLCUR,RLOLD,RLDOT,
     *  NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,TM,DTM,EV,NODIR,THL,THU)
       CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
C      
C     don't set global rotations here for homoclinics, but in autlib5.c
       IF(IPS.NE.9)CALL SETRTN(IAP(23),NTST,NDX,UPS,PAR)
C
       IF(NODIR.EQ.1 .AND. ISW.GT.0)THEN
         CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT,
     *    NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,0,P0,P1,THL,THU)
       ELSEIF(IRS.NE.0 .AND. ISW.LT.0)THEN
         CALL STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,RLDOT,
     *    NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,1,P0,P1,THL,THU)
       ENDIF
C
C Store plotting data for restart point :
C
       CALL STHD(IAP,RAP,PAR,ICP,THL,THU)
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
C
       CALL EXTRBV(IAP,RAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,
     *  UDOTPS)
C
       ITP=0
       IAP(27)=ITP
       GOTO 2
C
 1     ITP=0
       IAP(27)=ITP
       NTOT=IAP(32)
C
C Adapt the mesh to the solution.
C
       IF(IAD.NE.0)THEN
         IF(MOD(NTOT,IAD).EQ.0)
     *   CALL ADAPT(IAP,RAP,NTST,NCOL,NTST,NCOL,TM,DTM,NDX,UPS,UOLDPS)
       ENDIF
C
C Adapt the stepsize along the branch.
C
       IF(IADS.NE.0)THEN
         IF(MOD(NTOT,IADS).EQ.0)CALL ADPTDS(IAP,RAP,RDS)
       ENDIF
C
C Provide initial approximation and determine next point.
C
       CALL CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT,
     *   NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
2      CALL STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS,
     *   RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,
     *   TM,DTM,P0,P1,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)GOTO 3
C
C Check for user supplied parameter output parameter-values.
C
       IF(NUZR.GT.0)THEN
         DO IUZR=1,NUZR
           IAP(26)=IUZR 
           CALL LCSPBV(IAP,RAP,PAR,ICP,FNUZBV,FUNI,BCNI,ICNI,PVLI,
     *        UZR(IUZR),RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,
     *        UDOTPS,UPOLDP,FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
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
C
C Check for fold.
C
       IF(IABS(ILP).GT.0)THEN
         CALL LCSPBV(IAP,RAP,PAR,ICP,FNLPBV,FUNI,BCNI,ICNI,PVLI,RLP,
     *    RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,
     *    FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
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
C            *Stop at the first found fold
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
           ENDIF
         ENDIF
       ENDIF
C
C Check for branch point.
C
       IF(IABS(ISP).GE.2)THEN
         CALL LCSPBV(IAP,RAP,PAR,ICP,FNBPBV,FUNI,BCNI,ICNI,PVLI,BP1,
     *    RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,
     *    FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
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
C            *Stop at the first found BP
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
           ENDIF
         ENDIF
       ENDIF
C
C Check for period-doubling and torus bifurcation.
C
       IF(IABS(ISP).GT.0 .AND.
     *     (IPS.EQ.2.OR.IPS.EQ.7.OR.IPS.EQ.12) )THEN
         CALL LCSPBV(IAP,RAP,PAR,ICP,FNSPBV,FUNI,BCNI,ICNI,PVLI,SP1,
     *   RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,
     *   FA,FC,TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
         ISTOP=IAP(34)
         IF(ISTOP.EQ.1)GOTO 3
         ITP=IAP(27)
         IF(ITP.EQ.-1)THEN
           IF(ISP.GT.0)THEN
C            **Secondary periodic bifurcation: determine type
             CALL TPSPBV(IAP,RAP,PAR,ICP,EV)
             RLP=0.d0
             BP1=0.d0
             SP1=0.d0
           ELSE
C            *Stop at the first found SPB
             ISTOP=-1
             IAP(34)=ISTOP
             GOTO 3
           ENDIF
         ENDIF
       ENDIF
C
C Store plotting data.
C
 3     CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
       CALL STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,THL,THU)
C
       ISTOP=IAP(34)
       IF(ISTOP.EQ.0)THEN
         GOTO 1
       ENDIF
       DEALLOCATE(EV,UPS,UOLDPS,UPOLDP,DUPS,UDOTPS,FA,FC,TM,DTM,P0,P1)
       DEALLOCATE(UZR)
       RETURN
C
      END
C
C     ---------- ------
      SUBROUTINE CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Determines an initial approximation to the next solution point,
C by extrapolating from the two preceding points.
C The stepsize used in the preceding step has been stored in DSOLD.

      EXTERNAL FUNI
C
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*),UPOLDP(*),DTM(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       DSOLD=RAP(5)
C
C Compute rate of change (along branch) of PAR(ICP(1)) and U :
C
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
C        Rescale, to set the norm of (UDOTPS,RLDOT) equal to 1.
       CALL SCALEB(IAP,ICP,NDX,UDOTPS,RLDOT,DTM,THL,THU)
C
C Extrapolate to get initial approximation to next solution point.
C
       CALL EXTRBV(IAP,RAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,
     *  UDOTPS)
C
C Store time-derivative.
C
       CALL STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT,NDX,UPS,
     *  UOLDPS,UPOLDP)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE EXTRBV(IAP,RAP,FUNI,RDS,RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UDOTPS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Determines an initial approximation to the next solution by
C extrapolating from the two preceding points.
C The stepsize used in the preceding step has been stored in DSOLD.
C
      EXTERNAL FUNI
C
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
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
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UPOLDP)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Stores U-prime (derivative with respect to T) in UPOLDP.
C
      EXTERNAL FUNI
C
      DIMENSION UPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),IAP(*),RAP(*)
C Local
      ALLOCATABLE U(:),UOLD(:),F(:),DFDU(:),DFDP(:)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       DO I=1,NFPR
         PAR(ICP(I))=RLOLD(I)
       ENDDO
C
       ALLOCATE(U(NDIM),UOLD(NDIM),F(NDIM))
       ALLOCATE(DFDU(NDIM**2),DFDP(NDIM*NPARX))
C
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
C
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
C
       DO I=1,NFPR
         PAR(ICP(I))=RLCUR(I)
       ENDDO
C
      DEALLOCATE(U,UOLD,F,DFDU,DFDP)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS,
     * RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,
     * TM,DTM,P0,P1,THL,THU)
C
      USE SOLVEBV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Controls the solution of the nonlinear equations (by Newton's method)
C for the next solution (PAR(ICP(*)) , U) on a branch of solutions.
C
      EXTERNAL FUNI,BCNI,ICNI,PVLI
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*),UDOTPS(NDX,*)
      DIMENSION UPOLDP(NDX,*),DUPS(NDX,*),FA(NDX,*),FC(*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
      DIMENSION P0(*),P1(*)
      LOGICAL DONE
C
       NDIM=IAP(1)
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
C
       DSMIN=RAP(2)
       EPSL=RAP(11)
       EPSU=RAP(12)
C
 1     DSOLD=RDS
       RAP(5)=DSOLD
       NITPS=0
       IAP(31)=NITPS
C
C Write additional output on unit 9 if requested.
C
       CALL WRTBV9(IAP,RAP,PAR,ICP,RLCUR,NDX,UPS,TM,DTM,THL,THU)
C
C Generate the Jacobian matrix and the right hand side.
C
       DO 2 NIT1=1,ITNW
C
         NITPS=NIT1
         IAP(31)=NITPS
         NLLV=0
C
         IFST=0
         IF(NITPS.LE.NWTN)IFST=1
C
         CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDS,NLLV,
     *    RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC,
     *    P0,P1,THL,THU)
C
C Add Newton increments.
C
         DO I=1,NDIM
           UPS(I,NTST+1)=UPS(I,NTST+1)+FC(I)
         ENDDO
         DO I=1,NFPR
           RLCUR(I)=RLCUR(I)+FC(NDIM+I)
           PAR(ICP(I))=RLCUR(I)
         ENDDO
C
         DUMX=0.d0
         UMX=0.d0
         NROW=NDIM*NCOL
         DO J=1,NTST
           DO I=1,NROW
             ADU=DABS(FA(I,J))
             IF(ADU.GT.DUMX)DUMX=ADU
             AU=DABS(UPS(I,J))
             IF(AU.GT.UMX)UMX=AU
             UPS(I,J)=UPS(I,J)+FA(I,J)
           ENDDO
         ENDDO
C
         CALL WRTBV9(IAP,RAP,PAR,ICP,RLCUR,NDX,UPS,TM,DTM,THL,THU)
C
C Check whether user-supplied error tolerances have been met :
C
         DONE=.TRUE.
         RDRL=0.d0
         DO I=1,NFPR
           ADRL=DABS(FC(NDIM+I))/(1.d0+DABS(RLCUR(I)))
           IF(ADRL.GT.EPSL)DONE=.FALSE.
           IF(ADRL.GT.RDRL)RDRL=ADRL
         ENDDO
         RDUMX=DUMX/(1.d0+UMX)
         IF(DONE.AND.RDUMX.LT.EPSU)THEN
	   CALL PVLI(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
           IF(IID.GE.2)WRITE(9,*)  
           RETURN
         ENDIF
C
         IF(NITPS.EQ.1)THEN
           DELREF=20*DMAX1(RDRL,RDUMX)
         ELSE
           DELMAX=DMAX1(RDRL,RDUMX)
           IF(DELMAX.GT.DELREF)GOTO 3
         ENDIF
C
 2     CONTINUE
C
C Maximum number of iterations reached.
C
 3     IF(IADS.EQ.0)WRITE(9,101)IBR,NTOP
       IF(IADS.EQ.0)GOTO 13
C
C Reduce stepsize and try again.
C
       MXT=ITNW
       IAP(31)=MXT
       CALL ADPTDS(IAP,RAP,RDS)
       IF(DABS(RDS).LT.DSMIN)GOTO 12
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
C
C Minimum stepsize reached.
C
 12    WRITE(9,103)IBR,NTOP
 13    DO I=1,NFPR
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
C
 101   FORMAT(I4,I6,' NOTE:No convergence with fixed step size')
 102   FORMAT(I4,I6,' NOTE:Retrying step')
 103   FORMAT(I4,I6,' NOTE:No convergence using minimum step size')
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      Restart of Solution Branches ( Differential Equations )
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE RSPTBV(IAP,RAP,PAR,ICP,FUNI,STPNT,RDS,RLCUR,RLOLD,
     * RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,TM,DTM,EV,NODIR,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Restarts computation of a branch of solutions at point labelled IRS.
C The output written on unit 8 by a previous run is now expected as
C input on unit 3. The label IRS, where computation is to resume, must
C be specified in the user-supplied subroutine INIT.
C If IRS=0 then the starting point must be provided analytically in the
C user-supplied subroutine STPNT.
C
      EXTERNAL FUNI, STPNT
C
      COMPLEX*16 EV(*)
C
      DIMENSION IAP(*),RAP(*)
      DIMENSION UPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*),UDOTPS(NDX,*)
      DIMENSION TM(*),DTM(*),PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*)
C
      LOGICAL FOUND
      ALLOCATABLE UPSN(:,:),UPOLDN(:,:),UDOTPN(:,:),TMN(:),DTMN(:)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       NTST=IAP(5)
       NCOL=IAP(6)
       ISW=IAP(10)
       NDM=IAP(23)
       NFPR=IAP(29)
C
C Get restart data :
C
C     First take a peek at the file to see if ntst, ndim and
C     ncol are different then the values found in
C     the parameter file fort.2.
C
       IF(IRS.GT.0)THEN
         CALL FINDLB(IAP,RAP,IRS,NFPRS,FOUND)
         READ(3,*)IBR,NTOTRS,ITPRS,LAB,NFPRS,ISWRS,NTPLRS,NARS,NSKIP,
     *        NTSRS,NCOLRS,NPARR
         NTST3=NTSRS
         NCOL3=NCOLRS
         NDIM3=NARS-1
       ELSE
         NTST3=NTST
         NCOL3=NCOL
         NDIM3=NDIM
       ENDIF
       
C use the bigger of the size defined in fort.2 and the one defined in fort.8
       NTSTU=MAX(NTST,NTST3)
       NCOLU=MAX(NCOL,NCOL3)
       NDIMU=NDIM
       NDXLOC=NDIMU*NCOLU
       NTSTCU=(NTSTU+1)*NCOLU
C
C Autodetect special case when homoclinic branch switching is
C completed and the orbit's representation has to be
C changed.
C
       IF(IPS.EQ.9.AND.NDIM3.GT.(NDM*2).AND.NDIM3.GT.NDIM)THEN
         NTSTCU=(NTSTU+1)*(NDIM3/NDM)
         NDIMU=NDIM3
         NDXLOC=NDIMU*NCOLU
         IAP(1)=NDIMU
       ENDIF
       ALLOCATE(UPSN(NDXLOC,NTSTCU),UPOLDN(NDXLOC,NTSTCU))
       ALLOCATE(UDOTPN(NDXLOC,NTSTCU),TMN(NTSTCU),DTMN(NTSTCU))
C initialize arrays
       DO I=1,NTSTCU
         DO J=1,NDXLOC
           UPSN(J,I)=0.0d0
           UPOLDN(J,I)=0.0d0
           UDOTPN(J,I)=0.0d0
         ENDDO
       ENDDO
C
       CALL STPNT(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT,
     *  NDXLOC,UPSN,UDOTPN,UPOLDN,TMN,DTMN,NODIR,THL,THU)
       IAP(1)=NDIM
C
C Determine a suitable starting label and branch number.
C
       CALL NEWLAB(IAP,RAP)
C
       DO J=1,NTSRS
         DTMN(J)=TMN(J+1)-TMN(J)
       ENDDO
C
C Adapt mesh if necessary :
C
       IF( NTST.NE.NTSRS .OR. NCOL.NE.NCOLRS)THEN
         CALL ADAPT(IAP,RAP,NTSRS,NCOLRS,NTST,NCOL,TMN,DTMN,NDXLOC,
     *   UPSN,UDOTPN)
       ENDIF
C Copy from the temporary large arrays into the normal arrays.
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
C
C Set UOLDPS, RLOLD.
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
         RLOLD(I)=RLCUR(I)
       ENDDO
C
       NROW=NDIM*NCOL
       DO J=1,NTST+1
         DO I=1,NROW
           UOLDPS(I,J)=UPS(I,J)
         ENDDO
       ENDDO
C
C Store U-prime (derivative with respect to time or space variable).
C
       IF(NODIR.EQ.-1)THEN
C        ** Restart from a Hopf bifurcation.
         NODIR=0
         ISW=1
       ELSE
C        ** Restart from orbit.
          CALL STUPBV(IAP,RAP,PAR,ICP,FUNI,RLCUR,RLOLD,RLDOT,
     *    NDX,UPS,UOLDPS,UPOLDP)
        ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE READBV(IAP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPS,
     *      UDOTPS,TM,ITPRS,NDX)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),PAR(*),ICPRS(*)
      DIMENSION RLDOTRS(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*)
C Local
      DIMENSION TEMP(7)
      LOGICAL EOF3
C
       NDIM=IAP(1)
       READ(3,*)IBR,NTOT,ITPRS,LAB,NFPR,ISW,NTPL,NARS,NSKIP,
     * NTSRS,NCOLRS,NPARR
       IAP(30)=IBR
       IAP(37)=LAB
       NRSP1=NTSRS+1
C
       NDIMRS=NARS-1
       NSKIP1=(NDIMRS+1)/8 - NDIM/7
       NSKIP2=(NDIMRS+1)/9 - NDIM/8
       IF(NDIM.LE.NDIMRS)THEN
         NDIMRD=NDIM
       ELSE
         NDIMRD=NDIMRS
       ENDIF
C
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
C
       READ(3,*)(ICPRS(K),K=1,NFPR)
       READ(3,*)(RLDOTRS(K),K=1,NFPR)
C
C Read U-dot (derivative with respect to arclength).
C
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
C
C Read the parameter values.
C
       IF(NPARR.GT.NPARX)THEN
         NPARR=NPARX
         WRITE(6,100)NPARR
 100     FORMAT(' Warning : NPARX too small for restart data : ',/,
     *          ' PAR(i) set to zero, for i > ',I3)
       ENDIF
       READ(3,*)(PAR(I),I=1,NPARR)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNBV(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT,
     * NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      USE INTERFACES, ONLY:PDBLE
      USE HOMCONT, ONLY:PREHO
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C This subroutine locates and retrieves the information required to
C restart computation at the point with label IRS.
C This information is expected on unit 3.
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
C Local
      DIMENSION ICPRS(NPARX)
C
      LOGICAL FOUND,EOF3
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ISW=IAP(10)
       NFPR=IAP(29)
C
       CALL FINDLB(IAP,RAP,IRS,NFPRS,FOUND)
       CALL READBV(IAP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOT,UPS,
     *      UDOTPS,TM,ITPRS,NDX)
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
C Special case : Preprocess restart data in case of homoclinic
C continuation
C
       IF(IPS.EQ.9)THEN
         CALL PREHO(IAP,RAP,PAR,ICP,NDX,NTSRS,NDIMRD,NCOLRS,UPS,
     *         UDOTPS,TM,DTM)
C
C Special case : Preprocess restart data in case of branch switching
C at a period doubling bifurcation.
C
       ELSE IF((IPS.EQ.2.OR.IPS.EQ.7).AND.ISW.EQ.-1.AND.ITPRS.EQ.7) THEN
         CALL PDBLE(IAP,RAP,NDIM,NTSRS,NCOLRS,NDX,UPS,UDOTPS,TM,PAR)
         RETURN
       ENDIF
C
C Take care of the case where the free parameters have been changed at
C the restart point.
C
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
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STPNUB(IAP,RAP,PAR,ICP,NTSRS,NCOLRS,RLCUR,RLDOT,
     * NDX,UPS,UDOTPS,UPOLDP,TM,DTM,NODIR,THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates a starting point for the continuation of a branch of
C of solutions to general boundary value problems by calling the user
C supplied subroutine STPNT where an analytical solution is given.
C
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLDOT(*)
C Local
      ALLOCATABLE U(:)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
       ALLOCATE(U(NDIM))
C
C Generate the (initially uniform) mesh.
C
       CALL MSH(IAP,RAP,TM)
       DT=1.d0/(NTST*NCOL)
C
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
C
       NTSRS=NTST
       NCOLRS=NCOL
       IBR=1
       IAP(30)=IBR
       LAB=0
       IAP(37)=LAB
C
       DO I=1,NFPR
         RLCUR(I)=PAR(ICP(I))
       ENDDO
C
       NODIR=1
C
      DEALLOCATE(U)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE SETRTN(NDM,NTST,NDX,UPS,PAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Initialization for rotations
C
      POINTER NRTN(:)
      COMMON /BLRTN/ NRTN,IRTN
      DIMENSION UPS(NDX,*),PAR(*)
C
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
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE STDRBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RLCUR,RLOLD,
     * RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DTM,IPERP,
     * P0,P1,THL,THU)
C
      USE SOLVEBV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates a direction vector (UDOTPS,RLDOT) that is needed to start
C the computation of a branch when no direction vector is given.
C
      EXTERNAL FUNI,BCNI,ICNI
C
      DIMENSION IAP(*),RAP(*),UDOTPS(NDX,*),FA(NDX,*),FC(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
      DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UPOLDP(NDX,*),P0(*),P1(*)
C
C Generate the Jacobian matrix with zero direction vector.
C (Then the last row of the Jacobian will be zero)
C in case the starting direction is to be determined.
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       IID=IAP(18)
       NFPR=IAP(29)
C
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
C
        RDSZ=0.d0
        NLLV=1
        IFST=1
        CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV,
     *    RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC,
     *    P0,P1,THL,THU)
C
C Compute the starting direction.
C
         DO I=1,NDIM
           UDOTPS(I,NTST+1)=FC(I)
         ENDDO
         DO I=1,NFPR
           RLDOT(I)=FC(NDIM+I)
           PAR(ICP(I))=RLCUR(I)
         ENDDO
C
         DO J=1,NTST
           DO I=1,NROW
             UDOTPS(I,J)=FA(I,J)
           ENDDO
         ENDDO
C
C Scale the starting direction.
C
         CALL SCALEB(IAP,ICP,NDX,UDOTPS,RLDOT,DTM,THL,THU)
C
C Make sure that RLDOT(1) is positive (unless zero).
C
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
C
         IF(IID.GE.2)THEN
           WRITE(9,101)
            DO I=1,NFPR 
              WRITE(9,102)ICP(I),RLDOT(I)
            ENDDO
         ENDIF
C
 101     FORMAT(/,' Starting direction of the free parameter(s) : ')
 102     FORMAT(' PAR(',I3,') :',E11.3)
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  Detection and Location of Branch Points in Boundary Value Problems
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE LCSPBV(IAP,RAP,PAR,ICP,FNCS,FUNI,BCNI,ICNI,PVLI,Q,
     * RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,
     * TM,DTM,P0,P1,EV,THL,THU,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C This subroutine uses the Secant method to accurately locate folds
C branch points, and zero(es) of user parameter values.
C Such points are located as points on a solution branch where the
C EXTERNAL function FNCS changes sign.
C It involves calling the basic solution subroutines CONTBV and STEP
C with decreasing values of RDS (stepsize along branch).
C The point is assumed to have been found with sufficient accuracy if
C the ratio between RDS and the user supplied value of DS is less than
C the user-supplied tolerance EPSS.
C This subroutine is called from CNRLB, which controls the computation
C of branches of solutions to general boundary value problems.
C
      EXTERNAL FNCS,FUNI,BCNI,ICNI,PVLI
C
      COMPLEX*16 EV(*)
C
      LOGICAL CHNG
C
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),TM(*),DTM(*),FA(*),FC(*)
      DIMENSION UPS(*),UDOTPS(*),UOLDPS(*),UPOLDP(*),DUPS(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*),P0(*),P1(*)
C
       IID=IAP(18)
       ITMX=IAP(19)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       DS=RAP(1)
       DSMAX=RAP(3)
       DSOLD=RAP(5)
       EPSS=RAP(13)
C
C Check for zero.
C
       Q0=Q
       Q1=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,
     * RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,
     * TM,DTM,THL,THU,IUZ,VUZ)
C
       PQ=Q0*Q1
       IF(PQ.GE.0.d0 .OR. (.NOT. CHNG))THEN
         Q=Q1
         RETURN
       ENDIF
C
C Use the secant method for the first step:
C
       S0=0.d0
       S1=DSOLD
       NITSP1=0
       DQ=Q0-Q1
       RDS=Q1/DQ*(S1-S0)
 1     RDS=(1.d0+HMACH)*RDS
       S=S1+RDS
C
C Return if tolerance has been met :
C
       RRDS=DABS(RDS)/(1+DSQRT(DABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS) THEN
         ITP=-1
         IAP(27)=ITP
cxx???   Q=0.d0
         WRITE(9,102)RDS
         RETURN
       ENDIF
C
C If requested write additional output on unit 9 :
C
       IF(IID.GE.2)THEN
         WRITE(9,101)NITSP1,RDS
       ENDIF
C
       CALL CONTBV(IAP,RAP,PAR,ICP,FUNI,RDS,RLCUR,RLOLD,RLDOT,
     *  NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THL,THU)
       CALL STEPBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,PVLI,RDS,
     *  RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,
     *  FA,FC,TM,DTM,P0,P1,THL,THU)
       ISTOP=IAP(34)
       IF(ISTOP.NE.0)THEN
         Q=0.d0
         RETURN
       ENDIF
C
C Check for zero.
C
       Q=FNCS(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,
     *  RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,
     *  TM,DTM,THL,THU,IUZ,VUZ)
C
       NITSP1=NITSP1+1
       IF(NITSP1.LE.ITMX)THEN
C        Use Mueller's method with bracketing for subsequent steps
         CALL MUELLER(Q0,Q1,Q,S0,S1,S,RDS)
         GOTO 1
       ENDIF
C
       WRITE(9,103)IBR,NTOP+1
       Q=0.d0
 101   FORMAT(' ==> Location of special point :  Iteration ',I3,
     *  '  Step size = ',1PE13.5)
 102   FORMAT(' ==> Location of special point : ',
     *        ' Convergence.   Step size = ',1PE13.5)
 103    FORMAT(I4,I6,' NOTE:Possible special point')
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNLPBV
     *(IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
C
      USE SOLVEBV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C RETURNS A QUANTITY THAT CHANGES SIGN AT A LIMIT POINT (BVP)
C
      COMPLEX*16 EV(*)
C
      LOGICAL CHNG
C
      EXTERNAL FUNI,BCNI,ICNI
C
      DIMENSION IAP(*),RAP(*),PAR(*),ICP(*),UDOTPS(NDX,*),FA(NDX,*)
      DIMENSION FC(*)
      DIMENSION RLCUR(*),RLOLD(*),RLDOT(*),TM(*),DTM(*),THL(*),THU(*)
      DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UPOLDP(NDX,*),P0(*),P1(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       ISP=IAP(9)
       IID=IAP(18)
       NFPR=IAP(29)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
C Find the direction vector.
C
         NLLV=-1
         IFST=0
         RDSZ=0.d0
C
         CALL SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDSZ,NLLV,
     *    RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,FA,FC,
     *    P0,P1,THL,THU)
C
         DO I=1,NDIM
           UDOTPS(I,NTST+1)=FC(I)
         ENDDO
C
         DO I=1,NFPR
           RLDOT(I)=FC(NDIM+I)
         ENDDO
C
         NROW=NDIM*NCOL
         DO J=1,NTST
           DO I=1,NROW
             UDOTPS(I,J)=FA(I,J)
          ENDDO
         ENDDO
C
C Scale the direction vector.
C
         CALL SCALEB(IAP,ICP,NDX,UDOTPS,RLDOT,DTM,THL,THU)
         IF(IID.GE.2)THEN
           WRITE(9,101)IABS(IBR),NTOP+1,RLDOT(1)
         ENDIF
C
C Set the quantity to be returned.
C
         FNLPBV=RLDOT(1)
         CHNG=.TRUE.
         RAP(16)=FNLPBV
C
 101     FORMAT(I4,I6,9X,'Fold Function ',1PE14.5)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNBPBV
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      COMPLEX*16 EV(*)
C
      LOGICAL CHNG
C
      EXTERNAL FUNI,BCNI,ICNI
C
      DIMENSION IAP(*),RAP(*),P1(*)
C Local
      ALLOCATABLE IR(:),IC(:),PP(:)
      DOUBLE PRECISION U(1),F(1)
C
       NDIM=IAP(1)
       IID=IAP(18)
C
C Save the determinant of the reduced system.
C
      DET=RAP(14)
      DET0=DET
      IBR=IAP(30)
      NTOT=IAP(32)
      NTOP=MOD(NTOT-1,9999)+1
C
C Compute the determinant of P1.
C
      ALLOCATE(IR(NDIM),IC(NDIM),PP(NDIM**2))
      DO I=1,NDIM**2
        PP(I)=P1(I)
      ENDDO                               
      CALL GE(0,NDIM,NDIM,PP,0,1,U,1,F,IR,IC,DET)
      DEALLOCATE(IR,IC,PP)
      RAP(14)=DET
C
C Set the determinant of the normalized reduced system.
C
      IF(ABS(DET0)/HUGE(DET).LT.ABS(DET))THEN
        FNBPBV=DET0/DET
        CHNG=.TRUE.
      ELSE
        FNBPBV=0.d0
        CHNG=.FALSE.
      ENDIF
      RAP(18)=FNBPBV
C
      IF(IID.GE.2)WRITE(9,101)IABS(IBR),NTOP+1,FNBPBV
 101  FORMAT(I4,I6,9X,'BP   Function ',1PE14.5)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNSPBV
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV, RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
C
      USE FLOQUET
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
C This function returns a quantity that changes sign when a complex
C pair of eigenvalues of the linearized Poincare map moves in or out
C of the unit circle or when a real eigenvalues passes through -1.

      DOUBLE COMPLEX EV(*),ZTMP
      DIMENSION IAP(*),RAP(*),P0(*),P1(*)
C Local
      ALLOCATABLE WRK(:)
C
      LOGICAL CHNG
C
      EXTERNAL FUNI,BCNI,ICNI
C
       NDIM=IAP(1)
       ISP=IAP(9)
       ISW=IAP(10)
       IID=IAP(18)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
C Initialize.
C
       FNSPBV=0.d0
       RAP(19)=FNSPBV
       D=0.d0
       CHNG=.FALSE.
C
       IF(IID.GE.4)THEN
         CALL EVECS(NDIM,P0,P1)
       ENDIF
C
C  Compute the Floquet multipliers
      ALLOCATE(WRK(NDIM**2))
      CALL FLOWKM(NDIM, P0, P1, IID, WRK, EV)
      DEALLOCATE(WRK)
C
C Find the multiplier closest to z=1.
C
       AMIN=RLARGE
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
C
C Order the remaining Floquet multipliers by distance from |z|=1.
C
       IF(NDIM.GE.3)THEN
         DO I=2,NDIM-1
           AMIN=RLARGE
           DO J=I,NDIM
             AZM1= ABS(EV(J)) - 1.d0 
             AZM1=DABS(AZM1)
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
C
C Print error message if the Floquet multiplier at z=1 is inaccurate.
C (ISP is set to negative and detection of bifurations is discontinued)
C
       AMIN= ABS( EV(1) - 1.d0 )
       IF(AMIN.GT.5.0E-2 .AND. ISP.EQ.2) THEN
         IF(IID.GE.2)WRITE(9,101)IABS(IBR),NTOP+1
         DO I=1,NDIM
            WRITE(9,105)IABS(IBR),NTOP+1,I,EV(I)
         ENDDO
         NINS=0
         IAP(33)=NINS
         WRITE(9,104)IABS(IBR),NTOP+1,NINS
         ISP=-ISP
         IAP(9)=ISP
         RETURN
       ENDIF
C
C Restart automatic detection if the Floquet multiplier at z=1 is
C sufficiently accurate again.
C
       IF(ISP.LT.0)THEN
         IF(AMIN.LT.1.0E-2)THEN
           WRITE(9,102)IABS(IBR),NTOP+1
           ISP=-ISP
           IAP(9)=ISP
         ELSE
           DO I=1,NDIM
              WRITE(9,105)IABS(IBR),NTOP+1,I,EV(I)
           ENDDO
           RETURN
         ENDIF
       ENDIF
C
C Count the number of Floquet multipliers inside the unit circle.
C
C Set tolerance for deciding if a multiplier is outside |z=1|.
C Use, for example, tol=1d-3 for conservative systems.
       tol=1.d-5
C
       IF(NDIM.EQ.1) THEN
         D=0.d0
         FNSPBV=D
         RAP(19)=FNSPBV
       ELSE
         NINS1=1
         DO I=2,NDIM
           IF( ABS(EV(I)).LE.(1.d0+tol))NINS1=NINS1+1
         ENDDO
         IF(ISP.EQ.2) THEN
           IF(DIMAG(EV(2)).EQ.0.d0 .AND. DREAL(EV(2)).GT.0.d0)THEN
C            *Ignore if second multiplier is real positive
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
C
       NINS=NINS1
       IAP(33)=NINS
       IF( IID.GE.2 .AND. (ISP.EQ.1 .OR. ISP.EQ.2))THEN
          WRITE(9,103)IABS(IBR),NTOP+1,D
       ENDIF
C
C Print the Floquet multipliers.
C
       NINS=IAP(33)
       WRITE(9,104)IABS(IBR),NTOP+1,NINS
       DO I=1,NDIM
          WRITE(9,105)IABS(IBR),NTOP+1,I,EV(I),ABS(EV(I))
       ENDDO
C
 101   FORMAT(I4,I6,' NOTE:Multiplier inaccurate')
 102   FORMAT(I4,I6,' NOTE:Multiplier accurate again')
 103   FORMAT(I4,I6,9X,'SPB  Function ',1PE14.5)
 104   FORMAT(I4,I6,9X,'Multipliers:     Stable:',I4)
 105   FORMAT(I4,I6,9X,'Multiplier',I3,1X,1P2E14.5,
     *                 '  Abs. Val.',1P1E14.5)
C
      RETURN
      END
C
C     ------ --------- -------- ------
      DOUBLE PRECISION FUNCTION FNUZBV
     * (IAP,RAP,PAR,ICP,CHNG,FUNI,BCNI,ICNI,P0,P1,EV,RLCUR,RLOLD,RLDOT,
     * NDX,UPS,UOLDPS,UDOTPS,UPOLDP,FA,FC,DUPS,TM,DTM,THL,THU,IUZ,VUZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMPLEX*16 EV(*)
C
      LOGICAL CHNG
C
      EXTERNAL FUNI,BCNI,ICNI
C
      DIMENSION IAP(*),PAR(*),IUZ(*),VUZ(1)
C
       IID=IAP(18)
       IUZR=IAP(26)
       IBR=IAP(30)
       NTOT=IAP(32)
       NTOP=MOD(NTOT-1,9999)+1
C
       FNUZBV=PAR(IABS(IUZ(IUZR)))-VUZ(IUZR)
       CHNG=.TRUE.
C
       IF(IID.GE.3)WRITE(9,101)IABS(IBR),NTOP+1,IUZR,FNUZBV
 101   FORMAT(I4,I6,9X,'User Func.',I3,1X,1PE14.5)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE TPSPBV(IAP,RAP,PAR,ICP,EV)
C
C Determines type of secondary periodic bifurcation.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      COMPLEX*16 EV(*)
C
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*)
C
       NDIM=IAP(1)
C
       EPSS=RAP(13)
       ITPST=IAP(28)
C
C Find the eigenvalue closest to z=1.
C
       LOC=1
       AMIN=RLARGE
       DO I=1,NDIM
         AZM1= ABS( EV(I) - 1.d0 )
         IF(AZM1.LE.AMIN)THEN
           AMIN=AZM1
           LOC=I
         ENDIF
       ENDDO
C
C Find the eigenvalue closest to the unit circle
C (excluding the eigenvalue at z=1).
C
       LOC1=1
       AMIN=RLARGE
       DO I=1,NDIM
         IF(I.NE.LOC)THEN
           D= ABS(EV(I)) - 1.d0
           AD=DABS(D)
           IF(AD.LE.AMIN)THEN
             AMIN=AD
             LOC1=I
           ENDIF
         ENDIF
       ENDDO
C
      IF(DABS(DIMAG(EV(LOC1))).GT.DSQRT(EPSS))THEN
C       ** torus bifurcation
        ITP=8+10*ITPST
        IAP(27)=ITP
        PAR(12)=DASIN(DIMAG(EV(LOC1)))
      ELSE IF(DREAL(EV(LOC1)).LT.-.5d0)THEN
C       ** period doubling
        ITP=7+10*ITPST
        IAP(27)=ITP
      ELSE
C       ** something else...
        ITP=0
        IAP(27)=ITP
      ENDIF
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Output (Boundary Value Problems)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE STPLBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,
     * THL,THU)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Writes the bifurcation diagram on unit 7 (Differential Equations)
C (Also controls the writing of complete solutions on unit 8).
C Every line written contains, in order, the following:
C
C  IBR    : The label of the branch.
C  NTOT   : The index of the point on the branch.
C           (Points are numbered consecutively along a branch).
C           If IPS=2 or 3, then the sign of NTOT indicates stability :
C            - = stable , + = unstable, or unknown.
C  ITP    : An integer indicating the type of point :
C
C             4  (  )  :   Output point (Every NPR steps along branch).
C            -4  (UZ)  :   Output point (Zero of user function).
C             5  (LP)  :   Fold (fold).
C             6  (BP)  :   Branch point.
C             7  (PD)  :   Period doubling bifurcation.
C             8  (TR)  :   Bifurcation to an invariant torus.
C             9  (EP)  :   End point of branch, normal termination.
C            -9  (MX)  :   End point of branch, abnormal termination.
C
C  LAB        : The label of a special point.
C  PAR(ICP(1)): The principal parameter.
C  A          : The L2-norm of the solution vector, or other measure of
C               the solution (see the user-supplied parameter IPLT).
C  MAX U(*)   : The maxima of the first few solution components.
C  PAR(ICP(*)): Further free parameters (if any).
C
      DIMENSION PAR(*),ICP(*),IAP(*),RAP(*),TM(*),DTM(*),UPS(*),THU(*)
C Local
      DIMENSION UMX(7)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       IRS=IAP(3)
       ISW=IAP(10)
       IPLT=IAP(11)
       NMX=IAP(14)
       NPR=IAP(16)
       NDM=IAP(23)
       ITP=IAP(27)
       ITPST=IAP(28)
       IBR=IAP(30)
C
       RL0=RAP(6)
       RL1=RAP(7)
       A0=RAP(8)
       A1=RAP(9)
C
       NTOT=IAP(32)
       NTOT=NTOT+1
       IAP(32)=NTOT
C
C ITP is set to 4 every NPR steps along a branch of solns and the entire
C solution is written on unit 8.
C
       IF(NPR.NE.0)THEN
         IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
         IAP(27)=ITP
       ENDIF
C
C Check whether limits of the bifurcation diagram have been reached :
C
       IAB=IABS(IPLT)
       IF(IAB.EQ.0.OR.IAB.GT.3*NDM)
     *                 AMP=DSQRT(RNRMSQ(IAP,NDM,NDX,UPS,DTM,THU))
       IF(IPLT.GT.0.AND.IAB.LE.NDM)AMP=RMXUPS(IAP,NDX,IAB,UPS)
       IF(IPLT.GT.NDM.AND.IAB.LE.2*NDM)
     *                        AMP=RINTG(IAP,NDX,IAB-NDM,UPS,DTM)
       IF(IPLT.GT.2*NDM.AND.IAB.LE.3*NDM)
     *                        AMP=RNRM2(IAP,NDX,IAB-2*NDM,UPS,DTM)
       IF(IPLT.LT.0.AND.IAB.LE.NDM)AMP=RMNUPS(IAP,NDX,IAB,UPS)
C
       RAP(10)=AMP
C
       ISTOP=IAP(34)
       IF(ISTOP.EQ.1)THEN
C        ** Maximum number of iterations reached somewhere.
         ITP=-9-10*ITPST
         IAP(27)=ITP
       ELSEIF(ISTOP.EQ.-1)THEN
C        ** UZR endpoint
         ITP=9+10*ITPST
         IAP(27)=ITP
       ELSE
         IF(PAR(ICP(1)).LT.RL0.OR.PAR(ICP(1)).GT.RL1
     *   .OR. AMP.LT.A0.OR.AMP.GT.A1 .OR. NTOT.GE.NMX)THEN
           ISTOP=1
           IAP(34)=ISTOP
           ITP=9+10*ITPST
           IAP(27)=ITP
         ENDIF
       ENDIF
C
C All special points receive label:
C
       LABW=0
       IF(MOD(ITP,10).NE.0) THEN
         LAB=IAP(37)
         LAB=LAB+1
         IAP(37)=LAB
         LABW=LAB
       ENDIF
C
C Compute maxima of solution components.
C
       N2=NDM
       IF(N2.GT.7)N2=7
       DO I=1,N2
         ITMP=I
         UMX(I)=RMXUPS(IAP,NDX,ITMP,UPS)
       ENDDO
C
C Branch number is negative for periodic solutions
C
       IF(IPS.EQ.2)THEN
         IBRS=-IBR
       ELSE
         IBRS=IBR
       ENDIF
C
C Determine stability, and write output on units 7 and 8.
C
       NTOTS=NTOT
       IF(IABS(ISW).LE.1 .AND. (IPS.EQ.2.OR.IPS.EQ.7))THEN
         NINS=IAP(33)
         IF(NINS.EQ.NDIM)NTOTS=-NTOT
       ENDIF
       CALL WRLINE(IAP,PAR,ICP,ICP(NPARX+1),IBRS,NTOTS,LABW,AMP,UMX)
C
C Write plotting and restart data on unit 8.
C
       IF(MOD(ITP,10).NE.0)
     * CALL WRTBV8(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE WRTBV8(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Writes plotting and restart data on unit 8, viz.:
C (1) data identifying the corresponding point on unit 7,
C (2) the complete solution,
C (3) the direction of the branch.
C
C Specifically the following is written:
C
C  IBR   : The index of the branch.
C  NTOT  : The index of the point.
C  ITP   : The type of point (see STPLBV above).
C  LAB   : The label of the point.
C  NFPR : The number of free parameters used in the computation.
C  ISW   : The value of ISW used in the computation.
C  NTPL  : The number of points in the time interval [0,1] for which
C          solution values are written.
C  NAR   : The number of values written per point.
C          (NAR=NDIM+1, since T and U(i), i=1,..,NDIM are written).
C  NROWPR: The number of lines printed following the identifying line
C          and before the next data set or the end of the file.
C          (Used for quickly skipping a data set when searching).
C  NTST  : The number of time intervals used in the discretization.
C  NCOL  : The number of collocation points used.
C  NPARX : The dimension of the array PAR.
C
C  Following the above described identifying line there are NTPL lines
C containing :
C     T , U-1(T) , U-2(T) , ... , U-NDIM(T),
C where NDIM is the dimension of the system of differential equations.
C
C Following this is a line containing the indices of the free parameters
C    ICP(I),I=1,NFPR,
C
C followed by a line containing the values
C    RL-dot(i) , i=1,NFPR,
C
C and following this are NTPL lines each containing
C    U-dot-1(T), U-dot-2(T), ... , U-dot-NDIM(T).
C
C Finally the parameter values PAR(i) , i=1,NPARX, are written.
C
C  Above, RL-dot(.) and U-dot(.) specify the direction of the branch.
C
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLDOT(*)
Cxxx====================================================================
Cxxx Test problem: compute the error
      err(x,t)=x - 2*DATAN(1.d0)*PAR(2)*DSIN(4*DATAN(1.d0)*t)
Cxxx====================================================================
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       ISW=IAP(10)
       ITP=IAP(27)
       NFPR=IAP(29)
       IBR=IAP(30)
       NTOT=IAP(32)
       LAB=IAP(37)
C
C Write information identifying the solution :
C
       NTPL=NCOL*NTST+1
       NAR=NDIM+1
       NRD=2+NDIM/7+(NDIM-1)/7
       NROWPR=NRD*(NCOL*NTST+1) + (NFPR-1)/7+1 + (NPARX-1)/7+1
     *                          + (NFPR-1)/20+1
C
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,
     *             NTST,NCOL,NPARX
C
C Write the entire solution on unit 8 :
C
Cxxx====================================================================
Cxxx Test problem
       eg=0.d0
       em=0.d0
Cxxx====================================================================
       DO J=1,NTST
         RN=1.d0/NCOL
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           T=TM(J)+(I-1)*RN*DTM(J)
           WRITE(8,102)T,(UPS(K,J),K=K1,K2)
Cxxx====================================================================
Cxxx Test problem
           er = err(ups(k1,j),T)
           if(dabs(er).gt.eg)eg=dabs(er)
           if(i.eq.1 .and. dabs(er).gt.em)em=dabs(er)
Cxxx====================================================================
         ENDDO
       ENDDO
Cxxx====================================================================
Cxxx Test problem
C Write global error and mesh error
Cxxx       write(10,100)ncol,ntst,eg,em
Cxxx 100   FORMAT(4X,I2,I4,1P7D11.3)
Cxxx====================================================================
       WRITE(8,102)TM(NTST+1),(UPS(I,NTST+1),I=1,NDIM)
C
C Write the free parameter indices:
C
       WRITE(8,103)(ICP(I),I=1,NFPR)
C
C Write the direction of the branch:
C
       WRITE(8,102)(RLDOT(I),I=1,NFPR)
       DO J=1,NTST
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           WRITE(8,102)(UDOTPS(K,J),K=K1,K2)
         ENDDO
       ENDDO
       WRITE(8,102)(UDOTPS(K,NTST+1),K=1,NDIM)
C
C Write the parameter values.
C
       WRITE(8,102)(PAR(I),I=1,NPARX)
C
 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,1P7E19.10)
 103   FORMAT(20I5)
C
      CALL FLUSH(8)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE WRTBV9(IAP,RAP,PAR,ICP,RLCUR,NDX,UPS,TM,DTM,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Writes additional output on unit 9.
C
      DIMENSION IAP(*),RAP(*)
      DIMENSION DTM(*),UPS(NDX,*),TM(*),PAR(*),ICP(*),RLCUR(*),THU(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       IPLT=IAP(11)
       IID=IAP(18)
       NDM=IAP(23)
       NFPR=IAP(29)
       IBR=IAP(30)
       NITPS=IAP(31)
       NTOT=IAP(32)
       DS=RAP(1)
C
       IAB=IABS(IPLT)
       IF(IAB.EQ.0.OR.IAB.GT.NDIM)
     * AMP=DSQRT(RNRMSQ(IAP,NDM,NDX,UPS,DTM,THU))
       IF(IPLT.GT.0.AND.IAB.LE.NDIM)AMP=RMXUPS(IAP,NDX,IAB,UPS)
       IF(IPLT.LT.0.AND.IAB.LE.NDIM)AMP=RMNUPS(IAP,NDX,IAB,UPS)
       RAP(10)=AMP
       IF(IID.GE.2)THEN
         IF(NFPR.LE.5)THEN
           NFPRP=NFPR
         ELSE
           NFPRP=5
         ENDIF
         IF(NITPS.EQ.0)CALL WRBAR("=",47)
         IF(NITPS.EQ.0 .OR. IID.GE.3)THEN
            WRITE(9,102)
         ENDIF
         MTOT=MOD(NTOT-1,9999)+1
         WRITE(9,103)IBR,MTOT+1,NITPS,RLCUR(1),AMP
       ENDIF
C
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
C
 102   FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
 103   FORMAT(I4,I6,I4,5X,1P6E14.5)
 104   FORMAT(' UPS :')
 105   FORMAT(1X,1P7E14.5)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE PVLSAE(IAP,RAP,U,PAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),RAP(*),U(*),PAR(*)
C
        CALL SETPAE(IAP,RAP)
        NDM=IAP(23)
        CALL PVLS(NDM,U,PAR)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE PVLSBV(IAP,RAP,ICP,DTM,NDX,UPS,NDIM,P0,P1,PAR)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),RAP(*),ICP(*),DTM(*),UPS(NDX,*),PAR(*)
      DIMENSION P0(NDIM,*),P1(NDIM,*)
C
        CALL SETPBV(IAP,RAP,DTM)
        NDM=IAP(23)
        CALL PVLS(NDM,UPS,PAR)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE EVECS(NDIM,P0,P1)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION P0(NDIM,*),P1(NDIM,*)
C
C Local
      ALLOCATABLE Q0(:,:), Q1(:,:), P(:,:), Z(:,:), WR(:), WI(:)
      ALLOCATABLE IR(:), IC(:), IV1(:), FV1(:)
C
      ALLOCATE(Q0(NDIM,NDIM), Q1(NDIM,NDIM), P(NDIM,NDIM))
      ALLOCATE(Z(NDIM,NDIM), WR(NDIM), WI(NDIM))
      ALLOCATE(IR(NDIM), IC(NDIM))
      ALLOCATE(IV1(NDIM), FV1(NDIM))
C
        DO I=1,NDIM
          DO J=1,NDIM
             Q0(I,J)=-P0(I,J)
             Q1(I,J)= P1(I,J)
          ENDDO
        ENDDO
C
        CALL GE(0,NDIM,NDIM,Q1,NDIM,NDIM,P,NDIM,Q0,IR,IC,DET)
        CALL RG(NDIM,NDIM,P,WR,WI,1,Z,IV1,FV1,IERR)
C
        WRITE(9,100)
        WRITE(9,101)
        DO I=1,NDIM
          WRITE(9,102)WR(I),WI(I),(Z(I,J),J=1,NDIM)
        ENDDO
        WRITE(9,101)
 100    FORMAT(" Multipliers + eigenvectors obtained from - P0^-1 P1 :")
cxx
        write(9,112)WR(1)*WR(2)
112     format(" Product = ",1PE16.7)       
cxx
 101    FORMAT(" ")
 102    FORMAT(1P2E14.5," | ",1P8E14.5)
C
      DEALLOCATE(Q0,Q1,P,Z,WR,WI,IR,IC,IV1,FV1)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE SETPAE(IAP,RAP)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:)
      COMMON /BLPV/ DTV,RAV,IAV
      TARGET IAP(NIAP),RAP(NRAP)
C
      IAV=>IAP
      RAV=>RAP
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE SETPBV(IAP,RAP,DTM)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:)
      COMMON /BLPV/ DTV,RAV,IAV
      TARGET IAP(NIAP),RAP(NRAP),DTM(IAP(5)+1)
C
      IAV=>IAP
      RAV=>RAP
      DTV=>DTM
C
      RETURN
      END
C
C     ------ --------- -------- ----
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:)
      COMMON /BLPV/ DTV,RAV,IAV
      DIMENSION UPS(IAV(1)*IAV(6),*)
      CHARACTER*3 CODE
C
        IPS=IAV(2)
        NX=IAV(1)*IAV(6)
C
        IF( IABS(IPS).LE.1 .OR. IPS.EQ.5)THEN
          IF(CODE.EQ.'NRM'.OR.CODE.EQ.'nrm')THEN
            GETP=ABS(UPS(IC,1))    
          ELSEIF(CODE.EQ.'INT'.OR.CODE.EQ.'int')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'MAX'.OR.CODE.EQ.'max')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'MIN'.OR.CODE.EQ.'min')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'BV0'.OR.CODE.EQ.'bv0')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'BV1'.OR.CODE.EQ.'bv1')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'STP'.OR.CODE.EQ.'stp')THEN
            GETP=RAV(5)
          ELSEIF(CODE.EQ.'FLD'.OR.CODE.EQ.'fld')THEN
            GETP=RAV(16)
          ELSEIF(CODE.EQ.'HBF'.OR.CODE.EQ.'hbf')THEN
            GETP=RAV(17)
          ELSEIF(CODE.EQ.'BIF'.OR.CODE.EQ.'bif')THEN
            GETP=RAV(14)
          ELSEIF(CODE.EQ.'SPB'.OR.CODE.EQ.'spb')THEN
            GETP=0.
          ELSEIF(CODE.EQ.'STA'.OR.CODE.EQ.'sta')THEN
            GETP=IAV(33)
          ENDIF
        ELSE
          IF(CODE.EQ.'NRM'.OR.CODE.EQ.'nrm')THEN
            GETP=RNRM2(IAV,NX,IC,UPS,DTV)    
          ELSEIF(CODE.EQ.'INT'.OR.CODE.EQ.'int')THEN
            GETP=RINTG(IAV,NX,IC,UPS,DTV)
          ELSEIF(CODE.EQ.'MAX'.OR.CODE.EQ.'max')THEN
            GETP=RMXUPS(IAV,NX,IC,UPS)
          ELSEIF(CODE.EQ.'MIN'.OR.CODE.EQ.'min')THEN
            GETP=RMNUPS(IAV,NX,IC,UPS)
          ELSEIF(CODE.EQ.'BV0'.OR.CODE.EQ.'bv0')THEN
            GETP=UPS(IC,1)
          ELSEIF(CODE.EQ.'BV1'.OR.CODE.EQ.'bv1')THEN
            GETP=UPS(IC,IAV(5)+1)
          ELSEIF(CODE.EQ.'STP'.OR.CODE.EQ.'stp')THEN
            GETP=RAV(5)
          ELSEIF(CODE.EQ.'FLD'.OR.CODE.EQ.'fld')THEN
            GETP=RAV(16)
          ELSEIF(CODE.EQ.'HBF'.OR.CODE.EQ.'hbf')THEN
            GETP=0.
          ELSEIF(CODE.EQ.'BIF'.OR.CODE.EQ.'bif')THEN
            GETP=RAV(18)
          ELSEIF(CODE.EQ.'SPB'.OR.CODE.EQ.'spb')THEN
            GETP=RAV(19)
          ELSEIF(CODE.EQ.'STA'.OR.CODE.EQ.'sta')THEN
            GETP=IAV(33)
          ENDIF
        ENDIF
C
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C          System Dependent Subroutines for Timing AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE AUTIM0(T)
C
C$    USE OMP_LIB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL etime
      REAL timaray(2)
C
C Set initial time for measuring CPU time used.
C
      T=etime(timaray)
C$    T=omp_get_wtime()
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE AUTIM1(T)
C
C$    USE OMP_LIB
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL etime
      REAL timaray(2)
C
C Set final time for measuring CPU time used.
C
      T=etime(timaray)
C$    T=omp_get_wtime()
C
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
