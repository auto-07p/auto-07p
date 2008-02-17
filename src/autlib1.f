C     ------- ----
      PROGRAM AUTO
C
      USE IO
      USE SUPPORT
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
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
         CALL FINDLB(IAP,IRS,NFPR,FOUND)
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
      CALL CLEANUP()
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
C
C     ---------- ----
      SUBROUTINE AUTOI(IAP,RAP,PAR)
C
      USE INTERFACES
      USE AUTO_CONSTANTS
      USE AE
      USE BVP
      USE HOMCONT, ONLY:FNHO,BCHO,ICHO,PVLSHO,STPNHO
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER IAP(*)
      DOUBLE PRECISION RAP(*),PAR(*)
C
      ITP=IAP(27)
      NFPR=IAP(29)
C
      IF(IAP(38)==0)THEN
        CALL INIT1(IAP,RAP,ICP,PAR)
        CALL CHDIM(IAP)
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
           CALL AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FUNI,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF(IPS.EQ.11 .AND. ABS(ISW).LE.1 ) THEN
C        ** Waves : Spatially homogeneous solutions,
         IF(IRS.EQ.0) THEN
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNWS,STPNUS,THL,THU,IUZ,VUZ)
         ELSE
           CALL AUTOAE(IAP,RAP,PAR,ICP,FNWS,STPNAE,THL,THU,IUZ,VUZ)
         ENDIF
C
       ELSE IF((IPS.EQ.-1) .AND. ABS(ISW).LE.1 ) THEN
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
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).LE.1 ) THEN
C        ** Periodic solutions
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPS,BCPS,ICPS,STPNPS,
     *     PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.12 .AND. ABS(ISW).LE.1 ) THEN
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
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)<=1) THEN
C        ** Boundary value problems. (4)
C        ** Boundary value problems with Floquet multipliers. (7)
          CALL AUTOBV(IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,STPNPS,
     *         PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.9 .AND. ABS(ISW).LE.1) THEN
C        ** Homoclinic bifurcation analysis.
          CALL AUTOBV(IAP,RAP,PAR,ICP,FNHO,BCHO,ICHO,STPNHO,
     *         PVLSHO,THL,THU,IUZ,VUZ)
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
       ELSE IF(IPS.EQ.15.AND.ABS(ISW).EQ.1) THEN
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
 2     IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 .AND. (ITP.EQ.2) )
     * THEN
C        ** Fold continuation (algebraic problems).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNLP,STPNLP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).EQ.2 
     *         .AND. ( (ABS(ITP)/10).EQ.2 ) )
     * THEN
C        ** Fold continuation (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNLP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 .AND. (ITP.EQ.1) )
     * THEN
C        ** BP cont (algebraic problems) (by F. Dercole).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNBP,STPNBP,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.LE.1 .AND. ABS(ISW).GE.2 
     *         .AND. ( (ABS(ITP)/10).EQ.1 ) )
     * THEN
C        ** BP cont (algebraic problems, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNBP,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.0.OR.IPS.EQ.1).AND.ABS(ISW).EQ.2.AND.ITP.EQ.3 )
     * THEN
C        ** Hopf bifurcation continuation (ODE).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHB,STPNHB,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.0.OR.IPS.EQ.1).AND.ABS(ISW).EQ.2.AND.
     * (ABS(ITP)/10).EQ.3 ) THEN
C        ** Hopf bifurcation continuation (ODE, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHB,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.11.AND.ABS(ISW).EQ.2.AND.ITP.EQ.3 )
     * THEN
C        ** Hopf bifurcation continuation (Waves).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHW,STPNHW,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.11.AND.ABS(ISW).EQ.2.AND.
     * (ABS(ITP)/10).EQ.3 ) THEN
C        ** Hopf bifurcation continuation (Waves, restart).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHW,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.-1 .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.3 ) THEN
C        ** Hopf bifurcation continuation (Maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHD,STPNHD,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.-1 .AND. ABS(ISW).EQ.2 .AND.(ABS(ITP)/10).EQ.3)
     * THEN
C        ** Hopf bifurcation continuation (Maps).
         CALL AUTOAE(IAP,RAP,PAR,ICP,FNHD,STPNAE,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS==2 .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN 
C        ** Fold continuation (Periodic solutions, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPL,BCPL,ICPL,STPNPL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS==2 .AND. ABS(ISW)==2 .AND. (ABS(ITP)/10)==5 )
     * THEN
C        ** Fold continuation (Periodic solutions, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPL,BCPL,ICPL,STPNBV,
     *   PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS==2 .AND. ABS(ISW)>=2 .AND. 
     *         (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
C        ** BP cont (Periodic sol., start and restart) (by F. Dercole).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPBP,BCPBP,ICPBP,STPNPBP,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.2 .OR. IPS.EQ.7)
     *      .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.7 ) THEN
C        ** Continuation of period doubling bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPD,BCPD,ICPD,STPNPD,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS.EQ.2 .OR. IPS .EQ.7)
     *      .AND. ABS(ISW).EQ.2 .AND. (ABS(ITP)/10).EQ.7)
     * THEN
C        ** Continuation of period doubling bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNPD,BCPD,ICPD,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.2 .AND. ITP.EQ.8 ) THEN
C        ** Continuation of torus bifurcations (start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNTR,BCTR,ICTR,STPNTR,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF(IPS.EQ.2 .AND. ABS(ISW).EQ.2 .AND. (ABS(ITP)/10).EQ.8)
     * THEN
C        ** Continuation of torus bifurcations (restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNTR,BCTR,ICTR,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. ITP==5 ) THEN
C        ** Continuation of folds (BVP, start).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBL,BCBL,ICBL,STPNBL,
     *      PVLSBV,THL,THU,IUZ,VUZ)
C
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)==2 .AND. 
     *         (ABS(ITP)/10)==5 ) THEN
C        ** Continuation of folds (BVP, restart).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBL,BCBL,ICBL,STPNBV,
     *      PVLSBV,THL,THU,IUZ,VUZ)
       ELSE IF((IPS==4.OR.IPS==7) .AND. ABS(ISW)>=2 .AND.
     *          (ITP==6.OR.(ABS(ITP)/10)==6) ) THEN
C        ** BP cont (BVP, start and restart) (by F. Dercole).
         CALL AUTOBV(IAP,RAP,PAR,ICP,FNBBP,BCBBP,ICBBP,STPNBBP,
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
      INTEGER IBR,I,IUZR,ITHU,NBIF,NFPR,NDM,NNT0,NBC0
      INTEGER NINS,NIT,LAB,NTOT,ITP,IPOS,ISTOP,ITPST
      DOUBLE PRECISION AMP,BIFF,DET,DSOLD,SPBF,TIVP,HBFF,FLDF
C
      DO I=1,NPARX
        ICP(I)=I
        ICP(NPARX+I)=0
        PAR(I)=0.d0
        PAR(NPARX+I)=0.d0
      ENDDO
C
      READ(2,*,END=5) NDIM,IPS,IRS,ILP
C
C     we allocate THU (a pointer to the THU array in the
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
      DSMIN=ABS(DSMIN)
      DSMAX=ABS(DSMAX)
      READ(2,*) NTHL
      IF(NTHL.GT.0)THEN
        ALLOCATE(ITHL(NTHL),VTHL(NTHL))
        DO I=1,NTHL
          READ(2,*)ITHL(I),VTHL(I)
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
C     ---------- -------
      SUBROUTINE CLEANUP()
C
C     Deallocate some globally allocated arrays.
C
      USE AUTO_CONSTANTS

      IMPLICIT NONE

      DEALLOCATE(THU,IUZ,VUZ,THL)
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
C     ---------- -----
      SUBROUTINE INIT1(IAP,RAP,ICP,PAR)
C
      USE HOMCONT, ONLY:INHO
      USE AUTO_CONSTANTS, ONLY:ITHL,VTHL,THL,NTHL
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
             JC=ABS(IC)-20        
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
         ELSE IF( (ABS(ITP)/10)==5 .AND. IPS==2 )THEN
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
       IF(NTHL>0)THEN
         DEALLOCATE(ITHL,VTHL)
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
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
