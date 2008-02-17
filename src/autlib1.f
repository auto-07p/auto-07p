C     ------- ----
      PROGRAM AUTO
C
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
        IF(ABS(B).LE.RSMALL)THEN
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
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Output (Algebraic Problems)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE STHD(IAP,RAP,ICP)
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
      DIMENSION ICP(*),IAP(*),RAP(*)
       CHARACTER (LEN=*), PARAMETER :: D3 = "('   0',3(A8,ES11.4))"
       CHARACTER (LEN=*), PARAMETER :: I4 = "('   0',4(A8,I4))"
       CHARACTER (LEN=*), PARAMETER :: I5 = "('   0',3(A8,I4),2(A7,I4))"
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
      SUBROUTINE HEADNG(IAP,ICP,IUNIT,N1,N2)
C
      IMPLICIT NONE
C
C Prints headings above columns on unit 6, 7, and 9.
C N1 = number of parameters to print (maximum: 7 for screen output)
C N2 = number of (max) variables to print (maximum: max(0,7-N1,7))
C
      INTEGER IAP(*),ICP(*),IUNIT,N1,N2
C Local
      INTEGER I,J,IPS,IPLT,NDM
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
             IF(ICP(I)==11.AND.IPS>0.AND.IPS/=4.AND.IPS/=7)THEN
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
      SUBROUTINE WRLINE(IAP,PAR,ICU,IBR,NTOT,LAB,VAXIS,U)
C
      IMPLICIT NONE
C
C Write one line of output on unit 6 and 7.
C
      INTEGER IAP(*),ICU(*),IBR,NTOT,LAB
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
       IF(ABS(NTOT).EQ.1)CALL HEADNG(IAP,ICU,6,N1,N2)
       IF(ABS(NTOT).EQ.1)CALL HEADNG(IAP,ICU,7,NICP,N2)
       CALL HEADNG(IAP,ICU,9,N1,N2)
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
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Mesh and Weight Generation
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ---
      SUBROUTINE MSH(NTST,TM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates a uniform mesh on [0,1].
C
      DIMENSION TM(*)
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
       WRITE(9,101)ABS(IBR),NTOP,NIT
       WRITE(9,102)ABS(IBR),NTOP,RDS
 101   FORMAT(/,I4,I6,8X,' Iterations   : ',I3)
 102   FORMAT(I4,I6,8X,' Next Step    : ',ES13.5)
C
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE ADAPT(IAP,NOLD,NCOLD,NNEW,NCNEW,TM,DTM,NDX,UPS,VPS)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Adapts the distribution of the mesh points so that the increase of the
C monotone function EQDF becomes approximately equidistributed over the
C intervals. The functions UPS and VPS are interpolated on new mesh.
C
      DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),TM(*),DTM(*)
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
       IF(IPS.EQ.2 .AND. ABS(ISW).LE.1) THEN
         IPER=1
       ELSE
         IPER=0
       ENDIF
C
C Generate the new mesh :
C
       CALL NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TM,DTM,NNEW,TINT,IPER)
C
C Replace UPS by its interpolant on the new mesh :
C
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,UPS,NNEWP1,NCNEW,
     *  TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UPS(I,J)=UINT(I,J)
         ENDDO
       ENDDO
C
C Replace VPS by its interpolant on the new mesh :
C
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,VPS,NNEWP1,NCNEW,
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
      SUBROUTINE INTERP(NDIM,N,NC,TM,NDX,UPS,N1,NC1,TM1,UPS1,
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
         CALL ORDR(N,TM,N1M1,TM2,ITM1)
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
      SUBROUTINE NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TMOLD,DTMOLD,
     * NNEW,TMNEW,IPER)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Redistributes the mesh according to the function EQDF.
C
      DIMENSION TMOLD(*),DTMOLD(*),TMNEW(*)
C Local
      ALLOCATABLE EQF(:),UNEQ(:),IAL(:)
      ALLOCATE(EQF(NOLD+1),UNEQ(NNEW+1),IAL(NNEW+1))
C
C Put the values of the monotonely increasing function EQDF in EQF.
C
       CALL EQDF(NOLD,NDIM,NCOLD,DTMOLD,NDX,UPS,EQF,IPER)
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
       CALL ORDR(NOLDP1,EQF,NNEWP1,UNEQ,IAL)
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
      SUBROUTINE ORDR(N,TM,N1,TM1,ITM1)
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
      SUBROUTINE EQDF(NTST,NDIM,NCOL,DTM,NDX,UPS,EQF,IPER)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      DIMENSION UPS(NDX,*),EQF(*),DTM(*)
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
           IF(ABS(HD(I,J)).GT.HMACH)SMALL=.FALSE.
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
           E=E+ABS( HD(I,J) )**PWR
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
      COMPLEX(KIND(1.0D0)) EV(*)
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
          EV(I) = CMPLX(WR(I),WI(I),KIND(1.0D0))
       ENDDO
C

 101   FORMAT(I4,I6,' NOTE:Error return from EISPACK routine RG')
 102   FORMAT(/,' Eigenvalues:')
 103   FORMAT(/,' Eigenvectors (by row):')
 104   FORMAT(4X,7ES19.10)
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
             P=ABS(A(IR(I),IC(J)))
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
             P=ABS(A(IR(I),IC(J)))
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
      SUBROUTINE GESC(IAM,N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
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
             P=ABS(A(IR(I),IC(J)))
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
      SUBROUTINE NEWLAB(IAP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Determine a suitable label when restarting.
C
      LOGICAL EOF3
      DIMENSION IAP(*)
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
       ELSEIF( (ABS(ITP).LT.10.AND.ABS(ISW).EQ.2)
     *    .OR. (IPS.EQ.2.AND.ITP.EQ.3)
     *    .OR. (IPS.EQ.4.AND.ISW.EQ.2.AND.ABS(ITP).LT.10)
     *    .OR. (IPS.EQ.5.AND.MOD(ITP,10).EQ.2) )THEN
         IBR=IRS
         IAP(30)=IBR
       ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE FINDLB(IAP,IRS,NFPR,FOUND)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL FOUND,EOF3
C
      DIMENSION IAP(*)
C
C Locates restart point with label IRS and determines type.
C If the label can not be located on unit 3 then FOUND will be .FALSE.
C
       FOUND=.FALSE.
       REWIND 3
       ISW=IAP(10)
C
       DO
         READ(3,*,END=2)IBR,NTOTRS,ITP,LABRS,NFPR,ISWRS,NTPLRS,
     *   NARS,NSKIP
         IAP(27)=ITP
         IAP(30)=IBR
         IF(LABRS.EQ.IRS)THEN
           FOUND=.TRUE.
           IF(ABS(ISW).GE.2)THEN
             IF(ABS(ITP).LT.10)THEN
               ITPST=ABS(ITP)
               IAP(28)=ITPST
             ELSE
               ITPST=ABS(ITP/10)
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
       ENDDO
C
 2    CONTINUE
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE READLB(IAP,U,PAR)
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
      SUBROUTINE SCALEB(IAP,NDIM1,NDX,DVPS,RLD,DTM,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Scales the vector (DVPS,RLD) so its norm becomes 1.
C
      DIMENSION IAP(*),DVPS(NDX,*),DTM(*),RLD(*),THL(*),THU(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       NFPR=IAP(29)
C
       SS=RNRMSQ(IAP,NDIM1,NDX,DVPS,DTM,THU)
C
       DO I=1,NFPR
         SS=SS+THL(I)*RLD(I)**2
       ENDDO
C
       SC=1.d0/DSQRT(SS)
C
       DO J=1,NTST
         DO I=1,NCOL
           K1=(I-1)*NDIM
           DO K=K1+1,K1+NDIM1
             DVPS(K,J)=DVPS(K,J)*SC
           ENDDO
         ENDDO
       ENDDO
C
       DO I=1,NDIM1
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
C     ------ --------- -------- ----
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
C
      INCLUDE 'auto.h'
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      POINTER DTV(:),RAV(:),IAV(:),P0V(:,:),P1V(:,:)
      COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V
      DIMENSION UPS(IAV(1)*IAV(6),*)
      CHARACTER*3 CODE
C
        IPS=IAV(2)
        NX=IAV(1)*IAV(6)
        GETP=0
C
        IF( ABS(IPS).LE.1 .OR. IPS.EQ.5)THEN
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
C
C     ---------- -------
      SUBROUTINE GETMDMX(NDIM1,P0,P1,NMM)
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(OUT) :: P0(NDIM1,NDIM1),P1(NDIM1,NDIM1)
      INTEGER, INTENT(IN) :: NDIM1
      LOGICAL, INTENT(OUT) :: NMM

      DOUBLE PRECISION, POINTER :: DTV(:),RAV(:),P0V(:,:),P1V(:,:)
      INTEGER, POINTER :: IAV(:)
      COMMON /BLPV/ DTV,RAV,IAV,P0V,P1V

      INTEGER NDIM,IPS,ISP,NTOT

        NDIM=IAV(1)
        IPS=IAV(2)
        ISP=IAV(9)
        NTOT=IAV(32)
        NMM=.FALSE.
        IF(NDIM==NDIM1.AND.NTOT>0.AND.ABS(ISP)>0.AND.
     *       (IPS==2.OR.IPS==7.OR.IPS==12))THEN
          P0=P0V
          P1=P1V
          NMM=.TRUE.
        ENDIF

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
