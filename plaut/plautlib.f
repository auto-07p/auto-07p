C-----------------------------------------------------------------------
C      DEFINES DISTANCE OF RELATIVE AND ABSOLUTE MODE
C-----------------------------------------------------------------------
       SUBROUTINE DFDIST
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /ABSRL/ ABSDX,ABSDY,ABSDXY,RLDX,RLDY,RLDXY
C---
C---                                    *ABSOLUTE MODE
       ABSDX  = MAXSX - MINSX
       ABSDY  = MAXSY - MINSY
       ABSDXY = SQRT(ABSDX ** 2 + ABSDY ** 2)
C---                                    *RELATIVE MODE
       RLDX   = XMAX - XMIN
       RLDY   = YMAX - YMIN
       RLDXY  = SQRT(RLDX ** 2 + RLDY ** 2)
       RETURN
       END
C---
C-----------------------------------------------------------------------
C      CONVERT TO PACKAGE USE
C-----------------------------------------------------------------------
       FUNCTION COVTX(XNUM)
       COMMON /REVTN/ P10DX,P10DY,P10DXY,PDX,PDY,PDXY
C---
C---                                    *PLOT-10
         COVTX = XNUM
C---
       RETURN
       END
C---
C-----------------------------------------------------------------------
C      CONVERT Y NUMBER TO GRAPHICS PACKAGE UNITS
C-----------------------------------------------------------------------
       FUNCTION COVTY(YNUM)
       COMMON /REVTN/ P10DX,P10DY,P10DXY,PDX,PDY,PDXY
C---
         COVTY = YNUM
C---
       RETURN
       END
C---
C-----------------------------------------------------------------------
C      CONVERT XYNUM TO GRAPHICS PACKAGE UNITS
C-----------------------------------------------------------------------
       FUNCTION COVTXY(XYNUM)
       COMMON /REVTN/ P10DX,P10DY,P10DXY,PDX,PDY,PDXY
C---
         COVTXY = XYNUM
C---
       RETURN
       END
C---
C-----------------------------------------------------------------------
C      CONVERTS RELATIVE MODE TO ABSOLUTE MODE
C-----------------------------------------------------------------------
       SUBROUTINE WINCT(XIN,YIN,XOUT,YOUT)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /ABSRL/ ABSDX,ABSDY,ABSDXY,RLDX,RLDY,RLDXY
C---
C---
       DX   = (XIN - XMIN) / RLDX
       DY   = (YIN - YMIN) / RLDY
       XOUT = DX * ABSDX + MINSX
       YOUT = DY * ABSDY + MINSY
       RETURN
       END
C---
C-----------------------------------------------------------------------
C      CONVERTS FROM ABSOLUTE MODE TO RELATIVE MODE
C-----------------------------------------------------------------------
       SUBROUTINE REVCT(XIN,YIN,XOUT,YOUT)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /ABSRL/ ABSDX,ABSDY,ABSDXY,RLDX,RLDY,RLDXY
C---
C---
       DX    = (XIN - MINSX) / ABSDX
       DY    = (YIN - MINSY) / ABSDY
       XOUT  = DX * RLDX + XMIN
       YOUT  = DY * RLDY + YMIN
       RETURN
       END
C---
C-----------------------------------------------------------------------
C   CALCULATES THE AMOUNT OF CHARACTER SPACE NECESSARY
C   TO PRINT AXES LABELS IN.  IT DOES THIS BY CALCULATING THE MAX SIZE
C   OF THE INTEGER PART AND THE MAX SIZE OF THE FRACTION AND ADDING 2
C   TO THE TOTAL.  THE TOTAL SIZE IS PASSED BACK IN WHOLE AND THE
C   FRACTION SIZE IS PASSED BACK IN FRACT.
C-----------------------------------------------------------------------
       SUBROUTINE CALCSZ(BMIN,BMAX,WHOLE,FRACT,XINCR)
       INTEGER WHOLE,FRACT
C---
C---
       FRACT = 0
       INTSD = 0
       TMAX  = ABS(BMAX)
       TMIN  = ABS(BMIN)
       IF (TMAX.GT.TMIN) THEN
         BIGNUM = TMAX
       ELSE
         BIGNUM = TMIN
       END IF
       IF (BIGNUM.LT.10) THEN
         INTSD = 0
       ELSE
         INTSD = INT(LOG10(ABS(BIGNUM)) * 1.00001) + 1
       END IF
       XINV = 1 / XINCR
       IF (XINV.GT.1) THEN
         I1 = INT(LOG10(ABS(XINV)))
         IF (I1.GT.0) THEN
           I2 = INT(XINV / (I1 * 10))
         ELSE
           I2 = INT(XINV)
         END IF
         IF (I2.EQ.4) THEN
           FRACT = I1 + 2
         ELSE
           FRACT = I1 + 1
         END IF
       ELSE
         IF (XINCR.EQ.2.5) THEN
           FRACT = 1
         ELSE
           FRACT = 0
         END IF
       END IF
       WHOLE = INTSD + FRACT + 2
       RETURN
       END
C---
C-----------------------------------------------------------------------
C   DRAWS THE X GRID LINES AND
C   SPACES THE X AXIS GRID LABELS.
C-----------------------------------------------------------------------
       SUBROUTINE XAXIS(XINCR)
       LOGICAL ODD,DFT,USR,PLTR,BRNCH,POINT,TIT,AXLB,QLBS,GRIDS,DP
       LOGICAL TOP,BOTTOM
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       INTEGER WHOLE,FRACT,NVX(2),SPLPT
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /PLOTT/ PLTR,POINT
C---                                    *FIND THE SIZE OF THE WHOLE
C---                                    *AND FRACTIONAL PARTS OF
C---                                    *LARGEST LABEL
C---
       CALL CALCSZ(XMIN,XMAX,WHOLE,FRACT,XINCR)
       ODD = .FALSE.
       IF (GRIDS) THEN
         GRDEND = MAXSY
       ELSE
         GRDEND = MINSY
       END IF
       C     = 0
       X1    = XMIN
       XTEMP = MINSX
       IX    = XTEMP - COVTX(C+40)
       IY    = MINSY - COVTY(C+35)
C---                                    *WRITE FIRST GRID LABEL
       NGRIDS = INT(((XMAX - XMIN)/XINCR) + 0.00001)
       SPACE  = (MAXSX - MINSX) / NGRIDS
       INDX   = NINT(SPACE / COVTX(C+17))
       WHOLE=WHOLE+1
       IF(FRACT.EQ.0)ICODE=1
       IF(FRACT.NE.0)ICODE=2
CXX THIS IS A "FIX" TO GET THE X-AXIS NUMBERS ALIGNED WITH THE TIC MARKS
CXX                                                      (EJD AUG. 1985)
CXX IBM GDDM GRAPHICS :
CXX    FINC=WHOLE-6
CXX GDDM/TEK 614 GRAPHICS :
       FINC=0.0
CXX
       CALL CVTIFE(WHOLE,FRACT,X1,ICODE)
       IF (INDX.LE.WHOLE) ODD = .TRUE.
       IX     = IX - FINC
       CALL PLCMDS(21)
       CALL PLCTNM(23)
       CALL PLCMDS(20)
       INDX   = 0
 1     XTEMP  = XTEMP + SPACE
       INDX   = INDX  + 1
       IX     = XTEMP - COVTX(C+40)
       IY     = MINSY - COVTY(C+35)
C---                                    *WRITE OTHER GRID LABELS
       X1     = X1 + XINCR
       CALL CVTIFE(WHOLE,FRACT,X1,ICODE)
       IX     = IX - FINC
       IF (ODD.AND.MOD(INDX,2).EQ.1) THEN
         IY   = IY - COVTY(C + 30)
       END IF
       CALL PLCMDS(21)
       CALL PLCTNM(23)
       CALL PLCMDS(20)
       IX     = XTEMP
       IY     = MINSY - COVTY(C+15)
C---                                    *DRAW SHORT OR LONG GRID LINE
       CALL PLCMDS(22)
       CALL PLCMDS(2)
       CALL PLCMDS(20)
       IY = GRDEND
       CALL PLCMDS(3)
       NGRIDS = NGRIDS - 1
       IF (NGRIDS.GT.1) THEN
         GO TO 1
       ELSE
         IF(NGRIDS.EQ.1) THEN
C---                                    *LAST GRID LINE ALWAYS LONG
           GRDEND = MAXSY
           GO TO 1
         ENDIF
       END IF
       RETURN
       END
C---
C-----------------------------------------------------------------------
C   OUTPUTS THE Y GRID LINES AND SPACES THE
C   Y AXIS GRID LABELS. IT IS SIMILAR TO THE X AXIS.
C-----------------------------------------------------------------------
       SUBROUTINE YAXIS(YINCR)
       INTEGER WHOLE,FRACT,NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       LOGICAL DFT,USR,PLTR,BRNCH,POINT,TIT,AXLB,QLBS,GRIDS,DP
       LOGICAL TOP,BOTTOM
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /PLOTT/ PLTR,POINT
C---
C---
       CALL CALCSZ(YMIN,YMAX,WHOLE,FRACT,YINCR)
       IF (GRIDS) THEN
         GRDEND = MAXSX
       ELSE
         GRDEND = MINSX
       END IF
       C     = 0
       YTEMP = MINSY
       Y1    = YMIN
       IX    = MINSX - COVTX(C+149)
       IY    = YTEMP
       IF(FRACT.EQ.0)ICODE=1
       IF(FRACT.NE.0)ICODE=2
       WHOLE=10
       CALL CVTIFE(WHOLE,FRACT,Y1,ICODE)
       CALL PLCMDS(21)
       CALL PLCTNM(23)
       CALL PLCMDS(20)
       NGRIDS = INT(((YMAX - YMIN)/YINCR) + 0.00001)
       SPACE  = (MAXSY - MINSY) / NGRIDS
1      YTEMP  = YTEMP + SPACE
       IX     = MINSX - COVTX(C+149)
       IY     = YTEMP
       Y1     = Y1 + YINCR
       CALL CVTIFE(WHOLE,FRACT,Y1,ICODE)
       CALL PLCMDS(21)
       CALL PLCTNM(23)
       CALL PLCMDS(20)
       IX     = MINSX - COVTX(C+15)
       IY     = YTEMP
       CALL PLCMDS(22)
       CALL PLCMDS(2)
       CALL PLCMDS(20)
       IX     = GRDEND
       CALL PLCMDS(22)
       CALL PLCMDS(3)
       CALL PLCMDS(20)
       NGRIDS = NGRIDS - 1
       IF (NGRIDS.GT.1) THEN
         GO TO 1
       ELSE
C---                                    *LAST GRID LINE ALWAYS LONG
         IF (NGRIDS.EQ.1) THEN
           GRDEND = MAXSX
           GO TO 1
         ENDIF
       END IF
       RETURN
       END
C---                                    *LAST GRID LINE ALWAYS LONG
C-----------------------------------------------------------------------
C      CONVERTS NUMERIC TO STRING CHARACTER:
C      IF ICODE = 1 THEN CONVERT FROM <I> FORMAT TO STRING CHARACTER
C      IF ICODE = 2 THEN CONVERT FROM <F> FORMAT TO STRING CHARACTER
C      IF ICODE = 3 THEN CONVERT FROM <E> FORMAT TO STRING CHARACTER
C-----------------------------------------------------------------------
       SUBROUTINE CVTIFE(IWHOLE,IFRACT,XYNUM,ICODE)
       GO TO (1,2,3) ICODE
 1     CALL COVTI(IWHOLE,IFRACT,XYNUM)
       RETURN
 2     CALL COVTF(IWHOLE,IFRACT,XYNUM)
       RETURN
 3     CALL COVTF(IWHOLE,IFRACT,XYNUM)
       RETURN
       END
C---                                    *LAST GRID LINE ALWAYS LONG
C-----------------------------------------------------------------------
C      CONVERTS FROM NUMERIC TO STRING  (EXPONENT)
C-----------------------------------------------------------------------
       SUBROUTINE COVTE(WHOLE,FRACT,XYNUM)
       INTEGER WHOLE,FRACT
       CHARACTER*1 CHONE,CH(0:9),CHTWO*2,CHFIF*15
       LOGICAL NEG
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
       DATA CH /'0','1','2','3','4','5','6','7','8','9'/
       CHFIF = ' '
       INDXCH = WHOLE
       NEG = .FALSE.
       F   = LOG10(ABS(XYNUM) + 0.00001)
       KF  = INT(F)
       IF (KF.LT.0) KF = KF + 1
       NUM = INT(10 ** (FRACT + F - KF))
       IF (KF.LT.0) NEG = .TRUE.
       KF = ABS(KF)
       INDEX = WHOLE - 1
       DO 1 I=WHOLE,INDEX,-1
         INDX = MOD(KF,10)
         KF   = KF / 10
         CHFIF(I:I) = CH(INDX)
 1     CONTINUE
       I = WHOLE - 3
       IF (NEG) THEN
         CHFIF(I:I+1) = 'E-'
       ELSE
         CHFIF(I:I+1) = 'E+'
       END IF
       INDX = WHOLE - 4
       INDEX = INDX - FRACT + 1
       DO 2 I=INDX,INDEX,-1
         KF = MOD(NUM,10)
         NUM = NUM / 10
         CHFIF(I:I) = CH(KF)
 2     CONTINUE
       CHFIF(INDEX-1:INDEX-1) = '.'
       IF (XYNUM.GT.0) THEN
         CHFIF(INDEX-2:INDEX-2) = '0'
       ELSE
         KF = WHOLE - FRACT - 5
         IF (KF.EQ.2) THEN
           CHFIF(1:2) = '-0'
         ELSE
           CHFIF(1:1) = '-'
         END IF
       END IF
       RETURN
       END
C---                                    *LAST GRID LINE ALWAYS LONG
C-----------------------------------------------------------------------
C      CONVERT FROM NUMERIC TO STRING CHARACTER (FN.0 FORMAT)
C-----------------------------------------------------------------------
       SUBROUTINE COVTI(WHOLE,FRACT,XYNUM)
       INTEGER WHOLE,FRACT
       CHARACTER*1 CHONE,CH(0:9),CHTWO*2,CHFIF*15
       LOGICAL NEG
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
       DATA CH/'0','1','2','3','4','5','6','7','8','9'/
       CHFIF = ' '
       INDXCH = WHOLE
       NEG = .FALSE.
       IF (XYNUM.LT.0) NEG = .TRUE.
       NUM = INT(ABS((1.0+1.0E-5)*XYNUM))
       CHFIF(WHOLE:WHOLE) = '.'
       INDX = WHOLE - 1
       IF (NUM.EQ.0) THEN
         CHFIF(INDX:INDX) = '0'
         RETURN
       END IF
       DO 1 I=INDX,1,-1
         IF (NUM.EQ.0) THEN
           IF (NEG) CHFIF(I:I) = '-'
           RETURN
         END IF
         KF = MOD(NUM,10)
         NUM = NUM / 10
         CHFIF(I:I) = CH(KF)
 1     CONTINUE
       RETURN
       END
C---
C-----------------------------------------------------------------------
C      CONVERT FROM NUMERIC TO STRING CHARACTER (<FN.M> FORMAT)
C-----------------------------------------------------------------------
       SUBROUTINE COVTF(WHOLE,FRACT,XYNUM)
       INTEGER WHOLE,FRACT
       CHARACTER*1 CHONE,CH(0:9),CHTWO*2,CHFIF*15
       LOGICAL NEG
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
       DATA CH/'0','1','2','3','4','5','6','7','8','9'/
       CHFIF = ' '
       INDXCH = WHOLE
       NEG = .FALSE.
       IF (XYNUM.LT.0) NEG = .TRUE.
       NUM = ABS(INT((1.0+1.0E-5)*XYNUM))
       F = ABS(XYNUM) - NUM
       KF = NINT(F * 10 ** FRACT)
       IF (NUM.EQ.0.AND.KF.EQ.0) NEG = .FALSE.
       INDX = WHOLE - FRACT + 1
       DO 1 I=WHOLE,INDX,-1
         K = MOD(KF,10)
         KF = KF / 10
         CHFIF(I:I) = CH(K)
1      CONTINUE
       CHFIF(INDX-1:INDX-1) = '.'
       INDX = WHOLE - FRACT - 1
       IF (INDX.EQ.0) RETURN
       IF (INDX.EQ.1.AND.NUM.EQ.0.AND.NEG) THEN
         CHFIF(1:1) = '-'
         RETURN
       ELSE IF (INDX.EQ.1.AND.NUM.EQ.0) THEN
         CHFIF(1:1) = '0'
         RETURN
       END IF
       IF (NUM.EQ.0) THEN
         IF (NEG) THEN
           CHFIF(INDX-1:INDX) = '-0'
         ELSE
           CHFIF(INDX:INDX) = '0'
         END IF
         RETURN
       END IF
       DO 2 I=INDX,1,-1
         IF (NUM.EQ.0)THEN
           IF (NEG) CHFIF(I:I) = '-'
           RETURN
         END IF
         KF = MOD(NUM,10)
         NUM = NUM / 10
         CHFIF(I:I) = CH(KF)
 2     CONTINUE
       RETURN
       END
C-----------------------------------------------------------------------
C       SAVE THE CURRENT PLOT IN FILE UNIT ISAVE.
C-----------------------------------------------------------------------
        SUBROUTINE ASKSVE(SAVE)
        LOGICAL SAVE
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        SAVE = .FALSE.
        IF (IWRITE.NE.ISAVE) THEN
          IWRITE = ISAVE
          SAVE   = .TRUE.
        ELSE
          IWRITE = ITERM
          CALL COMPLT
        ENDIF
        RETURN
        END
C---
C-----------------------------------------------------------------------
C        WRITES A MESSAGE TO CONFIRM
C        THAT A PLOT HAS BEEN SAVED.
C-----------------------------------------------------------------------
        SUBROUTINE COMPLT
        CHARACTER OST2*20
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        OST2 = ' SAVE COMPLETE'
        WRITE(IWRITE,1) OST2
 1      FORMAT(A20)
        RETURN
        END
C---
C-----------------------------------------------------------------------
C---
C       JOINS THE POINTS (X0,Y0) AND (X,Y) BY ONE OF
C       FOUR LINE TYPES DEPENDING ON THE VALUE OF 'ID'.
C---
C-----------------------------------------------------------------------
        SUBROUTINE SYMBLS(X0,Y0,ID,IDS,FIRST,SYMBOL)
        INTEGER NVX(2),SPLPT
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
        LOGICAL SYMBOL,FIRST,SOLID,DSH,DSHS,LHCIR,LNKDSH
        LOGICAL LSYBL,LDSH,LDOT
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /SYBL/ SOLID,DSH,DSHS,LHCIR,LSYBL,LDSH,LDOT
        COMMON /MUDSH/ XDSH,YDSH,XDSHS,YDSHS,XD1,YD1
        COMMON /LINKDA/ LNKDSH
        COMMON /RADI/ RASY,RATY,RADI,RADINC
        COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
        COMMON /DASHC/ CDASH,DSKDSH,CDASHS,DKDSHS,CD1,CSK1,ITYPE
        COMMON /DSH1/ SKD1,DSH1,DSH2,S1,D1,D2,IDASH
        COMMON /TWOPT/ XNEW1,YNEW1,XOLD1,YOLD1
        COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
C---
C---
C---                                    *SOLID LINES
        IF (ID.EQ.2) THEN
          CALL PLCMDS(36)
          IF (.NOT.SOLID.AND.FIRST) THEN
            CALL CLIP(X0,Y0,1)
            SOLID = .TRUE.
          ELSE IF (.NOT.SOLID.AND..NOT.FIRST) THEN
            CALL CLIP(XOLD1,YOLD1,3)
            CALL CLIP(X0,Y0,2)
            SOLID = .TRUE.
          ELSE
            CALL CLIP(X0,Y0,2)
          END IF
          CALL PLCMDS(20)
        END IF
C---
        IF (ID.EQ.1) THEN
C---                                    *DASHED LINES
          CALL PLCMDS(35)
          IF (.NOT.DSH.AND.FIRST) THEN
            IF (FIRST) CALL CLIP(X0,Y0,1)
            XDSH   = X0
            YDSH   = Y0
            DSH    = .TRUE.
            LNKDSH = .TRUE.
            CDASH  = DSDSH
            DSKDSH = SKDSH
          ELSE IF (.NOT.DSH.AND..NOT.FIRST) THEN
            XDSH   = XOLD1
            YDSH   = YOLD1
            DSH    = .TRUE.
            LNKDSH = .TRUE.
            CDASH  = DSDSH
            CALL DPDSH
          ELSE
            CALL DPDSH
          END IF
          CALL PLCMDS(20)
        END IF
        IF (ID.EQ.3) THEN
C---                                    *OPEN CIRCLES
          IF (.NOT.LHCIR.AND.FIRST) THEN
            CALL PLCMDS(7)
            CALL HCIR
            LHCIR = .TRUE.
            SKCIR = DSCIR
          ELSE IF (.NOT.LHCIR.AND..NOT.FIRST) THEN
            X     = XOLD1
            Y     = YOLD1
            CALL PLCMDS(7)
            CALL HCIR
            LHCIR = .TRUE.
            SKCIR = DSCIR
            CALL DPCIR(ID)
          ELSE
            CALL DPCIR(ID)
          END IF
        END IF
        IF (ID.EQ.4) THEN
C---                                    *SOLID CIRCLES
          IF (.NOT.LHCIR.AND.FIRST) THEN
            CALL PLCMDS(7)
            CALL SCIR
            LHCIR = .TRUE.
            SKCIR = DSCIR
          ELSE IF (.NOT.LHCIR.AND..NOT.FIRST) THEN
            X     = XOLD1
            Y     = YOLD1
            CALL PLCMDS(7)
            CALL SCIR
            LHCIR = .TRUE.
            SKCIR = DSCIR
            CALL DPCIR(ID)
          ELSE
            CALL DPCIR(ID)
          END IF
        END IF
        IF (ID.EQ.5) THEN
C---                                    *DOTTED LINES
          IF (.NOT.LDOT.AND.FIRST) THEN
            CALL CLIP(X0,Y0,1)
            LDOT = .TRUE.
          ELSE IF (.NOT.LDOT.AND..NOT.FIRST) THEN
            CALL CLIP(XOLD1,YOLD1,1)
            LDOT = .TRUE.
            CALL DPDOLN
          ELSE
            CALL DPDOLN
          END IF
        END IF
        IF (ID.EQ.6) THEN
C---                                    *DASHED LINES
          IF (.NOT.DSHS.AND.FIRST) THEN
            XDSHS = X0
            YDSHS = Y0
            DSHS  = .TRUE.
            ITYPE = 1
            CD1   = CDASHS
            CSK1  = DKDSHS
            IF (FIRST) CALL CLIP(X0,Y0,1)
          ELSE IF (.NOT.DSHS.AND..NOT.FIRST) THEN
            XDSHS = XOLD1
            YDSHS = YOLD1
            DSHS  = .TRUE.
            ITYPE = 1
            CD1   = CDASHS
            CSK1  = DKDSHS
            CALL DASHES
          ELSE
            CALL DASHES
          END IF
        END IF
C---                                    *LONG AND SHORT DASHES
        IF (ID.EQ.7) THEN
          IF (.NOT.LDSH.AND.FIRST) THEN
            XD1   = X0
            YD1   = Y0
            LDSH  = .TRUE.
            IDASH = 1
            D1    = DSH1
            D2    = DSH2
            S1    = SKD1
            IF (FIRST) CALL CLIP(X0,Y0,1)
          ELSE IF (.NOT.LDSH.AND..NOT.FIRST) THEN
            XD1   = XOLD1
            YD1   = YOLD1
            LDSH  = .TRUE.
            IDASH = 1
            D1    = DSH1
            D2    = DSH2
            S1    = SKD1
            CALL LSDSH
          ELSE
            CALL LSDSH
          END IF
        END IF
C---                                    *SYMBOLS
        IF (IDS.GE.8.AND.IDS.LE.27) THEN
          IF (.NOT.LSYBL.AND.FIRST) THEN
            X     = X0
            Y     = Y0
            RADI  = RATY
            CALL PLCMDS(7)
            CALL DPSYBL(IDS)
            MBACK = 1
            LSYBL = .TRUE.
          ELSE IF (.NOT.LSYBL.AND..NOT.FIRST) THEN
            IX    = XOLD
            IY    = YOLD
            RADI  = RATY
            CALL DPSYBL(IDS)
            CALL CLSYBL(IDS,MBACK)
            MBACK = 1
            LSYBL = .TRUE.
          ELSE
            CALL CLSYBL(IDS,MBACK)
          END IF
          IF (MBACK.EQ.1.AND.SOLID) CALL CLIP(X0,Y0,3)
        END IF
C---                                    *WITH SYMBOL
        IF (SYMBOL) THEN
          IF (IDS.EQ.0.AND.ID.NE.3.AND.ID.NE.4) THEN
            X     = X0
            Y     = Y0
            RADI  = RASY
            CALL PLCMDS(7)
            CALL DRAW
          END IF
        END IF
        RETURN
        END
C---
C-----------------------------------------------------------------------
C---
C       IS USED TO SET THE PLOTTING SYMBOL FOR
C       BRANCHES IN THE BIFURCATION DIAGRAM. TO DIFFERENTIATE BETWEEN
C       BRANCHES, THE SIGNS OF THE BRANCH NUMBER AND THE POINT NUMBER
C       ARE USED (I.E., THE FIRST AND SECOND COLUMNS IN UNIT 7)
C---
C       FOR EXAMPLE:
C---
C             SIGN OF
C       BRANCH #      POINT #      REPRESENTS             SYMBOL USED
C       ===============================================================
C          +              +     UNSTABLE STEADY STATE           1
C          +              -     STABLE STEADY STATE             2
C          -              +     UNSTABLE PERIODIC               3
C          -              -     STABLE PERIODIC                 4
C       ===============================================================
C---
C-----------------------------------------------------------------------
        SUBROUTINE SDFPLT(ICD,ICDS)
        CHARACTER*2 SIGN(4)
        DIMENSION ICD(4),ICDS(4)
        DATA SIGN/'++','+-','-+','--'/
C---
C---
        CALL PRSYBL
        DO 1 I=1,4
          IF (ICD(I).NE.0.AND.ICDS(I).NE.0) THEN
            WRITE(6,2) SIGN(I),ICD(I),ICDS(I)
          ELSE IF (ICD(I).NE.0) THEN
            WRITE(6,3) SIGN(I),ICD(I)
          ELSE IF (ICDS(I).NE.0) THEN
            WRITE(6,3) SIGN(I),ICDS(I)
          ELSE
            WRITE(6,4) SIGN(I)
          END IF
          CALL READSY(ICD(I),ICDS(I))
 1      CONTINUE
        RETURN
 2      FORMAT(T5,'ENTER CODES FOR ',A2,' OLD VALUES WERE ',I3,
     +         2X,I3,'   ')
 3      FORMAT(T5,'ENTER CODE FOR ',A2,' OLD VALUE WAS',I3,
     +         '     ')
 4      FORMAT(T5,'ENTER CODE FOR ',A2,'   ')
        END
C-----------------------------------------------------------------------
C       PRINTS TYPE AND SYMBOL
C-----------------------------------------------------------------------
        SUBROUTINE PRSYBL
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        WRITE(ITERM,1)
 1      FORMAT(    '  TYPES :',
     +         /,T5 ,'<1>     DASHED LINE',
     +           T40,'<5)     DOTTED  LINE',
     +         /,T5 ,'<2>     SOLID  LINE',
     +           T40,'<6>     DASHED AND DOTTED LINE',
     +         /,T5 ,'<3>     OPEN CIRCLE  (FIXED SIZE)',
     +           T40,'<7>     LONG AND SHORT DASHES',
     +         /,T5 ,'<4>     SOLID CIRCLE (FIXED SIZE)',
     +         /,
     +         /,T5 ,'<8>     OPEN SQUARE',
     +           T40,'<18>    SOLID DOWN TRIANGLE',
     +         /,T5 ,'<9>     SOLID SQUARE',
     +           T40,'<19>    SQUARE AND "X" SIGN',
     +         /,T5 ,'<10>    OPEN TRIANGLE',
     +           T40,'<20>    CIRCLE AND PLUS SIGN')
        WRITE(ITERM,2)
 2      FORMAT(  T5 ,'<11>    SOLID TRIANGLE',
     +           T40,'<21>    DOUBLE TRIANGLE',
     +         /,T5 ,'<12>    OPEN CIRCLE  (VAR. SIZE)',
     +           T40,'<22>    CIRCLE AND "X" SIGN',
     +         /,T5 ,'<13>    SOLID CIRCLE (VAR. SIZE)',
     +           T40,'<23>    SQUARE AND TRIANGLE',
     +         /,T5 ,'<14>    PLUS SIGN',
     +           T40,'<24>    SQUARE AND PLUS SIGN',
     +         /,T5 ,'<15>    OPEN DIAMOND',
     +           T40,'<25>    DIAMOND AND PLUS SIGN',
     +         /,T5 ,'<16>    SOLID DIAMOND',
     +           T40,'<26>    "X" SIGN',
     +         /,T5 ,'<17>    OPEN DOWN TRIANGLE',
     +           T40,'<27>    START',/)
        RETURN
        END
C-----------------------------------------------------------------------
C       PRINTS SYMBOLS FOR SPECIAL SYMBOLS COMMAND <SS>
C-----------------------------------------------------------------------
        SUBROUTINE PRSYM
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        WRITE(ITERM,1)
        WRITE(ITERM,2)
 1      FORMAT(/,T30,'SYMBOLS',/)
 2      FORMAT(T5 ,'<1>   OPEN SQUARE',
     +        T40,'<11>  SOLID DOWN TRIANGLE',/,
     +        T5 ,'<2>   SOLID SQUARE',
     +        T40,'<12>  SQUARE AND "X" SIGN',/,
     +        T5 ,'<3>   OPEN TRIANGLE',
     +        T40,'<13>  CIRCLE AND PLUS SIGN',/,
     +        T5 ,'<4>   SOLID TRIANGLE',
     +        T40,'<14>  DOUBLE TRIANGLE',/,
     +        T5 ,'<5>   OPEN CIRCLE',
     +        T40,'<15>  CIRCLE AND "X" SIGN',/,
     +        T5 ,'<6>   SOLID CIRCLE',
     +        T40,'<16>  SQUARE AND TRIANGLE',/,
     +        T5 ,'<7>   PLUS SIGN',
     +        T40,'<17>  SQUARE AND PLUS SIGN',/,
     +        T5 ,'<8>   OPEN DIAMOND',
     +        T40,'<18>  DIAMOND AND PLUS SIGN',/,
     +        T5 ,'<9>   SOLID DIAMOND',
     +        T40,'<19>  "X" SIGN',/,
     +        T5 ,'<10>  OPEN DOWN TRIANGLE',
     +        T40,'<20>  START',/)
        RETURN
        END
C-----------------------------------------------------------------------
C       PRINTS DEFAULT SPECIAL SYMBOLS
C-----------------------------------------------------------------------
        SUBROUTINE SYSDF(IPT,SYMBOL)
        INTEGER IPT(10),SYMBOL(10)
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        WRITE(ITERM,2)
        DO 1 I=1,5
          J = I + 5
          IF (SYMBOL(I).NE.0.AND.SYMBOL(J).NE.0) THEN
            WRITE(ITERM,3) I,IPT(I),SYMBOL(I),J,IPT(J),SYMBOL(J)
          ELSE IF (SYMBOL(I).NE.0) THEN
            WRITE(ITERM,4) I,IPT(I),SYMBOL(I),J
          ELSE IF (SYMBOL(J).NE.0) THEN
            WRITE(ITERM,5) I,J,IPT(J),SYMBOL(J)
          ELSE
            WRITE(ITERM,6) I,J
          END IF
 1      CONTINUE
        RETURN
 2      FORMAT(T10,'DEFAULT SYMBOLS ARE : ',/,
     +         T10,'INDEX',T21,'IPT',T31,'SYMBOL',
     +         T41,'INDEX',T53,'IPT',T61,'SYMBOL')
 3      FORMAT(T12,I2,T20,I3,T32,I3,T42,I3,T52,I3,T62,I3)
 4      FORMAT(T12,I2,T20,I3,T32,I3,T42,I3)
 5      FORMAT(T12,I2,              T42,I3,T52,I3,T62,I3)
 6      FORMAT(T12,I2,              T42,I3)
        END
C-----------------------------------------------------------------------
C       ENTER NEW SYMBOLS
C-----------------------------------------------------------------------
        SUBROUTINE ETSYBL(ITP,SYMBOL)
        INTEGER PARRY(1999),ITP(10),SYMBOL(10)
        CHARACTER*80 ISTR,OSTR
        LOGICAL VALID
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        INDEX = 0
        OSTR  = ' '
        WRITE(ITERM,2) OSTR
        OSTR  = ' ENTER : INDEX, POINT TYPE, AND SYMBOL NUMBER'
        WRITE(ITERM,2) OSTR
        OSTR  = ' ( TO TERMINATE TYPING : <E> OR <END> OR <RETURN> )'
        WRITE(ITERM,2) OSTR
 1      CALL READS8(ISTR)
        IF (ISTR.EQ.'END'.OR.ISTR.EQ.'end'.OR.
     +      ISTR.EQ.'E  '.OR.ISTR.EQ.'e  '.OR.
     +      ISTR.EQ.' ') RETURN
        I = 1
        IF (ISTR(1:1).EQ.' ') I = 2
        CALL CONVT(ISTR,I,NUM,PARRY,VALID)
        IF (.NOT.VALID) GO TO 1
        IF (NUM.EQ.1.AND.PARRY(1).GE.1.AND.PARRY(1).LE.10) THEN
          SYMBOL(PARRY(1)) = 0
          INDEX            = INDEX + 1
        END IF
        IF (NUM.EQ.3.AND.PARRY(1).GE.1.AND.PARRY(1).LE.10) THEN
          J = PARRY(1)
          IF (PARRY(3).GE.1.AND.PARRY(3).LE.20) THEN
            ITP(J)    = PARRY(2)
            SYMBOL(J) = PARRY(3)
            INDEX     = INDEX + 1
          ELSE
            SYMBOL(J) = 0
          END IF
        END IF
C---                                    *ERROR
        IF (.NOT.(NUM.EQ.1.OR.NUM.EQ.3)) THEN
          OSTR = ' ERROR , ENTER AGAIN'
          WRITE(ITERM,2) OSTR
        END IF
        IF (INDEX.LT.10) GO TO 1
        RETURN
 2      FORMAT(A80)
        END
C-----------------------------------------------------------------------
C       INITIALIZES SPECIAL SYMBOLS
C-----------------------------------------------------------------------
        SUBROUTINE IBDSY(IPT,SYMBOL)
        INTEGER IPT(10),SYMBOL(10)
C---
C---
        DO 1 I=1,10
          IPT(I)    = I
          SYMBOL(I) = 0
 1      CONTINUE
        SYMBOL(1) = 1
        SYMBOL(3) = 2
        SYMBOL(6) = 8
        SYMBOL(7) = 14
        SYMBOL(8) = 9
        RETURN
        END
C-----------------------------------------------------------------------
C       READS STRING DATA AND CONVERTS TO
C       INTEGER AND ASSIGNS ID AND IDS
C-----------------------------------------------------------------------
        SUBROUTINE READSY(ICDT,ICDS)
        INTEGER PARRY(1999)
        LOGICAL VALID
        CHARACTER*80 ISTR
C---
C---
        CALL READS8(ISTR)
        I = 1
        IF (ISTR(1:1).EQ.' ') I=2
        IF (ISTR.NE.' ') THEN
          CALL CONVT(ISTR,I,NUM,PARRY,VALID)
          IF (VALID) THEN
            IDATA1 = PARRY(1)
            IF (NUM.EQ.1) THEN
              IF (IDATA1.GE.1.AND.IDATA1.LE.7) THEN
                ICDT = IDATA1
              ELSE
                ICDT = 0
              END IF
              IF (IDATA1.GE.8.AND.IDATA1.LE.27) THEN
                ICDS = IDATA1
              ELSE
                ICDS = 0
              END IF
            ELSE IF (NUM.GE.2) THEN
              IDATA2 = PARRY(2)
              IF (IDATA1.GE.1.AND.IDATA1.LE.7) THEN
                ICDT = IDATA1
              ELSE IF (IDATA2.GE.1.AND.IDATA2.LE.7) THEN
                ICDT = IDATA2
              ELSE
                ICDT = 0
              END IF
              IF (IDATA1.GE.8.AND.IDATA1.LE.27) THEN
                ICDS = IDATA1
              ELSE IF (IDATA2.GE.8.AND.IDATA2.LE.27) THEN
                ICDS = IDATA2
              ELSE
                ICDS = 0
              END IF
            END IF
          END IF
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C    DRAWS THE SYMBOLS WHEN THE SYMBOL FLAG IS TRUE.
C-----------------------------------------------------------------------
        SUBROUTINE DRAW
        INTEGER NVX(2),SPLPT,ITP(10),SYMBL(10)
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /BDSY/ITP,SYMBL
C---
C---
        DO 1 I=1,10
          IF (SYMBL(I).NE.0) THEN
            IF (ITP(I).EQ.SPLPT) THEN
              ID = SYMBL(I) + 7
              CALL DPSYBL(ID)
              CALL PLCMDS(4)
              RETURN
            END IF
          END IF
 1      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C      DISPLAYS ALL DEFAULT PLOTTING SYMBOLS.
C-----------------------------------------------------------------------
       SUBROUTINE SHWSYS
       INTEGER ITP(10),SYMBL(10)
       COMMON /BDSY/ ITP,SYMBL
C---
C---
       CALL PRSYM
       CALL SYSDF(ITP,SYMBL)
       CALL ETSYBL(ITP,SYMBL)
       RETURN
       END
C-----------------------------------------------------------------------
C      READS STRING DATA AND CONVERTS TO INTEGER
C-----------------------------------------------------------------------
       SUBROUTINE READAT(IDATA)
       INTEGER PARRY(1999)
       LOGICAL VALID
       CHARACTER*80 ISTR
C---
C---
       CALL READS8(ISTR)
       I = 1
       IF (ISTR(1:1).EQ.' ') I = 2
       IF (ISTR.NE.' ') THEN
         CALL CONVT(ISTR,I,NUM,PARRY,VALID)
         IF (VALID) IDATA = PARRY(1)
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C      DEFINES RADIUS OF THE SYMBOLS IN <SS> SET
C      SYMBOLS OR RADIUS OF <SD>
C-----------------------------------------------------------------------
       SUBROUTINE DFRADI(RADI,ICODE)
       COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
       C  = 0
       D1 = COVTXY(C+2)
       D2 = COVTXY(C+15)
1      IF (ICODE.EQ.1) THEN
         WRITE(ITERM,2)D1,D2,RADI
       ELSE
         WRITE(ITERM,3)D1,D2,RADI
       END IF
       READ(5,*,ERR=1) DATA
       IF (DATA.GE.D1.AND.DATA.LE.D2) THEN
         RADI = DATA
       ELSE
         GOTO 1
       END IF
       RETURN
 2     FORMAT(' ENTER SYMBOL RADIUS; LIMITS(',F4.1,',',F4.1,
     *        '); CURRENT ',F4.1,' : ')
 3     FORMAT(' ENTER CURVE SYMBOL RADIUS; LIMITS(',F4.1,',',F4.1,
     *        '); CURRENT ',F4.1,' : ')
       END
C-----------------------------------------------------------------------
C      COMPUTES THE DISTANCE BETWEEN TWO POINTS
C-----------------------------------------------------------------------
       SUBROUTINE DISXY(XMAX,YMAX,XMIN,YMIN,DISTCE)
C---
C---
       DX     = XMAX - XMIN
       DY     = YMAX - YMIN
       DISTCE = SQRT(DX ** 2 + DY ** 2)
       RETURN
       END
C-----------------------------------------------------------------------
C      DISPLAYS CURVES USING DASHED LINES
C-----------------------------------------------------------------------
       SUBROUTINE DPDSH
       LOGICAL LNKDSH
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
       COMMON /LINKDA/ LNKDSH
       COMMON /DASHC/ CDASH,DSKDSH,CDASHS,DKDSHS,CD1,CSK1,ITYPE
       COMMON /MUDSH/ XDSH,YDSH,XDSHS,YDSHS,XD1,YD1
C---
C---
       X1 = XOLD
       Y1 = YOLD
 1     CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
C---                                    *DASHED LINES
       IF (LNKDSH) THEN
         IF (CDASH.GE.DIS) THEN
           CALL CLIP(XDSH,YDSH,3)
           CALL REVCT(XNEW,YNEW,XDSH,YDSH)
           CALL CLIP(XDSH,YDSH,2)
           CDASH = CDASH - DIS
         ELSE
           CALL CLIP(XDSH,YDSH,3)
           RINC   = CDASH / DIS
           DX     = XNEW - XOLD
           DY     = YNEW - YOLD
           XOLD  = XOLD + RINC * DX
           YOLD  = YOLD + RINC * DY
           CALL REVCT(XOLD,YOLD,XDSH,YDSH)
           CALL CLIP(XDSH,YDSH,2)
           CDASH  = DSDSH
           LNKDSH = .FALSE.
         END IF
       END IF
C---                                    *SKIP
       IF (.NOT.LNKDSH) THEN
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         IF (DSKDSH.GE.DIS) THEN
           DSKDSH = DSKDSH - DIS
         ELSE
           DX     = XNEW - XOLD
           DY     = YNEW - YOLD
           RINC   = DSKDSH / DIS
           XOLD  = XOLD + RINC * DX
           YOLD  = YOLD + RINC * DY
           DSKDSH = SKDSH
           LNKDSH = .TRUE.
           CALL REVCT(XOLD,YOLD,XDSH,YDSH)
           GO TO 1
         END IF
       END IF
C---                                    *KEEP LAST POINT
       XOLD = X1
       YOLD = Y1
       RETURN
       END
C-----------------------------------------------------------------------
C      DISPLAYS CURVES USING DASHED LINES
C-----------------------------------------------------------------------
       SUBROUTINE DASHES
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
       COMMON /DASHC/ CDASH,DSKDSH,CDASHS,DKDSHS,CD1,CSK1,ITYPE
       COMMON /MUDSH/ XDSH,YDSH,XDSHS,YDSHS,XD1,YD1
C---
C---
C---                                    *KEEP LAST POINT
       X1 = XOLD
       Y1 = YOLD
 1     CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
C---                                    *DASHED LINES
       CALL PLCMDS(35)
       IF (ITYPE.EQ.1) THEN
         IF (CD1.GE.DIS) THEN
           CALL CLIP(XDSHS,YDSHS,3)
           CALL REVCT(XNEW,YNEW,XDSHS,YDSHS)
           CALL CLIP(XDSHS,YDSHS,2)
           CD1   = CD1 - DIS
         ELSE
           RINC  = CD1 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL CLIP(XDSHS,YDSHS,3)
           CALL REVCT(XOLD,YOLD,XDSHS,YDSHS)
           CALL CLIP(XDSHS,YDSHS,2)
           CD1   = CDASHS
           ITYPE = 2
         END IF
       END IF
C---                                    *POINT BETWEEN DASHES
       IF (ITYPE.EQ.2) THEN
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         IF (CSK1.GE.DIS) THEN
           CSK1  = CSK1 - DIS
         ELSE
           RINC  = CSK1 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL REVCT(XOLD,YOLD,XDSHS,YDSHS)
           CALL CLIP(XDSHS,YDSHS,1)
           CSK1  = DKDSHS
           ITYPE = 3
         END IF
       END IF
C---                                    *SKIP
       IF (ITYPE.EQ.3) THEN
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         IF (CSK1.GE.DIS) THEN
           CSK1  = CSK1 - DIS
         ELSE
           RINC  = CSK1 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL REVCT(XOLD,YOLD,XDSHS,YDSHS)
           CSK1  = DKDSHS
           ITYPE = 1
           GO TO 1
         END IF
       END IF
C---                                    *RETURN  LAST POINT
       XOLD = X1
       YOLD = Y1
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C      DISPLAYS CURVES WITH LONG AND SHORT DASHES
C-----------------------------------------------------------------------
       SUBROUTINE LSDSH
       COMMON /DSH1/ SKD1,DSH1,DSH2,S1,D1,D2,IDASH
       COMMON /MUDSH/ XDSH,YDSH,XDSHS,YDSHS,XD1,YD1
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
C---                                    *KEEP LAST POINT
       X1 = XOLD
       Y1 = YOLD
 1     CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
       CALL PLCMDS(38)
       IF (IDASH.EQ.1) THEN
         IF (D1.GE.DIS) THEN
           CALL CLIP(XD1,YD1,3)
           CALL REVCT(XNEW,YNEW,XD1,YD1)
           CALL CLIP(XD1,YD1,2)
           D1  = D1 - DIS
         ELSE
           RINC  = D1 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL CLIP(XD1,YD1,3)
           CALL REVCT(XOLD,YOLD,XD1,YD1)
           CALL CLIP(XD1,YD1,2)
           D1    = DSH1
           IDASH = 2
         END IF
       END IF
C---                                    *SKIP
       IF (IDASH.EQ.2) THEN
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         IF (S1.GE.DIS) THEN
           S1 = S1 - DIS
         ELSE
           RINC  = S1 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL REVCT(XOLD,YOLD,XD1,YD1)
           S1    = SKD1
           IDASH = 3
         END IF
       END IF
C---                                    *SHORT DASH
       IF (IDASH.EQ.3) THEN
         CALL PLCMDS(39)
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         CALL CLIP(XD1,YD1,3)
         IF (D2.GE.DIS) THEN
           CALL REVCT(XNEW,YNEW,XD1,YD1)
           CALL CLIP(XD1,YD1,2)
           D2  = D2 - DIS
         ELSE
           RINC  = D2 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL REVCT(XOLD,YOLD,XD1,YD1)
           CALL CLIP(XD1,YD1,2)
           D2    = DSH2
           IDASH = 4
         END IF
       END IF
C---                                    *SKIP
       IF (IDASH.EQ.4) THEN
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         IF (S1.GE.DIS) THEN
           S1 = S1 - DIS
         ELSE
           RINC  = S1 / DIS
           DX    = XNEW - XOLD
           DY    = YNEW - YOLD
           XOLD = XOLD + RINC * DX
           YOLD = YOLD + RINC * DY
           CALL REVCT(XOLD,YOLD,XD1,YD1)
           S1    = SKD1
           IDASH = 1
           GO TO 1
         END IF
       END IF
       XOLD = X1
       YOLD = Y1
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C---
C---
C-----------------------------------------------------------------------
       SUBROUTINE DPCIR(ID)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
C---
C---
 1     CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
       IF (DIS.LE.SKCIR) THEN
         SKCIR = SKCIR - DIS
       ELSE
         RINC  = SKCIR / DIS
         DX    = XNEW - XOLD
         DY    = YNEW - YOLD
         XOLD = XOLD + DX * RINC
         YOLD = YOLD + DY * RINC
         IX    = XOLD
         IY    = YOLD
         IF (ID.EQ.3) CALL HCIR
         IF (ID.EQ.4) CALL SCIR
         SKCIR = DSCIR
         GO TO 1
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C      DISPLAYS CURVES USING DOTTED LINES
C-----------------------------------------------------------------------
       SUBROUTINE DPDOLN
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DICIR,DSSYBL
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
C---
C---
       X1 = XOLD
       Y1 = YOLD
 1     CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
       CALL PLCMDS(37)
       IF (DIS.LE.SKDOT) THEN
         SKDOT = SKDOT - DIS
       ELSE
         RINC  = SKDOT / DIS
         DX    = XNEW - XOLD
         DY    = YNEW - YOLD
         XOLD = XOLD + DX * RINC
         YOLD = YOLD + DY * RINC
         CALL REVCT(XOLD,YOLD,X,Y)
         CALL CLIP(X,Y,1)
         SKDOT = DSDOT
         GO TO 1
       END IF
       XOLD = X1
       YOLD = Y1
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C       DISPLAYS CURVES USING SYMBOLS ( OPEN SQUARE,
C       SOLID SQUARE, OPEN TRIANGLE, SOLID TRIANGLE, OPEN CIRCLE,
C       SOLID CIRCLE, PLUS SIGN)
C-----------------------------------------------------------------------
       SUBROUTINE CLSYBL(ID,MBACK)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
C---
C---
       MBACK = 0
 1     CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
       IF (DIS.LE.SKSYBL) THEN
         SKSYBL = SKSYBL - DIS
       ELSE
         MBACK = 1
         RINC  = SKSYBL / DIS
         DX    = XNEW - XOLD
         DY    = YNEW - YOLD
         XOLD = XOLD + DX * RINC
         YOLD = YOLD + DY * RINC
         IX    = XOLD
         IY    = YOLD
         CALL DPSYBL(ID)
         SKSYBL = DSSYBL
         GO TO 1
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C      READS 80 CHARACTER FROM INTERACTIVE INPUT,
C      IF END OF FILE THEN RETURN BLANKS
C-----------------------------------------------------------------------
       SUBROUTINE READS8(ISTR)
       CHARACTER*80 ISTR
C---
C---
       ISTR = ' '
       READ(5,2,END=1) ISTR
 1     RETURN
 2     FORMAT(A80)
       END
C-----------------------------------------------------------------------
C      READS 10 CHARACTERS FROM INTERACTIVE INPUT
C      IF END OF FILE THEN RETURN BLANKS
C-----------------------------------------------------------------------
       SUBROUTINE READS1(ISTR)
       CHARACTER*10 ISTR
C---
C---
       ISTR = ' '
       READ(5,2,END=1) ISTR
 1     RETURN
 2     FORMAT(A10)
       END
C-----------------------------------------------------------------------
C      READ TWO CHARACTERS FROM INPUT
C-----------------------------------------------------------------------
       SUBROUTINE READ2(CH1,CH2)
       CHARACTER*1 CH1,CH2
C---
C---
       CH1 = ' '
       CH2 = ' '
       READ(5,2,END=1) CH1,CH2
 1     RETURN
 2     FORMAT(2A1)
       END
C-----------------------------------------------------------------------
C      READS SINGLE CHARACTER
C-----------------------------------------------------------------------
       SUBROUTINE READ1(CH)
       CHARACTER*1 CH
C---
C---
       CH = ' '
       READ(5,2,END=1) CH
 1     RETURN
 2     FORMAT(A1)
       END
C-----------------------------------------------------------------------
C      DEFINES RELATIVE WINDOW ON SCREEN
C-----------------------------------------------------------------------
       SUBROUTINE DFSCR(XMIN,XMAX,YMIN,YMAX)
C---
C---
       C     = 0
       XMIN  = COVTX(C+150)
       XMAX  = COVTX(C+950)
       YMIN  = COVTY(C+150)
       YMAX  = COVTY(C+650)
       RETURN
       END
C-----------------------------------------------------------------------
C      DEFINED INTERVAL OF DOTTED LINE, DASHED LINE, CIR, SYMBOLS,
C      DISTANCE, AND RADIUS OF SYMBOLS
C-----------------------------------------------------------------------
       SUBROUTINE DFITVL
       COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
       COMMON /UTDSH/ UDSH,UDSHS,ULD,USD,RIDS1
       COMMON /RADI/  RASY,RATY,RADI,RAINC
       COMMON /SPNUM/ SP1,SP2,SP3,SP4
C---
C---
       C       = 0
       RIDASH  = 80
       RIDOT   = 110
       RICIR   = 50
       RISYBL  = 50
       RIDIS   = 500
       RIDSHS  = 60
       RIDS1   = 50
       UDSH    =   0.5
       UDSHS   =   0.4
       ULD     =   0.3
       USD     =   0.2
       RASY    = COVTX(C+7)
       RATY    = COVTX(C+5)
       RAINC   = COVTX(C+1)
       SP1     = COVTX(C+150)
       SP2     = COVTX(C+10)
       SP3     = COVTY(C+150)
       SP4     = COVTY(C+10)
       RETURN
       END
C-----------------------------------------------------------------------
C      SUBROUTINE BDS EXECUTES THE BD, BD0 AND BD1 COMMANDS.
C-----------------------------------------------------------------------
       SUBROUTINE BDS(ISTR,II,SAVE,ICDS,ICD,SYMBOL)
       REAL Z(10),IX,IY,MINSX,MAXSX,MINSY,MAXSY,PNTS(1999,4)
       CHARACTER*80 OSTR,ISTR
       CHARACTER*1 CHAR,MP(81,251)
       LOGICAL BRNCH,POINT,DFT,USR,PLTR,TIT,AXLB,QLBS,GRIDS,DP
       LOGICAL TOP,BOTTOM,SAVE,SYMBOL,FIRST,LNKDSH,IBD2
       LOGICAL SOLID,DSH,DSHS,LHCIR,LSYBL,LDSH,LDOT
       INTEGER GRNUM,SPLPT,SPTNM,ERROR
       INTEGER PTNUM,PINDX,PRESGN,PNTCNT,NVX(2),SYMBL(10)
       INTEGER ICDS(4),ITP(10),XAXIS,YAXIS,ICD(4)
C---
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /PLOTT/ PLTR,POINT
       COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
       COMMON /TWOPT/ XNEW1,YNEW1,XOLD1,YOLD1
       COMMON /DASHC/ CDASH,DSKDSH,CDASHS,DKDSHS,CD1,CSK1,ITYPE
       COMMON /LINKDA/ LNKDSH
       COMMON /UTDSH/ UDSH,UDSHS,ULD,USD,RIDS1
       COMMON /BD2/ IBD2
       COMMON /BDSY/ ITP,SYMBL
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
       COMMON /SYBL/ SOLID,DSH,DSHS,LHCIR,LSYBL,LDSH,LDOT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
       COMMON /DSH1/ SKD1,DSH1,DSH2,S1,D1,D2,IDASH
       COMMON /SPNUM/ SP1,SP2,SP3,SP4
C---
C---
       PINDX  = 0
       PNTCNT = 0
       PRESGN = 0
       ID     = 0
       IDS    = 0
       XAXIS  = NVX(1)
       YAXIS  = NVX(2)
       NMX    = MAX(XAXIS,YAXIS)
       IF(SAVE) GO TO 5
       REWIND 17
       CHAR=ISTR(II:II)
 30    IF(CHAR.EQ.'0') THEN
C---                                    *SCAN UNIT 7 FOR LARGEST
C---                                    *AND SMALLEST VALUES OF X AND Y.
C---                                    *STORE IN /PLVARS/
         CALL SCANBD(ERROR)
         IF(ERROR.NE.0) RETURN
         IBD2 = .TRUE.
       ELSE IF (CHAR.EQ.'1') THEN
C---                                    *READ PLOT LIMITS FROM UNIT 7
         READ(17,*,ERR=70)GRNUM,XMIN,XMAX,YMIN,YMAX
         IBD2 = .TRUE.
       ELSE IF (CHAR.EQ.'2') THEN
         IF (.NOT.IBD2) THEN
           CALL SCANBD(ERROR)
           IF (ERROR.NE.0) RETURN
           IBD2 = .TRUE.
         END IF
       ELSE
         IF (USR) THEN
           OSTR = ' LIMITS'
         ELSE
           OSTR = '  ENTER XMIN,XMAX,YMIN,YMAX'
         END IF
         WRITE(IWRITE,2) OSTR
C---                                    *READ USER LIMITS
         READ(5,*,ERR=30)XMIN,XMAX,YMIN,YMAX
         IBD2 = .TRUE.
       END IF
       CALL SINGLE
       IF(.NOT.DFT) THEN
C---                                    *QUERY BRANCH OR POINT
         CALL RLABL
       END IF
C---                                    *SET PLOT PAGE

 5     CALL SPAGE
C---                                    *INITIALIZE MAP
C---                                    *AND POINT SAVING ARRAY
       CALL INARRS(MP,PNTS)
       CALL INITIN
C---                                    *COMPUTE THE DISTANCE
C---                                    *DEFINE DASHES, ETC.

       CALL DISXY(MAXSX,MAXSY,MINSX,MINSY,DIS1)
       TOOCLS = DIS1 / RIDIS
C---
       REWIND 17
 10    READ(17,*,ERR=70,END=90) GRNUM
       IF (GRNUM.EQ.0) GO TO 10
       BACKSPACE 17
C---
       FIRST  = .TRUE.
C---
 20    READ(17,*,END=90,ERR=80)GRNUM,PTNUM,SPLPT,SPTNM,(Z(I),I=1,NMX)
       IF (GRNUM.EQ.0) GOTO 10
C---
C---                                    *NEW CURVE
       IF (ABS(PTNUM).EQ.1) THEN
         CALL DFNCUR
         FIRST = .TRUE.
       END IF
C---
       X = Z(XAXIS)
       Y = Z(YAXIS)
       CALL PLCMDS(7)
       XNEW = IX
       YNEW = IY
       XNEW1 = X
       YNEW1 = Y
       IF (BRNCH.OR.POINT) THEN
         IF (FIRST) THEN
           CALL MAP(MP,XNEW,YNEW,XNEW,YNEW)
         ELSE
           CALL MAP(MP,XNEW,YNEW,XOLD,YOLD)
         END IF
       END IF
       IF(X.GE.XMIN.AND.X.LE.XMAX.AND.Y.GE.YMIN.AND.Y.LE.YMAX)THEN
         IF (POINT.AND.SPTNM.NE.0) THEN
C---                                    *SAVE SPECIAL POINT
           PINDX         = PINDX + 1
           PNTS(PINDX,1) = IX
           PNTS(PINDX,2) = IY
           PNTS(PINDX,3) = SPTNM
           PNTS(PINDX,4) = 1
         END IF
         IF (BRNCH.AND.GRNUM.NE.PRESGN) THEN
C---                                    *ON NEW BRANCH ?
           PNTCNT = PNTCNT + 1
         END IF
C---
         IF (PNTCNT.EQ.10.AND.GRNUM.NE.PRESGN) THEN
C---                                    *LABELLED POINT
           PRESGN        = GRNUM
           PINDX         = PINDX + 1
           PNTS(PINDX,1) = IX
           PNTS(PINDX,2) = IY
           PNTS(PINDX,3) = GRNUM
           PNTS(PINDX,4) = 4
           PNTCNT        = 0
         END IF
       END IF
C---
C---                                    *ASSIGN TYPE
       IF (GRNUM.GT.0.AND.PTNUM.GT.0) THEN
         ID  = ICD(1)
         IDS = ICDS(1)
       ELSE IF (GRNUM.GT.0.AND.PTNUM.LT.0) THEN
         ID  = ICD(2)
         IDS = ICDS(2)
       ELSE IF (GRNUM.LT.0.AND.PTNUM.GT.0) THEN
         ID  = ICD(3)
         IDS = ICDS(3)
       ELSE IF (GRNUM.LT.0.AND.PTNUM.LT.0) THEN
         ID  = ICD(4)
         IDS = ICDS(4)
       END IF
C---
       IF (.NOT.DP) ID = 2
C---
       IF (ID.NE.1)  DSH   = .FALSE.
       IF (ID.NE.2)  SOLID = .FALSE.
       IF (.NOT.(ID.EQ.3.OR.ID.EQ.4)) LHCIR = .FALSE.
       IF (ID.NE.5)  LDOT  = .FALSE.
       IF (ID.NE.6)  DSHS  = .FALSE.
       IF (ID.NE.7)  LDSH  = .FALSE.
       IF (IDS.EQ.0) LSYBL = .FALSE.
C---
       IF (.NOT.FIRST) THEN
         CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
         IF (TOOCLS.GT.DIS) THEN
           IF (SYMBOL) THEN
             IF (IDS.EQ.0.AND.ID.NE.3.AND.ID.NE.4) THEN
               RADI = RASY
               IX    = XOLD
               IY    = YOLD
               CALL PLCMDS(6)
               IX    = XNEW
               IY    = YNEW
               CALL DRAW
             END IF
             IF(MOD(IABS(SPLPT),10).EQ.9.AND.IABS(PTNUM).NE.1)THEN
               GOTO 10
             ELSE
               GOTO 20
             ENDIF
           END IF
         END IF
       END IF
C---
       CALL SYMBLS(XNEW1,YNEW1,ID,IDS,FIRST,SYMBOL)
C---
       FIRST = .FALSE.
       XOLD = XNEW
       YOLD = YNEW
       XOLD1 = XNEW1
       YOLD1 = YNEW1
       IF(MOD(IABS(SPLPT),10).EQ.9.AND.IABS(PTNUM).NE.1)THEN
         GOTO 10
       ELSE
        GOTO 20
       ENDIF
C---
70     IF(GRNUM.EQ.0)GO TO 10
       ERROR = 8
       RETURN
C---
80     ERROR = 6
       GO TO 10
90     CONTINUE
C---
       IF (POINT.OR.BRNCH) THEN
         CALL LGRPHS(MP,PNTS)
       END IF
C---
       CALL PLCMDS(9)
C---
       IF(PLTR) THEN
         CALL PLCHDW(2)
       END IF
2      FORMAT(A80)
       RETURN
       END
C-----------------------------------------------------------------------
C      DEFINE INTERVAL, LENGTH, DISTANCE
C-----------------------------------------------------------------------
       SUBROUTINE INITIN
       INTEGER NVX(2),SPLPT,COUNTC
       REAL XS(4),YS(4),XYPT(4,2),IX,IY,MINSX,MAXSX,MINSY,MAXSY
C---
       COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
       COMMON /DASHC/ CDASH,DSKDSH,CDASHS,DKDSHS,CD1,CSK1,ITYPE
       COMMON /LRBT/  XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
       COMMON /UTDSH/ UDSH,UDSHS,ULD,USD,RIDS1
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /DSH1/ SKD1,DSH1,DSH2,S1,D1,D2,IDASH
C---
C---                                    *DEFINE SCREEN
C---
       XLEFT  = XMIN
       XRIGHT = XMAX
       YBOTTM = YMIN
       YTOP   = YMAX
C---                                    *DEFINE LENGTH
C---                                    *DASH, DOT, CIR, SYMBOL
       CALL DFSCR(XI,XA,YI,YA)
       CALL DISXY(XA,YA,XI,YI,DIS)
       DSDSH  = DIS / RIDASH
       DSDOT  = DIS / RIDOT
       DSCIR  = DIS / RICIR
       DSSYBL = DIS / RISYBL
       CDASHS = DIS / RIDSHS
       DSH1   = DIS / RIDS1
C---
C---
       SKDSH  = DSDSH * (1.0 - UDSH)
       DSDSH  = DSDSH * UDSH
       DKDSHS = CDASHS * (1.0 - UDSHS) / 2.0
       CDASHS = CDASHS * UDSHS
       SKD1   = DSH1   * (1.0 - ULD - USD) / 2.0
       DSH2   = DSH1   * USD
       DSH1   = DSH1   * ULD
       CALL DFNCUR
       RETURN
       END
C-----------------------------------------------------------------------
C      DEFINE INTERVAL FOR NEW CURVES
C-----------------------------------------------------------------------
       SUBROUTINE DFNCUR
       LOGICAL SOLID,DSH,DSHS,LHCIR,LSYBL,LDSH,LNKDSH,LDOT
C---
       COMMON /SKDS/ SKDSH,SKDOT,SKCIR,SKSYBL,DSDSH,DSDOT,DSCIR,DSSYBL
       COMMON /DASHC/ CDASH,DSKDSH,CDASHS,DKDSHS,CD1,CSK1,ITYPE
       COMMON /LINKDA/ LNKDSH
       COMMON /DSH1/ SKD1,DSH1,DSH2,S1,D1,D2,IDASH
       COMMON /SYBL/ SOLID,DSH,DSHS,LHCIR,LSYBL,LDSH,LDOT
C---
C---
       SOLID  = .FALSE.
       DSH    = .FALSE.
       DSHS   = .FALSE.
       LHCIR  = .FALSE.
       LSYBL  = .FALSE.
       LDSH   = .FALSE.
       LDOT   = .FALSE.
       LNKDSH = .TRUE.
C---
       CDASH  = DSDSH
       DSKDSH = SKDSH
       SKDOT  = DSDOT
       SKCIR  = DSCIR
       SKSYBL = DSSYBL
       ITYPE  = 1
       CD1    = CDASHS
       CSK1   = DKDSHS
       IDASH  = 1
       S1     = SKD1
       D1     = DSH1
       D2     = DSH2
       RETURN
       END
C-----------------------------------------------------------------------
C      INITIALIZE OF ICDS EQUAL TO ZEROS
C-----------------------------------------------------------------------
        SUBROUTINE ICDS0(ICDS)
        INTEGER ICDS(4)
C---
C---
        DO 1 I=1,4
          ICDS(I) = 0
 1      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C    THE FOLLOWING SUBROUTINE SCANS UNIT 7 TO FIND THE
C    SMALLEST AND LARGEST VALUES OF X AND Y IN THE FILE. THESE
C    ARE RETURNED TO SUBROUTINE BDS AS XMIN, XMAX, YMIN, YMAX
C    IN COMMON AREA /PLVARS/.
C-----------------------------------------------------------------------
       SUBROUTINE SCANBD(ERROR)
       REAL Z(50),IX,IY,MINSX,MAXSX,MINSY,MAXSY
       INTEGER GRNUM,PTNUM,SPLPT,SPTNUM,ERROR,NVX(2),XAXIS,YAXIS
       LOGICAL FPASS
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
C---
C---
       XAXIS = NVX(1)
       YAXIS = NVX(2)
       NMX = MAX(XAXIS,YAXIS)
       ERROR = 0
       FPASS = .TRUE.
 1     READ(17,*,ERR=4,END=5)GRNUM
       IF(GRNUM.EQ.0)GO TO 1
       BACKSPACE 17
 2     READ(17,*,ERR=3,END=5)GRNUM,PTNUM,SPLPT
     + ,SPTNUM,(Z(I),I=1,NMX)
         X = Z(XAXIS)
         Y = Z(YAXIS)
       IF (GRNUM.EQ.0) GOTO 1
       IF(FPASS) THEN
         XMIN  = X
         XMAX  = X
         YMIN  = Y
         YMAX  = Y
         FPASS = .FALSE.
       ELSE
         IF(X.LT.XMIN)XMIN = X
         IF(X.GT.XMAX)XMAX = X
         IF(Y.LT.YMIN)YMIN = Y
         IF(Y.GT.YMAX)YMAX = Y
       END IF
       IF(MOD(IABS(SPLPT),10).EQ.9.AND.IABS(PTNUM).NE.1)GOTO 1
       GO TO 2
 3     IF (GRNUM.EQ.0) GO TO 1
 4     ERROR = 11
 5     RETURN
       END
C-----------------------------------------------------------------------
C   REDEFINES A PROPER WINDOW SIZE FOR PLOTTING
C   A SINGLE POINT, A HORIZONTAL OR A VERTICAL LINE
C-----------------------------------------------------------------------
       SUBROUTINE SINGLE
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
C---
C---
       CALL REDFMM(XMIN,XMAX)
       CALL REDFMM(YMIN,YMAX)
       RETURN
       END
C-----------------------------------------------------------------------
C      REDEFINES MINIMUM AND MAXIMUM OF X, Y OR Z
C-----------------------------------------------------------------------
       SUBROUTINE REDFMM(FMIN,FMAX)
C---
C---
       IF (FMIN.EQ.FMAX) THEN
         FMIN = FMIN * 0.95
         FMAX = FMAX * 1.05
       END IF
C---                                    *IF MIN AND MAX ZERO
       IF (FMIN.EQ.0.AND.FMAX.EQ.0) THEN
         FMIN = -0.1
         FMAX =  0.1
       END IF
C---                                    *CHECK FMIN
       IF (FMIN.GT.FMAX) THEN
         S1   = FMIN
         FMIN = FMAX
         FMAX = S1
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C   SETS UP THE BASIC OUTLINE OF THE GRAPH
C   ON THE SCREEN.  THIS INCLUDES AXIS LINES, AXIS LABELS,
C   GRAPH TITLE AND TYPE OF GRID LINE WANTED.  IT CALLS
C   SUBROUTINE SAXIS TO LABEL AND PUT OUT GRID LINES AND
C   INFORMATION PASSED BACK IN /PLVARS/ IS USED TO SET THE
C   VIRTUAL PAGE AND SCREEN WINDOW.
C-----------------------------------------------------------------------
       SUBROUTINE SPAGE
       CHARACTER*80 TTITLE,BTITLE,XLAB,YLAB,ADJ
       CHARACTER*1 AXIS
       INTEGER NVX(2),SPLPT
       LOGICAL DFT,USR,PLTR,BRNCH,POINT,TIT,AXLB,QLBS,GRIDS,DP
       LOGICAL TOP,BOTTOM,QSCOM
       REAL NAVRX,NAVRY,IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /LABLS/TTITLE,BTITLE,XLAB,YLAB
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /QSC/ QSCOM
       COMMON /PLOTT/ PLTR,POINT
       COMMON /PTITLE/ ADJ,NCHR
C---
C---
       IF((.NOT.DFT).AND.(IWRITE.EQ.ITERM)) THEN
         CALL ENTGRD(2)
       END IF
       CALL PLCMDS(1)
       CALL PLCHDW(3)
       IF(PLTR) THEN
         CALL PLCHDW(1)
       END IF
C---                                    *HEAVY AXIS LINES
       C  = 0
       IX = MINSX - COVTX(C+2)
       IY = MINSY - COVTY(C+15)
       CALL PLCMDS(2)
       IY = MAXSY
       CALL PLCMDS(3)
       IX = MINSX
       CALL PLCMDS(3)
       IY = MINSY
       CALL PLCMDS(3)
       IX = MINSX - COVTX(C+15)
       CALL PLCMDS(2)
       IX = MAXSX
       CALL PLCMDS(3)
       IY = MINSY - COVTY(C+2)
       CALL PLCMDS(3)
       IX = MINSX - COVTX(C+15)
       CALL PLCMDS(3)
C---
C---
       IF (QSCOM) THEN
         IX = MINSX
         IY = MAXSY
         CALL PLCMDS(2)
         IX = MAXSX
         CALL PLCMDS(3)
         IY = MINSY
         CALL PLCMDS(3)
       END IF
C---
C---                                    *TOP TITLES
       IF (TIT) THEN
         IF(TOP.AND.TTITLE.NE.' ') THEN
           NAVRX = (MAXSX+MINSX)/2
           NCHR  = 60
           IX    = NAVRX-COVTX(C+370)
           IY    = MAXSY+COVTY(C+60)
           CALL ADJT(TTITLE,NCHR,ADJ)
           CALL PLCMDS(25)
           CALL PLCTNM(1)
           CALL PLCMDS(20)
         END IF
C---
C---
C---                                    *BOTTOM TITLES
         IF(BOTTOM.AND.BTITLE.NE.' ') THEN
           NAVRX = (MAXSX+MINSX)/2
           NCHR  = 60
           IX    = NAVRX-COVTX(C+370)
           IY    = MINSY-COVTY(C+100)
           CALL ADJT(BTITLE,NCHR,ADJ)
           CALL PLCMDS(26)
           CALL PLCTNM(1)
           CALL PLCMDS(20)
         END IF
       END IF
C---
C---
C---                                    *LABELS ON X-AXIS
C---
       IF (AXLB) THEN
         IF (XLAB.NE.' ') THEN
           NAVRX = (MAXSX+MINSX)/2
           NCHR  = 30
           IX    = NAVRX+COVTX(C+30)
           IY    = MINSY-COVTY(C+70)
           CALL ADJX(XLAB,NCHR,ADJ)
           CALL PLCMDS(27)
           CALL PLCTNM(1)
           CALL PLCMDS(20)
         END IF
C---
C---                                    *LABELS ON Y-AXIS
C---
         IF (YLAB.NE.' ') THEN
           NAVRY = (MAXSY+MINSY)/2
           NCHR  = 30
           IX    = MINSX-COVTX(C+75)
           IY    = NAVRY+COVTY(C+280)
           CALL ADJY(YLAB,NCHR,ADJ)
           CALL PLCMDS(27)
           CALL PLCTNM(1)
           CALL PLCMDS(20)
         END IF
       END IF
C---
       IF (.NOT.QSCOM) THEN
C---                                    *X AXIS
         AXIS = 'X'
         CALL SAXIS(XMIN,XMAX,AXIS,XINCR)
         CALL XAXIS(XINCR)
C---                                    *Y AXIS
         AXIS = 'Y'
         CALL SAXIS(YMIN,YMAX,AXIS,YINCR)
         CALL YAXIS(YINCR)
       END IF
       CALL DFDIST
       RETURN
       END
C-----------------------------------------------------------------------
C   ELIMINATES ANY BLANK
C   CHARACTERS AT THE BEGINNING OF A TITLE.
C-----------------------------------------------------------------------
        SUBROUTINE RESLAB(LAB)
        CHARACTER*(*) LAB
        LOGICAL BLANK
C---
C---
        BLANK = .TRUE.
        K     = 0
	N     = LEN(LAB)
        DO 1 J=1,N
          IF (BLANK) K = K + 1
          IF (LAB(J:J).NE.' ') BLANK = .FALSE.
 1      CONTINUE
        L = 0
        DO 2 J=K,N
        L = L + 1
        LAB(L:L) = LAB(J:J)
 2      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C   DECOMPOSES A STRING OF
C   CHARACTERS FOR CENTERING IT ON THE GRAPH.
C-----------------------------------------------------------------------
        SUBROUTINE ADJT(LAB,NCHR,ADJ)
        CHARACTER*80 ADJ,LAB
C---
C---
        CALL RESLAB(LAB)
        ADJ = ' '
        J = 0
 2      J = J + 1
        IF(J.GT.NCHR) GO TO 3
        IF(LAB(J:J).NE.' ')      GO TO 2
        IF(LAB(J+1:J+1).NE.' ')  GO TO 2
 3      J = J - 1
        IDF = (NCHR-J)/2
        DO 4 IP=1,J
          IDF      = IDF + 1
 4        ADJ(IDF:IDF) = LAB(IP:IP)
        RETURN
        END
C-----------------------------------------------------------------------
C   DECOMPOSES A CHARACTER STRING FOR RIGHT
C   ADJUSTMENT ON THE HORIZONTAL AXIS.
C-----------------------------------------------------------------------
        SUBROUTINE ADJX(LAB,NCHR,ADJ)
        CHARACTER*80 ADJ,LAB
C---
C---
        CALL RESLAB(LAB)
        ADJ = ' '
        J = 0
 2      J = J+1
        IF(J.GT.NCHR) GO TO 3
        IF(LAB(J:J).NE.' ')      GO TO 2
        IF(LAB(J+1:J+1).NE.' ')  GO TO 2
 3      J   = J-1
        IDF = NCHR - J
        DO 4 IP=1,J
          IDF      = IDF + 1
 4        ADJ(IDF:IDF) = LAB(IP:IP)
        RETURN
        END
C-----------------------------------------------------------------------
C   DECOMPOSES A CHARACTER STRING FOR LEFT
C   ADJUSTMENT ON THE VERTICAL AXIS.
C-----------------------------------------------------------------------
        SUBROUTINE ADJY(LAB,NCHR,ADJ)
        CHARACTER*80 ADJ,LAB
C---
C---
        CALL RESLAB(LAB)
        ADJ = ' '
        J = 0
 2      J = J+1
        IF(J.GT.NCHR) GO TO 3
        IF(LAB(J:J).NE.' ')      GO TO 2
        IF(LAB(J+1:J+1).NE.' ')  GO TO 2
 3      J = J - 1
        DO 4 IP=1,J
 4        ADJ(IP:IP) = LAB(IP:IP)
        RETURN
        END
C-----------------------------------------------------------------------
C   USED TO ENTER TITLES AND AXIS LABELS.
C   WITH A SERIAL USAGE OPTION IT ALSO ASKS IF TITLES, AXIS LABELS
C   OR GRIDS LINES ARE WANTED.
C-----------------------------------------------------------------------
       SUBROUTINE ENTGRD(CALR)
       INTEGER CALR
       CHARACTER*80 OSTR,TTITLE*80,BTITLE*80,XLAB*80,YLAB*80
       CHARACTER*1 CHAR,CHAR1
       LOGICAL DFT,USR,PLTR,BRNCH,POINT,TOP,BOTTOM,TIT
       LOGICAL AXLB,QLBS,RTIT,RAXLB,GRIDS,DP,QSCOM
       COMMON /LABLS/TTITLE,BTITLE,XLAB,YLAB
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /QSC/ QSCOM
       COMMON /PLOTT/ PLTR,POINT
C---
C---
C---                                    *<ST> USER OPTION*
       IF(CALR.EQ.1) THEN
         RAXLB   = .TRUE.
         RTIT    = .TRUE.
         TOP     = .TRUE.
         BOTTOM  = .TRUE.
         GO TO 20
       END IF
       TIT    = .FALSE.
       TOP    = .FALSE.
       BOTTOM = .FALSE.
       AXLB   = .FALSE.
       RTIT   = .FALSE.
       RAXLB  = .FALSE.
       GRIDS  = .FALSE.
C---                                    *GRID LINES ?
       IF (.NOT.QSCOM) THEN
         IF (USR) THEN
           OSTR = ' GL'
         ELSE
          OSTR = ' GRID LINES ?  (Y OR N)'
         END IF
         WRITE(IWRITE,1)OSTR
         CALL READ1(CHAR)
         IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
           GRIDS = .TRUE.
         ELSE
           GRIDS = .FALSE.
         END IF
       END IF
C---                                    *SHORT TITLE AND AXES ?
       IF (USR) THEN
         OSTR  = ' T - C'
         WRITE(IWRITE,1)OSTR
         CALL READ2(CHAR,CHAR1)
         IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
           TIT = .TRUE.
           TOP = .TRUE.
           BOTTOM = .TRUE.
           IF ((CHAR1.EQ.'Y').OR.(CHAR1.EQ.'y')) THEN
             RTIT   = .TRUE.
             TOP    = .TRUE.
             BOTTOM = .TRUE.
           END IF
         END IF
         OSTR  = ' A - C'
         WRITE(IWRITE,1)OSTR
         CALL READ2(CHAR,CHAR1)
         IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
           AXLB = .TRUE.
           IF ((CHAR1.EQ.'Y').OR.(CHAR1.EQ.'y')) THEN
             RAXLB = .TRUE.
           END IF
         END IF
       ELSE
C---                                    *LONG TITLE AND AXES ?
         OSTR = ' TITLE ?  (Y OR N)'
         WRITE(IWRITE,1)OSTR
         CALL READ1(CHAR)
         IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
           TIT = .TRUE.
           TOP = .TRUE.
           BOTTOM = .TRUE.
             IF(ICT.EQ.0) THEN
               ICT    = 1
               RTIT   = .TRUE.
               TOP    = .TRUE.
               BOTTOM = .TRUE.
             ELSE
           OSTR =  ' CHANGE TITLES ?  (Y OR N)'
           WRITE(IWRITE,1)OSTR
           CALL READ1(CHAR)
           IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
               RTIT   = .TRUE.
               TOP    = .TRUE.
               BOTTOM = .TRUE.
             END IF
           END IF
         END IF
         OSTR = ' AXES LABELS ?  (Y OR N)'
         WRITE(IWRITE,1)OSTR
         CALL READ1(CHAR)
         IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
           AXLB = .TRUE.
            IF(ICL.EQ.0) THEN
               ICL    = 1
               RAXLB  = .TRUE.
            ELSE
           OSTR = ' CHANGE AXES LABELS ?  (Y OR N)'
           WRITE(IWRITE,1) OSTR
           CALL READ1(CHAR)
           IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
             RAXLB = .TRUE.
           END IF
          END IF
         END IF
       END IF
20     IF (RAXLB) THEN
C---                                    *READ X AXIS LABEL
         IF (.NOT.USR) THEN
           OSTR = ' ENTER X AXIS LABEL BETWEEN THE QUOTES'
           WRITE(IWRITE,1)OSTR
         END IF
         OSTR = ' "                              "'
         WRITE(IWRITE,1)OSTR
         CALL READS8(XLAB)
C---                                    *READ Y AXIS LABEL
         IF (.NOT.USR) THEN
           OSTR = ' ENTER Y AXIS LABEL BETWEEN THE QUOTES'
           WRITE(IWRITE,1)OSTR
         END IF
         OSTR = ' "                              "'
         WRITE(IWRITE,1)OSTR
         CALL READS8(YLAB)
       END IF
       IF (RTIT) THEN
C---                                    *READ TOP AND BOTTOM TI
         IF (TOP) THEN
           IF (.NOT.USR) THEN
             OSTR = ' ENTER TOP TITLE BETWEEN THE QUOTES'
             WRITE(IWRITE,1)OSTR
           END IF
         WRITE(IWRITE,15)
         CALL READS8(TTITLE)
         END IF
         IF (BOTTOM) THEN
           IF (.NOT.USR) THEN
             OSTR = ' ENTER BOTTOM TITLE BETWEEN THE QUOTES'
             WRITE(IWRITE,1)OSTR
           END IF
         WRITE(IWRITE,15)
         CALL READS8(BTITLE)
        END IF
       END IF
1      FORMAT(A80)
15     FORMAT(1X,'"                              '
     +   ,'                              "')
       RETURN
       END
C-----------------------------------------------------------------------
C   WRITES THE AXIS LINES AND FORMATS PARAMETERS
C   TO OUTPUT THE GRID LINES AND GRID LABELS.
C-----------------------------------------------------------------------
       SUBROUTINE SAXIS(MIN,MAX,AXIS,INCR)
       CHARACTER*1 AXIS
       REAL INCR,MIN,MAX
C---
C---
       DIFF = MAX - MIN
       TEMP = LOG10(ABS(DIFF))
       IF (TEMP.GT.1.0) THEN
         EXPON = AINT(TEMP)
       ELSE
         IF (TEMP.LE.0.0) THEN
           EXPON = AINT(TEMP) - 1
         ELSE
           EXPON = 0
         END IF
       END IF
       DIFF = DIFF / (10 ** EXPON)
C---                                    *INCR IS THE DIFFERENCE
C---                                    *BETWEEN GRID AND
C---                                    *LABEL VALUES
       INCR = 1
       IF (DIFF.LE.2) THEN
         INCR = 0.25
       ELSE
        IF (DIFF.LE.4) THEN
           INCR = 0.5
         END IF
       END IF
       INCR   = INCR * (10 ** EXPON)
C---                                    *MIN IS THE SMALLEST AXIS
C---                                    *LABEL VALUE
       FACTOR = 1 / INCR
       IF (MIN.GE.0) THEN
         MIN = AINT(MIN * FACTOR * 1.00001) / FACTOR
       ELSE
         MIN = AINT(MIN * FACTOR - 0.99999) / FACTOR
       ENDIF
C---                                    *MAX IS THE LARGEST AXIS
C---                                    *LABEL VALUE
       IF (MAX.GE.0) THEN
         MAX = AINT(MAX * FACTOR + 0.99999) / FACTOR
       ELSE
         MAX = AINT(MAX * FACTOR * 0.99999) / FACTOR
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C    ASKS IF BRANCH AND POINT
C    LABELLING IS REQUIRED. IT SETS THE BRANCH AND POINT FLAGS
C    IN THE COMMON AREA /UOPTS/.
C-----------------------------------------------------------------------
       SUBROUTINE RLABL
       LOGICAL BRNCH,POINT,USR,DFT,TIT,AXLB,QLBS,GRIDS,PLTR,DP
       LOGICAL TOP,BOTTOM
       CHARACTER*1 OSTR*80,CHAR
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /PLOTT/ PLTR,POINT
C---
C---
       POINT = .FALSE.
       BRNCH = .FALSE.
C---                                    *SHORT QUESTION*
       IF (USR) THEN
         OSTR = ' LAB'
         WRITE(IWRITE,2)OSTR
       ELSE
C---                                    *LONG QUESTION*
         OSTR = ' SOLUTION LABELS ?  ( <Y> OR <N> ) '
         WRITE(IWRITE,2)OSTR
       END IF
       CALL READ1(CHAR)
C---
       IF ((CHAR.EQ.'Y').OR.(CHAR.EQ.'y')) THEN
C---                                    *SET POINT LABELLING FLAG
         POINT = .TRUE.
       END IF
 2     FORMAT(A80)
       RETURN
       END
C-----------------------------------------------------------------------
C   SUBROUTINE CONVT CONVERTS A CHARACTER STRING OF DIGITS SEPARATED
C   BY BLANKS TO AN ARRAY CONTAINING THE EQUIVALENT INTEGERS (PARRY).
C   IT IS USED TO DECODE THE POINTS INPUT IN THE "2D" COMMAND.
C-----------------------------------------------------------------------
       SUBROUTINE CONVT(ISTR,II,NUM,PARRY,VALID)
       CHARACTER*1 CHAR,BLANK,ZERO,NINE,COMMA
       CHARACTER*80 ISTR,OSTR
       INTEGER TEMP,TOTAL,PARRY(1999),POS
       LOGICAL VALID,LINT
       COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
       ZERO  = '0'
       NINE  = '9'
       BLANK = ' '
       COMMA = ','
       POS   = II
C---                                    *FIRST CHARACTER POSITION
       TOTAL = 0
       VALID = .TRUE.
       LINT  = .FALSE.
C---                                    *INITIALIZE PARRY TO ZERO
       DO 1 I = 1,1999
         PARRY(I) = 0
 1     CONTINUE
       INDX = 0
 2     IF(POS.LE.80)THEN
         CHAR = ISTR(POS:POS)
         POS  = POS+1
         IF(CHAR.GE.ZERO.AND.CHAR.LE.NINE)THEN

C---                                    *CONVERT
           TEMP = ICHAR(CHAR)-ICHAR(ZERO)
           IF(LINT)THEN
             TOTAL = TOTAL*10+TEMP
           ELSE
C---                                    *STARTING NEW INTEGER
             TOTAL = TEMP
             LINT = .TRUE.
           END IF
         ELSE
           IF(LINT)THEN
             INDX        = INDX+1
             PARRY(INDX) = TOTAL
             LINT        = .FALSE.
           END IF
           IF(CHAR.NE.BLANK.AND.CHAR.NE.COMMA)THEN
             OSTR  = '    INVALID COMMAND,  REENTER'
             VALID = .FALSE.
             WRITE(IWRITE,3)OSTR
           END IF
         END IF
       IF(VALID)THEN
C---                                    *CONVERT
       GO TO 2
       END IF
       END IF
       NUM = INDX
       IF (INDX.EQ.0.AND.VALID) THEN
         OSTR  = '  INVALID COMMAND'
         WRITE(IWRITE,3) OSTR
         VALID = .FALSE.
       END IF
       RETURN
 3     FORMAT(A80)
       END
C-----------------------------------------------------------------------
C   THE MAIN ROUTINE FOR PUTTING
C   BRANCH AND POINT LABELS ON THE CURVES.
C-----------------------------------------------------------------------
       SUBROUTINE LGRPHS(MP,PNTS)
       CHARACTER*1 MP(81,251)
       REAL PNTS(1999,4)
C---
C---
       I = 1
 1     IF((PNTS(I,1).EQ.0).OR.(I.GT.1999)) RETURN
       CALL FINDSP(I,PNTS,MP)
       I = I + 1
       GO TO 1
       END
C-----------------------------------------------------------------------
C   TRIES TO FIND AN EMPTY SPACE TO PUT THE
C   POINT OR BRANCH LABEL FROM PNTS IN BY SEARCHING FOR THREE
C   UNCHANGED ENTRIES IN MP.  THE SEARCH IS OUTWARD FROM THE
C   POINT TO BE LABELLED.  IF SPACE CAN'T BE FOUND THE LABEL
C   WILL BE WRITTEN OUT ANYWAY.
C-----------------------------------------------------------------------
       SUBROUTINE FINDSP(II,PNTS,MP)
       INTEGER XD,YD,NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,PNTS(1999,4)
       CHARACTER*1 MP(81,251),ANS,CHONE,CHTWO*2,CHFIF*15
       COMMON/PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /SPNUM/ SP1,SP2,SP3,SP4
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
C---
C---
       X1 = PNTS(II,1)
       Y1 = PNTS(II,2)
       NUM = INT(ABS(PNTS(II,3)))
       CALL DICHR(NUM)
C---                                    *X1, Y1 ARE SCREEN COORDINATES
       XI = (X1 - SP1) / SP2 + 1
       YI = (Y1 - SP3) / SP4 + 1
       RD = ACOS(-1.0) / 180.0
       START = RD * 30.0
       A45   = RD * 45.0
       A135  = RD * 135.0
       A225  = RD * 225.0
       A315  = RD * 315.0
       RADIUS = 1
       NPOINT = 16
C---
 1     RADIUS = RADIUS + 1
       NPOINT = NPOINT + 8
       ST     = START
       RINC   = RD * 360.0 / REAL(NPOINT)
C---
       DO 2 I=1,NPOINT
         XD = INT(RADIUS * COS(ST) + XI)
         YD = INT(RADIUS * SIN(ST) + YI)
         CALL EMPTSP(XD,YD,MP,ANS)
         IF (ANS.EQ.'Y') THEN
           R1 = (RADIUS + 3) * SP2
           CALL DXY(R1,ST,X1,Y1,IX,IY)
           IF (IX.GE.MINSX.AND.IX.LE.MAXSX.AND.IY.GE.MINSY.AND.
     +         IY.LE.MAXSY) THEN
             DO 4 I1=XD-2,XD+2
               DO 3 I2=YD-1,YD+1
                 MP(I1,I2) = 'X'
 3             CONTINUE
 4           CONTINUE
             GOTO 5
           END IF
         END IF
         ST = ST + RINC
 2     CONTINUE
       IF (RADIUS.LT.70) GOTO 1
C---                                    *IF NO EMPTY SPACE
       RETURN
 5     IF (ST.GE.A135.AND.ST.LE.A225) THEN
         R1 = (RADIUS + INDXCH + 0.5) * SP2
         CALL DXY(R1,ST,X1,Y1,IX,IY)
       ELSE IF (ST.LE.A45.OR.ST.GE.A315) THEN
         R1 = (RADIUS + 0.5) * SP2
         CALL DXY(R1,ST,X1,Y1,IX,IY)
       ELSE IF (ST.GE.A225.AND.ST.LE.A315) THEN
         R1 = (RADIUS + 1.0) * SP2
         CALL DXY(R1,ST,X1,Y1,IX,IY)
         IX = IX - (INDXCH - 1) * SP2
         IY = IY - SP2
       ELSE
         R1 = (RADIUS + 0.5) * SP2
         CALL DXY(R1,ST,X1,Y1,IX,IY)
         IX = IX - (INDXCH - 1) * SP2
       END IF
C---                                    *SAVE  LABEL
       RIX=IX
       RIY=IY
C---                                    *LABEL DASH
       RADIUS = RADIUS * SP2
       CALL DXY(RADIUS,ST,X1,Y1,IX,IY)
       CALL PLCMDS(23)
       CALL PLCMDS(2)
       IX = X1
       IY = Y1
       CALL PLCMDS(3)
C---
C                                       *PRINT LABEL
       IX=RIX
       IY=RIY
       CALL PLCTNM(22)
       CALL PLCMDS(20)
C---
       RETURN
       END
C-----------------------------------------------------------------------
C   ORGANIZES THE SEARCH ALONG TWO SIDES OF
C   A SQUARE OF SIZE 2(XD) WITH PNT XI, YI IN ITS CENTER.
C-----------------------------------------------------------------------
       SUBROUTINE EMPTSP(IX,IY,MP,ANS)
       CHARACTER*1 MP(81,251),ANS
C---
       ANS = 'N'
       IF (IX.GT.77.OR.IX.LT.4) RETURN
       IF (IY.GT.47.OR.IY.LT.4) RETURN
       DO 2 I=IX-2,IX+2
         DO 1 J=IY-2,IY+2
           IF (MP(I,J).EQ.'X') RETURN
 1       CONTINUE
 2     CONTINUE
       ANS = 'Y'
       RETURN
       END
C-----------------------------------------------------------------------
C    INITIALIZES THE MAP AND POINT ARRAYS THAT ARE
C    USED TO LABEL THE PLOTTED CURVES.
C-----------------------------------------------------------------------
       SUBROUTINE INARRS(MP,PNTS)
       CHARACTER*1 MP(81,251)
       REAL PNTS(1999,4)
C---
C---
       DO 2 I = 1,81
         DO 1 J = 1,251
           MP(I,J) = 'O'
 1       CONTINUE
 2     CONTINUE
       DO 3 I = 1,1999
         PNTS(I,1) = 0
         PNTS(I,2) = 0
         PNTS(I,3) = 0
         PNTS(I,4) = 0
 3     CONTINUE
       RETURN
       END
C-----------------------------------------------------------------------
C   SCANS THE "2D" FILE TO FIND THE MAX AND MIN POINTS T
C   BE PLOTTED.  THESE ARE PUT IN ARARY BNDS.  THE SPECIAL POINTS TO BE
C   PLOTTED ARE ALSO CHECKED FOR DUPLICATE POINTS (ERROR NO. 4).  ERROR
C   MESSAGES WILL BE GENERATED IF THE "Q" FILE ISN'T STRUCTURED PROPERLY
C   (ERROR NO. 3).
C-----------------------------------------------------------------------
       SUBROUTINE SCAN2D(PARRY,BNDS,NUM,ERROR)
       INTEGER PARRY(1999),ERROR,TEMP,SLAB(1999),ENLAB(1999)
       REAL BNDS(251,2)
       REAL MINA(1999,251),MAXA(1999,251),AVERA(1999,251)
       COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
       COMMON /MMA/ MINA,MAXA,AVERA

C---
C---
C---
C---                                    *SHOW POINTS FROM <2D>
       ERROR = 0
C---                                    *DUPLICATE <2D>
       DO 20 I = 1,NUM-1
         TEMP = PARRY(I)
         DO 18 J = I+1,NUM
           IF (TEMP.EQ.PARRY(J)) THEN
             ERROR = 4
             RETURN
           END IF
18       CONTINUE
20     CONTINUE
C---                                    *CHECK LABELS
       DO 2 I=1,NUM
         DO 1 J=1,NLAB
           IF (PARRY(I).EQ.SLAB(J)) GO TO 2
 1       CONTINUE
         ERROR = 5
         RETURN
 2     CONTINUE
C---                                    *GET MIN AND MAX OF ALL LABELS
C---                                    *ENTERED
       DO 4 I=1,NUM
         DO 3 J=1,NLAB
           IF (PARRY(I).NE.SLAB(J)) GO TO 3
           IF (I.EQ.1) THEN
             DO 5 K=1,NAXIS
               BNDS(K,1) = MINA(J,K)
               BNDS(K,2) = MAXA(J,K)
 5           CONTINUE
           ELSE
             DO 6 K=1,NAXIS
               IF(BNDS(K,1).GT.MINA(J,K)) BNDS(K,1)=MINA(J,K)
               IF(BNDS(K,2).LT.MAXA(J,K)) BNDS(K,2)=MAXA(J,K)
 6           CONTINUE
           END IF
           GO TO 4
 3       CONTINUE
 4     CONTINUE
       RETURN
       END
C-----------------------------------------------------------------------
C   THIS IS THE MAIN ROUTINE FOR PLOTTING THE "Q" FILE.
C-----------------------------------------------------------------------
       SUBROUTINE PLOT2D(XAXS,YAXS,PARRY,NUM,BNDS,ANS)
       CHARACTER*2 ANS,CHTWO,CHFIF*15
       CHARACTER*80 XLAB,YLAB,TTITLE,BTITLE,CHONE*1
       INTEGER ERROR,XAXS,YAXS,PARRY(1999),NVX(2),SPLPT
       INTEGER ID2D(1999),IDS2D(1999)
       REAL BNDS(251,2),IX,IY,MINSX,MAXSX,MINSY,MAXSY
       LOGICAL DFT,USR,PLTR,BRNCH,POINT,TIT,AXLB,QLBS,GRIDS,DP
       LOGICAL TOP,BOTTOM
       COMMON /UOPTS/DFT,USR,BRNCH,
     + ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /LABLS/TTITLE,BTITLE,XLAB,YLAB
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /PLOTT/ PLTR,POINT
       COMMON /SY2D/ ID2D,IDS2D
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
C---
C---
       ANS   = '  '
       DO 1 I=1,NUM
         ID2D(I)  = 2
         IDS2D(I) = 0
 1     CONTINUE
       XAXS = 1
       YAXS = 2
       XMIN = BNDS(XAXS,1)
       XMAX = BNDS(XAXS,2)
       YMIN = BNDS(YAXS,1)
       YMAX = BNDS(YAXS,2)
       CALL SINGLE
C---                                    *<2D>
 2     CALL ENT2D(XAXS,YAXS,PARRY,NUM,BNDS,ICODE)
       IF (ICODE.EQ.3) THEN
         RETURN
       ELSE IF (ICODE.EQ.2) THEN
         ANS = '3D'
         RETURN
       END IF
C---                                   *DISPLAY
       IF (.NOT.DFT) THEN
         CALL RLABL
       END IF
       IF (QLBS) THEN
         XLAB = ' '
         YLAB = ' '
         IF (XAXS.EQ.1) THEN
           XLAB = 'TIME'
         ELSE
           CALL DICHR(XAXS-1)
           XLAB(1:1) = 'X'
           IF (INDXCH.EQ.1) THEN
             XLAB(2:3) = CHONE
           ELSE
             XLAB(2:3) = CHTWO
           END IF
         END IF
C---                                    *PUT TITLE ON Y AXIS
         IF (YAXS.EQ.1) THEN
           YLAB = 'TIME'
         ELSE
           CALL DICHR(YAXS-1)
           YLAB(1:1) = 'X'
           IF (INDXCH.EQ.1) THEN
             YLAB(2:3) = CHONE
           ELSE
             YLAB(2:3) = CHTWO
           END IF
         END IF
       END IF
       CALL SPAGE
       CALL DRW2D(XAXS,YAXS,PARRY,ERROR,NUM,POINT,PLTR)
       CALL PLCHDW(3)
       GO TO 2
       END
C-----------------------------------------------------------------------
C      EXECUTES <2D> COMMANDS
C-----------------------------------------------------------------------
       SUBROUTINE ENT2D(XAXS,YAXS,PARRY,NUM,BNDS,ICODE)
       INTEGER SLAB(1999),ENLAB(1999),PARR1(1999),ID2D(1999),IDS2D(1999)
       INTEGER XAXS,YAXS,PARRY(1999),NVX(2),SPLPT,ERROR
       REAL BNDS(251,2),IX,IY,MINSX,MAXSX,MINSY,MAXSY
       CHARACTER*80 ISTR,OSTR
       LOGICAL PLTR,POINT,VALID,DFT,USR,BRNCH,TOP,BOTTOM,TIT,AXLB
       LOGICAL QLBS,GRIDS,DP
C---
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /PLOTT/ PLTR,POINT
       COMMON /SY2D/ ID2D,IDS2D
       COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
       COMMON /IO/  IWRITE,ITERM,ISAVE
       COMMON /UOPTS/ DFT,USR,BRNCH,ICL,ICT,TOP,BOTTOM,TIT,AXLB,
     + QLBS,GRIDS,DP
C---
C---
  1    OSTR      = '  NUMBER OF COMPONENTS :'
       WRITE(ITERM,3) OSTR(1:24),NAXIS
       OSTR      =
     + '  ENTER AXES  (DEFAULT ),  <D> (DISPLAY), OR <EX> (EXIT)'
       WRITE(ITERM,7) OSTR(1:23),XAXS,YAXS,OSTR(24:59)
C---                                    *READ DATA INPUT
       CALL READS8(ISTR)
C---                                    *DISPLAY
       IF (ISTR.EQ.'D '.OR.ISTR.EQ.'d ') THEN
         ICODE = 1
         RETURN
       END IF
C---                                    *PLOTTER
       IF (ISTR.EQ.'PO'.OR.ISTR.EQ.'po') THEN
         PLTR  = .TRUE.
         ICODE = 1
         RETURN
       END IF
C---                                    *SAVE GRAPH
       IF (ISTR.EQ.'SA'  .OR.ISTR.EQ.'sa'  .OR.
     *     ISTR.EQ.'SAV' .OR.ISTR.EQ.'sav' .OR.
     *     ISTR.EQ.'SAVE'.OR.ISTR.EQ.'save' )THEN
         IWRITE = ISAVE
	 CALL CRFLNM(1)
         CALL SPAGE
         CALL DRW2D(XAXS,YAXS,PARRY,ERROR,NUM,POINT,PLTR)
         CALL PLCHDW(3)
	 CALL CRFLNM(2)
         IWRITE = ITERM
         CALL COMPLT
         GO TO 1
       END IF
C---                                    *SET UP TITLE
       IF (ISTR.EQ.'ST'.OR.ISTR.EQ.'st') THEN
         CALL ENTGRD(1)
         TIT  = .TRUE.
         AXLB = .TRUE.
         QLBS = .FALSE.
         GO TO 1
       END IF
C---                                    *SET DIFFERENTIAL PLOT
       IF (ISTR.EQ.'SD'.OR.ISTR.EQ.'sd') THEN
         CALL ENTSY
         GO TO 1
       END IF
C---                                    *RESET DIFFERENTIAL PLOT
       IF (ISTR.EQ.'RS'.OR.ISTR.EQ.'rs') THEN
         DO 2 I=1,NUMLB
           ID2D(I)  = 2
           IDS2D(I) = 0
 2       CONTINUE
         GO TO 1
       END IF
C---                                    *GET NEW LABELS FROM USER
       IF (ISTR.EQ.'2D'.OR.ISTR.EQ.'2d') THEN
         CALL PRLAB
         OSTR = ' ENTER NEW LABELS : '
         WRITE(ITERM,4) OSTR
         CALL READS8(ISTR)
         I = 1
         IF (ISTR(1:1).EQ.' ') I = 2
         CALL CONVT(ISTR,I,NUM1,PARR1,VALID)
         IF (.NOT.VALID) GO TO 1
         CALL SCAN2D(PARR1,BNDS,NUM1,ERROR)
         IF (ERROR.NE.0) THEN
           CALL WERRS(ERROR)
           GO TO 1
         END IF
         NUMLB = NUM1
         NUM   = NUM1
C---                                    *RESET DIFFERENTIAL PLOT
         DO 6 I=1,NUM
           PARRY(I) = PARR1(I)
           ID2D(I)  = 2
           IDS2D(I) = 0
           ENLAB(I) = PARR1(I)
 6       CONTINUE
         XAXS = 1
         YAXS = 2
         XMIN = BNDS(XAXS,1)
         XMAX = BNDS(XAXS,2)
         YMIN = BNDS(YAXS,1)
         YMAX = BNDS(YAXS,2)
         CALL SINGLE
         TIT  = .FALSE.
         AXLB = .FALSE.
         GO TO 1
       END IF
C---                                    *CLEAR SCREEN
       IF (ISTR.EQ.'CL'.OR.ISTR.EQ.'cl') THEN
         CALL PLCMDS(1)
         CALL PLCHDW(3)
         GO TO 1
       END IF
C---                                    *CHANGE TO <3D>
       IF (ISTR.EQ.'3D'.OR.ISTR.EQ.'3d') THEN
         ICODE = 2
         RETURN
       END IF
C---                                    *TERMINATE <2D>
       IF (ISTR.EQ.'END'.OR.ISTR.EQ.'end'.OR.
     +     ISTR.EQ.'EX' .OR.ISTR.EQ.'ex' .OR.
     +     ISTR.EQ.'E'  .OR.ISTR.EQ.'e') THEN
         ICODE = 3
         RETURN
       END IF
C---                                    *GET NEW AXES
       IF (ISTR.EQ.' ') GO TO 1
       I = 1
       IF (ISTR(1:1).EQ.' ') I = 2
       CALL CONVT(ISTR,I,NUM1,PARR1,VALID)
       IF (.NOT.VALID) GO TO 1
       IF (NUM1.NE.2) THEN
         OSTR = ' ERROR - REENTER TWO AXIS'
         WRITE(ITERM,4) OSTR
         OSTR = ' DEFAULT AXES ARE : '
         WRITE(ITERM,5) OSTR,XAXS,YAXS
         OSTR = ' LABELS HAVE DIMENSION '
         WRITE(ITERM,5) OSTR,NAXIS
         GO TO 1
       END IF
       IX1   = PARR1(1)
       IY1   = PARR1(2)
       IF (IX1.LT.1.OR.IX1.GT.NAXIS.OR.
     +     IY1.LT.1.OR.IY1.GT.NAXIS) THEN
         OSTR = ' ERROR - REENTER TWO INTEGERS .GT. 0 AND .LT.'
         WRITE(ITERM,5) OSTR,NAXIS
         GO TO 1
       END IF
C---*GET TWO VALID AXES AND MI
       XAXS = IX1
       YAXS = IY1
       XMIN = BNDS(IX1,1)
       XMAX = BNDS(IX1,2)
       YMIN = BNDS(IY1,1)
       YMAX = BNDS(IY1,2)
       CALL SINGLE
       GO TO 1
 3     FORMAT(A,I5)
 4     FORMAT(A)
 5     FORMAT(A,2I5)
 7     FORMAT(A,2I3,A)
       END
C-----------------------------------------------------------------------
C   DRAWS UNIT 8 FILE IN 2D.
C-----------------------------------------------------------------------
       SUBROUTINE DRW2D(XAXS,YAXS,PARRY,ERROR,NUM,POINT,PLTR)
       INTEGER PARRY(1999),PTNUM,DOUBLE(1999)
       INTEGER ERROR,XAXS,YAXS
       INTEGER GNUM,SPLPT,SPNUM,NUMPTS,BLKSZE,NVX(2)
       INTEGER ID2D(1999),IDS2D(1999)
       CHARACTER*1 MP(81,251)
       LOGICAL POINT,LABYET,PLTR,SYMBOL,FIRST
       REAL HOLD(251),IX,IY,MINSX,MAXSX,MINSY,MAXSY,PNTS(1999,4)
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /SY2D/ ID2D,IDS2D
       COMMON /ITWOPT/ XNEW,YNEW,XOLD,YOLD
       COMMON /TWOPT/  XNEW1,YNEW1,XOLD1,YOLD1
       COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
       COMMON /SPNUM/ SP1,SP2,SP3,SP4
C---
       CALL DISXY(MAXSX,MAXSY,MINSX,MINSY,DIS)
       TOOCLS = DIS / RIDIS
       SYMBOL = .FALSE.
       REWIND 18
       LBPT   = 10
       NOLAB  = NUM
       DO 50 I=1,NUM
         DOUBLE(I) = 0
 50    CONTINUE
       CALL INITIN
C---                                    *INITIALIZE CURVE LABEL
       CALL INARRS(MP,PNTS)
       INDX = 0
15     IF (NOLAB.EQ.0) GO TO 80
C---                                    *READ BLOCK HEADER LINE
       READ(18,*,END=80)GNUM,PTNUM,SPLPT,SPNUM,X,Y,NUMPTS,NAXIS,BLKSZE
       INDEX  = 0
       LABYET = .FALSE.
       DO 20 I = 1,NUM
         IF (DOUBLE(I).EQ.1) GO TO 20
         IF (PARRY(I).EQ.SPNUM) THEN
           INDEX     = I
           DOUBLE(I) = 1
         END IF
 20    CONTINUE
C---                                    *CHECK BLOCK
       IF (INDEX.EQ.0) GO TO 2
       FIRST = .TRUE.
       CALL DFNCUR
       NOLAB = NOLAB - 1
       ID    = ID2D(INDEX)
       IDS   = IDS2D(INDEX)
       DO 1 I=1,NUMPTS
C---                                    *PLOT CURVE
         READ(18,*)(HOLD(J),J=1,NAXIS)
         X     = HOLD(XAXS)
         Y     = HOLD(YAXS)
         CALL PLCMDS(24)
         CALL PLCMDS(7)
         CALL PLCMDS(20)
         XNEW = IX
         YNEW = IY
         XNEW1 = X
         YNEW1 = Y
         IF (POINT) THEN
C---                                    *SAVE
           IF (NUMPTS.LE.15)LBPT = 2
           IF (I.EQ.LBPT.AND..NOT.LABYET) THEN
C---                                    *INCREASE
             LBPT = LBPT + 10
             IF (LBPT.GE.NUMPTS)LBPT = 15
             INDX         = INDX + 1
             PNTS(INDX,1) = IX
             PNTS(INDX,2) = IY
             PNTS(INDX,3) = SPNUM
             PNTS(INDX,4) = 4
             LABYET       = .TRUE.
           END IF
C---                                    *SAVE CURVE MAP
           IF (I.EQ.1) THEN
             CALL MAP(MP,XNEW,YNEW,XNEW,YNEW)
           ELSE
             CALL MAP(MP,XNEW,YNEW,XOLD,YOLD)
           END IF
         END IF
         IF (.NOT.FIRST) THEN
           CALL DISXY(XNEW,YNEW,XOLD,YOLD,DIS)
           IF (TOOCLS.GT.DIS) GO TO 1
         END IF
         CALL SYMBLS(XNEW1,YNEW1,ID,IDS,FIRST,SYMBOL)
         FIRST = .FALSE.
         XOLD1 = XNEW1
         YOLD1 = YNEW1
         XOLD = XNEW
         YOLD = YNEW
 1     CONTINUE
       NA1 = INT(NAXIS/7)
       IF (MOD(NAXIS,7).NE.0) NA1 = NA1 + 1
       BLKSZE = BLKSZE - NUMPTS * NA1
 2     CONTINUE
C---                                    *SKIP TO THE NEXT BLOCK
       DO 60 I = 1,BLKSZE
60       READ(18,*)
       GO TO 15
80     IF (POINT) THEN
         CALL LGRPHS(MP,PNTS)
       END IF
       CALL PLCMDS(9)
       IF (PLTR) THEN
         CALL PLCHDW(2)
         PLTR = .FALSE.
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C      DEFINES TYPE AND SYMBOLS FOR <2D>
C      AND PREPARES CURVES FOR DISPLAY
C-----------------------------------------------------------------------
       SUBROUTINE ENTSY
       INTEGER SLAB(1999),ENLAB(1999),ID2D(1999),IDS2D(1999),PARRY(1999)
       CHARACTER*80 OSTR,ISTR,OSTR1*25
       LOGICAL VALID
       COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
       COMMON /SY2D/ ID2D,IDS2D
       COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
       NUM 1 = NUMLB
       OSTR1 = '    LABEL   TYPE   SYMBOL'
       CALL PRSYBL
       OSTR  = ' LABELS, TYPE AND SYMBOLS DEFAULT ARE :'
       WRITE(ITERM,4) OSTR
C---
C---
       IF (NUMLB.EQ.1) THEN
         WRITE(ITERM,5) OSTR1
       ELSE IF (NUMLB.EQ.2) THEN
         WRITE(ITERM,5) OSTR1,OSTR1
       ELSE
         WRITE(ITERM,5) OSTR1,OSTR1,OSTR1
       END IF
C---
C---                                    *PRINT LABELS
C---
       WRITE(ITERM,6) (ENLAB(I),ID2D(I),IDS2D(I),I=1,NUMLB)
       OSTR = ' ENTER OLD LABEL AND NEW TYPE OR SYMBOL'
       WRITE(ITERM,4) OSTR
       OSTR = ' <END> OR <E> OR <RETURN> TERMINATE DEFINE'
       WRITE(ITERM,4) OSTR
C---                                    *READ DATA
 1     CALL READS8(ISTR)
       IF (ISTR.EQ.'END'.OR.ISTR.EQ.'end'.OR.
     +     ISTR.EQ.'EX' .OR.ISTR.EQ.'ex' .OR.
     +     ISTR.EQ.'E'  .OR.ISTR.EQ.'e'  .OR.
     +     ISTR.EQ.' ') RETURN
       I = 1
       IF (ISTR(I:I).EQ.' ') I = 2
       CALL CONVT(ISTR,I,NUM,PARRY,VALID)
       IF (.NOT.VALID) GO TO 1
       IF (NUM.EQ.1) THEN
         OSTR = ' ERROR - DATA INPUT, REENTER AGAIN'
         WRITE(ITERM,4) OSTR
         GO TO 1
       END IF
C---                                    *CHECK LABEL
       INDEX = 0
       DO 2 I=1,NUMLB
         IF (ENLAB(I).EQ.PARRY(1)) INDEX = I
 2     CONTINUE
       IF (INDEX.EQ.0) THEN
         OSTR = ' ERROR - LABEL NOT FOUND, REENTER '
         WRITE(ITERM,4) OSTR
         GO TO 1
       END IF
C---                                    *REDEFINE
       ID2D(INDEX)  = 0
       IDS2D(INDEX) = 0
       J            = 2
 3     ISYBL        = PARRY(J)
       IF (ISYBL.LE.7.AND.ISYBL.GE.1) THEN
         ID2D(INDEX) = ISYBL
       ELSE IF (ISYBL.LE.27.AND.ISYBL.GE.8) THEN
         IDS2D(INDEX) = ISYBL
       END IF
       IF (NUM.GE.3.AND.J.EQ.2) THEN
         J = 3
         GO TO 3
       END IF
       NUM1 = NUM1 - 1
       IF (NUM1.EQ.0) RETURN
       GO TO 1
 4     FORMAT(A)
 5     FORMAT(3A25)
 6     FORMAT(3(5X,I3,5X,I2,5X,I3,2X))
       END
C-----------------------------------------------------------------------
C      CONVERTS THE INTEGER IN INT TO THE TWO
C      CHARACTER EQUIVALENT IN DSTG.
C-----------------------------------------------------------------------
       SUBROUTINE DICHR(INT)
       CHARACTER*1 CHONE,CH9(0:9),CHTWO*2,CHFIF*15
       COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
       DATA CH9/'0','1','2','3','4','5','6','7','8','9'/
C---
C---
       IF (INT.LT.10) THEN
         INDXCH     = 1
         CHONE      = CH9(INT)
       ELSE
         INDXCH     = 2
         I          = INT / 10
         CHTWO(1:1) = CH9(I)
         I          = MOD(INT,10)
         CHTWO(2:2) = CH9(I)
       END IF
       RETURN
       END
C-----------------------------------------------------------------------
C   WRITES OUT ERROR MESSAGES.
C-----------------------------------------------------------------------
       SUBROUTINE WERRS(ERROR)
       CHARACTER*80 OSTR
       INTEGER ERROR
       COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
       GO TO (1,2,3,4,5,6,7,8,9,10,11,12),ERROR
1      OSTR = ' WARNING - DUPLICATE BLOCK NUMBERS IN UNIT 8'
       GO TO 13
2      OSTR = ' ERROR - NUMBER OF UNIT 8 DIMENSIONS GREATER THAN 251'
       GO TO 13
3      OSTR = ' ERROR - FORMAT, NUMPTS OR BLKSIZE'
       GO TO 13
4      OSTR = ' ERROR - DUPLICATE POINTS ENTERED IN <2D> COMMAND'
       GO TO 13
5      OSTR = ' ERROR - POINT ENTERED NOT IN UNIT 8'
       GO TO 13
6      OSTR = ' ERROR - UNIT 7 HAS BAD FORMAT'
       GO TO 13
7      OSTR = ' ERROR IN PLCMDS CALL. BAD COMMAND NUMBER'
       GO TO 13
8      OSTR = ' ERROR - FIRST LINE IN UNIT 7 HAS BAD FORMAT'
       GO TO 13
9      OSTR = ' ERROR - IN UNIT 8'
       GO TO 13
10     OSTR = ' WARNING - NUMBER OF AXES ARE DIFFERENT'
       GO TO 13
11     OSTR = ' ERROR - DIMENSION IS 2, YOU CAN NOT USE <3D> COMMAND'
       GO TO 13
12     OSTR = ' MORE THAN 1999 LABELS IN UNIT 8,'
13     WRITE(IWRITE,14) OSTR
       RETURN
14     FORMAT(A80)
       END
C-----------------------------------------------------------------------
C     CURVE SYMBOLS
C-----------------------------------------------------------------------
       SUBROUTINE DPSYBL(ID)
C---
C---
       ICODE = ID - 7
       GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
     +        ICODE
C---
 1     CALL OPNSQR
       RETURN
 2     CALL FILSQR
       RETURN
 3     CALL OPNTRI
       RETURN
 4     CALL FILTRI
       RETURN
 5     CALL HCRCLE
       RETURN
 6     CALL FILCIR
       RETURN
 7     CALL PLUS
       RETURN
 8     CALL OPNDIA
       RETURN
 9     CALL FILDIA
       RETURN
 10    CALL OPNDTR
       RETURN
 11    CALL FILDTR
       RETURN
 12    CALL SQRMUT
       RETURN
 13    CALL CIRPLS
       RETURN
 14    CALL DBLTRI
       RETURN
 15    CALL CIRMUT
       RETURN
 16    CALL SQRTRI
       RETURN
 17    CALL SQRPLS
       RETURN
 18    CALL DIAPLS
       RETURN
 19    CALL MUTSGN
       RETURN
 20    CALL START
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS AN OPEN SQUARE AS AN ITP SYMBOL.
C-----------------------------------------------------------------------
       SUBROUTINE OPNSQR
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX = IX
       TY = IY
       RINC   = ACOS(-1.0) / 2.0
       RADIUS = RADI
       ANGLE  = ACOS(-1.0) / 4.0
       NPOINT = 5
       CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS A FILLED SQUARE AS AN ITP SYMBOL.
C-----------------------------------------------------------------------
       SUBROUTINE FILSQR
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(30)
       TX     = IX
       TY     = IY
       RINC   = ACOS(-1.0) / 2.0
       NPOINT = 5
       R = RADI
 1     IF (R.LT.RADINC) GOTO 2
         RADIUS = R
         ANGLE  = ACOS(-1.0) / 4.0
         CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
         R = R - RADINC
         GOTO 1
 2     CONTINUE
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS A TRIANGLE AS AN ITP SYMBOL.
C-----------------------------------------------------------------------
       SUBROUTINE OPNTRI
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       RADIUS = RADI
       RINC   = ACOS(-1.0) * 2.0 / 3.0
       ANGLE  = ACOS(-1.0) / 2.0
       NPOINT = 4
       CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS A FILLED TRIANGLE AS AN ITP SYMBOL.
C-----------------------------------------------------------------------
       SUBROUTINE FILTRI
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(30)
       TX     = IX
       TY     = IY
       NPOINT = 4
       RINC   = ACOS(-1.0) * 2.0 / 3.0
       R = RADI
 1     IF (R.LT.RADINC) GOTO 2
         RADIUS = R
         ANGLE  = ACOS(-1.0) / 2.0
         CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
         R = R - RADINC
         GOTO 1
 2     CONTINUE
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS A CIRCLE AS AN ITP SYMBOL.
C-----------------------------------------------------------------------
       SUBROUTINE HCRCLE
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       RADIUS = RADI
       RINC   = ACOS(-1.0) / 5.0
       ANGLE  = 0.0
       NPOINT = 11
       CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS A FILLED CIRCLE AS AN ITP SYMBOL
C-----------------------------------------------------------------------
       SUBROUTINE FILCIR
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.LT.MINSX.OR.IX.GT.MAXSX) RETURN
       IF (IY.LT.MINSY.OR.IY.GT.MAXSY) RETURN
       CALL PLCMDS(30)
       TX     = IX
       TY     = IY
       RINC   = ACOS(-1.0) / 5.0
       NPOINT = 11
       R = RADI
 1     IF (R.LT.RADINC) GOTO 2
         RADIUS = R
         ANGLE  = 0
         CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
         R = R - RADINC
         GOTO 1
 2     CONTINUE
       IX = TX
       CALL PLCMDS(20)
       IY = TY
       RETURN
       END
C-----------------------------------------------------------------------
C       DRAWS A FILLED TRIANGLE AS AN ITP SYMBOL
C-----------------------------------------------------------------------
       SUBROUTINE PLUS
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(30)
       TX = IX
       TY = IY
       IY = TY + RADI
       CALL PLCMDS(2)
       IY = TY - RADI
       CALL PLCMDS(3)
       IY = TY
       IX = TX - RADI
       CALL PLCMDS(2)
       IX = TX + RADI
       CALL PLCMDS(3)
       IX = TX
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     OPEN DIAMOND
C-----------------------------------------------------------------------
       SUBROUTINE OPNDIA
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       RADIUS = RADI
       IX = TX + RADIUS
       CALL PLCMDS(2)
       IX = TX
       IY = TY + RADIUS
       CALL PLCMDS(3)
       IX = TX - RADIUS
       IY = TY
       CALL PLCMDS(3)
       IX = TX
       IY = TY - RADIUS
       CALL PLCMDS(3)
       IX = TX + RADIUS
       IY = TY
       CALL PLCMDS(3)
       IX = TX
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     SOLID DIAMOND
C-----------------------------------------------------------------------
       SUBROUTINE FILDIA
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(30)
       TX     = IX
       TY     = IY
       RINC   = ACOS(-1.0) / 2.0
       NPOINT = 5
       R = RADI
 1     IF (R.LT.RADINC) GOTO 2
         RADIUS = R
         ANGLE  = RINC
         CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
         R = R - RADINC
         GOTO 1
 2     CONTINUE
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     OPEN DOWN TRIANGLE
C-----------------------------------------------------------------------
       SUBROUTINE OPNDTR
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       RINC   = ACOS(-1.0) * 2.0 / 3.0
       NPOINT = 4
       ANGLE  = ACOS(-1.0) / 6.0
       RADIUS = RADI
       CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     SOLID DOWN TRIANGLE
C-----------------------------------------------------------------------
       SUBROUTINE FILDTR
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(30)
       TX     = IX
       TY     = IY
       NPOINT = 4
       RINC   = ACOS(-1.0) * 2.0 / 3.0
       R = RADI
 1     IF (R.LT.RADINC) GOTO 2
         RADIUS = R
         ANGLE  = ACOS(-1.0) / 6.0
         CALL DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
         R = R - RADINC
         GOTO 1
 2     CONTINUE
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     "X" SIGN
C-----------------------------------------------------------------------
       SUBROUTINE MUTSGN
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       ANGLE  = ACOS(-1.0) / 4.0
       RINC   = ACOS(-1.0)
       RADIUS = RADI
       CALL DXY(RADIUS,ANGLE,TX,TY,IX,IY)
       CALL PLCMDS(2)
       ANGLE = ANGLE + RINC
       CALL DXY(RADIUS,ANGLE,TX,TY,IX,IY)
       CALL PLCMDS(3)
       ANGLE = ACOS(-1.0) * 3.0 / 4.0
       CALL DXY(RADIUS,ANGLE,TX,TY,IX,IY)
       CALL PLCMDS(2)
       ANGLE = ANGLE + RINC
       CALL DXY(RADIUS,ANGLE,TX,TY,IX,IY)
       CALL PLCMDS(3)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     DOUBLE TRIANGLE
C-----------------------------------------------------------------------
       SUBROUTINE DBLTRI
       INTEGER NVX(2),SPLPT
       REAL X1(4),Y1(4),IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       ANGLE  = ACOS(-1.0) / 4.0
       RINC   = ACOS(-1.0) / 2.0
       RADIUS = RADI
       DO 1 I= 1,4
         CALL DXY(RADIUS,ANGLE,TX,TY,X1(I),Y1(I))
         ANGLE = ANGLE + RINC
 1     CONTINUE
       IX1 = INT((X1(1) - X1(2)) / 2.0 + X1(2))
       IX  = X1(1)
       IY  = Y1(1)
       CALL PLCMDS(2)
       IX  = X1(2)
       CALL PLCMDS(3)
       IX  = IX1
       IY  = Y1(3)
       CALL PLCMDS(3)
       IX  = X1(1)
       IY  = Y1(1)
       CALL PLCMDS(3)
       IX  = X1(4)
       IY  = Y1(4)
       CALL PLCMDS(2)
       IX  = IX1
       IY  = Y1(1)
       CALL PLCMDS(3)
       IX  = X1(3)
       IY  = Y1(3)
       CALL PLCMDS(3)
       IX  = X1(4)
       IY  = Y1(4)
       CALL PLCMDS(3)
       IX  = TX
       IY  = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     OPEN SQUARE AND OPEN TRIANGLE
C-----------------------------------------------------------------------
       SUBROUTINE SQRTRI
       INTEGER NVX(2),SPLPT
       REAL X1(5),Y1(5),IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL PLCMDS(31)
       TX     = IX
       TY     = IY
       RADIUS = RADI
       RINC   = ACOS(-1.0) / 2.0
       ANGLE  = ACOS(-1.0) / 4.0
       DO 1 I=1,5
         CALL DXY(RADIUS,ANGLE,TX,TY,IX,IY)
         X1(I) = IX
         Y1(I) = IY
         IF (I.EQ.1) THEN
           CALL PLCMDS(2)
         ELSE
           CALL PLCMDS(3)
         END IF
         ANGLE = ANGLE + RINC
 1     CONTINUE
       IX = X1(4)
       IY = Y1(4)
       CALL PLCMDS(2)
       IX = (X1(1) - X1(2)) / 2.0 + X1(2)
       IY = Y1(1)
       CALL PLCMDS(3)
       IX = X1(3)
       IY = Y1(3)
       CALL PLCMDS(3)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     OPEN SQUARE AND PLUS SIGN
C-----------------------------------------------------------------------
       SUBROUTINE SQRPLS
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       CALL PLCMDS(31)
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       TX     = IX
       TY     = IY
       CALL OPNSQR
       RADIUS = RADI / 1.4142
       ANGLE  = ACOS(-1.0) / 4.0
       CALL DXY(RADIUS,ANGLE,TX,TY,IX,IY)
       RADIUS = IX - TX
       IX     = TX + RADIUS
       IY     = TY
       CALL PLCMDS(2)
       IX     = TX - RADIUS
       CALL PLCMDS(3)
       IX     = TX
       IY     = TY + RADIUS
       CALL PLCMDS(2)
       IY     = TY - RADIUS
       CALL PLCMDS(3)
       IX = TX
       IY = TY
       CALL PLCMDS(20)
       RETURN
       END
C-----------------------------------------------------------------------
C     CIRCLE AND PLUS SIGN
C-----------------------------------------------------------------------
       SUBROUTINE CIRPLS
C---
C---
       CALL HCRCLE
       CALL PLUS
       RETURN
       END
C-----------------------------------------------------------------------
C     CIRCLE AND "X" SIGN
C-----------------------------------------------------------------------
       SUBROUTINE CIRMUT
C---
C---
       CALL HCRCLE
       CALL MUTSGN
       RETURN
       END
C-----------------------------------------------------------------------
C     OPEN SQUARE AND "X" SIGN
C-----------------------------------------------------------------------
       SUBROUTINE SQRMUT
C---
C---
       CALL OPNSQR
       CALL MUTSGN
       RETURN
       END
C-----------------------------------------------------------------------
C      OPEN DIAMOND AND PLUS SIGN
C-----------------------------------------------------------------------
       SUBROUTINE DIAPLS
C---
C---
       CALL OPNDIA
       CALL PLUS
       RETURN
       END
C-----------------------------------------------------------------------
C     START
C-----------------------------------------------------------------------
       SUBROUTINE START
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
       COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
       IF (IX.GT.MAXSX.OR.IX.LT.MINSX) RETURN
       IF (IY.GT.MAXSY.OR.IY.LT.MINSY) RETURN
       CALL MUTSGN
       TX = IX
       IX = TX - RADI
       CALL PLCMDS(2)
       IX = TX + RADI
       CALL PLCMDS(3)
       IX = TX
       RETURN
       END
C-----------------------------------------------------------------------
C       DISPLAYS THE SYMBOLS WITH TX,TY AS CENTER
C-----------------------------------------------------------------------
       SUBROUTINE DPSHAP(RADIUS,RINC,ANGLE,NPOINT,TX,TY)
       INTEGER NVX(2),SPLPT
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     + MINSY,MAXSY,NVX,SPLPT
C---
C---
       DO 1 I=1,NPOINT
         IX = TX + RADIUS * COS(ANGLE)
         IY = TY + RADIUS * SIN(ANGLE)
         ANGLE = ANGLE + RINC
         IF (I.EQ.1) THEN
           CALL PLCMDS(2)
         ELSE
           CALL PLCMDS(3)
         END IF
 1     CONTINUE
       RETURN
       END
C-----------------------------------------------------------------------
C---
C      3D COMMANDS
C      DATE JUNE 1, 1984 TO AUGUST 1, 1985
C      AUTHOR :  NGUYEN, THANH LONG
C---
C      LISTING OF SUBROUTINES :
C---
C   SUB LIST3D : LISTS 3D COMMANDS
C---
C   SUB C2D3D  : EXECUTES <2D> AND <3D> COMMANDS.
C       A. IF LABELS ARE NOT YET DEFINED THEN PRINT ALL LABELS IN UNIT 8
C          RECEIVE LABELS FROM USER.
C       B. CONVERT LABELS FROM CHARACTERS TO INTEGERS, MAXIMUM 1999 LABELS
C       C. EXECUTE BODY OF <2D> OR <3D> COMMAND.
C---
C   SUB GERR    : EXECUTES <2D>, <3D> AND <LAB> COMMANDS
C       A. GET ALL LABELS MIN, MAX AND AVERAGE FROM UNIT 8
C       B. IF ERROR IN UNIT 8, PRINT ERROR MESSAGE AND RETURN
C       C. IF <LAB> COMMAND THEN PRINT ALL LABELS IN UNIT 8
C       D. IF <2D> COMMAND CHECK FOR ERROR IN UNIT 8
C       E. IF <3D> COMMAND AND NUMBER OF DIMENSION = 2 THEN PRINT ERROR
C---
C   SUB PLAB    : PRINT LABELS ON THE SCREEN
C       A. PRINT ALL LABELS OF UNIT 8 ON THE SCREEN
C       B. IF THE LABELS' DIMENSIONS ARE DIFFERENT THEN PRINT MESSAGE
C---
C   SUB GETNAX  : GET NUMBER OF AXES AFTER GETTING LABELS.
C       IF THE LABELS' DIMENSIONS ARE DIFFERENT THEN GET THE MINIMUM
C       DIMENSION OF THE LABELS DEFINED.
C---
C   SUB IDENTI  : SET TRAN MATRIX TO IDENTITY MATRIX.
C---
C   SUB COPYMA  : COPY MATRIX M2 TO MATRIX M1
C---
C   SUB AXYZ    : ADD X,Y,Z TO TRAN MATRIX (TRANSFORMATION OR PARALLEL
C---                 PROJECTIO
C---
C   SUB MULTMA  : MULTIPLY MATRICES TRAN AND MATRIX AND PUT RESULT
C                 IN TRAN
C---
C   SUB GETLB   : GET ALL LABELS FROM UNIT 8, AND THEIR MIN AND MAX.
C      IF ERROR (FORMAT,EMPTY,DIMENSIONS LESS THAN 2 OR GREATER THAN 251)
C      THEN PRINT MESSAGE AND RETURN.
C       IF WARNING (DUPLICATE LABELS, VARYING DIMENSIONS)
C       THEN PRINT MESSAGE AND RETURN.
C---
C   SUB GTLB3D  : GET LABELS ENTERED BY USER.
C       - CHECK FOR ERRORS IN ENTERED LABELS.
C       - COMPARE THE LABELS ENTERED WITH THE LABELS IN UNIT 8.
C---
C   SUB GETWLB  : EXECUTE <A> COMMAND.
C       IF LESS THAN OR EQUAL TO 1999 LABELS IN UNIT 8, THEN GET ALL
C       ELSE GET FIRST 1999 LABELS.  NO DUPLICATE LABELS.
C---
C   SUB SNGXYZ :
C       IF MIN, MAX OF X,Y OR Z ARE EQUAL THEN
C          MIN = MIN * 0.95
C          MAX = MAX * 1.05.
C       IF MIN, MAX OF X,Y OR Z ARE EQUAL TO ZERO THEN
C          MIN = -0.1
C          MAX =  0.1
C---
C   SUB GETDIM  : GET THE 3 DIMENSIONS AFTER GETTING LABELS FROM <3D>.
C       ON ERROR GET NEW DIMENSIONS.
C       GET MIN,MAX AND AVERAGE OF X,Y AND Z .
C---
C   SUB DFUNIT  : DEFINE UNIT OF X,Y AND Z.
C---
C   SUB SETSMA  : SET MIN AND MAX OF X,Y AND Z.
C---
C   SUB SVIEWP  : DEFINE VIEW POINT BY :
C       VIEW POINT(X,Y,Z) = MAX(X,Y,Z) + 3.05 * DISTANCE(X,Y,Z)
C---
C   SUB SO3D    : SOFTWARE FOR 3D.
C       IF CODE EQUAL TO
C          <1> THEN TRANSLATION IN X,Y AND Z
C          <2> THEN SCALING OF X,Y AND Z
C          <3> THEN ROTATION ABOUT X AXIS
C          <4> THEN ROTATION ABOUT Y AXIS
C          <5> THEN ROTATION ABOUT Z AXIS
C          <6> THEN ROTATION ABOUT ABITRARY AXIS.
C---
C   SUB CINFO   : CLEAR ALL TRANSFORMATIONS DEFINED BY USER.
C---
C   SUB KINFO   : KEEP 3D INFORMATION ON MIN, MAX, AVERAGE
C                 IN ARRAY INFO
C---
C   SUB INIFO   : INITIALIZATION OF INFORMATION
C       CLEAR TRANSFORMATION MATRIX.
C       MIN, MAX AND AVERAGE(X,Y,Z) OF THE CURVES.
C       SET MIN, MAX EQUAL TO DISTANCE FROM ORIGIN.
C       SET MIN, MAX OF THE SCREEN.
C---
C   SUB DFCOOR  : SET MIN, MAX, ERASE TRANSFORMATION,
C                 DEFINE  MIN, MAX, SET UP VIEW POINT.
C---
C   SUB SCTWO   :SCALING OF (X,Y,Z) BY DEFAULTS <D2>, <D3> AND <D4>
C---
C   SUB ENT3DC  : ENTER <3D> COMMAND.
C       RECEIVES COMMAND FROM USER AND EXECUTES :
C       1.  DEFAULT        : D1,D2,D3,D4,D5,D6.
C       2.  LABELS         : LAB,AX.
C       3.  TRANSFORMATION : TR,SC,RX,RY,RZ,RA,ET.
C       4.  LINE TYPE      : SL,SA,SO,DA,DO,HL
C       5   SYMBOLS,TITLES : CI,DCI,ST,DST,BR,DBR,PN,DPN.
C       5.  ORIGIN         : CA,CZ,CM,CU,C1,C2,C3,C4,C5.
C       6.  PROJECTIONS    : PJ,PJU,DPJ,PSL.
C       7.  VIEW POINTS    : VD,VU.
C       8.  DISPLAY        : D,DIS,SAV,PLOT.
C       9.  USER LINE TYPE : SDO,SDA,LDA,RES.
C       10. SCREEN         : SN,SU,CL,CLEAR.
C       11. <2D> COMMAND   : 2D.
C       12. INFORMATION    : LI,LIST,INFO,HELP.
C       13. TERMINATE <3D> : END EXIT.
C       14. STOP EXECUTION : STOP.
C---
C   SUB DEFINE  : DEFINE CURVES AND SYMBOLS OF <3D> OR <BD> COMMAND:
C       DASHED LINE, DOTTED LINE, SMALL CIRCLE, SYMBOLS OF <BD>,
C       DASHES, AND PLOTTING ACCURACY
C---
C   SUB HELP3D  : LISTS <3D> COMMANDS.
C---
C   SUB SWIND   :
C       - SET PARALLEL PROJECTION FOR 3 DIMENSIONS.
C       - SET PROPER WINDOW FOR 2 DIMENSIONS.
C       - KEEP MIN, MAX OF TRANSFORMATION.
C---
C   SUB NUST    : CONVERTS FROM NUMERIC TO STRING CHARACTERS
C---
C   SUB ADDT    : ADD TRANSFORMATION
C---
C   SUB SCIR    : DISPLAY SOLID CIRCLE ON CURVE.
C---
C   SUB HCIR    : DISPLAY OPEN CIRCLE ON CURVE.
C---
C   SUB DICRAV  :
C       COMPUTE AFTER CONVERSION FROM 3D TO 2D:
C             1. THE ORIGIN
C             2. (XMAX,CRY,CRZ)
C             3. (CRX,YMAX,CRZ)
C             4. (CRX,CRY,ZMAX)
C       AND ANGLE.
C---
C   SUB FDA     :
C       GET DISTANCE FROM ORIGIN TO THE DISPLAY POINT
C        <DISTANCE * 0.95)
C       COMPUTE THE ARROW ON THE AXIS
C       FIND SPACE TO PUT (X,Y,Z) OR COORDINATE NUMBER.
C       IF <PN> COMMAND THEN
C         PRINT COORDINATE NUMBER AND MAX.
C---
C   SUB PRAXIS  :
C       PRINT (X,Y,Z) ON EACH AXIS OR PRINT COORDINATE NUMBERS
C---
C   SUB DAXIS   : DISPLAY THE COORDINATE AXES AND ARROWS
C---
C   FUNCTION FANGLE : COMPUTE ANGLE
C---
C   SUB DXY     : COMPUTE DISTANCE
C---
C   SUB ARROW   : COMPUTE LOCATION TO PRINT (X,Y,Z) OR COORDINATE NUMBER
C                 ON EACH AXIS TO THE RIGHT OF THE ARROW.
C---
C   SUB ARROWL  : SAME AS SUB ARROW BUT POINT IS ON LEFT SIDE
C---
C   SUB DN      : COMPUTE THE 3 MAXIMUM POINTS AFTER TRANSFORMATION AND
C                 3 POINTS TO MARK THE ORIGIN.
C---
C   SUB DZERO   : COMPUTE TWO POINTS ON (X,Y,Z)
C---
C   SUB DISDOT  : DISPLAY WITH DOTTED LINE.
C---
C   SUB LINDAS  : DISPLAY WITH SOLID LINE AND DASHED LINE.
C---
C   SUB DASHSL  : DISPLAY WITH DASHED LINE ONLY.
C---
C   SUB LINDOT  : DISPLAY WITH DOTTED LINE AND SOLID LINE
C---
C   SUB DISTLN  : COMPUTE DISTANCE
C---
C   SUB DISP3D  : DISPLAY IN 3 DIMENSIONS.
C---
C   SUB CLIP    : IF LINE IS OUTSIDE WINDOW THENCLIP
C       (LEFT, RIGHT, BOTTOM AND TOP).
C       IF (OPCODE = 1) THEN
C         DISPLAY (X1,Y1) WITH DOTTED LINE.
C       IF (OPCODE = 2) THEN
C         MOVE THE PEN TO (X1,Y1)
C       ELSE
C         DISPLAY (X1,Y1) WITH SOLID LINE.
C---
C   SUB CLEFT   : CLIP ON LEFT
C---
C   SUB CRIGHT  : CLIP ON RIGHT
C---
C   SUB CBOTTOM : CLIP AT BOTTOM
C---
C   SUB CTOP    : CLIP AT TOP
C---
C   SUB SAVECL  : SAVE POINT AFTER CLIP.
C---
C   SUB STRCI   : COMPUTE POINT WHERE LINE CROSSES COORDINATE PLANE
C---
C   SUB DIPJ    : PROJECTION
C       IF (X0,Y0,Z0) IS THE ORIGIN
C       1. PROJECTION ON X AXIS (X,Y,Z0)
C       2. PROJECTION ON Y AXIS (X0,Y,Z)
C       3. PROJECTION ON Z AXIS (X,Y0,Z)
C---
C-----------------------------------------------------------------------
C       LISTS <3D> COMMANDS
C-----------------------------------------------------------------------
        SUBROUTINE LIST3D
        CHARACTER*50 EMPTY,OSTR
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
        CALL PLCMDS(1)
        EMPTY = ' '
        WRITE(IWRITE,1) EMPTY
        OSTR = '  LIST OF <3D> AND <B3D> COMMANDS'
        WRITE(IWRITE,1) OSTR
        WRITE(IWRITE,1) EMPTY
C---
        OSTR = '  DEFAULT        : D1,D2,D3,D4,D5,D6'
        WRITE(IWRITE,1) OSTR
        OSTR = '  LABELS         : LAB,AX'
        WRITE(IWRITE,1) OSTR
        OSTR = '  TRANSFORMATION : TR,SC,RX,RY,RZ,RA,ET'
        WRITE(IWRITE,1) OSTR
        OSTR = '  LINE TYPE      : SL,SA,SO,DA,DO,HL'
        WRITE(IWRITE,1) OSTR
        OSTR = '  TITLE, SYMBOLS : CI,DCI,ST,DST,BR,DBR,PN,DPN'
        WRITE(IWRITE,1) OSTR
        OSTR = '  SET ORIGIN     : CA,CZ,CM,CU,C1,C2,C3,C4,C5'
        WRITE(IWRITE,1) OSTR
        OSTR = '  PROJECTIONS    : PJ,PJU,DPJ,PSL'
        WRITE(IWRITE,1) OSTR
        OSTR = '  VIEW POINT     : VD,VU,DVX,DVY'
        WRITE(IWRITE,1) OSTR
        OSTR = '  DISPLAY        : D,DIS,SAV,PLOT'
        WRITE(IWRITE,1) OSTR
        OSTR = '  USER LINE TYPE : SDO,SDA,LDA,RES'
        WRITE(IWRITE,1) OSTR
        OSTR = '  SCREEN         : SN,SU,CL,CLEAR '
        WRITE(IWRITE,1) OSTR
        OSTR = '  <2D> COMMAND   : 2D'
        WRITE(IWRITE,1) OSTR
        OSTR = '  INFORMATION    : LI,LIST,INFO,HELP'
        WRITE(IWRITE,1) OSTR
        OSTR = '  TERMINATE <3D> : END,EXIT'
        WRITE(IWRITE,1) OSTR
        OSTR = '  STOP EXECUTION : STOP'
        WRITE(IWRITE,1) OSTR
        WRITE(IWRITE,1) EMPTY
        RETURN
 1      FORMAT(A50)
        END
C-----------------------------------------------------------------------
C       EXECUTES <2D> AND <3D> COMMAND
C-----------------------------------------------------------------------
        SUBROUTINE C2D3D(ISTR,II,XAXS,YAXS,PARRY,NUM,CHECKQ)
        INTEGER XAXS,YAXS,PARRY(1999),ERROR,SLAB(1999),ENLAB(1999)
        INTEGER NAX(1999),DAX(1999)
        REAL MIMA(3,2),VIEW(3),BNDS(251,2)
        CHARACTER*80 ISTR,OSTR,COM*2
        LOGICAL CHECKQ,VALID,QSCOM
C---
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
        COMMON /QSC/ QSCOM
        COMMON /DAXIS1/ NAX,DAX,NDIM
C---
C---
        ERROR = 0
        IF (QSCOM) THEN
          COM = '3D'
        ELSE
          COM = '2D'
        END IF
C---                                    *LABELS OF <2D> OR <3D>
C---                                    *IF NOT ENTERED
        IF (ISTR(II:).EQ.' ') THEN
          CALL PRLAB
          OSTR = ' ENTER LABELS, OR <A> (ALL)'
          WRITE(IWRITE,5) OSTR
          CALL READS8(ISTR)
          II = 1
          IF (ISTR(1:1).EQ.' ') II = 2
        END IF
C---                                    *GET LABELS FOR <3D>
        IF (QSCOM) THEN
          CALL GTLB3D(ISTR,II,ERROR)
          IF (ERROR.NE.0) RETURN
          CALL GETNAX(SLAB,NLAB,ENLAB,NUMLB,NAX,NAXIS)
          GO TO 1
        END IF
C---                                    *GET LABELS FOR <2D>
        IF (ISTR(II:).EQ.'A'.OR.ISTR(II:).EQ.'a') THEN
          CALL GETWLB(NUM,PARRY,NLAB,SLAB)
        ELSE
          CALL CONVT(ISTR,II,NUM,PARRY,VALID)
          IF (.NOT.VALID) RETURN
        END IF
        CALL GETNAX(SLAB,NLAB,PARRY,NUM,NAX,NAXIS)
        CALL SCAN2D(PARRY,BNDS,NUM,ERROR)
        IF (ERROR.NE.0) THEN
          CALL WERRS(ERROR)
          RETURN
        END IF
 1      CONTINUE
C---                                    *<3D>
        IF (COM.EQ.'3D'.OR.COM.EQ.'3d') THEN
          QSCOM = .TRUE.
          CALL GETDIM(MIMA)
          CALL ENT3DC(MIMA,VIEW,COM)
C---                                    *<2D>
          QSCOM = .FALSE.
          IF (COM.EQ.'2D'.OR.COM.EQ.'2d') THEN
            CHECKQ = .TRUE.
            NUM    = NUMLB
            DO 2 J = 1,NUM
              PARRY(J) = ENLAB(J)
 2          CONTINUE
            CALL SCAN2D(PARRY,BNDS,NUM,ERROR)
            GO TO 1
          ELSE
            CHECKQ = .FALSE.
            RETURN
          END IF
C---                                    *<2D>
        ELSE IF(COM.EQ.'2D'.OR.COM.EQ.'2d') THEN
          NUMLB = NUM
          DO 3 J=1,NUM
            ENLAB(J) = PARRY(J)
 3        CONTINUE
          CALL PLOT2D(XAXS,YAXS,PARRY,NUM,BNDS,COM)
          CHECKQ = .TRUE.
C---                                    *<3D>
          IF (COM.EQ.'3D'.OR.COM.EQ.'3d') THEN
            CHECKQ = .FALSE.
            IF (NAXIS.EQ.2) THEN
              CALL WERRS(11)
              RETURN
            END IF
            GO TO 1
          END IF
        END IF
 5      FORMAT(A80)
        RETURN
        END
C-----------------------------------------------------------------------
C       GETS LABELS IN UNIT 8 AND CHECKS ERRORS
C-----------------------------------------------------------------------
        SUBROUTINE GERR(ERRQ,IERR8,ERROR,INTGLB,ICODE)
        INTEGER SLAB(1999),ENLAB(1999),NAX(1999),DAX(1999)
        LOGICAL ERRQ,INTGLB
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
        COMMON /DAXIS1/ NAX,DAX,NDIM
C---
C---
        ERROR = 0
        IF (INTGLB) THEN
          IERR8  = 0
          CALL GETLB(ERRQ,IERR8)
          INTGLB = .FALSE.
        END IF
        IF (IERR8.NE.0) CALL WERRS(IERR8)
C---                                    *PRINT LABELS AND DIMENSIONS
C---                                    *IN CASE OF DUPLICATE LABELS
C---                                    *OR VARIABLE DIMENSION
        IF (IERR8.EQ.10.OR.IERR8.EQ.2) THEN
          CALL PRLAB
          RETURN
        END IF
C---                                    *DUPLICATE LABELS
        IF (IERR8.EQ.1) THEN
          CALL PRLAB
          RETURN
        END IF
C---                                    *PRINT LABELS FOR <LAB>
        IF (ICODE.EQ.1) THEN
          IF (IERR8.EQ.0.OR.IERR8.EQ.12) THEN
            CALL PRLAB
          END IF
C---                                    *<2D> OR <3D>
        ELSE
          IF (ERRQ) THEN
            ERROR = 1
            RETURN
          END IF
          IF (ICODE.EQ.3) THEN
            IF (NAXIS.EQ.2) THEN
              ERROR = 1
              CALL WERRS(11)
            END IF
          END IF
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       PRINTS LABELS AND NUMBER OF DIMENSIONS
C-----------------------------------------------------------------------
        SUBROUTINE PRLAB
        INTEGER SLAB(1999),ENLAB(1999),NAX(1999),DAX(1999),PR(1999)
        LOGICAL DDIM
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
        COMMON /DAXIS1/ NAX,DAX,NDIM
        COMMON /DIFFA/ DDIM
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
C---                                    *PRINT LABELS
        IF (.NOT.DDIM) THEN
          WRITE(IWRITE,3) (SLAB(I),I=1,NLAB)
        ELSE
          DO 2 I=1,NDIM
            NUM = 0
            DO 1 J=1,NLAB
              IF (DAX(I).NE.NAX(J)) GO TO 1
              NUM     = NUM + 1
              PR(NUM) = SLAB(J)
 1          CONTINUE
            WRITE(IWRITE,3) (PR(J),J=1,NUM)
            WRITE(IWRITE,4) DAX(I)
 2        CONTINUE
        END IF
        RETURN
 3      FORMAT(/,2X,'THE LABELS ARE :',10(T22,10I5,/))
 4      FORMAT(2X,'HAS',I5,' DIMENSIONS')
        END
C-----------------------------------------------------------------------
C       GETS NUMBER OF AXES OF LABELS
C       IF DIMENSIONS DIFFER THEN USE SMALLEST DIMENSION
C-----------------------------------------------------------------------
        SUBROUTINE GETNAX(SLAB,NLAB,PARRY,NUM,NAX,NAXIS)
        INTEGER SLAB(1999),PARRY(1999),NAX(1999)
        LOGICAL DDIM
        COMMON /DIFFA/ DDIM
C---
C---
        IF (.NOT.DDIM) THEN
          NAXIS = NAX(1)
          RETURN
        END IF
        DO 2 I=1,NUM
          DO 1 J=1,NLAB
            IF (PARRY(I).NE.SLAB(J)) GO TO 1
            IF (I.EQ.1) NAXIS = NAX(J)
            IF (NAXIS.GT.NAX(J)) NAXIS = NAX(J)
            GO TO 2
 1        CONTINUE
 2      CONTINUE

        RETURN
        END
C-----------------------------------------------------------------------
C       INITIALIZES MATRIX TRAN AS IDENTITY
C-----------------------------------------------------------------------
        SUBROUTINE IDENTI(TRAN)
        REAL TRAN(4,4)
C---
C---
        DO 2 I=1,4
          DO 1 J=1,4
            IF (I.EQ.J) THEN
              TRAN(I,J) = 1
            ELSE
              TRAN(I,J) = 0
            END IF
 1          CONTINUE
 2        CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       COPIES MATRIX M2 TO M1
C-----------------------------------------------------------------------
        SUBROUTINE COPYMA(M1,M2)
        REAL M1(4,4),M2(4,4)
C---
C---
        DO 2 I=1,4
          DO 1 J=1,4
            M1(I,J) = M2(I,J)
 1          CONTINUE
 2         CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       ADDS TRANSFORMATION TO X,Y AND Z
C-----------------------------------------------------------------------
        SUBROUTINE AXYZ(X,Y,Z,X0,Y0,Z0,TRAN)
        REAL TRAN(4,4)
C---
C---
        X0 = X*TRAN(1,1) + Y*TRAN(2,1) + Z*TRAN(3,1) + TRAN(4,1)
        Y0 = X*TRAN(1,2) + Y*TRAN(2,2) + Z*TRAN(3,2) + TRAN(4,2)
        Z0 = X*TRAN(1,3) + Y*TRAN(2,3) + Z*TRAN(3,3) + TRAN(4,3)
        RETURN
        END
C-----------------------------------------------------------------------
C       MULTIPLIES TRAN AND MATRIX
C-----------------------------------------------------------------------
        SUBROUTINE MULTMA(TRAN,MATRIX)
        REAL TRAN(4,4),MATRIX(4,4),T(4,4)
C---
C---
        DO 3 I=1,4
          DO 2 J=1,4
            T(I,J) = 0
            DO 1 L=1,4
              T(I,J) = T(I,J) + TRAN(I,L) * MATRIX(L,J)
 1          CONTINUE
 2        CONTINUE
 3      CONTINUE
        CALL COPYMA(TRAN,T)
        RETURN
        END
C-----------------------------------------------------------------------
C       GETS UNIT 8 LABELS AND MIN, MAX AND
C        AVER OF EACH COORDINATE OF LABEL
C-----------------------------------------------------------------------
        SUBROUTINE GETLB(ERRORQ,IERR8)
        INTEGER SLAB(1999),GNUM,PTNUM,SPLPT,SPNUM,NUMPTS,BLKSZE
        INTEGER ENLAB(1999),CNAXIS,NAX(1999),DAX(1999)
        REAL MINA(1999,251),MAXA(1999,251),AVERA(1999,251),AXIS(251)
        LOGICAL ERRORQ,INITL,DDIM
        COMMON /MMA/ MINA,MAXA,AVERA
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
        COMMON /DAXIS1/ NAX,DAX,NDIM
        COMMON /DIFFA/ DDIM
C---
C---
        INITL = .TRUE.
        NLAB    = 0
        NDIM    = 0
        REWIND 18
 1        READ(18,*,END=8,ERR=6)GNUM,PTNUM,SPLPT,SPNUM,X,Y,NUMPTS,
     +  NAXIS,BLKSZE
C---                                    *CHECK AXIS
        IF (INITL) THEN
          CNAXIS  = NAXIS
          INITL = .FALSE.
        ELSE
          IF (CNAXIS.NE.NAXIS) DDIM = .TRUE.
        END IF
C---                                    *CHECK LABELS IN UNIT 8
C---                                    *(MAX 1999)
        IF (NLAB.GT.1999) THEN
          IERR8 = 12
          GO TO 8
        END IF
        NLAB       = NLAB + 1
        NAX(NLAB)  = NAXIS
        SLAB(NLAB) = SPNUM
        IF (NAXIS.GT.251.OR.NAXIS.LT.2) THEN
          ERRORQ = .TRUE.
          IERR8  = 2
          RETURN
        END IF
C---                                    *DIMENSION TOO LARGE OR SMALL
C---                                    *KEEP LOW,HIGH AND AVERAGE OF
C---                                    *EACH LABEL
        DO 3 I=1,NUMPTS
          READ(18,*,END=8,ERR=7) (AXIS(J),J=1,NAXIS)
          DO 2 J=1,NAXIS
            IF (I.EQ.1) THEN
              MINA(NLAB,J)  = AXIS(J)
              MAXA(NLAB,J)  = AXIS(J)
              AVERA(NLAB,J) = AXIS(J)
            ELSE
              IF (MINA(NLAB,J).GT.AXIS(J)) MINA(NLAB,J) = AXIS(J)
              IF (MAXA(NLAB,J).LT.AXIS(J)) MAXA(NLAB,J) = AXIS(J)
              AVERA(NLAB,J) = AVERA(NLAB,J) + AXIS(J)
            END IF
 2          CONTINUE
 3        CONTINUE
        DO 4 I=1,NAXIS
          AVERA(NLAB,I) = AVERA(NLAB,I) / REAL(NUMPTS)
 4      CONTINUE
        NA1 = INT(NAXIS/7)
        IF (MOD(NAXIS,7).NE.0) NA1 = NA1 + 1
        BLKSZE = BLKSZE - NUMPTS * NA1
C---                                    *SKIP BLOCK
        DO 5 I=1,BLKSZE
          READ(18,*,ERR=7)
 5        CONTINUE
        GO TO 1
 6      CONTINUE
C---                                    *ERROR IN UNIT 8
        ERRORQ = .TRUE.
        IERR8 = 9
        RETURN
 7      CONTINUE
C---                                    *ERROR IN NUMBER OF POINTS
C---                                    *OR BLOCK SIZE
        ERRORQ = .TRUE.
        IERR8  = 3
        RETURN
 8      CONTINUE
C---                                    *NO LABELS IN UNIT 8
        IF (NLAB.EQ.0) THEN
          ERRORQ = .TRUE.
          IERR8  = 9
          RETURN
        END IF
C---                                    *DUPLICATE LABELS
        DO 10 I=1,NLAB-1
          DO 9 J=I+1,NLAB
            IF (SLAB(I).EQ.SLAB(J)) THEN
              IERR8 = 1
            END IF
 9        CONTINUE
 10     CONTINUE
        IF (.NOT.DDIM) RETURN
        NDIM   = 1
        DAX(1) = NAX(1)
        DO 12 I=2,NLAB
          DO 11 J=1,NDIM
            IF (DAX(J).EQ.NAX(I)) GO TO 12
 11       CONTINUE
          NDIM      = NDIM + 1
          DAX(NDIM) = NAX(I)
 12     CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       GETS USER LABELS FOR THE <3D> COMMAND
C-----------------------------------------------------------------------
        SUBROUTINE GTLB3D(ISTR,II,ERROR)
        CHARACTER*80 ISTR
        INTEGER SLAB(1999),PARRY(1999),ENLAB(1999),ERROR
        LOGICAL VALID
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
C---
C---
        ERROR = 0
        IF (ISTR(II:).EQ.'A'.OR.ISTR(II:).EQ.'a') THEN
          CALL GETWLB(NUMLB,ENLAB,NLAB,SLAB)
          RETURN
        END IF
        CALL CONVT(ISTR,II,NUM,PARRY,VALID)
        IF (.NOT.VALID) THEN
          ERROR = 1
          RETURN
        END IF
        NUMLB = NUM
        DO 1 I=1,NUMLB
          ENLAB(I) = PARRY(I)
 1        CONTINUE
C---                                    *DUPLICATE LABELS ENTER
        IF (NUMLB.EQ.1) GO TO 4
        DO 3 I=1,NUMLB-1
          DO 2 J=I+1,NUMLB
            IF (ENLAB(I).EQ.ENLAB(J)) THEN
              ERROR = 4
              CALL WERRS(ERROR)
              WRITE(IWRITE,7) (SLAB(JJ),JJ=1,NLAB)
              RETURN
            END IF
 2          CONTINUE
 3        CONTINUE
 4        CONTINUE
C---                                    *CHECK LABELS
        DO 6 I=1,NUMLB
          DO 5 J=1,NLAB
            IF (SLAB(J).EQ.ENLAB(I)) GO TO 6
 5        CONTINUE
          ERROR = 5
          CALL WERRS(ERROR)
          WRITE(IWRITE,7) (SLAB(JJ),JJ=1,NLAB)
          RETURN
 6      CONTINUE
 7      FORMAT(/,2X,'THE LABELS ARE : ',10(T22,10I5,/))
        RETURN
        END
C-----------------------------------------------------------------------
C        GETS ALL LABELS IN UNIT 8.
C        NO MORE THAN 1999 LABELS
C        NO DUPLICATE LABELS
C-----------------------------------------------------------------------
         SUBROUTINE GETWLB(NUM,PARRY,NLAB,SLAB)
         INTEGER PARRY(1999),SLAB(1999)
         LOGICAL DOUBLE
         COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
         IF (NLAB.GT.1999) THEN
           INUM = 1999
         ELSE
           INUM = NLAB
         END IF
C---                                    *GET LABELS FROM UNIT 8
         NUM = 0
         DO 2 I =1,INUM
           NUM    = NUM + 1
           DOUBLE = .FALSE.
           IF (NUM.GT.1) THEN
             DO 1 J=1,NUM - 1
               IF (PARRY(J).EQ.SLAB(I)) DOUBLE = .TRUE.
 1           CONTINUE
           END IF
           IF (DOUBLE) THEN
             NUM = NUM - 1
           ELSE
             PARRY(NUM) = SLAB(I)
           END IF
 2       CONTINUE
         WRITE(IWRITE,3) (PARRY(I),I=1,NUM)
         RETURN
 3       FORMAT(/,2X,'LABELS DEFINED ARE :',20(T24,10I5,/))
         END
C-----------------------------------------------------------------------
C        REDEFINES A PROPER WINDOW SIZE FOR PLOTTING
C        A SINGLE POINT AT MIN, MAX OF (X,Y,Z)
C-----------------------------------------------------------------------
         SUBROUTINE SNGXYZ
         REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
         COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +   MAXQX,MAXQY,MAXQZ
C---
C---
C---                                    *CHECK X
         CALL REDFMM(MINQX,MAXQX)
C---                                    *CHECK Y
         CALL REDFMM(MINQY,MAXQY)
C---                                    *CHECK Z
         CALL REDFMM(MINQZ,MAXQZ)
         RETURN
         END
C-----------------------------------------------------------------------
C        GETS THREE DIMENSIONS OF THE LABELS,
C        MIN, MAX, AVERAGE AND VIEW POINT
C-----------------------------------------------------------------------
        SUBROUTINE GETDIM(MIMA)
        INTEGER ENLAB(1999),SLAB(1999),XAXIS,YAXIS,ZAXIS,ERROR
        INTEGER PARRY(1999)
        REAL MINA(1999,251),MAXA(1999,251),AVERA(1999,251),MIMA(3,2)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        CHARACTER*80 OST3,ISTR,OST1*16,OST2*5
        LOGICAL CAXIS,VALID
        COMMON /MMA/ MINA,MAXA,AVERA
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /INKA/ CAXIS
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
C---
C---
 10     ERROR = 0
        IF (NAXIS.GT. 3.OR.CAXIS) THEN
          OST1 = ' EACH POINT HAS'
          OST2 = ' AXES'
          WRITE(IWRITE,1) OST1,NAXIS,OST2
 1          FORMAT(A,I3,A)
          OST3 = ' ENTER THE THREE AXES THAT YOU WANT PLOTTED'
          WRITE(IWRITE,2) OST3
 2          FORMAT(A)
          CALL READS8(ISTR)
          II = 1
          IF (ISTR(1:1).EQ.' ') II = 2
          CALL CONVT(ISTR,II,NA,PARRY,VALID)
          IF (.NOT.VALID.OR.NA.NE.3) GO TO 10
          XAXIS = PARRY(1)
          YAXIS = PARRY(2)
          ZAXIS = PARRY(3)
          MIAX  = MIN(XAXIS,YAXIS,ZAXIS)
          MAAX  = MAX(XAXIS,YAXIS,ZAXIS)
          IF (MIAX.LT.1.OR.MAAX.GT.NAXIS) ERROR = 1
          IF (ERROR.EQ.1) THEN
            OST3 = ' CHOOSE AXES NUMBERS .GT. 0 AND .LT. '
            WRITE(IWRITE,3) OST3(1:37),NAXIS
 3          FORMAT(A,I3)
            GO TO 10
          END IF
          CAXIS = .TRUE.
        ELSE
          XAXIS = 1
          YAXIS = 2
          ZAXIS = 3
        END IF
C---                                    *GET MIN, MAX AND AVER
        AVERX = 0
        AVERY = 0
        AVERZ = 0
        DO 6 I=1,NUMLB
          DO 5 J=1,NLAB
            IF (ENLAB(I).NE.SLAB(J)) GO TO 5
            IF (I.EQ.1) THEN
              MINQX = MINA(J,XAXIS)
              MINQY = MINA(J,YAXIS)
              MINQZ = MINA(J,ZAXIS)
              MAXQX = MAXA(J,XAXIS)
              MAXQY = MAXA(J,YAXIS)
              MAXQZ = MAXA(J,ZAXIS)
            ELSE
              IF(MINQX.GT.MINA(J,XAXIS)) MINQX = MINA(J,XAXIS)
              IF(MINQY.GT.MINA(J,YAXIS)) MINQY = MINA(J,YAXIS)
              IF(MINQZ.GT.MINA(J,ZAXIS)) MINQZ = MINA(J,ZAXIS)
              IF(MAXQX.LT.MAXA(J,XAXIS)) MAXQX = MAXA(J,XAXIS)
              IF(MAXQY.LT.MAXA(J,YAXIS)) MAXQY = MAXA(J,YAXIS)
              IF(MAXQZ.LT.MAXA(J,ZAXIS)) MAXQZ = MAXA(J,ZAXIS)
            END IF
            AVERX = AVERX + AVERA(J,XAXIS)
            AVERY = AVERY + AVERA(J,YAXIS)
            AVERZ = AVERZ + AVERA(J,ZAXIS)
            GO TO 6
 5          CONTINUE
 6        CONTINUE
        AVERX     = AVERX / REAL(NUMLB)
        AVERY     = AVERY / REAL(NUMLB)
        AVERZ     = AVERZ / REAL(NUMLB)
        MIMA(1,1) = MINQX
        MIMA(1,2) = MAXQX
        MIMA(2,1) = MINQY
        MIMA(2,2) = MAXQY
        MIMA(3,1) = MINQZ
        MIMA(3,2) = MAXQZ
        RETURN
        END
C-----------------------------------------------------------------------
C       DEFINES UNIT OF X,Y AND Z
C-----------------------------------------------------------------------
        SUBROUTINE DFUNIT(TRAN)
        REAL TRAN(4,4)
        COMMON /UNIT1/ SX,SY,SZ
        COMMON /TRANSF/ TX,TY,TZ,RZ
C---
C---
        TX    = SX
        TY    = SY
        TZ    = SZ
        ICODE = 2
        CALL SO3D(ICODE,TRAN)
        RETURN
        END
C-----------------------------------------------------------------------
C---
C-----------------------------------------------------------------------
        SUBROUTINE SETSMA(INFO)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ,INFO(13,3)
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /UNIT1/ SX,SY,SZ
C---
C---
        CALL SNGXYZ
        X7 = ABS(MAXQX - AVERX)
        X8 = ABS(AVERX - MINQX)
        Y7 = ABS(MAXQY - AVERY)
        Y8 = ABS(AVERY - MINQY)
        Z7 = ABS(MAXQZ - AVERZ)
        Z8 = ABS(AVERZ - MINQZ)
        SX = MAX(X7,X8)
        SY = MAX(Y7,Y8)
        SZ = MAX(Z7,Z8)
C---
        INFO(7,1) = AVERX - SX
        INFO(7,2) = AVERY - SY
        INFO(7,3) = AVERZ - SZ
        INFO(8,1) = AVERX + SX
        INFO(8,2) = AVERY + SY
        INFO(8,3) = AVERZ + SZ
        XYZ       = MAX(SX,SY,SZ)
C---
        MAXQX = AVERX + XYZ
        MINQX = AVERX - XYZ
        MAXQY = AVERY + XYZ
        MINQY = AVERY - XYZ
        MAXQZ = AVERZ + XYZ
        MINQZ = AVERZ - XYZ
C---
        SX = XYZ / SX
        SY = XYZ / SY
        SZ = XYZ / SZ
        RETURN
        END
C-----------------------------------------------------------------------
C       SETS UP THE VIEW POINT
C-----------------------------------------------------------------------
        SUBROUTINE SVIEWP(VIEW)
        REAL VIEW(3),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        LOGICAL VIEWNG
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /DL/ XL,YL,ZL,DIS
C---
C---
        CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
        DIS     = DIS * 3.05
        VIEW(1) = MAXQX + DIS
CXX     CHANGED (EJD AUG. 1985)
CXX     VIEW(2) = MAXQY + DIS
        VIEW(2) = MAXQY + DIS*0.8
        VIEW(3) = MAXQZ + DIS
        IF (VIEWNG(VIEW)) THEN
          VIEW(1) = VIEW(1) + DIS
          VIEW(2) = VIEW(2) + DIS
          VIEW(3) = VIEW(3) + DIS
          IF (VIEWNG(VIEW)) THEN
            VIEW(1) = ABS(VIEW(1))
            VIEW(2) = ABS(VIEW(2))
            VIEW(3) = ABS(VIEW(3))
          END IF
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       THIS FUNCTION CHECKS IF THE VIEW POINT IS NEGATIVE
C-----------------------------------------------------------------------
        LOGICAL FUNCTION VIEWNG(V)
        REAL V(3)
        VIEWNG = .FALSE.
        IF (MIN(V(1),V(2),V(3)).LT.0) VIEWNG = .TRUE.
        RETURN
        END
C-----------------------------------------------------------------------
C       USED FOR TRANSLATION, SCALING AND ROTATION
C-----------------------------------------------------------------------
        SUBROUTINE SO3D(CODE,TRAN)
        REAL TRAN(4,4),T(4,4),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ,L
        INTEGER CODE
        COMMON /TRANSF/ TX,TY,TZ,RZ
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
C---
C---
        CALL IDENTI(T)
C---                                    *MOVE ORIGIN
        IF (CODE.EQ.1) GO TO 1
        T(4,1) = - AVERX
        T(4,2) = - AVERY
        T(4,3) = - AVERZ
        CALL MULTMA(TRAN,T)
        CALL IDENTI(T)
        GO TO(1,2,3,4,5,6) CODE
C---                                    *TRANSLATION
 1      T(4,1) = TX
        T(4,2) = TY
        T(4,3) = TZ
        CALL MULTMA(TRAN,T)
        RETURN
C---                                    *SCALING
 2      T(1,1) = TX
        T(2,2) = TY
        T(3,3) = TZ
        CALL MULTMA(TRAN,T)
        GO TO 10
C---                                    *ROTATION ABOUT X AXIS
 3      T(2,2) =   COS(TX)
        T(2,3) =   SIN(TX)
        T(3,2) = - SIN(TX)
        T(3,3) =   COS(TX)
        CALL MULTMA(TRAN,T)
        GO TO 10
C---                                    *ROTATION ABOUT Y AXIS
 4      T(1,1) =   COS(TY)
        T(1,3) = - SIN(TY)
        T(3,1) =   SIN(TY)
        T(3,3) =   COS(TY)
        CALL MULTMA(TRAN,T)
        GO TO 10
C---                                    *ROTATION ABOUT Z AXIS
 5      T(1,1) =   COS(TZ)
        T(1,2) =   SIN(TZ)
        T(2,1) = - SIN(TZ)
        T(2,2) =   COS(TZ)
        CALL MULTMA(TRAN,T)
        GO TO 10
C---                                    *ROTATION ABOUT OTHERAXIS
 6      V = SQRT(TY**2 + TZ**2)
        L = SQRT(TX**2 + TY**2 + TZ**2)
C---                                    *ROTATION ABOUT X AXIS
        T(2,2) =   TZ / V
        T(2,3) =   TY / V
        T(3,2) = - TY / V
        T(3,3) =   TZ / V
        CALL MULTMA(TRAN,T)
        CALL IDENTI(T)
C---                                    *ROTATION ABOUT Y AXIS
        T(1,1) =   V / L
        T(1,3) =   TX / L
        T(3,1) = - TX / L
        T(3,3) =   V / L
        CALL MULTMA(TRAN,T)
        CALL IDENTI(T)
C---                                    *ROTATION ABOUT Z AXIS
        T(1,1) =   COS(RZ)
        T(1,2) =   SIN(RZ)
        T(2,1) = - SIN(RZ)
        T(2,2) =   COS(RZ)
        CALL MULTMA(TRAN,T)
        CALL IDENTI(T)
C---                                    *INVERT ROTATION
        T(1,1) =   V / L
        T(1,3) = - TX / L
        T(3,1) =   TX / L
        T(3,3) =   V / L
        CALL MULTMA(TRAN,T)
        CALL IDENTI(T)
C---                                    *INVERT ROTATION
        T(2,2) =   TZ / V
        T(2,3) = - TY / V
        T(3,2) =   TY / V
        T(3,3) =   TZ / V
        CALL MULTMA(TRAN,T)
 10     CONTINUE
C---                                    *INVERT
        CALL IDENTI(T)
        T(4,1) = AVERX
        T(4,2) = AVERY
        T(4,3) = AVERZ
        CALL MULTMA(TRAN,T)
        RETURN
        END
C-----------------------------------------------------------------------
C       INITIALIZES INFORMATION ON TRANSFORMATION
C       AND MIN, MAX, AVER OF THE CURVES
C-----------------------------------------------------------------------
        SUBROUTINE CINFO (INFO)
        REAL INFO(13,3)
C---
C---
        DO 2 I=1,4
          DO 1 J=1,3
            IF (I.EQ.2) THEN
              INFO(I,J) = 1
            ELSE
              INFO(I,J) = 0
            END IF
 1        CONTINUE
 2      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       KEEPS INFORMATION ON TRANSLATION AND MINIMUM,
C       MAXIMUM, AVERAGE
C-----------------------------------------------------------------------
        SUBROUTINE KINFO(INFO,VIEW,INDEX)
        REAL INFO(13,3),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        REAL VIEW(3)
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
C---
C---
        I         = INDEX
        INFO(I,1) = MINQX
        INFO(I,2) = MINQY
        INFO(I,3) = MINQZ
C---
        I         = I + 1
        INFO(I,1) = MAXQX
        INFO(I,2) = MAXQY
        INFO(I,3) = MAXQZ
C---
        I         = I + 1
        INFO(I,1) = AVERX
        INFO(I,2) = AVERY
        INFO(I,3) = AVERZ
C---
        I         = 13
        INFO(I,1) = VIEW(1)
        INFO(I,2) = VIEW(2)
        INFO(I,3) = VIEW(3)
        RETURN
        END
C-----------------------------------------------------------------------
C       INITIALIZES INFORMATION IN ARRAYINFO
C-----------------------------------------------------------------------
        SUBROUTINE ININFO(INFO,VIEW,MIMA)
        REAL INFO(13,3),VIEW(3),MIMA(3,2)
C---
C---
        CALL CINFO(INFO)
        INFO(5,1) = MIMA(1,1)
        INFO(5,2) = MIMA(2,1)
        INFO(5,3) = MIMA(3,1)
C---
        INFO(6,1) = MIMA(1,2)
        INFO(6,2) = MIMA(2,2)
        INFO(6,3) = MIMA(3,2)
C---
        INDEX     = 7
        CALL KINFO(INFO,VIEW,INDEX)
        INDEX     = 10
        CALL KINFO(INFO,VIEW,INDEX)
        RETURN
        END
C-----------------------------------------------------------------------
C        DEFINE ORIGIN AS AVERAGE AND ERASE TRANSFORMATION
C-----------------------------------------------------------------------
        SUBROUTINE DFCOOR(TRAN,INFO,VIEW)
        REAL TRAN(4,4),INFO(13,3),VIEW(3),MINQX,MINQY,MINQZ
        REAL MAXQX,MAXQY,MAXQZ
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
C---
C---
        MINQX = INFO(5,1)
        MINQY = INFO(5,2)
        MINQZ = INFO(5,3)
C---
        MAXQX = INFO(6,1)
        MAXQY = INFO(6,2)
        MAXQZ = INFO(6,3)
C---
        CALL SETSMA(INFO)
        CALL IDENTI(TRAN)
        CALL DFUNIT(TRAN)
        CALL CINFO(INFO)
        CALL SVIEWP(VIEW)
        CALL CINFO(INFO)
        INDEX = 10
        CALL KINFO(INFO,VIEW,INDEX)
        RETURN
        END
C-----------------------------------------------------------------------
C        SETS SCALING TO 2 FOR <D2>,<D3>,<D4>
C-----------------------------------------------------------------------
         SUBROUTINE SCTWO(TRAN,INFO)
         REAL TRAN(4,4),INFO(13,3)
         COMMON /TRANSF/ TX,TY,TZ,RZ
C---
C---
         TX    = 2
         TY    = 2
         TZ    = 2
         ICODE = 2
         CALL SO3D(ICODE,TRAN)
C---
         INFO(2,1) = 2
         INFO(2,2) = 2
         INFO(2,3) = 2
         RETURN
         END
C-----------------------------------------------------------------------
C        GETS <3D> COMMAND FOR TRANSFORMATION, DISPLAY
C        OR HARD COPY PLOT.
C-----------------------------------------------------------------------
        SUBROUTINE ENT3DC(MIMA,VIEW,COM)
        CHARACTER*10 COMAND,OST1*80,OST2*80,ISTR*80
        CHARACTER*1 CH,WAXIS(3),COM*2,TIT3C(3)*15
        INTEGER XAXIS,YAXIS,ZAXIS,ENLAB(1999),CODE,ERROR,SLAB(1999)
        INTEGER CLAB(1999),CNUM,CERROR,NVX(2),SPLPT
        INTEGER PARRY(1999),NAX(1999),DAX(1999),CTIT3C(3)
        REAL TRAN(4,4),MIMA(3,2),VIEW(3),ROWIND(4,4)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        REAL CMIMA(3,2),CVIEW(3),INFO(13,3)
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
        LOGICAL PLTR,POINT,CAXIS,CIR
        LOGICAL DFT,USR,BRNCH,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
        LOGICAL DFDOT,MIDDLE,DIAXIS,B3DC,ST3D
        LOGICAL PJXY,PJYZ,PJXZ,VALID,PSL,DCOOR,PRNUM
C---
        COMMON /UOPTS/ DFT,USR,BRNCH,
     +  ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /TRANSF/ TX,TY,TZ,RZ
        COMMON /PLOTT/ PLTR,POINT
        COMMON /INKA/ CAXIS
        COMMON /DS/ CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,ST3D
        COMMON /DS1/ DAINT,DOINT,PSIX,PFOUR
        COMMON /ROWI/ ROWIND
        COMMON /SYMB/ KTYPE
        COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /PJXYZ/ PJXY,PJYZ,PJXZ,IPJ,PSL
        COMMON /UNIT1/ SX,SY,SZ
        COMMON /DAXIS1/ NAX,DAX,NDIM
        COMMON /CB3D/ B3DC
        COMMON /SPNUM/ SP1,SP2,SP3,SP4
        COMMON /T3D/ CTIT3C,TIT3C
        DATA WAXIS/'X','Y','Z'/
C---
C---
C---
        CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
        IWRITE = ITERM
        MIDDLE = .FALSE.
        DIAXIS = .TRUE.
        DFDOT  = .FALSE.
        CIR    = .TRUE.
        PLTR   = .FALSE.
        CHRAD  = ACOS(-1.0) / 180.0
        DAINT  = 50.0
        DOINT  = 125.0
        PSIX   = 0.6
        PFOUR  = 0.4
        KTYPE  = 3
        POINT  = .TRUE.
        DFT    = .TRUE.
        TIT    = .FALSE.
        AXLB   = .FALSE.
        CALL IDENTI(ROWIND)
        CALL ININFO(INFO,VIEW,MIMA)
        CALL DFCOOR(TRAN,INFO,VIEW)
        AINFO = 0
        PJXY  = .FALSE.
        PJYZ  = .FALSE.
        PJXZ  = .FALSE.
        PSL   = .FALSE.
        DCOOR = .TRUE.
        PRNUM = .FALSE.
        ST3D  = .FALSE.
        IDP2C = 0
        COM   = '  '
        IF (B3DC) THEN
          OST1 = ' ENTER <B3D> COMMAND :'
        ELSE
          OST1 = ' ENTER <3D> COMMAND :'
        END IF
 1      WRITE(IWRITE,10) OST1
 3      CALL READS1(COMAND)
 50     CONTINUE
C---                                    *D1
        IF (COMAND.EQ.'D1'.OR.COMAND.EQ.'d1') THEN
          AVERX = INFO(9,1)
          AVERY = INFO(9,2)
          AVERZ = INFO(9,3)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          KTYPE = 3
          CIR   = .TRUE.
          TIT   = .FALSE.
          AXLB  = .FALSE.
          DFDOT = .FALSE.
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          DCOOR  = .TRUE.
          PRNUM  = .FALSE.
          ST3D   = .FALSE.
          IDP2C  = 0
          COMAND = 'DIS'
        END IF
C---                                    *D2
        IF (COMAND.EQ.'D2'.OR.COMAND.EQ.'d2') THEN
          AVERX = 0
          AVERY = 0
          AVERZ = 0
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          CALL SCTWO(TRAN,INFO)
          KTYPE  = 3
          CIR    = .TRUE.
          TIT    = .FALSE.
          AXLB   = .FALSE.
          DFDOT  = .TRUE.
          PJXY   = .TRUE.
          PJYZ   = .TRUE.
          PJXZ   = .TRUE.
          IPJ    = 3
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          PSL    = .FALSE.
          DCOOR  = .TRUE.
          PRNUM  = .FALSE.
          ST3D   = .FALSE.
          IDP2C  = 0
          COMAND = 'DIS'
        END IF
C---                                    *D3
        IF (COMAND.EQ.'D3'.OR.COMAND.EQ.'d3') THEN
          AVERX = MIMA(1,1)
          AVERY = MIMA(2,1)
          AVERZ = MIMA(3,1)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          CALL SCTWO(TRAN,INFO)
          KTYPE  = 3
          CIR    = .TRUE.
          TIT    = .FALSE.
          AXLB   = .FALSE.
          DFDOT  = .FALSE.
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          DCOOR  = .TRUE.
          PRNUM  = .FALSE.
          ST3D   = .FALSE.
          IDP2C  = 0
          COMAND = 'DIS'
        END IF
C---                                    *D4
        IF (COMAND.EQ.'D4'.OR.COMAND.EQ.'d4') THEN
          AVERX = MIMA(1,1)
          AVERY = MIMA(2,1)
          AVERZ = MIMA(3,1)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          CALL SCTWO(TRAN,INFO)
          KTYPE  = 3
          CIR    = .TRUE.
          TIT    = .FALSE.
          AXLB   = .FALSE.
          DFDOT  = .TRUE.
          PJXY   = .TRUE.
          PJYZ   = .TRUE.
          PJXZ   = .TRUE.
          PSL    = .FALSE.
          DCOOR  = .TRUE.
          IPJ    = 3
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          PRNUM  = .FALSE.
          ST3D   = .FALSE.
          IDP2C  = 0
          COMAND = 'DIS'
        END IF
C---                                    *D5
        IF (COMAND.EQ.'D5'.OR.COMAND.EQ.'d5') THEN
          AVERX = INFO(9,1)
          AVERY = INFO(9,2)
          AVERZ = INFO(9,3)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          KTYPE  = 3
          CIR    = .TRUE.
          TIT    = .FALSE.
          AXLB   = .FALSE.
          DFDOT  = .FALSE.
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          DCOOR  = .FALSE.
          PRNUM  = .FALSE.
          ST3D   = .FALSE.
          IDP2C  = 0
          COMAND = 'DIS'
        END IF
C---                                    *D6
        IF (COMAND.EQ.'D6'.OR.COMAND.EQ.'d6') THEN
          AVERX = INFO(9,1)
          AVERY = INFO(9,2)
          AVERZ = INFO(9,3)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          KTYPE  = 3
          CIR    = .TRUE.
          TIT    = .FALSE.
          AXLB   = .FALSE.
          DFDOT  = .TRUE.
          PJXY   = .TRUE.
          PJYZ   = .TRUE.
          PJXZ   = .TRUE.
          PSL    = .FALSE.
          IPJ    = 3
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          DCOOR  = .FALSE.
          PRNUM  = .FALSE.
          ST3D   = .FALSE.
          IDP2C  = 0
          COMAND = 'DIS'
        END IF
C---                                    *TRANSLATION
        IF (COMAND.EQ.'TR'.OR.COMAND.EQ.'tr') THEN
          OST2 = ' TRANSLATION, ENTER THREE TRANSLATIONS FOR (X,Y,Z)'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) TX,TY,TZ
          INFO(1,1) = INFO(1,1) + TX
          INFO(1,2) = INFO(1,2) + TY
          INFO(1,3) = INFO(1,3) + TZ
          CODE      = 1
          CALL SO3D(CODE,TRAN)
          CALL SO3D(CODE,ROWIND)
          GO TO 1
        END IF
C---                                    *SCALING
        IF (COMAND.EQ.'SC'.OR.COMAND.EQ.'sc') THEN
          OST2 = ' SCALING, ENTER THREE SCALING FACTORS FOR (X,Y,Z)'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) TX,TY,TZ
          INFO(2,1) = INFO(2,1) * TX
          INFO(2,2) = INFO(2,2) * TY
          INFO(2,3) = INFO(2,3) * TZ
          CODE      = 2
          CALL SO3D(CODE,TRAN)
          GO TO 1
        END IF
C---                                    *ROTATION ABOUT X AXIS
        IF (COMAND.EQ.'RX'.OR.COMAND.EQ.'rx') THEN
          OST2 = ' ROTATION ABOUT X AXIS, ENTER DEGREES'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) TX
          INFO(3,1) = INFO(3,1) + TX
          TX        = TX * CHRAD
          CODE      = 3
          CALL SO3D(CODE,TRAN)
          CALL SO3D(CODE,ROWIND)
          GO TO 1
        END IF
C---                                    *ROTATION ABOUT Y AXIS
        IF (COMAND.EQ.'RY'.OR.COMAND.EQ.'ry') THEN
          OST2 = ' ROTATION ABOUT Y AXIS, ENTER DEGREES'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) TY
          INFO(3,2) = INFO(3,2) + TY
          TY        = TY * CHRAD
          CODE      = 4
          CALL SO3D(CODE,TRAN)
          CALL SO3D(CODE,ROWIND)
          GO TO 1
        END IF
C---                                    *ROTATION ABOUT Z AXIS
        IF (COMAND.EQ.'RZ'.OR.COMAND.EQ.'rz') THEN
          OST2 = ' ROTATION ABOUT Z AXIS, ENTER DEGREES'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) TZ
          INFO(3,3) = INFO(3,3) + TZ
          TZ        = TZ * CHRAD
          CODE      = 5
          CALL SO3D(CODE,TRAN)
          CALL SO3D(CODE,ROWIND)
          GO TO 1
        END IF
C---                                    *ROTATION ABOUT OTHER A
        IF (COMAND.EQ.'RA'.OR.COMAND.EQ.'ra') THEN
          OST2 = ' ROTATION ABOUT ANOTHER AXIS THROUGH (0,0,0)'
          WRITE(IWRITE,10) OST2
          OST2 = ' ENTER THREE COORDINATES TO DEFINE AXIS'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) TX,TY,TZ
          IF (TY.EQ.0.AND.TZ.EQ.0) THEN
            OST2 = ' Y AND Z CAN NOT BE BOTH ZERO '
            WRITE(IWRITE,10) OST2
            GO TO 1
          END IF
          OST2 = ' ENTER ANGLE OF ROTATION IN DEGREES'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) RZ
          INFO(4,1) = INFO(4,1) + TX
          INFO(4,2) = INFO(4,2) + TY
          INFO(4,3) = INFO(4,3) + TZ
          AINFO     = AINFO + RZ
          RZ        = RZ * CHRAD
          CODE      = 6
          CALL SO3D(CODE,TRAN)
          CALL SO3D(CODE,ROWIND)
          GO TO 1
        END IF
C---                                    *IDENTIFY THE LABELS IN UNIT 8
C---                                    *AND LABELS DEFINED BY USER
        IF (.NOT.B3DC) THEN
          IF (COMAND.EQ.'LAB'.OR.COMAND.EQ.'lab') THEN
            CALL PRLAB
            WRITE(IWRITE,43) (ENLAB(I),I=1,NUMLB)
 43         FORMAT(/,2X,'THE LABELS DEFINED ARE :',20(T28,10I5,/))
            WRITE(IWRITE,44) XAXIS,YAXIS,ZAXIS
 44         FORMAT(/,2X,'AXES DEFINED  ARE :',3I3,/)
            GO TO 1
          END IF
C---                                    *GET NEW LABELS FROM USER
          IF (COMAND.EQ.'3D'.OR.COMAND.EQ.'3d')THEN
            CALL PRLAB
            WRITE(IWRITE,43) (ENLAB(I),I=1,NUMLB)
            OST2 = ' '
            WRITE(IWRITE,10) OST2
            OST2 = ' ENTER LABELS '
            WRITE(IWRITE,10) OST2
            CALL READS8(ISTR)
            II = 1
            IF (ISTR(1:1).EQ.' ') II = II + 1
            CNUM = NUMLB
            DO 101 I=1,NUMLB
              CLAB(I) = ENLAB(I)
101         CONTINUE
            CALL GTLB3D(ISTR,II,CERROR)
            IF (CERROR.EQ.0) THEN
              CALL GETNAX(SLAB,NLAB,ENLAB,NUMLB,NAX,NAXIS)
              CAXIS = .TRUE.
              CALL GETDIM(CMIMA)
              DO 103 I =1,3
                VIEW(I)   = CVIEW(I)
                MIMA(I,1) = CMIMA(I,1)
                MIMA(I,2) = CMIMA(I,2)
 103          CONTINUE
              CALL ININFO(INFO,VIEW,MIMA)
              CALL DFCOOR(TRAN,INFO,VIEW)
              CALL IDENTI(ROWIND)
              KTYPE  = 3
              DFDOT  = .FALSE.
              MIDDLE = .FALSE.
              DIAXIS = .TRUE.
              DCOOR  = .TRUE.
              CIR    = .TRUE.
              DAINT  = 50.0
              DOINT  =125.0
              PSIX   = 0.6
              PFOUR  = 0.4
              TIT    = .FALSE.
              AXLB   = .FALSE.
              PRNUM  = .FALSE.
            ELSE
              NUMLB = CNUM
              DO 105 I=1,CNUM
 105            ENLAB(I) = CLAB(I)
            END IF
            ST3D  = .FALSE.
            IDP2C = 0
            GO TO 1
          END IF
        END IF
C---                                    *BRANCH NUMBERS
        IF (COMAND.EQ.'BR'.OR.COMAND.EQ.'br') THEN
          POINT = .TRUE.
          GO TO 1
        END IF
C---                                    *RESET BRANCH NUMBERS
        IF (COMAND.EQ.'DBR'.OR.COMAND.EQ.'dbr') THEN
          POINT = .FALSE.
          GO TO 1
        END IF
C---                                    *SET WINDOW
        IF (COMAND.EQ.'SN'.OR.COMAND.EQ.'sn') THEN
          CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
          GO TO 1
        END IF
C---                                    *SCREEN
        IF (COMAND.EQ.'SU'.OR.COMAND.EQ.'su') THEN
          WRITE(IWRITE,60) MINSX,MAXSX,MINSY,MAXSY,SP1
          CALL READS8(ISTR)
          II = 1
          IF (ISTR(1:1).EQ.' ') II = II + 1
          CALL CONVT(ISTR,II,NA,PARRY,VALID)
          IF (.NOT.VALID) GO TO 1
          IF (NA.NE.4) THEN
            OST2 = ' ERROR - INVALID SIZE'
            WRITE(IWRITE,10) OST2
            GO TO 1
          END IF
          IX1   = MIN(PARRY(1),PARRY(2))
          IX2   = MAX(PARRY(1),PARRY(2))
          IY1   = MIN(PARRY(3),PARRY(4))
          IY2   = MAX(PARRY(3),PARRY(4))
          MINSX = IX1
          MAXSX = IX2
          MINSY = IY1
          MAXSY = IY2
          GO TO 1
        END IF
C---                                    *SET UP TITLE
        IF (COMAND.EQ.'ST'.OR.COMAND.EQ.'st') THEN
          CALL ENTGRD(1)
          TIT  = .TRUE.
          AXLB = .TRUE.
          OST2 = ' AXES LABELS ?  ( <Y> OR <N> ) '
          WRITE(ITERM,10) OST2
          CALL READ1(CH)
          IF (CH.EQ.'Y'.OR.CH.EQ.'y') THEN
            CALL TIT3D
            ST3D = .TRUE.
          END IF
          GO TO 1
        END IF
C---                                    *RESET <ST>
        IF (COMAND.EQ.'DST'.OR.COMAND.EQ.'dst') THEN
          TIT  = .FALSE.
          AXLB = .FALSE.
          ST3D = .FALSE.
          GO TO 1
        END IF
C---                                    *ERASE TRANSFORMATION
        IF (COMAND.EQ.'ET'.OR.COMAND.EQ.'et') THEN
          CALL IDENTI(ROWIND)
          CALL DFCOOR(TRAN,INFO,VIEW)
          AINFO = 0
          GO TO 1
        END IF
C---                                    *CLEAR SCREEN
        IF (COMAND.EQ.'CLR'.OR.COMAND.EQ.'clr'.OR.
     +      COMAND.EQ.'CL '.OR.COMAND.EQ.'cl ') THEN
          CALL PLCMDS(1)
          CALL PLCHDW(3)
          GO TO 1
        END IF
C---                                    *CHANGE COORDINATES
        IF (COMAND.EQ.'AX'.OR.COMAND.EQ.'ax') THEN
          WRITE(IWRITE,44) XAXIS,YAXIS,ZAXIS
          IF (.NOT.B3DC) THEN
            CAXIS = .TRUE.
            CALL GETDIM(MIMA)
          ELSE
            CALL GAXB3D(MIMA,ERROR)
            IF (ERROR.NE.0) GO TO 1
          END IF
          CALL ININFO(INFO,VIEW,MIMA)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          DAINT = 50.0
          DOINT =125.0
          IDP2C = 0
          GO TO 1
        END IF
C---                                    *SAVE PLOT IN UNIT 16
        IF (COMAND.EQ.'SAV'.OR.COMAND.EQ.'sav') THEN
          IWRITE = ISAVE
	  CALL CRFLNM(1)
          CALL SWIND(TRAN,VIEW,MIMA,INFO,IDP2C)
          CALL PLCHDW(3)
	  CALL CRFLNM(2)
          IWRITE = ITERM
          CALL COMPLT
          GO TO 1
        END IF
C---                                    *PLOTTER
        IF (COMAND.EQ.'PLOT'.OR.COMAND.EQ.'plot') THEN
          PLTR = .TRUE.
          CALL SWIND(TRAN,VIEW,MIMA,INFO,IDP2C)
          CALL PLCHDW(3)
          PLTR = .FALSE.
          GO TO 1
        END IF
C---                                    *SET UP 2 DIFFERENT VIEW POINTS
        IF (COMAND.EQ.'DVX'.OR.COMAND.EQ.'dvx') THEN
          IDP2C = 1
          GO TO 1
        END IF
        IF (COMAND.EQ.'DVY'.OR.COMAND.EQ.'dvy') THEN
          IDP2C = 2
          GO TO 1
        END IF
C---                                    *SOLID LINES
        IF (COMAND.EQ.'SL'.OR.COMAND.EQ.'sl') THEN
          KTYPE = 1
          GO TO 1
        END IF
C---                                    *DASHES
        IF (COMAND.EQ.'DA'.OR.COMAND.EQ.'da') THEN
          KTYPE = 2
          GO TO 1
        END IF
C---                                    *SOLID AND DASHED LINES
        IF (COMAND.EQ.'SA'.OR.COMAND.EQ.'sa') THEN
          KTYPE = 3
          GO TO 1
        END IF
C---                                    *DOTTED LINES
        IF (COMAND.EQ.'DO'.OR.COMAND.EQ.'do') THEN
          KTYPE = 4
          DFDOT = .FALSE.
          GO TO 1
        END IF
C---                                    * SOLID AND DOTTED
        IF (COMAND.EQ.'SO'.OR.COMAND.EQ.'so') THEN
          KTYPE = 5
          GO TO 1
        END IF
C---                                    * HEAVY LINE
       IF (COMAND.EQ.'HL'.OR.COMAND.EQ.'hl') THEN
          KTYPE = 6
          GO TO 1
        END IF
C---                                    *PROJECTION
      IF  (COMAND.EQ.'PJ'.OR.COMAND.EQ.'pj') THEN
         DFDOT = .TRUE.
         IPJ   = 3
         PJXY  = .TRUE.
         PJYZ  = .TRUE.
         PJXZ  = .TRUE.
         PSL   = .FALSE.
         GO TO 1
       END IF
C---                                    *PROJECTION DEFINED BY USER
       IF (COMAND.EQ.'PJU'.OR.COMAND.EQ.'pju') THEN
         OST2 = '  ENTER <1> FOR PROJECTION ON (X,Y,Z0)'
         WRITE(IWRITE,10) OST2
         OST2 = '  ENTER <2> FOR PROJECTION ON (X0,Y,Z)'
         WRITE(IWRITE,10) OST2
         OST2 = '  ENTER <3> FOR PROJECTION ON (X,Y0,Z)'
         WRITE(IWRITE,10) OST2
         CALL READS8(ISTR)
         II = 1
         IF (ISTR(1:1).EQ.' ') II = 2
         CALL CONVT(ISTR,II,NA,PARRY,VALID)
         IF (.NOT.VALID) GO TO 1
         IF (NA.GT.3) THEN
           OST2 = '  ERROR - CODE CANNOT BE GREATER THAN 3'
           WRITE(IWRITE,10) OST2
           GO TO 1
         END IF
         PJXY = .FALSE.
         PJYZ = .FALSE.
         PJXZ = .FALSE.
         IPJ  = 0
         DO 110 I = 1,NA
           IF (PARRY(I).EQ.1.AND..NOT.PJXY) THEN
             IPJ  = IPJ + 1
             PJXY = .TRUE.
           END IF
           IF (PARRY(I).EQ.2.AND..NOT.PJYZ) THEN
             IPJ  = IPJ + 1
             PJYZ = .TRUE.
           END IF
           IF (PARRY(I).EQ.3.AND..NOT.PJXZ) THEN
             IPJ  = IPJ + 1
             PJXZ = .TRUE.
           END IF
 110     CONTINUE
         IF (IPJ .GE. 1) THEN
           DFDOT = .TRUE.
           PSL   = .FALSE.
         ELSE
           DFDOT = .FALSE.
         END IF
         GO TO 1
       END IF
C---                                    *PROJECTION USING SOLID LINE
       IF (COMAND.EQ.'PSL'.OR.COMAND.EQ.'psl') THEN
         PSL = .TRUE.
         GO TO 1
       END IF
C---                                    *RESET PROJECTION
       IF (COMAND.EQ.'DPJ'.OR.COMAND.EQ.'dpj') THEN
         DFDOT = .FALSE.
         GO TO 1
       END IF
C---                                    *DEFINE DASHES
       IF (COMAND.EQ.'DDL'.OR.COMAND.EQ.'ddl') THEN
         OST2 = ' OLD DASH LENGTH IS'
         WRITE(IWRITE,30) OST2(1:22),PSIX
 30      FORMAT(A,F6.2)
         OST2 = ' ENTER NEW LENGTH'
         WRITE(IWRITE,10) OST2
         READ(5,*,ERR=1) UNIT
         IF (UNIT.LE.0.OR.UNIT.GE.1) THEN
           OST2 = ' ERROR :   0 < LENGTH < 1'
           WRITE(IWRITE,10) OST2
           GO TO 1
         END IF
         PSIX  = UNIT
         PFOUR = 1.0 - UNIT
         GO TO 1
       END IF
C---                                    *RESET DASHES, DOTS
        IF (COMAND.EQ.'DDD'.OR.COMAND.EQ.'ddd') THEN
          PSIX  = 0.6
          PFOUR = 0.4
          DAINT = 50.0
          DOINT =125.0
          GO TO 1
        END IF
C---                                    *DEFINE NUMBER OF DOTS
        IF (COMAND.EQ.'SDO'.OR.COMAND.EQ.'sdo') THEN
          CALL DEFINE(DOINT,2)
          GO TO 1
        END IF
C---                                    *DEFINE NUMBER OF DASHES
        IF (COMAND.EQ.'SDA'.OR.COMAND.EQ.'sda') THEN
          CALL DEFINE(DAINT,1)
          GO TO 1
        END IF
C---                                    *VIEW POINT
        IF (COMAND.EQ.'VU'.OR.COMAND.EQ.'vu') THEN
          OST2 =
     +    '          MIN           MAX           COOR          VIEW'
          WRITE(IWRITE,10) OST2
          WRITE(IWRITE,20) WAXIS(1),MINQX,MAXQX,AVERX,VIEW(1)
          WRITE(IWRITE,20) WAXIS(2),MINQY,MAXQY,AVERY,VIEW(2)
          WRITE(IWRITE,20) WAXIS(3),MINQZ,MAXQZ,AVERZ,VIEW(3)
 20       FORMAT(2X,A1,4(2X,F12.5))
          OST2 = ' ENTER VIEW POINT '
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) VIEWX,VIEWY,VIEWZ
          IF (VIEWZ.EQ.0) THEN
            OST2 = ' VIEW POINT ON Z AXIS CAN NOT BE ZERO'
            WRITE(IWRITE,10) OST2
            GO TO 1
          END IF
          VIEW(1)    = VIEWX
          VIEW(2)    = VIEWY
          VIEW(3)    = VIEWZ
          INFO(13,1) = VIEWX
          INFO(13,2) = VIEWY
          INFO(13,3) = VIEWZ
          GO TO 1
        END IF
C---                                    *VIEW POINT WITH DISTANCE
        IF (COMAND.EQ.'VD'.OR.COMAND.EQ.'vd') THEN
          CALL SVIEWP(VIEW)
          INFO(13,1) = VIEW(1)
          INFO(13,2) = VIEW(2)
          INFO(13,3) = VIEW(3)
          GO TO 1
        END IF
C---                                    *DISPLAY ORIGIN AT AVERAGE
        IF (COMAND.EQ.'C1'.OR.COMAND.EQ.'c1') THEN
          MIDDLE = .FALSE.
          DIAXIS = .FALSE.
          DCOOR  = .TRUE.
          GO TO 1
        END IF
C---                                    *DISPLAY ORIGIN AT AVERAGE
C---                                    *AND PLOT COORDINATE NUMBER
        IF (COMAND.EQ.'C2'.OR.COMAND.EQ.'c2') THEN
          MIDDLE = .FALSE.
          DIAXIS = .TRUE.
          DCOOR  = .TRUE.
          GO TO 1
        END IF
C---                                    *DISPLAY ORIGIN AT MINIMUM
        IF (COMAND.EQ.'C3'.OR.COMAND.EQ.'c3') THEN
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          DCOOR  = .TRUE.
          GO TO 1
        END IF
        IF (COMAND.EQ.'C4'.OR.COMAND.EQ.'c4') THEN
          MIDDLE = .TRUE.
          DIAXIS = .TRUE.
          DCOOR  = .TRUE.
          GO TO 1
        END IF
        IF (COMAND.EQ.'C5'.OR.COMAND.EQ.'c5') THEN
          MIDDLE = .TRUE.
          DIAXIS = .FALSE.
          DCOOR  = .FALSE.
          GO TO 1
        END IF
C---                                    *PRINT COORDINATE NUMBER
C---                                    *AND MAXIMUM ON AXIS
        IF (COMAND.EQ.'PN'.OR.COMAND.EQ.'pn') THEN
          PRNUM = .TRUE.
          GO TO 1
        END IF
C---                                    *DELETE COORDINATE NUMBER
C---
        IF (COMAND.EQ.'DPN'.OR.COMAND.EQ.'dpn') THEN
          PRNUM = .FALSE.
          GO TO 1
        END IF
C---                                    *CIRCLE
C---                                    *(COORDINATE PLANE INTERCEPT)
        IF (COMAND.EQ.'CI'.OR.COMAND.EQ.'ci') THEN
          CIR = .TRUE.
          GO TO 1
        END IF
C---                                    *RESET CIRCLE
        IF (COMAND.EQ.'DCI'.OR.COMAND.EQ.'dci') THEN
          CIR = .FALSE.
          GO TO 1
        END IF
C---                                    *DISPLAY CURVE
        IF (COMAND.EQ.'DIS'.OR.COMAND.EQ.'dis'.OR.
     +      COMAND.EQ.'D  '.OR.COMAND.EQ.'d  ') THEN
          CALL SWIND(TRAN,VIEW,MIMA,INFO,IDP2C)
          CALL PLCHDW(3)
          GO TO 1
        END IF
C---                                    *ORIGIN AT (0,0,0)
        IF (COMAND.EQ.'CZ'.OR.COMAND.EQ.'cz') THEN
          AVERX = 0
          AVERY = 0
          AVERZ = 0
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          GO TO 1
        END IF
C---                                    *USER DEFINED ORIGIN
        IF (COMAND.EQ.'CU'.OR.COMAND.EQ.'cu') THEN
          OST2 = ' ENTER ORIGIN (X,Y,Z)'
          WRITE(IWRITE,10) OST2
          READ(5,*,ERR=1) A1,A2,A3
          AVERX = A1
          AVERY = A2
          AVERZ = A3
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          GO TO 1
        END IF
C---                                    *ORIGIN AT AVERAGE
        IF (COMAND.EQ.'CA'.OR.COMAND.EQ.'ca') THEN
          AVERX = INFO(9,1)
          AVERY = INFO(9,2)
          AVERZ = INFO(9,3)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          GO TO 1
        END IF
C---                                    *ORIGIN AT MIN(X,Y,Z)
        IF (COMAND.EQ.'CM'.OR.COMAND.EQ.'cm') THEN
          AVERX = INFO(5,1)
          AVERY = INFO(5,2)
          AVERZ = INFO(5,3)
          CALL DFCOOR(TRAN,INFO,VIEW)
          CALL IDENTI(ROWIND)
          GO TO 1
        END IF
C---                                    *PRINT INFORMATION
        IF (COMAND.EQ.'INFO'.OR.COMAND.EQ.'info') THEN
          OST2 = '  '
          WRITE(IWRITE,10) OST2
          WRITE(IWRITE,300)
          WRITE(IWRITE,10) OST2
          WRITE(IWRITE,302)
          WRITE(IWRITE,10) OST2
          DO 200 I=1,3
            WRITE(IWRITE,303) WAXIS(I),(INFO(J,I),J=1,6)
 200      CONTINUE
          WRITE(IWRITE,306) AINFO
          WRITE(IWRITE,10) OST2
          WRITE(IWRITE,301)
          WRITE(IWRITE,10) OST2
          WRITE(IWRITE,304)
          WRITE(IWRITE,10) OST2
          DO 201 I=1,3
            WRITE(IWRITE,305) WAXIS(I),(INFO(J,I),J=7,13)
 201      CONTINUE
          WRITE(IWRITE,10) OST2
          GO TO 1
        END IF
C---                                    *<2D>
        IF (.NOT.B3DC) THEN
          IF (COMAND.EQ.'2D'.OR.COMAND.EQ.'2d') THEN
            COM  = '2D'
            TIT  = .FALSE.
            AXLB = .FALSE.
            CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
            GO TO 100
          END IF
        END IF
C---                                    *TERMINATE <3D>
        IF (COMAND.EQ.'EXIT'.OR.COMAND.EQ.'exit'.OR.
     +      COMAND.EQ.'EX'  .OR.COMAND.EQ.'ex') THEN
          TIT  = .FALSE.
          AXLB = .FALSE.
          CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
          GO TO 100
        END IF
        IF (COMAND.EQ.'END'.OR.COMAND.EQ.'end'.OR.
     +      COMAND.EQ.'E'   .OR.COMAND.EQ.'e')  THEN
          TIT  = .FALSE.
          AXLB = .FALSE.
          CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
          GO TO 100
        END IF
C---                                    *LISTING OF <3D>
        IF (COMAND.EQ.'HELP'.OR.COMAND.EQ.'help') THEN
          COMAND = '         '
          CALL HELP3D(COMAND,1)
          IF (COMAND.NE.' ') GO TO 50
          GO TO 1
        END IF
        IF (COMAND.EQ.'STOP'.OR.COMAND.EQ.'stop') STOP
C---                                    *LISTING OF <3D>
        IF (COMAND.EQ.'LI  '.OR.COMAND.EQ.'li  '.OR.
     +      COMAND.EQ.'LIST'.OR.COMAND.EQ.'list') THEN
          CALL LIST3D
          GO TO 1
        END IF
        OST2 = ' <HELP> OR <LIST> IN CASE OF DIFFICULTY'
        WRITE(IWRITE,10) OST2
        OST2 = ' ILLEGAL COMMAND'
        WRITE(IWRITE,10) OST2
        GO TO 3
 100    CONTINUE
 10     FORMAT(A)
 300    FORMAT(14X,'TRANSFORMATION',18X,'     LIMITS     ')
 301    FORMAT(18X,'    GRAPH           ',16X,
     +            '    SCREEN          ')
 302    FORMAT(4X,'TRANSLATE',4X,'SCALE',4X,'ROTATE',6X,'RA',
     +         8X,'MIN',8X,'MAX')
 303    FORMAT(1X,A1,2X,F9.5,2X,F7.3,2X,F8.3,2X,F6.2,2X,
     +         F9.4,2X,F9.4)
 304    FORMAT(10X,'MIN',8X,'MAX',6X,'AVER',7X,'MIN',8X,
     +         'MAX',6X,'COOR',6X,'VIEW')
 305    FORMAT(1X,A1,2(2X,F9.4,2X,F9.4,2X,F8.4),2X,F8.4)
 306    FORMAT(/,T20,'ANGLE OF ROTATION ABOUT OTHER AXIS : ',F9.5)
 60     FORMAT(/,' THE DEFAULT SETTING FOR THE RELATIVE DIMENSION OF',
     +  ' THE SCREEN IS :',//,' MIN(X)=',F8.2,6X,'MAX(X)=',F8.2,6X,
     +  'MIN(Y)=',F8.2,6X,'MAX(Y)=',F8.2,//,
     +  ' ENTER THE NEW SCREEN DIMENSION SUCH THAT MIN(X) > ',F8.2)
        RETURN
        END
C-----------------------------------------------------------------------
C       DEFINES THE NUMBER OF DASHES AND DOTS
C-----------------------------------------------------------------------
        SUBROUTINE DEFINE(INTVAL,ICODE)
        REAL INTVAL
        CHARACTER*80 OSTR
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        IF (ICODE.EQ.1) THEN
          OSTR = ' OLD NUMBER OF DASHES IS      '
        ELSE IF (ICODE.EQ.2) THEN
          OSTR = ' OLD NUMBER OF DOTS IS        '
        ELSE IF (ICODE.EQ.3) THEN
          OSTR = ' OLD NUMBER OF CIRCLES IS     '
        ELSE IF (ICODE.EQ.4) THEN
          OSTR = ' OLD NUMBER OF SYMBOLS IS     '
        ELSE IF (ICODE.EQ.5) THEN
          OSTR = ' PLOTTING ACCURACY IS         '
        ELSE IF (ICODE.EQ.6) THEN
          OSTR = ' OLD NUMBER OF DASHES-DOTS IS '
        END IF
        WRITE(ITERM,1) OSTR(1:30),INTVAL
        OSTR = ' ENTER NEW NUMBER '
        WRITE(IWRITE,2) OSTR
        READ(5,*,ERR=3) RINTVL
        IF (RINTVL.LT.5) THEN
          OSTR = 'ERROR - NUMBER SHOULD BE >= 5'
          WRITE(IWRITE,2) OSTR
        ELSE
          INTVAL = RINTVL
        END IF
 3      RETURN
 1      FORMAT(A,F7.2)
 2      FORMAT(A80)
        END
C-----------------------------------------------------------------------
C       PRINTS INFORMATION ON <3D>
C-----------------------------------------------------------------------
        SUBROUTINE HELP3D(ISTR,ICODE)
        CHARACTER*70 OST1,BLANK,ENTC,LIST,ISTR*10
        COMMON /IO/ IWRITE,ITERM,ISAVE
C---
C---
        BLANK = ' '
        LIST = ' PRESS <RETURN> FOR MORE...'
        ENTC = ' PRESS <RETURN> FOR MORE, OR ENTER COMMAND ...'
C---
        CALL PLCMDS(1)
        WRITE(ITERM,1) BLANK
        OST1 = ' LIST OF 3D COMMANDS'
        WRITE(ITERM,1) BLANK
        WRITE(ITERM,1) BLANK
        OST1 = ' DEFAULT COMMANDS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <D1> SET ORIGIN AT "AVERAGE" OF THE CURVES,'
        WRITE(IWRITE,1) OST1
        OST1 = '       AND DISPLAY THE CURVES '
        WRITE(IWRITE,1) OST1
        OST1 = '  <D2> SET ORIGIN AT (0,0,0),'
        WRITE(IWRITE,1) OST1
        OST1 = '       AND DISPLAY THE CURVES AND THEIR PROJECTIONS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <D3> SET ORIGIN AT MIN(X,Y,Z),'
        WRITE(IWRITE,1) OST1
        OST1 = '       AND DISPLAY THE CURVES '
        WRITE(IWRITE,1) OST1
        OST1 = '  <D4> SET ORIGIN AT MIN(X,Y,Z),'
        WRITE(IWRITE,1) OST1
        OST1 = '       AND DISPLAY THE CURVES AND THEIR PROJECTIONS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <D5>                          '
        WRITE(IWRITE,1) OST1
        OST1 = '  <D6>                          '
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        IF (ICODE.EQ.1) THEN
          WRITE(IWRITE,1) ENTC
        ELSE
          WRITE(IWRITE,1) LIST
        END IF
C---
        CALL READS1(ISTR)
        IF (ISTR.NE.' ') RETURN
C---
        CALL PLCMDS(1)
        WRITE(IWRITE,1) BLANK
        OST1 = '  <LAB>  LIST LABELS IN UNIT 8,'
        WRITE(IWRITE,1) OST1
        OST1 = '         AND LABELS DEFINED BY THE USER'
        WRITE(IWRITE,1) OST1
        OST1 = '  <3D>   GET NEW LABELS FROM UNIT 8  (MAX 1999)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <AX>   CHANGE AXES'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        OST1 = '  TRANSFORMATIONS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <TR>   TRANSLATION IN (X,Y,Z)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <SC>   SCALING OF (X,Y,Z)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <RX>   ROTATION ABOUT X-AXIS, (IN DEGREES)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <RY>   ROTATION ABOUT Y-AXIS, (IN DEGREES)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <RZ>   ROTATION ABOUT Z-AXIS, (IN DEGREES)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <RA>   ROTATION ABOUT AN ABITRARY AXIS'
        WRITE(IWRITE,1) OST1
        OST1 = '         PASSING THROUGH (0,0,0) AND (PX,PY,PZ)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <ER>   RESET TRANSFORMATIONS'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        IF (ICODE.EQ.1) THEN
          WRITE(IWRITE,1) ENTC
        ELSE
          WRITE(IWRITE,1) LIST
        END IF
C---
        CALL READS1(ISTR)
        IF (ISTR.NE.' ') RETURN
C---
        CALL PLCMDS(1)
        WRITE(IWRITE,1) BLANK
        OST1 = '  CURVES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <SL>   DISPLAY ALL CURVES USING SOLID LINES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <SA>   USE SOLID  LINE WHEN INSIDE,'
        WRITE(IWRITE,1) OST1
        OST1 = '         AND DASHED LINE WHEN OUTSIDE THE MAIN OCTANT'
        WRITE(IWRITE,1) OST1
        OST1 = '  <SO>   USE SOLID  LINE WHEN INSIDE,'
        WRITE(IWRITE,1) OST1
        OST1 = '         AND DOTTED LINE WHEN OUTSIDE THE MAIN OCTANT'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DA>   DISPLAY CURVES USING DASHED LINES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DO>   DISPLAY CURVES USING DOTTED LINES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <HL>   DISPLAY CURVES USING HEAVY  LINES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <CI>   DRAW A CIRCLE AT COORDINATE-PLANE INTERCEPTS'
        WRITE(IWRITE,1) OST1
        OST1 = '         (SOLID  FOR MAIN OCTANT, OPEN OTHERWISE'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DCI>  RESET <CI> COMMAND'
        WRITE(IWRITE,1) OST1
        OST1 = '  <BR>   DISPLAY BRANCH NUMBERS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DBR>  RESET <BR> COMMAND'
        WRITE(IWRITE,1) OST1
        OST1 = '  <PN>   DISPLAY COORDINATE VALUES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DPN>  RESET <PN> COMMAND'
        WRITE(IWRITE,1) OST1
        OST1 = '  <ST>   SET UP TITLES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DST>  RESET <ST> COMMAND'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        IF (ICODE.EQ.1) THEN
          WRITE(IWRITE,1) ENTC
        ELSE
          WRITE(IWRITE,1) LIST
        END IF
C---
        CALL READS1(ISTR)
        IF (ISTR.NE.' ') RETURN
C---
        CALL PLCMDS(1)
        WRITE(IWRITE,1) BLANK
        OST1 = ' ORIGIN '
        WRITE(IWRITE,1) OST1
        OST1 = '  <CA>   SET ORIGIN AT "AVERAGE" OF THE CURVES'
        WRITE(IWRITE,1) OST1
        OST1 = '  <CZ>   SET ORIGIN AT (0,0,0)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <CU>   USER DEFINED ORIGIN'
        WRITE(IWRITE,1) OST1
        OST1 = '  <C1>   SET ORIGIN AT "AVERAGE" OF THE CURVES'
        WRITE(IWRITE,1) OST1
        OST1 = '         AND PLOT AXES NUMBERS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <C2>                                        '
        WRITE(IWRITE,1) OST1
        OST1 = '  <C3>                                        '
        WRITE(IWRITE,1) OST1
        OST1 = '  <C4>                                        '
        WRITE(IWRITE,1) OST1
        OST1 = '  <C5>   SET ORIGIN AT MIN(X,Y,Z) '
        WRITE(IWRITE,1) OST1
        OST1 = '         AND PLOT AXES NUMBERS'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        IF (ICODE.EQ.1) THEN
          WRITE(IWRITE,1) ENTC
        ELSE
          WRITE(IWRITE,1) LIST
        END IF
C---
        CALL READS1(ISTR)
        IF (ISTR.NE.' ') RETURN
C---
        CALL PLCMDS(1)
        WRITE(IWRITE,1) BLANK
        OST1 = ' PROJECTION COMMANDS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <PJ>   ALSO PLOT THE PROJECTIONS '
        WRITE(IWRITE,1) OST1
        OST1 = '         ON THE THREE COORDINATE PLANES '
        WRITE(IWRITE,1) OST1
        OST1 = '  <PJU>  PROJECTIONS CHOSEN  BY USER'
        WRITE(IWRITE,1) OST1
        OST1 = '         ENTER <1> FOR PROJECTION ON (X ,Y ,Z0)'
        WRITE(IWRITE,1) OST1
        OST1 = '               <2> FOR PROJECTION ON (X0,Y ,Z )'
        WRITE(IWRITE,1) OST1
        OST1 = '               <3> FOR PROJECTION ON (X ,Y0,Z )'
        WRITE(IWRITE,1) OST1
        OST1 = '  <PSL>  USE SOLID CURVE FOR DRAWING THE PROJECTIONS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <DPJ>  RESET PROJECTION COMMAND'
        WRITE(IWRITE,1) OST1
C---
C---
        WRITE(IWRITE,1) BLANK
        OST1 = ' VIEW POINT'
        WRITE(IWRITE,1) OST1
        OST1 = '  <VD>   SET DEFAULT VIEW POINT'
        WRITE(IWRITE,1) OST1
        OST1 = '  <VU>   VIEW POINT DEFINED BY USER'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        OST1 = ' DISPLAY COMMANDS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <D> OR <DIS> DISPLAY THE CURVES '
        WRITE(IWRITE,1) OST1
        OST1 = '  <SAV>  THIS COMMAND SAVES A PLOT IN UNIT 16'
        WRITE(IWRITE,1) OST1
        OST1 = '  <PLOT> PLOT DISPLAY ON HARDCOPY UNIT'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        IF (ICODE.EQ.1) THEN
          WRITE(IWRITE,1) ENTC
        ELSE
          WRITE(IWRITE,1) LIST
        END IF
C---
        CALL READS1(ISTR)
        IF (ISTR.NE.' ') RETURN
C---
        CALL PLCMDS(1)
        WRITE(IWRITE,1) BLANK
        OST1 = '  <SDO>  DEFINE SPACING OF DOTS,   DEFAULT IS 125'
        WRITE(IWRITE,1) OST1
        OST1 = '  <SDA>  DEFINE SPACING OF DASHES, DEFAULT IS 50'
        WRITE(IWRITE,1) OST1
        OST1 = '  <LDA>  DEFINE LENGTH OF DASHES,  DEFAULT IS 0.6'
        WRITE(IWRITE,1) OST1
        OST1 = '  <RES>  RESET <SDO>, <SDA>, AND <LDA>'
        WRITE(IWRITE,1) OST1
C---
C---
        OST1 = '  <SN>   NORMAL SCREEN (WINDOW) SIZE'
        WRITE(IWRITE,1) OST1
        OST1 = '          X : 150 - 950, AND Y : 150 - 650 (PLOT10)'
        WRITE(IWRITE,1) OST1
        OST1 = '  <SU>   TO CHANGE SCREEN (WINDOW) SIZE'
        WRITE(IWRITE,1) OST1
C---
C---
        OST1 = '  <INFO> PRINTS INFORMATION ON TRANSFORMATIONS USED,'
        WRITE(IWRITE,1) OST1
        OST1 = '         MIN, MAX, AVER, ORIGIN AND VIEW POINT'
        WRITE(IWRITE,1) OST1
        OST1 = '  <LI> OR <LIST> LISTS ALL <3D> COMMANDS'
        WRITE(IWRITE,1) OST1
        OST1 = '  <2D>   TO RETURN TO 2D'
        WRITE(IWRITE,1) OST1
        OST1 = '  <END> OR <EXIT> TERMINATE <3D> COMMAND'
        WRITE(IWRITE,1) OST1
        OST1 = '  <STOP> STOP EXECUTION'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
C---
        OST1 = '  ------------- END OF LIST -------------'
        WRITE(IWRITE,1) OST1
        WRITE(IWRITE,1) BLANK
        OST1 = '  ENTER COMMAND ...'
        WRITE(IWRITE,1) OST1
        CALL READS1(ISTR)
        RETURN
 1      FORMAT(A70)
        END
C-----------------------------------------------------------------------
C       DRAWS OUTLINE AND PRINTS TITLE
C-----------------------------------------------------------------------
        SUBROUTINE SWIND(TRAN,VIEW,MIMA,INFO,IDP2C)
        INTEGER AX1(8),AY1(8),AZ1(8)
        INTEGER SPLPT,NVX(2)
        REAL TRAN(4,4),VIEW(3),PARA(4,4),MMXYZ(3,2),PARADP(4,4)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ,ROWIND(4,4)
        REAL MIMA(3,2),INFO(13,3),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        LOGICAL DFDOT,MIDDLE,DIAXIS,ST3D,CIR,DCOOR,PRNUM
        LOGICAL PK,PLTR,POINT
        CHARACTER*1 CH
C---
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /DS/ CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,ST3D
        COMMON /DL/ XL,YL,ZL,DIS
        COMMON /ROWI/ ROWIND
        COMMON /COOR/ CRX,CRY,CRZ
        COMMON /PR/ PX,PY,PZ
        COMMON /PLOTT/ PLTR,POINT
        DATA AX1/1,1,1,1,2,2,2,2/,AY1/1,1,2,2,1,1,2,2/,
     +  AZ1/1,2,1,2,1,2,1,2/
C---
C---
        CALL IDENTI(PARA)
C---                                    *UP PARALLEL PROJECTION
        PARA(3,3)  = 0
        PARA(3,1)  = - VIEW(1) / VIEW(3)
        PARA(3,2)  = - VIEW(2) / VIEW(3)
        FOURTY     = 40.0
        CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
        XL         = XL / FOURTY
        YL         = YL / FOURTY
        ZL         = ZL / FOURTY
C---
        MMXYZ(1,1) = MINQX - XL
        MMXYZ(1,2) = MAXQX + XL
        MMXYZ(2,1) = MINQY - YL
        MMXYZ(2,2) = MAXQY + YL
        MMXYZ(3,1) = MINQZ - ZL
        MMXYZ(3,2) = MAXQZ + ZL
C---                                    *8 DIFFERENT POSITIONS
        DO 1 I=1,8
          X = MMXYZ(1,AX1(I))
          Y = MMXYZ(2,AY1(I))
          Z = MMXYZ(3,AZ1(I))
          CALL AXYZ(X,Y,Z,X0,Y0,Z0,PARA)
          IF (I.EQ.1) THEN
            XMIN = X0
            YMIN = Y0
            XMAX = X0
            YMAX = Y0
          ELSE
            IF (XMIN.GT.X0) XMIN = X0
            IF (YMIN.GT.Y0) YMIN = Y0
            IF (XMAX.LT.X0) XMAX = X0
            IF (YMAX.LT.Y0) YMAX = Y0
          END IF
 1        CONTINUE
        CALL SPAGE
C---                                    *SET UP ORIGIN
        IF (DCOOR) THEN
          CRX = AVERX
          CRY = AVERY
          CRZ = AVERZ
          PX  = AVERX
          PY  = AVERY
          PZ  = AVERZ
        ELSE
          CRX = MINQX
          CRY = MINQY
          CRZ = MINQZ
          PX  = INFO(7,1)
          PY  = INFO(7,2)
          PZ  = INFO(7,3)
        END IF
C---                                   *DIFFERENT VIEWPOINTS
        IF (IDP2C.NE.0) THEN
          CALL DISTLN(CRX,CRY,CRZ,VIEW(1),VIEW(2),VIEW(3))
          DIS1 = DIS * 0.05
          PK  = PLTR
          CALL COPYMA(PARADP,PARA)
          PARADP(3,IDP2C) = PARADP(3,IDP2C) + DIS1 / VIEW(3)
          CALL DICRAV(PARADP,TRAN,INFO)
          CALL DISP3D(TRAN,PARADP,MIMA)
          CALL PLCHDW(3)
          WRITE(6,2)
          CALL READ1(CH)
          PARADP(3,IDP2C) = PARA(3,IDP2C) - DIS1 / VIEW(3)
          PLTR = PK
          CALL SPAGE
          CALL DICRAV(PARADP,TRAN,INFO)
          CALL DISP3D(TRAN,PARADP,MIMA)
        ELSE
          CALL DICRAV(PARA,TRAN,INFO)
          CALL DISP3D(TRAN,PARA,MIMA)
        END IF
        RETURN
 2      FORMAT(' <RETURN> FOR DIFFERENT VIEW POINT')
        END
C-----------------------------------------------------------------------
C       RECEIVES DATA FOR TITLE ON 3D COORDINATE AXES
C-----------------------------------------------------------------------
        SUBROUTINE TIT3D
        CHARACTER*15 TIT3C(3),IST,OST*50
        INTEGER CTIT3C(3)
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /T3D/ CTIT3C,TIT3C
C---
C---
        OST = ' ENTER AXES LABELS FOR (X,Y,Z) '
        WRITE(ITERM,5) OST
        OST = ' BETWEEN THE QUOTE'
        WRITE(ITERM,5) OST
        DO 2 I=1,3
          WRITE(ITERM,4)
          TIT3C(I)  = ' '
          CTIT3C(I) = 0
          IST       = ' '
          READ(5,3,END=2) IST
          TIT3C(I) = IST
          DO 1 J=1,15
            IF (IST(J:).NE.' ') CTIT3C(I) = J
 1        CONTINUE
 2      CONTINUE
        RETURN
 3      FORMAT(1X,A15)
 4      FORMAT(T2,'"',T18,'"')
 5      FORMAT(A50)
        END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        SUBROUTINE FMM(D1,D2,TRAN)
        REAL D1(3,2),D2(3,2),TRAN(4,4)
        INTEGER AX(8),AY(8),AZ(8)
        DATA AX/1,1,1,1,2,2,2,2/,AY/1,1,2,2,1,1,2,2/
        DATA AZ/1,2,1,2,1,2,1,2/
C---
C---
        DO 1 I=1,8
          X = D1(1,AX(I))
          Y = D1(2,AY(I))
          Z = D1(3,AZ(I))
          CALL AXYZ(X,Y,Z,X0,Y0,Z0,TRAN)
          IF (I.EQ.1) THEN
            D2(1,1) = X0
            D2(1,2) = X0
            D2(2,1) = Y0
            D2(2,2) = Y0
            D2(3,1) = Z0
            D2(3,2) = Z0
          ELSE
            IF (D2(1,1).GT.X0) D2(1,1) = X0
            IF (D2(1,2).LT.X0) D2(1,2) = X0
            IF (D2(2,1).GT.Y0) D2(2,1) = Y0
            IF (D2(2,2).LT.Y0) D2(2,2) = Y0
            IF (D2(3,1).GT.Z0) D2(3,1) = Z0
            IF (D2(3,2).LT.Z0) D2(3,2) = Z0
          END IF
 1      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       CONVERTS FROM NUMERIC TO STRING CHARACTER
C       TO PRINT NUMBER ON AXES OF 3D
C-----------------------------------------------------------------------
        SUBROUTINE NUST(RNUM,INDEX,CH)
        CHARACTER*1 CH9(0:9),C(10),CH*6
        DATA CH9/'0','1','2','3','4','5','6','7','8','9'/
C---
C---
        IFIVE = 6
        CH    = ' '
        I     = 0
        F     = INT(RNUM * 10000) / 10000.0
        IF (F.EQ.0) THEN
          I       = 1
          CH(1:1) = '0'
          GO TO 4
        END IF
        IF (F.LT.0) THEN
          I       = 1
          CH(1:1) = '-'
        END IF
        F = ABS(F)
        M = INT(F)
        F = F - M
        J = 0
 1      IF (M.GT.0) THEN
          J    = J + 1
          K    = MOD(M,10)
          M    = M /10
          C(J) = CH9(K)
          GO TO 1
        END IF
        IF (J.GT.0) THEN
          DO 2 K =J,1,-1
            I       = I + 1
            CH(I:I) = C(K)
            IF (I.EQ.IFIVE) GO TO 4
 2        CONTINUE
          IF (J.EQ.4) GO TO 4
          I       = I + 1
          CH(I:I) = '.'
        ELSE
          I       = I + 1
          CH(I:I) = '.'
        END IF
 3      F = F * 10
        M = INT(F)
        F = F - M
        IF (I.EQ.IFIVE) GO TO 4
        I = I + 1
        CH(I:I) = CH9(M)
        GO TO 3
 4      INDEX = I
        RETURN
        END
C-----------------------------------------------------------------------
C       ADDS TRANSFORMATION AND PARALLEL PROJECTION
C       WITH X3, Y3 AND Z3 TO X,Y AND Z
C-----------------------------------------------------------------------
        SUBROUTINE ADDT(X,Y,Z,X3,Y3,Z3,TRAN,PARA)
        REAL TRAN(4,4),PARA(4,4)
C---
C---
        CALL AXYZ(X3,Y3,Z3,X0,Y0,Z0,TRAN)
        CALL AXYZ(X0,Y0,Z0,X,Y,Z,PARA)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS SOLID CIRCLE
C-----------------------------------------------------------------------
        SUBROUTINE SCIR
        INTEGER NVX(2),SPLPT
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /RADI/ RASY,RATY,RADI,RADINC
C---
C---
        IF (IX.LT.MINSX.OR.IX.GT.MAXSX) RETURN
        IF (IY.LT.MINSY.OR.IY.GT.MAXSY) RETURN
        CALL PLCMDS(33)
        TX     = IX
        TY     = IY
        AINCR  = ACOS(-1.0) / 5.0
        NPOINT = 11
        C      = 0
        R1     = COVTXY(C+5)
        R      = R1
 1      IF (R.LT.RADINC) GOTO 2
          RADIUS = R
          ANGLE  = 0.0
          CALL DPSHAP(RADIUS,AINCR,ANGLE,NPOINT,TX,TY)
          R = R - RADINC
          GOTO 1
 2      CONTINUE
        IX = TX
        IY = TY
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS OPEN CIRCLE
C-----------------------------------------------------------------------
        SUBROUTINE HCIR
        INTEGER NVX(2),SPLPT
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,
     +  MAXSX,MINSY,MAXSY,NVX,SPLPT
C---
C---
        C      = 0
        RADIUS = COVTXY(C+5)
        IF (IX.LT.MINSX.OR.IX.GT.MAXSX) RETURN
        IF (IY.LT.MINSY.OR.IY.GT.MAXSY) RETURN
        CALL PLCMDS(34)
        TX     = IX
        TY     = IY
        AINCR  = ACOS(-1.0) / 5.0
        ANGLE  = 0.0
        NPOINT = 11
        CALL DPSHAP(RADIUS,AINCR,ANGLE,NPOINT,TX,TY)
        IX = TX
        IY = TY
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS ORIGIN AT AVER(X,Y,Z)
C-----------------------------------------------------------------------
        SUBROUTINE DICRAV(PARA,TRAN,INFO)
        INTEGER NVX(2),SPLPT,TE(3)
        REAL PARA(4,4),IX,IY,MINSX,MAXSX,MINSY,MAXSY,MINQX,MINQY,MINQZ
        REAL MAXQX,MAXQY,MAXQZ,ANGXY(3),TRAN(4,4),INFO(13,3)
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /COOR/ CRX,CRY,CRZ
C---
C---
        CALL PLCMDS(28)
        CALL AXYZ(CRX,CRY,CRZ,X,Y,Z,PARA)
        CALL PLCMDS(7)
        TX = IX
        TY = IY
        CALL AXYZ(MAXQX,CRY,CRZ,X,Y,Z,PARA)
        CALL PLCMDS(7)
        SX = IX
        SY = IY
        IF (TX.EQ.SX.AND.TY.EQ.SY) THEN
          TE(1) = 1
        ELSE
          TE(1) = 0
          ANGXY(1) = FANGLE(SX,SY,TX,TY)
        END IF
C---
        CALL AXYZ(CRX,MAXQY,CRZ,X,Y,Z,PARA)
        CALL PLCMDS(7)
        SX = IX
        SY = IY
        IF (TX.EQ.SX.AND.TY.EQ.SY) THEN
          TE(2)    = 1
        ELSE
          TE(2)    = 0
          ANGXY(2) = FANGLE(SX,SY,TX,TY)
        END IF
C---
        CALL AXYZ(CRX,CRY,MAXQX,X,Y,Z,PARA)
        CALL PLCMDS(7)
        SX = IX
        SY = IY
        IF (TX.EQ.SX.AND.TY.EQ.SY) THEN
          TE(3)    = 1
        ELSE
          TE(3)    = 0
          ANGXY(3) = FANGLE(SX,SY,TX,TY)
          A1       = ACOS(-1.0) / 2.0
          IF (ANGXY(3).LE.A1) ANGXY(3) = ANGXY(3) + ACOS(-1.0)
        END IF
        CALL FDA(ANGXY,TX,TY,TE,PARA,TRAN,INFO)
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       FINDS THREE POINTS ON AXIS TO MAKE IT LONGER,
C       ANOTHER TWO POINTS FOR THE ARROW
C       AND ONE MORE POINT TO PRINT AXIS OR COORDINATE NUMBER
C-----------------------------------------------------------------------
        SUBROUTINE FDA(ANGXY,TX,TY,TE,PARA,TRAN,INFO)
        INTEGER NVX(2),SPLPT,TE(3)
        REAL ANGLE(4),ANGXY(3),RA(4),PARA(4,4),TRAN(4,4),INFO(13,3)
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,XC(3,4),YC(3,4)
        LOGICAL CIR,DFDOT,MIDDLE,DIAXIS,P1,DCOOR,PRNUM,ST3D
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /DS/ CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,ST3D
C---
C---
        P1       = DIAXIS
        S1       = MINSX
        S2       = MAXSX
        S3       = MINSY
        S4       = MAXSY
C---
        ANGLE(1) = FANGLE(S2,S4,TX,TY)
        ANGLE(2) = FANGLE(S1,S4,TX,TY)
        ANGLE(3) = FANGLE(S1,S3,TX,TY)
        ANGLE(4) = FANGLE(S2,S3,TX,TY)
C---
        RA(1)    = (S2 - TX) * 0.95
        RA(2)    = (S4 - TY) * 0.95
        RA(3)    = (TX - S1) * 0.95
        RA(4)    = (TY - S3) * 0.95
C---
        DA       = SQRT((S2-S1)**2 + (S4-S3)**2)
        DA       = DA / 40.0
C---                                    *FIND 3 POINTS
        DO 2 I=1,3
          IF (TE(I).EQ.1) GO TO 2
          DO 1 J=1,4
            IF (ANGXY(I).GT.ANGLE(J)) GO TO 1
            ALFA = ANGXY(I)
            IF (MOD(J,2).EQ.0) THEN
              RADIUS = ABS(RA(J) / SIN(ALFA))
            ELSE
              RADIUS = ABS(RA(J) / COS(ALFA))
            END IF
            CALL DXY(RADIUS,ALFA,TX,TY,IX,IY)
            XC(I,2) = IX
            YC(I,2) = IY
            CALL ARROW(I,IX,IY,ALFA,DA,ANGLE,XC,YC)
            GO TO 2
 1        CONTINUE
          ALFA    = ANGXY(I)
          RADIUS  = ABS(RA(1) / SIN(ALFA))
          CALL DXY(RADIUS,ALFA,TX,TY,IX,IY)
          XC(I,2) = IX
          YC(I,2) = IY
          CALL ARROW(I,IX,IY,ALFA,DA,ANGLE,XC,YC)
 2      CONTINUE
C---
        CALL PRAXIS(P1,XC,YC,TX,TY,TE,DA)
        IF (ST3D) THEN
          DO 4 I=1,3
            IF (TE(I).EQ.1) GO TO 4
            IX = XC(I,2)
            IY = YC(I,2)
            ALFA = ANGXY(I)
            CALL ARROWL(I,IX,IY,ALFA,DA,ANGLE,XC,YC,ICODE)
            CALL PRTIT3(XC,YC,DA,I,ICODE)
 4        CONTINUE
        ELSE IF (MIDDLE) THEN
          P1 = .NOT.P1
          DO 3 I =1,3
            IF (TE(I).EQ.1) GO TO 3
            IX   = XC(I,2)
            IY   = YC(I,2)
            ALFA = ANGXY(I)
            CALL ARROWL(I,IX,IY,ALFA,DA,ANGLE,XC,YC,ICODE)
 3        CONTINUE
          CALL PRAXIS(P1,XC,YC,TX,TY,TE,DA)
        END IF
        CALL DAXIS(XC,YC,TX,TY,TE)
C---                                    *PRINT NUMBER
        IF (PRNUM) THEN
          CALL DN(XC,YC,TX,TY,TE,DA,TRAN,INFO,PARA,ANGXY,ANGLE)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       PRINTS 3 AXES LABELS
C-----------------------------------------------------------------------
        SUBROUTINE PRTIT3(XC,YC,DIS,INDX,CODE)
        REAL XC(3,4),YC(3,4),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        INTEGER CTIT3C(3),NVX(2),SPLPT,CODE
        CHARACTER*15 TIT3C(3),CHFIF,CHONE*1,CHTWO*2
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /T3D/ CTIT3C,TIT3C
        COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
C---
C---
        IX     = XC(INDX,4)
        IY     = YC(INDX,4)
        C      = 0
        INDXCH = CTIT3C(INDX)
        SKIP   = COVTX(C+12.78)
        SKIP1  = COVTX(C+5)
        IF (CODE.NE.4.AND.CODE.NE.6.AND.CODE.NE.5) THEN
          IX = IX - (INDXCH - 1) * SKIP
        END IF
        IF ((CODE.EQ.2.OR.CODE.EQ.3).AND.INDXCH.GT.11) THEN
          IX = IX - SKIP1
        END IF
        CHFIF = TIT3C(INDX)
        CALL PLCMDS(27)
        CALL PLCTNM(23)
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       PRINTS (X,Y,Z) ON EACH AXIS
C-----------------------------------------------------------------------
        SUBROUTINE PRAXIS(PAXIS,XC,YC,TX,TY,TE,DA)
        INTEGER XAXIS,YAXIS,ZAXIS,NVX(2),SPLPT,AX1(3),TE(3)
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,XC(3,4),YC(3,4)
        CHARACTER*1 AX2(3),CHONE,CHTWO*2,CHFIF*15
        LOGICAL PAXIS
C---
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
        COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
        DATA AX2 /'X','Y','Z'/
C---
C---
        AX1(1) = XAXIS
        AX1(2) = YAXIS
        AX1(3) = ZAXIS
        D1     = DA / 2.0
C---
        IF (.NOT.PAXIS) THEN
          DO 1 I=1,3
            IF (TE(I).EQ.0) THEN
              IX = XC(I,4)
              IY = YC(I,4)
            ELSE
              IX = TX + D1
              IY = TY - D1
            END IF
            INDXCH = 1
            CHONE  = AX2(I)
            CALL PLCMDS(27)
            CALL PLCTNM(22)
            CALL PLCMDS(20)
 1        CONTINUE
        ELSE
          DO 2 I=1,3
            IF (TE(I).EQ.0) THEN
              IX = XC(I,4)
              IY = YC(I,4)
            ELSE
              IX = TX + D1
              IY = TY - D1
            END IF
            CALL DICHR(AX1(I))
            CALL PLCMDS(27)
            CALL PLCTNM(22)
            CALL PLCMDS(20)
 2        CONTINUE
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS THREE AXES AND ARROWS
C-----------------------------------------------------------------------
        SUBROUTINE DAXIS(XC,YC,TX,TY,TE)
        INTEGER TE(3),NVX(2),SPLPT
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,XC(3,4),YC(3,4)
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
C---
C---
        CALL PLCMDS(28)
        DO 1 I = 1,3
          IF (TE(I).EQ.1) GO TO 1
          IX = TX
          IY = TY
          CALL PLCMDS(2)
          IX = XC(I,2)
          IY = YC(I,2)
          CALL PLCMDS(3)
          IX = XC(I,1)
          IY = YC(I,1)
          CALL PLCMDS(2)
          IX = XC(I,2)
          IY = YC(I,2)
          CALL PLCMDS(3)
          IX = XC(I,3)
          IY = YC(I,3)
          CALL PLCMDS(3)
 1      CONTINUE
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       THIS FUNCTION FINDS ANGLE WITH THE POINT TX,TY
C-----------------------------------------------------------------------
        FUNCTION FANGLE(XT,YT,XB,YB)
C---
C---
        A    = YT - YB
        B    = XT - XB
        C    = SQRT(A**2 + B**2)
        D    = ABS(B/C)
        ALFA = ACOS(D)
        A1   = ACOS(-1.0) / 180.0
        TPI  = ACOS(-1.0) * 2
        IF (A.GE.0.AND.B.GE.0) THEN
          FANGLE = ALFA
        ELSE IF (A.GE.0.AND.B.LT.0) THEN
          FANGLE = 180.0 * A1 - ALFA
        ELSE IF (A.LT.0.AND.B.LE.0) THEN
          FANGLE = 180.0 * A1 + ALFA
        ELSE
          FANGLE = TPI - ALFA
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C      COMPUTES TWO POINTS X AND Y, RADIUS ALFA
C-----------------------------------------------------------------------
       SUBROUTINE DXY(RADIUS,ALFA,TX,TY,X,Y)
C---
C---
       X = RADIUS * COS(ALFA) + TX
       Y = RADIUS * SIN(ALFA) + TY
       RETURN
       END
C-----------------------------------------------------------------------
C       COMPUTES TWO POINTS FOR ARROW AND ANOTHER POINT
C       TO SET UP TITLE
C-----------------------------------------------------------------------
        SUBROUTINE ARROW(INDEX,IX,IY,ALFA,RADIUS,ANGLE,XC,YC)
        REAL ANGLE(4),XC(3,4),YC(3,4),IX,IY
        REAL MINSX,MAXSX,MINSY,MAXSY
        INTEGER NVX(2),SPLPT
        COMMON /PLVARS/ X1,Y1,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
C---
C---
        A1    = ACOS(-1.0) / 180.0
        TX    = IX
        TY    = IY
        TALFA = ALFA - 150.0 * A1
        CALL DXY(RADIUS,TALFA,TX,TY,IX,IY)
        XC(INDEX,1) = IX
        YC(INDEX,1) = IY
        TALFA       = ALFA - 210.0*A1
        CALL DXY(RADIUS,TALFA,TX,TY,IX,IY)
        XC(INDEX,3) = IX
        YC(INDEX,3) = IY
C---                                    *FIND EMPTY SPACE
        PIH  = 90 * A1
        PI   = 180 * A1
        PITH = 270 * A1
        R1   = RADIUS / 2.0
C---
        IF (ALFA.LE.ANGLE(1)) THEN
          XC(INDEX,4) = XC(INDEX,3)
          YC(INDEX,4) = YC(INDEX,3) + R1
        ELSE IF (ALFA.LE.PIH) THEN
          XC(INDEX,4) = XC(INDEX,1) + R1
          YC(INDEX,4) = YC(INDEX,1)
        ELSE IF (ALFA.LE.ANGLE(2)) THEN
          XC(INDEX,4) = XC(INDEX,1) + R1
          YC(INDEX,4) = YC(INDEX,1)
        ELSE IF (ALFA.LE.PI) THEN
          XC(INDEX,4) = XC(INDEX,1)
          YC(INDEX,4) = YC(INDEX,1) + R1
        ELSE IF (ALFA.LE.ANGLE(3)) THEN
          XC(INDEX,4) = XC(INDEX,1)
          YC(INDEX,4) = YC(INDEX,1) + R1
        ELSE IF (ALFA.LE.PITH) THEN
          XC(INDEX,4) = XC(INDEX,1)
          YC(INDEX,4) = YC(INDEX,1) + R1
        ELSE IF (ALFA.LE.ANGLE(4)) THEN
          XC(INDEX,4) = XC(INDEX,3) + R1
          YC(INDEX,4) = YC(INDEX,3)
        ELSE
          XC(INDEX,4) = XC(INDEX,3)
          YC(INDEX,4) = YC(INDEX,3) + R1
        END IF
        R1 = R1 / 2
        IF (XC(INDEX,4).LE.MINSX) XC(INDEX,4) = MINSX + R1
        IF (XC(INDEX,4).GE.MAXSX) XC(INDEX,4) = MAXSX - R1
        IF (YC(INDEX,4).LE.MINSY) YC(INDEX,4) = MINSY + R1
        IF (YC(INDEX,4).GE.MAXSY) YC(INDEX,4) = MAXSY - R1
        RETURN
        END
C-----------------------------------------------------------------------
C       FINDS SPACE TO PRINT DIMENSION
C       AT LEFT ARROW
C-----------------------------------------------------------------------
        SUBROUTINE ARROWL(INDEX,IX,IY,ALFA,RADIUS,ANGLE,XC,YC,ICODE)
        REAL ANGLE(4),XC(3,4),YC(3,4),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        INTEGER NVX(2),SPLPT
        COMMON /PLVARS/ X1,Y1,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
C---
C---
        A1    = ACOS(-1.0) / 180.0
        TX    = IX
        TY    = IY
        TALFA = ALFA - 150.0 * A1
        CALL DXY(RADIUS,TALFA,TX,TY,IX,IY)
        XC(INDEX,1) = IX
        YC(INDEX,1) = IY
        TALFA       = ALFA - 210.0 * A1
        CALL DXY(RADIUS,TALFA,TX,TY,IX,IY)
        XC(INDEX,3) = IX
        YC(INDEX,3) = IY
        PIH         =  90.0 * A1
        PI          = 180.0 * A1
        PITH        = 270.0 * A1
        R1          = RADIUS
        C           = 0
        SKIP        = COVTX(C+12.78)
C---
        IF (ALFA.LE.ANGLE(1)) THEN
          XC(INDEX,4) = XC(INDEX,1)
          YC(INDEX,4) = YC(INDEX,1) - R1
          ICODE       = 1
        ELSE IF (ALFA.LE.PIH) THEN
          XC(INDEX,4) = XC(INDEX,3) - R1
          YC(INDEX,4) = YC(INDEX,3)
          ICODE       = 2
        ELSE IF (ALFA.LE.ANGLE(2)) THEN
          XC(INDEX,4) = XC(INDEX,3) - R1
          YC(INDEX,4) = YC(INDEX,3)
          ICODE       = 3
        ELSE IF (ALFA.LE.PI) THEN
          XC(INDEX,4) = XC(INDEX,3) + SKIP
          YC(INDEX,4) = YC(INDEX,3) - R1
          ICODE       = 4
        ELSE IF (ALFA.LE.ANGLE(3)) THEN
          XC(INDEX,4) = XC(INDEX,3) + SKIP
          YC(INDEX,4) = YC(INDEX,3) - R1
          ICODE       = 5
        ELSE IF (ALFA.LE.PITH) THEN
          XC(INDEX,4) = XC(INDEX,3) + SKIP
          YC(INDEX,4) = YC(INDEX,3) - R1
          ICODE       = 6
        ELSE IF (ALFA.LE.ANGLE(4)) THEN
          XC(INDEX,4) = XC(INDEX,1) - R1
          YC(INDEX,4) = YC(INDEX,1)
          ICODE       = 7
        ELSE
          XC(INDEX,4) = XC(INDEX,1)
          YC(INDEX,4) = YC(INDEX,1) - R1
          ICODE       = 8
        END IF
        R1 = R1 / 2
        IF (XC(INDEX,4).LE.MINSX) XC(INDEX,4) = MINSX + R1
        IF (XC(INDEX,4).GE.MAXSX) XC(INDEX,4) = MAXSX - R1
        IF (YC(INDEX,4).LE.MINSY) YC(INDEX,4) = MINSY + R1
        IF (YC(INDEX,4).GE.MAXSY) YC(INDEX,4) = MAXSY - R1
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS MAX OF X,Y,Z ON THE SCREEN
C-----------------------------------------------------------------------
        SUBROUTINE DN(XC,YC,TX,TY,TE,DA,T,INFO,PA,ANGXY,ANGLE)
        REAL T(4,4),INFO(13,3),PA(4,4),ANGXY(3),ROWIND(4,4)
        REAL ANGLE(3),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        REAL D1(3,2),D2(3,2),D3(3,2)
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,XC(3,4),YC(3,4)
        INTEGER TE(3),NVX(2),SPLPT
        CHARACTER*6 STR,CHONE*1,CHTWO*2,CHFIF*15
C---
        COMMON /ROWI/ ROWIND
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /COOR/ CRX,CRY,CRZ
        COMMON /PR/ PX,PY,PZ
        COMMON /LBNM/ INDXCH,CHONE,CHTWO,CHFIF
C---
C---
        D1(1,1) = INFO(7,1)
        D1(2,1) = INFO(7,2)
        D1(3,1) = INFO(7,3)
        D1(1,2) = INFO(8,1)
        D1(2,2) = INFO(8,2)
        D1(3,2) = INFO(8,3)
        C       = 0
C---                                    *COMPUTE 6 POINTS
        CALL FMM(D1,D2,ROWIND)
C---
        CALL FMM(D1,D3,T)
        X2 = D3(1,2)
        Y2 = D3(2,2)
        Z2 = D3(3,2)
        CALL AXYZ(X2,CRY,CRZ,X,Y,Z,PA)
        CALL PLCMDS(7)
        TE(1) = 1
        IX1   = INT(MIN(TX,XC(1,2)))
        IX2   = INT(MAX(TX,XC(1,2)))
        IY1   = INT(MIN(TY,YC(1,2)))
        IY2   = INT(MAX(TY,YC(1,2)))
C---                                    *CHECK LENGTH OF X AXIS
        IF (IX.LT.IX1.OR.IX.GT.IX2) GO TO 1
        IF (IY.LT.IY1.OR.IY.GT.IY2) GO TO 1
        TE(1)   = 0
        ALFA    = ANGXY(1)
        XC(1,2) = IX + DA * COS(ALFA)
        YC(1,2) = IY + DA * SIN(ALFA)
 1      CONTINUE
        CALL AXYZ(CRX,Y2,CRZ,X,Y,Z,PA)
        CALL PLCMDS(7)
        TE(2) = 1
        IX1   = INT(MIN(TX,XC(2,2)))
        IX2   = INT(MAX(TX,XC(2,2)))
        IY1   = INT(MIN(TY,YC(2,2)))
        IY2   = INT(MAX(TY,YC(2,2)))
C---                                    *CHECK LENGTH OF Y AXIS
        IF (IX.LT.IX1.OR.IX.GT.IX2) GO TO 2
        IF (IY.LT.IY1.OR.IY.GT.IY2) GO TO 2
        TE(2)   = 0
        ALFA    = ANGXY(2)
        XC(2,2) = IX + DA * COS(ALFA)
        YC(2,2) = IY + DA * SIN(ALFA)
 2      CONTINUE
        CALL AXYZ(CRX,CRY,Z2,X,Y,Z,PA)
        CALL PLCMDS(7)
        TE(3) = 1
C---
        IX1   = INT(MIN(TX,XC(3,2)))
        IX2   = INT(MAX(TX,XC(3,2)))
        IY1   = INT(MIN(TY,YC(3,2)))
        IY2   = INT(MAX(TY,YC(3,2)))
C---                                    *CHECK LENGTH OF Z AXIS
        IF (IX.LT.IX1.OR.IX.GT.IX2) GO TO 3
        IF (IY.LT.IY1.OR.IY.GT.IY2) GO TO 3
        TE(3)   = 0
        ALFA    = ANGXY(3)
        XC(3,2) = IX + DA * COS(ALFA)
        YC(3,2) = IY + DA * SIN(ALFA)
 3      CONTINUE
C---                                    *FIND MIN, MAX(X,Y,Z)
        DO 4 I=1,3
          IF (TE(I).EQ.1) GO TO 4
          ALFA = ANGXY(I)
          CALL ARROWL(I,XC(I,2),YC(I,2),ALFA,DA,ANGLE,XC,YC,ICODE)
          IX   = XC(I,1)
          IY   = YC(I,1)
          CALL PLCMDS(2)
          IX   = XC(I,3)
          IY   = YC(I,3)
          CALL PLCMDS(3)
          CALL NUST(D2(I,2),INDEX,STR)
          IX   = XC(I,4)
          IY   = YC(I,4)
          IF (ICODE.EQ.1.OR.ICODE.EQ.4.OR.ICODE.EQ.5.OR.
     +        ICODE.EQ.8) THEN
            IX = IX - INDEX * COVTX(C+5)
          ELSE
            IX = IX - INDEX * COVTX(C+10)
          END IF
          CHFIF = STR
          INDXCH = INDEX
          CALL PLCMDS(21)
          CALL PLCTNM(23)
          CALL PLCMDS(20)
 4      CONTINUE
C---
        SKIP = 12.8
        SKIP = COVTX(SKIP)
        IX = TX + DA
        IY = TY + DA
        CALL NUST(PX,INDEX,STR)
        CHFIF = STR
        INDXCH = INDEX
        CALL PLCTNM(23)
C---
        IX = IX + SKIP * INDEX
        CALL NUST(PY,INDEX,STR)
        CHFIF = STR
        INDXCH = INDEX
        CALL PLCMDS(21)
        CALL PLCTNM(23)
        CALL PLCMDS(20)
C---
        IX = IX + SKIP * INDEX
        CALL NUST(PZ,INDEX,STR)
        CHFIF = STR
        INDXCH = INDEX
        CALL PLCMDS(21)
        CALL PLCTNM(23)
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       CHECKS DISTANCE OF (X,Y,Z), IF LESS THAN OR
C       EQUAL TO ERROR NUMBER THEN SKIP THAT POINT
C-----------------------------------------------------------------------
        SUBROUTINE DZERO(X,Y,Z)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /DL/ XL,YL,ZL,DIS
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /COOR/ CRX,CRY,CRZ
C---
C---
        ERR = 10.0 ** (-8.0)
        CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
        XZ   = ABS(XL)
        YZ   = ABS(YL)
        ZZ   = ABS(ZL)
        XYZZ = MIN(XZ,YZ,ZZ)
C---                                    *CHECK XL,YL,ZL > ERR
        IF (XYZZ.GT.ERR) THEN
          R1   = ABS((CRX-XOLD) / XL)
          R2   = ABS((CRY-YOLD) / YL)
          R3   = ABS((CRZ-ZOLD) / ZL)
          RINT = MIN(R1,R2,R3)
          X    = XOLD + XL * RINT
          Y    = YOLD + YL * RINT
          Z    = ZOLD + ZL * RINT
          GO TO 1
        END IF
C---                                    *CHECK XL,YL,ZL <= ERR
        XYZZ = MAX(XZ,YZ,ZZ)
        IF (XYZZ.LE.ERR) THEN
          X = X1
          Y = Y1
          Z = Z1
          GO TO 1
        END IF
C---                                    *CHECK XZ,YZ <= ERR
        IF (XZ.LE.ERR.AND.YZ.LE.ERR) THEN
          X    = X1
          Y    = Y1
          RINT = ABS((CRZ-ZOLD) / ZL)
          Z    = ZOLD + ZL * RINT
          GO TO 1
        END IF
C---                                    *CHECK YZ,ZZ <= ERR
        IF (YZ.LE.ERR.AND.ZZ.LE.ERR) THEN
          Y    = Y1
          Z    = Z1
          RINT = ABS((CRX-XOLD) / XL)
          X    = XOLD + XL * RINT
          GO TO 1
        END IF
C---                                    *CHECK XZ,ZZ <= ERR
        IF (XZ.LE.ERR.AND.ZZ.LE.ERR) THEN
          X    = X1
          Z    = Z1
          RINT = ABS((CRY-YOLD) / YL)
          Y    = YOLD + YL * RINT
          GO TO 1
        END IF
C---                                    *CHECK XZ <= ERR
        IF (XZ.LE.ERR) THEN
          X    = X1
          R1   = ABS((CRY-YOLD) / YL)
          R2   = ABS((CRZ-ZOLD) / ZL)
          RINT = MIN(R1,R2)
          Y    = YOLD + YL * RINT
          Z    = ZOLD + ZL * RINT
          GO TO 1
        END IF
C---                                    *CHECK YZ <= ERR
        IF (YZ.LE.ERR) THEN
          Y    = Y1
          R1   = ABS((CRX-XOLD) / XL)
          R2   = ABS((CRZ-ZOLD) / ZL)
          RINT = MIN(R1,R2)
          X    = XOLD + XL * RINT
          Z    = ZOLD + ZL * RINT
          GO TO 1
        END IF
C---                                    *CHECK ZZ <= ERR
        IF (ZZ.LE.ERR) THEN
          Z    = Z1
          R1   = ABS((CRX-XOLD) / XL)
          R2   = ABS((CRY-YOLD) / YL)
          RINT = MIN(R1,R2)
          X    = XOLD + XL * RINT
          Y    = YOLD + YL * RINT
        END IF
 1      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS CURVE USING DOTS
C-----------------------------------------------------------------------
        SUBROUTINE DISDOT(PARA)
        INTEGER NVX(2),SPLPT
        REAL PARA(4,4),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /DL/ XL,YL,ZL,DIS
C---
C---
 1      CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
        CALL PLCMDS(37)
        IF (RSKIP.GE.DIS) THEN
          RSKIP = RSKIP - DIS
        ELSE
          XYZINT = RSKIP / DIS
          XOLD   = XOLD + XL * XYZINT
          YOLD   = YOLD + YL * XYZINT
          ZOLD   = ZOLD + ZL * XYZINT
          CALL AXYZ(XOLD,YOLD,ZOLD,X,Y,Z,PARA)
          RSKIP  = DISD
          IF (X.GT.XMAX.OR.X.LT.XMIN) GO TO 1
          IF (Y.GT.YMAX.OR.Y.LT.YMIN) GO TO 1
          CALL PLCMDS(10)
          GO TO 1
        END IF
C---
        XOLD = X1
        YOLD = Y1
        ZOLD = Z1
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS CURVE USING DASHES AND SOLID LINE.
C       (DASHES WHEN X1.LT.AVERX .OR. Y1.LT.AVERY .OR. Z1.LT.AVERZ)
C-----------------------------------------------------------------------
        SUBROUTINE LINDAS(PARA)
        REAL PARA(4,4)
        LOGICAL SKIP,LINE,CONECT,CMPLTE,DASH
        COMMON /LDD1/ SKIP,LINE,CONECT,DASH
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /DL/ XL,YL,ZL,DIS
C---
C---
        CALL PLCMDS(36)
        CMPLTE = .TRUE.
C---
        IF (LINE.AND..NOT.DASH) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,2)
          XOLD = X1
          YOLD = Y1
          ZOLD = Z1
          DASH = .NOT. LINE
          CALL PLCMDS(20)
          RETURN
        END IF
C---
        IF (LINE.AND.DASH) THEN
          XK     = X1
          YK     = Y1
          ZK     = Z1
          CALL DZERO(X3,Y3,Z3)
          X1     = X3
          Y1     = Y3
          Z1     = Z3
          CMPLTE = .FALSE.
          LINE   = .FALSE.
        END IF
C---
        IF (.NOT.LINE.AND..NOT.DASH) THEN
          CALL DZERO(X3,Y3,Z3)
          XOLD   = X3
          YOLD   = Y3
          ZOLD   = Z3
          CALL AXYZ(XOLD,YOLD,ZOLD,X,Y,Z,PARA)
          CALL CLIP(X,Y,2)
          SKIP   = .TRUE.
          RSKIP  = FSKXYZ
          CONECT = .FALSE.
          DASH   = .TRUE.
        END IF
C---
        IF (.NOT.LINE.AND.DASH) THEN
          CALL DASHSL(PARA)
          CALL PLCMDS(36)
        END IF
C---                                    *CHECK IF COMPLETE
        IF (.NOT.CMPLTE) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,3)
          CALL AXYZ(XK,YK,ZK,X,Y,Z,PARA)
          CALL CLIP(X,Y,2)
          XOLD = XK
          YOLD = YK
          ZOLD = ZK
          LINE = .TRUE.
        END IF
        DASH = .NOT.LINE
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS CURVE USING DASHED LINE
C-----------------------------------------------------------------------
        SUBROUTINE DASHSL(PARA)
        REAL PARA(4,4)
        LOGICAL SKIP,LINE,CONECT,DASH
        COMMON /LDD1/ SKIP,LINE,CONECT,DASH
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /DL/ XL,YL,ZL,DIS
C---
C---
        CALL PLCMDS(35)
 1      CONTINUE
        IF (CONECT) THEN
          CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
          IF (RCONCT.GT.DIS) THEN
            XOLD   = X1
            YOLD   = Y1
            ZOLD   = Z1
            CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
            CALL CLIP(X,Y,2)
            RCONCT = RCONCT - DIS
            CALL PLCMDS(20)
            RETURN
          ELSE
            CONECT = .FALSE.
            SKIP   = .TRUE.
            RSKIP  = FSKXYZ
            XYZINT = RCONCT / DIS
            XOLD   = XOLD + XL * XYZINT
            YOLD   = YOLD + YL * XYZINT
            ZOLD   = ZOLD + ZL * XYZINT
            CALL AXYZ(XOLD,YOLD,ZOLD,X,Y,Z,PARA)
            CALL CLIP(X,Y,2)
          END IF
        END IF
C---
        IF (SKIP) THEN
          CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
          IF (RSKIP.GT.DIS) THEN
            XOLD  = X1
            YOLD  = Y1
            ZOLD  = Z1
            RSKIP = RSKIP - DIS
            RETURN
          ELSE
            SKIP   = .FALSE.
            CONECT = .TRUE.
            RCONCT = FDXYZ
            XYZINT = RSKIP / DIS
            XOLD   = XOLD + XL * XYZINT
            YOLD   = YOLD + YL * XYZINT
            ZOLD   = ZOLD + ZL * XYZINT
            CALL AXYZ(XOLD,YOLD,ZOLD,X,Y,Z,PARA)
            CALL CLIP(X,Y,3)
            GO TO 1
          END IF
        END IF
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS CURVE USING LINE OR DOT
C       USE DOT IF X1.LT.AVERX .OR. Y1.LT.AVERY .OR. Z1.LT.AVEZ
C-----------------------------------------------------------------------
        SUBROUTINE LINDOT(PARA)
        REAL PARA(4,4)
        LOGICAL SKIP,LINE,CONECT,CMPLTE,DASH
        COMMON /LDD1/ SKIP,LINE,CONECT,DASH
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /DL/ XL,YL,ZL,DIS
C---
C---
        CALL PLCMDS(36)
        CMPLTE = .TRUE.
C---
        IF (LINE.AND..NOT.DASH) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,2)
          XOLD = X1
          YOLD = Y1
          ZOLD = Z1
          DASH = .NOT. LINE
          CALL PLCMDS(20)
          RETURN
        END IF
C---
        IF (LINE.AND.DASH) THEN
          XK     = X1
          YK     = Y1
          ZK     = Z1
          CALL DZERO(X3,Y3,Z3)
          X1     = X3
          Y1     = Y3
          Z1     = Z3
          CMPLTE = .FALSE.
          LINE   = .FALSE.
        END IF
C---
        IF (.NOT.LINE.AND..NOT.DASH) THEN
          CALL DZERO(X3,Y3,Z3)
          XOLD  = X3
          YOLD  = Y3
          ZOLD  = Z3
          CALL AXYZ(XOLD,YOLD,ZOLD,X,Y,Z,PARA)
          CALL CLIP(X,Y,2)
          DASH  = .TRUE.
          RSKIP = DISD
        END IF
C---
        IF (.NOT.LINE.AND.DASH) THEN
          CALL DISDOT(PARA)
          CALL PLCMDS(36)
        END IF
        IF (.NOT.CMPLTE) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,3)
          XOLD = XK
          YOLD = YK
          ZOLD = ZK
          CALL AXYZ(XK,YK,ZK,X,Y,Z,PARA)
          CALL CLIP(X,Y,2)
          LINE = .TRUE.
        END IF
        DASH = .NOT.LINE
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       COMPUTES DISTANCE AND LENGTH
C-----------------------------------------------------------------------
        SUBROUTINE DISTLN(XMAX,YMAX,ZMAX,XMIN,YMIN,ZMIN)
        COMMON /DL/XL,YL,ZL,DIS
C---
C---
        XL  = XMAX - XMIN
        YL  = YMAX - YMIN
        ZL  = ZMAX - ZMIN
        DIS = SQRT(XL**2 + YL**2 + ZL**2)
        RETURN
        END
C-----------------------------------------------------------------------
C        DISPLAYS THE GRAPH ON THE SCREEN
C        WITH PARALLEL PROJECTION
C-----------------------------------------------------------------------
        SUBROUTINE DISP3D(TRAN,PARA,MIMA)
        INTEGER SPLPT,NVX(2),COUNTC
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        REAL TRAN(4,4),PARA(4,4),XS(4),YS(4),XYPT(4,2),MIMA(3,2)
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,PNTS(1999,4)
        LOGICAL PLTR,POINT,SKIP,LINE,CONECT,DASH,B3DC
        CHARACTER*1 MP(81,251)
C---
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /PLOTT/ PLTR,POINT
        COMMON /DL/ XL,YL,ZL,DIS
        COMMON /DS1/ DAINT,DOINT,PSIX,PFOUR
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /LDD1/ SKIP,LINE,CONECT,DASH
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /CB3D/ B3DC
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
C---
C---
        XLEFT  = XMIN
        XRIGHT = XMAX
        YBOTTM = YMIN
        YTOP   = YMAX
C---
        CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
        DIST = DIS / DAINT
        DISD = DIS / DOINT
        FDXYZ = DIST * PSIX
        FSKXYZ = DIST * PFOUR
        SKIP = .FALSE.
        CONECT = .FALSE.
        LINE   = .TRUE.
        DASH   = .FALSE.
        CALL INARRS(MP,PNTS)
C---                                   *<3D> OR <B3D>
        IF (B3DC) THEN
          CALL DPB3D(TRAN,PARA,MP,PNTS)
        ELSE
          CALL DP3D(TRAN,PARA,MP,PNTS)
        END IF
C---
        IF (POINT) THEN
          CALL LGRPHS(MP,PNTS)
        END IF
        CALL PLCMDS(9)
        IF (PLTR) THEN
          CALL PLCHDW(2)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       CLIPS IF THE LINE IS OUTSIDE THE WINDOW
C       CHECK FOUR SIDES OF THE WINDOW : LEFT, RIGHT, BOTTOM AND TOP
C-----------------------------------------------------------------------
        SUBROUTINE CLIP(X1,Y1,OP)
        INTEGER OP,COUNTC,SPLPT,NVX(2)
        REAL XS(4),YS(4),XYPT(4,2),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
C---
C---
        X = X1
        Y = Y1
        IF (OP.EQ.1.OR.OP.EQ.3) THEN
          DO 1 I=1,4
            XS(I) = X
            YS(I) = Y
 1        CONTINUE
C---                                    *CHECK IF INSIDE
          IF (X.GT.XRIGHT.OR.X.LT.XLEFT) RETURN
          IF (Y.GT.YTOP.OR.Y.LT.YBOTTM) RETURN
          IF (OP.EQ.1) THEN
            CALL PLCMDS(10)
          ELSE
            CALL PLCMDS(4)
          END IF
          RETURN
        END IF
C---                                    *CLIP
        IF (X.GT.XRIGHT.OR.X.LT.XLEFT.OR.
     +      XS(1).GT.XRIGHT.OR.XS(1).LT.XLEFT.OR.
     +      Y.GT.YTOP.OR.Y.LT.YBOTTM.OR.
     +      YS(1).GT.YTOP.OR.YS(1).LT.YBOTTM) GO TO 3
C---                                    *SOLID LINE
        CALL PLCMDS(5)
        DO 2 I=1,4
          XS(I) = X
          YS(I) = Y
 2      CONTINUE
        RETURN
C---                                    *CLIP
 3      COUNTC = 0
        CALL CLEFT(X,Y)
        IF (COUNTC.EQ.2) THEN
          X = XYPT(1,1)
          Y = XYPT(1,2)
          CALL PLCMDS(4)
          X = XYPT(2,1)
          Y = XYPT(2,2)
          CALL PLCMDS(5)
        ELSE IF(COUNTC.EQ.1) THEN
          X = XYPT(1,1)
          Y = XYPT(1,2)
          CALL PLCMDS(5)
        END IF
        X = X1
        Y = Y1
        RETURN
        END
C-----------------------------------------------------------------------
C       CLIPS LEFT SIDE OF THE WINDOW
C-----------------------------------------------------------------------
        SUBROUTINE CLEFT(X,Y)
        REAL XS(4),YS(4),XYTP(4,2)
        INTEGER COUNTC
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYTP,COUNTC
C---
C---
C---                                    *OUTSIDE IN
        IF (X.GE.XLEFT.AND.XS(1).LT.XLEFT) THEN
          X1 = XLEFT
          Y1 = (Y-YS(1)) * (XLEFT-X) / (X-XS(1)) + Y
          CALL CRIGHT(X1,Y1)
        END IF
C---                                    *INSIDE OUT
        IF (X.LE.XLEFT.AND.XS(1).GT.XLEFT) THEN
          X1 = XLEFT
          Y1 = (Y-YS(1)) * (XLEFT-X) / (X-XS(1)) + Y
          CALL CRIGHT(X1,Y1)
        END IF
C---                                    *SAVE POINT TO SERVE AS END
C---                                    *OF NEXT LINE
        XS(1) = X
        YS(1) = Y
C---                                    *POINT INSIDE
        IF (X.GE.XLEFT) THEN
          CALL CRIGHT(X,Y)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       CLIPS RIGHT OF THE WINDOW
C-----------------------------------------------------------------------
        SUBROUTINE CRIGHT(X,Y)
        REAL XS(4),YS(4),XYPT(4,2)
        INTEGER COUNTC
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
C---
C---
C---                                    *OUTSIDE IN
        IF (X.LE.XRIGHT.AND.XS(2).GT.XRIGHT) THEN
          X1 = XRIGHT
          Y1 = (Y-YS(2))*(XRIGHT-X) / (X-XS(2)) + Y
          CALL CBOTTM(X1,Y1)
        END IF
C---                                    *INSIDE OUT
        IF (X.GE.XRIGHT.AND.XS(2).LT.XRIGHT) THEN
          X1 = XRIGHT
          Y1 = (Y-YS(2))*(XRIGHT-X)/(X-XS(2)) + Y
          CALL CBOTTM(X1,Y1)
        END IF
C---                                    *SAVE POINT TO SERVE A
C---                                    *ENDPOINT OF NEXT LINE
        XS(2) = X
        YS(2) = Y
        IF (X.LE.XRIGHT) THEN
          CALL CBOTTM(X,Y)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       CLIPS BOTTOM OF THE WINDOW
C-----------------------------------------------------------------------
        SUBROUTINE CBOTTM(X,Y)
        REAL XS(4),YS(4),XYPT(4,2)
        INTEGER COUNTC
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
C---
C---
C---                                    *OUTSIDE IN
        IF (Y.GE.YBOTTM.AND.YS(3).LT.YBOTTM) THEN
          X1 = (X-XS(3))*(YBOTTM-Y) / (Y-YS(3)) + X
          Y1 = YBOTTM
          CALL CTOP(X1,Y1)
        END IF
C---                                    *INSIDE OUT
        IF (Y.LE.YBOTTM.AND.YS(3).GT.YBOTTM) THEN
          X1 = (X-XS(3))*(YBOTTM-Y) / (Y-YS(3)) + X
          Y1 = YBOTTM
          CALL CTOP(X1,Y1)
        END IF
C---                                    *SAVE POINT TO SERVE AS
C---                                    *ENDPOINT OF NEXT BRANCH
        XS(3) = X
        YS(3) = Y
C---                                    *POINT INSIDE
        IF (Y.GE.YBOTTM) THEN
          CALL CTOP(X,Y)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       CLIPS TOP SIDE AND SAVES THE POINT CLIPPED
C-----------------------------------------------------------------------
        SUBROUTINE CTOP(X,Y)
        REAL XS(4),YS(4),XYPT(4,2)
        INTEGER COUNTC
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
C---
C---
C---                                    *OUTSIDE IN
        IF (Y.LE.YTOP.AND.YS(4).GT.YTOP) THEN
          X1 = (X-XS(4))*(YTOP-Y) / (Y-YS(4)) + X
          Y1 = YTOP
          CALL SAVECL(X1,Y1)
        END IF
C---                                    *INSIDE OUT
        IF (Y.GE.YTOP.AND.YS(4).LT.YTOP) THEN
          X1 = (X-XS(4))*(YTOP-Y) / (Y-YS(4)) + X
          Y1 = YTOP
          CALL SAVECL(X1,Y1)
        END IF
        XS(4) = X
        YS(4) = Y
        IF (Y.LE.YTOP) THEN
          CALL SAVECL(X,Y)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       SAVES THE POINT AFTER CLIPPING
C-----------------------------------------------------------------------
        SUBROUTINE SAVECL(X,Y)
        INTEGER COUNTC
        REAL XS(4),YS(4),XYPT(4,2)
        COMMON /LRBT/ XLEFT,XRIGHT,YBOTTM,YTOP,XS,YS,XYPT,COUNTC
C---
C---
C---                                    *CHECK IF OUTSIDE WINDOW
        IF (X.GT.XRIGHT.OR.X.LT.XLEFT) RETURN
        IF (Y.GT.YTOP.OR.Y.LT.YBOTTM)  RETURN
        COUNTC         = COUNTC + 1
        XYPT(COUNTC,1) = X
        XYPT(COUNTC,2) = Y
        RETURN
        END
C-----------------------------------------------------------------------
C       STORES AVER(X,Y,Z)
C-----------------------------------------------------------------------
        SUBROUTINE STRCI(NUMCI,INFCI,TECI,PARA)
        INTEGER TECI(3),SPLPT,NVX(2)
        REAL PARA(4,4),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,INFCI(3,2)
C---
        COMMON /DL/ XL,YL,ZL,DIS
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /COOR/ CRX,CRY,CRZ
C---
C---
        ERR   = 10.0 ** (-8.0)
        NUMCI = 0
        X2    = MIN(X1,XOLD)
        X3    = MAX(X1,XOLD)
        Y2    = MIN(Y1,YOLD)
        Y3    = MAX(Y1,YOLD)
        Z2    = MIN(Z1,ZOLD)
        Z3    = MAX(Z1,ZOLD)
C---
        CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
C---                                    *CHECK
        IF (X2.LT.CRX.AND.X3.GT.CRX) THEN
          IF (ABS(XL).LE.ERR) THEN
            XK = X1
            YK = Y1
            ZK = Z1
          ELSE
            RINT = ABS((CRX-XOLD) / XL)
            XK   = XOLD + XL * RINT
            YK   = YOLD + YL * RINT
            ZK   = ZOLD + ZL * RINT
          END IF
C---
          CALL AXYZ(XK,YK,ZK,X,Y,Z,PARA)
          IF (X.GT.XMAX.OR.X.LT.XMIN) GO TO 1
          IF (Y.GT.YMAX.OR.Y.LT.YMIN) GO TO 1
          CALL PLCMDS(7)
          NUMCI      = 1
          INFCI(1,1) = IX
          INFCI(1,2) = IY
          IF (YK.GE.CRY.AND.ZK.GE.CRZ) THEN
            TECI(1) = 2
          ELSE
            TECI(1) = 1
          END IF
        END IF
 1      CONTINUE
C---                                    *CHECK
        IF (Y2.LT.CRY.AND.Y3.GT.CRY) THEN
          IF (ABS(YL).LE.ERR) THEN
            XK = X1
            YK = Y1
            ZK = Z1
          ELSE
            RINT = ABS((CRY-YOLD) / YL)
            XK   = XOLD + XL * RINT
            YK   = YOLD + YL * RINT
            ZK   = ZOLD + ZL * RINT
          END IF
C---
          CALL AXYZ(XK,YK,ZK,X,Y,Z,PARA)
          IF (X.GT.XMAX.OR.X.LT.XMIN) GO TO 3
          IF (Y.GT.YMAX.OR.Y.LT.YMIN) GO TO 3
          CALL PLCMDS(7)
          DO 2 I=1,NUMCI
            IF (IX.EQ.INFCI(I,1).AND.IY.EQ.INFCI(I,2)) THEN
              GO TO 3
            END IF
 2        CONTINUE
          NUMCI          = NUMCI + 1
          INFCI(NUMCI,1) = IX
          INFCI(NUMCI,2) = IY
          IF (XK.GE.CRX.AND.ZK.GE.CRZ) THEN
            TECI(NUMCI) = 2
          ELSE
            TECI(NUMCI) = 1
          END IF
        END IF
 3      CONTINUE
C---                                    *CHECK
        IF (Z2.LT.CRZ.AND.Z3.GT.CRZ) THEN
          IF (ABS(ZL).LE.ERR) THEN
            XK = X1
            YK = Y1
            ZK = Z1
          ELSE
            RINT = ABS((CRZ-ZOLD) / ZL)
            XK   = XOLD + XL * RINT
            YK   = YOLD + YL * RINT
            ZK   = ZOLD + ZL * RINT
          END IF
          CALL AXYZ(XK,YK,ZK,X,Y,Z,PARA)
          IF (X.GT.XMAX.OR.X.LT.XMIN) GO TO 5
          IF (Y.GT.YMAX.OR.Y.LT.YMIN) GO TO 5
          CALL PLCMDS(7)
          DO 4 I=1,NUMCI
            IF (IX.EQ.INFCI(I,1).AND.IY.EQ.INFCI(I,2)) GO TO 5
  4       CONTINUE
          NUMCI          = NUMCI + 1
          INFCI(NUMCI,1) = IX
          INFCI(NUMCI,2) = IY
          IF (XK.GE.CRX.AND.YK.GE.CRY) THEN
            TECI(NUMCI) = 2
          ELSE
            TECI(NUMCI) = 1
          END IF
        END IF
 5      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS THREE PROJECTIONS (X,Y), (Y,Z) AND (X,Z
C       WITH DOTTED LINE
C-----------------------------------------------------------------------
        SUBROUTINE DIPJ(PARA,ST3,NUMPTS)
        REAL PARA(4,4),ST3(500,3)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        LOGICAL PJXZ,PJYZ,PJXY,P1,P2,P3,PSL
C---
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /PJXYZ/ PJXY,PJYZ,PJXZ,IPJ,PSL
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /COOR/ CRX,CRY,CRZ
        COMMON /DL/ XL,YL,ZL,DIS
        COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
C---
C---
        P1 = PJXY
        P2 = PJYZ
        P3 = PJXZ
        CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
        TOOCLS = DIS / RIDIS
        DO 3 I = 1,IPJ
C---                                    *GET EACH POINT
          RSKIP = DISD
          DO 2 J=1,NUMPTS
            X1 = ST3(J,1)
            Y1 = ST3(J,2)
            Z1 = ST3(J,3)
C---
            IF (P1) THEN
              Z1 = CRZ
            ELSE IF (P2) THEN
              X1 = CRX
            ELSE IF (P3) THEN
              Y1 = CRY
            END IF
C---                                    *PROJECTION USING SOLID LINE
            IF (PSL) THEN
              IF (J.EQ.1) THEN
                CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
                CALL CLIP(X,Y,1)
              ELSE
                CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
                IF (TOOCLS.GT.DIS) GO TO 2
                CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
                CALL CLIP(X,Y,2)
              END IF
              XOLD = X1
              YOLD = Y1
              ZOLD = Z1
              GO TO 2
            END IF
C---                                    *PROJECTION USING DOTS
            IF (J.EQ.1) THEN
              CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
              CALL CLIP(X,Y,1)
              XOLD  = X1
              YOLD  = Y1
              ZOLD  = Z1
              RSKIP = DISD
            ELSE
              CALL DISDOT(PARA)
            END IF
 2        CONTINUE
C---
          IF (P1) THEN
            P1 = .FALSE.
          ELSE IF (P2) THEN
            P2 = .FALSE.
          ELSE IF (P3) THEN
            P3 = .FALSE.
          END IF
 3      CONTINUE
        RETURN
        END
C-----------------------------------------------------------------------
C       EXECUTES B3D COMMAND. CALL SCAN B3D TO GET
C       MIN, MAX AND AVERAGE OF X,Y AND Z
C-----------------------------------------------------------------------
        SUBROUTINE EXB3D
        INTEGER XAXIS,YAXIS,ZAXIS
        REAL MIMA(3,2),VIEW(3),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        CHARACTER*2 COM
        LOGICAL B3DC
C---
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
        COMMON /AXB3D/ IB3DX,IB3DY,IB3DZ
        COMMON /CB3D/ B3DC
C---
C---
        CALL SCNB3D(IERROR)
C---                                    *ON ERROR IN FILE 7 RETURN
        IF (IERROR.NE.0) THEN
          CALL WERRS(IERROR)
          RETURN
        END IF
        B3DC  = .TRUE.
        XAXIS = IB3DX
        YAXIS = IB3DY
        ZAXIS = IB3DZ
C---                                    *KEEP MIN AND MAX IN MIMA
        MIMA(1,1) = MINQX
        MIMA(2,1) = MINQY
        MIMA(3,1) = MINQZ
        MIMA(1,2) = MAXQX
        MIMA(2,2) = MAXQY
        MIMA(3,2) = MAXQZ
        CALL ENT3DC(MIMA,VIEW,COM)
C---                                    *RESET B3D
        B3DC = .FALSE.
        RETURN
        END
C-----------------------------------------------------------------------
C       SCAN FILE 7 TO FIND MIN, MAX (X,Y,Z) AND AVERAGE
C-----------------------------------------------------------------------
        SUBROUTINE SCNB3D(IERROR)
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ,HOLD(10)
        INTEGER GRNUM,PTNUM,SPLPT,SPTNM
        LOGICAL FIRSTP
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /AXB3D/ IB3DX,IB3DY,IB3DZ
C---
C---
        POINT = 0
        FIRSTP = .TRUE.
        IERROR = 0
        NAXIS = MAX(IB3DX,IB3DY,IB3DZ)
        AVERX = 0
        AVERY = 0
        AVERZ = 0
        REWIND 17
 1      READ(17,*,ERR=4,END=5) GRNUM
        IF (GRNUM.EQ.0) GO TO 1
        BACKSPACE 17
 2      READ(17,*,ERR=3,END=5) GRNUM,PTNUM,SPLPT,SPTNM,
     +  (HOLD(I),I=1,NAXIS)
        IF (GRNUM.EQ.0) GOTO 1
        X = HOLD(IB3DX)
        Y = HOLD(IB3DY)
        Z = HOLD(IB3DZ)
        POINT = POINT + 1
        AVERX = AVERX + X
        AVERY = AVERY + Y
        AVERZ = AVERZ + Z
        IF (FIRSTP) THEN
          MINQX = X
          MINQY = Y
          MINQZ = Z
          MAXQX = X
          MAXQY = Y
          MAXQZ = Z
        ELSE
          IF (MINQX.GT.X) MINQX = X
          IF (MINQY.GT.Y) MINQY = Y
          IF (MINQZ.GT.Z) MINQZ = Z
          IF (MAXQX.LT.X) MAXQX = X
          IF (MAXQY.LT.Y) MAXQY = Y
          IF (MAXQZ.LT.Z) MAXQZ = Z
        END IF
        FIRSTP = .FALSE.
        IF(MOD(IABS(SPLPT),10).EQ.9.AND.IABS(PTNUM).NE.1)GOTO 1
        GO TO 2
 3      IF (GRNUM.EQ.0) GO TO 1
 4      IERROR = 7
        RETURN
 5      IF (POINT.EQ.0) THEN
          IERROR = 7
          RETURN
        END IF
C---                                    *AVERAGE OF (X,Y,Z)
        AVERX = AVERX / POINT
        AVERY = AVERY / POINT
        AVERZ = AVERZ / POINT
        RETURN
        END
C-----------------------------------------------------------------------
C       GET 3 NEW AXES FROM USER, IF ERROR IN INPUT THEN KEEP
C       OLD 3 AXIS AND MIN, MAX, AVER OF (X,Y,Z)
C-----------------------------------------------------------------------
        SUBROUTINE GAXB3D(MIMA,ERROR)
        INTEGER PARRY(1999),XAXIS,YAXIS,ZAXIS,ERROR
        REAL MIMA(3,2),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
        CHARACTER*80 ISTR,OSTR
        LOGICAL VALID
C---
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /IO/ IWRITE,ITERM,ISAVE
        COMMON /AXB3D/ IB3DX,IB3DY,IB3DZ
        COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
C---
C---
        ERROR = 0
        OSTR = ' ENTER THE NEW THREE AXES FOR (X,Y,Z) <B3D>'
 2      WRITE(ITERM,3) OSTR
        CALL READS8(ISTR)
        I = 1
        IF (ISTR(1:1).EQ.' ') I = 2
        CALL CONVT(ISTR,I,NUM,PARRY,VALID)
        IF (.NOT.VALID.OR.NUM.LT.3) THEN
          OSTR = ' ERROR :  ENTER AGAIN '
          GO TO 2
        END IF
C---
        IX1 = PARRY(1)
        IY1 = PARRY(2)
        IZ1 = PARRY(3)
        IMIN = MIN(IX1,IY1,IZ1)
        IMAX = MAX(IX1,IY1,IZ1)
C---*CHECK FOR ERROR
        IF (IMIN.LE.0.OR.IMAX.GT.8) THEN
          OSTR = ' ENTER THREE POSITIVE INTEGERS'
          GO TO 2
        END IF
C---
        IB3DX = IX1
        IB3DY = IY1
        IB3DZ = IZ1
C---                                    *KEEP OLD VALUE OF MIN,MA
        AX = AVERX
        AY = AVERY
        AZ = AVERZ
        XMIN = MINQX
        YMIN = MINQY
        ZMIN = MINQZ
        XMAX = MAXQX
        YMAX = MAXQY
        ZMAX = MAXQZ
C---                                    *SCAN FILE 7 AND GET NEW
C---                                    *MIN, MAX AND AVERAGE
        CALL SCNB3D(ERROR)
        IF (ERROR.EQ.0) THEN
          XAXIS = IB3DX
          YAXIS = IB3DY
          ZAXIS = IB3DZ
          MIMA(1,1) = MINQX
          MIMA(2,1) = MINQY
          MIMA(3,1) = MINQZ
          MIMA(1,2) = MAXQX
          MIMA(2,2) = MAXQY
          MIMA(3,2) = MAXQZ
          RETURN
        END IF
C---                                    *KEEP NEW VALUES
        IB3DX = XAXIS
        IB3DY = YAXIS
        IB3DZ = ZAXIS
        AVERX = AX
        AVERY = AY
        AVERZ = AZ
        MINQX = XMIN
        MINQY = YMIN
        MINQZ = ZMIN
        MAXQX = XMAX
        MAXQY = YMAX
        MAXQZ = ZMAX
        RETURN
 3      FORMAT(A)
        END
C-----------------------------------------------------------------------
C       DISPLAYS CURVES USING SOLID, DASHED, DOTTED
C       OR HEAVY LINE, IN THREE DIMENSIONS
C-----------------------------------------------------------------------
        SUBROUTINE TYPE3D(IFIRST,PARA)
        INTEGER NVX(2),SPLPT
        REAL PARA(4,4),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        LOGICAL SKIP,LINE,CONECT,DASH
C---
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /COOR/ CRX,CRY,CRZ
        COMMON /LDD1/ SKIP,LINE,CONECT,DASH
        COMMON /SYMB/ KTYPE
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /HEAVY/ X3,Y3,Z3,X4,Y4,Z4
C---
C---
        GO TO (1,2,3,4,5,6) KTYPE
C---                                    *SOLID LINE
 1      CALL PLCMDS(36)
        CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
        IF (IFIRST.EQ.1) THEN
          CALL CLIP(X,Y,1)
        ELSE
          CALL CLIP(X,Y,2)
        END IF
        CALL PLCMDS(20)
        RETURN
C---
C---                                   *DASHED LINE
 2      IF (IFIRST.EQ.1) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,1)
          CONECT = .TRUE.
          RCONCT = FDXYZ
          SKIP   = .FALSE.
        ELSE
          CALL DASHSL(PARA)
        END IF
        RETURN
C---                                    *DASHED AND SOLID LINE
 3      IF (X1.LT.CRX.OR.Y1.LT.CRY.OR.Z1.LT.CRZ) THEN
          LINE = .FALSE.
        ELSE
          LINE = .TRUE.
        END IF
C---
        IF (IFIRST.EQ.1) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,1)
          DASH = .NOT. LINE
        ELSE
          CALL LINDAS(PARA)
        END IF
        RETURN
C---                                    *DOTTED LINE
 4      IF (IFIRST.EQ.1) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,1)
          RSKIP = DISD
        ELSE
          CALL DISDOT(PARA)
        END IF
        RETURN
C---                                    *DOTTED AND SOLID
 5      IF (X1.LT.CRX.OR.Y1.LT.CRY.OR.Z1.LT.CRZ) THEN
          LINE = .FALSE.
        ELSE
          LINE = .TRUE.
        END IF
C---
        IF (IFIRST.EQ.1) THEN
          CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
          CALL CLIP(X,Y,1)
          DASH = .NOT. LINE
        ELSE
          CALL LINDOT(PARA)
        END IF
        RETURN
C---                                    *HEAVY LINE
 6      CALL AXYZ(X1,Y1,Z1,X4,Y4,Z4,PARA)
        CALL PLCMDS(36)
        IF (IFIRST.EQ.1) THEN
          X  = X4
          Y  = Y4
          X3 = X4
          Y3 = Y4
          CALL CLIP(X,Y,1)
        ELSE
          X = X3
          Y = Y3
          CALL CLIP(X,Y,3)
          X = X4
          Y = Y4
          CALL CLIP(X,Y,4)
          X = X3
          Y = Y3
          CALL CLIP(X,Y,3)
          X = X4
          Y = Y4
          CALL CLIP(X,Y,2)
          X3 = X4
          Y3 = Y4
        END IF
        CALL PLCMDS(20)
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAY THE CURVES FROM FILE 7 OR OF <B3D>
C-----------------------------------------------------------------------
        SUBROUTINE DPB3D(TRAN,PARA,MP,PNTS)
        INTEGER XAXIS,YAXIS,ZAXIS,TECI(3),NVX(2)
        INTEGER GRNUM,PTNUM,SPLPT,SPTNM,PNTCNT,PRESGN
        REAL TRAN(4,4),PARA(4,4),HOLD(10),IX,IY,MINSX,MAXSX,MINSY,MAXSY
        REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ,INFCI(3,2),PNTS(1999,4)
        CHARACTER*1 MP(81,251)
        LOGICAL CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,PLTR,POINT
        LOGICAL SKIP,LINE,CONECT,DASH,DFT,USR,BRNCH
        LOGICAL TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP,ST3D
C---
C---
        COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +  MINSY,MAXSY,NVX,SPLPT
        COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +  X1,Y1,Z1,DIST,DISD
        COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
        COMMON /PLOTT/ PLTR,POINT
        COMMON /SYMB/ KTYPE
        COMMON /LDD1/ SKIP,LINE,CONECT,DASH
        COMMON /DS/   CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,ST3D
        COMMON /UOPTS/ DFT,USR,BRNCH,ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,
     +  GRIDS,DP
        COMMON /DL/ XL,YL,ZL,DIS
        COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
        COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +  MAXQX,MAXQY,MAXQZ
        COMMON /SPNUM/ SP1,SP2,SP3,SP4
C---
C---
        NUMCI = 0
        INDX = 0
        PNTCNT = 0
        PRESGN = 0
        NMX    = MAX(XAXIS,YAXIS,ZAXIS)
        CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
        TOOCLS = DIS / RIDIS
        REWIND 17
 1      READ(17,*,END=5) GRNUM
        IF (GRNUM.EQ.0) GO TO 1
        BACKSPACE 17
        IFIRST = 1
        RCONCT = FDXYZ
        SKIP   = .FALSE.
        CONECT = .TRUE.
 2      READ(17,*,END=5,ERR=4) GRNUM,PTNUM,SPLPT,SPTNM,
     +  (HOLD(I),I=1,NMX)
        IF (GRNUM.EQ.0) GOTO 1
C---
        IF (ABS(PTNUM).EQ.1) THEN
          IFIRST = 1
          SKIP   = .FALSE.
          CONECT = .TRUE.
          RCONCT = FDXYZ
        END IF
        X1 = HOLD(XAXIS)
        Y1 = HOLD(YAXIS)
        Z1 = HOLD(ZAXIS)
        CALL AXYZ(X1,Y1,Z1,X0,Y0,Z0,TRAN)
        X1 = X0
        Y1 = Y0
        Z1 = Z0
        IF (IFIRST.NE.1) THEN
          CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
          IF (TOOCLS.GT.DIS) GO TO 6
        END IF
        IF (CIR.AND.IFIRST.GT.1) THEN
          CALL STRCI(NUMCI,INFCI,TECI,PARA)
        END IF
C---                                    *DISPLAY THE CURVES
        CALL TYPE3D(IFIRST,PARA)
C---                                    *CIRCLE
        IF (CIR) THEN
          DO 3 J=1,NUMCI
            IF (TECI(J).EQ.1) THEN
              IX = INFCI(J,1)
              IY = INFCI(J,2)
              CALL HCIR
              CALL PLCMDS(4)
            ELSE
              IX = INFCI(J,1)
              IY = INFCI(J,2)
              CALL SCIR
              CALL PLCMDS(4)
            END IF
 3        CONTINUE
        END IF
        XOLD = X1
        YOLD = Y1
        ZOLD = Z1
C---
 6      CALL WINCT(X,Y,XNEW1,YNEW1)
        IF (BRNCH.OR.POINT) THEN
          IF (IFIRST.EQ.1) THEN
            CALL MAP(MP,XNEW1,YNEW1,XNEW1,YNEW1)
          ELSE
            CALL MAP(MP,XNEW1,YNEW1,XOLD1,YOLD1)
          END IF
          XOLD1 = XNEW1
          YOLD1 = YNEW1
        END IF
        IF (X.GE.XMIN.AND.X.LE.XMAX.AND.Y.GE.YMIN.AND.Y.LE.YMAX) THEN
          IF (POINT.AND.SPTNM.NE.0) THEN
            INDX = INDX + 1
            PNTS(INDX,1) = XNEW1
            PNTS(INDX,2) = YNEW1
            PNTS(INDX,3) = SPTNM
            PNTS(INDX,4) = 1
          END IF
C---
          IF (BRNCH.AND.GRNUM.NE.PRESGN) THEN
            PNTCNT = PNTCNT + 1
          END IF
C---
          IF (PNTCNT.EQ.10.AND.GRNUM.NE.PRESGN) THEN
            PRESGN = GRNUM
            INDX = INDX + 1
            PNTS(INDX,1) = XNEW1
            PNTS(INDX,2) = YNEW1
            PNTS(INDX,3) = GRNUM
            PNTS(INDX,4) = 4
            PNTCNT = 0
          END IF
        END IF
        IFIRST = IFIRST + 1
        IF(MOD(IABS(SPLPT),10).EQ.9.AND.IABS(PTNUM).NE.1)GOTO 1
        GO TO 2
C---
 4      IF (GRNUM.EQ.0) GO TO 1
 5      CONTINUE
C---                                    *DISPLAY PROJECTION FOR <B3D>
        IF (DFDOT) THEN
          CALL DPJB3D(TRAN,PARA)
        END IF
        RETURN
        END
C-----------------------------------------------------------------------
C       DISPLAYS CURVES FROM FILE 8 OR <3D>
C-----------------------------------------------------------------------
         SUBROUTINE DP3D(TRAN,PARA,MP,PNTS)
         INTEGER ENLAB(1999),PARRY(1999),XAXIS,YAXIS,ZAXIS
         INTEGER TECI(3),NVX(2),GRNUM,PTNUM,SPLPT,SPNUM
         INTEGER SLAB(1999),BLKSZE,MPJ(1999)
         REAL TRAN(4,4),PARA(4,4),HOLD(251),ST3(500,3),INFCI(3,2)
         REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY,PNTS(1999,4)
         REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
         LOGICAL CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,PLTR,POINT
         LOGICAL LABYET,FOUND,SKIP,LINE,CONECT,DASH,ST3D
         CHARACTER*1 MP(81,251)
C---
         COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +   MINSY,MAXSY,NVX,SPLPT
         COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
         COMMON /PLOTT/ PLTR,POINT
         COMMON /SYMB/ KTYPE
         COMMON /LBQ/  SLAB,NLAB,NAXIS,ENLAB,NUMLB
         COMMON /LDD1/ SKIP,LINE,CONECT,DASH
         COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +   X1,Y1,Z1,DIST,DISD
         COMMON /DS/ CIR,DFDOT,MIDDLE,DIAXIS,DCOOR,PRNUM,ST3D
         COMMON /DL/ XL,YL,ZL,DIS
         COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
         COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +   MAXQX,MAXQY,MAXQZ
         COMMON /SPNUM/ SP1,SP2,SP3,SP4
C---
C---
         LBPT  = 10
         INDX  = 0
         NOLAB = NUMLB
         NUMCI = 0
         IMPJ  = 0
         DO 1 I=1,NUMLB
           PARRY(I) = 0
 1       CONTINUE
C---
         CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
         TOOCLS = DIS / RIDIS
         REWIND 18
 2       IF (NOLAB.EQ.0) GO TO 9
         READ(18,*,END=9) GRNUM,PTNUM,SPLPT,SPNUM,X0,Y0,NUMPTS,NAXIS1,
     +   BLKSZE
         FOUND = .FALSE.
         LABYET = .FALSE.
         LINE = .TRUE.
C---
         DO 3 I=1,NUMLB
           IF (PARRY(I).EQ.0.AND.ENLAB(I).EQ.SPNUM) THEN
             PARRY(I) = 1
             FOUND    = .TRUE.
           END IF
 3       CONTINUE
         IF (.NOT.FOUND) GO TO 6
         NOLAB = NOLAB - 1
         SKIP = .FALSE.
         CONECT = .TRUE.
         RCONCT = FDXYZ
         ICOUNT = 0
C---
         DO 5 I=1,NUMPTS
           READ(18,*,END=8) (HOLD(J),J=1,NAXIS1)
           X1 = HOLD(XAXIS)
           Y1 = HOLD(YAXIS)
           Z1 = HOLD(ZAXIS)
           CALL AXYZ(X1,Y1,Z1,X0,Y0,Z0,TRAN)
           X1 = X0
           Y1 = Y0
           Z1 = Z0
           IF (I.NE.1) THEN
             CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
             IF (TOOCLS.GT.DIS) GO TO 8
           END IF
           ICOUNT = ICOUNT + 1
           IF (DFDOT.AND.ICOUNT.LE.500) THEN
             ST3(ICOUNT,1) = X1
             ST3(ICOUNT,2) = Y1
             ST3(ICOUNT,3) = Z1
           END IF
           IF (CIR.AND.I.GT.1) THEN
             CALL STRCI(NUMCI,INFCI,TECI,PARA)
           END IF
C---                                    *DISPLAY THE CURVES
           CALL TYPE3D(I,PARA)
           XOLD = X1
           YOLD = Y1
           ZOLD = Z1
           IF (CIR) THEN
             DO 4 J=1,NUMCI
               IF (TECI(J).EQ.1) THEN
                 IX = INFCI(J,1)
                 IY = INFCI(J,2)
                 CALL HCIR
                 CALL PLCMDS(4)
               ELSE IF (TECI(J).EQ.2) THEN
                 IX = INFCI(J,1)
                 IY = INFCI(J,2)
                 CALL SCIR
                 CALL PLCMDS(4)
               END IF
 4           CONTINUE
           END IF
C---
 8         IF (POINT) THEN
             CALL PLCMDS(7)
             XNEW1 = IX
             YNEW1 = IY
             IF (I.EQ.1) THEN
               CALL MAP(MP,XNEW1,YNEW1,XOLD1,YOLD1)
             ELSE
               CALL MAP(MP,XNEW1,YNEW1,XOLD1,YOLD1)
             END IF
             XOLD1 = XNEW1
             YOLD1 = YNEW1
             IF (X.GE.XMIN.AND.X.LE.XMAX.AND.
     +           Y.GE.YMIN.AND.Y.LE.YMAX) THEN
               IF (NUMPTS.LE.15) LBPT = 2
               IF (I.EQ.LBPT.AND..NOT.LABYET) THEN
                 LBPT = LBPT + 10
                 IF (LBPT.GE.NUMPTS) LBPT = 15
                 INDX = INDX + 1
                 PNTS(INDX,1) = IX
                 PNTS(INDX,2) = IY
                 PNTS(INDX,3) = SPNUM
                 PNTS(INDX,4) = 4
                 LABYET       = .TRUE.
               END IF
             END IF
           END IF
5        CONTINUE
         NA1 = INT(NAXIS1 / 7)
         IF (MOD(NAXIS1,7).NE.0) NA1 = NA1 + 1
         BLKSZE = BLKSZE - NUMPTS * NA1
         IF (DFDOT.AND.ICOUNT.LE.500) THEN
           CALL DIPJ(PARA,ST3,ICOUNT)
         ELSE IF (DFDOT.AND.ICOUNT.GT.500) THEN
           IMPJ      = IMPJ + 1
           MPJ(IMPJ) = SPNUM
         END IF
 6       CONTINUE
         DO 7 I=1,BLKSZE
           READ(18,*)
 7       CONTINUE
         GO TO 2
 9       IF (DFDOT.AND.IMPJ.GT.0) THEN
           CALL DIPJA(TRAN,PARA,MPJ,IMPJ)
         END IF
         RETURN
         END
C-----------------------------------------------------------------------
C        MAPS THE CURVES IN MP ARRAY
C-----------------------------------------------------------------------
         SUBROUTINE MAP(MP,XNEW,YNEW,XOLD,YOLD)
         CHARACTER*1 MP(81,251)
         COMMON /SPNUM/ SP1,SP2,SP3,SP4
         DX = (XNEW - XOLD) / SP2
         DY = (YNEW - YOLD) / SP4
         INDEX = INT(MAX(ABS(DX),ABS(DY))) + 1
         X1 = (XOLD - SP1) / SP2 + 1
         Y1 = (YOLD - SP3) / SP4 + 1
         DO I=0,INDEX-1
           F = REAL(I)/INDEX
           IX = INT(X1 + F * DX)
           IY = INT(Y1 + F * DY)
           IF (IX.GE.1.AND.IX.LE.81.AND.IY.GE.1.AND.IY.LE.251) THEN
             MP(IX,IY) = 'X'
           END IF
         ENDDO
         RETURN
         END
C-----------------------------------------------------------------------
C        DISPLAY PROJECTION OF UNIT 8
C-----------------------------------------------------------------------
         SUBROUTINE DIPJA(TRAN,PARA,MPJ,IMPJ)
         REAL PARA(4,4),TRAN(4,4),MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
         REAL IX,IY,MINSX,MINSY,MAXSX,MAXSY,HOLD(251)
         INTEGER GRNUM,PTNUM,SPNUM,BLKSZE,XAXIS,YAXIS,ZAXIS
         INTEGER SPLPT,NVX(2),DOUBLE(1999),MPJ(1999)
         LOGICAL PJXZ,PJYZ,PJXY,P1,P2,P3,PSL,FOUND
C---
C---
         COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     +   MINSY,MAXSY,NVX,SPLPT
         COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +   X1,Y1,Z1,DIST,DISD
         COMMON /PJXYZ/ PJXY,PJYZ,PJXZ,IPJ,PSL
         COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +   MAXQX,MAXQY,MAXQZ
         COMMON /COOR/ CRX,CRY,CRZ
         COMMON /DL/ XL,YL,ZL,DIS
         COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
         COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
C---
C---
         P1 = PJXY
         P2 = PJYZ
         P3 = PJXZ
         CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
         TOOCLS = DIS / RIDIS
         DO 1 I=1,IMPJ
           DOUBLE(I) = 0
 1       CONTINUE
         NOLAB = IMPJ
C---
         DO 3 I=1,IMPJ
           REWIND 18
 2         IF (NOLAB.EQ.0) GO TO 7
           READ(18,*) GRNUM,PTNUM,SPLPT,SPNUM,X0,Y0,NUMPTS,NAXIS,BLKSZE
           FOUND = .FALSE.
C---                                    *FIND LABEL
           DO 4 J=1,IMPJ
             IF (SPNUM.EQ.MPJ(J).AND.DOUBLE(J).EQ.0) THEN
               FOUND = .TRUE.
               DOUBLE(J) = 1
             END IF
 4         CONTINUE
C---                                    *IF NOT FOUND SKIP
           IF (.NOT.FOUND) GO TO 8
           NOLAB = NOLAB - 1
           RSKIP = DISD
C---                                    *GET EACH AXIS FOR DISPLAY
           DO 5 J =1,NUMPTS
             READ(18,*) (HOLD(L),L=1,NAXIS)
             X0 = HOLD(XAXIS)
             Y0 = HOLD(YAXIS)
             Z0 = HOLD(ZAXIS)
             CALL AXYZ(X0,Y0,Z0,X1,Y1,Z1,TRAN)
             IF (P1) THEN
               Z1 = CRZ
             ELSE IF (P2) THEN
               X1 = CRX
             ELSE IF (P3) THEN
               Y1 = CRY
             END IF
C---                                 *DISPLAY WITH SOLID LINE
             IF (PSL) THEN
               CALL PLCMDS(32)
               IF (J.EQ.1) THEN
                 CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
                 CALL CLIP(X,Y,1)
               ELSE
                 CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
                 IF (TOOCLS.GT.DIS) GO TO 5
                 CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
                 CALL CLIP(X,Y,2)
               END IF
               XOLD = X1
               YOLD = Y1
               ZOLD = Z1
               CALL PLCMDS(20)
               GO TO 5
             END IF
C---                                    *DISPLAY PROJECTION USING DOTS
             IF (J.EQ.1) THEN
               CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
               CALL CLIP(X,Y,1)
               XOLD = X1
               YOLD = Y1
               ZOLD = Z1
             ELSE
               CALL DISDOT(PARA)
               CALL PLCMDS(32)
             END IF
 5         CONTINUE
           NA = INT(NAXIS/7)
           IF (MOD(NAXIS,7).NE.0) NA = NA + 1
           BLKSZE = BLKSZE - NUMPTS * NA
 8         CONTINUE
C---                                    *SKIP BLOCK
           DO 6 J=1,BLKSZE
             READ(18,*)
 6         CONTINUE
           GO TO 2
C---                                    *RESET PROJECTION FLAG
 7         IF (P1) THEN
             P1 = .FALSE.
           ELSE IF (P2) THEN
             P2 = .FALSE.
           ELSE IF (P3) THEN
             P3 = .FALSE.
           END IF
 3       CONTINUE
         CALL PLCMDS(20)
         RETURN
         END
C-----------------------------------------------------------------------
C        DISPLAY PROJECTION OF UNIT 7
C-----------------------------------------------------------------------
         SUBROUTINE DPJB3D(TRAN,PARA)
         REAL PARA(4,4),TRAN(4,4),HOLD(10)
         REAL MINQX,MINQY,MINQZ,MAXQX,MAXQY,MAXQZ
         INTEGER XAXIS,YAXIS,ZAXIS,GRNUM,PTNUM,SPLPT,SPTNM
         LOGICAL PJXZ,PJYZ,PJXY,P1,P2,P3,PSL,FIRST
C---
         COMMON /PJXYZ/ PJXY,PJYZ,PJXZ,IPJ,PSL
         COMMON /LDD2/ RSKIP,RCONCT,FDXYZ,FSKXYZ,XOLD,YOLD,ZOLD,
     +   X1,Y1,Z1,DIST,DISD
         COMMON /COOR/ CRX,CRY,CRZ
         COMMON /AXIS/ XAXIS,YAXIS,ZAXIS
         COMMON /AMIMA/ AVERX,AVERY,AVERZ,MINQX,MINQY,MINQZ,
     +   MAXQX,MAXQY,MAXQZ
         COMMON /DL/ XL,YL,ZL,DIS
         COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
C---
C---
         CALL PLCMDS(32)
         P1 = PJXY
         P2 = PJYZ
         P3 = PJXZ
         RSKIP = DISD
         CALL DISTLN(MAXQX,MAXQY,MAXQZ,MINQX,MINQY,MINQZ)
         TOOCLS = DIS / RIDIS
         NMX = MAX(XAXIS,YAXIS,ZAXIS)
         DO 6 INDEX = 1,IPJ
           REWIND 17
 1         READ(17,*,END=5) GRNUM
           IF (GRNUM.EQ.0) GO TO 1
           BACKSPACE 17
           FIRST = .TRUE.
 2         READ(17,*,END=5,ERR=7) GRNUM,PTNUM,SPLPT,SPTNM,
     +     (HOLD(I),I=1,NMX)
           IF (GRNUM.EQ.0) GOTO 1
           IF (ABS(PTNUM).EQ.1) THEN
             FIRST = .TRUE.
             RSKIP = DISD
           END IF
           X2 = HOLD(XAXIS)
           Y2 = HOLD(YAXIS)
           Z2 = HOLD(ZAXIS)
           CALL AXYZ(X2,Y2,Z2,X1,Y1,Z1,TRAN)
           IF (P1) THEN
             Z1 = CRZ
           ELSE IF (P2) THEN
             X1 = CRX
           ELSE IF (P3) THEN
             Y1 = CRY
           END IF
C---                                    *SOLID LINE
           IF (PSL) THEN
             IF (FIRST) THEN
               CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
               CALL CLIP(X,Y,1)
             ELSE
               CALL DISTLN(X1,Y1,Z1,XOLD,YOLD,ZOLD)
               IF (TOOCLS.GT.DIS) GOTO 4
               CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
               CALL CLIP(X,Y,2)
             END IF
             XOLD = X1
             YOLD = Y1
             ZOLD = Z1
             GO TO 4
           END IF
C---
           IF (FIRST) THEN
             CALL AXYZ(X1,Y1,Z1,X,Y,Z,PARA)
             CALL CLIP(X,Y,1)
             XOLD = X1
             YOLD = Y1
             ZOLD = Z1
           ELSE
             CALL DISDOT(PARA)
             CALL PLCMDS(32)
           END IF
 4         CONTINUE
           FIRST = .FALSE.
           IF(MOD(IABS(SPLPT),10).EQ.9.AND.IABS(PTNUM).NE.1)GOTO 1
           GO TO 2
 7         IF (GRNUM.EQ.0) GO TO 1
 5         IF (P1) THEN
             P1 = .FALSE.
           ELSE IF (P2) THEN
             P2 = .FALSE.
           ELSE IF (P3) THEN
             P3 = .FALSE.
           END IF
 6       CONTINUE
         CALL PLCMDS(20)
         RETURN
         END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
         SUBROUTINE CRFLNM(ICODE)
         CHARACTER*20 FLNAME
C--
	 GO TO (1, 2) ICODE
 1	 WRITE(6,*) ' ENTER FILE NAME:'
	 READ(5,3) FLNAME
	 IF (FLNAME .EQ.' ') THEN
	   OPEN(UNIT=16, FILE='fig.1', STATUS='unknown',
     +           ACCESS='sequential')
	 ELSE 
	   CALL RESLAB(FLNAME)
	   OPEN(UNIT=16, FILE=FLNAME, STATUS='unknown',
     +          ACCESS='sequential')
	 END IF
	 RETURN
C--
 2	 CLOSE(16)
	 RETURN
C--
 3	 FORMAT(A20)
	 END
