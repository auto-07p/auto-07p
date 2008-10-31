C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    PLAUT : A program for plotting AUTO Unit 7 and Unit 8 Output
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    AUTHORS :       - WILLIAM R. MORRILL  (MAY  15, 1982)
C                    - PATRICK P.C. LEUNG  (NOV.  3, 1982) MODIFIED
C                    - MIKE MIKALAJUNAS    (FEB.  1, 1984) MODIFIED
C                    - NGUYEN, THANH LONG  (JUN.  1, 1985) MODIFIED + 3D
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  THE MAIN PROGRAM SETS UP INITIAL USER OPTIONS. IT READS AND DECODES
C  USER OPTIONS AND COMMANDS, SETTING THE APPROPRIATE FLAGS IN COMMON
C  AREA /UOPTS/ AND CALLS SUBROUTINES TO EXECUTE THE INDICATED COMMAND.
C  USER OPTIONS CAN BE SEPARATE FROM OR PREFIXED TO THE BASIC COMMAND
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C---
       USE COMPAT
       CHARACTER*80 ISTR,OSTR,PART2*2,INB,INS
       CHARACTER*4 TTITLE*80,BTITLE*80
     * ,XLAB*80,YLAB*80
       CHARACTER*1 CH,NOTC*10
       LOGICAL DFT,USR,PLTR,BRNCH,POINT,TIT,AXLB,QLBS,GRIDS,DP
       LOGICAL TOP,BOTTOM,CHECKB,CHECKQ,SAVE,SYMBOL
       LOGICAL QSCOM,CAXIS,ERRQ,INTGLB,DDIM,IBD2
       LOGICAL B3DC
       INTEGER ERROR,XAXS,YAXS,PARRY(1999)
       INTEGER SPLPT,NVX(2)
       INTEGER SLAB(1999),ERROR8,ICD(4)
       INTEGER SYMBL(10),ITP(10)
       INTEGER ENLAB(1999),ICDS(4),ID2D(1999),IDS2D(1999)
       REAL IX,IY,MINSX,MAXSX,MINSY,MAXSY
       COMMON /LABLS/TTITLE,BTITLE,XLAB,YLAB
       COMMON /PLVARS/ IX,IY,X,Y,XMIN,XMAX,YMIN,YMAX,MINSX,MAXSX,
     * MINSY,MAXSY,NVX,SPLPT
       COMMON /UOPTS/DFT,USR,BRNCH,
     * ICL,ICT,TOP,BOTTOM,TIT,AXLB,QLBS,GRIDS,DP
       COMMON /IO/ IWRITE,ITERM,ISAVE
       COMMON /QSC/ QSCOM
       COMMON /PLOTT/ PLTR,POINT
       COMMON /INKA/ CAXIS
       COMMON /LBQ/ SLAB,NLAB,NAXIS,ENLAB,NUMLB
       COMMON /DIFFA/ DDIM
       COMMON /IAOCS/ RIDASH,RIDOT,RICIR,RISYBL,RIDIS,RIDSHS
       COMMON /UTDSH/ UDSH,UDSHS,ULD,USD,RIDS1
       COMMON /BD2/ IBD2
       COMMON /RADI/ RASY,RATY,RADI,RADINC
       COMMON /SY2D/ ID2D,IDS2D
       COMMON /BDSY/ ITP,SYMBL
       COMMON /AXB3D/ IB3DX,IB3DY,IB3DZ
       COMMON /CB3D/ B3DC
       COMMON /SPNUM/ SP1,SP2,SP3,SP4
C---
       IF(AUTARGC()==2)THEN
         CALL AUTGETARG(1,INB)
         CALL AUTGETARG(2,INS)
       ELSE
         INB='fort.17'
         INS='fort.18'
       ENDIF
       OPEN(17,FILE=INB,STATUS='unknown',ACCESS='sequential')
       OPEN(18,FILE=INS,STATUS='unknown',ACCESS='sequential')
C
       IWRITE = 6
       ITERM  = 6
       ISAVE  = 16
       NVX(1) = 1
       NVX(2) = 2
       XLAB   = ' '
       YLAB   = ' '
       TTITLE = ' '
       BTITLE = ' '
       CHECKB = .FALSE.
       CHECKQ = .FALSE.
       USR    = .FALSE.
       DFT    = .FALSE.
       QLBS   = .FALSE.
       PLTR   = .FALSE.
       POINT  = .FALSE.
       BRNCH  = .FALSE.
       DP     = .FALSE.
       SYMBOL = .FALSE.
       QSCOM  = .FALSE.
       CAXIS  = .FALSE.
       ERRQ   = .FALSE.
       INTGLB = .TRUE.
       DDIM   = .FALSE.
       IBD2   = .FALSE.
       B3DC   = .FALSE.
       SAVE   = .FALSE.
       ICD(1) = 1
       ICD(2) = 2
       ICD(3) = 3
       ICD(4) = 4
       IB3DX  = 1
       IB3DY  = 3
       IB3DZ  = 4
       CALL IBDSY(ITP,SYMBL)
       CALL ICDS0(ICDS)
C---                                    *SCREEN SIZE
       CALL DFREV
       CALL DFITVL
C---                                    *RELATIVE DIMENSION
       CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
       OSTR = ' ENTER <HELP> IN CASE OF DIFFICULTY'
       WRITE(IWRITE,1)OSTR
 10    OSTR = ' ENTER COMMAND'
       WRITE(IWRITE,1)OSTR
       CALL READS8(ISTR)
C---                                    *TERMINATION
 11    IF(ISTR.EQ.'STOP'.OR.ISTR.EQ.'stop'.OR.
     +    ISTR.EQ.'EXIT'.OR.ISTR.EQ.'exit'.OR.
     +    ISTR.EQ.'QUIT'.OR.ISTR.EQ.'quit'.OR.
     +    ISTR.EQ.'END' .OR.ISTR.EQ.'end') STOP
C---                                    *SCREEN
       IF(ISTR.EQ.'SCR'.OR.ISTR.EQ.'SCREEN'.OR.
     +    ISTR.EQ.'scr'.OR.ISTR.EQ.'screen') THEN
         WRITE(ITERM,46) MINSX,MAXSX,MINSY,MAXSY,SP1
         READ(5,*,END=10,ERR=10) MINSX,MAXSX,MINSY,MAXSY
         GO TO 10
       END IF
C---                                    *SET SYMBOL
       IF (ISTR.EQ.'SS'.OR.ISTR.EQ.'ss'.OR.
     +     ISTR.EQ.'SET SYM'.OR.ISTR.EQ.'set sym') THEN
         RADI = RASY
         CALL SHWSYS
         GO TO 10
       END IF
C---                                    *SET DIFFERENTIAL PLOTS
         IF (ISTR.EQ.'SD '.OR.ISTR.EQ.'sd ') THEN
           CALL SDFPLT(ICD,ICDS)
           ITS = 1
           GOTO 10
         ENDIF
C---                                    *LABEL
       IF (ISTR.EQ.'LAB'.OR.ISTR.EQ.'LABEL'.OR.
     +     ISTR.EQ.'lab'.OR.ISTR.EQ.'label') THEN
         CALL GERR (ERRQ,IERR8,ERROR8,INTGLB,1)
         GO TO 10
       END IF
C---                                    *CLEAR
       IF (ISTR.EQ.'C'.OR.ISTR.EQ.'c'.OR.
     +     ISTR.EQ.'CL'.OR.ISTR.EQ.'cl'.OR.
     +     ISTR.EQ.'CLR'.OR.ISTR.EQ.'clr'.OR.
     +     ISTR.EQ.'CLEAR'.OR.ISTR.EQ.'clear') THEN
         CALL PLCMDS(1)
         CALL PLCHDW(3)
         GO TO 10
       END IF
C---                                    *SAVE
       IF (ISTR.EQ.'SAVE'.OR.ISTR.EQ.'save'.OR.
     +     ISTR.EQ.'SAV' .OR.ISTR.EQ.'sav' .OR.
     +     ISTR.EQ.'SA'  .OR.ISTR.EQ.'sa'  .OR.
     +     ISTR.EQ.'S'   .OR.ISTR.EQ.'s'   ) THEN
C---                                    *SAVE COMMAND FOR "BD"
         IF (CHECKB) THEN
           SAVE = .FALSE.
           CALL ASKSVE(SAVE)
	   IF (SAVE) CALL CRFLNM(1)
           CALL BDS(ISTR,I+2,SAVE,ICDS,ICD,SYMBOL)
           CALL PLCHDW(3)
	   IF (SAVE) CALL CRFLNM(2)
           CALL ASKSVE(SAVE)
           CHECKB = .FALSE.
         ELSE IF (CHECKQ) THEN
C---                                    *SAVE COMMAND FOR "2D"
           IWRITE = ISAVE
	   IF (SAVE) CALL CRFLNM(1)
           CALL SPAGE
           CALL DRW2D(XAXS,YAXS,PARRY,ERROR,NUM,POINT,PLTR)
           CALL PLCHDW(3)
	   IF (SAVE) CALL CRFLNM(2)
            IWRITE = ITERM
           CALL COMPLT
           CHECKQ = .FALSE.
         END IF
         GO TO 10
       END IF
C---                                    *HELP
       IF (ISTR.EQ.'HELP'.OR.ISTR.EQ.'help') THEN
         CALL HELPS(ISTR)
         IF(ISTR.NE.' ') GO TO 11
         GO TO 10
       END IF
C---                                    *SHOW SYMBOL
       IF (ISTR.EQ.'SY    '.OR.ISTR.EQ.'sy    '.OR.
     +     ISTR.EQ.'SYM   '.OR.ISTR.EQ.'sym   '.OR.
     +     ISTR.EQ.'SYMBOL'.OR.ISTR.EQ.'symbol') THEN
         SYMBOL = .TRUE.
         GO TO 10
       END IF
C---                                    *SYMBOL
       I = 1
       DO 100 J=1,6
         PART2 = ISTR(I:I+1)
         IF (PART2.EQ.'SY'.OR.PART2.EQ.'sy')  THEN
           SYMBOL = .TRUE.
           ITS    = 1
         END IF
         I = I + 2
 100   CONTINUE
C---                                    *DIFFERENTIAL PLOTS
         I = 1
         DO 103 J=1,6
           PART2 = ISTR(I:I+1)
           IF (PART2.EQ.'DP'.OR.PART2.EQ.'dp') THEN
             DP  = .TRUE.
             ITS = 1
           END IF
           I = I + 2
 103     CONTINUE
C---                                    *VERTICAL AXIS
       I = 1
       DO 105 J=1,6
         PART2 = ISTR(I:I+1)
         IF (PART2.EQ.'AX'.OR.PART2.EQ.'ax') THEN
       OSTR = ' ENTER HORIZONTAL AND VERTICAL AXIS NUMBER (1,2,...) :'
         WRITE(IWRITE,1)OSTR
         READ(5,*,ERR=10)(NVX(L),L=1,2)
         ITS  = 1
         IBD2 = .FALSE.
       ENDIF
       I = I + 2
 105   CONTINUE
C---                                    *SET TITLES
       I = 1
       DO 106 J=1,6
         PART2 = ISTR(I:I+1)
         IF (PART2.EQ.'ST'.OR.PART2.EQ.'st') THEN
           CALL ENTGRD(1)
           TIT  = .TRUE.
           AXLB = .TRUE.
           QLBS = .FALSE.
           ITS = 1
         END IF
         I = I + 2
 106   CONTINUE
C---                                    *D0
       I = 1
       DO 107 J=1,6
         PART2 = ISTR(I:I+1)
         IF (PART2.EQ.'D0'.OR.PART2.EQ.'d0') THEN
           DFT    = .TRUE.
           USR    = .FALSE.
           BRNCH  = .FALSE.
           POINT  = .FALSE.
           TIT    = .FALSE.
           TOP    = .FALSE.
           BOTTOM = .FALSE.
           AXLB   = .FALSE.
           QLBS   = .TRUE.
           GRIDS  = .FALSE.
           SYMBOL = .TRUE.
           DP     = .FALSE.
           ICD(1) = 1
           ICD(2) = 1
           ICD(3) = 1
           ICD(4) = 1
           ITS    = 1
           CALL ICDS0(ICDS)
         END IF
         I = I + 2
 107   CONTINUE
C---                                    *D1
       I = 1
       DO 108 J=1,6
         PART2 = ISTR(I:I+1)
         IF (PART2.EQ.'D1'.OR.PART2.EQ.'d1') THEN
           DFT    = .TRUE.
           USR    = .FALSE.
           BRNCH  = .FALSE.
           POINT  = .TRUE.
           TIT    = .FALSE.
           TOP    = .FALSE.
           BOTTOM = .FALSE.
           AXLB   = .FALSE.
           QLBS   = .TRUE.
           GRIDS  = .FALSE.
           SYMBOL = .TRUE.
           DP     = .TRUE.
           ICD(1) = 1
           ICD(2) = 2
           ICD(3) = 1
           ICD(4) = 2
           ITS    = 1
           CALL ICDS0(ICDS)
         END IF
         I = I + 2
 108   CONTINUE
C---                                    *D2
       I = 1
       DO 109 J=1,6
         PART2    = ISTR(I:I+1)
         IF (PART2.EQ.'D2'.OR.PART2.EQ.'d2') THEN
           DFT    = .TRUE.
           USR    = .FALSE.
           BRNCH  = .FALSE.
           POINT  = .FALSE.
           TIT    = .FALSE.
           TOP    = .FALSE.
           BOTTOM = .FALSE.
           AXLB   = .FALSE.
           QLBS   = .TRUE.
           GRIDS  = .FALSE.
           SYMBOL = .TRUE.
           DP     = .TRUE.
           ICD(1) = 1
           ICD(2) = 2
           ICD(3) = 1
           ICD(4) = 2
           ITS    = 1
           CALL ICDS0(ICDS)
         END IF
         I = I + 2
 109   CONTINUE
C---                                    *D3
       I = 1
       DO 110 J=1,6
         PART2    = ISTR(I:I+1)
         IF (PART2.EQ.'D3'.OR.PART2.EQ.'d3') THEN
           DFT    = .TRUE.
           USR    = .FALSE.
           BRNCH  = .FALSE.
           POINT  = .TRUE.
           TIT    = .FALSE.
           TOP    = .FALSE.
           BOTTOM = .FALSE.
           AXLB   = .FALSE.
           QLBS   = .TRUE.
           GRIDS  = .TRUE.
           SYMBOL = .TRUE.
           DP     = .TRUE.
           ICD(1) = 1
           ICD(2) = 2
           ICD(3) = 1
           ICD(4) = 2
           ITS    = 1
           CALL ICDS0(ICDS)
         END IF
         I = I + 2
 110   CONTINUE
C---                                    *D4
       I = 1
       DO 111 J=1,6
         PART2    = ISTR(I:I+1)
         IF (PART2.EQ.'D4'.OR.PART2.EQ.'d4') THEN
           DFT    = .TRUE.
           USR    = .FALSE.
           BRNCH  = .FALSE.
           POINT  = .FALSE.
           TIT    = .FALSE.
           TOP    = .FALSE.
           BOTTOM = .FALSE.
           AXLB   = .FALSE.
           QLBS   = .TRUE.
           GRIDS  = .TRUE.
           SYMBOL = .TRUE.
           DP     = .TRUE.
           ICD(1) = 1
           ICD(2) = 2
           ICD(3) = 1
           ICD(4) = 2
           ITS    = 1
           CALL ICDS0(ICDS)
         END IF
         I = I + 2
 111   CONTINUE
C---                                    *NU (NORMAL USE)
       I = 1
       DO 112 J=1,6
         PART2    = ISTR(I:I+1)
         IF (PART2.EQ.'NU'.OR.PART2.EQ.'nu') THEN
           DFT    = .FALSE.
           USR    = .FALSE.
           DP     = .FALSE.
           QLBS   = .FALSE.
           SYMBOL = .FALSE.
           CALL DFSCR(MINSX,MAXSX,MINSY,MAXSY)
           ICD(1) = 1
           ICD(2) = 2
           ICD(3) = 3
           ICD(4) = 4
           ITS    = 1
           IBD2   = .FALSE.
         END IF
         I = I + 2
 112   CONTINUE
C---                                    *"EXPERT"
         I = 1
         DO 113 J=1,6
           PART2 = ISTR(I:I+1)
           IF (PART2.EQ.'XP'.OR.PART2.EQ.'xp') THEN
             USR  = .TRUE.
             DFT  = .FALSE.
             QLBS = .FALSE.
             ITS  = 1
             IBD2 = .FALSE.
           END IF
           I = I + 2
 113     CONTINUE
C---                                    *EXECUTE BD COMMANDS
         I = 1
         DO 114 J=1,6
           PART2 = ISTR(I:I+1)
           IF (PART2.EQ.'BD'.OR.PART2.EQ.'bd') THEN
             IP2=I+2
             CALL BDS(ISTR,IP2,SAVE,ICDS,ICD,SYMBOL)
             CALL PLCHDW(3)
             CHECKB = .TRUE.
             ITS    = 0
             GO TO 10
           END IF
           I = I + 2
 114     CONTINUE
C---                                    *EXECUTE B3D
         IF (ISTR.EQ.'B3D'.OR.ISTR.EQ.'b3d') THEN
           QSCOM = .TRUE.
           CALL EXB3D
           QSCOM = .FALSE.
           IBD2  = .FALSE.
           GO TO 10
         END IF
C---                                    *EXECUTE HELP <3D>
        IF (ISTR.EQ.'HELP3D'.OR.ISTR.EQ.'help3d'.OR.
     +      ISTR.EQ.'H3D   '.OR.ISTR.EQ.'h3d   ') THEN
          CALL LIST3D
          OSTR = ' <RETURN> FOR MORE DETAIL ON <3D> COMMANDS'
          WRITE(IWRITE,1) OSTR
          READ(5,20,END=10) NOTC
 20       FORMAT(A10)
          IF (NOTC.NE.' ') GO TO 10
          CALL HELP3D(NOTC,0)
          GO TO 10
        END IF
C---                                    *EXECUTE <2D>
        I = 1
        PART2 = ISTR(I:I+1)
        IF (PART2.EQ.'3D'.OR.PART2.EQ.'3d') THEN
          CALL GERR(ERRQ,IERR8,ERROR8,INTGLB,3)
          IF (ERROR8.NE.0) GO TO 10
          QSCOM = .TRUE.
          CH    = ISTR(3:3)
          IF (CH.EQ.' ') I = I+1
          CALL C2D3D(ISTR,I+2,XAXS,YAXS,PARRY,NUM,CHECKQ)
          QSCOM = .FALSE.
          IBD2  = .FALSE.
          GO TO 10
        END IF
C---
        IF (PART2.EQ.'2D'.OR.PART2.EQ.'2d') THEN
          CALL GERR(ERRQ,IERR8,ERROR8,INTGLB,2)
          IF (ERROR8.NE.0) GO TO 10
          CH  = ISTR(3:3)
          IF (CH.EQ.' ') I = I+1
          CALL C2D3D(ISTR,I+2,XAXS,YAXS,PARRY,NUM,CHECKQ)
          ITS  = 1
          IBD2 = .FALSE.
          TIT  = .FALSE.
          AXLB = .FALSE.
          GO TO 10
        END IF
C---                                    *DASHED LINES
       IF (ISTR.EQ.'SDA'.OR.ISTR.EQ.'sda') THEN
         CALL DEFINE(RIDASH,1)
         GO TO 10
       END IF
C---                                    *DOTTED LINES
       IF (ISTR.EQ.'SDO'.OR.ISTR.EQ.'sdo') THEN
         CALL DEFINE(RIDOT,2)
         GO TO 10
       END IF
C---                                    *CIRCLES
       IF (ISTR.EQ.'SCI'.OR.ISTR.EQ.'sci') THEN
         CALL DEFINE(RICIR,3)
         GO TO 10
       END IF
C---                                    *SYMBOLS
       IF (ISTR.EQ.'SSY'.OR.ISTR.EQ.'ssy') THEN
         CALL DEFINE(RISYBL,4)
         GO TO 10
       END IF
C---                                    *PLOTTING ACCURACY
       IF (ISTR.EQ.'PA'.OR.ISTR.EQ.'pa') THEN
         CALL DEFINE(RIDIS,5)
         GO TO 10
       END IF
C---                                    *DASH-DOT
       IF (ISTR.EQ.'SDD'.OR.ISTR.EQ.'sdd') THEN
         CALL DEFINE(RIDSHS,6)
         GO TO 10
       END IF
C---                                    *LONG-SHORT DASH
       IF (ISTR.EQ.'SLS'.OR.ISTR.EQ.'sls') THEN
         CALL DEFINE(RIDS1,6)
         GO TO 10
       END IF
C---                                    *DASHES
       IF (ISTR.EQ.'LDA'.OR.ISTR.EQ.'lda') THEN
         OSTR = ' DASH LENGTH IS '
         WRITE(ITERM,2) OSTR(1:15),UDSH
         OSTR = ' ENTER NEW DASH LENGTH '
         WRITE(ITERM,1) OSTR
         READ(5,*,ERR=10) UNIT
         IF (UNIT.LT.1.AND.UNIT.GT.0) THEN
           UDSH = UNIT
         ELSE
           OSTR = ' ERROR, DASH LENGTH MUST SATISFY : 0 < LENGTH < 1'
           WRITE(ITERM,1) OSTR
         END IF
         GO TO 10
       END IF
C---                                    *DASHES
       IF (ISTR.EQ.'US'.OR.ISTR.EQ.'us') THEN
         OSTR = ' DASH LENGTH IS '
         WRITE(ITERM,2) OSTR(1:15),UDSHS
         OSTR = ' ENTER NEW LENGTH'
         WRITE(ITERM,1) OSTR
         READ(5,*,ERR=10) UNIT
         IF (UNIT.LT.1.AND.UNIT.GT.0) THEN
           UDSHS = UNIT
         ELSE
           OSTR = ' ERROR, DASH LENGTH MUST SATISFY :  0 < LENGTH < 1'
           WRITE(ITERM,1) OSTR
         END IF
         GO TO 10
       END IF
C---                                    *LONG-SHORT DASH
       IF (ISTR.EQ.'LLS'.OR.ISTR.EQ.'lss') THEN
         OSTR = ' LONG AND SHORT DASH LENGTHS ARE : '
         WRITE(ITERM,2) OSTR(1:33),ULD,USD
         OSTR = ' ENTER NEW LENGTHS'
         WRITE(ITERM,1) OSTR
         OSTR = ' '
         READ(5,*,ERR=10) UNIT1,UNIT2
         IF (UNIT1.LE.0.OR.UNIT2.LE.0) THEN
           OSTR = '  ERROR - 0 < LONG, SHORT DASH < 1'
         ELSE IF (UNIT1.GE.1.OR.UNIT2.GE.1) THEN
           OSTR = '  ERROR - 0 < LONG, SHORT DASH < 1'
         ELSE IF ((UNIT1+UNIT2).GE.1) THEN
           OSTR = '  ERROR - 0 < LONG, SHORT DASH < 1'
         ELSE
           ULD = UNIT1
           USD = UNIT2
         END IF
         IF (OSTR.NE.' ') WRITE(6,1) OSTR
         GO TO 10
       END IF
C---                                    *RADIUS OF SYMBOL IN <SS>
       IF (ISTR.EQ.'RSS'.OR.ISTR.EQ.'rss') THEN
         CALL DFRADI(RASY,1)
         GO TO 10
       END IF
C---                                    *RADIUS OF SYMBOL IN <SD>
       IF (ISTR.EQ.'RCS'.OR.ISTR.EQ.'rcs') THEN
         CALL DFRADI(RATY,2)
         GO TO 10
       END IF
C---                                    *RESET
       IF (ISTR.EQ.'RES'.OR.ISTR.EQ.'res') THEN
         CALL DFITVL
         GO TO 10
       END IF
       IF(ITS.EQ.0) THEN
         OSTR = ' ILLEGAL COMMAND - REENTER'
       ELSE IF (ITS.EQ.1.AND.ISTR(3:3).EQ.' ') THEN
         OSTR = ' '
         ITS  = 0
       END IF
       WRITE(ITERM,1) OSTR
       GO TO 10
 1     FORMAT(A80)
 2     FORMAT(A,2F7.3)
 46    FORMAT(/,' THE DEFAULT SETTING FOR THE RELATIVE DIMENSION OF',
     + ' THE SCREEN IS :',//,' MIN(X) =',F8.2,4X,'MAX(X) =',F8.2,
     + 4X,'MIN(Y) =',F8.2,4X,'MAX(Y) =',F8.2,//,
     + ' ENTER THE NEW SCREEN DIMENSION SUCH THAT MIN(X) >',F8.2)
       END
C---
C-----------------------------------------------------------------------
C     LISTS THE POSSIBLE COMMANDS.
C-----------------------------------------------------------------------
       SUBROUTINE HELPS(ISTR)
       CHARACTER*80 ISTR
       COMMON /IO/ IWRITE,ITERM,ISAVE
C---
       CALL PLCMDS(1)
       WRITE(ITERM,1)
 1     FORMAT(
     +       /,'         Principal PLAUT Commands :'
     +    ,///,'  <BD0>   Bifurcation diagram with default limits'
     +     ,//,'  <BD>    Bifurcation diagram with user-limits'
     +     ,//,'  <AX>    To select bifurcation diagram axes'
     +     ,//,'  <2D>    2D plot of labeled solutions'
     +     ,//,'  <SAV>   To save the current plot in a file'
     +     ,//,'  <CL>    To clear the graphics window'
     +     ,//,'  <LAB>   List all labeled solutions in Unit 8'
     +     ,//,'  <END>   To End PLAUT'
     +    ,///,'  Press RETURN for more or Enter Command ...'
     +      )
C
       CALL READS8(ISTR)
       IF(ISTR.NE.' ') RETURN
C---
       CALL PLCMDS(1)
       WRITE(ITERM,2)
 2     FORMAT(
     +       /,'        PLAUT Default Options :'
     +    ,///,'  <D0>    Use solid curves, symbols, but no labels'
     +     ,//,'  <D1>    As <D0>, showing stability and labels' 
     +     ,//,'  <D2>    As <D1>, without labels'
     +     ,//,'  <D3>    As <D1>, with grid lines'
     +     ,//,'  <D4>    AS <D2>, with grid lines'
     +    ,///,'        Individual Options :'
     +    ,///,'  <SY>    Use symbols for special points'
     +     ,//,'  <DP>    Differential Plot (show stability)'
     +     ,//,'  <ST>    Set up titles and axes labels'
     +     ,//,'  <NU>    Normal usage (Reset special options)'
     +    ,///,'  Press RETURN for more or Enter Command ...'
     +      )
C
       CALL READS8(ISTR)
       IF(ISTR.NE.' ') RETURN
C---
       CALL PLCMDS(1)
       WRITE(ITERM,3)
 3     FORMAT(
     +       /,'        Additional PLAUT Commands :'
     +    ,///,'  <SCR>   To change the plot size'
     +      ,/,'  <SS>    To define symbols'
     +      ,/,'  <RSS>   Set symbol size'
     +      ,/,'  <XP>    "Expert" (Abbreviated prompts)'
     +      ,/,'  <SD>    To change curve type'
     +     ,//,'  <SDA>   Set dash spacing'
     +      ,/,'  <LDA>   Set dash size'
     +      ,/,'  <SDO>   Set dot spacing'
     +      ,/,'  <SCI>   Set circle spacing'
     +      ,/,'  <SSY>   Set curve-symbol spacing'
     +      )
       WRITE(ITERM,4)
 4     FORMAT( '  <RCS>   Set curve-symbol size'
     +      ,/,'  <SDD>   Set dash-dot spacing'
     +      ,/,'  <SLS>   Set long-short dash spacing'
     +      ,/,'  <LLS>   Set long-short dash size'
     +      ,/,'  <PA>    Set plotting accuracy'
     +      ,/,'  <RES>   Reset curves and symbols'
     +    ,///,'  <3D>    3D plot of labeled solutions'
     +      ,/,'  <B3D>   3D bifurcation diagram'
     +      ,/,'  <H3D>   For list of <3D> and <B3D> commands'
     +    ,///,'         --- End of Help ---',//
     +      )
C---
       RETURN
       END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
