C======================================================================
C======================================================================
C Utility Program for Listing, Deleting, Relabeling Labeled Solutions
C                        in AUTO97 Data Files
C======================================================================
C======================================================================
C
      PROGRAM AUTLAB
      USE UTILITY

      PARAMETER (MXLB=10000)
      CHARACTER*1 CMD
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      LOGICAL CHCKLB
C
       OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
       OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
       OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
       OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
C
       CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
       CALL SYSTEM('clear')
1      CALL RDCMD(MXLB,CMD,NL,LFR,LTO)
       IF(CMD.EQ.'H')THEN
          CALL SYSTEM('clear')
          CALL HELP
          GOTO 1
       ELSEIF(CMD.EQ.'L')THEN
          CALL LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO,.TRUE.)
          GOTO 1
       ELSEIF(CMD.EQ.'D')THEN
          CALL DELETE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
          GOTO 1
       ELSEIF(CMD.EQ.'R')THEN
          CALL RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
          GOTO 1
       ELSEIF(CMD.EQ.'W')THEN
          IF(CHCKLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL))THEN
            WRITE(6,101)
            CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
            CALL WRFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
            STOP
          ELSE
            GOTO 1
          ENDIF
       ELSEIF(CMD.EQ.'Q')THEN
          WRITE(6,102)
          STOP
       ENDIF
C
 101   FORMAT(/,' Rewriting files ... ')
 102   FORMAT(' Relabeling discontinued. Recover original files')
C
      STOP
      END
C
C     ---------- -----
      SUBROUTINE RDCMD(MXLB,CMD,NL,LFR,LTO)
C
      COMMON /CBRDCMD/ IFIRST
      CHARACTER*1 CMD,GETCHR,CHR
      CHARACTER*80 LINE
      DIMENSION LFR(MXLB),LTO(MXLB)
      INTEGER GETNUM
      LOGICAL ISDIGIT
C
 1     IF(IFIRST.NE.1)THEN
         WRITE(6,101)
         IFIRST=1
       ELSE
         WRITE(6,102)
       ENDIF
C
       DO 2 I=1,80
         LINE(I:I)=' '
 2     CONTINUE
       READ(5,100)LINE
       IP=1
       CMD=GETCHR(LINE,IP)
       IF(CMD.EQ.'h')THEN
         CMD='H'
       ELSEIF(CMD.EQ.'l')THEN
         CMD='L'
       ELSEIF(CMD.EQ.'d')THEN
         CMD='D'
       ELSEIF(CMD.EQ.'r')THEN
         CMD='R'
       ELSEIF(CMD.EQ.'w')THEN
         CMD='W'
       ELSEIF(CMD.EQ.'q')THEN
         CMD='Q'
       ELSE 
         WRITE(6,103)
         IFIRST=0
         GOTO 1
       ENDIF
       IF(CMD.EQ.'L'.OR.CMD.EQ.'D'.OR.CMD.EQ.'R')THEN
         NL=0
 3       CHR=GETCHR(LINE,IP)
         IF(ISDIGIT(CHR,N))THEN
           IP=IP-1
           NL=NL+1
           LFR(NL)=GETNUM(LINE,IP)
           CHR=GETCHR(LINE,IP)
           IF(CHR.EQ.'-')THEN
             LTO(NL)=GETNUM(LINE,IP)
           ELSE
             IP=IP-1
             LTO(NL)=LFR(NL)
           ENDIF
           GOTO 3
         ENDIF
       ENDIF
C
 100   FORMAT(A80)
 101   FORMAT(/,' Enter Command ( h for Help) : ',$)
 102   FORMAT(/,' Enter Command : ',$)
 103   FORMAT(/,' Invalid Command ',$)
C
      RETURN
      END
C
C     ----------- -------- ------
      CHARACTER*1 FUNCTION GETCHR(LINE,IP)
C
C Gets next character from LINE that is not a ' ' or ','
C
      CHARACTER*80 LINE
      CHARACTER*1 CHR
C
      GETCHR=' '
 1    CHR=LINE(IP:IP)
      IF(CHR.NE.' '.AND.CHR.NE.',')THEN
        GETCHR=CHR
        IP=IP+1
        RETURN
       ELSE
         IP=IP+1
         IF(IP.LE.80)GOTO 1
       ENDIF
C
      RETURN
      END
C
C     ------- -------- ------
      INTEGER FUNCTION GETNUM(LINE,IP)
C
      CHARACTER*80 LINE
      CHARACTER*1 CHR, GETCHR
      LOGICAL ISDIGIT
C
        GETNUM=0
        CHR=GETCHR(LINE,IP)
 1      IF(ISDIGIT(CHR,N))THEN
          GETNUM=10*GETNUM+N
          CHR=LINE(IP:IP)
          IP=IP+1
          GOTO 1
        ELSE
          IP=IP-1
          RETURN
        ENDIF
C
      END
C
C     ------- -------- -------
      LOGICAL FUNCTION ISDIGIT(CHR,N)
C
      CHARACTER*1 CHR
C
       ISDIGIT=.TRUE.
       IF(CHR.EQ.'0')THEN
         N=0
       ELSEIF(CHR.EQ.'1')THEN
         N=1
       ELSEIF(CHR.EQ.'2')THEN
         N=2
       ELSEIF(CHR.EQ.'3')THEN
         N=3
       ELSEIF(CHR.EQ.'4')THEN
         N=4
       ELSEIF(CHR.EQ.'5')THEN
         N=5
       ELSEIF(CHR.EQ.'6')THEN
         N=6
       ELSEIF(CHR.EQ.'7')THEN
         N=7
       ELSEIF(CHR.EQ.'8')THEN
         N=8
       ELSEIF(CHR.EQ.'9')THEN
         N=9
       ELSE
         ISDIGIT=.FALSE.
         N=0
       ENDIF
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE HELP
C
       WRITE(6,101)
       WRITE(6,102)
       WRITE(6,103)
C
 101   FORMAT(/,' Available commands : ',//,
     *        '   l  :  list labels',/,
     *        '   d  :  delete labels',/,
     *        '   r  :  relabel',/,
     *        '   w  :  rewrite files',/,
     *        '   q  :  quit ',/,
     *        '   h  :  help ')
 102  FORMAT(//,
     * ' The l, d, and r commands can be followed on the ',/,
     * ' same line by a list of labels, for example, ',//,
     * ' l 13        (list label 13)',/, 
     * ' d 7 13      (delete labels 7 and 13)',/, 
     * ' r 1 13 6-9  (relabel 1, 13, and 6 to 9)') 
 103  FORMAT(//,
     * ' If a list is not specified then the actions are ',//,
     * ' l           (list all labels)',/, 
     * ' d           (delete/confirm all labels)',/, 
     * ' r           (automatic relabeling)',/) 
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE DELETE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      USE UTILITY
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      CHARACTER*1 CH1
C
       IF(NL.EQ.0)THEN
         DO 1 I=1,NLB
          IF(LNL(I).GT.0)THEN
            WRITE(6,101)LLB(I)
            READ(5,102)CH1
            IF(CH1.EQ.'y'.OR.CH1.EQ.'Y')THEN
              LNL(I)=0
            ENDIF
          ENDIF
 1       CONTINUE
       ELSE
         DO 2 I=1,NLB
          IF(INLIST(MXLB,LLB(I),NL,LFR,LTO))THEN
            LNL(I)=0
          ENDIF
 2       CONTINUE
       ENDIF
C
 101   FORMAT(' Delete (old) label ',I4,' ? : ',$)
 102   FORMAT(A1)
C
      RETURN
      END
C
C     ------- -------- ------
      LOGICAL FUNCTION CHCKLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
C
       CHCKLB=.TRUE.
       DO 2 I=1,NLB
         IF(LNL(I).GT.0)THEN
           DO 1 J=1,NLB
             IF(J.NE.I .AND. LNL(J).GT.0)THEN
               IF(LNL(J).EQ.LNL(I))THEN
                 WRITE(6,101)
                 CHCKLB=.FALSE.
                 RETURN
               ENDIF
             ENDIF 
 1         CONTINUE
         ENDIF
 2     CONTINUE
C
 101   FORMAT(' Duplicate labels: File not written')
C
      RETURN
      END
C======================================================================
C======================================================================
