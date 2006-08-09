C======================================================================
C======================================================================
C Utility Program for Listing, Deleting, Relabeling Labeled Solutions
C                        in AUTO97 Data Files
C======================================================================
C======================================================================
C
      PARAMETER (MXLB=10000)
      CHARACTER*1 CMD
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      LOGICAL CHCKLB
C
       OPEN(27,FILE='fort.27',STATUS='old')
       OPEN(28,FILE='fort.28',STATUS='old')
       OPEN(37,FILE='fort.37',STATUS='unknown')
       OPEN(38,FILE='fort.38',STATUS='unknown')
C
       CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
       CALL REDUCE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO)
       CALL RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,0,LFR,LTO)
       CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
       CALL WRFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      STOP
      END
C
C     ---------- ------
      SUBROUTINE REDUCE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
C
       IF(NLB.NE.0)THEN
         DO I=1,NLB
          IF(MOD(I,2).EQ.0 .AND. LTY(I).EQ.4)THEN
              LNL(I)=0
            ENDIF
         ENDDO
       ENDIF
C
      RETURN
      END
C
C======================================================================
C======================================================================
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
C     ------- -------- ------
      LOGICAL FUNCTION INLIST(MXLB,LAB,NL,LFR,LTO)
C
      DIMENSION LFR(MXLB),LTO(MXLB)      
C
       INLIST=.FALSE.
       DO 1 I=1,NL
         IF(LAB.GE.LFR(I).AND.LAB.LE.LTO(I))THEN
           INLIST=.TRUE.
           RETURN
         ENDIF
 1     CONTINUE
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      LOGICAL EOF
C
       REWIND 28
       NLB=0
 1     CONTINUE
         READ(28,*,END=2)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NSKIP
         IF(NLB.GE.MXLB)THEN
           WRITE(6,101)MXLB
           STOP
         ENDIF
         NLB=NLB+1
         LBR(NLB)=IABS(IBR)
         LPT(NLB)=IABS(NTOT)
	 IF(ITP.LT.0)THEN
           LTY(NLB)=-MOD(IABS(ITP),10)
	 ELSE
           LTY(NLB)= MOD(ITP,10)
	 ENDIF
         LLB(NLB)=LAB
         LNL(NLB)=LAB
         CALL SKIP(28,NSKIP,EOF)
         IF(EOF)GOTO 2
       GOTO 1
C
 101  FORMAT(' ERROR : Maximum number of labels (',I6,') exceeded.',
     *     /,' Increase MXLB in auto/97/src/autlab.f and recompile.')
C
 2    RETURN
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
      SUBROUTINE LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      CHARACTER*2 TYPE
      LOGICAL FIRST,INLIST
C
       IF(NLB.EQ.0)THEN
          WRITE(6,101)
       ELSE
         FIRST=.TRUE.
         DO 1 I=1,NLB
          IF(NL.EQ.0 .OR. INLIST(MXLB,LLB(I),NL,LFR,LTO))THEN
            IF(FIRST)THEN
              WRITE(6,102)
              FIRST=.FALSE.
            ENDIF
            IF(LLB(I).EQ.LNL(I))THEN
              WRITE(6,103)LBR(I),LPT(I),TYPE(LTY(I)),LLB(I),LNL(I)
            ELSEIF(LNL(I).EQ.0)THEN
              WRITE(6,104)LBR(I),LPT(I),TYPE(LTY(I)),LLB(I)
            ELSE
              WRITE(6,103)LBR(I),LPT(I),TYPE(LTY(I)),LLB(I),LNL(I)
            ENDIF
          ENDIF
 1       CONTINUE
       ENDIF
C
 101   FORMAT(/,' Empty solutions file')
 102   FORMAT(/,'  BR    PT  TY LAB  NEW')
 103   FORMAT(I4,I6,2X,A2,I4,I5)
 104   FORMAT(I4,I6,2X,A2,I4,'        DELETED')
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE DELETE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      CHARACTER*1 CH1
      LOGICAL INLIST
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
C     ---------- -------
      SUBROUTINE RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DIMENSION LFR(MXLB),LTO(MXLB)
      LOGICAL INLIST
C
       IF(NL.EQ.0)THEN
         NN=0
         DO 1 I=1,NLB
          IF(LNL(I).GT.0)THEN
            NN=NN+1
            LNL(I)=NN
          ENDIF
 1       CONTINUE
       ELSE
         DO 2 I=1,NLB
          IF(NL.EQ.0 .OR.
     *       (LNL(I).GT.0 .AND. INLIST(MXLB,LLB(I),NL,LFR,LTO)) )THEN
            WRITE(6,101)LLB(I)
            READ(5,*)LNL(I)
          ENDIF
 2       CONTINUE
       ENDIF
C
 101   FORMAT(' Old label ',I4,';  Enter new label : ',$)
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
C
C     ---------- -------
      SUBROUTINE WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      CHARACTER*132 LINE
      CHARACTER*4 CHR4,CLAB
      CHARACTER*1 CH1
C
       L=0
       LNUM=0
       REWIND 27
 1     CONTINUE
         LINE(1:132)=' '
         READ(27,100,END=99)LINE
         LNUM=LNUM+1
         CLAB=LINE(16:19)
         IF(LINE(1:4).NE.'   0' .AND. CLAB.NE.'   0')THEN
           L=L+1
           IF(CLAB.NE.CHR4(LLB(L)))THEN
             WRITE(6,101)CLAB,LNUM,CHR4(LLB(L))
             READ(5,102)CH1
             IF(CH1.NE.'y'.AND.CH1.NE.'Y')THEN
               WRITE(6,103)
               RETURN
             ENDIF
           ENDIF
           LINE(16:19)=CHR4(LNL(L))
         ENDIF
         LEN=LENGTH(132,LINE)
         WRITE(37,100)LINE(1:LEN)
         GOTO 1
C
 100   FORMAT(A)
 101   FORMAT(' WARNING : The two files have incompatible labels :',
     *      /,'  p-file label ',A4,' at line ',I5,
     *      /,'  q-file label ',A4,
     *      /,' New labels may be assigned incorrectly.',
     *      /,' Continue ? : ',$)
 102   FORMAT(A1)
     *
 103   FORMAT(' Rewrite discontinued. Recover original files')
C
 99   RETURN
      END
C
C     ------- -------- ------
      INTEGER FUNCTION LENGTH(N,LINE)
C
      CHARACTER*132 LINE
C
       LENGTH=1
       DO 1 I=N,1,-1
         IF(LINE(I:I).NE.' ')THEN
           LENGTH=I
           RETURN
         ENDIF
 1     CONTINUE 
C
      RETURN
      END
C
C     ----------- -------- ----
      CHARACTER*4 FUNCTION CHR4(N)
C
      CHARACTER*1 INT2CHR
C
      M=N
      CHR4(1:4)='    '
      DO 1 I=4,1,-1
       CHR4(I:I)=INT2CHR(MOD(M,10))
       M=M/10
       IF(M.EQ.0)RETURN
 1    CONTINUE     
C
      RETURN
      END
C
C     ----------- -------- -------
      CHARACTER*1 FUNCTION INT2CHR(N)
C
       IF(N.EQ.0)THEN
         INT2CHR='0'      
       ELSEIF(N.EQ.1)THEN
         INT2CHR='1'      
       ELSEIF(N.EQ.2)THEN
         INT2CHR='2'      
       ELSEIF(N.EQ.3)THEN
         INT2CHR='3'      
       ELSEIF(N.EQ.4)THEN
         INT2CHR='4'      
       ELSEIF(N.EQ.5)THEN
         INT2CHR='5'      
       ELSEIF(N.EQ.6)THEN
         INT2CHR='6'      
       ELSEIF(N.EQ.7)THEN
         INT2CHR='7'      
       ELSEIF(N.EQ.8)THEN
         INT2CHR='8'      
       ELSEIF(N.EQ.9)THEN
         INT2CHR='9' 
       ENDIF     
C
      RETURN
      END
C
C     ---------- -------
      SUBROUTINE WRFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
C
      PARAMETER (NDIMX=2000,NPARX=100)
      DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
      DOUBLE PRECISION U(NDIMX+1),RLDOT(NPARX),PAR(NPARX)
      INTEGER ICP(NPARX)
C
       L=0
       REWIND 28
 1     CONTINUE
         READ(28,*,END=99)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,
     *               NAR,NROWPR,NTST,NCOL,NPAR1
         IF(NAR.GT.NDIMX+1)THEN
           WRITE(6,104)LAB
           STOP
         ENDIF
         IF(NPAR1.GT.NPARX)THEN
           WRITE(6,103)
           NPAR2=NPARX
         ELSE
           NPAR2=NPAR1
         ENDIF
         L=L+1
         IF(LNL(L).GT.0)
     *   WRITE(38,101)IBR,NTOT,ITP,LNL(L),NFPR,ISW,NTPL,
     *             NAR,NROWPR,NTST,NCOL,NPAR2
         DO 2 J=1,NTPL
           READ(28,*)  (U(I),I=1,NAR) 
           IF(LNL(L).GT.0)
     *     WRITE(38,102)(U(I),I=1,NAR)
 2       CONTINUE
         IF(NTST.NE.0)THEN
           READ(28,*)  (ICP(I),I=1,NFPR)
           IF(LNL(L).GT.0)
     *     WRITE(38,100)(ICP(I),I=1,NFPR)
           READ(28,*)  (RLDOT(I),I=1,NFPR)
           IF(LNL(L).GT.0)
     *     WRITE(38,102)(RLDOT(I),I=1,NFPR)
           DO 3 J=1,NTPL
             READ(28,*)  (U(I),I=1,NAR-1) 
             IF(LNL(L).GT.0)
     *       WRITE(38,102)(U(I),I=1,NAR-1)
 3         CONTINUE
         ENDIF
         READ(28,*) (PAR(I),I=1,NPAR2)
         IF(NPAR1.NE.NPAR2)THEN
           N1= 1+ (NPAR1-1)/7
           N2= 1+ (NPAR2-1)/7
           IF(N1.GT.N2)THEN
             DO I=1,N1-N2
               READ(28,*)
             ENDDO
           ENDIF
         ENDIF
         IF(LNL(L).GT.0)
     *   WRITE(38,102)(PAR(I),I=1,NPAR2)
       GOTO 1
C
 100   FORMAT(20I5)
 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,1P7E19.10)
 103   FORMAT(' Warning : NPARX too small;',
     *        ' parameter values may be lost')
 104   FORMAT(/,' ERROR : NDIMX too small in auto/97/autlab.f .',/,
     *        ' Increase NDIMX in subroutine WRFILE8 and recompile.',/, 
     *        ' Error occured when writing Solution ',I5,'.',/,
     *        ' Execution terminated. Recover original files.',/)
C
 99   RETURN
      END
C
C     ---------- ----
      SUBROUTINE SKIP(IUNIT,NSKIP,EOF)
C
C Skips the specified number of lines on fort.IUNIT.
C
      LOGICAL EOF
C
       EOF=.FALSE.
       DO 1 I=1,NSKIP
         READ(IUNIT,*,END=2)
 1     CONTINUE
       RETURN
C
 2     CONTINUE
        EOF=.TRUE.
      RETURN
      END
C
C     ----------- -------- ----
      CHARACTER*2 FUNCTION TYPE(ITP)
C
       IF(MOD(ITP,10).EQ.1)THEN
         TYPE='BP'
       ELSE IF(MOD(ITP,10).EQ.2)THEN
         TYPE='LP'
       ELSE IF(MOD(ITP,10).EQ.3)THEN
         TYPE='HB'
       ELSE IF(MOD(ITP,10).EQ.4)THEN
         TYPE='  '
       ELSE IF(MOD(ITP,10).EQ.-4)THEN
         TYPE='UZ'
       ELSE IF(MOD(ITP,10).EQ.5)THEN
         TYPE='LP'
       ELSE IF(MOD(ITP,10).EQ.6)THEN
         TYPE='BP'
       ELSE IF(MOD(ITP,10).EQ.7)THEN
         TYPE='PD'
       ELSE IF(MOD(ITP,10).EQ.8)THEN
         TYPE='TR'
       ELSE IF(MOD(ITP,10).EQ.9)THEN
         TYPE='EP'
       ELSE IF(MOD(ITP,10).EQ.-9)THEN
         TYPE='MX'
       ELSE
         TYPE='  '
       ENDIF
C
      RETURN
      END
C======================================================================
C======================================================================
