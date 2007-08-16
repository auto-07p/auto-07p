!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM AUTLAB
  USE UTILITY

  PARAMETER (MXLB=10000)
  CHARACTER*1 CMD
  DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
  DIMENSION LFR(MXLB),LTO(MXLB)
  LOGICAL CHCKLB

  OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
  OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')

  CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
  CALL SYSTEM('clear')
  DO
     CALL RDCMD(MXLB,CMD,NL,LFR,LTO)
     SELECT CASE(CMD)
     CASE('H')
        CALL SYSTEM('clear')
        CALL HELP
     CASE('L')
        CALL LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO,.TRUE.)
     CASE('D')
        CALL DELETE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
     CASE('R')
        CALL RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)
     CASE('W')
        IF(CHCKLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL))THEN
           WRITE(6,"(/,' Rewriting files ... ')")
           OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
           OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
           CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
           CALL WRFILE8(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
           STOP
        ENDIF
     CASE('Q')
        WRITE(6,"(' Relabeling discontinued. Recover original files')")
        STOP
     END SELECT
  ENDDO
END PROGRAM AUTLAB

!--------- -----
SUBROUTINE RDCMD(MXLB,CMD,NL,LFR,LTO)

  COMMON /CBRDCMD/ IFIRST
  CHARACTER*1 CMD,GETCHR,CHR
  CHARACTER*80 LINE
  DIMENSION LFR(MXLB),LTO(MXLB)
  INTEGER GETNUM
  LOGICAL ISDIGIT

1 IF(IFIRST.NE.1)THEN
     WRITE(6,101)
     IFIRST=1
  ELSE
     WRITE(6,102)
  ENDIF

  DO I=1,80
     LINE(I:I)=' '
  ENDDO
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
3    CHR=GETCHR(LINE,IP)
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

100  FORMAT(A80)
101  FORMAT(/,' Enter Command ( h for Help) : ',$)
102  FORMAT(/,' Enter Command : ',$)
103  FORMAT(/,' Invalid Command ',$)

  RETURN
END SUBROUTINE RDCMD

!---------- -------- ------
CHARACTER*1 FUNCTION GETCHR(LINE,IP)
  !
  ! Gets next character from LINE that is not a ' ' or ','
  !
  CHARACTER*80 LINE
  CHARACTER*1 CHR
  !
  GETCHR=' '
1 CHR=LINE(IP:IP)
  IF(CHR.NE.' '.AND.CHR.NE.',')THEN
     GETCHR=CHR
     IP=IP+1
     RETURN
  ELSE
     IP=IP+1
     IF(IP.LE.80)GOTO 1
  ENDIF
  !
  RETURN
END FUNCTION GETCHR

!------ -------- ------
INTEGER FUNCTION GETNUM(LINE,IP)
  !
  CHARACTER*80 LINE
  CHARACTER*1 CHR, GETCHR
  LOGICAL ISDIGIT
  !
  GETNUM=0
  CHR=GETCHR(LINE,IP)
1 IF(ISDIGIT(CHR,N))THEN
     GETNUM=10*GETNUM+N
     CHR=LINE(IP:IP)
     IP=IP+1
     GOTO 1
  ELSE
     IP=IP-1
     RETURN
  ENDIF
  !
END FUNCTION GETNUM

!------ -------- -------
LOGICAL FUNCTION ISDIGIT(CHR,N)
  !
  CHARACTER*1 CHR
  !
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
  !
  RETURN
END FUNCTION ISDIGIT

!--------- ----
SUBROUTINE HELP

  WRITE(6,"(/A//A/A/A/A/A/A)") &
       ' Available commands : ', &
       '   l  :  list labels', &
       '   d  :  delete labels', &
       '   r  :  relabel', &
       '   w  :  rewrite files', &
       '   q  :  quit ', &
       '   h  :  help '
  WRITE(6,"(//A/A//A/A/A)") &
       ' The l, d, and r commands can be followed on the ', &
       ' same line by a list of labels, for example, ', &
       ' l 13        (list label 13)', &
       ' d 7 13      (delete labels 7 and 13)', &
       ' r 1 13 6-9  (relabel 1, 13, and 6 to 9)'
  WRITE(6,"(//A//A/A/A/)") &
       ' If a list is not specified then the actions are', &
       ' l           (list all labels)', &
       ' d           (delete/confirm all labels)', &
       ' r           (automatic relabeling)'

END SUBROUTINE HELP

!--------- ------
SUBROUTINE DELETE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)

  USE UTILITY
  DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
  DIMENSION LFR(MXLB),LTO(MXLB)
  CHARACTER*1 CH1

  IF(NL.EQ.0)THEN
     DO I=1,NLB
        IF(LNL(I).GT.0)THEN
           WRITE(6,101)LLB(I)
           READ(5,102)CH1
           IF(CH1.EQ.'y'.OR.CH1.EQ.'Y')THEN
              LNL(I)=0
           ENDIF
        ENDIF
     ENDDO
  ELSE
     DO I=1,NLB
        IF(INLIST(MXLB,LLB(I),NL,LFR,LTO))THEN
           LNL(I)=0
        ENDIF
     ENDDO
  ENDIF

101 FORMAT(' Delete (old) label ',I4,' ? : ',$)
102 FORMAT(A1)

  RETURN
END SUBROUTINE DELETE

!------ -------- ------
LOGICAL FUNCTION CHCKLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)

  DIMENSION LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)

  CHCKLB=.TRUE.
  DO I=1,NLB
     IF(LNL(I).GT.0)THEN
        DO J=1,NLB
           IF(J.NE.I .AND. LNL(J).GT.0)THEN
              IF(LNL(J).EQ.LNL(I))THEN
                 WRITE(6,101)
                 CHCKLB=.FALSE.
                 RETURN
              ENDIF
           ENDIF
        ENDDO
     ENDIF
  ENDDO

101 FORMAT(' Duplicate labels: File not written')

  RETURN
END FUNCTION CHCKLB
!======================================================================
!======================================================================
