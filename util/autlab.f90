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

  CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,28)
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

  IMPLICIT NONE
  INTEGER MXLB,NL
  INTEGER LFR(MXLB),LTO(MXLB)
  CHARACTER*1 CMD

  INTEGER, SAVE :: IFIRST
  CHARACTER*1 GETCHR,CHR
  CHARACTER*80 LINE
  INTEGER GETNUM,IP,N,i

  DO
     IF(IFIRST.NE.1)THEN
        WRITE(6,'(/A)',ADVANCE="no")' Enter Command ( h for Help) : '
        IFIRST=1
     ELSE
        WRITE(6,'(/A)',ADVANCE="no")' Enter Command : '
     ENDIF

     LINE=' '
     READ(5,'(A80)')LINE
     IP=1
     CMD=GETCHR(LINE,IP)
     i=INDEX('hldrwq',CMD)
     IF(i>=1)THEN
        CMD='HLDRWQ'(i:i)
        EXIT
     ENDIF
     WRITE(6,'(/A)',ADVANCE="no")' Invalid Command '
     IFIRST=0
  ENDDO
  IF(CMD=='L'.OR.CMD=='D'.OR.CMD=='R')THEN
     NL=0
     DO
        CHR=GETCHR(LINE,IP)
        IF(INDEX('0123456789',CHR)==0)EXIT
        IP=IP-1
        NL=NL+1
        LFR(NL)=GETNUM(LINE,IP)
        CHR=GETCHR(LINE,IP)
        IF(CHR=='-')THEN
           LTO(NL)=GETNUM(LINE,IP)
        ELSE
           IP=IP-1
           LTO(NL)=LFR(NL)
        ENDIF
     ENDDO
  ENDIF

END SUBROUTINE RDCMD

!---------- -------- ------
CHARACTER*1 FUNCTION GETCHR(LINE,IP)
  !
  ! Gets next character from LINE that is not a ' ' or ','
  !
  CHARACTER*80 LINE
  CHARACTER*1 CHR

  I=VERIFY(LINE(IP:),' ,')
  IF(I>0)THEN
     IP=IP+I
     GETCHR=LINE(IP-1:IP-1)
  ELSE
     IP=81
     GETCHR=' '
  ENDIF

END FUNCTION GETCHR

!------ -------- ------
INTEGER FUNCTION GETNUM(LINE,IP)

  CHARACTER*80 LINE
  CHARACTER*1 CHR, GETCHR

  GETNUM=0
  I=SCAN(LINE(IP+1:),'-, ')
  IF(I>0)THEN
     READ(LINE(IP:IP+I-1),*)GETNUM
     IP=IP+I
  ELSE
     READ(LINE(IP:),*)GETNUM
     IP=81
  ENDIF

END FUNCTION GETNUM

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
           WRITE(6,"(A,I4,A)",ADVANCE="no") &
                ' Delete (old) label ',LLB(I),' ? : '
           READ(5,'(A1)')CH1
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
