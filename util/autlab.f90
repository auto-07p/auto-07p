!======================================================================
!======================================================================
! Utility Program for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM AUTLAB
  USE UTILITY

  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: LBR(:),LPT(:),LTY(:),LLB(:),LNL(:)
  INTEGER, ALLOCATABLE :: LFR(:),LTO(:),LBI(:)
  INTEGER, ALLOCATABLE :: BBR(:),BNB(:)
  TYPE(LABEL), POINTER :: LIST
  CHARACTER(2) CMD
  INTEGER NBR,NLB,NL,ILB

  OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
  OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')

  NULLIFY(LIST)
  CALL RDFILE8(LIST,NLB,28)
  ALLOCATE(LBR(NLB),LPT(NLB),LTY(NLB),LLB(NLB),LNL(NLB),&
       LFR(NLB),LTO(NLB),LBI(NLB))
  CALL GTFILE8(LIST,NLB,LBR,LPT,LTY,LLB,LNL)
  CALL RDFILE7(LIST,NBR,NLB,LBI,27)
  ALLOCATE(BBR(NBR),BNB(NBR))
  CALL GTFILE7(LIST,NBR,BBR,BNB)
  CALL SYSTEM('clear')
  DO
     CALL RDCMD(NLB,CMD,NL,LFR,LTO)
     SELECT CASE(TRIM(CMD))
     CASE('H')
        CALL SYSTEM('clear')
        CALL HELP
     CASE('L')
        CALL LISTLB(NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO,.TRUE.)
     CASE('LB')
        CALL LISTBR(NBR,BBR,BNB,NLB,LBI,LNL,NL,LFR,LTO)
     CASE('D')
        CALL DELETE('label',NLB,LLB,LNL,NL,LFR,LTO)
     CASE('DB')
        CALL DELETE('branch',NBR,BBR,BNB,NL,LFR,LTO)
        ! delete all labels on the branch
        DO ILB=1,NLB
           IF(LBI(ILB)/=0)THEN
              IF(BNB(LBI(ILB))==0)THEN
                 LNL(ILB)=0
              ENDIF
           ENDIF
        ENDDO
     CASE('R')
        CALL RELABEL('label',NLB,LLB,LNL,NL,LFR,LTO)
     CASE('RB')
        CALL RELABEL('branch',NBR,BBR,BNB,NL,LFR,LTO)
        DO ILB=1,NLB ! sync solutions
           IF(LBI(ILB)/=0)THEN
              LBR(ILB)=BNB(LBI(ILB))
           ENDIF
        ENDDO
     CASE('W')
        IF(CHCKLB(NLB,LNL))THEN
           WRITE(6,"(/,' Rewriting files ... ')")
           OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
           OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
           CALL WRFILE7(NBR,BNB,NLB,LLB,LNL)
           CALL WRFILE8(LBR,LNL)
           STOP
        ENDIF
     CASE('Q')
        WRITE(6,"(' Relabeling discontinued. Recover original files')")
        STOP
     END SELECT
  ENDDO

CONTAINS

!--------- -----
SUBROUTINE RDCMD(NLB,CMD,NL,LFR,LTO)

  INTEGER NLB,NL
  INTEGER LFR(NLB),LTO(NLB)
  CHARACTER(1) CMD1
  CHARACTER(2) CMD

  INTEGER, SAVE :: IFIRST
  CHARACTER(1) CHR
  CHARACTER(80) LINE
  INTEGER IP,i

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
     CMD1=GETCHR(LINE,IP)
     i=INDEX('hldrwq',CMD1)
     IF(i>=1)THEN
        CMD1='HLDRWQ'(i:i)
        EXIT
     ENDIF
     WRITE(6,'(/A)',ADVANCE="no")' Invalid Command '
     IFIRST=0
  ENDDO
  CMD=CMD1
  IF(CMD1=='L'.OR.CMD1=='D'.OR.CMD1=='R')THEN
     NL=0
     DO
        CHR=GETCHR(LINE,IP)
        IF(CHR=='b'.OR.CHR=='B')THEN
           CMD(2:2)='B'
           CYCLE
        ENDIF
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

!----------- -------- ------
CHARACTER(1) FUNCTION GETCHR(LINE,IP)
  !
  ! Gets next character from LINE that is not a ' ' or ','
  !
  CHARACTER(80), INTENT(IN) :: LINE
  INTEGER, INTENT(INOUT) :: IP

  INTEGER I

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

  CHARACTER(80), INTENT(IN) :: LINE
  INTEGER, INTENT(INOUT) :: IP

  INTEGER I

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

  WRITE(6,"(/A//A/A/A/A/A/A/A)") &
       ' Available commands : ', &
       '   l  :  list labels', &
       '   lb :  list branch numbers', &
       '   d  :  delete labels', &
       '   db :  delete branches', &
       '   r  :  relabel', &
       '   rb :  relabel branches', &
       '   w  :  rewrite files', &
       '   q  :  quit ', &
       '   h  :  help '
  WRITE(6,"(//A/A//A/A/A/A)") &
       ' The l, d, r, lb, db, and rb commands can be followed on the ', &
       ' same line by a list of labels, for example, ', &
       ' l 13           (list label 13)', &
       ' d 7 13         (delete labels 7 and 13)', &
       ' r 1 13 6-9     (relabel 1, 13, and 6 to 9)', &
       ' rb 1, 3-5, 7-9 (relabel branches 1, 3 to 5, and 7 to 9)'
  WRITE(6,"(//A//A/A/A/A/)") &
       ' If a list is not specified then the actions are', &
       ' l              (list all labels)', &
       ' d              (delete/confirm all labels)', &
       ' r              (automatic relabeling)', &
       ' rb             (automatic relabeling of branches)'

END SUBROUTINE HELP

!--------- ------
SUBROUTINE DELETE(S,NLB,LLB,LNL,NL,LFR,LTO)

  USE UTILITY
  CHARACTER(*) S
  INTEGER NLB,LLB(NLB),LNL(NLB),LFR(NLB),LTO(NLB),NL
  CHARACTER(1) CH1
  INTEGER I

  IF(NL.EQ.0)THEN
     DO I=1,NLB
        IF(LNL(I).GT.0)THEN
           WRITE(6,"(A,A,I5,A)",ADVANCE="no") &
                ' Delete (old) ',TRIM(S),LLB(I),' ? : '
           READ(5,'(A1)')CH1
           IF(CH1.EQ.'y'.OR.CH1.EQ.'Y')THEN
              LNL(I)=0
           ENDIF
        ENDIF
     ENDDO
  ELSE
     DO I=1,NLB
        IF(INLIST(NLB,LLB(I),NL,LFR,LTO))THEN
           LNL(I)=0
        ENDIF
     ENDDO
  ENDIF

END SUBROUTINE DELETE

!------ -------- ------
LOGICAL FUNCTION CHCKLB(NLB,LNL)

  INTEGER, INTENT(IN) :: NLB, LNL(NLB)
  INTEGER I, J

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

! ---------- ------
  SUBROUTINE LISTBR(NBR,BBR,BNB,NLB,LBI,LNL,NB,BFR,BTO)
!
    USE UTILITY
    INTEGER NBR,BBR(NBR),BNB(NBR),NLB,LBI(NLB),LNL(NLB)
    INTEGER NB,BFR(NBR),BTO(NBR)
    LOGICAL FIRST
    INTEGER I, J
!
    IF(NBR.EQ.0)THEN
       WRITE(6,"(/,' Empty bifurcation diagram file')")
       RETURN
    ENDIF

    FIRST=.TRUE.
    J=1
    DO I=1,NBR
       IF(NB==0 .OR. INLIST(NLB,I,NB,BFR,BTO))THEN
          IF(FIRST)THEN
             WRITE(6,"(/,'  BR NEW  Labels in branch')")
             FIRST=.FALSE.
          ENDIF
          WRITE(6,"(I4)",ADVANCE="no")BBR(I)
          IF(I==BNB(I).OR.BNB(I)/=0)THEN
             WRITE(6,"(I4)",ADVANCE="no")BNB(I)
             IF(J<=NLB)THEN
                IF(LBI(J)==I)THEN
                   WRITE(6,"(I5)",ADVANCE="no")LNL(J)
                   J=J+1
                   DO WHILE(LBI(J)==I)
                      WRITE(6,"(A,I5)",ADVANCE="no")", ",LNL(J)
                      J=J+1
                   ENDDO
                ENDIF
             ENDIF
             WRITE(6,"()")
          ELSE
             WRITE(6,"('        DELETED')")
          ENDIF
       ENDIF
    ENDDO

  END SUBROUTINE LISTBR
!======================================================================

END PROGRAM AUTLAB
!======================================================================
