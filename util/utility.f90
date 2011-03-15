!======================================================================
!======================================================================
! Utility Module for Listing, Deleting, Relabeling Labeled Solutions
!                        in AUTO-07p Data Files
!======================================================================
!======================================================================
!
MODULE UTILITY

  IMPLICIT NONE
  PRIVATE
  PUBLIC KEEPMAIN,RDFILE7,RDFILE8,LISTLB,INLIST,WRFILE7,WRFILE8,RELABEL
  PUBLIC GTFILE7, GTFILE8, LABEL
 
  TYPE LABEL
     INTEGER BR, PT, TY, LAB
     TYPE(LABEL), POINTER :: NEXT
  END TYPE LABEL

CONTAINS

! ---------- --------
  SUBROUTINE KEEPMAIN(DELETEFN)
    LOGICAL, EXTERNAL :: DELETEFN

    INTEGER, ALLOCATABLE :: LBR(:),LPT(:),LTY(:),LLB(:),LNL(:)
    TYPE(LABEL), POINTER :: LIST
    INTEGER DUM(1)
    INTEGER NLB,I

    OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
    OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
    OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
    OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')

    NULLIFY(LIST)
    CALL RDFILE8(LIST,NLB,28)
    ALLOCATE(LBR(NLB),LPT(NLB),LTY(NLB),LLB(NLB),LNL(NLB))
    CALL GTFILE8(LIST,NLB,LBR,LPT,LTY,LLB,LNL)
    DO I=1,NLB
       IF(DELETEFN(I,MOD(LTY(I),10)))THEN
          LNL(I)=0
          LTY(I)=0
       ENDIF
    ENDDO
    CALL WRFILE7(0,DUM,NLB,LLB,LNL)
    CALL WRFILE8(LBR,LNL)

  END SUBROUTINE KEEPMAIN

! ---------- -------
  SUBROUTINE GTFILE7(LIST,NBR,BBR,BNB)

    TYPE(LABEL), POINTER :: LIST
    INTEGER NBR
    INTEGER BBR(NBR),BNB(NBR)
    TYPE(LABEL), POINTER :: P,Q
    INTEGER I

    P=>LIST
    DO I=1,NBR
       BBR(I)=P%BR
       BNB(I)=P%BR
       Q=>P
       P=>P%NEXT
       DEALLOCATE(Q)
    ENDDO
    IF(ASSOCIATED(P))DEALLOCATE(P)
    NULLIFY(LIST)
  END SUBROUTINE GTFILE7

! ---------- -------
  SUBROUTINE RDFILE7(LIST,NBR,NLB,LBI,UNIT)

    ! just scan branch numbers and labels on the branch
    TYPE(LABEL), POINTER :: LIST
    INTEGER, INTENT(IN) :: NLB, UNIT
    INTEGER, INTENT(OUT) :: NBR
    INTEGER, INTENT(OUT) :: LBI(NLB)
    INTEGER J,IBR,IPT,ITY,ILB,PREVPT
    TYPE(LABEL), POINTER :: P

    NBR=0
    J=1
    ALLOCATE(LIST)
    P=>LIST
    DO
       NULLIFY(P%NEXT)
       DO
          READ(UNIT,*,END=99,ERR=99)IBR
          IF(IBR/=0)EXIT
       ENDDO
       NBR=NBR+1
       P%BR=ABS(IBR)
       BACKSPACE UNIT
       READ(UNIT,*,END=99,ERR=98)IBR,IPT,ITY,ILB
       DO
          PREVPT=ABS(IPT)
          IF(ILB/=0)THEN
             LBI(J)=NBR
             J=J+1
          ENDIF
          READ(UNIT,*,END=99,ERR=98)IBR,IPT,ITY,ILB
          IF(IBR==0)EXIT
          IF(ABS(IPT)==1.AND.PREVPT/=9999.AND.PREVPT/=0)THEN
             BACKSPACE UNIT
             EXIT
          ENDIF
       ENDDO
98     CONTINUE
       ALLOCATE(P%NEXT)
       P=>P%NEXT
    ENDDO
99  RETURN
  END SUBROUTINE RDFILE7

! ---------- -------
  SUBROUTINE GTFILE8(LIST,NLB,LBR,LPT,LTY,LLB,LNL)

    TYPE(LABEL), POINTER :: LIST
    INTEGER NLB
    INTEGER LBR(NLB),LPT(NLB),LTY(NLB),LLB(NLB),LNL(NLB)
    TYPE(LABEL), POINTER :: P,Q
    INTEGER I

    P=>LIST
    DO I=1,NLB
       LBR(I)=P%BR
       LPT(I)=P%PT
       LTY(I)=P%TY
       LLB(I)=P%LAB
       LNL(I)=P%LAB
       Q=>P
       P=>P%NEXT
       DEALLOCATE(Q)
    ENDDO
    IF(ASSOCIATED(P))DEALLOCATE(P)
    NULLIFY(LIST)
  END SUBROUTINE GTFILE8

! ---------- -------
  SUBROUTINE RDFILE8(LIST,NLB,UNIT)

    TYPE(LABEL), POINTER :: LIST
    INTEGER NLB,UNIT
    LOGICAL EOF
    INTEGER IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NSKIP
    TYPE(LABEL), POINTER :: P

    NLB=0
    ALLOCATE(LIST)
    P=>LIST
    DO
       NULLIFY(P%NEXT)
       READ(UNIT,*,END=2)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NSKIP
       NLB=NLB+1
       P%BR=ABS(IBR)
       P%PT=ABS(NTOT)
       P%TY=ITP
       P%LAB=LAB
       CALL SKIP(UNIT,NSKIP,EOF)
       IF(EOF)RETURN
       ALLOCATE(P%NEXT)
       P=>P%NEXT
    ENDDO
2   RETURN

  END SUBROUTINE RDFILE8

! ---------- -------
  SUBROUTINE WRFILE7(NBR,BNB,NLB,LLB,LNL)

    INTEGER NBR,NEWBR,NLB
    INTEGER BNB(NBR),LLB(NLB),LNL(NLB)
    CHARACTER(LEN=132) LINE
    CHARACTER(LEN=1) CH1
    CHARACTER(LEN=17) FMT ! fits "(I99,I99,I99,I99)"
    INTEGER I,J,L,LNUM,LEN,BRI,BR,PREVPT,PT,TY,LAB,IND(4),L0,L1,L2,L3
    LOGICAL EOL, HEADER

    L=0
    LNUM=0
    L0=0
    L1=0
    L2=0
    L3=0
    BRI=0
    HEADER=.FALSE.
    PT=0
    REWIND 27
    DO
       EOL=.TRUE.
       READ(27,"(A)",ADVANCE='NO',EOR=97,END=99,SIZE=LEN)LINE
       EOL=.FALSE.
97     CONTINUE
       LNUM=LNUM+1
       J=1
       IND(1)=0
       DO I=1,4
          ! skip spaces
          DO
             IF(LINE(J:J)/=' ')EXIT
             J=J+1
             IF(J>LEN)EXIT
          ENDDO
          ! check for header line
          IF(I==1.AND.LINE(J:J)=='0')THEN
             IF(.NOT.HEADER)THEN
                BRI=BRI+1
             ENDIF
             HEADER=.TRUE.
             EXIT
          ENDIF
          ! look for next space after BR/PT/TY/LAB
          DO
             IF(LINE(J:J)==' ')EXIT
             J=J+1
             IF(J>LEN)EXIT
          ENDDO
          IF(J>LEN)THEN
             IND(1)=0
             EXIT
          ENDIF
          ! Put line index for PT, TY, LAB, rest here
          IND(I)=J
       ENDDO
       LAB=0
       BR=0
       IF(IND(1)/=0)THEN
          PREVPT=PT
          READ(LINE(:IND(4)-1),*)BR,PT,TY,LAB
          IF(.NOT.HEADER.AND.PT==1.AND.PREVPT/=9999.AND.PREVPT/=0)THEN
             BRI=BRI+1
          ENDIF
          HEADER=.FALSE.
       ENDIF
       NEWBR=BR
       IF(BRI<=NBR)THEN
          NEWBR=BNB(BRI)
          IF(IND(1)/=0)THEN
             NEWBR=SIGN(NEWBR,BR)
          ELSE
             BR=NEWBR
          ENDIF
       ENDIF

       IF(LAB/=0.OR.NEWBR/=BR)THEN
          IF(LAB/=0)L=L+1
          IF(LAB/=0.AND.LAB/=LLB(L))THEN
             WRITE(6,"(A/A,I5,A,I5/A,I5/A/A)", ADVANCE="NO") &
                  ' WARNING : The two files have incompatible labels :', &
                  '  b-file label',LAB,' at line ',LNUM, &
                  '  s-file label',LLB(L), &
                  ' New labels may be assigned incorrectly.', &
                  ' Continue ? : '
             READ(5,"(A1)")CH1
             IF(CH1/='y'.AND.CH1/='Y')THEN
                WRITE(6,"(A)") &
                     'Rewrite discontinued. Recover original files'
                RETURN
             ENDIF
          ENDIF
          IF(IND(1)-1/=L0.OR.IND(2)-IND(1)/=L1.OR. &
               IND(3)-IND(2)/=L2.OR.IND(4)-IND(3)/=L3) THEN
             L0=IND(1)-1
             L1=IND(2)-IND(1)
             L2=IND(3)-IND(2)
             L3=IND(4)-IND(3)
             WRITE(FMT,"(A,I2,A,I2,A,I2,A,I2,A)")&
                  '(I',L0,',I',L1,',I',L2,',I',L3,')'
          ENDIF
          IF(LAB==0)THEN
             WRITE(LINE(:IND(4)-1),FMT)NEWBR,PT,TY,LAB
          ELSE
             WRITE(LINE(:IND(4)-1),FMT)NEWBR,PT,TY,LNL(L)
          ENDIF
       ENDIF
       IF(NEWBR==0)THEN ! deleted branch; don't write
          IF(.NOT.EOL)THEN
             READ(27,"()",END=99)
          ENDIF
          CYCLE
       ENDIF
       IF(.NOT.EOL)THEN
          DO
             WRITE(37,"(A)",ADVANCE='NO')LINE(1:LEN)
             READ(27,"(A)",ADVANCE='NO',EOR=98,SIZE=LEN)LINE
          ENDDO
98        CONTINUE
       ENDIF
       WRITE(37,"(A)")LINE(1:LEN)
    ENDDO
99  RETURN
  END SUBROUTINE WRFILE7

! ---------- -------
  SUBROUTINE WRFILE8(LBR,LNL)

    INTEGER LBR(*),LNL(*)
    INTEGER IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR
    INTEGER NPARI,NDM,IPS,IPRIV
    INTEGER I,L
    CHARACTER(150) LINE
    LOGICAL EOF
!
    L=0
    REWIND 28
    DO
       READ(28,'(A)',END=99)LINE
       IF (LEN_TRIM(LINE) <= 73) THEN
          READ(LINE,*)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR
          L=L+1
          IF(LNL(L)>0)THEN
             WRITE(38,101)LBR(L),NTOT,ITP,LNL(L),NFPR,ISW,NTPL, &
                  NAR,NROWPR,NTST,NCOL,NPAR
          ENDIF
       ELSE
          READ(LINE,*)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
          L=L+1
          IF(LNL(L)>0)THEN
             WRITE(38,111)LBR(L),NTOT,ITP,LNL(L),NFPR,ISW,NTPL, &
                  NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
          ENDIF
       ENDIF
       IF(LNL(L)>0)THEN
          DO I=1,NROWPR
             LINE(:)=' '
             READ(28,'(A)',END=99)LINE
             WRITE(38,'(A)')LINE(1:LEN_TRIM(LINE))
          ENDDO
       ELSE
          CALL SKIP(28,NROWPR,EOF)
          IF(EOF)RETURN
       ENDIF
    ENDDO

 101   FORMAT(6I6,I8,I6,I8,3I5)
 111   FORMAT(6I6,I8,I6,I8,7I5)

99  RETURN
  END SUBROUTINE WRFILE8

! ------- -------- ------
  LOGICAL FUNCTION INLIST(NLB,LAB,NL,LFR,LTO)

    INTEGER NLB,LAB,NL,LFR(NLB),LTO(NLB)
    INTEGER I

    INLIST=.FALSE.
    DO I=1,NL
       IF(LAB.GE.LFR(I).AND.LAB.LE.LTO(I))THEN
          INLIST=.TRUE.
          RETURN
       ENDIF
    ENDDO

  END FUNCTION INLIST
!
! ---------- ----
  SUBROUTINE SKIP(IUNIT,NSKIP,EOF)
!
! Skips the specified number of lines on fort.IUNIT.
!
    INTEGER, INTENT(IN) :: IUNIT,NSKIP
    LOGICAL, INTENT(OUT) :: EOF
    CHARACTER(12) FMT
!
    WRITE(FMT,'(A,I9,A)')'(',NSKIP-1,'/)'
    EOF=.TRUE.
    READ(IUNIT,FMT,END=2)
    EOF=.FALSE.
2   RETURN

  END SUBROUTINE SKIP
!
! LISTLB, TYPE, and RELABEL are used by listlabels.f, autlab.f, or relabel.f
!
! ---------- ------
  SUBROUTINE LISTLB(NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO,NEW)
!
    LOGICAL NEW
    INTEGER NLB,LBR(NLB),LPT(NLB),LTY(NLB),LLB(NLB),LNL(NLB)
    INTEGER NL,LFR(NLB),LTO(NLB)
    LOGICAL FIRST
    INTEGER I
!
    IF(NLB.EQ.0)THEN
       WRITE(6,"(/,' Empty solutions file')")
       RETURN
    ENDIF

    FIRST=.TRUE.
    DO I=1,NLB
       IF(NL==0 .OR. INLIST(NLB,LLB(I),NL,LFR,LTO))THEN
          IF(FIRST)THEN
             IF(NEW)THEN
                WRITE(6,"(/,'  BR    PT  TY  LAB  NEW')")
             ELSE
                WRITE(6,"(/,'  BR    PT  TY  LAB')")
             ENDIF
             FIRST=.FALSE.
          ENDIF
          WRITE(6,"(I4,I6,1X,A3,I5)",ADVANCE="no")LBR(I),LPT(I), &
               ADJUSTR(TYPE(LTY(I))),LLB(I)
          IF(.NOT.NEW)THEN
             WRITE(6,"()")
          ELSEIF(LLB(I)==LNL(I).OR.LNL(I)/=0)THEN
             WRITE(6,"(I5)")LNL(I)
          ELSE
             WRITE(6,"('        DELETED')")
          ENDIF
       ENDIF
    ENDDO

  CONTAINS

    CHARACTER(3) FUNCTION TYPE(ITP)

      ! returns the string label type corresponding to numerical type ITP
      INTEGER, INTENT(IN) :: ITP

      CHARACTER(2), PARAMETER :: ATYPES(-9:9) = &
           (/ 'MX','R4','R3','R2','R1','UZ','ZH','CP','BT','  ', &
              'BP','LP','HB','  ','LP','BP','PD','TR','EP' /)

      SELECT CASE(ITP)
      CASE(-32)
         TYPE='GH'
      CASE(23,83)
         TYPE='LTR'
      CASE(77,87)
         TYPE='PTR'
      CASE(28,78)
         TYPE='LPD'
      CASE(88)
         TYPE='TTR'
      CASE DEFAULT
         TYPE=ATYPES(MOD(ITP,10))
      END SELECT
    END FUNCTION TYPE

  END SUBROUTINE LISTLB

! ---------- -------
  SUBROUTINE RELABEL(S,NLB,LLB,LNL,NL,LFR,LTO)

    CHARACTER(*) S
    INTEGER NLB,NL
    INTEGER LLB(NLB),LNL(NLB)
    INTEGER LFR(NLB),LTO(NLB)
    INTEGER NN,I

    IF(NL==0)THEN
       NN=0
       DO I=1,NLB
          IF(LNL(I)>0)THEN
             NN=NN+1
             LNL(I)=NN
          ENDIF
       ENDDO
    ELSE
       DO I=1,NLB
          IF(NL==0 .OR. &
               (LNL(I)>0 .AND. INLIST(NLB,LLB(I),NL,LFR,LTO)) )THEN
             WRITE(6,"(' Old ',A,I5,';  Enter new ',A,' : ')", &
                  ADVANCE="no")S,LLB(I),S
             READ(5,*)LNL(I)
          ENDIF
       ENDDO
    ENDIF
  END SUBROUTINE RELABEL
!======================================================================
!======================================================================
END MODULE UTILITY
