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
  PUBLIC KEEPMAIN,RDFILE,LISTLB,INLIST,WRFILE7,WRFILE8,RELABEL

CONTAINS

! ---------- --------
  SUBROUTINE KEEPMAIN(DELETEFN)
    LOGICAL, EXTERNAL :: DELETEFN

    INTEGER, PARAMETER :: MXLB=10000
    INTEGER LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
    INTEGER LFR(MXLB),LTO(MXLB)
    INTEGER NLB,I

    OPEN(27,FILE='fort.27',STATUS='old',ACCESS='sequential')
    OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
    OPEN(37,FILE='fort.37',STATUS='unknown',ACCESS='sequential')
    OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')

    CALL RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,28)
    DO I=1,NLB
       IF(DELETEFN(I,LTY(I)))THEN
          LNL(I)=0
          LTY(I)=0
       ENDIF
    ENDDO
    CALL WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)
    CALL WRFILE8(LNL)

  END SUBROUTINE KEEPMAIN

! ---------- ------
  SUBROUTINE RDFILE(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,UNIT)

    INTEGER MXLB,NLB
    INTEGER LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB),UNIT
    LOGICAL EOF
    INTEGER IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NSKIP

    NLB=0
    DO
       READ(UNIT,*,END=2)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NSKIP
       IF(NLB>=MXLB)THEN
          WRITE(6,"(A,I6,A,/,A)") &
               ' ERROR : Maximum number of labels (',MXLB,') exceeded.', &
               ' Increase MXLB in auto/07p/src/utility.f and recompile.'
          STOP
       ENDIF
       NLB=NLB+1
       LBR(NLB)=IABS(IBR)
       LPT(NLB)=IABS(NTOT)
       IF(ITP<0)THEN
          LTY(NLB)=-MOD(-ITP,10)
       ELSE
          LTY(NLB)= MOD(ITP,10)
       ENDIF
       LLB(NLB)=LAB
       LNL(NLB)=LAB
       CALL SKIP(UNIT,NSKIP,EOF)
       IF(EOF)RETURN
    ENDDO
2   RETURN

  END SUBROUTINE RDFILE

! ---------- -------
  SUBROUTINE WRFILE7(MXLB,NLB,LBR,LPT,LTY,LLB,LNL)

    INTEGER MXLB,NLB
    INTEGER LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
    CHARACTER*132 LINE
    CHARACTER*4 CHR4,CLAB
    CHARACTER*1 CH1
    INTEGER L,LNUM,LEN
    LOGICAL EOL

    L=0
    LNUM=0
    REWIND 27
    DO
       EOL=.TRUE.
       READ(27,"(A)",ADVANCE='NO',EOR=97,END=99,SIZE=LEN)LINE
       EOL=.FALSE.
97     CONTINUE
       LNUM=LNUM+1
       CLAB=LINE(16:19)
       IF(LINE(1:4).NE.'   0' .AND. CLAB.NE.'   0')THEN
          L=L+1
          WRITE(CHR4,'(I4)')LLB(L)
          IF(CLAB.NE.CHR4)THEN
             WRITE(6,"(A/A,A4,A,I5/A,A4/A/A)", ADVANCE="NO") &
                  ' WARNING : The two files have incompatible labels :', &
                  '  b-file label ',CLAB,' at line ',LNUM, &
                  '  s-file label ',CHR4, &
                  ' New labels may be assigned incorrectly.', &
                  ' Continue ? : '
             READ(5,"(A1)")CH1
             IF(CH1/='y'.AND.CH1/='Y')THEN
                WRITE(6,"(A)") &
                     'Rewrite discontinued. Recover original files'
                RETURN
             ENDIF
          ENDIF
          WRITE(LINE(11:14),'(I4)')LTY(L)
          WRITE(LINE(15:19),'(I5)')LNL(L)
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
  SUBROUTINE WRFILE8(LNL)

    INTEGER LNL(*)
    INTEGER IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR
    INTEGER I,L
    CHARACTER(150) LINE
    LOGICAL EOF
!
    L=0
    REWIND 28
    DO
       READ(28,*,END=99)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
            NAR,NROWPR,NTST,NCOL,NPAR
       L=L+1
       IF(LNL(L)>0)THEN
          WRITE(38,101)IBR,NTOT,ITP,LNL(L),NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR
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

99  RETURN
  END SUBROUTINE WRFILE8

! ------- -------- ------
  LOGICAL FUNCTION INLIST(MXLB,LAB,NL,LFR,LTO)

    INTEGER MXLB,LAB,NL,LFR(MXLB),LTO(MXLB)
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
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IUNIT,NSKIP
    LOGICAL, INTENT(OUT) :: EOF
    CHARACTER(12) FMT
!
    WRITE(FMT,'(AI9A)')'(',NSKIP-1,'/)'
    EOF=.TRUE.
    READ(IUNIT,FMT,END=2)
    EOF=.FALSE.
2   RETURN

  END SUBROUTINE SKIP
!
! LISTLB, TYPE and RELABEL are used by listlabels.f, autlab.f, or relabel.f
!
! ---------- ------
  SUBROUTINE LISTLB(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO,NEW)
!
    LOGICAL NEW
    INTEGER MXLB,NLB,LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
    INTEGER NL,LFR(MXLB),LTO(MXLB)
    LOGICAL FIRST
    INTEGER I
!
    IF(NLB.EQ.0)THEN
       WRITE(6,"(/,' Empty solutions file')")
       RETURN
    ENDIF

    FIRST=.TRUE.
    DO I=1,NLB
       IF(NL==0 .OR. INLIST(MXLB,LLB(I),NL,LFR,LTO))THEN
          IF(FIRST)THEN
             IF(NEW)THEN
                WRITE(6,"(/,'  BR    PT  TY LAB  NEW')")
             ELSE
                WRITE(6,"(/,'  BR    PT  TY LAB')")
             ENDIF
             FIRST=.FALSE.
          ENDIF
          WRITE(6,"(I4,I6,2X,A2,I4)",ADVANCE="no")LBR(I),LPT(I), &
               TYPE(LTY(I)),LLB(I)
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
  ! ----------- -------- ----
    CHARACTER*2 FUNCTION TYPE(ITP)
      INTEGER ITP
      CHARACTER*2, PARAMETER :: TYPESP(9) = &
           (/ 'BP','LP','HB','  ','LP','BP','PD','TR','EP' /)
      CHARACTER*2, PARAMETER :: TYPESN(9) = &
           (/ '  ','  ','  ','UZ','  ','  ','  ','  ','MX' /)
      IF(ITP>0)THEN
         TYPE=TYPESP(MOD(ITP,10))
      ELSEIF(ITP<0)THEN
         TYPE=TYPESN(MOD(-ITP,10))
      ELSE
         TYPE='  '
      ENDIF
    END FUNCTION TYPE

  END SUBROUTINE LISTLB

! ---------- -------
  SUBROUTINE RELABEL(MXLB,NLB,LBR,LPT,LTY,LLB,LNL,NL,LFR,LTO)

    INTEGER MXLB,NLB,NL
    INTEGER LBR(MXLB),LPT(MXLB),LTY(MXLB),LLB(MXLB),LNL(MXLB)
    INTEGER LFR(MXLB),LTO(MXLB)
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
               (LNL(I)>0 .AND. INLIST(MXLB,LLB(I),NL,LFR,LTO)) )THEN
             WRITE(6,"(' Old label ',I4,';  Enter new label : ')", &
                  ADVANCE="no")LLB(I)
             READ(5,*)LNL(I)
          ENDIF
       ENDDO
    ENDIF
  END SUBROUTINE RELABEL
!======================================================================
!======================================================================
END MODULE UTILITY
