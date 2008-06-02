!======================================================================
!======================================================================
!         Utility Program for Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM RELABELF
  PARAMETER (MXLB=10000)
  CHARACTER*1 CMD
  DIMENSION LLB(MXLB)
  CHARACTER*80 INB,OUTB,INS,OUTS

  N=COMMAND_ARGUMENT_COUNT()
  IF (N==4) THEN
     CALL GET_COMMAND_ARGUMENT(1,INB)
     CALL GET_COMMAND_ARGUMENT(2,INS)
     CALL GET_COMMAND_ARGUMENT(3,OUTB)
     CALL GET_COMMAND_ARGUMENT(4,OUTS)
  ELSE
     INB='fort.27'
     INS='fort.28'
     OUTB='fort.37'
     OUTS='fort.38'
  ENDIF

  OPEN(28,FILE=INS,STATUS='old',ACCESS='sequential')
  OPEN(38,FILE=OUTS,STATUS='replace',ACCESS='sequential')
  CALL SFILE(MXLB,NLB,LLB)
  CLOSE(28)
  CLOSE(38)

  OPEN(27,FILE=INB,STATUS='old',ACCESS='sequential')
  OPEN(37,FILE=OUTB,STATUS='replace',ACCESS='sequential')
  CALL BFILE(NLB,LLB)
  CLOSE(27)
  CLOSE(37)

  STOP

CONTAINS

! ---------- ------
  SUBROUTINE SFILE(MXLB,NLB,LLB)

    INTEGER MXLB,NLB,LLB(MXLB)
    INTEGER IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR
    INTEGER I,L,S
    CHARACTER(150) LINE
    LOGICAL EOF
!
    L=0
    NLB=0
    DO
       READ(28,*,END=99)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
            NAR,NROWPR,NTST,NCOL,NPAR

       IF(NLB>=MXLB)THEN
          WRITE(6,"(A,I6,A,/,A)") &
               ' ERROR : Maximum number of labels (',MXLB,') exceeded.', &
               ' Increase MXLB in auto/07p/src/utility.f and recompile.'
          STOP
       ENDIF
       NLB=NLB+1
       LLB(NLB)=LAB

       L=L+1
       WRITE(38,101)IBR,NTOT,ITP,L,NFPR,ISW,NTPL, &
            NAR,NROWPR,NTST,NCOL,NPAR
       DO I=1,NROWPR
          READ(28,"(A)",ADVANCE='NO',EOR=98,END=99,SIZE=LEN)LINE
          DO
             WRITE(38,"(A)",ADVANCE='NO')LINE(1:LEN)
             READ(28,"(A)",ADVANCE='NO',EOR=98,END=99,SIZE=LEN)LINE
          ENDDO
98        CONTINUE
          WRITE(38,"(A)")LINE(1:LEN)
       ENDDO
    ENDDO

 101   FORMAT(6I6,I8,I6,I8,3I5)

99  RETURN
  END SUBROUTINE SFILE

! ---------- -------
  SUBROUTINE BFILE(NLB,LLB)

    INTEGER NLB,LAB
    INTEGER LLB(NLB)
    CHARACTER*132 LINE
    CHARACTER*5 CHR5
    CHARACTER*1 CH1
    INTEGER L,LNUM,LEN
    LOGICAL EOL

    L=0
    LNUM=0
    DO
       EOL=.TRUE.
       READ(27,"(A)",ADVANCE='NO',EOR=97,END=99,SIZE=LEN)LINE
       EOL=.FALSE.
97     CONTINUE
       LNUM=LNUM+1
       IF(LINE(1:4).NE.'   0' .AND. LINE(15:19).NE.'    0')THEN
          L=L+1
          READ(LINE(15:19),'(I5)')LAB
          IF(L>NLB.OR.LAB/=LLB(L))THEN
             WRITE(*,"(A/A,A4,A,I5/A,A4/A/A)", ADVANCE="NO") &
                  ' WARNING : The two files have incompatible labels :', &
                  '  b-file label ',CLAB,' at line ',LNUM, &
                  '  s-file label ',CHR4, &
                  ' New labels may be assigned incorrectly.', &
                  ' Continue ? : '
             READ(*,"(A1)")CH1
             IF(CH1/='y'.AND.CH1/='Y')THEN
                WRITE(*,"(A)") &
                     'Rewrite discontinued. Recover original files'
                RETURN
             ENDIF
          ENDIF
          WRITE(LINE(15:19),'(I5)')L
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
  END SUBROUTINE BFILE

END PROGRAM RELABELF
!======================================================================
!======================================================================
