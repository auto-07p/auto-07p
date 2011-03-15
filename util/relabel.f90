!======================================================================
!======================================================================
!         Utility Program for Relabeling Labeled Solutions
!                        in AUTO97 Data Files
!======================================================================
!======================================================================
!
PROGRAM RELABELF
  USE COMPAT
  IMPLICIT NONE
  CHARACTER(80) INB,OUTB,INS,OUTS
  INTEGER N,NLB
  TYPE LABEL
     INTEGER LAB
     TYPE(LABEL), POINTER :: NEXT
  END TYPE LABEL
  TYPE(LABEL), POINTER :: LIST

  N=AUTARGC()
  IF (N==4) THEN
     CALL AUTGETARG(1,INB)
     CALL AUTGETARG(2,INS)
     CALL AUTGETARG(3,OUTB)
     CALL AUTGETARG(4,OUTS)
  ELSE
     INB='fort.27'
     INS='fort.28'
     OUTB='fort.37'
     OUTS='fort.38'
  ENDIF

  OPEN(28,FILE=INS,STATUS='old',ACCESS='sequential')
  OPEN(38,FILE=OUTS,STATUS='replace',ACCESS='sequential')
  CALL SFILE(LIST,NLB)
  CLOSE(28)
  CLOSE(38)

  OPEN(27,FILE=INB,STATUS='old',ACCESS='sequential')
  OPEN(37,FILE=OUTB,STATUS='replace',ACCESS='sequential')
  CALL BFILE(LIST,NLB)
  CLOSE(27)
  CLOSE(37)

  STOP

CONTAINS

! ---------- ------
  SUBROUTINE SFILE(LIST,NLB)

    INTEGER NLB
    INTEGER IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,NCOL,NPAR
    INTEGER NPARI,NDM,IPS,IPRIV
    INTEGER I,L,LEN
    CHARACTER(150) LINE
    LOGICAL OLD
    TYPE(LABEL), POINTER :: LIST, P
!
    L=0
    NLB=0
    ALLOCATE(LIST)
    P=>LIST
    DO
       NULLIFY(P%NEXT)
       READ(28,'(A)',END=99)LINE
       IF (LEN_TRIM(LINE) == 0) GOTO 99
       IF (LEN_TRIM(LINE) <= 73) THEN
          READ(LINE,*)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR
          OLD=.TRUE.
       ELSE
          READ(LINE,*)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
          OLD=.FALSE.
       ENDIF

       NLB=NLB+1
       P%LAB=LAB

       L=L+1
       IF(OLD)THEN
          WRITE(38,101)IBR,NTOT,ITP,L,NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR
       ELSE
          WRITE(38,111)IBR,NTOT,ITP,L,NFPR,ISW,NTPL, &
               NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
       ENDIF
       DO I=1,NROWPR
          READ(28,"(A)",ADVANCE='NO',EOR=98,END=99,SIZE=LEN)LINE
          DO
             WRITE(38,"(A)",ADVANCE='NO')LINE(1:LEN)
             READ(28,"(A)",ADVANCE='NO',EOR=98,END=99,SIZE=LEN)LINE
          ENDDO
98        CONTINUE
          WRITE(38,"(A)")LINE(1:LEN)
       ENDDO
       ALLOCATE(P%NEXT)
       P=>P%NEXT
    ENDDO

 101   FORMAT(6I6,I8,I6,I8,3I5)
 111   FORMAT(6I6,I8,I6,I8,7I5)

99  RETURN
  END SUBROUTINE SFILE

! ---------- -------
  SUBROUTINE BFILE(LIST,NLB)

    INTEGER NLB,LAB
    CHARACTER(132) LINE
    CHARACTER(1) CH1
    INTEGER I,J,I1,I2,L1,L,LNUM,LEN
    LOGICAL EOL
    CHARACTER(LEN=5) FMT ! fits "(I99)"
    TYPE(LABEL), POINTER :: LIST, P

    L=0
    LNUM=0
    L1=0
    P=>LIST
    DO
       EOL=.TRUE.
       READ(27,"(A)",ADVANCE='NO',EOR=97,END=99,SIZE=LEN)LINE
       EOL=.FALSE.
97     CONTINUE
       LNUM=LNUM+1
       J=1
       I2=0
       DO I=1,4
          ! skip spaces
          DO
             IF(LINE(J:J)/=' ')EXIT
             J=J+1
             IF(J>LEN)EXIT
          ENDDO
          ! check for header line
          IF(I==1.AND.LINE(J:J)=='0')THEN
             EXIT
          ENDIF
          ! look for next space after BR/PT/TY/LAB
          DO
             IF(LINE(J:J)==' ')EXIT
             J=J+1
             IF(J>LEN)EXIT
          ENDDO
          IF(J>LEN)THEN
             EXIT
          ENDIF
          ! Put line index for LAB, rest here
          IF(I==3)THEN
             I1=J
          ELSEIF(I==4)THEN
             I2=J
          ENDIF
       ENDDO
       LAB=0
       IF(I2/=0)THEN
          READ(LINE(I1:I2-1),*)LAB
       ENDIF

       IF(LAB/=0)THEN
          L=L+1
          IF(L>NLB.OR.LAB/=P%LAB)THEN
             WRITE(*,"(A/A,I5,A,I5/A,I5/A/A)", ADVANCE="NO") &
                  ' WARNING : The two files have incompatible labels :', &
                  '  b-file label ',LAB,' at line ',LNUM, &
                  '  s-file label ',P%LAB, &
                  ' New labels may be assigned incorrectly.', &
                  ' Continue ? : '
             READ(*,"(A1)")CH1
             IF(CH1/='y'.AND.CH1/='Y')THEN
                WRITE(*,"(A)") &
                     'Rewrite discontinued. Recover original files'
                RETURN
             ENDIF
          ENDIF
          IF(ASSOCIATED(P%NEXT))THEN
             P=P%NEXT
          ENDIF
          IF(I2-I1/=L1)THEN
             L1=I2-I1
             WRITE(FMT,"(A,I2,A)")'(I',L1,')'
          ENDIF
          WRITE(LINE(I1:I2-1),FMT)L
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
