!==============================================================================
!==============================================================================
!      Utility Program for ``tripling'' solutions in an AUTO q.xxx file
!==============================================================================
!==============================================================================
!
PROGRAM TRIPLE
  IMPLICIT NONE
!
  DOUBLE PRECISION, ALLOCATABLE :: TM(:),RLDOT(:),PAR(:),U(:,:)
  INTEGER, ALLOCATABLE :: ICP(:)
  LOGICAL OLD
  CHARACTER(120) LINE
  DOUBLE PRECISION, PARAMETER :: EPS = 1d-16
  INTEGER I,J,L,IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL,NTPL1,NROWPRSMALL
  INTEGER NAR,NROWPR,NROWPR1,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
!
  L=0
  OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
  OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
1 CONTINUE
  READ(28,'(A)',END=99)LINE
  IF (LEN_TRIM(LINE) <= 73) THEN
     READ(LINE,*)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
          NAR,NROWPR,NTST,NCOL,NPAR
     OLD=.TRUE.
  ELSE
     READ(LINE,*)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
          NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
     OLD=.FALSE.
  ENDIF
  NROWPRSMALL=((NAR-1)/7+1)*NTPL + (NPAR+6)/7
  IF(NTPL.EQ.1)THEN
     NTPL1=NTPL
     NROWPR1=NROWPR
  ELSE
     NTST=3*NTST
     NTPL1=3*NTPL-2
     NROWPR1=NROWPR+2*NTPL-2
     IF(NTST.NE.0.AND.NROWPR.GT.NROWPRSMALL)THEN
        NROWPR1=NROWPR1+2*NTPL-2
     ENDIF
  ENDIF
  IF(OLD)THEN
     WRITE(38,101)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL1, &
          NAR,NROWPR1,NTST,NCOL,NPAR
  ELSE
     WRITE(38,111)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL1, &
          NAR,NROWPR1,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
  ENDIF
  ALLOCATE(TM(NTPL),U(NTPL,NAR-1))
  DO J=1,NTPL
     READ(28,*) TM(J),(U(J,I),I=1,NAR-1) 
     WRITE(38,102)TM(J)/3,(U(J,I),I=1,NAR-1)
  ENDDO
  IF(NTPL.GT.1)THEN
     DO J=2,NTPL
        WRITE(38,102)(1+TM(J))/3, &
             (U(J,I)+U(NTPL,I)-U(1,I),I=1,NAR-1)
     ENDDO
     DO J=2,NTPL
        WRITE(38,102)(2+TM(J))/3, &
             (U(J,I)+2*(U(NTPL,I)-U(1,I)),I=1,NAR-1)
     ENDDO
  ENDIF
  IF(NTST.NE.0.AND.NROWPR.GT.NROWPRSMALL)THEN
     ALLOCATE(RLDOT(NPAR),ICP(NPAR))
     READ(28,*) (ICP(I),I=1,NFPR)
     WRITE(38,103)(ICP(I),I=1,NFPR)
     READ(28,*) (RLDOT(I),I=1,NFPR)
     WRITE(38,102)(RLDOT(I),I=1,NFPR)
     DO J=1,NTPL
        READ(28,*) (U(J,I),I=1,NAR-1) 
        WRITE(38,102)(U(J,I),I=1,NAR-1)
     ENDDO
     IF(NTPL.GT.1)THEN
        DO J=2,NTPL
           WRITE(38,102)(U(J,I),I=1,NAR-1)
        ENDDO
        DO J=2,NTPL
           WRITE(38,102)(U(J,I),I=1,NAR-1)
        ENDDO
     ENDIF
     DEALLOCATE(RLDOT,ICP)
  ENDIF
  ALLOCATE(PAR(NPAR))
  READ(28,*) (PAR(I),I=1,NPAR)
  IF(NTPL.GT.1)THEN
     PAR(11)=3*PAR(11)
  ENDIF
  WRITE(38,102)(PAR(I),I=1,NPAR)
  DEALLOCATE(PAR,TM,U)
  GOTO 1

101 FORMAT(6I6,I8,I6,I8,3I5)
111 FORMAT(6I6,I8,I6,I8,7I5)
102 FORMAT(4X,1P7E19.10)
103 FORMAT(20I5)

99 STOP
END PROGRAM TRIPLE
!==============================================================================
!==============================================================================
