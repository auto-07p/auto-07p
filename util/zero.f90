!=======================================================================
!=======================================================================
!    Utility Program for "zeroing small numbers" in an AUTO s.xxx file
!=======================================================================
!=======================================================================
!
PROGRAM ZERO
  IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!
  ALLOCATABLE TM(:),RLDOT(:),PAR(:),U(:,:),ICP(:)
  LOGICAL OLD
  CHARACTER(100) LINE
  PARAMETER (EPS = 1d-16)
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
  ALLOCATE(RLDOT(NPAR),PAR(NPAR),ICP(NPAR))
  IF(NTST.EQ.0)THEN
     IF(OLD)THEN
        WRITE(38,101)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
             NAR,NROWPR,NTST,NCOL,NPAR
     ELSE
        WRITE(38,111)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
             NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
     ENDIF
!    --------------------------------------------
     ALLOCATE(TM(1),U(1,NAR-1))
     J=1
     READ(28,*) TM(J),(U(J,I),I=1,NAR-1) 
     DO I=1,NAR-1
        IF(ABS(U(J,I)).LT.EPS) U(J,I)=0.d0
     ENDDO
     WRITE(38,102)TM(J),(U(J,I),I=1,NAR-1)
!    --------------------------------------------
     READ(28,*) (PAR(I),I=1,NPAR)
     DO I=1,NPAR
        IF(ABS(PAR(I)).LT.EPS) PAR(I)=0.d0
     ENDDO
     WRITE(38,102)(PAR(I),I=1,NPAR)
! --------------------------------------------
  ELSE
! --------------------------------------------
     IF(OLD)THEN
        WRITE(38,101)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL1, &
             NAR,NROWPR,NTST,NCOL,NPAR
     ELSE
        WRITE(38,111)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
             NAR,NROWPR,NTST,NCOL,NPAR,NPARI,NDM,IPS,IPRIV
     ENDIF
     ALLOCATE(TM(NTPL),U(NTPL,NAR-1))
     DO J=1,NTPL
        READ(28,*)   TM(J),(U(J,I),I=1,NAR-1) 
        DO I=1,NAR-1
           IF(ABS(U(J,I)).LT.EPS) U(J,I)=0.d0
        ENDDO
        WRITE(38,102)TM(J),(U(J,I),I=1,NAR-1)
     ENDDO
!    --------------------------------------------
     READ(28,*)   (ICP(I),I=1,NFPR)
     WRITE(38,103)(ICP(I),I=1,NFPR)
!    --------------------------------------------
     READ(28,*)   (RLDOT(I),I=1,NFPR)
     DO I=1,NFPR
        IF(ABS(RLDOT(I)).LT.EPS) RLDOT(I)=0.d0
     ENDDO
     WRITE(38,102)(RLDOT(I),I=1,NFPR)
!    --------------------------------------------
     DO J=1,NTPL
        READ(28,*)   (U(J,I),I=1,NAR-1) 
        DO I=1,NAR-1
           IF(ABS(U(J,I)).LT.EPS) U(J,I)=0.d0
        ENDDO
        WRITE(38,102)(U(J,I),I=1,NAR-1)
     ENDDO
!    --------------------------------------------
     READ(28,*)   (PAR(I),I=1,NPAR)
     DO I=1,NPAR
        IF(ABS(PAR(I)).LT.EPS) PAR(I)=0.d0
     ENDDO
     WRITE(38,102)(PAR(I),I=1,NPAR)
!    --------------------------------------------
  ENDIF
  DEALLOCATE(RLDOT,PAR,ICP,TM,U)
  GOTO 1

101 FORMAT(6I6,I8,I6,I8,3I5)
111 FORMAT(6I6,I8,I6,I8,7I5)
102 FORMAT(4X,1P7E19.10)
103 FORMAT(20I5)

99 STOP
END PROGRAM ZERO
!=======================================================================
!=======================================================================
