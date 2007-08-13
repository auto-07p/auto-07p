!==============================================================================
!==============================================================================
!      Utility Program for ``tripling'' solutions in an AUTO q.xxx file
!==============================================================================
!==============================================================================
!
PROGRAM TRIPLE
  IMPLICIT DOUBLE PRECISION(A-H,O-Z)
  ALLOCATABLE TM(:),RLDOT(:),PAR(:),U(:,:),ICP(:)

  L=0
  OPEN(28,FILE='fort.28',STATUS='old',ACCESS='sequential')
  OPEN(38,FILE='fort.38',STATUS='unknown',ACCESS='sequential')
1 CONTINUE
  READ(28,*,END=99)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
       NAR,NROWPR,NTST,NCOL,NPAR1
  ALLOCATE(RLDOT(NPAR),PAR(NPAR),ICP(NPAR))
  IF(NTST.EQ.0)THEN
     WRITE(38,101)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL, &
          NAR,NROWPR,NTST,NCOL,NPAR1
     ALLOCATE(TM(1),U(1,NAR-1))
     J=1
     READ(28,*) TM(J),(U(J,I),I=1,NAR-1) 
     WRITE(38,102)TM(J),(U(J,I),I=1,NAR-1)
     READ(28,*) (PAR(I),I=1,NPAR1)
     WRITE(38,102)(PAR(I),I=1,NPAR1)
  ELSE
     NTST=3*NTST
     NTPL1=3*NTPL-2
     NROWPR=NROWPR+4*NTPL-4
     WRITE(38,101)IBR,NTOT,ITP,LAB,NFPR,ISW,NTPL1, &
          NAR,NROWPR,NTST,NCOL,NPAR1
     ALLOCATE(TM(NTPL),U(NTPL,NAR-1))
     DO J=1,NTPL
        READ(28,*) TM(J),(U(J,I),I=1,NAR-1) 
        WRITE(38,102)TM(J)/3,(U(J,I),I=1,NAR-1)
     ENDDO
     DO J=2,NTPL
        WRITE(38,102)(1+TM(J))/3, &
             (U(J,I)+U(NTPL,I)-U(1,I),I=1,NAR-1)
     ENDDO
     DO J=2,NTPL
        WRITE(38,102)(2+TM(J))/3, &
             (U(J,I)+2*(U(NTPL,I)-U(1,I)),I=1,NAR-1)
     ENDDO
     READ(28,*) (ICP(I),I=1,NFPR)
     WRITE(38,103)(ICP(I),I=1,NFPR)
     READ(28,*) (RLDOT(I),I=1,NFPR)
     WRITE(38,102)(RLDOT(I),I=1,NFPR)
     DO J=1,NTPL
        READ(28,*) (U(J,I),I=1,NAR-1) 
        WRITE(38,102)(U(J,I),I=1,NAR-1)
     ENDDO
     DO J=2,NTPL
        WRITE(38,102)(U(J,I),I=1,NAR-1)
     ENDDO
     DO J=2,NTPL
        WRITE(38,102)(U(J,I),I=1,NAR-1)
     ENDDO
     READ(28,*) (PAR(I),I=1,NPAR1)
     PAR(11)=3*PAR(11)
     WRITE(38,102)(PAR(I),I=1,NPAR1)
  ENDIF
  DEALLOCATE(RLDOT,PAR,ICP,TM,U)
  GOTO 1

101 FORMAT(6I6,I8,I6,I8,3I5)
102 FORMAT(4X,1P7E19.10)
103 FORMAT(20I5)

99 STOP
END PROGRAM TRIPLE
!==============================================================================
!==============================================================================
