C     ------- ------
      PROGRAM AUTCON
C
C This program converts a data file into a labeled AUTO solution
C
C
      IMPLICIT NONE
      INTEGER, PARAMETER :: NPARX=36
C
      INTEGER NDIM,NOLD,NTST,NCOL,ISW,IPS,NPAR,ICP(1),I,J
      DOUBLE PRECISION RLDOT(1),TEMP,T,PERIOD
      DOUBLE PRECISION, ALLOCATABLE :: U(:),TM(:),UPS(:,:),VPS(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TMR(:),UPSR(:,:),VPSR(:,:),PAR(:)
C
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential')
       OPEN(3,FILE='fort.3',STATUS='unknown',ACCESS='sequential')
       OPEN(8,FILE='fort.8',STATUS='unknown',ACCESS='sequential')
C
        CALL INIT(NDIM,IPS,NTST,NCOL,ISW,NPAR)
        ALLOCATE(U(NDIM),PAR(NPAR))
        PAR(:)=0d0
C
        NOLD=0
        DO
          NOLD=NOLD+1
          READ(3,*,END=2)TEMP,(TEMP,I=1,NDIM)
        ENDDO
C
 2      NOLD=NOLD-2
        ALLOCATE(TM(0:NTST),TMR(0:NOLD))
        ALLOCATE(UPSR(NDIM,0:NOLD),VPSR(NDIM,0:NOLD))
        ALLOCATE(UPS(NDIM,0:NTST*NCOL),VPS(NDIM,0:NTST*NCOL))
C
        REWIND 3
        DO J=0,NOLD
          READ(3,*)TMR(J),UPSR(1:NDIM,J)
        ENDDO
        VPSR(:,:)=0d0
C
        PERIOD=TMR(NOLD)-TMR(0)
        DO I=NOLD,0,-1
          TMR(I)=(TMR(I)-TMR(0))/PERIOD
        ENDDO
        CALL ADAPT(NOLD,1,NDIM,NTST,NCOL,NDIM,TMR,UPSR,VPSR,TM,UPS,VPS,
     &       (IPS.EQ.2 .AND. ABS(ISW).LE.1))
C
        ICP(1)=1
        RLDOT(1)=1.d0
        PAR(11)=PERIOD
        CALL STPNT(NDIM,U,PAR,T)
        CALL WRTBV8(NDIM,NTST,NCOL,ISW,PAR,ICP,RLDOT,UPS,VPS,
     *       TM,NPAR)
C
        DEALLOCATE(U,TM,TMR,UPS,UPSR,VPS,VPSR)
C
      STOP
      CONTAINS
C
C     ---------- ----
      SUBROUTINE INIT(NDIM,IPS,NTST,NCOL,ISW,NPAR)
C
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: NDIM,IPS,NTST,NCOL,ISW,NPAR
      CHARACTER(LEN=20) STR
C
C Reads the file of continuation constants
C
      READ(2,'(A)') STR
      STR=ADJUSTL(STR)
      IF (STR(1:4)=='NPAR') THEN
         READ(STR(SCAN(STR,'=')+1:),*)NPAR
      ELSE
         NPAR=NPARX
         BACKSPACE 2
      ENDIF
      READ(2,*,END=1) NDIM,IPS
      READ(2,*,END=1) 
      READ(2,*,END=1) NTST,NCOL,ISW,ISW,ISW
C
      RETURN
C
 1    PRINT*,' Error : End of file encountered in AUTO constants-file'
      RETURN
      END SUBROUTINE INIT

C     ---------- -----
      SUBROUTINE ADAPT(NOLD,NCOLD,NDOLD,NNEW,NCNEW,NDIM,
     &     TMR,UPSR,VPSR,TM,UPS,VPS,IPER)

C Adapts the distribution of the mesh points so that the increase of the
C monotone function EQDF becomes approximately equidistributed over the
C intervals. The functions UPS and VPS are interpolated on new mesh.

      INTEGER, INTENT(IN) :: NOLD,NCOLD,NDOLD,NNEW,NCNEW,NDIM
      LOGICAL, INTENT(IN) :: IPER
      DOUBLE PRECISION, INTENT(IN) :: UPSR(NDOLD,0:*),VPSR(NDOLD,0:*),
     &     TMR(0:NOLD)
      DOUBLE PRECISION, INTENT(OUT) :: UPS(NDIM,0:*),VPS(NDIM,0:*),
     &     TM(0:*)
C Local
       INTEGER J
       DOUBLE PRECISION, ALLOCATABLE :: DTM(:)

       IF(NOLD==NNEW.AND.NCOLD==NCNEW)THEN
C Just copy
         DO J=0,NCNEW*NNEW
            UPS(1:NDIM,J)=UPSR(1:NDIM,J)
            VPS(1:NDIM,J)=VPSR(1:NDIM,J)
         ENDDO
         TM(0:NNEW)=TMR(0:NNEW)
         RETURN
       ENDIF

C Generate the new mesh :

       ALLOCATE(DTM(NOLD))
       DTM(:)=TMR(1:NOLD)-TMR(0:NOLD-1)
       CALL NEWMSH(NDIM,NDOLD,UPSR,NOLD,NCOLD,TMR,DTM,NNEW,TM,IPER)
       DEALLOCATE(DTM)

C Replace UPS by its interpolant on the new mesh :

       CALL INTERP(NDIM,NOLD,NCOLD,TMR,UPSR,NDOLD,NNEW,NCNEW,TM,UPS)

C Replace VPS by its interpolant on the new mesh :

       CALL INTERP(NDIM,NOLD,NCOLD,TMR,VPSR,NDOLD,NNEW,NCNEW,TM,VPS)

      END SUBROUTINE ADAPT

C     ---------- ------
      SUBROUTINE INTERP(NDIM,N,NC,TM,UPS,NDOLD,N1,NC1,TM1,UPS1)

C Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.

      INTEGER, INTENT(IN) :: NDIM,N,NC,N1,NC1,NDOLD
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:N*NC)
      DOUBLE PRECISION, INTENT(IN) :: TM(0:N),TM1(0:N1)
      DOUBLE PRECISION, INTENT(OUT) :: UPS1(NDIM,0:N1*NC1)
C Local
      INTEGER I,J,J1,K,L
      DOUBLE PRECISION X(0:NC),W(0:NC),Z,D

       J=1
       DO J1=0,N1-1
         DO I=0,NC1-1
           D=DBLE(I)/NC1
           Z=TM1(J1)+D*( TM1(J1+1)-TM1(J1) )
           DO
             IF(J>N)EXIT
             IF(TM(J)>Z)EXIT
             J=J+1
           ENDDO
           J=J-1
           D=( TM(J+1)-TM(J) )/NC
           DO L=0,NC
             X(L)=TM(J)+L*D
           ENDDO
           CALL INTWTS(NC,Z,X,W)
           DO K=1,NDIM
             UPS1(K,J1*NC1+I)=DOT_PRODUCT(W(:),UPS(K,J*NC:J*NC+NC))
           ENDDO
         ENDDO
       ENDDO

       DO I=1,NDIM
         UPS1(I,N1*NC1)=UPS(I,N*NC)
       ENDDO

      END SUBROUTINE INTERP

C     ---------- ------
      SUBROUTINE NEWMSH(NDIM,NDOLD,UPS,NOLD,NCOLD,TMOLD,DTMOLD,NNEW,
     &     TMNEW,IPER)

C Redistributes the mesh according to the function EQDF.

      INTEGER, INTENT(IN) :: NDIM,NDOLD,NOLD,NCOLD,NNEW
      LOGICAL, INTENT(IN) :: IPER
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:*),TMOLD(0:NOLD),
     &     DTMOLD(NOLD)
      DOUBLE PRECISION, INTENT(OUT) :: TMNEW(0:NNEW)
C Local
      INTEGER J,J1
      DOUBLE PRECISION X,DAL,UNEQ
      DOUBLE PRECISION, ALLOCATABLE :: EQF(:)
      ALLOCATE(EQF(0:NOLD))

C Put the values of the monotonely increasing function EQDF in EQF.

       CALL EQDF(NOLD,NDIM,NCOLD,DTMOLD,UPS,NDOLD,EQF,IPER)

C Uniformly divide the range of EQDF :

       DAL=EQF(NOLD)/NNEW

C Generate the new mesh in TMNEW :

       J=1
       DO J1=0,NNEW-1

C EQF is an ascending array with values in [0,1]. Get the
C value of the index of the TM-interval in which EQF(i) lies.
        UNEQ=J1*DAL
        DO
          IF(J>NOLD)EXIT
          IF(EQF(J)>UNEQ)EXIT
          J=J+1
        ENDDO
        J=J-1
        X=(UNEQ-EQF(J))/(EQF(J+1)-EQF(J))
        TMNEW(J1)=(1.d0-X)*TMOLD(J)+X*TMOLD(J+1)
      ENDDO

C Assign TMNEW(NNEW) explicitly because of loss of precision
C problems when EQF(NOLD) and EQF(NOLD-1) are very close

      TMNEW(NNEW)=TMOLD(NOLD)

      DEALLOCATE(EQF)
      END SUBROUTINE NEWMSH

C     ---------- ------
      SUBROUTINE INTWTS(N,Z,X,WTS)

C Generates weights for Lagrange interpolation.

      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: Z, X(0:N)
      DOUBLE PRECISION, INTENT(OUT) :: WTS(0:N)

      INTEGER IB,K
      DOUBLE PRECISION P

       DO IB=0,N
         P=1.d0
         DO K=0,N
           IF(K/=IB)THEN
             P=P*( Z-X(K) )/( X(IB)-X(K) )
           ENDIF
         ENDDO
         WTS(IB)=P
       ENDDO

      END SUBROUTINE INTWTS

C     ---------- ----
      SUBROUTINE EQDF(NTST,NDIM,NCOL,DTM,UPS,NDOLD,EQF,IPER)

      DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

      INTEGER, INTENT(IN) :: NTST,NDIM,NCOL,NDOLD
      LOGICAL, INTENT(IN) :: IPER
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:NTST*NCOL),DTM(NTST)
      DOUBLE PRECISION, INTENT(OUT) :: EQF(0:NTST)

C Local
      DOUBLE PRECISION SC,E,PWR,DTAV,ND
      DOUBLE PRECISION, ALLOCATABLE :: HD(:)
      LOGICAL SMALL
      INTEGER I,J,WH(0:NCOL)

       ALLOCATE(HD(NDIM))
       CALL CNTDIF(NCOL,WH)

C Compute approximation to NCOL-th and (NCOL+1)-st derivative
C and define the equidistribution function :

       SMALL=.TRUE.
       PWR=1.d0/(NCOL+1.d0)
       EQF(0)=0.d0
       DO J=1,NTST
         E=0.d0
         SC=(NCOL/DTM(J))**NCOL
         DO I=1,NDIM
           ND=SC*DOT_PRODUCT(WH(:),UPS(I,(J-1)*NCOL:J*NCOL))
           IF(J>1)THEN
             E=E+ABS( 2*( ND-HD(I) )/(DTM(J-1)+DTM(J)) )**PWR
           ENDIF
           IF(ABS(ND)>HMACH)SMALL=.FALSE.
           HD(I)=ND
         ENDDO
         IF(J>1)EQF(J-1)=EQF(J-2)+DTM(J-1)*E
       ENDDO

       E=0.d0
       IF(IPER)THEN
C      *Extend by periodicity :
         J=1
         DTAV=(DTM(NTST)+DTM(1))/2
       ELSE
C      *Extend by extrapolation :
         J=NTST-1
         DTAV=DTM(NTST)
       ENDIF
       SC=(NCOL/DTM(J))**NCOL
       DO I=1,NDIM
         ND=SC*DOT_PRODUCT(WH(:),UPS(I,(J-1)*NCOL:J*NCOL))
         E=E+ABS( (ND-HD(I))/DTAV )**PWR
         IF(ABS(ND)>HMACH)SMALL=.FALSE.
       ENDDO
       EQF(NTST)=EQF(NTST-1)+DTM(NTST)*E

C Take care of "small derivative" case.

       IF(SMALL)THEN
         DO I=0,NTST
            EQF(I)=I
         ENDDO
       ENDIF

       DEALLOCATE(HD)
      END SUBROUTINE EQDF

C     ---------- ------
      SUBROUTINE CNTDIF(N,D)

C Generates the coefficients of the central difference formula for
C Nth derivative at uniformly spaced points
      INTEGER, INTENT(IN) :: N
      INTEGER, INTENT(OUT) :: D(0:N)

      INTEGER I,K

       D(0)=1
       IF(N.EQ.0)RETURN

       DO I=1,N
         D(I)=0
         DO K=I,1,-1
           D(K)=D(K-1)-D(K)
         ENDDO
         D(0)=-D(0)
       ENDDO

      END SUBROUTINE CNTDIF

C     ---------- ------
      SUBROUTINE WRTBV8(NDIM,NTST,NCOL,ISW,PAR,ICP,RLDOT,UPS,UDOTPS,
     &     TM,NPAR)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NDIM,NTST,NCOL,ISW,ICP(*),NPAR
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:*),UDOTPS(NDIM,0:*),
     &     TM(0:*),PAR(*),RLDOT(*)

      INTEGER IBR,ITP,NFPR,NTOT,LAB,NTPL,NAR,NRD,NROWPR,MTOT

       ITP=9
       NFPR=1
       IBR=1
       NTOT=1
       LAB=1

C Write information identifying the solution :

       NTPL=NCOL*NTST+1
       NAR=NDIM+1
       NRD=(NDIM+7)/7+(NDIM+6)/7
       NROWPR=NRD*(NCOL*NTST+1) + (NFPR+6)/7 + (NPAR+6)/7 + (NFPR+19)/20
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,
     &      NTST,NCOL,NPAR

C Write the entire solution on unit 8 :

       DO J=0,NTST*NCOL
         T=TM(J/NCOL)+MOD(J,NCOL)*(TM(J/NCOL+1)-TM(J/NCOL))/NCOL
         WRITE(8,102)T,UPS(:,J)
       ENDDO

C Write the free parameter indices:

       WRITE(8,103)(ICP(I),I=1,NFPR)

C Write the direction of the branch:

       WRITE(8,102)(RLDOT(I),I=1,NFPR)
       DO J=0,NTST*NCOL
         WRITE(8,102)UDOTPS(:,J)
       ENDDO

C Write the parameter values.

       WRITE(8,102)(PAR(I),I=1,NPAR)

 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,7ES19.10)
 103   FORMAT(20I5)

      END SUBROUTINE WRTBV8

      END PROGRAM AUTCON

C
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
C Dummy routine (in case GETP is called in the user routine PVLS)
      GETP=0.0d0
      END FUNCTION GETP
