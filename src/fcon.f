C     ------- ------
      PROGRAM AUTCON
C
C This program converts a data file into a labeled AUTO solution
C
C
      IMPLICIT NONE
      include 'fcon.h'
C
      INTEGER NDIM,NOLD,NTST,NCOL,ISW,IPS,NPAR,I,J,ios
      DOUBLE PRECISION TEMP,T,PERIOD
      DOUBLE PRECISION, ALLOCATABLE :: U(:),TM(:),UPS(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TMR(:),UPSR(:,:),PAR(:)
C
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential',IOSTAT=ios)
       IF(ios/=0)THEN
          WRITE(6,'(A,A)')'The constants file (fort.2 or c. file) ',
     *         'could not be found.'
          STOP
       ENDIF
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
        ALLOCATE(UPSR(NDIM,0:NOLD),UPS(NDIM,0:NTST*NCOL))
C
        REWIND 3
        DO J=0,NOLD
          READ(3,*)TMR(J),UPSR(1:NDIM,J)
        ENDDO
C
        PERIOD=TMR(NOLD)-TMR(0)
        DO I=NOLD,0,-1
          TMR(I)=(TMR(I)-TMR(0))/PERIOD
        ENDDO
        CALL ADAPT(NOLD,NDIM,NTST,NCOL,NDIM,TMR,UPSR,TM,UPS,
     &       ((IPS.EQ.2.OR.IPS.EQ.12) .AND. ABS(ISW).LE.1))
C
        PAR(11)=PERIOD
        CALL STPNT(NDIM,U,PAR,T)
        CALL WRTBV8(NDIM,NTST,NCOL,ISW,PAR,UPS,TM,NPAR)
C
        DEALLOCATE(U,TM,TMR,UPS,UPSR)
C
      STOP
      CONTAINS

C     ---------- ----
      SUBROUTINE INIT(NDIM,IPS,NTST,NCOL,ISW,NPAR)
C
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: NDIM,IPS,NTST,NCOL,ISW,NPAR
      CHARACTER(LEN=2048) :: STR
C
C Reads the file of continuation constants
C
      INTEGER KEYEND,POS,NPOS
      INTEGER LINE
C
C Defaults
C
      NDIM=2
      IPS=1
      NTST=20
      NCOL=4
      ISW=1
      NPAR=NPARX

      LINE=0
      NPOS=1
      scanloop: DO
         IF(NPOS==1)THEN
            LINE=LINE+1
            READ(2,'(A)',END=1) STR
         ELSE
            STR=STR(NPOS:)
         ENDIF
         STR=ADJUSTL(STR)
         IF(LEN_TRIM(STR)==0)CYCLE
         DO I=1,LEN_TRIM(STR)
            ! comment on line
            IF(STR(I:I)=='#'.OR.STR(I:I)=='!')THEN
               NPOS=1
               CYCLE scanloop
            ENDIF
            ! keyword detected
            IF((LGE(STR(I:I),'A').AND.LLE(STR(I:I),'Z')).OR.
     &         (LGE(STR(I:I),'a').AND.LLE(STR(I:I),'z')))THEN
               STR=STR(I:)
               EXIT
            ELSE
               EXIT scanloop
            ENDIF
            IF(I==LEN_TRIM(STR))THEN
               NPOS=1
               CYCLE scanloop
            ENDIF
         ENDDO
         ! look for = after keyword
         KEYEND=SCAN(STR,'= ')-1
         IF(KEYEND==-1)THEN
            LINE=LINE-1
            EXIT scanloop
         ENDIF
         POS=SCAN(STR,'=')+1
         STR(POS:)=ADJUSTL(STR(POS:))
         NPOS=SCANVALUE(STR(POS:))
         IF(NPOS/=1)THEN
            NPOS=NPOS+POS-1
         ENDIF
         SELECT CASE(STR(1:KEYEND))
         CASE('NDIM')
            READ(STR(POS:),*,ERR=3)NDIM
         CASE('IPS')
            READ(STR(POS:),*,ERR=3)IPS
         CASE('NTST')
            READ(STR(POS:),*,ERR=3)NTST
         CASE('NCOL')
            READ(STR(POS:),*,ERR=3)NCOL
         CASE('ISW')
            READ(STR(POS:),*,ERR=3)ISW
         CASE('NPAR')
            READ(STR(POS:),*,ERR=3)NPAR
         ! ignore all others
         END SELECT
      ENDDO scanloop

      BACKSPACE 2
      READ(2,*,ERR=3,END=4) NDIM,IPS
      LINE=LINE+1
      READ(2,*,ERR=3,END=4)
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NTST,NCOL,ISW,ISW,ISW

 1    CONTINUE
      RETURN
 3    WRITE(6,"(A,I2,A)")
     *     " Error in fort.2 or c. file: bad value on line ",
     *     LINE,"."
      STOP
 4    WRITE(6,"(A,I2,A)")
     *     " Error in fort.2 or c. file: ends prematurely on line ",
     *     LINE,"."
      STOP
      END SUBROUTINE INIT

C     ------- -------- ---------
      INTEGER FUNCTION SCANVALUE(STR)
      IMPLICIT NONE
C
C     Scans STR(:) for a value
C     NPOS points to the next keyword on the same line,
C       or is set to 1 if there is none
C
      CHARACTER(*), INTENT(INOUT) :: STR

      INTEGER NPOS,I,LEVEL,LENSTR,ios
      CHARACTER(1) C,PREV,QUOTE
      LOGICAL QUOTEESC
      LEVEL=0
      QUOTE=' '
      QUOTEESC=.FALSE.
      PREV=' '

      NPOS=1
      LENSTR=LEN_TRIM(STR)
      I=1
      DO
         IF(I>LENSTR)THEN
            IF(LEVEL==0)EXIT
            LENSTR=LEN_TRIM(STR)
            READ(2,'(A)',IOSTAT=ios) STR(LENSTR+1:)
            IF(ios/=0)EXIT
            LENSTR=LEN_TRIM(STR)
         ENDIF
         NPOS=I
         C=STR(I:I)
         IF(QUOTE==' ')THEN
            SELECT CASE(C)
            CASE(',',' ')
               IF(LEVEL==0)EXIT
               IF(PREV==':')C=PREV !eat ',' and ' ' after ':'
            CASE(':')
            CASE(']','}')
               LEVEL=LEVEL-1
            CASE DEFAULT
               SELECT CASE(C)
               CASE('[','{')
                  LEVEL=LEVEL+1
               CASE('"',"'")
                  QUOTE=C
               END SELECT
            END SELECT
         ELSEIF(C==QUOTE)THEN
            ! ignore "" and ''
            IF(STR(I+1:I+1)==C.OR.QUOTEESC)THEN
               QUOTEESC=.NOT.QUOTEESC
            ELSE
               QUOTE=' '
            ENDIF
         ENDIF
         PREV=C
         I=I+1
      ENDDO
      NPOS=NPOS+VERIFY(STR(NPOS:)," ,")-1
      IF(NPOS>=LEN_TRIM(STR))NPOS=1
      SCANVALUE=NPOS
      END FUNCTION SCANVALUE
C
C     ---------- -----
      SUBROUTINE ADAPT(NOLD,NDOLD,NNEW,NCNEW,NDIM,
     &     TMR,UPSR,TM,UPS,IPER)

C Adapts the distribution of the mesh points so that the increase of the
C monotone function EQDF becomes approximately equidistributed over the
C intervals. The function UPS is interpolated on new mesh.

      INTEGER, INTENT(IN) :: NOLD,NDOLD,NNEW,NCNEW,NDIM
      LOGICAL, INTENT(IN) :: IPER
      DOUBLE PRECISION, INTENT(IN) :: UPSR(NDOLD,0:*), TMR(0:NOLD)
      DOUBLE PRECISION, INTENT(OUT) :: UPS(NDIM,0:*), TM(0:*)

C Generate the new mesh :

       CALL NEWMSH(NDIM,NDOLD,UPSR,NOLD,TMR,NNEW,TM,IPER)

C Replace UPS by its interpolant on the new mesh :

       CALL INTERP(NDIM,NOLD,TMR,UPSR,NDOLD,NNEW,NCNEW,TM,UPS)

      END SUBROUTINE ADAPT

C     ---------- ------
      SUBROUTINE INTERP(NDIM,N,TM,UPS,NDOLD,N1,NC1,TM1,UPS1)

C Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.

      INTEGER, INTENT(IN) :: NDIM,N,N1,NC1,NDOLD
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:N)
      DOUBLE PRECISION, INTENT(IN) :: TM(0:N),TM1(0:N1)
      DOUBLE PRECISION, INTENT(OUT) :: UPS1(NDIM,0:N1*NC1)
C Local
      INTEGER I,J,J1,K
      DOUBLE PRECISION W0,W1,Z

       J=1
       DO J1=0,N1-1
         DO I=0,NC1-1
           Z=TM1(J1)+I*( TM1(J1+1)-TM1(J1) )/NC1
           DO
             IF(J>N)EXIT
             IF(TM(J)>Z)EXIT
             J=J+1
           ENDDO
           J=J-1
           W0=( Z-TM(J+1) )/( TM(J)-TM(J+1) )
           W1=( Z-TM(J) )/( TM(J+1)-TM(J) )
           DO K=1,NDIM
             UPS1(K,J1*NC1+I)=W0*UPS(K,J)+W1*UPS(K,J+1)
           ENDDO
         ENDDO
       ENDDO

       DO I=1,NDIM
         UPS1(I,N1*NC1)=UPS(I,N)
       ENDDO

      END SUBROUTINE INTERP

C     ---------- ------
      SUBROUTINE NEWMSH(NDIM,NDOLD,UPS,NOLD,TMOLD,NNEW,TMNEW,IPER)

C Redistributes the mesh according to the function EQDF.

      INTEGER, INTENT(IN) :: NDIM,NDOLD,NOLD,NNEW
      LOGICAL, INTENT(IN) :: IPER
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:*),TMOLD(0:NOLD)
      DOUBLE PRECISION, INTENT(OUT) :: TMNEW(0:NNEW)
C Local
      INTEGER J,J1
      DOUBLE PRECISION X,DAL,UNEQ
      DOUBLE PRECISION, ALLOCATABLE :: EQF(:)
      ALLOCATE(EQF(0:NOLD))

C Put the values of the monotonely increasing function EQDF in EQF.

       CALL EQDF(NOLD,NDIM,TMOLD,UPS,NDOLD,EQF,IPER)

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

C     ---------- ----
      SUBROUTINE EQDF(NTST,NDIM,TM,UPS,NDOLD,EQF,IPER)

      DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

      INTEGER, INTENT(IN) :: NTST,NDIM,NDOLD
      LOGICAL, INTENT(IN) :: IPER
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:NTST),TM(0:NTST)
      DOUBLE PRECISION, INTENT(OUT) :: EQF(0:NTST)

C Local
      DOUBLE PRECISION E,DTAV,ND,DT
      DOUBLE PRECISION, ALLOCATABLE :: HD(:)
      LOGICAL SMALL
      INTEGER I,J

       ALLOCATE(HD(NDIM))

C Compute approximation to NCOL-th and (NCOL+1)-st derivative
C and define the equidistribution function :

       SMALL=.TRUE.
       EQF(0)=0.d0
       DO J=1,NTST
         E=0.d0
         DT=TM(J)-TM(J-1)
         DO I=1,NDIM
           ND=(UPS(I,J)-UPS(I,J-1))/DT
           IF(J>1)THEN
             E=E+SQRT(ABS( 2*( ND-HD(I) )/(TM(J)-TM(J-2)) ))
           ENDIF
           IF(ABS(ND)>HMACH)SMALL=.FALSE.
           HD(I)=ND
         ENDDO
         IF(J>1)EQF(J-1)=EQF(J-2)+(TM(J-1)-TM(J-2))*E
       ENDDO

       E=0.d0
       J=NTST-1
       DTAV=TM(NTST)-TM(NTST-1)
       IF(IPER)THEN
C      *Extend by periodicity :
         J=1
         DTAV=(DTAV+TM(1)-TM(0))/2
       ENDIF
C      *else extend by extrapolation :
       DO I=1,NDIM
         ND=(UPS(I,J)-UPS(I,J-1))/(TM(J)-TM(J-1))
         E=E+SQRT(ABS( (ND-HD(I))/DTAV ))
         IF(ABS(ND)>HMACH)SMALL=.FALSE.
       ENDDO
       EQF(NTST)=EQF(NTST-1)+(TM(NTST)-TM(NTST-1))*E

C Take care of "small derivative" case.

       IF(SMALL)THEN
         DO I=0,NTST
            EQF(I)=I
         ENDDO
       ENDIF

       DEALLOCATE(HD)
      END SUBROUTINE EQDF

C     ---------- ------
      SUBROUTINE WRTBV8(NDIM,NTST,NCOL,ISW,PAR,UPS,TM,NPAR)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NDIM,NTST,NCOL,ISW,NPAR
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:*),TM(0:*),PAR(*)

      INTEGER IBR,ITP,NFPR,NTOT,LAB,NTPL,NAR,NROWPR,MTOT

       ITP=9
       NFPR=1
       IBR=1
       NTOT=1
       LAB=1

C Write information identifying the solution :

       NTPL=NCOL*NTST+1
       NAR=NDIM+1
       NROWPR=((NDIM+6)/7)*NTPL + (NPAR+6)/7
       MTOT=MOD(NTOT-1,9999)+1
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,
     &      NTST,NCOL,NPAR

C Write the entire solution on unit 8 :

       DO J=0,NTST*NCOL
         T=TM(J/NCOL)+MOD(J,NCOL)*(TM(J/NCOL+1)-TM(J/NCOL))/NCOL
         WRITE(8,102)T,UPS(:,J)
       ENDDO

C Write the parameter values.

       WRITE(8,102)(PAR(I),I=1,NPAR)

 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,7ES19.10)

      END SUBROUTINE WRTBV8

      END PROGRAM AUTCON

C
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
C Dummy routine (in case GETP is called in the user routine PVLS)
      GETP=0.0d0
      END FUNCTION GETP
