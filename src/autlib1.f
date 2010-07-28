C     ------- ----
      PROGRAM AUTO
C
      USE AUTOMPI
      USE IO
      USE SUPPORT, ONLY:AP=>AV, CHECKSP, NAMEIDX
      USE AUTO_CONSTANTS,ONLY:SVFILE,SFILE,DATFILE,EFILE,
     *     ICU,parnames,AUTOPARAMETERS
C$    USE OMP_LIB
      USE COMPAT
C
      IMPLICIT NONE
C
      LOGICAL EOF,KEYS
C Local
      DOUBLE PRECISION TIME0,TIME1,TOTTIM
      INTEGER I,LINE,ios
      INTEGER,ALLOCATABLE :: IICU(:)
      LOGICAL FIRST
C
C Initialization :
C
       CALL MPIINI()
       IF(MPIIAM()/=0)THEN
         CALL MPIWORKER(AP)
         STOP
       ENDIF
C
       FIRST=.TRUE.
       EFILE=''
       SFILE=''
       SVFILE=''
       DATFILE=''
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential',IOSTAT=ios)
       IF(ios/=0)THEN
          WRITE(6,'(A,A)')'The constants file (fort.2 or c. file) ',
     *         'could not be found.'
          STOP
       ENDIF
C
       KEYS=.FALSE.
       LINE=0
 1     IF(MPIKWT()>1)THEN
         CALL MPITIM(TIME0)
       ELSE
         TIME0=AUTIM()
C$       TIME0=omp_get_wtime()
       ENDIF
       CALL INIT(AP,EOF,KEYS,LINE)
       IF(EOF)THEN
         CALL MPIEND()
         STOP
       ENDIF
       CALL FINDLB_OR_STOP(AP)
       CALL MPIIAP(AP)
       ALLOCATE(IICU(SIZE(ICU)))
       DO I=1,SIZE(ICU)
          IICU(I)=NAMEIDX(ICU(I),parnames)
       ENDDO
       CALL AUTOI(AP,IICU,SIZE(IICU),.FALSE.)
       DEALLOCATE(IICU)
C-----------------------------------------------------------------------
C
      IF(MPIKWT()>1)THEN
        CALL MPITIM(TIME1)
      ELSE
        TIME1=AUTIM()
C$      TIME1=omp_get_wtime()
      ENDIF
      TOTTIM=TIME1-TIME0
      IF(AP%IID>0)THEN
         CALL WRBAR("=",47)
         WRITE(9,301)TOTTIM
      ENDIF
      WRITE(6,301)TOTTIM
      CALL CLEANUP()
      GOTO 1
C
 301  FORMAT(/,' Total Time ',E12.3)
C
      CONTAINS
C
C     ---------- ---------
      SUBROUTINE MPIWORKER(AP)
      
      USE AUTOMPI
      IMPLICIT NONE

      TYPE(AUTOPARAMETERS) AP
      INTEGER ICU(1),IPS,IRS,ISW

      INTEGER FUNI_ICNI_PARAMS(5)

      DO WHILE(.TRUE.)
         CALL MPIBCASTI(FUNI_ICNI_PARAMS,5)
         ! figure out what funi and icni are from
         ! the iap array. We do it here, since I
         ! don't know how to pass function pointers
         ! through MPI in a possibly heterogeneous 
         ! environment :-)
         IPS     = FUNI_ICNI_PARAMS(1)
         AP%IPS  = IPS
         IRS     = FUNI_ICNI_PARAMS(2)
         AP%IRS  = IRS
         ISW     = FUNI_ICNI_PARAMS(3)
         AP%ISW = ISW
         AP%ITP = FUNI_ICNI_PARAMS(4) ! itp
         AP%NFPR = FUNI_ICNI_PARAMS(5) ! nfpr
         ICU(1) = 0
         CALL AUTOI(AP,ICU,SIZE(ICU),.TRUE.)
         ! autoi calls autobv which eventually calls solvbv;
         ! a return means another init message
      ENDDO
      END SUBROUTINE MPIWORKER
C
C     ---------- --------------
      SUBROUTINE FINDLB_OR_STOP(AP)
C
C Find restart label and determine type of restart point.
C or stop otherwise
C
      USE AUTO_CONSTANTS, ONLY: SIRS
      IMPLICIT NONE
      TYPE(AUTOPARAMETERS) AP
      CHARACTER(258) FILE

      INTEGER NFPR,NPARR,IRS
      LOGICAL FOUND

      IRS=AP%IRS

      FOUND=.FALSE.
      IF(IRS/=0) THEN
         IF(LEN_TRIM(SFILE)==0)THEN
            FILE='fort.3'
         ELSE
            FILE='s.'//SFILE
         ENDIF
         CALL FINDLB(FILE,AP,IRS,NFPR,NPARR,FOUND)
         AP%IRS=IRS
         AP%NFPR=NFPR
         IF(.NOT.FOUND) THEN
            WRITE(6,"(' Restart label ',A,' not found')")TRIM(SIRS)
            STOP
         ENDIF
         AP%NPAR=MAX(NPARR,AP%NPAR)
      ENDIF
      END SUBROUTINE FINDLB_OR_STOP
C
C     ---------- -----
      SUBROUTINE AUTOI(AP,ICU,NICU,WORKER)
C
      USE BVPCONT
      USE EQUILIBRIUM
      USE OPTIMIZATION
      USE PARABOLIC
      USE PERIODIC
      USE HOMCONT
      USE TIMEINT
      USE AUTO_CONSTANTS, ONLY: NBC,NINT,NDIM
C
      IMPLICIT NONE
      TYPE(AUTOPARAMETERS) AP
      INTEGER NICU,ICU(NICU)
      LOGICAL WORKER

      INTEGER IPS,ISW,NNICP,NPAR
      INTEGER, ALLOCATABLE :: ICP(:)

      IPS=AP%IPS
      ISW=AP%ISW
C
      IF(.NOT.WORKER)THEN
        NNICP=MAX(5*(NBC+NINT-NDIM+1)+NDIM+NINT+3,5*SIZE(ICU)+NDIM+3)
        ALLOCATE(ICP(NNICP))
        ICP(:SIZE(ICU))=ICU(:)
        ICP(SIZE(ICU)+1:)=0
        NPAR=AP%NPAR
        NPAR=MAX(MAXVAL(ABS(ICU)),NPAR)
        AP%NPAR=NPAR
        CALL INIT1(AP)
      ELSE
        ! ignored for MPI workers
        ALLOCATE(ICP(1))
      ENDIF

      SELECT CASE(IPS)
      CASE(-1,0,1)
         ! equilibria
         CALL AUTOEQ(AP,ICP,ICU)
      CASE(2,4,7)
         ! periodic solutions and general BVPs
         IF(IPS==2.OR.(IPS==7.AND.ABS(ISW)<=1))THEN
            CALL AUTOPS(AP,ICP,ICU)
         ELSE
            CALL AUTOBVP(AP,ICP,ICU)
         ENDIF 
      CASE(-2)
         ! time integration
         CALL AUTOTI(AP,ICP,ICU)
      CASE(11,12,14,16,17)
         ! parabolic PDEs
         CALL AUTOPE(AP,ICP,ICU)
      CASE(5,15)
         ! optimization
         CALL AUTOOP(AP,ICP,ICU)
      CASE(9)
         ! Homoclinic bifurcation analysis.
         CALL AUTOHO(AP,ICP,ICU)
      END SELECT

      IF(AP%NTOT==0.AND.MPIIAM()==0)THEN
C        ** Error in INIT.
         WRITE(6,500)
         STOP
      ENDIF
C
C Error Message.
 500  FORMAT(' Initialization Error')
C
      DEALLOCATE(ICP)

      END SUBROUTINE AUTOI
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                    Initialization
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE INIT(AP,EOF,KEYS,LINE)
C
      USE AUTO_CONSTANTS
      USE HOMCONT, ONLY : INSTRHO
C
      IMPLICIT NONE
C
C Reads the file of continuation constants
C
      TYPE(AUTOPARAMETERS), INTENT(OUT) :: AP
      LOGICAL, INTENT(OUT) :: EOF
      LOGICAL, INTENT(INOUT) :: KEYS
      INTEGER, INTENT(INOUT) :: LINE
C
      INTEGER IAP(23)
      DOUBLE PRECISION RAP(13)
      INTEGER IBR,I,J,NFPR,NDM
      INTEGER NINS,LAB,NTOT,ITP,ITPST,NUZR,NICP,NPARI,ITDS
      DOUBLE PRECISION BIFF,DET,SPBF,HBFF,FLDF
      CHARACTER(LEN=2048) :: STR
      CHARACTER(LEN=1) :: C,QUOTE,PREV
      LOGICAL QUOTEESC
      INTEGER KEYEND,POS,LISTLEN,NPOS,LISTLEN2,IERR,ios

      TYPE INDEXSTRL
         CHARACTER(13) INDEX
         CHARACTER(2048) STRL
      END TYPE INDEXSTRL
      TYPE(INDEXSTRL),ALLOCATABLE :: IVUZRS(:)

      CHARACTER(LEN=*), PARAMETER :: ICONSTANTS(23) = (/
     * "NDIM", "IPS ", "    ", "ILP ", "NTST", "NCOL", "IAD ", "IADS",
     * "ISP ", "ISW ", "IPLT", "NBC ", "NINT", "NMX ", "    ", "NPR ",
     * "MXBF", "IID ", "ITMX", "ITNW", "NWTN", "JAC ", "NPAR" /)
      INTEGER, PARAMETER :: IDEFAULTS(23) = (/
     * 2, 1, 0, 1, 20, 4, 3, 1,
     * 2, 1, 0, 0, 0, 99999, 0, 99999,
     * 10, 2, 9, 5, 3, 0, NPARX /)
      CHARACTER(LEN=*), PARAMETER :: RCONSTANTS(13) = (/
     * "DS   ", "DSMIN", "DSMAX", "     ", "     ", "RL0  ", "RL1  ",
     * "A0   ", "A1   ", "     ", "EPSL ", "EPSU ", "EPSS " /)
      DOUBLE PRECISION, PARAMETER :: RDEFAULTS(13) = (/
     * 0.01d0, 0.005d0, 0.1d0, 0d0, 0d0, -1d300, 1d300, -1d300, 1d300,
     * 0d0, 1d-7, 1d-7, 1d-5 /)
C
      IF(KEYS)THEN
         EOF=.TRUE.
         RETURN
      ENDIF
      IAP(:)=IDEFAULTS(:)
      RAP(:)=RDEFAULTS(:)
      RAP(6)=-HUGE(1d0)*0.99995d0 !avoid rounding up in sthd
      RAP(7)=HUGE(1d0)*0.99995d0
      RAP(8)=-HUGE(1d0)*0.99995d0
      RAP(9)=HUGE(1d0)*0.99995d0
      NICP=1
      ALLOCATE(ICU(1),IVUZR(0),IVTHU(0),parnames(0),unames(0),SP(0))
      ALLOCATE(STOPS(0),UVALS(0),PARVALS(0))
      ICU(1)='1'
      NUZR=0

      NPOS=1
      KEYS=.FALSE.
      EOF=.FALSE.
      scanloop: DO
         IF(NPOS==1)THEN
            LINE=LINE+1
            READ(2,'(A)',END=5) STR
            QUOTE=' '
            QUOTEESC=.FALSE.
            DO I=1,LEN_TRIM(STR)
               C=STR(I:I)
               IF(QUOTE==' ')THEN
                  ! replace a tab with a spaces if not in a string
                  IF(IACHAR(C)==9)THEN
                     STR(I:I)=' '
                  ELSEIF(C=="'".OR.C=='"')THEN
                     QUOTE=STR(I:I)
                  ENDIF
               ELSEIF(C==QUOTE)THEN
                  ! ignore "" and ''
                  IF(STR(I+1:I+1)==C.OR.QUOTEESC)THEN
                     QUOTEESC=.NOT.QUOTEESC
                  ELSE
                     QUOTE=' '
                  ENDIF
               ENDIF
            ENDDO
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
               KEYS=.TRUE.
               NEWCFILE=.TRUE.
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
         CALL SCANVALUE(STR(POS:),NPOS,LISTLEN)
         IF(NPOS/=1)THEN
            NPOS=NPOS+POS-1
         ENDIF
         DO I=1,23
            IF(STR(1:KEYEND)==TRIM(ICONSTANTS(I)))THEN
               READ(STR(POS:),*,ERR=3)IAP(I)
               CYCLE scanloop
            ENDIF
         ENDDO
         DO I=1,13
            IF(STR(1:KEYEND)==TRIM(RCONSTANTS(I)))THEN
               READ(STR(POS:),*,ERR=3)RAP(I)
               CYCLE scanloop
            ENDIF
         ENDDO
         SELECT CASE(STR(1:KEYEND))
         CASE('IRS')
            READ(STR(POS:),*,ERR=3)SIRS
            READ(SIRS,*,IOSTAT=ios)IAP(3)
            IF(ios/=0)IAP(3)=1
         CASE('ICP')
            NICP=LISTLEN
            DEALLOCATE(ICU)
            ALLOCATE(ICU(NICP))
            READ(STR(POS:),*,ERR=3)ICU            
         CASE('UZR')
            ALLOCATE(IVUZRS(LISTLEN))
            READ(STR(POS:),*,ERR=3)IVUZRS
            DO I=1,SIZE(IVUZR)
               DEALLOCATE(IVUZR(I)%VAR)
            ENDDO
            DEALLOCATE(IVUZR)
            ALLOCATE(IVUZR(LISTLEN))
            NUZR=0
            DO I=1,LISTLEN
               PREV=' '
               LISTLEN2=0
               DO J=1,LEN_TRIM(IVUZRS(I)%STRL)
                  C=IVUZRS(I)%STRL(J:J)
                  IF(C/=' '.AND.C/=','.AND.(PREV==' '.OR.PREV==','))THEN
                     LISTLEN2=LISTLEN2+1
                  ENDIF
                  PREV=C
               ENDDO
               ALLOCATE(IVUZR(I)%VAR(LISTLEN2))
               IVUZR(I)%INDEX=IVUZRS(I)%INDEX
               READ(IVUZRS(I)%STRL,*,ERR=3)IVUZR(I)%VAR
               NUZR=NUZR+LISTLEN2
            ENDDO
            DEALLOCATE(IVUZRS)
         CASE('THL')
            IF(ALLOCATED(IVTHL))DEALLOCATE(IVTHL)
            ALLOCATE(IVTHL(LISTLEN))
            READ(STR(POS:),*,ERR=3)IVTHL
         CASE('THU')
            DEALLOCATE(IVTHU)
            ALLOCATE(IVTHU(LISTLEN))
            READ(STR(POS:),*,ERR=3)IVTHU
         CASE('SP')
            IF(ALLOCATED(SP))DEALLOCATE(SP)
            ALLOCATE(SP(LISTLEN))
            READ(STR(POS:),*,ERR=3)SP
         CASE('STOP')
            IF(ALLOCATED(STOPS))DEALLOCATE(STOPS)
            ALLOCATE(STOPS(LISTLEN))
            READ(STR(POS:),*,ERR=3)STOPS
         CASE('PAR')
            IF(ALLOCATED(PARVALS))DEALLOCATE(PARVALS)
            ALLOCATE(PARVALS(LISTLEN))
            READ(STR(POS:),*,ERR=3)PARVALS
         CASE('U')
            IF(ALLOCATED(UVALS))DEALLOCATE(UVALS)
            ALLOCATE(UVALS(LISTLEN))
            READ(STR(POS:),*,ERR=3)UVALS
         CASE('parnames')
            IF(ALLOCATED(parnames))DEALLOCATE(parnames)
            ALLOCATE(parnames(LISTLEN))
            READ(STR(POS:),*,ERR=3)parnames
         CASE('unames')
            IF(ALLOCATED(unames))DEALLOCATE(unames)
            ALLOCATE(unames(LISTLEN))
            READ(STR(POS:),*,ERR=3)unames
         CASE('s')
            READ(STR(POS:),*)SFILE
         CASE('dat')
            READ(STR(POS:),*)DATFILE
         CASE('sv')
            READ(STR(POS:),*)SVFILE
         CASE('e')
            READ(STR(POS:),*)EFILE
         CASE DEFAULT
            CALL INSTRHO(STR(1:KEYEND),STR(POS:),LISTLEN,IERR)
            IF(IERR==3)GOTO 3
            IF(IERR==1)THEN
               WRITE(6,'(A,A,A,I2)')"Unknown AUTO constant ",
     &              STR(1:KEYEND)," on line ",LINE
            ENDIF
         END SELECT
      ENDDO scanloop

 1    NDIM=IAP(1)
      IPS=IAP(2)
      IRS=IAP(3)
      ILP=IAP(4)
      NTST=IAP(5)
      NCOL=IAP(6)
      IAD=IAP(7)
      IADS=IAP(8)
      ISP=IAP(9)
      ISW=IAP(10)
      IPLT=IAP(11)
      NBC=IAP(12)
      NINT=IAP(13)
      NMX=IAP(14)
      NPR=IAP(16)
      MXBF=IAP(17)
      IID=IAP(18)
      ITMX=IAP(19)
      ITNW=IAP(20)
      NWTN=IAP(21)
      JAC=IAP(22)
      NPAR=IAP(23)
C
      DS=RAP(1)
      DSMIN=RAP(2)
      DSMAX=RAP(3)
      RL0=RAP(6)
      RL1=RAP(7)
      A0=RAP(8)
      A1=RAP(9)
      EPSL=RAP(11)
      EPSU=RAP(12)
      EPSS=RAP(13)

      IF(EOF)GOTO 2 ! completely new-style, just keys
      BACKSPACE 2
      READ(2,*,ERR=3,END=4) NDIM,IPS,SIRS,ILP
      READ(SIRS,*,IOSTAT=ios)IRS
      IF(ios/=0)IRS=1
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NICP
      IF(NICP.GT.0)THEN
        DEALLOCATE(ICU)
        ALLOCATE(ICU(NICP))
        BACKSPACE 2
        READ(2,*,ERR=3,END=4) NICP,(ICU(I),I=1,NICP)
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NTST,NCOL,IAD,ISP,ISW,IPLT,NBC,NINT
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NMX,RL0,RL1,A0,A1
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) NPR,MXBF,IID,ITMX,ITNW,NWTN,JAC
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) EPSL,EPSU,EPSS
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) DS,DSMIN,DSMAX,IADS
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) LISTLEN
      !allocate: no THL vs. non-allocated:default THL (in SUB. INIT1)
      IF(ALLOCATED(IVTHL))DEALLOCATE(IVTHL)
      ALLOCATE(IVTHL(LISTLEN))
      IF(LISTLEN>0)THEN
        DO I=1,LISTLEN
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)IVTHL(I)
        ENDDO
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4) LISTLEN
      IF(LISTLEN>0)THEN
        DEALLOCATE(IVTHU)
        ALLOCATE(IVTHU(LISTLEN))
        DO I=1,LISTLEN
          LINE=LINE+1
          READ(2,*,ERR=3,END=4)IVTHU(I)
        ENDDO
      ENDIF
      LINE=LINE+1
      READ(2,*,ERR=3,END=4)NUZR
      IF(NUZR>0)THEN
        DO I=1,SIZE(IVUZR)
           DEALLOCATE(IVUZR(I)%VAR)
        ENDDO
        DEALLOCATE(IVUZR)
        ALLOCATE(IVUZR(NUZR))
        DO I=1,NUZR
          LINE=LINE+1
          ALLOCATE(IVUZR(I)%VAR(1))
          READ(2,*,ERR=3,END=4)IVUZR(I)%INDEX,IVUZR(I)%VAR(1)
        ENDDO
      ENDIF
      KEYS=.FALSE.
C
 2    AP%NDIM=NDIM
      AP%IPS=IPS
      AP%IRS=IRS
      AP%ILP=ILP
      AP%NTST=NTST
      AP%NCOL=NCOL
      AP%IAD=IAD
      AP%IADS=IADS
      AP%ISP=ISP
      AP%ISW=ISW
      AP%IPLT=IPLT
      AP%NBC=NBC
      AP%NINT=NINT
      AP%NMX=NMX
      AP%NUZR=NUZR
      AP%NPR=NPR
      AP%MXBF=MXBF
      AP%IID=IID
      AP%ITMX=ITMX
      AP%ITNW=ITNW
      AP%NWTN=NWTN      
      AP%JAC=JAC
C
      NDM=NDIM
      NPARI=0
      ITDS=1
      ITP=0
      ITPST=0
      NFPR=1
      IBR=1
      NTOT=0
      NINS=0
      LAB=0
C
      AP%NDM=NDM
      AP%NPARI=NPARI
      AP%ITDS=ITDS
      AP%ITP=ITP
      AP%ITPST=ITPST
      AP%NFPR=NFPR
      AP%IBR=IBR
      AP%NPAR=NPAR
      AP%NTOT=NTOT
      AP%NINS=NINS
      AP%LAB=LAB
      AP%NICP=NICP
C
      AP%DS=DS
      AP%DSMIN=ABS(DSMIN)
      AP%DSMAX=ABS(DSMAX)
      AP%RDS=DS
      AP%RL0=RL0
      AP%RL1=RL1
      AP%A0=A0
      AP%A1=A1
C
      DET=0.d0
      FLDF=0.d0
      HBFF=0.d0
      BIFF=0.d0
      SPBF=0.d0
C
      AP%EPSL=EPSL
      AP%EPSU=EPSU
      AP%EPSS=EPSS
      AP%DET=DET
      AP%FLDF=FLDF
      AP%HBFF=HBFF
      AP%BIFF=BIFF
      AP%SPBF=SPBF
C
      EOF=.FALSE.
      RETURN
 3    WRITE(6,"(A,I2,A)")
     *     " Error in fort.2 or c. file: bad value on line ",
     *     LINE,"."
      STOP
 4    WRITE(6,"(A,I2,A)")
     *     " Error in fort.2 or c. file: ends prematurely on line ",
     *     LINE,"."
      EOF=.TRUE.
      RETURN
 5    EOF=.TRUE.
      IF(KEYS)GOTO 1
      END SUBROUTINE INIT

C     ---------- ---------
      SUBROUTINE SCANVALUE(STR,NPOS,LISTLEN)
      IMPLICIT NONE
C
C     Scans STR(:) for a value
C     NPOS points to the next keyword on the same line,
C       or is set to 1 if there is none
C     LISTLEN gives the number of items in lists delimited by []
C     [] characters are removed
C
      CHARACTER(*), INTENT(INOUT) :: STR
      INTEGER, INTENT(OUT) :: NPOS,LISTLEN

      INTEGER I,LEVEL,LENSTR,ios
      CHARACTER(1) C,PREV,QUOTE
      LOGICAL QUOTEESC,ISDICT
      LISTLEN=1
      LEVEL=0
      QUOTE=' '
      QUOTEESC=.FALSE.
      PREV=' '

      NPOS=1
      ISDICT=.FALSE.
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
               STR(I:I)=','
            CASE(']','}')
               IF(C=='}') ISDICT=.FALSE.
               STR(I:I)=' '
               IF(LEVEL==1.AND.(PREV=='['.OR.PREV=='{'))LISTLEN=0
               LEVEL=LEVEL-1
               IF(C==']'.AND.ISDICT) STR(I:I)="'"
            CASE DEFAULT
               IF((PREV==','.OR.PREV==' ').AND.LEVEL==1)THEN
                  LISTLEN=LISTLEN+1
               ENDIF
               SELECT CASE(C)
               CASE('[','{')
                  STR(I:I)=' '
                  LEVEL=LEVEL+1
                  IF(C=='{')THEN
                     ISDICT=.TRUE.
                  ELSEIF(ISDICT)THEN
                     STR(I:I)="'"
                  ENDIF
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
      I=VERIFY(STR(NPOS:)," ,")
      IF(I==0)THEN
         NPOS=1
      ELSE
         NPOS=NPOS+I-1
         IF(NPOS>=LEN_TRIM(STR))NPOS=1
      ENDIF
      END SUBROUTINE SCANVALUE

C     ---------- -------
      SUBROUTINE CLEANUP()
C
C     Deallocate some globally allocated arrays.
C
      USE AUTO_CONSTANTS, ONLY : IVTHU,IVUZR,IVTHL,ICU,parnames,unames,
     *     SP,STOPS,PARVALS,UVALS

      IMPLICIT NONE

      DO I=1,SIZE(IVUZR)
         DEALLOCATE(IVUZR(I)%VAR)
      ENDDO
      DEALLOCATE(IVTHU,IVUZR,IVTHL,ICU,parnames,unames,SP,STOPS,
     *     PARVALS,UVALS)
      END SUBROUTINE CLEANUP
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C               The leading subroutines of AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- -----
      SUBROUTINE INIT1(AP)
C
      USE AUTO_CONSTANTS, ONLY:IVTHL
C
      DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7
C
C General initialization. Redefinition of constants.
C The following constants are redefined, ie. they are different than in
C fort.2 or c.*:

C   DS: if DS is set to 0 it'll be set to 0.1
C   DS: if DSMIN is set to 0 it'll be set to 1.0d-4 * |DS|
C   DSMIN is divided by 1+HMACH
C   DS and DSMAX are multiplied by 1+HMACH

C   NDIM: set to the dimension of the extended system
C   ILP: set to 0 dependent on problem type
C   ISP: set to 0 dependent on problem type
C   ISW: set to 1 if equal to 0, to -|ISW| for starts of ext systems
C   NBC: set by problem type
C   NINT: set by problem type
C   NMX: set to 5 for starts of extended systems

      TYPE(AUTOPARAMETERS) AP
C
C Local
      DOUBLE PRECISION DS,DSMIN,FC
C
       DS=AP%DS
       DSMIN=AP%DSMIN
C
       IF(AP%ISW.EQ.0)AP%ISW=1
C
C Check and perturb pseudo arclength stepsize and steplimits.
C (Perturbed to avoid exact computation of certain singular points).
C
       IF(DS.EQ.0.d0)DS=0.1
       IF(DSMIN.EQ.0.d0)DSMIN=1.0D-4*ABS(DS)
       FC=1.d0+HMACH
       AP%DS=FC*DS
       AP%DSMIN=DSMIN/FC
       AP%DSMAX=FC*AP%DSMAX
       AP%NPARI=0
       IF(.NOT.ALLOCATED(IVTHL))THEN
          ! set default for *THL
          IF(AP%IPS==2.OR.AP%IPS==12)THEN
             ALLOCATE(IVTHL(1))
             IVTHL(1)%INDEX='11'
             IVTHL(1)%VAR=0d0
          ELSE
             ALLOCATE(IVTHL(0))
          ENDIF
       ENDIF
C
      RETURN
      END SUBROUTINE INIT1

      END PROGRAM AUTO
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
