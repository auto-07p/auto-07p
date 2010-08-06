!     ------- ----
      PROGRAM AUTO

      USE AUTOMPI
      USE IO
      USE SUPPORT, ONLY:AP=>AV, NAMEIDX
      USE AUTO_CONSTANTS,ONLY: ICU,parnames,AUTOPARAMETERS
!$    USE OMP_LIB
      USE COMPAT

      IMPLICIT NONE

      LOGICAL EOF,KEYS
! Local
      DOUBLE PRECISION TIME0,TIME1,TOTTIM
      INTEGER I,LINE,ios
      INTEGER,ALLOCATABLE :: IICU(:)
      LOGICAL FIRST

! Initialization :

       CALL MPIINI()
       IF(MPIIAM()/=0)THEN
         CALL MPIWORKER(AP)
         STOP
       ENDIF

       FIRST=.TRUE.
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential',IOSTAT=ios)
       IF(ios/=0)THEN
          WRITE(6,'(A,A)')'The constants file (fort.2 or c. file) ', &
               'could not be found.'
          STOP
       ENDIF

       KEYS=.FALSE.
       LINE=0
       DO
          IF(MPIKWT()>1)THEN
             CALL MPITIM(TIME0)
          ELSE
             TIME0=AUTIM()
!$           TIME0=omp_get_wtime()
          ENDIF
          CALL INIT(AP,EOF,KEYS,LINE)
          IF(EOF)EXIT
          CALL FINDLB_OR_STOP(AP)
          CALL MPIIAP(AP)
          ALLOCATE(IICU(SIZE(ICU)))
          DO I=1,SIZE(ICU)
             IICU(I)=NAMEIDX(ICU(I),parnames)
          ENDDO
          CALL AUTOI(AP,IICU)
          DEALLOCATE(IICU)
!-----------------------------------------------------------------------

          IF(MPIKWT()>1)THEN
             CALL MPITIM(TIME1)
          ELSE
             TIME1=AUTIM()
!$           TIME1=omp_get_wtime()
          ENDIF
          TOTTIM=TIME1-TIME0
          IF(AP%IID>0)THEN
             CALL WRBAR("=",47)
             WRITE(9,301)TOTTIM
          ENDIF
          WRITE(6,301)TOTTIM
          CALL CLEANUP()
          IF(KEYS)EXIT
       ENDDO
       CALL MPIEND()

 301  FORMAT(/,' Total Time ',E12.3)

      CONTAINS

!     ---------- ---------
      SUBROUTINE MPIWORKER(AP)
      
      USE AUTOMPI
      IMPLICIT NONE

      TYPE(AUTOPARAMETERS) AP
      INTEGER, ALLOCATABLE :: ICU(:)

      DO WHILE(.TRUE.)
         CALL MPIBCASTAP(AP)
         ALLOCATE(ICU(AP%NICP))
         CALL AUTOI(AP,ICU)
         DEALLOCATE(ICU)
         ! autoi eventually calls autobv with the subroutines based on
         ! ap, which eventually calls solvbv;
         ! a return means another init message
      ENDDO
      END SUBROUTINE MPIWORKER

!     ---------- --------------
      SUBROUTINE FINDLB_OR_STOP(AP)

! Find restart label and determine type of restart point.
! or stop otherwise

      USE AUTO_CONSTANTS, ONLY: SIRS
      IMPLICIT NONE
      TYPE(AUTOPARAMETERS) AP

      INTEGER NFPR,NPARR,IRS
      LOGICAL FOUND

      IRS=AP%IRS

      FOUND=.FALSE.
      IF(IRS/=0) THEN
         CALL FINDLB(AP,IRS,NFPR,NPARR,FOUND)
         AP%IRS=IRS
         AP%NFPR=NFPR
         IF(.NOT.FOUND) THEN
            WRITE(6,"(' Restart label ',A,' not found')")TRIM(SIRS)
            STOP
         ENDIF
         AP%NPAR=MAX(NPARR,AP%NPAR)
      ENDIF
      END SUBROUTINE FINDLB_OR_STOP

!     ---------- -----
      SUBROUTINE AUTOI(AP,ICU)

      USE TOOLBOXAE
      USE TOOLBOXBV
      USE EQUILIBRIUM
      USE MAPS
      USE OPTIMIZATION
      USE PARABOLIC
      USE PERIODIC
      USE HOMCONT
      USE TIMEINT
      USE AUTO_CONSTANTS, ONLY: NBC,NINT,NDIM

      IMPLICIT NONE
      TYPE(AUTOPARAMETERS) AP
      INTEGER ICU(AP%NICP)

      INTEGER IPS,ISW,NNICP,NPAR
      INTEGER, ALLOCATABLE :: ICP(:)

      IPS=AP%IPS
      ISW=AP%ISW

      ! transfer ICU array on MPI so the AUTO** subroutines can do their
      ! work normally on the workers.
      CALL MPIBCASTI(ICU,AP%NICP)
      NNICP=MAX(5*(NBC+NINT-NDIM+1)+NDIM+NINT+3,5*SIZE(ICU)+NDIM+3)
      ALLOCATE(ICP(NNICP))
      ICP(:SIZE(ICU))=ICU(:)
      ICP(SIZE(ICU)+1:)=0
      NPAR=AP%NPAR
      NPAR=MAX(MAXVAL(ABS(ICU)),NPAR)
      AP%NPAR=NPAR
      CALL INIT1(AP)

      SELECT CASE(IPS)
      CASE(0,1)
         ! equilibria
         CALL AUTOEQ(AP,ICP,ICU)
      CASE(-1)
         ! fixed points in maps
         CALL AUTODS(AP,ICP,ICU)
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
!        ** Error in INIT.
         WRITE(6,500)
         STOP
      ENDIF

! Error Message.
 500  FORMAT(' Initialization Error')

      DEALLOCATE(ICP)

      END SUBROUTINE AUTOI
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Initialization
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!     ---------- ----
      SUBROUTINE INIT(AP,EOF,KEYS,LINE)

      USE AUTO_CONSTANTS
      USE HOMCONT, ONLY : INSTRHO

      IMPLICIT NONE

      TYPE(AUTOPARAMETERS), INTENT(OUT) :: AP
      LOGICAL, INTENT(OUT) :: EOF
      LOGICAL, INTENT(INOUT) :: KEYS
      INTEGER, INTENT(INOUT) :: LINE

      INTEGER NPOS, IERR, KEYEND, POS, LISTLEN
      CHARACTER(LEN=2048) :: STR

!     set default values
      NDIM = 2
      IPS  = 1
      ILP  = 1
      NTST = 20
      NCOL = 4
      IAD  = 3
      IADS = 1
      ISP  = 2
      ISW  = 1
      IPLT = 0
      NBC  = 0
      NINT = 0
      NMX  = 99999
      NPR  = 99999
      MXBF = 10
      IID  = 2
      ITMX = 9
      ITNW = 5
      NWTN = 3
      JAC  = 0
      NPAR = NPARX

      DS    = 0.01d0
      DSMIN = 0.005d0
      DSMAX = 0.1d0
      RL0   = -HUGE(1d0)*0.99995d0 !avoid rounding up in sthd
      RL1   = HUGE(1d0)*0.99995d0
      A0    = -HUGE(1d0)*0.99995d0
      A1    = HUGE(1d0)*0.99995d0
      EPSL  = 1d-7
      EPSU  = 1d-7
      EPSS  = 1d-5

      EFILE=''
      SFILE=''
      SVFILE=''
      DATFILE=''

      ALLOCATE(ICU(1),IVUZR(0),IVTHU(0),parnames(0),unames(0),SP(0))
      ALLOCATE(STOPS(0),UVALS(0),PARVALS(0))
      ICU(1)='1'

      NPOS=1
      DO
         CALL READC(EOF,LINE,NPOS,STR,KEYEND,POS,LISTLEN,IERR)
         ! IERR=-1: old-style constants file detected and read
         ! IERR= 0: no problems
         ! IERR= 1: unknown AUTO constant: check with HomCont
         ! IERR= 3: bad value
         IF(EOF.OR.IERR==-1)EXIT
         KEYS=.TRUE.
         IF(IERR==1)THEN
            CALL INSTRHO(STR(1:KEYEND),STR(POS:),LISTLEN,IERR)
         ENDIF
         IF(IERR==1)THEN
            WRITE(6,'(A,A,A,I2)')"Unknown AUTO constant ", &
                 STR(1:KEYEND)," on line ",LINE
            STOP
         ELSEIF(IERR==3)THEN
            WRITE(6,"(A,I2,A)") &
                 " Error in fort.2 or c. file: bad value on line ", LINE,"."
            STOP
         ENDIF
      ENDDO

      IF(EOF.AND.IERR/=-1.AND..NOT.KEYS)THEN
         RETURN
      ENDIF

      AP%NDIM=NDIM
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
      AP%NUZR=0
      DO I=1,SIZE(IVUZR)
         AP%NUZR=AP%NUZR+SIZE(IVUZR(I)%VAR)
      ENDDO
      AP%NPR=NPR
      AP%MXBF=MXBF
      AP%IID=IID
      AP%ITMX=ITMX
      AP%ITNW=ITNW
      AP%NWTN=NWTN      
      AP%JAC=JAC
      AP%NPAR=NPAR
      AP%NICP=SIZE(ICU)

      AP%NDM=NDIM
      AP%NPARI=0
      AP%ITDS=1
      AP%ITP=0
      AP%ITPST=0
      AP%NFPR=1
      AP%IBR=1
      AP%NTOT=0
      AP%NINS=0
      AP%LAB=0

      AP%DS=DS
      AP%DSMIN=ABS(DSMIN)
      AP%DSMAX=ABS(DSMAX)
      AP%RDS=DS
      AP%RL0=RL0
      AP%RL1=RL1
      AP%A0=A0
      AP%A1=A1

      AP%EPSL=EPSL
      AP%EPSU=EPSU
      AP%EPSS=EPSS
      AP%DET=0.d0
      AP%FLDF=0.d0
      AP%HBFF=0.d0
      AP%BIFF=0.d0
      AP%SPBF=0.d0

      EOF=.FALSE.

      END SUBROUTINE INIT

!     ---------- -------
      SUBROUTINE CLEANUP()

!     Deallocate some globally allocated arrays.

      USE AUTO_CONSTANTS, ONLY : IVTHU,IVUZR,IVTHL,ICU,parnames,unames, &
           SP,STOPS,PARVALS,UVALS

      IMPLICIT NONE

      DO I=1,SIZE(IVUZR)
         DEALLOCATE(IVUZR(I)%VAR)
      ENDDO
      DEALLOCATE(IVTHU,IVUZR,IVTHL,ICU,parnames,unames,SP,STOPS, &
           PARVALS,UVALS)
      END SUBROUTINE CLEANUP

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!               The leading subroutines of AUTO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!     ---------- -----
      SUBROUTINE INIT1(AP)

      USE AUTO_CONSTANTS, ONLY:IVTHL

      DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

! General initialization. Redefinition of constants.
! The following constants are redefined, ie. they are different than in
! fort.2 or c.*:

!   DS: if DS is set to 0 it'll be set to 0.1
!   DS: if DSMIN is set to 0 it'll be set to 1.0d-4 * |DS|
!   DSMIN is divided by 1+HMACH
!   DS and DSMAX are multiplied by 1+HMACH

!   NDIM: set to the dimension of the extended system
!   ILP: set to 0 dependent on problem type
!   ISP: set to 0 dependent on problem type
!   ISW: set to 1 if equal to 0, to -|ISW| for starts of ext systems
!   NBC: set by problem type
!   NINT: set by problem type
!   NMX: set to 5 for starts of extended systems

      TYPE(AUTOPARAMETERS) AP

! Local
      DOUBLE PRECISION DS,DSMIN,FC

       DS=AP%DS
       DSMIN=AP%DSMIN

       IF(AP%ISW.EQ.0)AP%ISW=1

! Check and perturb pseudo arclength stepsize and steplimits.
! (Perturbed to avoid exact computation of certain singular points).

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

      RETURN
      END SUBROUTINE INIT1

      END PROGRAM AUTO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
