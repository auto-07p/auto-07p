!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for the Continuation of general BVPs
!        (incl. BPs and Folds)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
MODULE TOOLBOXBV

  USE AUTO_CONSTANTS, ONLY : AUTOPARAMETERS
  USE INTERFACES
  USE BVP
  USE SUPPORT

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: INITBVP,AUTOBVP,STPNBV,STPNBV1,FNCSBV,FNBLF,BCBLF,ICBLF,STPNBL

  DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

CONTAINS

! ---------- -------
  SUBROUTINE INITBVP(AP,ICP)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)

    INTEGER I, ISW, ITP, NDIM, NBC, NINT, NFPR, NXP, NPAR
    ISW = AP%ISW
    ITP = AP%ITP
    NDIM = AP%NDIM
    NBC = AP%NBC
    NINT = AP%NINT
    NPAR = AP%NPAR

    ! Two-Parameter Continuation for IPS=4 or IPS=7.
    IF(ABS(ISW)==2.AND.(ITP==5.OR.(ABS(ITP)/10)==5))THEN
       ! ** Continuation of folds (BVP; start/restart)
       NDIM=2*NDIM
       NBC=2*NBC
       NINT=2*NINT+1
       NFPR=NBC+NINT-NDIM+1
       DO I=2,NFPR/2
          ICP(NFPR/2+I)=NPAR+I-1
       ENDDO
       ! PAR(NPAR+NFPR/2) contains a norm
       AP%NPARI=NFPR/2
       IF(ITP==5)THEN
          ! ** Continuation of folds (BVP; start)
          ICP(NFPR/2+1)=NPAR+NFPR/2
          AP%ISW=-2
       ENDIF
    ENDIF
    IF(ABS(ISW)>=2.AND.(ITP==6.OR.ABS(ITP)/10==6)) THEN
       ! ** BP cont (BVP, start and restart) (by F. Dercole).
       NXP=NBC+NINT-NDIM+1
       DO I=1,NXP+NDIM-1
          ICP(NXP+I+2)=NPAR+I ! psi^*_2,psi^*_3
       ENDDO
       DO I=1,NXP
          ICP(2*NXP+NDIM+I+1)=NPAR+NXP+NDIM+I ! d
       ENDDO
       IF(ITP==6)THEN
          ! ** BP cont (BVP; start)
          ICP(NXP+1)=NPAR+NXP+NDIM ! a
          ICP(NXP+2)=NPAR+2*NXP+NDIM+1 ! b
          DO I=1,NXP
             ICP(3*NXP+NDIM+I+1)=NPAR+2*NXP+NDIM+1+I ! q
             ICP(4*NXP+NDIM+I+1)=NPAR+3*NXP+NDIM+1+I ! r
          ENDDO
          ICP(5*NXP+NDIM+2)=NPAR+4*NXP+NDIM+2 ! c1
          ICP(5*NXP+NDIM+3)=NPAR+4*NXP+NDIM+3 ! c2
          AP%NPARI=4*NXP+NDIM+3
          AP%ISW=-ABS(ISW)
          NDIM=4*NDIM
          NBC=3*NBC+NDIM/2+NXP
          NINT=3*NINT+NXP+5
       ELSE
          ! ** BP cont (BVP; restart 1 or 2)
          AP%NPARI=2*NXP+NDIM
          IF(ABS(ISW)==2)THEN
             ! ** Non-generic case
             ICP(NXP+2)=NPAR+2*NXP+NDIM+1 ! b
             AP%NPARI=AP%NPARI+1
          ENDIF
          NDIM=2*NDIM
          NBC=NBC+NDIM+NXP
          NINT=NINT+NXP+1
       ENDIF
    ENDIF
    AP%NDIM = NDIM
    AP%NBC = NBC
    AP%NINT = NINT
    AP%NFPR = NBC+NINT-NDIM+1

  END SUBROUTINE INITBVP

! ---------- -------
  SUBROUTINE AUTOBVP(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    INTEGER ISW, ITP

    CALL INITBVP(AP,ICP)

    ISW = AP%ISW
    ITP = AP%ITP

    IF(ABS(ISW)<=1)THEN
       ! ** Boundary value problems (here IPS=4)
       CALL AUTOBV(AP,ICP,ICU,FUNI,BCNI,ICNI,STPNBV,FNCSBV)
    ELSE
       ! Two-Parameter Continuation for IPS=4 or IPS=7.
       IF(ABS(ISW)==2)THEN
          IF(ITP==5)THEN
             ! ** Continuation of folds (BVP, start).
             CALL AUTOBV(AP,ICP,ICU,FNBL,BCBL,ICBL,STPNBL,FNCSBV)
          ELSE IF(ABS(ITP)/10==5) THEN
             ! ** Continuation of folds (BVP, restart).
             CALL AUTOBV(AP,ICP,ICU,FNBL,BCBL,ICBL,STPNBV,FNCSBV)
          ENDIF
       ENDIF
       IF((ITP==6.OR.(ABS(ITP)/10)==6)) THEN
          ! ** BP cont (BVP, start and restart) (by F. Dercole).
          CALL AUTOBV(AP,ICP,ICU,FNBBP,BCBBP,ICBBP,STPNBBP,FNCSBV)
       ENDIF
    ENDIF

  END SUBROUTINE AUTOBVP

! ---------- ------
  SUBROUTINE STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE MESH

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR),TM(0:*)
    DOUBLE PRECISION, INTENT(OUT) :: UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*)

    INTEGER NDIM,IPS,ISW,NTST,NCOL,NDIMRD,NTSRS
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    NDIM=AP%NDIM
    IPS=AP%IPS
    ISW=AP%ISW
    NTST=AP%NTST
    NCOL=AP%NCOL

    IF(AP%IRS==0)THEN
       CALL STPNUB(AP,PAR,RLDOT,UPS,UDOTPS,TM,NODIR)
       RETURN
    ENDIF

    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),UDOTPSR(NDIM,0:NCOLRS*NTSR), &
         TMR(0:NTSR))
    CALL STPNBV1(AP,PAR,ICP,NDIM,NTSRS,NDIMRD,NCOLRS,RLDOT, &
         UPSR,UDOTPSR,TMR,NODIR)
    CALL ADAPT2(NTSR,NCOLRS,NDIM,NTST,NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,(IPS==2.OR.IPS==12) .AND. ABS(ISW)<=1)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNBV

! ---------- -------
  SUBROUTINE STPNBV1(AP,PAR,ICP,NDIM,NTSRS,NDIMRD,NCOLRS,RLDOT, &
       UPS,UDOTPS,TM,NODIR)

    USE IO

! This subroutine locates and retrieves the information required to
! restart computation at the point with label IRS.
! This information is expected on unit 3.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    INTEGER, INTENT(OUT) :: NTSRS,NCOLRS,NDIMRD,NODIR
    DOUBLE PRECISION, INTENT(OUT) :: UPS(*),UDOTPS(*),TM(*)
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR)
! Local
    INTEGER NFPR,NFPRS,ITPRS,NDIMRS,I
    INTEGER, ALLOCATABLE :: ICPRS(:)

    NFPR=AP%NFPR

    ALLOCATE(ICPRS(NFPR))
    ICPRS(:)=0
    CALL READBV(AP,PAR,ICPRS,NTSRS,NCOLRS,NDIMRD,RLDOT,UPS, &
         UDOTPS,TM,ITPRS,NDIM)

! Take care of the case where the free parameters have been changed at
! the restart point.

    NODIR=0
    NFPRS=GETNFPR3()
    NDIMRS=GETNDIM3()
    IF(NFPRS.NE.NFPR.OR.NDIM.NE.NDIMRS)THEN
       NODIR=1
    ELSE
       DO I=1,NFPR
          IF(ICPRS(I).NE.ICP(I)) THEN
             NODIR=1
             EXIT
          ENDIF
       ENDDO
    ENDIF
    DEALLOCATE(ICPRS)

  END SUBROUTINE STPNBV1

! ---------- ------
  SUBROUTINE STPNUB(AP,PAR,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE MESH
    USE AUTO_CONSTANTS, ONLY : DATFILE, UVALS, PARVALS, unames, parnames
    USE SUPPORT, ONLY: NAMEIDX, AUTOSTOP

! Generates a starting point for the continuation of a branch of
! of solutions to general boundary value problems by calling the user
! supplied subroutine STPNT where an analytical solution is given.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR)
    DOUBLE PRECISION, INTENT(OUT) :: TM(0:*),UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*)

    INTEGER NDIM,IPS,NTST,NCOL,ISW,NTSR,ios,I,J
    DOUBLE PRECISION TEMP,PERIOD
    DOUBLE PRECISION, ALLOCATABLE :: TMR(:),UPSR(:,:),UDOTPSR(:,:),U(:)

    NDIM=AP%NDIM
    IPS=AP%IPS
    NTST=AP%NTST
    NCOL=AP%NCOL
    ISW=AP%ISW

! Generate the (initially uniform) mesh.

    CALL MSH(NTST,TM)

    IF(DATFILE/='')THEN
       OPEN(3,FILE=TRIM(DATFILE),STATUS='old',ACCESS='sequential',&
            IOSTAT=ios)
       IF(ios/=0)THEN
          OPEN(3,FILE=TRIM(DATFILE)//'.dat',STATUS='old',&
               ACCESS='sequential',IOSTAT=ios)
       ENDIF
       IF(ios/=0)THEN
          WRITE(6,"(A,A,A)")'Datafile ',TRIM(DATFILE),' not found.'
          CALL AUTOSTOP()
       ENDIF
       NTSR=-1
       DO
          READ(3,*,END=2)TEMP,(TEMP,I=1,NDIM)
          NTSR=NTSR+1
       ENDDO
2      CONTINUE
       ALLOCATE(TMR(0:NTSR),UPSR(NDIM,0:NTSR),UDOTPSR(NDIM,0:NTSR),U(NDIM))
       REWIND 3
       DO J=0,NTSR
          READ(3,*)TMR(J),UPSR(:,J)
       ENDDO
       CLOSE(3)
       UDOTPSR(:,:)=0.d0
       PERIOD=TMR(NTSR)-TMR(0)
       DO I=NTSR,0,-1
          TMR(I)=(TMR(I)-TMR(0))/PERIOD
       ENDDO
       CALL ADAPT2(NTSR,1,NDIM,NTST,NCOL,NDIM, &
            TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,(IPS==2.OR.IPS==12).AND.ABS(ISW)<=1)
       IF(AP%NPAR>10)THEN
          PAR(11)=PERIOD
       ENDIF
       CALL STPNT(NDIM,U,PAR,0d0)
       RLDOT(:)=0.d0
       DEALLOCATE(TMR,UPSR,UDOTPSR,U)
    ELSE
       DO J=0,NTST*NCOL
          UPS(:,J)=0.d0
          CALL STPNT(NDIM,UPS(1,J),PAR,DBLE(J)/(NTST*NCOL))
       ENDDO
    ENDIF

! override parameter values with values from constants file

    DO I=1,SIZE(UVALS)
       UPS(NAMEIDX(UVALS(I)%INDEX,unames),0:NTST*NCOL)=UVALS(I)%VAR
    ENDDO
    DO I=1,SIZE(PARVALS)
       PAR(NAMEIDX(PARVALS(I)%INDEX,parnames))=PARVALS(I)%VAR
    ENDDO

    NODIR=1

  END SUBROUTINE STPNUB

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNCSBV(AP,ICP,UPS,NDIM,PAR,ITEST,ATYPE) RESULT(Q)

    USE AUTO_CONSTANTS, ONLY : NPARX
    USE SUPPORT, ONLY: P0=>P0V, P1=>P1V, EV=>EVV

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(IN) :: ITEST
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE

    Q=0.d0
    ATYPE=''
    SELECT CASE(ITEST)
    CASE(0)
       CALL PVLSI(AP,UPS,NDIM,PAR)
    CASE(1)
       Q=FNLPBV(AP,ATYPE)
    CASE(2)
       Q=FNBPBV(AP,ATYPE,P1)
    END SELECT

  END FUNCTION FNCSBV
       
! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNLPBV(AP,ATYPE)

    USE MESH
    USE SOLVEBV
    USE SUPPORT, ONLY: CHECKSP

! RETURNS A QUANTITY THAT CHANGES SIGN AT A LIMIT POINT (BVP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE

    INTEGER IID,IBR,NTOT,NTOP

    ATYPE=''
    FNLPBV=0d0
    IF(.NOT.CHECKSP('LP',AP%IPS,AP%ILP,AP%ISP))RETURN

    IID=AP%IID
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    FNLPBV=AP%FLDF
    IF(IID.GE.2)THEN
       WRITE(9,101)ABS(IBR),NTOP+1,FNLPBV
    ENDIF

! Set the quantity to be returned.

    ATYPE='LP'

101 FORMAT(I4,I6,9X,'Fold Function ',ES14.5)

  END FUNCTION FNLPBV

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNBPBV(AP,ATYPE,P1)

    USE SUPPORT

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    DOUBLE PRECISION, INTENT(IN) :: P1(*)

! Local
    DOUBLE PRECISION, ALLOCATABLE :: PP(:)
    INTEGER IID,I,IBR,NTOP,NTOT,NDIM
    DOUBLE PRECISION U(1),F(1),DET

    ATYPE=''
    FNBPBV=0d0
    AP%BIFF=FNBPBV
    IF(.NOT.CHECKSP('BP',AP%IPS,AP%ILP,AP%ISP))RETURN

    NDIM=AP%NDIM
    IID=AP%IID

    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

! Compute the determinant of P1.

    ALLOCATE(PP(NDIM**2))
    DO I=1,NDIM**2
       PP(I)=P1(I)
    ENDDO
    CALL GEL(NDIM,PP,0,U,F,DET)
    DEALLOCATE(PP)

! AP%DET contains the determinant of the reduced system.
! Set the determinant of the normalized reduced system.

    IF(ABS(AP%DET)/HUGE(DET).LT.ABS(DET))THEN
       FNBPBV=AP%DET/DET
       ATYPE='BP'
    ENDIF
    AP%BIFF=FNBPBV

    IF(IID.GE.2)WRITE(9,101)ABS(IBR),NTOP+1,FNBPBV
101 FORMAT(I4,I6,9X,'BP   Function ',ES14.5)

  END FUNCTION FNBPBV

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for the Continuation of Folds for BVP.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ----
  SUBROUTINE FNBL(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNBLF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

  END SUBROUTINE FNBL

! ---------- -----
  SUBROUTINE FNBLF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FUNI)

    ! Generates the equations for the 2-parameter continuation
    ! of folds (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:),FF1(:),FF2(:)
    INTEGER NDM,NFPR,NFPX,NPAR,I,J
    DOUBLE PRECISION UU,UMX,EP,P

    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Generate the function.

    CALL FFBL(AP,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP,FUNI)

    IF(IJAC==0)RETURN

    ! Generate the Jacobian.

    CALL EXPANDJAC(DFDU,NDM,NDM,NDIM)
    CALL EXPANDJAC(DFDP,NPAR,NDM,NDIM)
    DFDU(1:NDM,NDM+1:NDIM)=0d0
    DFDU(NDM+1:NDIM,NDM+1:NDIM)=DFDU(1:NDM,1:NDM)
    IF(IJAC==2)THEN
       NFPX=NFPR/2-1
       DO I=1,NFPR-NFPX
          DFDP(1:NDM,ICP(I))=DFDP(1:NDM,ICP(I))
       ENDDO
       DO I=1,NFPX
          DFDP(1:NDM,ICP(NFPR-NFPX+I))=0d0
          DFDP(NDM+1:NDIM,ICP(NFPR-NFPX+I))=DFDP(1:NDM,ICP(I+1))
       ENDDO
    ENDIF

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I))>UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    ALLOCATE(FF1(NDIM),FF2(NDIM),DFU(NDM,NDM),DFP(NDM,NPAR))
    DO I=1,NDM
       UU=U(I)
       U(I)=UU-EP
       CALL FFBL(AP,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP,FUNI)
       U(I)=UU+EP
       CALL FFBL(AP,U,UOLD,ICP,PAR,FF2,NDM,DFU,DFP,FUNI)
       U(I)=UU
       DO J=NDM+1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF (IJAC==1)THEN
       DEALLOCATE(DFU,DFP,FF1)
       RETURN
    ENDIF

    NFPX=NFPR/2-1
    DO I=1,NFPR-NFPX
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FFBL(AP,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP,FUNI)
       DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(DFU,DFP,FF1)
  END SUBROUTINE FNBLF

! ---------- ----
  SUBROUTINE FFBL(AP,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP,FUNI)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(2*NDM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(2*NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    include 'interfaces.h'

    INTEGER NFPR,NFPX,I,J

    NFPR=AP%NFPR

    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)

    NFPX=NFPR/2-1
    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
       ENDDO
       DO J=1,NFPX
          F(NDM+I)=F(NDM+I) + DFDP(I,ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
       ENDDO
    ENDDO

  END SUBROUTINE FFBL

! ---------- ----
  SUBROUTINE BCBL(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    ! Generates the boundary conditions for the 2-parameter continuation
    ! of folds (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    CALL BCBLF(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC,BCNI)

  END SUBROUTINE BCBL

! ---------- -----
  SUBROUTINE BCBLF(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC,BCNI)

    ! Generates the boundary conditions for the 2-parameter continuation
    ! of folds (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: FF1(:),FF2(:),DFU(:,:)
    INTEGER NDM,NBC0,NFPR,NPAR,I,J
    DOUBLE PRECISION UMX,EP,P,UU

    NDM=AP%NDM
    NBC0=NBC/2
    NFPR=AP%NFPR
    NPAR=AP%NPAR
    ALLOCATE(DFU(NBC0,2*NDM+NPAR))

    ! Generate the function.

    CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,F,DFU,BCNI)

    IF(IJAC==0)THEN
       DEALLOCATE(DFU)
       RETURN
    ENDIF

    ALLOCATE(FF1(NBC),FF2(NBC))

    ! Derivatives with respect to U0.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U0(I))>UMX)UMX=DABS(U0(I))
    ENDDO
    EP=HMACH*(1+UMX)
    DO I=1,NDIM
       UU=U0(I)
       U0(I)=UU-EP
       CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,FF1,DFU,BCNI)
       U0(I)=UU+EP
       CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,FF2,DFU,BCNI)
       U0(I)=UU
       DO J=1,NBC
          DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    ! Derivatives with respect to U1.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U1(I))>UMX)UMX=DABS(U1(I))
    ENDDO
    EP=HMACH*(1+UMX)
    DO I=1,NDIM
       UU=U1(I)
       U1(I)=UU-EP
       CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,FF1,DFU,BCNI)
       U1(I)=UU+EP
       CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,FF2,DFU,BCNI)
       U1(I)=UU
       DO J=1,NBC
          DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF1)
    IF(IJAC==1)THEN
       DEALLOCATE(FF2,DFU)
       RETURN
    ENDIF

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,FF2,DFU,BCNI)
       DO J=1,NBC
          DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(DFU,FF2)
  END SUBROUTINE BCBLF

! ---------- ----
  SUBROUTINE FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,F,DBC,BCNI)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC0
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC0+AP%NDM+AP%NFPR/2-1)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC0,*)
    include 'interfaces.h'

    INTEGER NDM,NFPR,NFPX,I,J

    NDM=AP%NDM
    NFPR=AP%NFPR

    NFPX=NFPR/2-1
    CALL BCNI(AP,NDM,PAR,ICP,NBC0,U0,U1,F,2,DBC)
    DO I=1,NBC0
       F(NBC0+I)=0.d0
       DO J=1,NDM
          F(NBC0+I)=F(NBC0+I)+DBC(I,J)*U0(NDM+J)
          F(NBC0+I)=F(NBC0+I)+DBC(I,NDM+J)*U1(NDM+J)
       ENDDO
       DO J=1,NFPX
          F(NBC0+I)=F(NBC0+I) + DBC(I,NDIM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
       ENDDO
    ENDDO

  END SUBROUTINE FBBL

! ---------- ----
  SUBROUTINE ICBL(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Generates integral conditions for the 2-parameter continuation of
    ! folds (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    CALL ICBLF(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT,ICNI)

  END SUBROUTINE ICBL

! ---------- -----
  SUBROUTINE ICBLF(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT,ICNI)

    ! Generates integral conditions for the 2-parameter continuation of
    ! folds (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
    include 'interfaces.h'
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: FF1(:),FF2(:),DFU(:,:)
    INTEGER NDM,NNT0,NFPR,NPAR,I,J,NFPX
    DOUBLE PRECISION UMX,EP,P,UU

    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Note that PAR(NPAR) is used to keep the norm of the null vector

    F(NINT)=-PAR(NPAR)
    DO I=1,NDM
       F(NINT)=F(NINT)+U(NDM+I)*U(NDM+I)
    ENDDO
    IF(IJAC/=0)THEN
       DINT(NINT,NDM+1:NDIM)=2*U(NDM+1:NDIM)
       IF(IJAC/=1.AND.ICP(NFPR/2+1)==NPAR)THEN
          DINT(NINT,NDIM+NPAR)=-1
       ENDIF
    ENDIF

    IF(NINT==1)RETURN

    NNT0=(NINT-1)/2
    NFPX=NFPR/2-1
    ALLOCATE(DFU(NNT0,NDM+NPAR))

    ! Generate the function.

    CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DFU,ICNI)
    DO I=1,NFPX
       F(NINT)=F(NINT)+PAR(ICP(NFPR-NFPX+I))**2
    ENDDO

    IF(IJAC==0)THEN
       DEALLOCATE(DFU)
       RETURN
    ENDIF

    ! Generate the Jacobian.

    DINT(1:NNT0,1:NDM)=DFU(1:NNT0,1:NDM)
    DINT(NNT0+1:NINT-1,NDM+1:NDIM)=DFU(1:NNT0,1:NDM)
    IF(IJAC/=1)THEN
       DO I=1,NFPR
          DINT(1:NNT0,NDIM+ICP(I))=DFU(1:NNT0,NDM+ICP(I))
          IF(I>=NFPR-NFPX+1)THEN
             DINT(NNT0+1:NINT-1,NDIM+ICP(I))=DFU(1:NNT0,NDM+ICP(I-NFPX-1))
             DINT(NINT,NDIM+ICP(I))=2*PAR(ICP(I))
          ENDIF
       ENDDO
    ENDIF

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I))>UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    ALLOCATE(FF1(NINT),FF2(NINT))
    DO I=1,NDM
       UU=U(I)
       U(I)=UU-EP
       CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FF1,DFU,ICNI)
       U(I)=UU+EP
       CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FF2,DFU,ICNI)
       U(I)=UU
       DO J=NNT0+1,NINT-1
          DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC==1)THEN
       DEALLOCATE(FF1,DFU)
       RETURN
    ENDIF

    DO I=1,NFPR-NFPX
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FF1,DFU,ICNI)
       DO J=NNT0+1,NINT-1
          DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(FF1,DFU)
  END SUBROUTINE ICBLF

! ---------- ----
  SUBROUTINE FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DINT,ICNI)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NINT,NNT0
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*),UDOT(*),UPOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(*),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NNT0,*)
    include 'interfaces.h'
    INTEGER NDM,NFPR,NFPX,I,J

    NDM=AP%NDM
    NFPR=AP%NFPR
    NFPX=NFPR/2-1

    CALL ICNI(AP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,F,2,DINT)
    DO I=1,NNT0
       F(NNT0+I)=0.d0
       DO J=1,NDM
          F(NNT0+I)=F(NNT0+I)+DINT(I,J)*U(NDM+J)
       ENDDO
       DO J=1,NFPX
          F(NNT0+I)=F(NNT0+I) + DINT(I,NDM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
       ENDDO
    ENDDO

  END SUBROUTINE FIBL

! ---------- ------
  SUBROUTINE STPNBL(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE IO
    USE MESH

    ! Generates starting data for the 2-parameter continuation of folds.
    ! (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    INTEGER, ALLOCATABLE :: ICPRS(:)
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER NDIM,NDM,NFPR,NDIMRD,ITPRS,I,NPAR

    NDIM=AP%NDIM
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ALLOCATE(ICPRS(NFPR))
    ALLOCATE(UPSR(NDM,0:NCOLRS*NTSR),UDOTPSR(NDM,0:NCOLRS*NTSR),TMR(0:NTSR))
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOT,UPSR, &
         UDOTPSR,TMR,ITPRS,NDM)
    DEALLOCATE(ICPRS)

    DO I=NFPR/2+1,NFPR
       PAR(ICP(I))=0.d0
    ENDDO

    NODIR=0
    CALL ADAPT2(NTSR,NCOLRS,NDM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)

  END SUBROUTINE STPNBL

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!   Subroutines for BP cont (BVPs) (by F. Dercole)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- -----
  SUBROUTINE FNBBP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for the 2-parameter continuation
    ! of BP (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:),FF1(:),FF2(:)
    INTEGER NDM,NFPR,NPAR,I,J
    DOUBLE PRECISION UMX,EP,P,UU

    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    ! Generate the function.

    CALL FFBBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,F,NDM,DFDU,DFDP)

    IF(IJAC==0)RETURN

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I))>UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    ALLOCATE(FF1(NDIM),FF2(NDIM),DFU(NDM,NDM),DFP(NDM,NPAR))
    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FFBBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,FF1,NDM,DFU,DFP)
       U(I)=UU+EP
       CALL FFBBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,FF2,NDM,DFU,DFP)
       U(I)=UU
       DO J=1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF (IJAC==1)THEN
       DEALLOCATE(DFU,DFP,FF1)
       RETURN
    ENDIF

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FFBBP(AP,NDIM,U,UOLD,UOLD(NDIM+NFPR+1),ICP,PAR,FF1,NDM,DFU,DFP)
       DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(DFU,DFP,FF1)

  END SUBROUTINE FNBBP

! ---------- -----
  SUBROUTINE FFBBP(AP,NDIM,U,UOLD,UPOLD,ICP,PAR,F,NDM,DFDU,DFDP)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NDM
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UPOLD(NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: FI(:),DINT(:,:)
    DOUBLE PRECISION DUM(1)
    INTEGER ISW,NBC,NINT,NBC0,NNT0,NFPX,I,J,NPARU

    ISW=AP%ISW
    NBC=AP%NBC
    NINT=AP%NINT
    NPARU=AP%NPAR-AP%NPARI ! real - internal

    IF(ISW<0) THEN
       !        ** start
       NBC0=(4*NBC-NINT-5*NDM+2)/15
       NNT0=(-NBC+4*NINT+5*NDM-23)/15
    ELSE IF(ISW==2) THEN
       !        ** Non-generic case
       NBC0=(2*NBC-NINT-3*NDM)/3
       NNT0=(-NBC+2*NINT+3*NDM-3)/3
    ELSE
       !        ** generic case
       NBC0=(2*NBC-NINT-3*NDM)/3
       NNT0=(-NBC+2*NINT+3*NDM-3)/3
    ENDIF
    NFPX=NBC0+NNT0-NDM+1

    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)

    IF((ISW==2).OR.(ISW<0)) THEN
       !        ** Non-generic and/or start
       DO I=1,NDM
          F(I)=F(I)-PAR(NPARU+2*NFPX+NDM+1)*U(NDM+I)
       ENDDO
    ENDIF

    DO I=1,NDM
       F(NDM+I)=0.d0
       DO J=1,NDM
          F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
       ENDDO
    ENDDO

    IF(NNT0>0) THEN
       ALLOCATE(FI(NNT0),DINT(NNT0,NDM))
       CALL ICNI(AP,NDM,PAR,ICP,NNT0,U,UOLD,DUM,UPOLD,FI,1,DINT)
       DO I=1,NDM
          DO J=1,NNT0
             F(NDM+I)=F(NDM+I)+DINT(J,I)*PAR(NPARU+NBC0+J)
          ENDDO
       ENDDO
       DEALLOCATE(FI,DINT)
    ENDIF

    IF(ISW<0) THEN
       !        ** start
       DO I=1,NDM
          F(NDM+I)=F(NDM+I)+PAR(NPARU+4*NFPX+NDM+2)*U(2*NDM+I)+ &
               PAR(NPARU+4*NFPX+NDM+3)*U(3*NDM+I)
          F(2*NDM+I)=0.d0
          F(3*NDM+I)=0.d0
          DO J=1,NDM
             F(2*NDM+I)=F(2*NDM+I)+DFDU(I,J)*U(2*NDM+J)
             F(3*NDM+I)=F(3*NDM+I)+DFDU(I,J)*U(3*NDM+J)
          ENDDO
          DO J=1,NFPX
             F(2*NDM+I)=F(2*NDM+I)+DFDP(I,ICP(J))*PAR(NPARU+2*NFPX+NDM+1+J)
             F(3*NDM+I)=F(3*NDM+I)+DFDP(I,ICP(J))*PAR(NPARU+3*NFPX+NDM+1+J)
          ENDDO
       ENDDO
    ENDIF

  END SUBROUTINE FFBBP

! ---------- -----
  SUBROUTINE BCBBP(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    ! Generates the boundary conditions for the 2-parameter continuation
    ! of BP (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: FF1(:),FF2(:),DFU(:,:)
    INTEGER ISW,NINT,NDM,NFPR,NPAR,NBC0,I,J
    DOUBLE PRECISION UMX,EP,P,UU

    ISW=AP%ISW
    NINT=AP%NINT
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    IF(ISW<0) THEN
       ! ** start
       NBC0=(4*NBC-NINT-5*NDM+2)/15
    ELSE IF(ISW==2) THEN
       ! ** Non-generic case
       NBC0=(2*NBC-NINT-3*NDM)/3
    ELSE
       ! ** generic case
       NBC0=(2*NBC-NINT-3*NDM)/3
    ENDIF

    ! Generate the function.

    ALLOCATE(DFU(NBC0,2*NDM+NPAR))
    CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,F,DFU)

    IF(IJAC==0)THEN
       DEALLOCATE(DFU)
       RETURN
    ENDIF

    ALLOCATE(FF1(NBC),FF2(NBC))

    ! Derivatives with respect to U0.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U0(I))>UMX)UMX=DABS(U0(I))
    ENDDO
    EP=HMACH*(1+UMX)
    DO I=1,NDIM
       UU=U0(I)
       U0(I)=UU-EP
       CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF1,DFU)
       U0(I)=UU+EP
       CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF2,DFU)
       U0(I)=UU
       DO J=1,NBC
          DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    ! Derivatives with respect to U1.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U1(I))>UMX)UMX=DABS(U1(I))
    ENDDO
    EP=HMACH*(1+UMX)
    DO I=1,NDIM
       UU=U1(I)
       U1(I)=UU-EP
       CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF1,DFU)
       U1(I)=UU+EP
       CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF2,DFU)
       U1(I)=UU
       DO J=1,NBC
          DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC==1)THEN
       DEALLOCATE(FF1,DFU)
       RETURN
    ENDIF

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF1,DFU)
       DO J=1,NBC
          DBC(J,2*NDIM+ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(FF1,DFU)

    RETURN
  END SUBROUTINE BCBBP

  !     ---------- -----
  SUBROUTINE FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FB,DBC)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,NBC0
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC0,*)

    INTEGER ISW,NINT,NDM,NNT0,NFPX,I,J,NPARU,IBC

    ISW=AP%ISW
    NINT=AP%NINT
    NDM=AP%NDM
    NPARU=AP%NPAR-AP%NPARI

    IF(ISW<0) THEN
       !        ** start
       NNT0=(-NBC+4*NINT+5*NDM-23)/15
    ELSE IF(ISW==2) THEN
       !        ** Non-generic case
       NNT0=(-NBC+2*NINT+3*NDM-3)/3
    ELSE
       !        ** generic case
       NNT0=(-NBC+2*NINT+3*NDM-3)/3
    ENDIF
    NFPX=NBC0+NNT0-NDM+1

    CALL BCNI(AP,NDM,PAR,ICP,NBC0,U0,U1,FB,2,DBC)

    IF((ISW==2).OR.(ISW<0)) THEN
       !        ** Non-generic and/or start
       DO I=1,NBC0
          FB(I)=FB(I)+PAR(NPARU+2*NFPX+NDM+1)*PAR(NPARU+I)
       ENDDO
    ENDIF

    DO I=1,NDM
       FB(NBC0+I)=-U0(NDM+I)
       FB(NBC0+NDM+I)=U1(NDM+I)
       DO J=1,NBC0
          FB(NBC0+I)=FB(NBC0+I)+DBC(J,I)*PAR(NPARU+J)
          FB(NBC0+NDM+I)=FB(NBC0+NDM+I)+DBC(J,NDM+I)*PAR(NPARU+J)
       ENDDO
    ENDDO
    DO I=1,NFPX
       FB(NBC0+2*NDM+I)=PAR(NPARU+NFPX+NDM+I)
       DO J=1,NBC0
          FB(NBC0+2*NDM+I)=FB(NBC0+2*NDM+I)+ &
               DBC(J,2*NDM+ICP(I))*PAR(NPARU+J)
       ENDDO
    ENDDO

    IF(ISW<0) THEN
       !        ** start
       IBC=NBC0+2*NDM+NFPX
       DO I=1,NBC0
          FB(IBC+I)=0.d0
          FB(IBC+NBC0+I)=0.d0
          DO J=1,NDM
             FB(IBC+I)=FB(IBC+I)+DBC(I,J)*U0(2*NDM+J)
             FB(IBC+I)=FB(IBC+I)+DBC(I,NDM+J)*U1(2*NDM+J)
             FB(IBC+NBC0+I)=FB(IBC+NBC0+I)+DBC(I,J)*U0(3*NDM+J)
             FB(IBC+NBC0+I)=FB(IBC+NBC0+I)+DBC(I,NDM+J)*U1(3*NDM+J)
          ENDDO
          DO J=1,NFPX
             FB(IBC+I)=FB(IBC+I)+DBC(I,2*NDM+ICP(J))*PAR(NPARU+2*NFPX+NDM+1+J)
             FB(IBC+NBC0+I)=FB(IBC+NBC0+I)+ &
                  DBC(I,2*NDM+ICP(J))*PAR(NPARU+3*NFPX+NDM+1+J)
          ENDDO
       ENDDO
    ENDIF

  END SUBROUTINE FBBBP

! ---------- -----
  SUBROUTINE ICBBP(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Generates integral conditions for the 2-parameter continuation
    ! of BP (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: FF1(:),FF2(:),DFU(:)
    INTEGER ISW,NBC,NDM,NFPR,NPAR,NNT0,I,J
    DOUBLE PRECISION UMX,EP,P,UU

    ISW=AP%ISW
    NBC=AP%NBC
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPAR=AP%NPAR

    IF(ISW<0) THEN
       !        ** start
       NNT0=(-NBC+4*NINT+5*NDM-23)/15
    ELSE IF(ISW==2) THEN
       !        ** Non-generic case
       NNT0=(-NBC+2*NINT+3*NDM-3)/3
    ELSE
       !        ** generic case
       NNT0=(-NBC+2*NINT+3*NDM-3)/3
    ENDIF

    ! Generate the function.

    IF(NNT0>0) THEN
       ALLOCATE(DFU(NNT0*(NDM+NPAR)))
    ELSE
       ALLOCATE(DFU(1))
    ENDIF
    CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DFU)

    IF(IJAC==0)THEN
       DEALLOCATE(DFU)
       RETURN
    ENDIF

    ALLOCATE(FF1(NINT),FF2(NINT))

    ! Generate the Jacobian.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U(I))>UMX)UMX=DABS(U(I))
    ENDDO

    EP=HMACH*(1+UMX)

    DO I=1,NDIM
       UU=U(I)
       U(I)=UU-EP
       CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FF1,DFU)
       U(I)=UU+EP
       CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FF2,DFU)
       U(I)=UU
       DO J=1,NINT
          DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DEALLOCATE(FF2)
    IF(IJAC==1)THEN
       DEALLOCATE(FF1,DFU)
       RETURN
    ENDIF

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FF1,DFU)
       DO J=1,NINT
          DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(FF1,DFU)

  END SUBROUTINE ICBBP

  !     ---------- -----
  SUBROUTINE FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,FI,DINT)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,NNT0
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NNT0,*)

    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: F(:),DFU(:,:),DFP(:,:)
    INTEGER ISW,NBC,NDM,NBC0,NFPX,NPAR,I,J,K,NPARU

    ISW=AP%ISW
    NBC=AP%NBC
    NDM=AP%NDM
    NPAR=AP%NPAR
    NPARU=NPAR-AP%NPARI

    IF(ISW<0) THEN
       ! ** start
       NBC0=(4*NBC-NINT-5*NDM+2)/15
    ELSE IF(ISW==2) THEN
       ! ** Non-generic case
       NBC0=(2*NBC-NINT-3*NDM)/3
    ELSE
       ! ** generic case
       NBC0=(2*NBC-NINT-3*NDM)/3
    ENDIF
    NFPX=NBC0+NNT0-NDM+1

    ALLOCATE(F(NDM),DFU(NDM,NDM),DFP(NDM,NPAR))
    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
    IF(NNT0>0) THEN
       CALL ICNI(AP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,FI,2,DINT)

       IF((ISW==2).OR.(ISW<0)) THEN
          ! ** Non-generic and/or start
          ! (18) int_0^1 h(x,p) dt + b phi_3^* = 0
          DO I=1,NNT0
             FI(I)=FI(I)+PAR(NPARU+2*NFPX+NDM+1)*PAR(NPARU+NBC0+I)
          ENDDO
       ENDIF
    ENDIF

    ! (13b) -d + int_0^1 (-f_p(x,p)^T phi_1^* + h_p(x,p)^T phi_3^*) dt = 0
    DO I=1,NFPX
       FI(NNT0+I)=-PAR(NPARU+NFPX+NDM+I)
       DO J=1,NDM
          FI(NNT0+I)=FI(NNT0+I)-DFP(J,ICP(I))*U(NDM+J)
       ENDDO
       DO J=1,NNT0
          FI(NNT0+I)=FI(NNT0+I)+DINT(J,NDM+ICP(I))*PAR(NPARU+NBC0+J)
       ENDDO
    ENDDO

    ! ** start
    IF(ISW<0) THEN
       ! (15b) int_0^1 -f(x,p)^T phi_1^* dt + c_1 q + c_2 r = 0
       DO I=1,NFPX
          FI(NNT0+I)=FI(NNT0+I)+ &
               PAR(NPARU+4*NFPX+NDM+2)*PAR(NPARU+2*NFPX+NDM+1+I)+ &
               PAR(NPARU+4*NFPX+NDM+3)*PAR(NPARU+3*NFPX+NDM+1+I)
       ENDDO

       ! (9a) int_0^1 (h_x(x,p)v + h_p(x,p)q) dt = 0
       ! (9b) int_0^1 (h_x(x,p)w + h_p(x,p)r) dt = 0
       DO K=2,3
          DO I=1,NNT0
             FI(NFPX+K*NNT0+I)=0.d0
             DO J=1,NDM
                FI(NFPX+K*NNT0+I)=FI(NFPX+K*NNT0+I)+DINT(I,J)*U(K*NDM+J)
             ENDDO
             DO J=1,NFPX
                FI(NFPX+K*NNT0+I)=FI(NFPX+K*NNT0+I)+DINT(I,NDM+ICP(J))* &
                     PAR(NPARU+K*NFPX+NDM+1+J)
             ENDDO
          ENDDO
       ENDDO
       ! (10a) int_0^1 <v, v_old> dt + <q, q_old> - 1 = 0
       ! (10b) int_0^1 <w, w_old> dt + <r, r_old> - 1 = 0
       ! (11a) int_0^1 <v, w_old> dt + <q, r_old> = 0
       ! (11b) int_0^1 <w, v_old> dt + <r, q_old> = 0
       FI(NFPX+4*NNT0+1)=-1.d0
       FI(NFPX+4*NNT0+2)=-1.d0
       FI(NFPX+4*NNT0+3)=0.d0
       FI(NFPX+4*NNT0+4)=0.d0
       DO I=1,NDM
          FI(NFPX+4*NNT0+1)=FI(NFPX+4*NNT0+1)+U(2*NDM+I)*UOLD(2*NDM+I)
          FI(NFPX+4*NNT0+2)=FI(NFPX+4*NNT0+2)+U(3*NDM+I)*UOLD(3*NDM+I)
          FI(NFPX+4*NNT0+3)=FI(NFPX+4*NNT0+3)+U(2*NDM+I)*UOLD(3*NDM+I)
          FI(NFPX+4*NNT0+4)=FI(NFPX+4*NNT0+4)+U(3*NDM+I)*UOLD(2*NDM+I)
       ENDDO
       DO I=1,NFPX
          FI(NFPX+4*NNT0+1)=FI(NFPX+4*NNT0+1)+PAR(NPARU+2*NFPX+NDM+1+I)* &
               UOLD(NDIM+3*NFPX+NDM+1+I)
          FI(NFPX+4*NNT0+2)=FI(NFPX+4*NNT0+2)+PAR(NPARU+3*NFPX+NDM+1+I)* &
               UOLD(NDIM+4*NFPX+NDM+1+I)
          FI(NFPX+4*NNT0+3)=FI(NFPX+4*NNT0+3)+PAR(NPARU+2*NFPX+NDM+1+I)* &
               UOLD(NDIM+4*NFPX+NDM+1+I)
          FI(NFPX+4*NNT0+4)=FI(NFPX+4*NNT0+4)+PAR(NPARU+3*NFPX+NDM+1+I)* &
               UOLD(NDIM+3*NFPX+NDM+1+I)
       ENDDO
       DEALLOCATE(F,DFU,DFP)
    ENDIF

    ! (13c) -a + int_0^1 ||phi_1^*||^2 dt + ||phi_2^*||^2 + ||phi_3^*||^2 = 0
    FI(NINT)=-PAR(NPARU+NFPX+NDM)
    DO I=1,NDM
       FI(NINT)=FI(NINT)+U(NDM+I)**2
    ENDDO
    DO I=1,NBC0+NNT0
       FI(NINT)=FI(NINT)+PAR(NPARU+I)**2
    ENDDO

  END SUBROUTINE FIBBP

! ---------- -------
  SUBROUTINE STPNBBP(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE SOLVEBV
    USE IO
    USE MESH

    ! Generates starting data for the 2-parameter continuation
    ! of BP (BVP).

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    INTEGER, ALLOCATABLE :: ICPRS(:)
    DOUBLE PRECISION, ALLOCATABLE :: RVDOT(:)
    DOUBLE PRECISION, ALLOCATABLE :: THU1(:),THL1(:)
    DOUBLE PRECISION, ALLOCATABLE :: P0(:,:),P1(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: U(:),RLDOTRS(:),RLCUR(:)
    DOUBLE PRECISION, ALLOCATABLE :: DTM(:),UPST(:,:),UDOTPST(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: VDOTPST(:,:),UPOLDPT(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER NDIM,ISW,NBC,NINT,NFPR,I,J,IFST,NLLV
    INTEGER ITPRS,NDM,NBC0,NNT0,NFPX,NDIMRD,NPARU
    DOUBLE PRECISION DUM(1),DET,RDSZ
    TYPE(AUTOPARAMETERS) AP2

    NDIM=AP%NDIM
    ISW=AP%ISW
    NBC=AP%NBC
    NINT=AP%NINT
    NDM=AP%NDM
    NFPR=AP%NFPR
    NPARU=AP%NPAR-AP%NPARI

    IF(ISW>0) THEN
       !        ** restart
       CALL STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)
       RETURN
    ENDIF

    ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR), &
         UDOTPSR(NDIM,0:NCOLRS*NTSR),TMR(0:NTSR))
    !        ** start
    NBC0=(4*NBC-NINT-5*NDM+2)/15
    NNT0=(-NBC+4*NINT+5*NDM-23)/15
    NFPX=NBC0+NNT0-NDM+1

    ALLOCATE(ICPRS(NFPR),RLCUR(NFPR),RLDOTRS(NFPR))

    ! Start
    
    ! ** allocation
    ALLOCATE(UPST(NDM,0:NTSR*NCOLRS),UDOTPST(NDM,0:NTSR*NCOLRS))
    ALLOCATE(UPOLDPT(NDM,0:NTSR*NCOLRS))
    ALLOCATE(VDOTPST(NDM,0:NTSR*NCOLRS),RVDOT(NFPX))
    ALLOCATE(THU1(NDM),THL1(NFPX))
    ALLOCATE(P0(NDM,NDM),P1(NDM,NDM))
    ALLOCATE(U(NDM),DTM(NTSR))

    ! ** read the std branch
    CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPST, &
         UDOTPST,TMR,ITPRS,NDM)

    DO I=1,NTSR
       DTM(I)=TMR(I)-TMR(I-1)
    ENDDO

    DO I=1,NFPX
       RLCUR(I)=PAR(ICPRS(I))
    ENDDO

    ! Compute the second null vector

    ! ** redefine AP
    AP2=AP
    AP2%NDIM=NDM
    AP2%NTST=NTSR
    AP2%NCOL=NCOLRS
    AP2%NBC=NBC0
    AP2%NINT=NNT0
    AP2%NFPR=NFPX

    ! ** compute UPOLDP
    IF(NNT0>0) THEN
       DO J=0,NTSR*NCOLRS
          U(:)=UPST(:,J)
          CALL FUNI(AP2,NDM,U,U,ICPRS,PAR,0,UPOLDPT(1,J),DUM,DUM)
       ENDDO
    ENDIF

    ! ** unit weights
    THL1(1:NFPX)=1.d0
    THU1(1:NDM)=1.d0

    ! ** call SOLVBV
    RDSZ=0.d0
    NLLV=1
    IFST=1
    CALL SOLVBV(IFST,AP2,DET,PAR,ICPRS,FUNI,BCNI,ICNI,RDSZ,NLLV, &
         RLCUR,RLCUR,RLDOTRS,NDM,UPST,UPST,UDOTPST,UPOLDPT, &
         DTM,VDOTPST,RVDOT,P0,P1,THL1,THU1)

    !        ** normalization
    CALL SCALEB(NTSR,NCOLRS,NDM,NFPX,UDOTPST,RLDOTRS,DTM,THL1,THU1)
    CALL SCALEB(NTSR,NCOLRS,NDM,NFPX,VDOTPST,RVDOT,DTM,THL1,THU1)

    !        ** init UPS,PAR
    UPSR(1:NDM,:)=UPST(:,:)
    UPSR(NDM+1:2*NDM,:)=0.d0
    UPSR(2*NDM+1:3*NDM,:)=UDOTPST(:,:)
    UPSR(3*NDM+1:4*NDM,:)=VDOTPST(:,:)
    UDOTPSR(:,:)=0.d0

    !        ** init a,d,b
    PAR(NPARU+1:NPARU+2*NFPX+NDM+1)=0
    !        ** init q,r
    PAR(NPARU+2*NFPX+NDM+2:NPARU+3*NFPX+NDM+1)=RLDOTRS(1:NFPX)
    PAR(NPARU+3*NFPX+NDM+2:NPARU+4*NFPX+NDM+1)=RVDOT(1:NFPX)
    !        ** init c1,c2
    PAR(NPARU+4*NFPX+NDM+2:NPARU+4*NFPX+NDM+3)=0

    RLDOT(1:NFPX+1)=0
    RLDOT(NFPX+2)=1 ! b
    RLDOT(NFPX+3:5*NFPX+NDM+3)=0

    DEALLOCATE(UPST,UPOLDPT,UDOTPST,VDOTPST,RVDOT)
    DEALLOCATE(THU1,THL1)
    DEALLOCATE(P0,P1)
    DEALLOCATE(U,DTM)

    NODIR=0

    DEALLOCATE(ICPRS,RLDOTRS,RLCUR)

    CALL ADAPT2(NTSR,NCOLRS,NDIM,AP%NTST,AP%NCOL,NDIM, &
         TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
    DEALLOCATE(TMR,UPSR,UDOTPSR)
  END SUBROUTINE STPNBBP

END MODULE TOOLBOXBV
