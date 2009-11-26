!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    General Support Routines
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE SUPPORT

USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

IMPLICIT NONE
PRIVATE
PUBLIC :: MUELLER, EIG, PI, GESC, GELI, GEL, NLVC, NRMLZ, RNRMV
PUBLIC :: CHECKSP, INITSTOPCNTS, STOPPED
PUBLIC :: DTV,AV,P0V,P1V,EVV
 
DOUBLE PRECISION, POINTER, SAVE :: DTV(:),P0V(:,:),P1V(:,:)
TYPE(AUTOPARAMETERS), POINTER, SAVE :: AV
COMPLEX(KIND(1.0D0)), POINTER, SAVE :: EVV(:)

CONTAINS

! ---------- -------
  SUBROUTINE MUELLER(Q0,Q1,Q,S0,S1,S,RDS)

! Mueller's method with bracketing

    DOUBLE PRECISION, INTENT(IN) :: Q,S
    DOUBLE PRECISION, INTENT(INOUT) :: Q0,Q1,S0,S1
    DOUBLE PRECISION, INTENT(OUT) :: RDS

    DOUBLE PRECISION, PARAMETER :: RSMALL=1.0d-30
    DOUBLE PRECISION H0,H1,D,A,B,C,R,DQ

    H0=S0-S
    H1=S1-S
    D=H0*H1*(H1-H0)
    A=( H1**2*(Q0-Q) - H0**2*(Q1-Q) ) / D
    B=(-H1*(Q0-Q)    + H0*(Q1-Q)    ) / D
    IF(ABS(B).LE.RSMALL)THEN
       RDS=-Q/A
    ELSE
       C=A/(2*B)
       R=DSQRT(C**2-Q/B)
       IF(C.LT.0.d0)THEN
          RDS=-C - R
       ELSE
          RDS=-C + R
       ENDIF
    ENDIF

    DQ=Q1*Q
    IF(DQ<0.d0)THEN
       Q0=Q1
       S0=S1
    ENDIF
    Q1=Q
    S1=S
  END SUBROUTINE MUELLER

! ---------- ---
  SUBROUTINE EIG(AP,NDIM,M1A,A,EV)

! This subroutine uses the LAPACK subroutine DGEEV to compute the
! eigenvalues of the general real matrix A.
! NDIM is the dimension of A.
! M1A is the first dimension of A as in the DIMENSION statement.
! The eigenvalues are to be returned in the complex vector EV.
    
    USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM, M1A
    DOUBLE PRECISION, INTENT(INOUT) :: A(M1A,NDIM)
    COMPLEX(KIND(1.0D0)), INTENT(OUT) :: EV(NDIM)

! Local
    INTEGER IID,IBR,NTOT,NTOP,IER,MATZ,I,IV1(1)
    DOUBLE PRECISION, ALLOCATABLE :: WR(:),WI(:),Z(:,:)
    DOUBLE PRECISION FV1(1)
    ALLOCATE(WR(NDIM),WI(NDIM),Z(M1A,NDIM))

    IID=AP%IID
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    IER=0
    IF(IID>=4)THEN 
       MATZ=1
    ELSE
       MATZ=0
    ENDIF

    CALL RG(M1A,NDIM,A,WR,WI,MATZ,Z,IV1,FV1,IER)
    IF(IER/=0)WRITE(9,'(I4,I6,A)') IBR,NTOP,&
         'NOTE:Error return from EISPACK routine RG'
    IF(MATZ/=0)THEN
       WRITE(9,'(A)')' Eigenvalues:'
       DO I=1,NDIM
          WRITE(9,'(4X,7ES19.10)')WR(I),WI(I)
       ENDDO
       WRITE(9,'(A)')' Eigenvectors (by row):'
       DO I=1,NDIM
          WRITE(9,'(4X,7ES19.10)')Z(:NDIM,I)
       ENDDO
    ENDIF

    DO I=1,NDIM
       EV(I) = CMPLX(WR(I),WI(I),KIND(1.0D0))
    ENDDO

    DEALLOCATE(WR,WI,Z)
  END SUBROUTINE EIG

! ---------- ------
  SUBROUTINE NULLVC(m,n,k,A,u,ic)

! Finds a null-vector of a singular matrix A.
! The null space of A is assumed to be K-dimensional.
!
! Parameters :
!
!     m : number of equations,
!     k : dimension of nullspace,
!     A : m * n matrix of coefficients,
!     u : on exit U contains the null vector,
!    ic : integer array of dimension at least N.
!

    INTEGER, INTENT(IN) :: m,n,k
    DOUBLE PRECISION, INTENT(INOUT) :: A(m,n)
    INTEGER, INTENT(OUT) :: ic(n)
    DOUBLE PRECISION, INTENT(OUT) :: U(n)

    INTEGER i,j,jj,kk,l,ipiv,jpiv
    DOUBLE PRECISION p,piv,rm,sm,tmp,amax

    DO i=1,n
       ic(i)=i
    ENDDO

!   Elimination.

    amax = 0
    DO JJ=1,N-K
       IPIV=JJ
       JPIV=JJ
       PIV=0.d0
       DO i=jj,m
          DO j=jj,n
             p=ABS(A(i,ic(j)))
             IF(p>piv)THEN
                piv=p
                ipiv=i
                jpiv=j
             ENDIF
          ENDDO
       ENDDO
       IF(JJ==1)THEN
          ! now piv is ||A||_max
          amax = piv
       ENDIF
       IF(piv<=amax/HUGE(piv))THEN
          WRITE(9,"(8x,A,I3,A,ES11.4E3,A,ES11.4E3,A,ES11.4E3,A/A)") &
               ' NOTE:Pivot ',jj,' = ',piv,' <= ||A||_max/',HUGE(piv),' = ', &
               amax/HUGE(piv),' in NLVC : ',&
               '        A null space may be multi-dimensional'
          ! to avoid overflow in most cases
          piv = MAX(amax/HUGE(piv),TINY(piv))
          A(ipiv,ic(jpiv)) = piv
       ENDIF

       IF(jj/=ipiv)THEN
          DO i=1,n
             tmp=A(jj,i)
             A(jj,i)=A(ipiv,i)
             A(ipiv,i)=tmp
          ENDDO
       ENDIF

       IF(jj/=jpiv)THEN
          kk=ic(jj)
          ic(jj)=ic(jpiv)
          ic(jpiv)=kk
       ENDIF

       piv=A(jj,ic(jj))
       DO l=jj+1,m
          rm=A(l,ic(jj))/piv
          IF(rm/=0.d0)THEN
             DO i=jj+1,n
                A(l,ic(i))=A(l,ic(i))-rm*A(jj,ic(i))
             ENDDO
          ENDIF
       ENDDO
    ENDDO

!   Backsubstitution :

    DO i=n,n-k+1,-1
       u(ic(i))=1.d0
    ENDDO

    DO i=n-k,1,-1
       sm=0.d0
       DO j=i+1,n
          sm=sm+A(i,ic(j))*u(ic(j))
       ENDDO
       u(ic(i))=-sm/A(i,ic(i))
    ENDDO

  END SUBROUTINE NULLVC

! ---------- ----
  SUBROUTINE NLVC(m,n,k,A,U)

! Finds a null-vector of a singular matrix A.
! The null space of A is assumed to be k-dimensional.
!
! Parameters :
!
!     M : number of equations,
!     K : dimension of nullspace,
!     A : M * N matrix of coefficients,
!     U : on exit U contains the null vector,
!
    INTEGER, INTENT(IN) :: m,n,k
    DOUBLE PRECISION, INTENT(INOUT) :: A(m,n)
    DOUBLE PRECISION, INTENT(OUT) :: U(n)

    INTEGER, ALLOCATABLE :: ic(:)

    ALLOCATE(ic(n))
    CALL NULLVC(m,n,k,A,U,ic)
    DEALLOCATE(ic)

  END SUBROUTINE NLVC

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RNRMV(N,V)

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(IN) :: V(N)

    INTEGER I

! Returns the L2-norm of the vector V.

    RNRMV = 0.d0
    DO I=1,N
       RNRMV=RNRMV+V(I)**2
    ENDDO
    RNRMV=SQRT(RNRMV)

  END FUNCTION RNRMV

! ---------- -----
  SUBROUTINE NRMLZ(NDIM,V)

    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: V(NDIM)

    INTEGER I
    DOUBLE PRECISION SS

! Scale the vector V so that its discrete L2-norm becomes 1.

    SS=0.d0
    DO I=1,NDIM
       SS=SS+V(I)*V(I)
    ENDDO
    V(:)=V(:)/SQRT(SS)

  END SUBROUTINE NRMLZ

! ------ --------- --------
  DOUBLE PRECISION FUNCTION PI(R)

    DOUBLE PRECISION, INTENT(IN) :: R
    PI=R*4.0d0*ATAN(1.d0)

  END FUNCTION PI

! ---------- ----
  SUBROUTINE GELI(N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET,SCALE)

! Solves the linear system  A U = F by Gauss elimination
! with complete pivoting. Optionally returns a scaled determinant.
!
! Parameters :
!
!   N   : number of equations,
!   M1A : first dimension of A from DIMENSION statement,
!   A   : N * N matrix of coefficients,
!   NRHS: 0   if no right hand sides (determinant only),
!         >0   if there are NRHS right hand sides,
!   NDX : first dimension of U from DIMENSION statement,
!   U   : on exit U contains the solution vector(s),
!   M1F : first dimension of F from DIMENSION statement,
!   F   : right hand side vector(s),
!  IR,IC: integer vectors of dimension at least N.
!
! The input matrix A is overwritten.

    INTEGER, INTENT(IN) :: N,M1A,NRHS,NDX,M1F
    DOUBLE PRECISION, INTENT(INOUT) :: A(M1A,N),F(M1F,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(NDX,NRHS),DET
    INTEGER, INTENT(OUT) :: IR(N),IC(N)
    LOGICAL, INTENT(IN) :: SCALE

    INTEGER I,J,K,L,JJ,IRH,IPIV,JPIV
    DOUBLE PRECISION P,PIV,AP,RM,SM,amax
    DOUBLE PRECISION, EXTERNAL :: DLAMCH

    DO I=1,N
       IC(I)=I
       IR(I)=I
    ENDDO

!   Elimination.

    DET=1.d0

    amax = ABS(A(1,1))
    DO JJ=1,N-1
       IPIV=JJ
       JPIV=JJ
       PIV=0.d0
       DO I=JJ,N
          DO J=JJ,N
             P=ABS(A(IR(I),IC(J)))
             IF(P.GT.PIV)THEN
                PIV=P
                IPIV=I
                JPIV=J
             ENDIF
          ENDDO
       ENDDO

       AP=A(IR(IPIV),IC(JPIV))
       IF(SCALE)THEN
          DET=DET*LOG10(10+ABS(AP)) * atan(AP)
       ELSE
          DET=DET*AP
       ENDIF
       IF(IPIV.NE.JJ)DET=-DET
       IF(JPIV.NE.JJ)DET=-DET

       IF(JJ==1)THEN
          ! now PIV is ||A||_max
          amax = PIV
       ENDIF
       IF(PIV<=amax/HUGE(PIV))THEN
          WRITE(9,"(8x,A,I3,A,ES11.4E3,A,ES11.4E3,A,ES11.4E3,A)")&
               ' NOTE:Pivot ',JJ,' = ',PIV,' <= ||A||_max/',HUGE(PIV),' = ', &
               amax/HUGE(PIV),' in GE'
          ! to avoid overflow in most cases
          A(IR(IPIV),IC(JPIV)) = MAX(amax/HUGE(PIV),TINY(PIV))
       ENDIF
       K=IR(JJ)
       IR(JJ)=IR(IPIV)
       IR(IPIV)=K

       K=IC(JJ)
       IC(JJ)=IC(JPIV)
       IC(JPIV)=K

       DO L=JJ+1,N
          RM=A(IR(L),IC(JJ))/A(IR(JJ),IC(JJ))
          IF(RM.NE.0.d0)THEN
             DO I=JJ+1,N
                A(IR(L),IC(I))=A(IR(L),IC(I))-RM*A(IR(JJ),IC(I))
             ENDDO
             DO IRH=1,NRHS
                F(IR(L),IRH)=F(IR(L),IRH)-RM*F(IR(JJ),IRH)
             ENDDO
          ENDIF
       ENDDO
    ENDDO
    AP=A(IR(N),IC(N))
    IF(ABS(AP)<=amax/HUGE(AP))THEN
       WRITE(9,"(8x,A,I3,A,ES11.4E3,A,ES11.4E3,A,ES11.4E3,A)")&
            ' NOTE:Pivot ',N,' = ',ABS(AP),' <= ||A||_max/',HUGE(AP),' = ', &
            amax/HUGE(AP),' in GE'
       ! to avoid overflow in most cases
       A(IR(N),IC(N)) = MAX(amax/HUGE(AP),TINY(AP))
    ENDIF
    IF(SCALE)THEN
       DET=DET*LOG10(10+ABS(AP)) * atan(AP)
    ELSE
       DET=DET*AP
    ENDIF

!   Backsubstitution :

    DO IRH=1,NRHS
       U(IC(N),IRH)=F(IR(N),IRH)/A(IR(N),IC(N))
       DO I=N-1,1,-1
          SM=0.d0
          DO J=I+1,N
             SM=SM+A(IR(I),IC(J))*U(IC(J),IRH)
          ENDDO
          U(IC(I),IRH)=(F(IR(I),IRH)-SM)/A(IR(I),IC(I))
       ENDDO
    ENDDO

  END SUBROUTINE GELI

! ---------- ---
  SUBROUTINE GEL(N,A,NRHS,U,F,DET)
    INTEGER, INTENT(IN) :: N,NRHS
    DOUBLE PRECISION, INTENT(INOUT) :: A(N,N),F(N,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(N,NRHS),DET
    INTEGER, ALLOCATABLE :: IR(:),IC(:)
    ALLOCATE(IR(N),IC(N))
    CALL GELI(N,N,A,NRHS,N,U,N,F,IR,IC,DET,.FALSE.)
    DEALLOCATE(IR,IC)
  END SUBROUTINE GEL

! ---------- ----
  SUBROUTINE GESC(N,A,NRHS,U,F,DET)
    INTEGER, INTENT(IN) :: N,NRHS
    DOUBLE PRECISION, INTENT(INOUT) :: A(N,N),F(N,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(N,NRHS),DET
    INTEGER, ALLOCATABLE :: IR(:),IC(:)
    ALLOCATE(IR(N),IC(N))
    CALL GELI(N,N,A,NRHS,N,U,N,F,IR,IC,DET,.TRUE.)
    DEALLOCATE(IR,IC)
  END SUBROUTINE GESC

! ------- -------- -------
  LOGICAL FUNCTION CHECKSP(ITP,IPS,ILP,ISP)
    USE AUTO_CONSTANTS, ONLY : SP
    INTEGER, INTENT(IN) :: ITP,IPS,ILP,ISP

    ! determine if the given TY label needs to be checked
    CHARACTER(LEN=2), PARAMETER :: ATYPES(-3:8) = &
         (/ 'BT','CP','  ','  ','BP','LP','HB','UZ','LP','BP','PD','TR' /)
    CHARACTER(LEN=2) ATYPE
    INTEGER NTY,I,M

    NTY=MOD(ITP,10)

    IF(NTY==-4)THEN
       CHECKSP=.TRUE.
       RETURN
    ENDIF

    CHECKSP = .FALSE.
    SELECT CASE(NTY)
    CASE(-2,1) ! BP, CP, ZH
       IF(ITP==-32)THEN
          CHECKSP = ABS(IPS)==1.OR.IPS==11 ! ZH
       ELSE
          CHECKSP = ISP/=0
       ENDIF
    CASE(2,5) ! LP, GH
       IF(ITP==35)THEN
          CHECKSP = ABS(IPS)==1.OR.IPS==11 ! GH
       ELSE
          CHECKSP = ILP/=0
       ENDIF
    CASE(-3,3) ! BT, Hopf, ZH
       CHECKSP = ABS(IPS)==1.OR.IPS==11
    CASE(6) ! BP (BVP)
       CHECKSP = ABS(ISP)>=2.AND.ABS(ISP)/=4
    CASE(7,8) ! PD,TR
       CHECKSP = ISP/=0 .AND. (IPS==-1.OR.IPS==2.OR.IPS==7.OR.IPS==12)
    END SELECT

    IF(ITP==23.OR.ITP==-32)THEN
       ATYPE='ZH'
    ELSEIF(ITP==35)THEN
       ATYPE='GH'
    ELSEIF(ITP==58)THEN
       ATYPE='R1'
    ELSEIF(ITP==78)THEN
       ATYPE='R2'
    ELSE
       ATYPE=ATYPES(NTY)
    ENDIF
    DO I=1,SIZE(SP)
       IF (SP(I)(1:2)==ATYPE) THEN
          CHECKSP=.TRUE.
          IF (LEN_TRIM(SP(I))>2)THEN
             READ(SP(I)(3:),*)M
             IF(M==0)THEN
                CHECKSP=.FALSE.
             ENDIF
          ENDIF
          EXIT
       ENDIF
    ENDDO
  END FUNCTION CHECKSP

! ---------- ------------
  SUBROUTINE INITSTOPCNTS(COUNTS)
    USE AUTO_CONSTANTS, ONLY : STOPS, SP
    INTEGER, INTENT(OUT) :: COUNTS(-9:9)

    ! initialize the COUNTS array that determines when we need to stop
    ! at a special point
    CHARACTER(LEN=2), PARAMETER :: ATYPES(-9:9) = &
         (/ 'MX','  ','  ','  ','  ','UZ','  ','  ','  ', '  ', &
            'BP','LP','HB','UZ','LP','BP','PD','TR','EP' /)
    INTEGER NTY,I

    COUNTS(:) = 0
    ! look both at SP and STOP: SP is for backwards compatibility
    ! but STOP is preferred
    DO I=1,SIZE(SP)
       IF (LEN_TRIM(SP(I))>2) THEN
          DO NTY=-9,9
             IF(SP(I)(1:2)==ATYPES(NTY)) THEN
                READ(SP(I)(3:),*)COUNTS(NTY)
             ENDIF
          ENDDO
       ENDIF
    ENDDO
    DO I=1,SIZE(STOPS)
       IF (LEN_TRIM(STOPS(I))>2) THEN
          DO NTY=-9,9
             IF(STOPS(I)(1:2)==ATYPES(NTY)) THEN
                READ(STOPS(I)(3:),*)COUNTS(NTY)
             ENDIF
          ENDDO
       ENDIF
    ENDDO
  END SUBROUTINE INITSTOPCNTS

! ------- -------- -------
  LOGICAL FUNCTION STOPPED(ITP,COUNTS)
    INTEGER, INTENT(IN) :: ITP
    INTEGER, INTENT(INOUT) :: COUNTS(-9:9)

    ! determine if the given TY label has been reached n times so
    ! we need to stop
    INTEGER NTY

    NTY = MOD(ITP,10)
    STOPPED = .FALSE.
    IF (COUNTS(NTY) > 0) THEN
       IF (COUNTS(NTY)==1) THEN
          STOPPED=.TRUE.
       ENDIF
       COUNTS(NTY) = COUNTS(NTY) - 1
    ENDIF
  END FUNCTION STOPPED

END MODULE SUPPORT

! The following functions and subroutines are called from
! various demos so cannot be inside the module

! ---------- --
  SUBROUTINE GE(IAM,N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
    USE SUPPORT
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IAM,N,M1A,NRHS,NDX,M1F
    DOUBLE PRECISION, INTENT(INOUT) :: A(M1A,N),F(M1F,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(NDX,NRHS),DET
    INTEGER, INTENT(OUT) :: IR(N),IC(N)
    CALL GELI(N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET,.FALSE.)
  END SUBROUTINE GE

! ------ --------- -------- ----
  DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)

    USE MESH
    USE SUPPORT
    IMPLICIT NONE

    CHARACTER*3, INTENT(IN) :: CODE
    INTEGER, INTENT(IN) :: IC
    DOUBLE PRECISION, INTENT(IN) :: UPS(AV%NDIM*AV%NCOL,*)

    INTEGER NDIM,IPS,NTST,NCOL,NBC,NINT
    DOUBLE PRECISION WI(0:AV%NCOL)

    NDIM=AV%NDIM
    IPS=AV%IPS
    NTST=AV%NTST
    NCOL=AV%NCOL
    NBC=AV%NBC
    NINT=AV%NINT

    GETP=0

    IF( IPS<=1 .OR. IPS==5 .OR. IPS==11)THEN
       SELECT CASE(CODE)
       CASE('NRM','nrm')
          GETP=ABS(UPS(IC,1))
       CASE('INT','int','MAX','max','MIN','min','BV1','bv1')
          GETP=UPS(IC,1)
       CASE('HBF','hbf')
          GETP=AV%HBFF
       CASE('BIF','bif')
          GETP=AV%DET
       CASE('SPB','spb','MXT','mxt','MNT','mnt','DTM','dtm','WIN','win')
          GETP=0.
       END SELECT
    ELSE
       SELECT CASE(CODE)
       CASE('NRM','nrm')
          GETP=RNRM2(NTST,NCOL,NDIM,IC,UPS,DTV)
       CASE('INT','int')
          GETP=RINTG(NTST,NCOL,NDIM,IC,UPS,DTV)
       CASE('MAX','max')
          GETP=RMXUPS(NTST,NCOL,NDIM,IC,UPS)
       CASE('MIN','min')
          GETP=RMNUPS(NTST,NCOL,NDIM,IC,UPS)
       CASE('MXT','mxt')
          GETP=RMXUPST(NTST,NCOL,NDIM,IC,UPS,DTV)
       CASE('MNT','mnt')
          GETP=RMNUPST(NTST,NCOL,NDIM,IC,UPS,DTV)
       CASE('DTM','dtm')
          GETP=DTV(IC)
       CASE('WIN','win')
          CALL WINT(NCOL,WI)
          GETP=WI(IC)
       CASE('BV1','bv1')
          GETP=UPS(IC,AV%NTST+1)
       CASE('HBF','hbf')
          GETP=0.d0
       CASE('BIF','bif')
          GETP=AV%BIFF
       CASE('SPB','spb')
          GETP=AV%SPBF
       END SELECT
    ENDIF
    SELECT CASE(CODE) 
    CASE('BV0','bv0')
       GETP=UPS(IC,1)
    CASE('STP','stp')
       GETP=AV%RDS
    CASE('FLD','fld')
       GETP=AV%FLDF
    CASE('STA','sta')
       GETP=AV%NINS
    CASE('EIG','eig')
       IF(MOD(IC,2)==1)THEN
          GETP=REAL(EVV((IC+1)/2))
       ELSE
          GETP=AIMAG(EVV(IC/2))
       ENDIF
    CASE('NDI','ndi','NDX','ndx')
       GETP=NDIM
    CASE('NTS','nts')
       GETP=NTST
    CASE('NCO','nco')
       GETP=NCOL
    CASE('NBC','nbc')
       GETP=NBC
    CASE('NIN','nin')
       GETP=NINT
    END SELECT

  END FUNCTION GETP

! ---------- -------
  SUBROUTINE GETMDMX(NDIM1,P0,P1,NMM)
    USE SUPPORT
    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(OUT) :: P0(NDIM1,NDIM1),P1(NDIM1,NDIM1)
    INTEGER, INTENT(IN) :: NDIM1
    LOGICAL, INTENT(OUT) :: NMM

    INTEGER NDIM,IPS,ISP,NTOT

    NDIM=AV%NDIM
    IPS=AV%IPS
    ISP=AV%ISP
    NTOT=AV%NTOT
    NMM=.FALSE.
    IF(NDIM==NDIM1.AND.NTOT>0.AND.ABS(ISP)>0.AND. &
         (IPS==2.OR.IPS==7.OR.IPS==12))THEN
       P0=P0V
       P1=P1V
       NMM=.TRUE.
    ENDIF

  END SUBROUTINE GETMDMX
!
! because some demos call RG here is a wrapper around DGEEV
!
  SUBROUTINE RG(M1A,NDIM,A,WR,WI,MATZ,Z,IV1,FV1,IER)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: M1A,NDIM,MATZ,IV1(NDIM)
    DOUBLE PRECISION, INTENT(IN) :: FV1(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: A(M1A,NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: WR(NDIM),WI(NDIM),Z(M1A,NDIM)
    INTEGER, INTENT(OUT) :: IER

    CHARACTER(1) JOBVR
    INTEGER LWORK
    DOUBLE PRECISION, ALLOCATABLE :: WORK(:)

    IF(MATZ==1)THEN
       JOBVR='V'
    ELSE
       JOBVR='N'
    ENDIF
    ALLOCATE(WORK(1))
    CALL DGEEV('N',JOBVR,NDIM,A,M1A,WR,WI,Z,M1A,Z,M1A,WORK,-1,IER)
    LWORK=NINT(WORK(1))
    DEALLOCATE(WORK)
    ALLOCATE(WORK(LWORK))
    CALL DGEEV('N',JOBVR,NDIM,A,M1A,WR,WI,Z,M1A,Z,M1A,WORK,LWORK,IER)
    DEALLOCATE(WORK)

  END SUBROUTINE RG
