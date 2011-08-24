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
PUBLIC :: MUELLER, EIG, PI, GESC, GELI, GEL, NLVC, NULLVC, NRMLZ, RNRMV
PUBLIC :: LBTYPE, LBITP, CHECKSP, INITSTOPCNTS, STOPPED, FNCS, INIT2, INIT3
PUBLIC :: NAMEIDX, PVLI, DIRECTION, AUTOSTOP, EXPANDJAC
PUBLIC :: DTV,AV,P0V,P1V,EVV
 
DOUBLE PRECISION, ALLOCATABLE, SAVE :: DTV(:),P0V(:,:),P1V(:,:)
TYPE(AUTOPARAMETERS), SAVE :: AV
COMPLEX(KIND(1.0D0)), ALLOCATABLE, SAVE :: EVV(:)

CHARACTER(LEN=3), PARAMETER :: ATYPES(-9:14) = &
     (/ 'MX ','R4 ','R3 ','R2 ','R1 ','UZ ','ZH ','CP ','BT ','   ', &
        'BP ','LP ','HB ','   ','LP ','BP ','PD ','TR ','EP ', &
        'GH ','LPD','LTR','PTR','TTR' /)

CONTAINS

! ---------- -------
  SUBROUTINE MUELLER(Q0,Q1,Q,S0,S1,S,RDS)

! Mueller's method with bracketing

    DOUBLE PRECISION, INTENT(IN) :: Q,S
    DOUBLE PRECISION, INTENT(INOUT) :: Q0,Q1,S0,S1
    DOUBLE PRECISION, INTENT(OUT) :: RDS

    DOUBLE PRECISION H0,H1,D,A,B,R,DQ

    H0=S0-S
    H1=S1-S
    D=H0*H1*(H1-H0)
    A=( H1**2*(Q0-Q) - H0**2*(Q1-Q) ) / D
    B=(-H1*(Q0-Q)    + H0*(Q1-Q)    ) / D
    IF(A**2>4*B*Q)THEN
       R=SQRT(A**2-4*B*Q)
       IF(A<0)THEN
          R=-R
       ENDIF
    ELSE
       R=0
    ENDIF
    RDS=-2*Q / (A+R)

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
  SUBROUTINE NULLVC(m,n,k,A,u,icpiv,irpiv)

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
    INTEGER, INTENT(OUT) :: icpiv(n), irpiv(n)
    DOUBLE PRECISION, INTENT(OUT) :: U(n)

    INTEGER i,j,jj,l,ipiv,jpiv
    DOUBLE PRECISION p,piv,rm,sm,tmp,amax

!   Elimination.

    amax = 0
    DO JJ=1,N-K
       IPIV=JJ
       JPIV=JJ
       PIV=0.d0
       DO i=jj,m
          DO j=jj,n
             p=ABS(A(i,j))
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
          A(ipiv,jpiv) = piv
       ENDIF

       irpiv(jj)=ipiv
       IF(jj/=ipiv)THEN
          DO i=1,n
             tmp=A(jj,i)
             A(jj,i)=A(ipiv,i)
             A(ipiv,i)=tmp
          ENDDO
       ENDIF

       icpiv(jj)=jpiv
       IF(jj/=jpiv)THEN
          DO i=1,m
             tmp=A(i,jj)
             A(i,jj)=A(i,jpiv)
             A(i,jpiv)=tmp
          ENDDO
       ENDIF

       piv=A(jj,jj)
       DO l=jj+1,m
          rm=A(l,jj)/piv
          IF(rm/=0.d0)THEN
             DO i=jj+1,n
                A(l,i)=A(l,i)-rm*A(jj,i)
             ENDDO
          ENDIF
       ENDDO
    ENDDO

!   Backsubstitution :

    DO i=n,n-k+1,-1
       u(i)=1.d0
    ENDDO

    DO i=n-k,1,-1
       sm=0.d0
       DO j=i+1,n
          sm=sm+A(i,j)*u(j)
       ENDDO
       u(i)=-sm/A(i,i)
    ENDDO

!   undo column swapping on u

    DO i=n-k,1,-1
       jpiv=icpiv(i)
       IF(jpiv/=i)THEN
          tmp=u(i)
          u(i)=u(jpiv)
          u(jpiv)=tmp
       ENDIF
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

    INTEGER, ALLOCATABLE :: icpiv(:),irpiv(:)

    ALLOCATE(icpiv(n),irpiv(n))
    CALL NULLVC(m,n,k,A,U,icpiv,irpiv)
    DEALLOCATE(icpiv,irpiv)

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

! ---------- ---------
  SUBROUTINE EXPANDJAC(JAC,M,N1,N2)

! The Jacobian JAC(N1,M) is unpacked to JAC(N2,M) where N2 > N1

    INTEGER, INTENT(IN) :: M, N1, N2
    DOUBLE PRECISION, INTENT(INOUT) :: JAC(M*N2)

    INTEGER I, J

    DO I=M-1,1,-1
       DO J=N1,1,-1
          JAC(I*N2+J)=JAC(I*N1+J)
       ENDDO
    ENDDO
  END SUBROUTINE EXPANDJAC

! ---------- ----
  SUBROUTINE GELI(N,M1A,A,NRHS,NDX,U,M1F,F,ICPIV,DET,SCALE)

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
!  ICPIV: integer vector of dimension at least N.
!
! The input matrix A is overwritten.

    INTEGER, INTENT(IN) :: N,M1A,NRHS,NDX,M1F
    DOUBLE PRECISION, INTENT(INOUT) :: A(M1A,N),F(M1F,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(NDX,NRHS),DET
    INTEGER, INTENT(OUT) :: ICPIV(N)
    LOGICAL, INTENT(IN) :: SCALE

    INTEGER I,J,L,JJ,IRH,IPIV,JPIV
    DOUBLE PRECISION P,PIV,AP,RM,SM,amax,TMP

!   Elimination.

    DET=1.d0

    amax = ABS(A(1,1))
    DO JJ=1,N-1
       IPIV=JJ
       JPIV=JJ
       PIV=0.d0
       DO I=JJ,N
          DO J=JJ,N
             P=ABS(A(I,J))
             IF(P.GT.PIV)THEN
                PIV=P
                IPIV=I
                JPIV=J
             ENDIF
          ENDDO
       ENDDO

       AP=A(IPIV,JPIV)
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
          A(IPIV,JPIV) = MAX(amax/HUGE(PIV),TINY(PIV))
       ENDIF

       IF(IPIV/=JJ)THEN
          ! swap rows
          DO I=1,N
             TMP=A(JJ,I)
             A(JJ,I)=A(IPIV,I)
             A(IPIV,I)=TMP
          ENDDO
          DO IRH=1,NRHS
             TMP=F(JJ,IRH)
             F(JJ,IRH)=F(IPIV,IRH)
             F(IPIV,IRH)=TMP
          ENDDO
       ENDIF

       ICPIV(JJ)=JPIV
       IF(JPIV/=JJ)THEN
          ! swap columns
          DO J=1,N
             TMP=A(J,JJ)
             A(J,JJ)=A(J,JPIV)
             A(J,JPIV)=TMP
          ENDDO
       ENDIF

       DO L=JJ+1,N
          RM=A(L,JJ)/A(JJ,JJ)
          IF(RM.NE.0.d0)THEN
             DO I=JJ+1,N
                A(L,I)=A(L,I)-RM*A(JJ,I)
             ENDDO
             DO IRH=1,NRHS
                F(L,IRH)=F(L,IRH)-RM*F(JJ,IRH)
             ENDDO
          ENDIF
       ENDDO
    ENDDO
    AP=A(N,N)
    IF(ABS(AP)<=amax/HUGE(AP))THEN
       WRITE(9,"(8x,A,I3,A,ES11.4E3,A,ES11.4E3,A,ES11.4E3,A)")&
            ' NOTE:Pivot ',N,' = ',ABS(AP),' <= ||A||_max/',HUGE(AP),' = ', &
            amax/HUGE(AP),' in GE'
       ! to avoid overflow in most cases
       A(N,N) = MAX(amax/HUGE(AP),TINY(AP))
    ENDIF
    IF(SCALE)THEN
       DET=DET*LOG10(10+ABS(AP)) * atan(AP)
    ELSE
       DET=DET*AP
    ENDIF

!   Backsubstitution :

    DO IRH=1,NRHS
       U(N,IRH)=F(N,IRH)/A(N,N)
       DO I=N-1,1,-1
          SM=0.d0
          DO J=I+1,N
             SM=SM+A(I,J)*U(J,IRH)
          ENDDO
          U(I,IRH)=(F(I,IRH)-SM)/A(I,I)
       ENDDO
    ENDDO

!   undo column swapping on U

    DO I=N-1,1,-1
       JPIV=ICPIV(I)
       IF(JPIV/=I)THEN
          DO IRH=1,NRHS
             TMP=U(I,IRH)
             U(I,IRH)=U(JPIV,IRH)
             U(JPIV,IRH)=TMP
          ENDDO
       ENDIF
    ENDDO

  END SUBROUTINE GELI

! ---------- ---
  SUBROUTINE GEL(N,A,NRHS,U,F,DET)
    INTEGER, INTENT(IN) :: N,NRHS
    DOUBLE PRECISION, INTENT(INOUT) :: A(N,N),F(N,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(N,NRHS),DET
    INTEGER, ALLOCATABLE :: ICPIV(:)
    ALLOCATE(ICPIV(N))
    CALL GELI(N,N,A,NRHS,N,U,N,F,ICPIV,DET,.FALSE.)
    DEALLOCATE(ICPIV)
  END SUBROUTINE GEL

! ---------- ----
  SUBROUTINE GESC(N,A,NRHS,U,F,DET)
    INTEGER, INTENT(IN) :: N,NRHS
    DOUBLE PRECISION, INTENT(INOUT) :: A(N,N),F(N,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(N,NRHS),DET
    INTEGER, ALLOCATABLE :: ICPIV(:)
    ALLOCATE(ICPIV(N))
    CALL GELI(N,N,A,NRHS,N,U,N,F,ICPIV,DET,.TRUE.)
    DEALLOCATE(ICPIV)
  END SUBROUTINE GESC

! ------------ -------- ------
  CHARACTER(3) FUNCTION LBTYPE(ITP)

    ! returns the string label type corresponding to numerical type ITP
    INTEGER, INTENT(IN) :: ITP

    SELECT CASE(ITP)
    CASE(-32)
       LBTYPE='GH'
    CASE(23,83)
       LBTYPE='LTR'
    CASE(77,87)
       LBTYPE='PTR'
    CASE(28,78)
       LBTYPE='LPD'
    CASE(88)
       LBTYPE='TTR'
    CASE DEFAULT
       LBTYPE=ATYPES(MOD(ITP,10))
    END SELECT
  END FUNCTION LBTYPE

! ------- -------- -----
  INTEGER FUNCTION LBITP(ATYPE,BVP) RESULT(ITP)

    ! returns the basic numeric type corresponding to string label ATYPE
    CHARACTER(LEN=*), INTENT(IN) :: ATYPE
    LOGICAL, INTENT(IN) :: BVP
    INTEGER NTY

    ITP=0
    SELECT CASE(TRIM(ATYPE))
    CASE('GH')
       ITP=-2
    CASE('LTR')
       ITP=3
    CASE('PTR')
       ITP=7
    CASE('LPD','TTR')
       ITP=8
    CASE DEFAULT
       DO NTY=-9,9
          IF(ATYPE==ATYPES(NTY).AND..NOT.(BVP.AND.(NTY==1.OR.NTY==2))) THEN
             ITP=NTY
             RETURN
          ENDIF
       ENDDO
    END SELECT
  END FUNCTION LBITP

! ------- -------- -------
  LOGICAL FUNCTION CHECKSP(ATYPE,IPS,ILP,ISP)
    USE AUTO_CONSTANTS, ONLY : SP
    CHARACTER(LEN=*), INTENT(IN) :: ATYPE
    INTEGER, INTENT(IN) :: IPS,ILP,ISP

    ! determine if the given TY label needs to be checked
    INTEGER I,J,M

    CHECKSP = .FALSE.
    SELECT CASE(TRIM(ATYPE))
    CASE('UZ')
       CHECKSP = .TRUE.
    CASE('HB','BT','ZH','GH','CP')
       CHECKSP = (IPS==-1.OR.IPS==1.OR.IPS==11).AND.ISP/=0.AND.ISP/=3
    CASE('LP')
       CHECKSP = ILP/=0
    CASE('BP')
       IF(IPS<2.OR.IPS==5.OR.IPS==11)THEN
          CHECKSP = ISP/=0 ! AE
       ELSE
          CHECKSP = ABS(ISP)>=2.AND.ABS(ISP)/=4 ! BVP
       ENDIF
    CASE('PD','TR','R1','R2','R3','R4','LPD','LTR','PTR','TTR')
       IF(IPS==-1)THEN ! maps
          CHECKSP = ISP/=0.AND.IPS/=3
       ELSE ! cycles
          CHECKSP = (ISP==2.OR.ISP==4) .AND. (IPS==2.OR.IPS==7.OR.IPS==12)
       ENDIF
    END SELECT

    DO I=1,SIZE(SP)
       J=SCAN(SP(I)(3:),"-0123456789")+2
       IF(J==2)THEN
          J=LEN_TRIM(SP(I))+1
       ENDIF
       IF (SP(I)(1:J-1)==TRIM(ATYPE)) THEN
          CHECKSP=.TRUE.
          IF (LEN_TRIM(SP(I))>J-1)THEN
             READ(SP(I)(J:),*)M
             IF(M==0)THEN
                CHECKSP=.FALSE.
             ENDIF
          ENDIF
          EXIT
       ENDIF
    ENDDO
  END FUNCTION CHECKSP

! ---------- ------------
  SUBROUTINE INITSTOPCNTS(ISP,ILP,ITPST,COUNTS)
    USE AUTO_CONSTANTS, ONLY : STOPS, SP
    INTEGER, INTENT(IN) :: ISP,ILP,ITPST
    INTEGER, INTENT(OUT) :: COUNTS(-9:14)

    ! initialize the COUNTS array that determines when we need to stop
    ! at a special point
    INTEGER NTY,I,J

    COUNTS(:) = 0

    ! initialize from IPS, ISP, ILP, ITPST
    IF(ISP<0)THEN
       COUNTS(-8:-5) = 1 ! R4,R3,R2,R1
       COUNTS(-3:-1) = 1 ! ZH,Cusp,BT
       COUNTS(1) = 1 ! BP (AE)
       COUNTS(3) = 1 ! HB (AE)
       COUNTS(6:8) = 1 ! BP(BVP),PD,TR
       COUNTS(10:14) = 1 ! GH,LPD,LTR,PTR,TTR
    ENDIF
    IF(ILP<0)THEN
       COUNTS(2) = 1 ! LP (AE)
       COUNTS(5) = 1 ! LP (BVP)
    ENDIF
    IF(ITPST==3)THEN
       !BT on Hopf bif must always stop it
       COUNTS(-1) = 1
    ENDIF

    ! look both at SP and STOP: SP is for backwards compatibility
    ! but STOP is preferred
    DO I=1,SIZE(SP)
       J=SCAN(SP(I)(3:),"-0123456789")+2
       IF (J>2) THEN
          DO NTY=-9,14
             IF(SP(I)(1:J-1)==TRIM(ATYPES(NTY))) THEN
                READ(SP(I)(J:),*)COUNTS(NTY)
             ENDIF
          ENDDO
       ENDIF
    ENDDO
    DO I=1,SIZE(STOPS)
       J=SCAN(STOPS(I)(3:),"-0123456789")+2
       IF (J>2) THEN
          DO NTY=-9,14
             IF(STOPS(I)(1:J-1)==TRIM(ATYPES(NTY))) THEN
                READ(STOPS(I)(J:),*)COUNTS(NTY)
             ENDIF
          ENDDO
       ENDIF
    ENDDO
  END SUBROUTINE INITSTOPCNTS

! ------- -------- -------
  LOGICAL FUNCTION STOPPED(IUZ,ITEST,NUZR,ITP,COUNTS)
    INTEGER, INTENT(IN) :: IUZ(*),ITEST,NUZR,ITP
    INTEGER, INTENT(INOUT) :: COUNTS(-9:10)

    ! determine if the given TY label has been reached n times so
    ! we need to stop
    INTEGER NTY

    IF(ITEST<=NUZR)THEN
       IF(IUZ(ITEST)<0)THEN
          STOPPED=.TRUE.
          RETURN
       ENDIF
    ENDIF
       
    SELECT CASE(ITP)
    CASE(-32)
       NTY=10 ! 'GH'
    CASE(28,78)
       NTY=11 ! 'LPD'
    CASE(23,83)
       NTY=12 ! 'LTR'
    CASE(77,87)
       NTY=13 ! 'PTR'
    CASE(88)
       NTY=14 ! 'TTR'
    CASE DEFAULT
       NTY=MOD(ITP,10)
    END SELECT

    STOPPED = .FALSE.
    IF (COUNTS(NTY) > 0) THEN
       IF (COUNTS(NTY)==1) THEN
          STOPPED=.TRUE.
       ENDIF
       COUNTS(NTY) = COUNTS(NTY) - 1
    ENDIF
  END FUNCTION STOPPED

! ------- -------- ---------
  LOGICAL FUNCTION DIRECTION(IIS, ITP)

    ! include direction info in solution files depending on IIS and
    ! the type of the point
    INTEGER, INTENT(IN) :: IIS, ITP

    INTEGER ITP0

    ITP0=MOD(ITP,10)
    DIRECTION=.TRUE.
    SELECT CASE(IIS)
    CASE(0) ! never
       DIRECTION=.FALSE.
    CASE(1) ! only for BP/LP(BVP)/PD/TR
       DIRECTION=ITP0==1.OR.(ITP0>=5.AND.ITP0<=8)
    CASE(2) ! for all points except regular (every NPR step)
       DIRECTION=ITP0/=4
    END SELECT
  END FUNCTION DIRECTION

! FNCS and FNUZ are the same for AE and BVP
! ------ --------- -------- ----
  DOUBLE PRECISION FUNCTION FNCS(AP,ICP,UPS,PAR,ATYPE,IUZ,VUZ,ITEST,FNCI)

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    DOUBLE PRECISION, INTENT(IN) :: UPS(*)
    INTEGER, INTENT(IN) :: IUZ(*),ICP(*),ITEST
    DOUBLE PRECISION, INTENT(IN) :: VUZ(*)

    INTEGER NUZR

    NUZR=AP%NUZR

    IF(ITEST<=NUZR)THEN
       FNCS=FNUZ(AP,PAR,ATYPE,IUZ,VUZ,ITEST)
    ELSE
       FNCS=FNCI(AP,ICP,UPS,AP%NDIM,PAR,ITEST-NUZR,ATYPE)
    ENDIF

  END FUNCTION FNCS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION FNUZ(AP,PAR,ATYPE,IUZ,VUZ,IUZR)

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    INTEGER, INTENT(IN) :: IUZ(*),IUZR
    DOUBLE PRECISION, INTENT(IN) :: VUZ(*)

    INTEGER IID,IBR,NTOT,NTOP

    ATYPE=''
    IF(.NOT.CHECKSP('UZ',AP%IPS,AP%ILP,AP%ISP))RETURN

    IID=AP%IID
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    FNUZ=PAR(ABS(IUZ(IUZR)))-VUZ(IUZR)
    ATYPE='UZ'

    IF(IID.GE.3)WRITE(9,101)ABS(IBR),NTOP+1,IUZR,FNUZ
101 FORMAT(I4,I6,9X,'User Func.',I3,1X,ES14.5)

  END FUNCTION FNUZ

! ---------- ----
  SUBROUTINE PVLI(AP,ICP,UPS,NDIM,PAR,FNCI)

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

    DOUBLE PRECISION Q
    CHARACTER(4) ATYPEDUM

    Q=FNCI(AP,ICP,UPS,NDIM,PAR,0,ATYPEDUM)

  END SUBROUTINE PVLI

! ------- -------- -------
  INTEGER FUNCTION NAMEIDX(NAME,NAMES)

    USE AUTO_CONSTANTS, ONLY: INDEXSTR
    CHARACTER(13), INTENT(IN) :: NAME
    TYPE(INDEXSTR), INTENT(IN) :: NAMES(:)
    CHARACTER(13) PNAME
    INTEGER I,SIGN,ios

    ! map a symbolic parameter name or an ascii integer to an index

    SIGN=1
    IF(NAME(1:1)=='-')THEN
       PNAME=NAME(2:)
       SIGN=-1
    ELSE
       PNAME=NAME
    ENDIF
    DO I=1,SIZE(NAMES)
       IF(NAMES(I)%STR==PNAME)THEN
          NAMEIDX=NAMES(I)%INDEX*SIGN
          RETURN
       ENDIF
    ENDDO
    IF(TRIM(PNAME)=='PERIOD')THEN
       NAMEIDX=SIGN*11
    ELSE
       READ(NAME,*,IOSTAT=ios)NAMEIDX
       IF(ios/=0)THEN
          WRITE(6,"(A,A,A)")"Name '",TRIM(NAME),"' not found."
          CALL AUTOSTOP()
       ENDIF
    ENDIF
  END FUNCTION NAMEIDX

! ---------- -----
  SUBROUTINE INIT2(AP,ICP,ICU)

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*), ICU(*)

    INTEGER I,IPS,NICP,NFPR,NPAR,NPARI

    ! check output (user-specified) parameters
    NICP=AP%NICP
    DO I=1,NICP
       IF(ICU(I)<=0)THEN
          WRITE(6,'(A,I5,A,I5)') &
                  "Invalid parameter index ",ICU(I), &
                  " specified in ICP index ",I
          CALL AUTOSTOP()
       ENDIF
    ENDDO
    ! check active continuation parameters
    NFPR=AP%NFPR
    DO I=NICP+1,NFPR
       IF(ICP(I)==0)THEN
          WRITE(6,'(A/A,I5,A,I5,A)') &
               "Insufficient number of parameters in ICP.", &
               "You specified ",NICP," but need at least ", &
               NFPR-I+1+NICP, " continuation parameters."
          CALL AUTOSTOP()
       ENDIF
    ENDDO
    NPARI=AP%NPARI
    NPAR=AP%NPAR
    NPAR=MAX(MAXVAL(ICP(:NFPR)),NPAR+NPARI)
    IPS=AP%IPS
    IF(ABS(IPS)==1.OR.IPS==2.OR.IPS>=7)THEN
       !HB period and period for periodic orbits stored in PAR(11)
       NPAR=MAX(11,NPAR)
       IF(CHECKSP('TR',IPS,AP%ILP,AP%ISP))THEN
          ! the torus angle is stored in PAR(12)
          NPAR=MAX(12,NPAR)
       ENDIF
    ENDIF
    AP%NPAR=NPAR
    AP%NTEST=AP%NTEST+AP%NUZR
  END SUBROUTINE INIT2

! ---------- -----
  SUBROUTINE INIT3(AP,ICP,PAR,THL,THU,IUZ,VUZ)

    USE AUTO_CONSTANTS, ONLY: IVTHL,IVTHU,IVUZR,IVUZSTOP,unames,parnames,SVFILE

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(:), THL(*), THU(*), VUZ(*)
    INTEGER, INTENT(INOUT) :: IUZ(*)

    INTEGER I,J,K,IND
    CHARACTER(258) :: SOLFILE, BIFFILE, DIAFILE

    PAR(:)=0.d0
    ! redefine thl to be nfpr indexed
    DO I=1,AP%NFPR
       THL(I)=1.0D0
       DO J=1,SIZE(IVTHL)
          IF(ICP(I)==NAMEIDX(IVTHL(J)%INDEX,parnames))THEN
             THL(I)=IVTHL(J)%VAR
          ENDIF
       ENDDO
    ENDDO
    DO I=1,AP%NDIM
       THU(I)=1.d0
    ENDDO
    DO I=1,SIZE(IVTHU)
       THU(NAMEIDX(IVTHU(I)%INDEX,unames))=IVTHU(I)%VAR
    ENDDO
    !     set IUZ/VUZ
    K=0
    DO I=1,SIZE(IVUZR)
       IND=NAMEIDX(IVUZR(I)%INDEX,parnames)
       DO J=1,SIZE(IVUZR(I)%VAR)
          K=K+1
          IUZ(K)=IND
          VUZ(K)=IVUZR(I)%VAR(J)
       ENDDO
    ENDDO
    DO I=1,SIZE(IVUZSTOP)
       IND=NAMEIDX(IVUZSTOP(I)%INDEX,parnames)
       DO J=1,SIZE(IVUZSTOP(I)%VAR)
          K=K+1
          IUZ(K)=-IND
          VUZ(K)=IVUZSTOP(I)%VAR(J)
       ENDDO
    ENDDO

    ! only now open the output files
    IF(AP%NTOT==0)THEN
       IF(SVFILE/='')THEN
          BIFFILE='b.'//SVFILE
          SOLFILE='s.'//SVFILE
          DIAFILE='d.'//SVFILE
       ELSE
          BIFFILE='fort.7'
          SOLFILE='fort.8'
          DIAFILE='fort.9'
       ENDIF
       OPEN(7,FILE=BIFFILE,STATUS='unknown',ACCESS='sequential')
       OPEN(8,FILE=SOLFILE,STATUS='unknown',ACCESS='sequential')
       OPEN(9,FILE=DIAFILE,STATUS='unknown',ACCESS='sequential')
    ENDIF

  END SUBROUTINE INIT3

  SUBROUTINE AUTOSTOP()
    USE AUTOMPI, ONLY: MPIEND
    CALL MPIEND()
    STOP
  END SUBROUTINE AUTOSTOP

END MODULE SUPPORT

! The following functions and subroutines are called from
! various demos so cannot be inside the module

! ---------- --
  SUBROUTINE GE(IAM,N,M1A,A,NRHS,NDX,U,M1F,F,IR,IC,DET)
    USE SUPPORT
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IAM,N,M1A,NRHS,NDX,M1F,IR(N)
    DOUBLE PRECISION, INTENT(INOUT) :: A(M1A,N),F(M1F,NRHS)
    DOUBLE PRECISION, INTENT(OUT) :: U(NDX,NRHS),DET
    INTEGER, INTENT(OUT) :: IC(N)
    CALL GELI(N,M1A,A,NRHS,NDX,U,M1F,F,IC,DET,.FALSE.)
  END SUBROUTINE GE

! ------ --------- -------- ----
  DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)

    USE MESH
    USE SUPPORT
    IMPLICIT NONE

    CHARACTER(3), INTENT(IN) :: CODE
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

    INTEGER, INTENT(IN) :: NDIM1
    DOUBLE PRECISION, INTENT(OUT) :: P0(NDIM1,NDIM1),P1(NDIM1,NDIM1)
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
    DOUBLE PRECISION WORKINFO(1)
    DOUBLE PRECISION, ALLOCATABLE :: WORK(:)

    IF(MATZ==1)THEN
       JOBVR='V'
    ELSE
       JOBVR='N'
    ENDIF
    CALL DGEEV('N',JOBVR,NDIM,A,M1A,WR,WI,Z,M1A,Z,M1A,WORKINFO,-1,IER)
    LWORK=NINT(WORKINFO(1))
    ALLOCATE(WORK(LWORK))
    CALL DGEEV('N',JOBVR,NDIM,A,M1A,WR,WI,Z,M1A,Z,M1A,WORK,LWORK,IER)
    DEALLOCATE(WORK)

  END SUBROUTINE RG
