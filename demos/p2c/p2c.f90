!----------------------------------------------------------------------
!   lor:  Point-to-cycle connection in the Lorenz model via homotopy
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!     Adjoint scaled eigenfunction of the cycle in the Lorenz model
!
!     PAR(10)=square norm of the eigenfunction at T=0
!     PAR(12)=log(FM)
!---------------------------------------------------------------------- 

  SUBROUTINE RHS(N,U,PAR,F,DFDU,IJAC) 
! ---------- ---- 

    IMPLICIT NONE
    INTEGER N
    DOUBLE PRECISION U(N),PAR(*),F(N),DFDU(N,N)
    LOGICAL IJAC

    DOUBLE PRECISION x,y,z,r,b,sigma

!     System parameters
!     PAR(1)		! r
!     PAR(2)		! b
!     PAR(3)		! sigma

!     Variables
    x=U(1)
    y=U(2)
    z=U(3)
!     Parameters
    r=PAR(1)
    b=PAR(2)
    sigma=PAR(3)
!     RHS
    F(1)=sigma*(y-x)
    F(2)=r*x-y-x*z
    F(3)=x*y-b*z

    IF(.NOT.IJAC)RETURN

!     Jacobian elements
    DFDU(1,1)=-sigma
    DFDU(1,2)=sigma
    DFDU(1,3)=0.0d0
    DFDU(2,1)=r-z
    DFDU(2,2)=-1.d0
    DFDU(2,3)=-x
    DFDU(3,1)=y
    DFDU(3,2)=x
    DFDU(3,3)=-b

  END SUBROUTINE RHS

  SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
! ---------- ---- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,*), DFDP(NDIM,*)

    DOUBLE PRECISION A(3,3)
!     Variables
!     U(1)-U(3) = limit cycle
!     U(4)-U(6) = adjoint eigenfunction 
!     U(7)-U(9) = connecting orbit 
!     Cycle equations
    CALL RHS(3,U,PAR,F,A,NDIM/=3)
    IF(NDIM==3)RETURN
    F(1:3) = PAR(11)*F(1:3)

!     Adjoint variational equations
!     PAR(11) = cycle period
!     PAR(12) = log(FM) 
    F(4:6) = -PAR(11)*MATMUL(TRANSPOSE(A(:,:)),U(4:6))-PAR(12)*U(4:6)
    IF(NDIM==6)RETURN

!     Connecting orbit equations
    CALL RHS(3,U(7),PAR,F(7),A,.FALSE.) 
!     PAR(13) = connection interval     
    F(7:9) = PAR(13)*F(7:9)

  END SUBROUTINE FUNC

  SUBROUTINE STPNT(NDIM,U,PAR,T) 
! ---------- ----- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM), PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: T

    DOUBLE PRECISION r,b,sigma,v1,v2,v3,vnorm,eps,i


    IF (NDIM==9) THEN
       !extend system
       eps=PAR(23)
       U(7:9)=PAR(14:16) + eps*PAR(17:19)
       RETURN
    ENDIF

! Parameter values for the starting equilibrium
    r=0.d0
    b=8.0d0/3.0d0
    sigma=10.0d0
    U(1:3)=0.

    PAR(1)=r
    PAR(2)=b
    PAR(3)=sigma
!     Eigenvalue approx
    PAR(12)=-1.0d0
!     Initial connection time
    PAR(13)=0.0d0

!     Equilibrium coordinates
    PAR(14:16)=U(1:3)

!     Normalized exit eigenvector of the equilibrium
    r=21.0d0
    v1=1.0-sigma+SQRT((1.0-sigma)**2+4.0*r*sigma)
    v2=2.0*r
    v3=0.0D0
    vnorm=SQRT(v1**2+v2**2+v3**2)
    PAR(17)=v1/vnorm
    PAR(18)=v2/vnorm
    PAR(19)=v3/vnorm
!     Exit eigenvalue
    PAR(20)=(-1.0d0-sigma+SQRT((1.0d0-sigma)**2+4.0d0*r*sigma))/2.0d0

! epsilon for the starting point y0
    PAR(23)=-1d-4

  END SUBROUTINE STPNT


  SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
! ---------- ---- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    DOUBLE PRECISION X(3),R(3),A(3,3),V(3),H(3),epsilon,rlambda

!     Periodic boundary conditions
!     ----------------------------
    FB(1:3) = U0(1:3) - U1(1:3)
    FB(4:6) = U1(4:6) - U0(4:6)

!     Eigenfunction normalisation
    FB(7) = DOT_PRODUCT(U0(4:6),U0(4:6)) - PAR(10)
    IF(NBC == 7)RETURN

    ! Exit eigenvector
    V(:) = PAR(17:19)

    ! Connecting orbit starting point 
    epsilon = PAR(23)
    FB(8:10) = U0(7:9)-PAR(14:16)-epsilon*V(:)
       
    ! Equilibrium coordinates
    CALL RHS(3,PAR(14:16),PAR,FB(11:13),A,.TRUE.) 

    ! Exit eigenvalue
    rlambda = PAR(20)
    FB(14:16) = MATMUL(A(:,:),V(:))-rlambda*V(:)

    ! Exit vector normalization
    FB(17) = DOT_PRODUCT(V(:),V(:)) - 1.0d0

    ! Connection endpoint in the orthogonal plane to velocity
    H(:) = U1(7:9) - U0(1:3)
    CALL RHS(3,U0,PAR,R,A,.FALSE.)
    FB(18) = DOT_PRODUCT(H(:),R(:)) - PAR(21)

    ! Projection boundary condition at the endpoint
    FB(19) = DOT_PRODUCT(H(:),U0(4:6)) - PAR(22)

  END SUBROUTINE BCND

  SUBROUTINE PVLS(NDIM,U,PAR)
! ---------- ----

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    DOUBLE PRECISION GETP

    DOUBLE PRECISION X(3),W(3),F(3),A(3,3),H(3),length

    IF(NDIM==6)THEN
       ! Initial values for homotopy parameters
       ! Base point on the cycle       
       X(1)=GETP('BV0',1,U)
       X(2)=GETP('BV0',2,U)
       X(3)=GETP('BV0',3,U)
       ! End point of the initial connection
       length=PAR(23)*EXP(PAR(20)*PAR(13))
       H(1:3)=PAR(14:16)+length*PAR(17:19)-X(1:3)
       CALL RHS(3,X,PAR,F,A,.FALSE.)

       ! Homotopy parameter
       PAR(21) = DOT_PRODUCT(H,F)

       ! Eigenfunction at t=0           
       W(1)=GETP('BV0',4,U)
       W(2)=GETP('BV0',5,U)
       W(3)=GETP('BV0',6,U)
       ! Homotopy parameter
       PAR(22) = DOT_PRODUCT(H,W)
    ENDIF

  END SUBROUTINE PVLS

  SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
! ---------- ---- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    ! Integral phase condition 
    ! ------------------------
    FI(1) = DOT_PRODUCT(U(:3),UPOLD(:3))

  END SUBROUTINE ICND

  SUBROUTINE FOPT 
  END SUBROUTINE FOPT
