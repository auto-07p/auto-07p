!---------------------------------------------------------------------
!---------------------------------------------------------------------
! pcl: Finding a point-to-cycle heteroclinic connection in the
!      Lorenz equations
! 
! Parameters:
!   PAR(1) : rho
!   PAR(2) : beta
!   PAR(3) : sigma
!
!   PAR(11) : T: period of the cycle
!   PAR(12) : mu: log of the Floquet multiplier
!   PAR(13) : h: norm of eigenfunction for cycle at 0
!   PAR(14) : T^+: time for connection from section to cycle (U(7:9))
!   PAR(15) : delta: distance from end connection to cycle
!   PAR(16) : T^-: time for connection from point to section (U(10:12))
!   PAR(17) : eps: distance from point to start connection
!   PAR(21) : sigma+: U0(7)-10 (x-distance W^s(P) from section x=10)
!   PAR(22) : sigma-: U1(10)-10 (x-distance W^u(E) from section x=10)
!   PAR(23) : eta: gap size for Lin vector
!   PAR(24) : Z_x: Lin vector (x-coordinate)
!   PAR(25) : Z_y: Lin vector (y-coordinate)
!   PAR(26) : Z_z: Lin vector (z-coordinate)
!---------------------------------------------------------------------
!---------------------------------------------------------------------

  SUBROUTINE RHS(U,PAR,F,JAC,A)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: U(3), PAR(*)
    LOGICAL, INTENT(IN) :: JAC
    DOUBLE PRECISION, INTENT(OUT) :: F(3), A(3,3)

    DOUBLE PRECISION rho, beta, sigma
    DOUBLE PRECISION x, y, z

    rho = PAR(1)
    beta = PAR(2)
    sigma = PAR(3)

    x = U(1)
    y = U(2)
    z = U(3)

    F(1) = sigma * (y - x)
    F(2) = rho * x - y - x * z
    F(3) = x * y - beta * z

    IF(JAC)THEN
       A(1,1) = -sigma
       A(1,2) = sigma
       A(1,3) = 0

       A(2,1) = rho - z
       A(2,2) = -1
       A(2,3) = -x

       A(3,1) = y
       A(3,2) = x
       A(3,3) = -beta
    ENDIF

  END SUBROUTINE RHS

  SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
! ---------- --- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    DOUBLE PRECISION T,mu
    DOUBLE PRECISION A(3,3)

    CALL RHS(U,PAR,F,NDIM>3,A)
    IF(NDIM==3)RETURN

    F(4:6) = MATMUL(A,U(4:6))

    T = PAR(11)
    F(1:6) = F(1:6) * T

    ! log of Floquet multiplier in PAR(12)
    mu = PAR(12)
    F(4:6) = F(4:6) - mu*U(4:6)

    IF (NDIM==6) RETURN

    CALL RHS(U(7:9),PAR,F(7:9),.FALSE.,A)

    T = PAR(14)
    F(7:9) = F(7:9) * T

    IF (NDIM==9) RETURN

    CALL RHS(U(10:12),PAR,F(10:12),.FALSE.,A)

    T = PAR(16)
    F(10:12) = F(10:12) * T

  END SUBROUTINE FUNC

  SUBROUTINE STPNT(NDIM,U,PAR,T)
  !--------- -----
  
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: T

    DOUBLE PRECISION, PARAMETER :: delta = 1d-7, eps = 1d-7
    DOUBLE PRECISION rho, beta, sigma, ev(3), nev
    DOUBLE PRECISION, SAVE :: s(6)

    IF(NDIM==9)THEN
       IF(T==0)THEN
          s(1:6) = U(1:6)
       ENDIF
       U(7:9) = s(1:3) + delta*s(4:6)
       RETURN
    ELSEIF(NDIM==12)THEN
       rho = PAR(1)
       beta = PAR(2)
       sigma = PAR(3)

       ! unstable eigenvector at the 0 equilibrium
       ev(1) = rho/(-0.5+0.5*sigma+0.5*sqrt(1-2*sigma+sigma*sigma+4*rho*sigma))
       ev(2) = 1
       ev(3) = 0
       nev = sqrt(DOT_PRODUCT(ev,ev))
       ev(1:3) = ev(1:3) / nev

       U(10:12) = eps*ev(1:3)
       RETURN
    ENDIF

    rho = 0
    beta = 8d0/3d0
    sigma = 10d0
    PAR(1:3) = (/rho,beta,sigma/)
    PAR(15) = delta
    PAR(17) = eps
    PAR(21:22) = 0

    U(1:3) = 0

  END SUBROUTINE STPNT

  SUBROUTINE PVLS(NDIM,U,PAR)
  !--------- ----

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    
    DOUBLE PRECISION, EXTERNAL :: GETP
    DOUBLE PRECISION d(3),normlv
    INTEGER i, NBC
    LOGICAL, SAVE :: FIRST = .TRUE.

    IF (FIRST) THEN ! initialization for BCND
       FIRST = .FALSE.
       IF (NDIM==9) THEN
          PAR(21) = GETP("BV0",7,U) - 10
       ELSEIF (NDIM == 12) THEN
          NBC = AINT(GETP("NBC",0,U))
          IF (NBC == 15) THEN
             PAR(22) = GETP("BV1",10,U) - 10
          ELSE
             ! check if Lin vector initialized:
             IF (DOT_PRODUCT(PAR(24:26),PAR(24:26)) > 0) RETURN
             DO i=1,3
                d(i) = GETP("BV0",6+i,U) - GETP("BV1",9+i,U)
             ENDDO
             normlv = sqrt(DOT_PRODUCT(d,d))
             ! gap size in PAR(23)
             PAR(23) = normlv
             ! Lin vector in PAR(24)-PAR(26)
             PAR(24:26) = d(1:3)/normlv
          ENDIF
       ENDIF
       RETURN
    ENDIF

  END SUBROUTINE PVLS

  SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
  !--------- ----

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    DOUBLE PRECISION rho, beta, sigma, delta, eps, ev(3), nev, eta

    ! Periodicity boundary conditions on state variables
    FB(1:3) = U0(1:3) - U1(1:3)

    ! Floquet boundary condition
    FB(4:6) = U1(4:6) - U0(4:6)

    ! normalization
    FB(7) = PAR(13) - DOT_PRODUCT(U0(4:6),U0(4:6))
    IF (NBC==7) RETURN

    delta = PAR(15)
    FB(8:10) = U1(7:9) - (U0(1:3) + delta*U0(4:6))
    FB(11) = U0(7) - 10 - PAR(21)

    IF (NBC==11) RETURN

    rho = PAR(1)
    beta = PAR(2)
    sigma = PAR(3)
    eps = PAR(17)

    ! unstable eigenvector at the 0 equilibrium
    ev(1) = rho/(-0.5+0.5*sigma+0.5*sqrt(1-2*sigma+sigma*sigma+4*rho*sigma))
    ev(2) = 1
    ev(3) = 0
    nev = sqrt(DOT_PRODUCT(ev,ev))
    ev(1:3) = ev(1:3) / nev

    FB(12:14) = U0(10:12) - eps*ev(1:3)

    IF (NBC==15) THEN
       FB(15) = U1(10) - 10 - PAR(22)
       RETURN
    ENDIF

    eta = PAR(23)
    FB(15:17) = U0(7:9) - U1(10:12) - eta*PAR(24:26)

  END SUBROUTINE BCND

  SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
  !--------- ----
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    ! Integral phase condition
    FI(1) = DOT_PRODUCT(U(1:3),UPOLD(1:3))
    IF (NINT==1) RETURN

    FI(2) = DOT_PRODUCT(UPOLD(10:12),U(10:12)-UOLD(10:12))
  END SUBROUTINE ICND

  SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP)
  END SUBROUTINE FOPT
