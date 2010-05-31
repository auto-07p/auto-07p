!---------------------------------------------------------------------
!---------------------------------------------------------------------
! snh: Bifurcations of global reinjection orbits near a saddle-node
!      Hopf bifurcation: cycle-to-point connections
!
! Parameters:
!   PAR(1) : nu1
!   PAR(2) : nu2
!   PAR(3) : d
!   PAR(4) : codimension of connection (0 or 1)
!
!   PAR(5) : delta: distance from cycle to start connection
!   PAR(6) : eps: distance from end connection to point
!   PAR(7) : mu: log of the Floquet multiplier
!   PAR(8) : h: norm of eigenfunction for cycle at 0
!   PAR(9) : T^-: time for connection from cycle (U(7:9)) to section
!   PAR(10) : T^+: time for connection from section (U(10:12)) to point
!   PAR(11) : T: period of the cycle
!   PAR(21) : sigma-: U1(9)-pi (phi-distance W^u(P) from section phi=pi)
!                     ( distance to W^u(b) for codim 0 connection)
!   PAR(22) : sigma+: U0(12)-pi (phi-distance W^s(E) from section phi=pi)
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

    DOUBLE PRECISION x, y, phi
    DOUBLE PRECISION nu1, nu2, alpha, beta, omega, c, d, fp, s
    DOUBLE PRECISION sphi, x2y2, cphinu, cphinu2

    nu1 = PAR(1)
    nu2 = PAR(2)
    d = PAR(3)
    fp = 4*atan(1.0d0)*d
    alpha = -1
    s = -1
    c = 0
    omega = 1
    beta = 0

    x = U(1)
    y = U(2)
    phi = U(3)

    sphi = sin(phi)
    x2y2 = x*x + y*y
    cphinu = 2*cos(phi)+nu2
    cphinu2 = cphinu*cphinu

    F(1) = nu1*x - omega*y - (alpha*x - beta*y)*sphi - x2y2*x + d*cphinu2
    F(2) = nu1*y + omega*x - (alpha*y + beta*x)*sphi - x2y2*y + fp*cphinu2
    F(3) = nu2 + s*x2y2 + 2.0*cos(phi) + c*x2y2*x2y2

    IF(JAC)THEN
       A(1,1) = nu1 - alpha*sphi - 3*x**2 - y**2
       A(1,2) = -omega + beta*sphi - 2*x*y
       A(1,3) = -(alpha*x - beta*y)*cos(phi) - d*4*(2*cos(phi)+nu2)*sphi

       A(2,1) = omega - beta*sphi - 2*x*y
       A(2,2) = nu1 - alpha*sphi - 3*y**2 - x*x
       A(2,3) = -(alpha*y + beta*x)*cos(phi) - fp*4*(2*cos(phi) + nu2)*sphi

       A(3,1) = 2*s*x + 4*c*x2y2*x
       A(3,2) = 2*s*y + 4*c*x2y2*y
       A(3,3) = -2*sphi
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

    ! log of Floquet multiplier in PAR(7)
    mu = PAR(7)
    F(4:6) = F(4:6) - mu*U(4:6)

    IF (NDIM==6) RETURN

    CALL RHS(U(7:9),PAR,F(7:9),.FALSE.,A)

    T = PAR(9)
    F(7:9) = F(7:9) * T

    IF (NDIM==9) RETURN

    CALL RHS(U(10:12),PAR,F(10:12),.FALSE.,A)

    T = PAR(10)
    F(10:12) = F(10:12) * T

  END SUBROUTINE FUNC

  SUBROUTINE STPNT(NDIM,U,PAR,T)
  !--------- -----
  
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: T

    INTEGER, PARAMETER :: codim = 1
    DOUBLE PRECISION, PARAMETER :: nu1 = 0, nu2 = -1.46d0, d = 0.01d0
    DOUBLE PRECISION, PARAMETER :: delta = -1d-5, eps = 1d-6
    DOUBLE PRECISION fp(3), ev(3), pi
    DOUBLE PRECISION, SAVE :: s(6)

    IF(NDIM==9)THEN
       IF(T==0)THEN
          s(1:6) = U(1:6)
       ENDIF
       U(7:9) = s(1:3) + PAR(5)*s(4:6)
       RETURN
    ELSEIF(NDIM==12)THEN
       fp = (/0d0, 0d0, acos(-nu2/2)/)
       ev = (/0d0, 0d0, 1d0/)
       U(10:12) = fp(1:3) + PAR(6)*ev(1:3)
       RETURN
    ENDIF

    PAR(1:3) = (/nu1,nu2,d/)
    PAR(4) = codim
    PAR(5) = delta
    PAR(6) = eps
    PAR(21:22) = 0.0

    ! homotopy: equilibrium b and its phase-shifted version, to find
    ! the "heteroclinic" orbit in negative time.
    pi = 4 * ATAN(1D0)
    PAR(11) = -0.1
    PAR(12) = 0
    PAR(13) = 0
    PAR(14) = acos(-PAR(2)/2)
    PAR(15) = 0
    PAR(16) = 0
    PAR(17) = acos(-PAR(2)/2)+2*pi
    PAR(18) = 0.01d0 ! epsilon_0 distance

    ! equilibrium a
    U(1:3) = (/ 0.0d0, 0.0d0, 2*pi-acos(-nu2/2) /)

  END SUBROUTINE STPNT

  SUBROUTINE PVLS(NDIM,U,PAR)
  !--------- ----

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    
    DOUBLE PRECISION, EXTERNAL :: GETP
    DOUBLE PRECISION d(3), normlv
    INTEGER i, NBC, codim
    DOUBLE PRECISION pi
    LOGICAL, SAVE :: FIRST = .TRUE.

    IF (FIRST) THEN
       FIRST = .FALSE.
       ! initialization for BCND
       pi = 4d0 * ATAN(1d0)
       IF (NDIM==9) THEN
          codim = NINT(PAR(4))
          IF (codim == 0) THEN
             IF (PAR(21)==0) THEN
                ! distance to W^u(b) where b is the phase-shifted equilibrium
                PAR(21) = GETP("BV1",9,U) - PAR(17)
             ENDIF
          ELSE
             PAR(21) = GETP("BV0",9,U) - pi
          ENDIF
       ELSEIF (NDIM==12) THEN
          NBC = AINT(GETP("NBC",0,U))
          IF (NBC==15) THEN
             PAR(22) = GETP("BV0",12,U) - pi
          ELSE
             ! check if Lin vector initialized:
             IF (DOT_PRODUCT(PAR(24:26),PAR(24:26)) > 0) RETURN
             DO i=1,3
                d(i) = GETP("BV1",6+i,U) - GETP("BV0",9+i,U)
             ENDDO
             normlv = sqrt(DOT_PRODUCT(d,d))
             ! gap size in PAR(23)
             PAR(23) = normlv
             ! Lin vector in PAR(24)-PAR(26)
             PAR(24:26) = d(1:3)/normlv
          ENDIF
       ENDIF
    ENDIF
  END SUBROUTINE PVLS

  SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
  !--------- ----

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    DOUBLE PRECISION nu2, delta, eps, fp(3), ev(3), eta
    DOUBLE PRECISION pi
    INTEGER codim

    ! Periodicity boundary conditions on state variables
    FB(1:3) = U0(1:3) - U1(1:3)

    ! Floquet boundary condition
    FB(4:6) = U1(4:6) - U0(4:6)

    ! normalization
    FB(7) = PAR(8) - DOT_PRODUCT(U0(4:6),U0(4:6))
    IF (NBC==7) RETURN

    delta = PAR(5)
    FB(8:10) = U0(7:9) - (U0(1:3) + delta*U0(4:6))
    pi = 4d0 * ATAN(1d0)
    codim = NINT(PAR(4))
    IF(codim == 0)THEN
       ! projection boundary condition for codimension-zero connection
       nu2 = PAR(2)
       FB(11) = COS(U1(9) - PAR(21)) +nu2/2
       RETURN
    ENDIF
    FB(11) = U1(9) - pi - PAR(21)

    IF (NBC==11) RETURN

    nu2 = PAR(2)
    eps = PAR(6)
    IF (abs(nu2) > 2) THEN
       ! truncate nu2 to [-2,2] to avoid floating point exceptions with acos
       nu2 = sign(2d0, nu2)
    ENDIF

    fp = (/0d0, 0d0, acos(-nu2/2)/)
    ev = (/0d0, 0d0, 1d0/)

    FB(12:14) = U1(10:12) - (fp(1:3) + eps*ev(1:3))

    IF (NBC==15) THEN
       FB(15) = U0(12) - pi - PAR(22)
       RETURN
    ENDIF

    eta = PAR(23)
    FB(15:17) = U1(7:9) - U0(10:12) - eta*PAR(24:26)

  END SUBROUTINE BCND

  SUBROUTINE ICND(NDIM,PAR,ICP,NINTS,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
  !--------- ----
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NINTS, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINTS)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINTS,*)

    INTEGER codim

    ! Integral phase condition
    FI(1) = DOT_PRODUCT(U(1:3),UPOLD(1:3))
    IF (NINTS==1) RETURN

    codim = NINT(PAR(4))
    IF (codim == 0) THEN
       ! phase condition for codimension-zero connection
       FI(2) = DOT_PRODUCT(UPOLD(7:9),U(7:9)-UOLD(7:9))
       RETURN
    ENDIF
    FI(2) = DOT_PRODUCT(UPOLD(10:12),U(10:12)-UOLD(10:12))
  END SUBROUTINE ICND

  SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP)
  END SUBROUTINE FOPT
