!----------------------------------------------------------------------
!     Redoing the local bifurcation of the model analysed by
!     Martin Boer, ch. 4 of thesis.
!     Cycle-to-cycle connection 3D food chain
!
!     Homoclinic, but set up also for heteroclinic
!
!     U(1-3) = limit cycle
!     U(4-6) = eigenfunction (ef) stable
!     U(7-9) = limit cycle again ("second cycle")
!     U(10-12) = ef unstable
!     U(13-15) = connecting orbit
!
!     George van Voorn, June 20th 2007
!     Bart Oldeman, December 2008
!---------------------------------------------------------------------- 

  SUBROUTINE RHS(N,U,PAR,F,DFDU,IJAC)
! ---------- --- 

    IMPLICIT NONE
    INTEGER N
    DOUBLE PRECISION U(N),PAR(*),F(N),DFDU(N,N)
    LOGICAL IJAC
    DOUBLE PRECISION f1,f2
    DOUBLE PRECISION d1,d2,x,y,z,a1,a2,b1,b2
!     Declaring variables
    x=U(1)
    y=U(2)
    z=U(3)
!     Parameter values
    d1=PAR(1)
    d2=PAR(2)

    a1 = 5.d0
    a2 = 0.1d0
    b1 = 3.d0
    b2 = 2.d0

    f1=a1/(1d0 + b1*x)
    f2=a2/(1d0 + b2*y)
!     Formulas
    F(1)= x*(1d0 - x) - f1*x*y
    F(2)= f1*x*y - d1*y - f2*y*z
    F(3)= f2*y*z - d2*z

    IF(.NOT.IJAC)RETURN

!     Jacobian elements
    DFDU(1,1)=1d0-2*x-f1*y+b1*x*y*(f1**2)/a1
    DFDU(1,2)=-f1*x
    DFDU(1,3)=0.0d0
    DFDU(2,1)=f1*y-b1*x*y*(f1**2)/a1
    DFDU(2,2)=f1*x-d1-f2*z+b2*y*z*(f2**2)/a2
    DFDU(2,3)=-f2*y
    DFDU(3,1)=0.0d0
    DFDU(3,2)=f2*z-b2*y*z*(f2**2)/a2
    DFDU(3,3)=f2*y-d2

  END SUBROUTINE RHS

  SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
! ---------- --- 

    IMPLICIT NONE
    INTEGER NDIM,ICP(*),IJAC
    DOUBLE PRECISION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,*),DFDP(NDIM,*)
    DOUBLE PRECISION A(3,3)

    CALL RHS(3,U,PAR,F,A,NDIM/=3)
    IF(NDIM==3)RETURN

    F(1:3) = PAR(11)*F(1:3)

    IF(NDIM==6)THEN
!     Variational equations
!     PAR(11) = cycle period
!     PAR(12) = log(FM) 
       F(4:6) = PAR(11)*MATMUL(A(:,:),U(4:6))-PAR(12)*U(4:6)
       RETURN
    ENDIF

!     Adjoint variational equations
!     PAR(11) = cycle period
!     PAR(4) = log(FM) 
    F(4:6) = -PAR(11)*MATMUL(TRANSPOSE(A(:,:)),U(4:6))-PAR(4)*U(4:6)

!     Functions limit cycle doubled U(7-9)
!     We pretend this is a second different cycle
    CALL RHS(3,U(7:9),PAR,F(7:9),A,.TRUE.)
    F(7:9) = PAR(6)*F(7:9)
!     Period=PAR(6), obviously equal to PAR(11), U(10-12)
!     PAR(5) = log(FM) 
    F(10:12) = -PAR(6)*MATMUL(TRANSPOSE(A(:,:)),U(10:12))-PAR(5)*U(10:12)

    IF(NDIM==12)RETURN

!     Connection rescaled, PAR(7) = Tc
    CALL RHS(3,U(13:15),PAR,F(13:15),A,.FALSE.)
    F(13:15) = PAR(7)*F(13:15)

  END SUBROUTINE FUNC

  SUBROUTINE STPNT(NDIM,U,PAR) 
! ---------- --- 

    IMPLICIT NONE
    INTEGER NDIM
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM), PAR(*)

    DOUBLE PRECISION FLOQ,epsilon

    IF(NDIM==12)THEN
       ! extending from NDIM=6 to NDIM=12
       U(7:12)=U(1:6)
       RETURN
    ELSEIF(NDIM==15)THEN
       ! extending from NDIM=12 to NDIM=15
       epsilon=PAR(13)
       U(13:15)=PAR(14:16) + epsilon*PAR(17:19)
       RETURN
    ENDIF

!     0.16<par1=d1<0.32,0.0075<par2=d2<0.015

    PAR(1)=0.5d0
    PAR(2)=0.0125d0
    U(1)=0.7415816238d0
    U(2)=0.1666666667d0
    U(3)=8.664398854d0

!     Guess for FM
    FLOQ=-10d0
    PAR(4)=FLOQ
    PAR(5)=FLOQ
    PAR(12)=FLOQ
    PAR(13)=0.01d0 ! epsilon

!     Eigenvector norms
    PAR(10)=0.

    PAR(25)=0.
    PAR(26)=0.

  END SUBROUTINE STPNT

  SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
! ---------- ---- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    DOUBLE PRECISION g1(3),h1(3),h2(3),A(3,3)
    DOUBLE PRECISION epsilon

!     Periodicity boundary conditions 
!     -------------------------------

    FB(1:3) = U0(1:3) - U1(1:3)
    FB(4:6) = U1(4:6) - U0(4:6)

    IF(NBC==7)THEN
       FB(7) = DOT_PRODUCT(U0(4:6),U0(4:6)) - PAR(10)
       RETURN
    ENDIF

    FB(7) = DOT_PRODUCT(U0(4:6),U0(4:6)) - PAR(25)

    ! Double

    FB(8:10) = U0(7:9) - U1(7:9)
    FB(11:13) = U1(10:12) - U0(10:12)

    FB(14) = DOT_PRODUCT(U0(10:12),U0(10:12)) - PAR(26)

    IF(NBC==14)RETURN

    ! Cycle 1 end-point connection
    CALL RHS(3,U0(1:3),PAR,g1,A,.FALSE.)
    ! Displacement from the cycle; end connection
    h1(1:3)=U1(13:15)-U0(1:3)
     
    ! Eq.(12g)/(17h) - h11
    FB(15) = DOT_PRODUCT(h1(1:3),g1(1:3)) - PAR(21)

    ! cycle 2 starting point connection
    ! For coherence we use U(7-9)
    CALL RHS(3,U0(7:9),PAR,g1,A,.FALSE.)
    ! Displacement from the cycle; start connection
    h2(1:3)=U0(13:15)-U0(7:9)

    ! Eq.(12h)/(17i) - h12
    FB(16) = DOT_PRODUCT(h2(1:3),g1(1:3)) - PAR(22)
    ! Eq.(12i)/(17j) - h21
    FB(17) = DOT_PRODUCT(U0(4:6),h1(1:3)) - PAR(23)
    ! Eq.(12j)/(17k) - h22
    FB(18) = DOT_PRODUCT(U0(10:12),h2(1:3)) - PAR(24)
    ! Eq.(12k)
    FB(19) = DOT_PRODUCT(h2(1:3),h2(1:3)) - PAR(8)
  END SUBROUTINE BCND

  SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
! ---------- ---- 

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
    DOUBLE PRECISION, INTENT(IN) :: PAR(*)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

!   Integral phase condition 
!   ------------------------
    FI(1) = U(1)*UPOLD(1)+U(2)*UPOLD(2)+U(3)*UPOLD(3)
    IF(NINT==1)RETURN
    FI(2) = U(7)*UPOLD(7)+U(8)*UPOLD(8)+U(9)*UPOLD(9)

  END SUBROUTINE ICND

  SUBROUTINE FOPT 
  END SUBROUTINE FOPT

  SUBROUTINE PVLS(NDIM,U,PAR)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NDIM
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    DOUBLE PRECISION GETP

    DOUBLE PRECISION u0(12),g1(3),g2(3),h1(3),h2(3),A(3,3),b(3),epsilon
    INTEGER I

    IF(NDIM==6)THEN
       DO I=1,6
          PAR(13+I)=GETP('BV0',I,U)
       ENDDO
       PAR(6)=PAR(11)
    ELSEIF(NDIM==12)THEN
      DO I=1,12
         u0(I) = GETP('BV0',I,U)
      ENDDO

      ! f' at base point cycle 1
      CALL RHS(3,u0(1:3),PAR,g1,A,.FALSE.)
      ! f' at base point cycle 2
      CALL RHS(3,u0(7:9),PAR,g2,A,.FALSE.)
      ! start & end of initial point
      epsilon=PAR(13)
      b(1:3) = PAR(14:16)+epsilon*PAR(17:19)

      ! Displacement from the cycle at end connection
      h1(1:3)=b(1:3)-u0(1:3)
      ! Displacement from the cycle at start connection
      h2(1:3)=b(1:3)-u0(7:9)

!     Homotopy parameter values at start continuation
      ! Eq.(12h)/(17i)
      PAR(21) = DOT_PRODUCT(h1(1:3),g1(1:3))
      ! Eq.(12i)/(17j)
      PAR(22) = DOT_PRODUCT(h2(1:3),g2(1:3))
      ! Eq.(12j)/(17k)
      PAR(23) = DOT_PRODUCT(u0(4:6),h1(1:3))
      ! Eq.(12k)/(17l)
      PAR(24) = DOT_PRODUCT(u0(10:12),h2(1:3))

      ! for BC(19)/Eq.(12k)
      PAR(8) = DOT_PRODUCT(h2(1:3),h2(1:3))
   ELSEIF(NDIM==15)THEN
      PAR(27)=GETP('MIN',3,U)
      PAR(28)=GETP('MIN',15,U)
   ENDIF


  END SUBROUTINE PVLS
