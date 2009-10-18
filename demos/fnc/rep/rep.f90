! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Slow manifolds computation in the self-coupled FitzHugh-Nagumo system 
! ----------------------------------------------------------------------
! C o m p.  o f  t h e  r e p e l l i n g  s l o w  m a n i f o l d   
! ----------------------------------------------------------------------
! Homotopy step 1: "away from the folded node along the fold curve"
! followed by
! Homotopy step 2: "away from the fold curve on the critical manifold"
! followed by
! Actual computation of the repelling slow manifold
! ----------------------------------------------------------------------
! Ref.: Desroches, Krauskopf and Osinga, CHAOS 18, 015107 (2008)        
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

       DOUBLE PRECISION v, h, s, epsilon, gamma, delta, T

       ! Define the state variables
       v = U(1)
       h = U(2)
       s = U(3)

       ! Define the system parameters
       epsilon = PAR(1)
       gamma   = PAR(2)
       delta   = PAR(3)

       ! Define the integration time as a parameter
       T       = PAR(11)

       ! Define the right-hand sides
       F(1) = T * (h - (v**3 - v + 1) / 2 - gamma * s * v)
       F(2) = T * (-epsilon * (2 * h + 2.6d0 * v))
       F(3) = T * (-epsilon * delta * s)
  
      END SUBROUTINE FUNC

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       !             epsilon  gamma  delta
       PAR(1:3) = (/ 0.015d0, 0.5d0, 0.565d0 /)
       PAR(11) = 0

       !            v        h          s
       U(1:3) = (/ -0.490d0, 0.61760d0, 0.27970d0 /)

       PAR(4) = U(1) ! v(1)
       PAR(5) = U(3) ! s(0)
       PAR(6) = U(3) ! s(1)

      END SUBROUTINE STPNT

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

       ! Define boundary conditions */

       ! Define the critical manifold S as {(v,h,s) ; 2*h-v^3+v-1-v*s=0}
       ! (we use the fact that gamma is fixed at the value 0.5)

       FB(1) = 2*U1(2)-U1(1)**3+U1(1)-1-U1(3)*U1(1)
       FB(2) = U1(1) - PAR(4)  ! End point is on the intersection between S 
       FB(3) = U1(3) - PAR(6)  ! and the plane {s=0.05/v=0.0}

       FB(4) = U0(3) - PAR(5)  ! Initial point is in a cross-section containing
                               ! the folded node: Sigma_fn={s=0.27970}

       IF(NBC==4) RETURN

       ! FB(1) and FB(5): {(v,h,s) ; s - (1 - 3*v^2=0)}
       ! together define the folded node F
       FB(5) = U1(3) - (1 - 3*U1(1)**2)

      END SUBROUTINE BCND

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE PVLS(NDIM,U,PAR)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

      DOUBLE PRECISION, EXTERNAL :: GETP

       ! Define external parameter which monitors the v- and the h-coordinate 
       ! of the initial point in section Sigma_fn */
       PAR(8) = GETP("BV0", 1, U)
       PAR(9) = GETP("BV0", 2, U)
      END SUBROUTINE PVLS
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE ICND
      END SUBROUTINE ICND
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE FOPT
      END SUBROUTINE FOPT
