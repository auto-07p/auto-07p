! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Slow manifolds computation in the self-coupled FitzHugh-Nagumo system 
! ----------------------------------------------------------------------
! Continuation of canard orbits in parameter space
! ----------------------------------------------------------------------
! Ref.: Desroches, Krauskopf and Osinga, preprint of the UoB, 2009
! URL : http://rose.bris.ac.uk/dspace/handle/1983/1312
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

      SUBROUTINE STPNT
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

       ! Define the critical manifold S as {(v,h,s) ; 2*h-v^3+v-1-s*v=0}
       ! (we use the fact that gamma is fixed at the value 0.5)

       ! Define boundary conditions */
       FB(1) = U0(1) - PAR(4)  ! Initial point is on the intersection between S 
       FB(2) = U0(2) - PAR(7)  ! and the plane {h=-6.0}
       FB(3) = 2*U0(2)-U0(1)**3+U0(1)-1-U0(3)*U0(1)

       FB(4) = U1(1)
       FB(5) = U1(2) - 0.5
       FB(6) = U1(3) - PAR(6)  ! End point is in a cross-section containing the
                               ! folded node: Sigma_fn={s=0.27970}
      END SUBROUTINE BCND

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE PVLS
      END SUBROUTINE PVLS
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE ICND
      END SUBROUTINE ICND
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
      SUBROUTINE FOPT
      END SUBROUTINE FOPT
