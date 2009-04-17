!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   nag :    Heteroclinic orbits : A saddle-saddle connection 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Parameter assignment:
!
!           PAR(1) , PAR(2) : a      , c      (parameters)
!           PAR(3) , PAR(4) : eps-0  , eps-1  (radii)
!           PAR(5) , PAR(6) : mu-0   , mu-1   (eigenvalues)
!           PAR(7) , PAR(8) : v-0(1) , v-0(2) (eigenvector)
!           PAR(9) , PAR(10): v-1(1) , v-1(2) (eigenvector)
!           PAR(11)         : period
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION PERIOD, DUMMY(1)

       CALL FFFF(2,U,ICP,PAR,0,F,DUMMY)

       PERIOD=PAR(11)
       F(1)=PERIOD*F(1)
       F(2)=PERIOD*F(2)

      END SUBROUTINE FUNC

      SUBROUTINE FFFF(NDM,U,ICP,PAR,IJAC,F,DFDU)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM)

      DOUBLE PRECISION A,C

       A=PAR(1)
       C=PAR(2)
       F(1)= U(2)
       F(2)= C*U(2) - U(1) * (1-U(1)) * (U(1)-A)

      IF(IJAC.EQ.0)RETURN

       DFDU(1,1)= 0
       DFDU(1,2)= 1

       DFDU(2,1)= - (1-U(1))*(U(1)-A) + U(1)*(U(1)-A) - U(1)*(1-U(1))
       DFDU(2,2)= C

      END SUBROUTINE FFFF

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION PERIOD, SIGN, R, V1, V2, X, E, U0, V0, EPS

! The following initialization is done only in the first call
       IF(T==0)THEN
!        Select period and connection (read from the constants-file)
         PERIOD=100.
         SIGN=1.0
         R = 0.5*SQRT(2.D0)
         V1= 1./SQRT(1+R**2)
         V2= R /SQRT(1+R**2)
         X  = -0.5*PERIOD
         E  = EXP(X/SQRT(2.D0))
         U0 = E/(1+E)
         V0 = ( E/(1+E)**2 ) / SQRT(2.D0)
         EPS= SQRT( U0**2 + V0**2 )
         PAR(1)= 0.5
         PAR(2)= 0
         PAR(3)= EPS
         PAR(4)= EPS
         PAR(5)= R
         PAR(6)=-R
         PAR(7)= SIGN*V1
         PAR(8)= SIGN*V2
         PAR(9)= -SIGN*V1
         PAR(10)= SIGN*V2
         PAR(11)= PERIOD
         PAR(12)= SIGN
       ENDIF

! Specify exact solution as starting point :

       PERIOD=PAR(11)
       SIGN  =PAR(12)

       X=PERIOD*SIGN*(T-0.5)
       E=DEXP(X/DSQRT(2.D0))
       U(1)=E/(1+E)
       U(2)=SIGN*E/(1+E)**2/DSQRT(2.D0)

      END SUBROUTINE STPNT

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
! Local
      INTEGER, PARAMETER :: NDM=2
      DOUBLE PRECISION V0(NDM),V1(NDM),G0(NDM),G1(NDM)
      DOUBLE PRECISION DGDU0(NDM,NDM),DGDU1(NDM,NDM)

      V0(1)=U0(1) - PAR(3)*PAR(7)
      V0(2)=U0(2) - PAR(3)*PAR(8)
      V1(1)=U1(1) - PAR(4)*PAR(9)
      V1(2)=U1(2) - PAR(4)*PAR(10)

      CALL FFFF(NDM,V0,ICP,PAR,1,G0,DGDU0)
      CALL FFFF(NDM,V1,ICP,PAR,1,G1,DGDU1)

      FB(1)= DGDU0(1,1)*PAR(7) + DGDU0(1,2)*PAR(8) - PAR(5)*PAR(7)
      FB(2)= DGDU0(2,1)*PAR(7) + DGDU0(2,2)*PAR(8) - PAR(5)*PAR(8)
      FB(3)= DGDU1(1,1)*PAR(9) + DGDU1(1,2)*PAR(10)- PAR(6)*PAR(9)
      FB(4)= DGDU1(2,1)*PAR(9) + DGDU1(2,2)*PAR(10)- PAR(6)*PAR(10)
      FB(5)= PAR(7)**2 + PAR(8)**2 -1
      FB(6)= PAR(9)**2 + PAR(10)**2 -1
      FB(7)= G0(1)
      FB(8)= G0(2)
      FB(9)= G1(1)
      FB(10)=G1(2)

      END SUBROUTINE BCND

      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!     ---------- ----

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
! Local
      INTEGER, PARAMETER :: NDM=2
      DOUBLE PRECISION F(NDM),F0(NDM),DFDU(NDM,NDM),DUMMY(1)

      CALL FFFF(NDM,U   ,ICP,PAR,1,F ,DFDU)
      CALL FFFF(NDM,UOLD,ICP,PAR,0,F0,DUMMY)

       FI(1)= ( F(1) - F0(1) ) * ( DFDU(1,1)*F(1) + DFDU(1,2)*F(2) ) &
            + ( F(2) - F0(2) ) * ( DFDU(2,1)*F(1) + DFDU(2,2)*F(2) )

      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
