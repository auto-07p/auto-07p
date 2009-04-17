!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   fsh :     Heteroclinic orbits : a saddle-node copnnection
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Parameter assignment:
!
!           PAR(1)           :                   (unused)
!           PAR(2)           : c                 (wave speed)
!           PAR(4)           : eps-1        1    (radius)
!           PAR(11)          : period
!           PAR(12)          : mu-1              (eigenvalue  at 1)
!           PAR(13) , PAR(14): v(1)    , v(2)    (eigenvector at 1)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION PERIOD,DUMMY(1)
      INTEGER I

       CALL FFFF(2,U,ICP,PAR,IJAC,F,DUMMY)
       PERIOD=PAR(11)
       DO I=1,NDIM
         F(I)=PERIOD*F(I)
       ENDDO

      END SUBROUTINE FUNC

      SUBROUTINE FFFF(NDM,U,ICP,PAR,IJAC,F,DFDU)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM)

      DOUBLE PRECISION C

       C=PAR(2)
       F(1)= U(2)
       F(2)= C*U(2) - U(1) * (1-U(1))

      IF(IJAC.EQ.0)RETURN

       DFDU(1,1)= 0
       DFDU(1,2)= 1

       DFDU(2,1)= -1 + 2*U(1)
       DFDU(2,2)= C

      END SUBROUTINE FFFF

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION PERIOD, C, EP1, D, RMU1, V11, V12

      IF(T==0)THEN
!       Set the starting period, wave speed, and radius
        PERIOD=0.01
        C=11.
        EP1=0.001
        D = SQRT(C**2+4)
        PAR(2)= C
        PAR(4)= EP1
        PAR(11)= PERIOD
        PAR(12)= (C-D)/2
        PAR(13) =    1./SQRT(1+PAR(12)**2)
        PAR(14)=PAR(12)/SQRT(1+PAR(12)**2)
       ENDIF

       C     =PAR(2)
       EP1   =PAR(4)
       PERIOD=PAR(11)
       D=SQRT(C**2+4)
       RMU1= (C-D)/2
       V11 =  1./SQRT(1+RMU1**2)
       V12 =RMU1/SQRT(1+RMU1**2)

       U(1)=1-EP1*V11
       U(2)= -EP1*V12

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
      DOUBLE PRECISION V1(NDM),G1(NDM),DGDU1(NDM,NDM)

      V1(1)=U1(1) + PAR(4)*PAR(13)
      V1(2)=U1(2) + PAR(4)*PAR(14)

      CALL FFFF(NDM,V1,ICP,PAR,1,G1,DGDU1)

      FB(1)= DGDU1(1,1)*PAR(13) + DGDU1(1,2)*PAR(14)- PAR(12)*PAR(13)
      FB(2)= DGDU1(2,1)*PAR(13) + DGDU1(2,2)*PAR(14)- PAR(12)*PAR(14)
      FB(3)= PAR(13)**2 + PAR(14)**2 -1
      FB(4)= G1(1)
      FB(5)= G1(2)

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
