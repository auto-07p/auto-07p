!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   tim :    A test problem for timing AUTO
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      INTEGER NDIM2,I,I1,I2
      DOUBLE PRECISION E
      DOUBLE PRECISION, EXTERNAL :: FEXP

       NDIM2=NDIM/2
       DO I=1,NDIM2
         I1=2*(I-1)+1
         I2=I1+1
         E=FEXP(U(I1))
         F(I1)=U(I2)
         F(I2)=-PAR(1)*E
       ENDDO

      END SUBROUTINE FUNC
!----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION FEXP(U)
!     ------ --------- -------- ----

      DOUBLE PRECISION, INTENT(IN) :: U

      INTEGER NTERMS,K
      DOUBLE PRECISION TRM

       NTERMS=25
       FEXP=1.d0
       TRM=FEXP
       DO K=1,NTERMS
        TRM=TRM*U/K
        FEXP=FEXP + TRM
       ENDDO

      END FUNCTION FEXP
!----------------------------------------------------------------------

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      INTEGER I

      DO I=1,NDIM
        U(I)=0.0
      ENDDO

      END SUBROUTINE STPNT
!----------------------------------------------------------------------

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
      DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

      INTEGER NDIM2,I,I1,I2

       NDIM2=NDIM/2
       DO I=1,NDIM2
         I1=2*(I-1)+1
         I2=I1+1
         FB(I1)=U0(I1)
         FB(I2)=U1(I1)
       ENDDO

      END SUBROUTINE BCND
!----------------------------------------------------------------------

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
!----------------------------------------------------------------------
