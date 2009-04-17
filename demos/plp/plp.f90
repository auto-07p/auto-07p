!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   1cl :    A one-cell, two-substrate enzyme model 
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION S,A,S0,A0,AL,RH,RK,D,R,DRDS,DRDA,DRDK

       S=U(1)
       A=U(2)

       S0=PAR(1)
       A0=PAR(2)
       AL=PAR(3)
       RH=PAR(4)
       RK=PAR(5)

       D=1+S+RK*S**2
       R=S*A/D

       F(1)=   (S0-S) - RH*R
       F(2)=AL*(A0-A) - RH*R

       IF(IJAC.EQ.0)RETURN

       DRDS=( A*D - S*A*(1+2*RK*S) ) / D**2
       DRDA=S/D
       DRDK=-S**3*A/D**2

       DFDU(1,1)=-1 - RH*DRDS
       DFDU(1,2)=   - RH*DRDA
       DFDU(2,1)=   - RH*DRDS
       DFDU(2,2)=-AL- RH*DRDA

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives

       DFDP(1,1)=1
       DFDP(1,2)=0
       DFDP(1,3)=0
       DFDP(1,4)=-R
       DFDP(1,5)=-RH*DRDK

       DFDP(2,1)=0
       DFDP(2,2)=AL
       DFDP(2,3)=A0-A
       DFDP(2,4)=-R
       DFDP(2,5)=-RH*DRDK

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=110.
       PAR(2)=500.
       PAR(3)=0.2
       PAR(4)=2.021628
       PAR(5)=0.1

       U(1)=4.555974E+01
       U(2)=1.777987E+02

      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
