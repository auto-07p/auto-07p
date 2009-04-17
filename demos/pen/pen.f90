!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   pen :    Coupled pendula (or rotations in coupled Josephson junctions)
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION GAMMA,EPS,RI,PH1,PH2,PS1,PS2

       GAMMA = PAR(1)
       EPS   = PAR(2)
       RI    = PAR(3)

       PH1=U(1)
       PH2=U(2)
       PS1=U(3)
       PS2=U(4)

       F(1)= PS1
       F(2)= PS2
       F(3)= -EPS*PS1 - SIN(PH1) + RI + GAMMA*(PH2-PH1)
       F(4)= -EPS*PS2 - SIN(PH2) + RI + GAMMA*(PH1-PH2)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION GAMMA,EPS,RI

       GAMMA=0.175
       EPS=0.1
       RI=0.4
       PAR(1)=GAMMA
       PAR(2)=EPS
       PAR(3)=RI

! Set the actual period (since the data in pen.dat have scaled time variable)
         PAR(11)=1.5738797205

      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
