!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   tor :    Torus Bifurcation in an Electronic Oscillator
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION rn,be,ga,r,a3,b3,x,y,z

       rn=PAR(1) 
       be=PAR(2) 
       ga=PAR(3) 
       r =PAR(4) 
       a3=PAR(5) 
       b3=PAR(6) 

       x=U(1)
       y=U(2)
       z=U(3)

       F(1)= ( -(be+rn)*x + be*y - a3*x**3 + b3*(y-x)**3 )/r
       F(2)=  be*x - (be+ga)*y - z - b3*(y-x)**3
       F(3)= y

       IF(IJAC.EQ.0)RETURN

       DFDU(1,1)=( -(be+rn) -3*a3*x**2 - 3*b3*(y-x)**2  )/r
       DFDU(1,2)=( be + 3*b3*(y-x)**2 )/r
       DFDU(1,3)=0

       DFDU(2,1)=be + 3*b3*(y-x)**2
       DFDU(2,2)=-(be+ga) - 3*b3*(y-x)**2
       DFDU(2,3)=-1

       DFDU(3,1)=0
       DFDU(3,2)=1
       DFDU(3,3)=0

      IF(IJAC.EQ.1)RETURN 

!      *Parameter derivatives
       DFDP(1,1)=-x/r
       DFDP(2,1)=0
       DFDP(3,1)=0

       DFDP(1,2)=( -x + y )/r
       DFDP(2,2)=x-y
       DFDP(3,2)=0

       DFDP(1,3)=0
       DFDP(2,3)=-y
       DFDP(3,3)=0

       DFDP(1,4)=-F(1)/r
       DFDP(2,4)=0
       DFDP(3,4)=0

       DFDP(1,5)=x**3/r
       DFDP(2,5)=0
       DFDP(3,5)=0

       DFDP(1,6)=(y-x)**3 / r
       DFDP(2,6)=-(y-x)**3
       DFDP(3,6)=0

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=-0.9
       PAR(2)=0.5
       PAR(3)=-0.6
       PAR(4)=0.6
       PAR(5)=0.328578
       PAR(6)=0.933578

       U(1)=0.
       U(2)=0.
       U(3)=0.

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
