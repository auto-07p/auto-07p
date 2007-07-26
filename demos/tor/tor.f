C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   tor :    Torus Bifurcation in an Electronic Oscillator
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*)
C
       rn=PAR(1) 
       be=PAR(2) 
       ga=PAR(3) 
       r =PAR(4) 
       a3=PAR(5) 
       b3=PAR(6) 
C
       x=U(1)
       y=U(2)
       z=U(3)
C
       F(1)= ( -(be+rn)*x + be*y - a3*x**3 + b3*(y-x)**3 )/r
       F(2)=  be*x - (be+ga)*y - z - b3*(y-x)**3
       F(3)= y
C
       IF(IJAC.EQ.0)RETURN
C 
       DFDU(1,1)=( -(be+rn) -3*a3*x**2 - 3*b3*(y-x)**2  )/r
       DFDU(1,2)=( be + 3*b3*(y-x)**2 )/r
       DFDU(1,3)=0
C 
       DFDU(2,1)=be + 3*b3*(y-x)**2
       DFDU(2,2)=-(be+ga) - 3*b3*(y-x)**2
       DFDU(2,3)=-1
C 
       DFDU(3,1)=0
       DFDU(3,2)=1
       DFDU(3,3)=0
C 
      IF(IJAC.EQ.1)RETURN 
C
C      *Parameter derivatives
       DFDP(1,1)=-x/r
       DFDP(2,1)=0
       DFDP(3,1)=0
C
       DFDP(1,2)=( -x + y )/r
       DFDP(2,2)=x-y
       DFDP(3,2)=0
C
       DFDP(1,3)=0
       DFDP(2,3)=-y
       DFDP(3,3)=0
C
       DFDP(1,4)=-F(1)/r
       DFDP(2,4)=0
       DFDP(3,4)=0
C
       DFDP(1,5)=x**3/r
       DFDP(2,5)=0
       DFDP(3,5)=0
C
       DFDP(1,6)=(y-x)**3 / r
       DFDP(2,6)=-(y-x)**3
       DFDP(3,6)=0
C
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
C 
       PAR(1)=-0.9
       PAR(2)=0.5
       PAR(3)=-0.6
       PAR(4)=0.6
       PAR(5)=0.328578
       PAR(6)=0.933578
C 
       U(1)=0.
       U(2)=0.
       U(3)=0.
C 
      RETURN 
      END 
C 
      SUBROUTINE BCND 
      RETURN 
      END 
C 
      SUBROUTINE ICND 
      RETURN 
      END 
C 
      SUBROUTINE FOPT 
      RETURN 
      END 
C 
      SUBROUTINE PVLS
      RETURN 
      END 
