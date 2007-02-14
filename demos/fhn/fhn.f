C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   fhn :        The FitzHugh - Nagumo Equation
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*), F(NDIM), ICP(*)
C
       a=PAR(1)
       b=PAR(2)
       c=PAR(3)
C
       x=U(1)
       y=U(2)
C
       F(1)= c * ( x - x**3/3 + y )
       F(2)=-( x - a + b*y ) / c
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE STPNT(NDIM,U,PAR)
C     ---------- -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*)
C
C Initialize the equation parameters
       PAR(1)=0.
       PAR(2)=0.8
       PAR(3)=3.0
C
C Initialize the solution (assuming PAR(1)=0 )
       U(1)=0.
       U(2)=0.
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
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
C----------------------------------------------------------------------
C----------------------------------------------------------------------
