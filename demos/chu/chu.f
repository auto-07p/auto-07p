C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C                   chu :     Chua's circuit
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
      ab(x)=2*x*DATAN(RK*x)/pi
      h(x)=a1*x + 0.5*(a0-a1) * ( ab(x+1) - ab(x-1) ) 
C
       pi=4*DATAN(1.d0)
       alpha=PAR(1)
       beta =PAR(2)
       RK   =PAR(3)
       a0   =PAR(4)
       a1   =PAR(5) 
C
       x=U(1)
       y=U(2)
       z=U(3)   
C 
       F(1)= alpha * ( y - h(x) )
       F(2)=  x - y + z
       F(3)= -beta * y
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
       PAR(1)=-1
       PAR(2)= 14.3 
       PAR(3)= 10.
       PAR(4)= -1./7.
       PAR(5)= 2./7.
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
