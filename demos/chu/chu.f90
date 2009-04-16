!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!                   chu :     Chua's circuit
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
      ab(x)=2*x*DATAN(RK*x)/pi
      h(x)=a1*x + 0.5*(a0-a1) * ( ab(x+1) - ab(x-1) ) 
!
       pi=4*DATAN(1.d0)
       alpha=PAR(1)
       beta =PAR(2)
       RK   =PAR(3)
       a0   =PAR(4)
       a1   =PAR(5) 
!
       x=U(1)
       y=U(2)
       z=U(3)   
! 
       F(1)= alpha * ( y - h(x) )
       F(2)=  x - y + z
       F(3)= -beta * y
! 
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
!
       PAR(1)=-1
       PAR(2)= 14.3 
       PAR(3)= 10.
       PAR(4)= -1./7.
       PAR(5)= 2./7.
!
       U(1)=0.
       U(2)=0.
       U(3)=0.
! 
      RETURN 
      END 
! 
      SUBROUTINE BCND 
      RETURN 
      END 
! 
      SUBROUTINE ICND 
      RETURN 
      END 
! 
      SUBROUTINE FOPT 
      RETURN 
      END 
! 
      SUBROUTINE PVLS
      RETURN 
      END 
