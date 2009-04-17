!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!                   chu :     Chua's circuit
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION ab,h,pi,alpha,beta,RK,a0,a1,x,y,z

      ab(x)=2*x*DATAN(RK*x)/pi
      h(x)=a1*x + 0.5*(a0-a1) * ( ab(x+1) - ab(x-1) ) 

       pi=4*DATAN(1.d0)
       alpha=PAR(1)
       beta =PAR(2)
       RK   =PAR(3)
       a0   =PAR(4)
       a1   =PAR(5) 

       x=U(1)
       y=U(2)
       z=U(3)   

       F(1)= alpha * ( y - h(x) )
       F(2)=  x - y + z
       F(3)= -beta * y

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=-1
       PAR(2)= 14.3 
       PAR(3)= 10.
       PAR(4)= -1./7.
       PAR(5)= 2./7.

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
