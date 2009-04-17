!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   frc :      A periodically forced system
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION a,b,c,amp,beta,x,y,sn,cs,ss

! FHN parameters
       a = PAR(1) 
       b = PAR(2) 
       c = PAR(3) 

! Forcing parameters
       amp = PAR(4) 
       beta= PAR(5) 

! FHN variables
       x=U(1)
       y=U(2)

! Forcing variables
       sn=U(3)
       cs=U(4)
       ss = sn**2 + cs**2    

! FHN equations
       F(1)= c * ( x - x**3/3 + y - amp*cs )
       F(2)=-( x - a + b*y ) / c

! Oscillator
       F(3) =  sn + beta*cs - sn*ss    
       F(4) = -beta*sn + cs - cs*ss    

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,t)  
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: t

      DOUBLE PRECISION a,b,c,amp,beta,TPI

! FHN parameters
       a = 0.
       b = 0.8
       c = 3.

       PAR(1)=a
       PAR(2)=b
       PAR(3)=c

! Forcing parameters
       amp=0.
       beta=10

       PAR(4)=amp
       PAR(5)=beta

       TPI=8*ATAN(1.d0)
       PAR(11)=TPI/beta

!      Initial FHB stationary solution (assuming a=0)
       U(1)= 0.
       U(2)= 0.

! Initialize the oscillator
       U(3)=SIN(TPI*t)      
       U(4)=COS(TPI*t)      

      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
