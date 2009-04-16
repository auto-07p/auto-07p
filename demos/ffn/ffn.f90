!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   frc :      A periodically forced system
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
! 
! FHN parameters
       a = PAR(1) 
       b = PAR(2) 
       c = PAR(3) 
!
! Forcing parameters
       amp = PAR(4) 
       beta= PAR(5) 
!
! FHN variables
       x=U(1)
       y=U(2)
!
! Forcing variables
       sn=U(3)
       cs=U(4)
       ss = sn**2 + cs**2    
! 
! FHN equations
       F(1)= c * ( x - x**3/3 + y - amp*cs )
       F(2)=-( x - a + b*y ) / c
!
! Oscillator
       F(3) =  sn + beta*cs - sn*ss    
       F(4) = -beta*sn + cs - cs*ss    
! 
      RETURN    
      END      
! 
      SUBROUTINE STPNT(NDIM,U,PAR,t)  
!     ---------- -----
! 
      IMPLICIT REAL*8(A-H,O-Z)  
      DIMENSION U(NDIM),PAR(*)  
! 
! FHN parameters
       a = 0.
       b = 0.8
       c = 3.
!
       PAR(1)=a
       PAR(2)=b
       PAR(3)=c
!
! Forcing parameters
       amp=0.
       beta=10
!
       PAR(4)=amp
       PAR(5)=beta
!
       TPI=8*DATAN(1.d0)      
       PAR(11)=TPI/beta
!
!      Initial FHB stationary solution (assuming a=0)
       U(1)= 0.
       U(2)= 0.
!
! Initialize the oscillator
       U(3)=DSIN(TPI*t)      
       U(4)=DCOS(TPI*t)      
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
