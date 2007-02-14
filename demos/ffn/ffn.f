C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C   frc :      A periodically forced system
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
C FHN parameters
       a = PAR(1) 
       b = PAR(2) 
       c = PAR(3) 
C
C Forcing parameters
       amp = PAR(4) 
       beta= PAR(5) 
C
C FHN variables
       x=U(1)
       y=U(2)
C
C Forcing variables
       sn=U(3)
       cs=U(4)
       ss = sn**2 + cs**2    
C 
C FHN equations
       F(1)= c * ( x - x**3/3 + y - amp*cs )
       F(2)=-( x - a + b*y ) / c
C
C Oscillator
       F(3) =  sn + beta*cs - sn*ss    
       F(4) = -beta*sn + cs - cs*ss    
C 
      RETURN    
      END      
C 
      SUBROUTINE STPNT(NDIM,U,PAR,t)  
C     ---------- -----
C 
      IMPLICIT REAL*8(A-H,O-Z)  
      DIMENSION U(NDIM),PAR(*)  
C 
C FHN parameters
       a = 0.
       b = 0.8
       c = 3.
C
       PAR(1)=a
       PAR(2)=b
       PAR(3)=c
C
C Forcing parameters
       amp=0.
       beta=10
C
       PAR(4)=amp
       PAR(5)=beta
C
       TPI=8*DATAN(1.d0)      
       PAR(11)=TPI/beta
C
C      Initial FHB stationary solution (assuming a=0)
       U(1)= 0.
       U(2)= 0.
C
C Initialize the oscillator
       U(3)=DSIN(TPI*t)      
       U(4)=DCOS(TPI*t)      
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
