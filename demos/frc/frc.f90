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
       FHN(Z) = Z*(Z-A)*(1-Z) 
! 
       A  = PAR(1) 
       B  = PAR(2) 
       R  = PAR(3) 
       EPS= PAR(4) 
       BET= PAR(5) 
       D  = PAR(6)
!
       V=U(1)
       W=U(2)
       X=U(3)
       Y=U(4)
       SS = X**2 + Y**2    
! 
       F(1) = ( FHN(V) - W )/EPS
       F(2) = V - D*W - ( B + R*X )
       F(3) =  X + BET*Y - X*SS    
       F(4) = -BET*X + Y - Y*SS    
! 
      RETURN    
      END      
! 
      SUBROUTINE STPNT(NDIM,U,PAR,T)  
!     ---------- -----
! 
      IMPLICIT REAL*8(A-H,O-Z)  
      DIMENSION U(NDIM),PAR(*)  
! 
       A  =0.5
       B  = A
       R  =0.
       EPS=0.005
       BET=100
       D  =1.0
!
       PAR(1)=A
       PAR(2)=B
       PAR(3)=R
       PAR(4)=EPS
       PAR(5)=BET
       PAR(6)=D
       TPI=8*DATAN(1.0D0)      
       PAR(11)=TPI/BET
!
       U(1)=B
       U(2)=B*(B-A)*(1-B)
       U(3)=DSIN(TPI*T)      
       U(4)=DCOS(TPI*T)      
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
