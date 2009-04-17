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

      DOUBLE PRECISION FHN,Z,A,B,R,EPS,BET,D,V,W,X,Y,SS

       FHN(Z) = Z*(Z-A)*(1-Z) 

       A  = PAR(1) 
       B  = PAR(2) 
       R  = PAR(3) 
       EPS= PAR(4) 
       BET= PAR(5) 
       D  = PAR(6)

       V=U(1)
       W=U(2)
       X=U(3)
       Y=U(4)
       SS = X**2 + Y**2    

       F(1) = ( FHN(V) - W )/EPS
       F(2) = V - D*W - ( B + R*X )
       F(3) =  X + BET*Y - X*SS    
       F(4) = -BET*X + Y - Y*SS    

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)  
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION A,B,R,EPS,BET,D,TPI

       A  =0.5
       B  = A
       R  =0.
       EPS=0.005
       BET=100
       D  =1.0

       PAR(1)=A
       PAR(2)=B
       PAR(3)=R
       PAR(4)=EPS
       PAR(5)=BET
       PAR(6)=D
       TPI=8*ATAN(1.0D0)
       PAR(11)=TPI/BET

       U(1)=B
       U(2)=B*(B-A)*(1-B)
       U(3)=SIN(TPI*T)
       U(4)=COS(TPI*T)

      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
