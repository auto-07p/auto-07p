C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   r3b :    The Restricted 3-Body Problem
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       x  = U(1)        
       y  = U(2) 
       z  = U(3) 
       xp = U(4) 
       yp = U(5) 
       zp = U(6) 
C
       rl  = PAR(1)
       rmu = PAR(2)
C
       dE  = SQRT((x+rmu)**2 + y**2 + z**2)
       dM  = SQRT( (x-1+rmu)**2 + y**2 + z**2 )
       rmc = 1 - rmu
       dE3 = 1./dE**3
       dM3 = 1./dM**3
C
       F(1)= xp
       F(2)= yp
       F(3)= zp
       F(4)= 2*yp + x - rmc*dE3*(x+rmu) - rmu*dM3*(x-1+rmu) + rl*xp
       F(5)=-2*xp + y - rmc*dE3*y       - rmu*dM3*y         + rl*yp
       F(6)=          - rmc*dE3*z       - rmu*dM3*z         + rl*zp
C 
      RETURN 
      END 
C 
      SUBROUTINE PVLS(NDIM,U,PAR)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
C
        rmu = PAR(2)
C
        x  = GETP("BV0", 1, U)
        y  = GETP("BV0", 2, U)
        z  = GETP("BV0", 3, U)
        xp = GETP("BV0", 4, U)
        yp = GETP("BV0", 5, U)
        zp = GETP("BV0", 6, U)
C
        d1 = SQRT((x+rmu)**2 + y**2 + z**2)
        d2 = SQRT( (x-1+rmu)**2 + y**2 + z**2 )
C
        EU = (x**2 + y**2)/2 + (1-rmu)/d1 + rmu/d2
        E  = (xp**2 + yp**2 + zp**2)/2 - EU - rmu*(1-rmu)/2
        PAR(3) = E
C
        det     = GETP("BIF", 1, U)
        PAR(4)  = LOG10(10+DABS(det)) * DATAN(det)
        PAR(5)  = GETP("STA", 1, U)
        PAR(21) = GETP("INT", 1, U)
        PAR(22) = GETP("INT", 2, U)
        PAR(23) = GETP("NRM", 3, U)
C
      RETURN
      END 
C---------------------------------------------------------------------- 
      SUBROUTINE STPNT
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
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
