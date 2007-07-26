C------------------------------------------------------------
C------------------------------------------------------------
C     pla :     Plant's Model of Bursting Nerve Cells
C------------------------------------------------------------
C------------------------------------------------------------
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
      DOUBLE PRECISION Kp, Kc 
C
       VI=30
       VK=-75
       VL=-40
       VCa=140
       gK=0.3
       gL=0.003
       Kp=0.5
       Kc=0.0085
       rho=0.0003
       txT=235
       zeta=1
C
       gI=PAR(1)
       gP=PAR(2)
       gT=PAR(3)
C
       V =U(1)
       xT=U(2)
       xK=U(3)
       yI=U(4)
       c =U(5)
C
       a=127/(VI-VK)
       b=(115*VK+12*VI)/(VI-VK)
       Vs=a*V-b
C
       am=0.1*(50-Vs)/(DEXP((50-Vs)/10)-1)
       ah=0.07*DEXP((25-Vs)/20)
       an=0.01*(55-Vs)/(DEXP((55-Vs)/10)-1)
       bm=4*DEXP((25-Vs)/18)
       bh=1./(DEXP((55-Vs)/10)+1)
       bn=0.125*DEXP((45-Vs)/80)
C
       sI=am/(am+bm)
       sK=an/(an+bn)
       txK=12.5/(an+bn)
       tyI=12.5/(ah+bh)
       zI=ah/(ah+bh)
       sT=1./(DEXP(0.15*(-50-V))+1)
C
       F(1)=  (gI*sI**3*yI + gT*xT) * (VI-V)
     *      + (gK*xK**4 + gP*c/(Kp+c)) * (VK-V)
     *      + gL * (VL-V)
       F(2)=  (sT-xT) / (txT*zeta)
       F(3)=  (sK-xK) / (txK*zeta)
       F(4)=  (zI-yI) / (tyI*zeta)
       F(5)=  rho * (Kc*xT*(VCa-V) - c)
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
       gI=50.
       gP=0.03
       gT=0.01
C 
       PAR(1)=gI
       PAR(2)=gP
       PAR(3)=gT
C   
       U(1)=-2.329601E+00
       U(2)= 9.992162E-01
       U(3)= 7.373314E-01
       U(4)= 6.135948E-03
       U(5)= 1.208853E+00
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
