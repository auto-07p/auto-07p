!------------------------------------------------------------
!------------------------------------------------------------
!     pla :     Plant's Model of Bursting Nerve Cells
!------------------------------------------------------------
!------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)
      DOUBLE PRECISION VI,VK,VL,VCa,gK,gL,Kp,Kc,rho,txT,zeta,gi,gP,gT
      DOUBLE PRECISION V,xT,xK,yI,c,a,b,Vs,am,ah,an,bm,bh,bn,si,sK
      DOUBLE PRECISION txK,tyI,zI,sT

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

       gI=PAR(1)
       gP=PAR(2)
       gT=PAR(3)

       V =U(1)
       xT=U(2)
       xK=U(3)
       yI=U(4)
       c =U(5)

       a=127/(VI-VK)
       b=(115*VK+12*VI)/(VI-VK)
       Vs=a*V-b

       am=0.1*(50-Vs)/(DEXP((50-Vs)/10)-1)
       ah=0.07*DEXP((25-Vs)/20)
       an=0.01*(55-Vs)/(DEXP((55-Vs)/10)-1)
       bm=4*DEXP((25-Vs)/18)
       bh=1./(DEXP((55-Vs)/10)+1)
       bn=0.125*DEXP((45-Vs)/80)

       sI=am/(am+bm)
       sK=an/(an+bn)
       txK=12.5/(an+bn)
       tyI=12.5/(ah+bh)
       zI=ah/(ah+bh)
       sT=1./(DEXP(0.15*(-50-V))+1)

       F(1)=  (gI*sI**3*yI + gT*xT) * (VI-V)                            &
            + (gK*xK**4 + gP*c/(Kp+c)) * (VK-V)                         &
            + gL * (VL-V)
       F(2)=  (sT-xT) / (txT*zeta)
       F(3)=  (sK-xK) / (txK*zeta)
       F(4)=  (zI-yI) / (tyI*zeta)
       F(5)=  rho * (Kc*xT*(VCa-V) - c)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      DOUBLE PRECISION gI,gP,gT

       gI=50.
       gP=0.03
       gT=0.01

       PAR(1)=gI
       PAR(2)=gP
       PAR(3)=gT

       U(1)=-2.329601E+00
       U(2)= 9.992162E-01
       U(3)= 7.373314E-01
       U(4)= 6.135948E-03
       U(5)= 1.208853E+00

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS 
      END SUBROUTINE PVLS
