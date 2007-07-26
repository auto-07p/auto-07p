C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   mtn : Homoclinic bifurcations in M. Scheffer's preditor-prey model
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C 
      DEM=U(2)**2+PAR(4)**2
      PSIZ=0.4D0*U(1)/(0.6D0+U(1))
      PSIF=PAR(2)*U(2)**2/DEM
      PSIZU1=0.4D0*((1/(0.6D0+U(1)))- (U(1)/((0.6D0+U(1))**2)))
      PSIFU2=2*PAR(2)*( (U(2)/DEM) -(U(2)**3/(DEM**2) ))
C
      F(1)=(0.5D0*U(1)*(1.0D0-U(1)/PAR(1))-U(2)*PSIZ+PAR(3)*PAR(1))
      F(2)=(0.6D0*PSIZ*U(2)-0.15D0*U(2)-PSIF)
C
      IF(IJAC.EQ.0)RETURN
C
        DFDU(1,1)=0.5D0-U(1)/PAR(1) -U(2)*PSIZU1
        DFDU(1,2)=-PSIZ
C
        DFDU(2,1)=0.6D0*PSIZU1*U(2)
        DFDU(2,2)=0.6D0*PSIZ-0.15D0-PSIFU2
C
      IF(IJAC.EQ.1)RETURN
C
        DFDP(1,1)=PAR(3)
        DFDP(2,1)=0.d0
C
        DFDP(1,2)=0.0d0
        DFDP(2,2)=-U(2)**2/DEM
C
        DFDP(1,3)=PAR(1)
        DFDP(2,3)=0.d0
C
        DFDP(1,4)=0.0d0
        DFDP(2,4)=2*PAR(4)*PAR(2)*U(2)**2/DEM**2
C
      RETURN
      END
C
      SUBROUTINE STPNT(NDIM,U,PAR,T)
C     ----------------
C
C Sets parameter values for homoclinic bifurcation analysis (IPS=9).
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(*),PAR(*)
C
C----------------------------------------------------------------------
C Problem parameters (only PAR(1-9) are available to the user) :
C     PAR(11) is the trunction interval or `PERIOD'
C
        PAR(1) = 6.0D0           ! K
        PAR(2) = 0.06729762D0    ! GF
        PAR(3) = 0.01D0          ! D
        PAR(4) = 0.5D0           ! HZ
        PAR(11)= 1046.178D0      ! truncated time interval 
C----------------------------------------------------------------------
C Since IEQUIB>0 put the equilibrium in PAR(11+i), i=1,...,NDIM :
        PAR(12) = 5.738626D0
        PAR(13) = 0.5108401D0 
C----------------------------------------------------------------------
C
      RETURN
      END
C
      SUBROUTINE PVLS
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




