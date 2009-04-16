!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   mtn : Homoclinic bifurcations in M. Scheffer's preditor-prey model
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
! 
      DEM=U(2)**2+PAR(4)**2
      PSIZ=0.4D0*U(1)/(0.6D0+U(1))
      PSIF=PAR(2)*U(2)**2/DEM
      PSIZU1=0.4D0*((1/(0.6D0+U(1)))- (U(1)/((0.6D0+U(1))**2)))
      PSIFU2=2*PAR(2)*( (U(2)/DEM) -(U(2)**3/(DEM**2) ))
!
      F(1)=(0.5D0*U(1)*(1.0D0-U(1)/PAR(1))-U(2)*PSIZ+PAR(3)*PAR(1))
      F(2)=(0.6D0*PSIZ*U(2)-0.15D0*U(2)-PSIF)
!
      IF(IJAC.EQ.0)RETURN
!
        DFDU(1,1)=0.5D0-U(1)/PAR(1) -U(2)*PSIZU1
        DFDU(1,2)=-PSIZ
!
        DFDU(2,1)=0.6D0*PSIZU1*U(2)
        DFDU(2,2)=0.6D0*PSIZ-0.15D0-PSIFU2
!
      IF(IJAC.EQ.1)RETURN
!
        DFDP(1,1)=PAR(3)
        DFDP(2,1)=0.d0
!
        DFDP(1,2)=0.0d0
        DFDP(2,2)=-U(2)**2/DEM
!
        DFDP(1,3)=PAR(1)
        DFDP(2,3)=0.d0
!
        DFDP(1,4)=0.0d0
        DFDP(2,4)=2*PAR(4)*PAR(2)*U(2)**2/DEM**2
!
      RETURN
      END
!
      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ----------------
!
! Sets parameter values for homoclinic bifurcation analysis (IPS=9).
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(*),PAR(*)
!
!----------------------------------------------------------------------
! Problem parameters (only PAR(1-9) are available to the user) :
!     PAR(11) is the trunction interval or `PERIOD'
!
        PAR(1) = 6.0D0           ! K
        PAR(2) = 0.06729762D0    ! GF
        PAR(3) = 0.01D0          ! D
        PAR(4) = 0.5D0           ! HZ
        PAR(11)= 1046.178D0      ! truncated time interval 
!----------------------------------------------------------------------
! Since IEQUIB>0 put the equilibrium in PAR(11+i), i=1,...,NDIM :
        PAR(12) = 5.738626D0
        PAR(13) = 0.5108401D0 
!----------------------------------------------------------------------
!
      RETURN
      END
!
      SUBROUTINE PVLS
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




