C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   kpr : Homoclinic bifurcations in Koper's extended Van der Pol model
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C 
        F(1)=PAR(3) * ( PAR(2)*U(2) - U(1)**3 + 3.0*U(1) - PAR(1) )
        F(2)=U(1) - 2*U(2) + U(3)
        F(3)=U(2) - U(3)
C
      IF(IJAC.EQ.0)RETURN
C
        DFDU(1,1)=PAR(3)*(-3.0*U(1)**2 + 3.0)
        DFDU(1,2)=PAR(3)*PAR(2)
        DFDU(1,3)=0.0
C
        DFDU(2,1)=1.0
        DFDU(2,2)=-2.0
        DFDU(2,3)=1.0
C
        DFDU(3,1)=0.0
        DFDU(3,2)=1.0
        DFDU(3,3)=-1.0
C
      IF(IJAC.EQ.1)RETURN
C
        DFDP(1,1)=- PAR(3)
        DFDP(2,1)=0.d0
        DFDP(3,1)=0.d0
C
        DFDP(1,2)=PAR(3) *U(2)
        DFDP(2,2)=0.d0
        DFDP(3,2)=0.d0
C
        DFDP(1,3)=PAR(2)*U(2) - U(1)**3 + 3.0*U(1) - PAR(1)
        DFDP(2,3)=0.d0
        DFDP(3,3)=0.d0

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
C COMMON block needed if IPS=9 (homoclinic bifurcations) :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB
C
C----------------------------------------------------------------------
C Problem parameters (only PAR(1-9) are available to the user) :
C
        PAR(1) = -1.8512    ! lambda
        PAR(2) = -0.15D0         ! kappa 
        PAR(3) = 10.0d0          ! 1/epsilon_1
C
        PAR(11)=  0.1            ! truncated time interval 
C----------------------------------------------------------------------
C If IEQUIB=1 then put the equilibrium in PAR(11+i), i=1,...,NDIM :
C
        IF (IEQUIB.NE.0) THEN
          PAR(12) = -0.9591016
          PAR(13) = -0.9591016 
          PAR(14) = -0.9591016 
        ENDIF
C----------------------------------------------------------------------
C Distance along the unstable manifold :
C
        IF (ISTART.EQ.3) THEN
          PAR(12+NDIM*IEQUIB)=-0.00001
        ENDIF
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




