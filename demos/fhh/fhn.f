C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   fhn : Homoclinic bifurcations in Fitz-Hugh Nagumo System
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C 
      F(1)= U(2)
      F(2)= PAR(1)*U(2)+U(1)*(U(1)-PAR(2))*(U(1)-1.0)+U(3)
      F(3)= PAR(3)*U(1)/PAR(1)
C
      IF(IJAC.EQ.0)RETURN
C
      DFDU(1,1)= 0.0
      DFDU(1,2)= 1.0 
      DFDU(1,3)= 0.0
C     
      DFDU(2,1)= 3*U(1)*U(1)-2*(1+PAR(2))*U(1)+PAR(2)
      DFDU(2,2)= PAR(1)
      DFDU(2,3)= 1.0
C
      DFDU(3,1)= PAR(3)/PAR(1)
      DFDU(3,2)= 0.0
      DFDU(3,3)= 0.0
C
      IF(IJAC.EQ.1)RETURN
C
C No parameter derivatives are specified with this example
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
C COMMON block needed if IPS=9 (homoclinic bifurcations) :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB
C
C----------------------------------------------------------------------
C Problem parameters (only PAR(1-9) are available to the user) :
C
        PAR(1) = 0.21         ! c
        PAR(2) = 0.2         ! a 
        PAR(3) = 0.0025      ! b = epsilon
C
        PAR(11)=  0.1            ! truncated time interval 
C----------------------------------------------------------------------
C If IEQUIB=1 then put the equilibrium in PAR(11+i), i=1,...,NDIM :
C
        IF (IEQUIB.NE.0) THEN
          PAR(12) = 0.0
          PAR(13) = 0.0
          PAR(14) = 0.0
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




