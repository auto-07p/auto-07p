!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   hom : Homoclinic bifurcations in general test example equation 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,*),DFDP(NDIM,*)
! 
      F(1)= PAR(1)*U(1) + PAR(2)*U(2) - PAR(1)*U(1)*U(1)                &
          + (PAR(8) - PAR(4)*U(3)) * U(1)*(2.0D0 - 3.0D0*U(1))
      F(2)= PAR(2)*U(1) + PAR(1)*U(2)                                   &
          - 1.5D0*PAR(2)*U(1)*U(1) - 1.5D0*PAR(1)*U(1)*U(2)             &
          - (PAR(8) - PAR(4)*U(3)) * 2.0D0*U(2)
      F(3)= PAR(3)*U(3) + PAR(7)*U(1) + PAR(6)*U(1)*U(3)                &
          + PAR(4)*PAR(5)*(U(1)*U(1)*(1.0D0-U(1))-U(2)*U(2))
!
      IF(IJAC.EQ.0)RETURN
!
      DFDU(1,1)= PAR(1) - 2.0D0*PAR(1)*U(1)                             &
          + (PAR(8)-PAR(4)*U(3)) * (2.0D0-6.0D0*U(1))
      DFDU(1,2)= PAR(2) 
      DFDU(1,3)= - PAR(4) * U(1)*(2.0D0-3.0D0*U(1))
!     
      DFDU(2,1)= PAR(2) - 3.0D0*PAR(2)*U(1) - 1.5D0*PAR(1)*U(2)
      DFDU(2,2)= PAR(1) - 1.5D0*PAR(1)*U(1)                             &
          - (PAR(8)-PAR(4)*U(3)) * 2.0D0
      DFDU(2,3)= 2.0D0*PAR(4)*U(2)
!
      DFDU(3,1)= PAR(7) + PAR(6)*U(3)                                   &
          + PAR(4)*PAR(5) * U(1)*(2.0D0-3.0D0*U(1))
      DFDU(3,2)= -2.0D0*PAR(4)*PAR(5) * U(2)
      DFDU(3,3)= PAR(3) + PAR(6)*U(1)
!
      IF(IJAC.EQ.1)RETURN
!
! No parameter derivatives are specified with this example
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
!
      DIMENSION U(NDIM),PAR(*)
!
! COMMON block needed if IPS=9 (homoclinic bifurcations) :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
!
!----------------------------------------------------------------------
! Problem parameters (only PAR(1-9) are available to the user) :
!
        PAR(1) = 0.0D0           ! a
        PAR(2) = 1.0D0           ! b
        PAR(3) = -2.0D0          ! c
        PAR(4) = 0.0D0           ! alpha
        PAR(5) = 1.0D0           ! beta
        PAR(6) = 0.0D0           ! gamma
        PAR(7) = 0.0D0           ! mu
        PAR(8) = 0.0D0           ! tilde mu
!
        PAR(11)=  20.0D0         ! truncated time interval 
!----------------------------------------------------------------------
! If IEQUIB=1 then put initial equilibrium in PAR(11+i), i=1,...,NDIM :
!
        IF (IEQUIB.NE.0) THEN
          PAR(12) = 0.0
          PAR(13) = 0.0
          PAR(14) = 0.0
        ENDIF
!----------------------------------------------------------------------
! IF ISTART=2 then put analytic homoclinic orbit here with T in the
!   interval [0,1]
! 
! test example (a=0,b=1)
!
      S=(T-0.5)*PAR(11)
      U(1) = 1.0D0 - ( (1.0D0-DEXP(S))/(1.0D0+DEXP(S)) )**2
      U(2) = 4.0D0 * DEXP(S) * (1.0D0-DEXP(S)) / (1.0D0+DEXP(S))**3
      U(3) = 0.0D0
!
      RETURN
      END
!
!
!
      SUBROUTINE PVLS(NDIM,U,PAR)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*)
! Homoclinic bifurcations COMMON block needed here :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB
!
! If IEQUIB=0 put analytic equilibrium in PAR(11+i), i=1,...,NDIM :
!
      DO I=1,NDIM
      PAR(11+i)=0
      ENDDO
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




