!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   fhn : Homoclinic bifurcations in Fitz-Hugh Nagumo System
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      F(1)= U(2)
      F(2)= PAR(1)*U(2)+U(1)*(U(1)-PAR(2))*(U(1)-1)+U(3)
      F(3)= PAR(3)*U(1)/PAR(1)

      IF(IJAC.EQ.0)RETURN

      DFDU(1,1)= 0
      DFDU(1,2)= 1
      DFDU(1,3)= 0

      DFDU(2,1)= 3*U(1)*U(1)-2*(1+PAR(2))*U(1)+PAR(2)
      DFDU(2,2)= PAR(1)
      DFDU(2,3)= 1

      DFDU(3,1)= PAR(3)/PAR(1)
      DFDU(3,2)= 0
      DFDU(3,3)= 0

      IF(IJAC.EQ.1)RETURN

      DFDP(1,1)= 0
      DFDP(1,2)= 0
      DFDP(1,3)= 0

      DFDP(2,1)= U(2)
      DFDP(2,2)= -U(1)*(U(1)-1)
      DFDP(2,3)= 0

      DFDP(3,1)= -PAR(3)*U(1)/PAR(1)**2
      DFDP(3,2)= 0
      DFDP(3,3)= U(1)/PAR(1)

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ----------------

! Sets parameter values for homoclinic bifurcation analysis (IPS=9).

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T
!
! COMMON block needed if IPS=9 (homoclinic bifurcations) :
      INTEGER ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV

!----------------------------------------------------------------------
! Problem parameters (only PAR(1-9) are available to the user) :

        PAR(1) = 0.21         ! c
        PAR(2) = 0.2         ! a 
        PAR(3) = 0.0025      ! b = epsilon

        PAR(11)=  0.1            ! truncated time interval 
!----------------------------------------------------------------------
! If IEQUIB=1 then put the equilibrium in PAR(11+i), i=1,...,NDIM :

        IF (IEQUIB.NE.0) THEN
          PAR(12) = 0.0
          PAR(13) = 0.0
          PAR(14) = 0.0
        ENDIF
!----------------------------------------------------------------------
! Distance along the unstable manifold :

        IF (ISTART.EQ.3) THEN
          PAR(12+NDIM*IEQUIB)=0.00001
        ENDIF
!----------------------------------------------------------------------

      END SUBROUTINE STPNT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT




