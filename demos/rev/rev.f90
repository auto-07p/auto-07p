!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   cir :    Homoclinic Bifurcation in an Electronic Circuit
!                (the same equations as in demo tor)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
!
       P=PAR(1) 
!
       F(1)= U(2)
       F(2)= U(3)
       F(3)= U(4)
       F(4)= -P*U(3)-U(1)+U(1)**3
!
       IF(IJAC.EQ.0)RETURN
! 
       DFDU(1,1)=0.0d0
       DFDU(1,2)=1.0d0
       DFDU(1,3)=0.0d0
       DFDU(1,4)=0.0d0
! 
       DFDU(2,1)=0.0d0
       DFDU(2,2)=0.0d0
       DFDU(2,3)=1.0d0
       DFDU(2,4)=0.0d0
! 
       DFDU(3,1)=0.0d0
       DFDU(3,2)=0.0d0
       DFDU(3,3)=0.0d0
       DFDU(3,4)=1.0d0
! 
       DFDU(4,1)=-1.0+3.0d0*U(1)**2
       DFDU(4,2)=0.0d0
       DFDU(4,3)=-P
       DFDU(4,4)=0.0d0
! 
      IF(IJAC.EQ.1)RETURN 
!
!      *Parameter derivatives
       DFDP(1,1)=0.0d0
       DFDP(2,1)=0.0d0
       DFDP(3,1)=0.0d0
       DFDP(4,1)=-U(3)
!
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*) 
! 
! COMMON block needed if IPS=9 (homoclinic bifurcations) :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
!
!----------------------------------------------------------------------
! Problem parameters (only PAR(1-9) are available to the user) :
!
       PAR(1)=1.6                ! P
!
!----------------------------------------------------------------------
! If IEQUIB >0 put initial equilibrium in PAR(11+i), i=1,...,NDIM :
!
      PAR(12) = 0.0
      PAR(13) = 0.0
      PAR(14) = 0.0
      PAR(15) = 0.0
! 
      RETURN 
      END 
!
      SUBROUTINE PVLS(NDIM,U,PAR)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*)
! Homoclinic bifurcations COMMON block needed here :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB
!
! If IEQUIB =0 put analytic equilibrium in PAR(11+i), i=1..NDIM
!
      IF(IEQUIB.EQ.0)THEN
        DO I=1,NDIM
          PAR(11+I)= 0.0
        ENDDO
      ENDIF
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
! 








