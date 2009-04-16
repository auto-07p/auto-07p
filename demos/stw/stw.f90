!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   stw :         Sharp traveling waves
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! Parameter assignment: ( Components of PAR(.) )
!
!      PAR :             (1)  : homotopy parameter
!                   (2)  (3)  : A0, A1, A2      (problem parameters)
!              (4)  (5)  (6)  : B0, B1, B2      (problem parameters)
!              (7)  (8)  (9)  : C0, C1, C2      (problem parameters)
!                       (10)  :         c       (wave speed)
!                       (11)  :         T       (period)
!                  (12) (13)  : eps-0, eps-1    (radii)
!                  (14) (15)  : mu-0 , mu-1     (eigenvalues)
!                  (16) (17)  : v-0(1), v-0(2)  (eigenvector)
!                  (18) (19)  : v-1(1), v-1(2)  (eigenvector)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
! 
       PERIOD=PAR(11)
       CALL FFFF(NDIM,U,ICP,PAR,IJAC,F,DFDU)
       F(1)=PERIOD*F(1)
       F(2)=PERIOD*F(2)
! 
      RETURN 
      END 
!
      SUBROUTINE FFFF(NDIM,U,ICP,PAR,IJAC,F,DFDU)
!     ---------- ----
!
! The reduced system for traveling waves is defined here. A separate subroutine 
! is used because the system and the Jacobian are also needed in the subroutines
! BCND and ICND below. The computation should be done with JAC=0. The derivatives
! below are for use in BCND and ICND only. 
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,NDIM)
!
! A0, A1, A2:
       PAR(2)= (1-PAR(1))* 2   + PAR(1)* 2
       PAR(3)= (1-PAR(1))* 0   + PAR(1)* 1
!
! B0, B1, B2:
       PAR(4)= (1-PAR(1))* 2   + PAR(1)* 0
       PAR(5)= (1-PAR(1))* 0   + PAR(1)* 1
       PAR(6)= (1-PAR(1))* 0   + PAR(1)* 0
!
! C0, C1, C2:
       PAR(7)= (1-PAR(1))* 0   + PAR(1)* 0
       PAR(8)= (1-PAR(1))* 1   + PAR(1)* 1
       PAR(9)= (1-PAR(1))*(-1) + PAR(1)*(-1)
!
      FA     =          PAR(2)*U(1) + PAR(3)*U(1)**2
      FB     = PAR(4) + PAR(5)*U(1) + PAR(6)*U(1)**2
      FC     = PAR(7) + PAR(8)*U(1) + PAR(9)*U(1)**2
!
      C      = PAR(10)
!
       F(1)= FA * U(2)
       F(2)= -C * U(2)  -  FB * U(2)**2  -  FC
!
      IF(IJAC.EQ.0)RETURN
!
      DFA    = PAR(2) + 2*PAR(3)*U(1)
      DFB    = PAR(5) + 2*PAR(6)*U(1)
      DFC    = PAR(8) + 2*PAR(9)*U(1)
!
       DFDU(1,1)= DFA*U(2)
       DFDU(1,2)= FA
!
       DFDU(2,1)= -DFB*U(2)**2 - DFC
       DFDU(2,2)= -C - 2*FB*U(2)
!
      RETURN
      END
! 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- ----- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
! 
! homotopy parameter :
       PAR(1)= 0
!
! starting period
       PERIOD=100.
!
! c, T:
       PAR(10)= 1
       PAR(11)= PERIOD
!
! eps-0, eps-1:
       PAR(12)= 0.5*DSQRT(5.D0)/(1+EXP(0.5*PERIOD))
       PAR(13)= PAR(12)
!
!  mu-0,  mu-1:
       PAR(14)= 1
       PAR(15)= -1
!
! v-0(1), v-0(2):
       PAR(16)= 2/DSQRT(5.D0)
       PAR(17)= 1/DSQRT(5.D0)
!
! v-1(1), v-1(2):
       PAR(18)= 1
       PAR(19)= 0
!
! Exact solution
       TSC=PERIOD*(T-0.5)
       E=DEXP(TSC)
       U(1)=1/(1+E)
       U(2)=-0.5*E/(1+E)
!
      RETURN
      END

      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC),DBC(NBC,*)
! Local
      DIMENSION V0(2),V1(2),G0(2),G1(2),DGDU0(2,2),DGDU1(2,2)
!
      V0(1)=U0(1) - PAR(12)*PAR(16)
      V0(2)=U0(2) - PAR(12)*PAR(17)
      V1(1)=U1(1) - PAR(13)*PAR(18)
      V1(2)=U1(2) - PAR(13)*PAR(19)
!
      CALL FFFF(NDIM,V0,ICP,PAR,1,G0,DGDU0)
      CALL FFFF(NDIM,V1,ICP,PAR,1,G1,DGDU1)
!
! Define eigenvalues and eigenvectors at t=0:
      FB(1)= DGDU0(1,1)*PAR(16) + DGDU0(1,2)*PAR(17) - PAR(14)*PAR(16)
      FB(2)= DGDU0(2,1)*PAR(16) + DGDU0(2,2)*PAR(17) - PAR(14)*PAR(17)
!
! Define eigenvalues and eigenvectors at t=1:
      FB(3)= DGDU1(1,1)*PAR(18) + DGDU1(1,2)*PAR(19) - PAR(15)*PAR(18)
      FB(4)= DGDU1(2,1)*PAR(18) + DGDU1(2,2)*PAR(19) - PAR(15)*PAR(19)
!
! Normalize the eigenvectors:
      FB(5)= PAR(16)**2 + PAR(17)**2 -1
      FB(6)= PAR(18)**2 + PAR(19)**2 -1
!
! Boundary condition at t=0:
      FB(7)= G0(1)
      FB(8)= G0(2)
!
! Boundary condition at t=1:
      FB(9)= G1(1)
      FB(10)=G1(2)
!
      RETURN
      END
!
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DIMENSION FI(NINT),DINT(NINT,1),ICP(*),PAR(*)
! Local
      DIMENSION DFDU(2,2),F(2),F0(2)
!
      CALL FFFF(NDIM,U   ,ICP,PAR,1,F ,DFDU )
      CALL FFFF(NDIM,UOLD,ICP,PAR,0,F0,DGDU0)
!
! The integral phase condition is defined here:
       FI(1)= ( F(1) - F0(1) ) * ( DFDU(1,1)*F(1) + DFDU(1,2)*F(2) ) &
            + ( F(2) - F0(2) ) * ( DFDU(2,1)*F(1) + DFDU(2,2)*F(2) )
!
      RETURN
      END
! 
      SUBROUTINE FOPT 
      RETURN 
      END 
! 
      SUBROUTINE PVLS
      RETURN 
      END 
