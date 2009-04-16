!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   ext :    Spurious solutions to a boundary value problem
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
       P= U(1) + U(1)**2 + U(1)**3
       S=DSIN(P)
       PI=4*DATAN(1.d0)
       Q=(PAR(1)*PI)**2
!
       F(1)=U(2)
       F(2)=-Q*S
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
       PAR(1)=0.d0
!
       U(1)=0.d0
       U(2)=0.d0
!
      RETURN
      END
!
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
!
       FB(1)=U0(1)
       FB(2)=U1(1)
!
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
      SUBROUTINE PVLS
      RETURN 
      END 
