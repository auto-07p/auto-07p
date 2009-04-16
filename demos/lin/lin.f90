!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   lin :    A linear ODE eigenvalue problem
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
       SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!      ---------- ----
!
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),PAR(*),F(NDIM)
!
        PI=4*ATAN(1.0D0)
        F(1) = U(2)
        F(2) = -( PAR(1)*PI )**2 * U(1)
!
       RETURN
       END
!
       SUBROUTINE STPNT(NDIM,U,PAR,T)
!      ---------- -----
!
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),PAR(*)
!
        PAR(1)=0.
        PAR(2)=0.
        PAR(3)=0.
!
        U(1)=0.0
        U(2)=0.0
!
       RETURN
       END
!
       SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
!      ---------- ----
!
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
!
        FB(1)=U0(1)-PAR(2)
        FB(2)=U1(1)
!
       RETURN
       END
!
       SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
!      ---------- ----
!
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
       DIMENSION FI(NINT),ICP(*),PAR(*)
!
        FI(1)=U(1)*U(1)-PAR(3)
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
