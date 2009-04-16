!---------------------------------------------------------------------
!---------------------------------------------------------------------
!   lin :    A problem with a "vertical" Hopf bifurcation
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
       SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!      ---------- ----
!
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),PAR(*),F(NDIM)
!
        F(1) = PAR(1)*U(1) - U(2)
        F(2) = U(1)*(1-U(1))
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
        PAR(1)=-1.
!
        U(1)=0.0
        U(2)=0.0
!
       RETURN
       END
!
       SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
       RETURN
       END
!
       SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
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
