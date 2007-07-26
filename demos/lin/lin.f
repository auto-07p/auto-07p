C---------------------------------------------------------------------
C---------------------------------------------------------------------
C   lin :    A linear ODE eigenvalue problem
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
       SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C      ---------- ----
C
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),PAR(*),F(NDIM)
C
        PI=4*DATAN(1.0D 00)
        F(1) = U(2)
        F(2) = -( PAR(1)*PI )**2 * U(1)
C
       RETURN
       END
C
       SUBROUTINE STPNT(NDIM,U,PAR,T)
C      ---------- -----
C
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),PAR(*)
C
        PAR(1)=0.
        PAR(2)=0.
        PAR(3)=0.
C
        U(1)=0.0
        U(2)=0.0
C
       RETURN
       END
C
       SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
C      ---------- ----
C
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
C
        FB(1)=U0(1)-PAR(2)
        FB(2)=U1(1)
C
       RETURN
       END
C
       SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
C      ---------- ----
C
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
       DIMENSION FI(NINT),ICP(*),PAR(*)
C
        FI(1)=U(1)*U(1)-PAR(3)
C
       RETURN
       END
C
       SUBROUTINE FOPT
       RETURN
       END
C 
      SUBROUTINE PVLS
      RETURN 
      END 
