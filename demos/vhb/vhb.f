C---------------------------------------------------------------------
C---------------------------------------------------------------------
C   lin :    A problem with a "vertical" Hopf bifurcation
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
       SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C      ---------- ----
C
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION U(NDIM),PAR(*),F(NDIM)
C
        F(1) = PAR(1)*U(1) - U(2)
        F(2) = U(1)*(1-U(1))
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
        PAR(1)=-1.
C
        U(1)=0.0
        U(2)=0.0
C
       RETURN
       END
C
       SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
       RETURN
       END
C
       SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
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
