C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   enz :    A two-cell, one-substrate enzyme model 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       R(S)=S/(1+S+RK*S**2)
       S1=U(1)
       S2=U(2)
       S0=PAR(1)
       RM=PAR(2)
       RH=PAR(3)
       RK=PAR(4)
C
       F(1)=(S0   -S1) + (S2-S1) - RH * R(S1)
       F(2)=(S0+RM-S2) + (S1-S2) - RH * R(S2)
C
      RETURN
      END
C
      SUBROUTINE STPNT(NDIM,U,PAR)
C     ---------- -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
C
       PAR(1)=0.
       PAR(2)=0.
       PAR(3)=100.
       PAR(4)=1.
C
       U(1)=0.0
       U(2)=0.0
C
      RETURN
      END
C
      SUBROUTINE BCND
      END
C
      SUBROUTINE ICND
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
