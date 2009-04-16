!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   enz :    A two-cell, one-substrate enzyme model 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
!
       R(S)=S/(1+S+RK*S**2)
       S1=U(1)
       S2=U(2)
       S0=PAR(1)
       RM=PAR(2)
       RH=PAR(3)
       RK=PAR(4)
!
       F(1)=(S0   -S1) + (S2-S1) - RH * R(S1)
       F(2)=(S0+RM-S2) + (S1-S2) - RH * R(S2)
!
      RETURN
      END
!
      SUBROUTINE STPNT(NDIM,U,PAR)
!     ---------- -----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
!
       PAR(1)=0.
       PAR(2)=0.
       PAR(3)=100.
       PAR(4)=1.
!
       U(1)=0.0
       U(2)=0.0
!
      RETURN
      END
!
      SUBROUTINE BCND
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
