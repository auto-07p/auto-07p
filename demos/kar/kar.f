C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   kar :       The Von Karman swirling flow
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       U1=U(1)
       U2=U(2)
       U3=U(3)
       U4=U(4)
       U5=U(5)
C
       GAMMA=PAR(1)
       ZINF =PAR(3)
C
       F(1)=ZINF*U2
       F(2)=ZINF*U3
       F(3)=ZINF*( - 2*GAMMA*U4 + U2*U2 - 2*U1*U3 - U4*U4)
       F(4)=ZINF*U5
       F(5)=ZINF*(2*GAMMA*U2 + 2*U2*U4 - 2*U1*U5)
C
      RETURN
      END
C
      SUBROUTINE STPNT(NDIM,U,PAR,T)
C     ---------- -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
C
       GAMMA=1.
       FINF =0.
       ZINF =500.
C
       PAR(1)=GAMMA 
       PAR(2)=-FINF
       PAR(3)=ZINF
C
       U(1)=0.
       U(2)=0.
       U(3)=0.
       U(4)=0.
       U(5)=0.
C
      RETURN
      END
C
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),F(NBC)
C
       GAMMA=PAR(1)
       FINF=-PAR(2)
C
       C=DSQRT(FINF**4 + 4*GAMMA**2)
       A=DSQRT(C + FINF**2) / DSQRT(2.d0)
       B=DSQRT(C - FINF**2) / DSQRT(2.d0)
C
       F(1)=U0(1)
       F(2)=U0(2)
       F(3)=U0(4)-1+GAMMA
       F(4)=(FINF+A)*U1(2) + U1(3) - GAMMA*U1(4)/A
       F(5)=A*B**2*U1(2)/GAMMA + (FINF+A)*U1(4) + U1(5)
       F(6)=U1(1)-FINF
C
      RETURN
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
