C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   obv :    Optimization in a boundary value problem
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       P=U(1) + PAR(2)*U(1)**2 + PAR(3)*U(1)**4
       E=EXP(P)
C
       F(1)= U(2)
       F(2)=-PAR(1)*E
       F(3)= PAR(1)*E*(1+2*PAR(2)*U(1)+4*PAR(3)*U(1)**3 )*U(4)
     *                       + PAR(15)*2*(U(1)-1.0)
       F(4)=-U(3)
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
       PAR(1)=0.001
       PAR(10)=1.0
C
       U(1)=0.0
       U(2)=0.0
       U(3)=0.0
       U(4)=0.0
C
      RETURN
      END
C
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
C
       FB(1)=U0(1)
       FB(2)=U1(1)
       FB(3)=U0(3)-PAR(13)
       FB(4)=U0(4)
       FB(5)=U1(3)+PAR(14)
       FB(6)=U1(4)
C
      RETURN
      END
C
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DIMENSION FI(NINT),ICP(*),PAR(*)
C
       P=U(1) + PAR(2)*U(1)**2 + PAR(3)*U(1)**4
       E=EXP(P)
C
       FI(1)=U(3)**2 - PAR(16)
       FI(2)=PAR(10)-(U(1)-1.0)**2
     *         - 0.1*( PAR(1)**2+PAR(2)**2+PAR(3)**2 )
       FI(3)=-E*U(4)-PAR(15)*0.2*PAR(1)
         IF(NINT.EQ.3)RETURN
       FI(4)=-PAR(1)*E*U(1)**2*U(4)-PAR(15)*0.2*PAR(2) - PAR(17)
         IF(NINT.EQ.4)RETURN
       FI(5)=-PAR(1)*E*U(1)**4*U(4)-PAR(15)*0.2*PAR(3) - PAR(18)
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
