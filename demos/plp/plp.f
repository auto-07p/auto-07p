C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   1cl :    A one-cell, two-substrate enzyme model 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*)
C
       S=U(1)
       A=U(2)
C
       S0=PAR(1)
       A0=PAR(2)
       AL=PAR(3)
       RH=PAR(4)
       RK=PAR(5)
C
       D=1+S+RK*S**2
       R=S*A/D
C
       F(1)=   (S0-S) - RH*R
       F(2)=AL*(A0-A) - RH*R
C
       IF(IJAC.EQ.0)RETURN
C
       DRDS=( A*D - S*A*(1+2*RK*S) ) / D**2
       DRDA=S/D
       DRDK=-S**3*A/D**2
C
       DFDU(1,1)=-1 - RH*DRDS
       DFDU(1,2)=   - RH*DRDA
       DFDU(2,1)=   - RH*DRDS
       DFDU(2,2)=-AL- RH*DRDA
C 
      IF(IJAC.EQ.1)RETURN 
C
C      *Parameter derivatives
C
       DFDP(1,1)=1
       DFDP(1,2)=0
       DFDP(1,3)=0
       DFDP(1,4)=-R
       DFDP(1,5)=-RH*DRDK
C
       DFDP(2,1)=0
       DFDP(2,2)=AL
       DFDP(2,3)=A0-A
       DFDP(2,4)=-R
       DFDP(2,5)=-RH*DRDK
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
       PAR(1)=110.
       PAR(2)=500.
       PAR(3)=0.2
       PAR(4)=2.021628
       PAR(5)=0.1
C
       U(1)=4.555974E+01
       U(2)=1.777987E+02
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
