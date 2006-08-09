C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   ezp :    Complex bifurcation in a boundary value problem
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 PAR(*)
      DIMENSION U(NDIM),F(NDIM)
C
       U1=U(1)
       U2=U(2)
C
       RL=PAR(1)
C
       E=CDEXP(U1)
       F(1)=U2
       F(2)=-RL*E
C
      RETURN
      END
C
      SUBROUTINE STPNT(NDIM,U,PAR,T)
C     ---------- -----
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 PAR(*)
      DIMENSION U(NDIM)
C
       U(1)=0
       U(2)=0
C
      RETURN
      END
C
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C     ---------- ----
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      REAL*8 PAR(*)
      DIMENSION ICP(*),U0(NDIM),U1(NDIM),F(NBC)
C
       F(1)=U0(1)
       F(2)=U1(1)
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
