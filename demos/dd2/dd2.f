C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   dd2 :    Basic computations for discrete dynamical systems
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
      DIMENSION F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*) 
C 
       F(1)=PAR(1)*U(1)*(1-U(1)) - PAR(2)*U(1)*U(2) 
       F(2)=(1-PAR(3))*U(2) + PAR(2)*U(1)*U(2) 
C 
      IF(IJAC.EQ.0)RETURN 
C 
       DFDU(1,1)=PAR(1)*(1-2*U(1))-PAR(2)*U(2) 
       DFDU(1,2)=-PAR(2)*U(1) 
       DFDU(2,1)=PAR(2)*U(2) 
       DFDU(2,2)=1-PAR(3) + PAR(2)*U(1) 
C 
      IF(IJAC.EQ.1)RETURN 
C 
       DFDP(1,1)=U(1)*(1-U(1)) 
       DFDP(2,1)=0.0 
       DFDP(1,2)=-U(1)*U(2) 
       DFDP(2,2)= U(1)*U(2) 
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
       PAR(1)=0.0 
       PAR(2)=0.2 
       PAR(3)=0.1 
C 
       U(1)=0.0 
       U(2)=0.0 
C 
      RETURN 
      END 
C
      SUBROUTINE BCND 
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
