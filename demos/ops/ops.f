C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C       ops :    Optimization of periodic solutions 
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM),DFDU(NDIM,NDIM),DFDP(NDIM,*)
C
       x=U(1)
       y=U(2)
       z=U(3)
C
       p1=PAR(1)
       p2=PAR(2)
       p3=PAR(3)
       p4=PAR(4)
C 
       F(1)=( -p4*(x**3/3-x) + (z-x)/p2 - y ) / p1  
       F(2)=x-p3 
       F(3)=-(z-x)/p2 
C     
      IF(IJAC.EQ.0)RETURN
C
       DFDU(1,1)=( -p4*(x**2-1) - 1/p2 ) /p1
       DFDU(1,2)=-1/p1
       DFDU(1,3)=1/(p2*p1)
C
       DFDU(2,1)=1
       DFDU(2,2)=0
       DFDU(2,3)=0
C
       DFDU(3,1)=1/p2
       DFDU(3,2)=0
       DFDU(3,3)=-1/p2
C     
      IF(IJAC.EQ.1)RETURN
C
C      *Parameter derivatives
       DO 2 I=1,3
         DO 1 J=1,9
           DFDP(I,J)=0.d0
 1       CONTINUE
 2     CONTINUE
C
       DFDP(1,1)=-( -p4*(x**3/3-x) + (z-x)/p2 - y )/p1**2
       DFDP(1,2)=-(z-x)/(p2**2*p1)
       DFDP(1,3)=0
       DFDP(1,4)=-(x**3/3-x)/p1
C
       DFDP(2,1)=0
       DFDP(2,2)=0
       DFDP(2,3)=-1
       DFDP(2,4)=0
C
       DFDP(3,1)=0
       DFDP(3,2)=(z-x)/p2**2
       DFDP(3,3)=0
       DFDP(3,4)=0
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
      p1=0.5
      p2=4
      p3=0.9
      p4=2.
C
      U(1)=p3 
      U(2)=-p4*(p3**3/3-p3)
      U(3)=p3 
C 
      PAR(1)=p1 
      PAR(2)=p2
      PAR(3)=p3 
      PAR(4)=p4
C 
      RETURN 
      END 
C
      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP)
C     ---------- ----
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),ICP(*),PAR(*),DFDU(NDIM),DFDP(*)
C
       FS=PAR(3)
C
      IF(IJAC.EQ.0)RETURN
C
       DO 1 I=1,NDIM
         DFDU(I)=0.d0
 1     CONTINUE
C     
      IF(IJAC.EQ.1)RETURN
C
C      *Parameter derivatives
       DO 2 I=1,9
         DFDP(I)=0.d0
 2     CONTINUE
C
       DFDP(3)=1.d0
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
      SUBROUTINE PVLS
      RETURN 
      END 
