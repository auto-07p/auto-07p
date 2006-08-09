C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   man :  Stable manifold of the origen in the Lorenz model 
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C      
       Period=PAR(11)
C      
       F(1)= Period * (PAR(3) * (U(2)- U(1)))
       F(2)= Period * (PAR(1)*U(1) - U(2) - U(1)*U(3))
       F(3)= Period * (U(1)*U(2) -  PAR(2)*U(3))
C 
      RETURN 
      END 
C---------------------------------------------------------------------- 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
      DIMENSION V1(3),V2(3)
C
       PAR(1)=28.
       PAR(2)=8.d0/3.d0 
       PAR(3)=10. 
C
       rad=5.0
       h=0.1
       Period=1e-5
       RLength=0.
C
       PAR(4)=rad
       PAR(5)=h
C
       CALL EIGV(NDIM,PAR,V1,V2)
C
C Set initial approximate solution (for small Period)
       pi=4*DATAN(1.d0)
       theta=2*pi*h
       cs=DCOS(theta)
       sn=DSIN(theta)
C
       U(1)= rad * ( cs*V1(1) + sn*V2(1) )
       U(2)= rad * ( cs*V1(2) + sn*V2(2) )
       U(3)= rad * ( cs*V1(3) + sn*V2(3) )
C
       PAR(6) = U(1)
       PAR(7) = U(2)
       PAR(8) = U(3)
       PAR(9) = SQRT(U(1)**2 + U(2)**2 + U(3)**2)      
       PAR(11)=Period   
       PAR(12)=RLength
C
      RETURN 
      END 
C---------------------------------------------------------------------- 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC),DBC(NBC,*) 
      DIMENSION V1(3),V2(3)
C 
       CALL EIGV(NDIM,PAR,V1,V2)
C 
       pi=4*DATAN(1.d0)
       rad=PAR(4)
       h=PAR(5)
       theta=2*pi*h
       cs=DCOS(theta)
       sn=DSIN(theta)
C At time=0
       FB(1)= U0(1) - rad * ( cs*V1(1) + sn*V2(1) )
       FB(2)= U0(2) - rad * ( cs*V1(2) + sn*V2(2) )
       FB(3)= U0(3) - rad * ( cs*V1(3) + sn*V2(3) )
C At time=1
       FB(4)= U1(1) - PAR(6)
       FB(5)= U1(2) - PAR(7)
       FB(6)= U1(3) - PAR(8)
       FB(7)= SQRT(U1(1)**2 + U1(2)**2 + U1(3)**2) - PAR(9)
C
      RETURN 
      END 
C---------------------------------------------------------------------- 
      SUBROUTINE EIGV(NDIM,PAR,V1,V2) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),V1(NDIM),V2(NDIM)
      COMMON /FIRST/ ifirst
C
C Stable eigenvalue/vector 1
       V1(1)= 0.
       V1(2)= 0.
       V1(3)= 1./2.66667
C 
C Stable eigenvalue/vector 2
       e=-(11+DSQRT(1201.d0))/2
       V2(1)=1+e
       V2(2)=PAR(1)
       V2(3)= 0.
       ss=DSQRT(V2(1)**2+V2(2)**2)
       V2(1)=V2(1)/(ss*22.8277)
       V2(2)=V2(2)/(ss*22.8277)
       IF(ifirst.NE.123)THEN
         WRITE(10,*)V2(1),V2(2)
         ifirst=123
       ENDIF
C
      RETURN 
      END  
C---------------------------------------------------------------------- 
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM) 
      DIMENSION FI(NINT),DINT(NINT,*),ICP(*),PAR(*) 
      DIMENSION FF(3)
C 
       CALL FUNC(NDIM,U,ICP,PAR,0,FF,DFDU,DFDP) 
       FI(1)=SQRT(FF(1)**2 + FF(2)**2 + FF(3)**2 ) - PAR(12) 
C
      RETURN 
      END 
C
      SUBROUTINE PVLS
      RETURN
      END
C 
      SUBROUTINE FOPT 
      RETURN 
      END 
