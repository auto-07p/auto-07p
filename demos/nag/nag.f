C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   nag :    Heteroclinic orbits : A saddle-saddle connection 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C Parameter assignment:
C
C           PAR(1) , PAR(2) : a      , c      (parameters)
C           PAR(3) , PAR(4) : eps-0  , eps-1  (radii)
C           PAR(5) , PAR(6) : mu-0   , mu-1   (eigenvalues)
C           PAR(7) , PAR(8) : v-0(1) , v-0(2) (eigenvector)
C           PAR(9) , PAR(10): v-1(1) , v-1(2) (eigenvector)
C           PAR(11)         : period
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C
       CALL FFFF(2,U,ICP,PAR,0,F,DUMMY)
C
       PERIOD=PAR(11)
       F(1)=PERIOD*F(1)
       F(2)=PERIOD*F(2)
C
      RETURN
      END
C
      SUBROUTINE FFFF(NDM,U,ICP,PAR,IJAC,F,DFDU)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDM),PAR(*),F(NDM),DFDU(NDM,NDM)
C
       A=PAR(1)
       C=PAR(2)
       F(1)= U(2)
       F(2)= C*U(2) - U(1) * (1-U(1)) * (U(1)-A)
C
      IF(IJAC.EQ.0)RETURN
C
       DFDU(1,1)= 0
       DFDU(1,2)= 1
C
       DFDU(2,1)= - (1-U(1))*(U(1)-A) + U(1)*(U(1)-A) - U(1)*(1-U(1))
       DFDU(2,2)= C
C
      RETURN
      END
C
      SUBROUTINE STPNT(NDIM,U,PAR,T)
C     ---------- -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /FRST/ IFRST
      DIMENSION U(NDIM),PAR(*)
C
C
C The following initialization is done only in the first call
       IF(IFRST.NE.1)THEN
         IFRST=1
C        Select period and connection (read from the constants-file)
         PERIOD=100.
         SIGN=1.0
         R = 0.5*DSQRT(2.D0)
         V1= 1./DSQRT(1+R**2)
         V2= R /DSQRT(1+R**2)
         X  = -0.5*PERIOD
         E  = DEXP(X/DSQRT(2.D0))
         U0 = E/(1+E)
         V0 = ( E/(1+E)**2 ) / DSQRT(2.D0)
         EPS= DSQRT( U0**2 + V0**2 )
         PAR(1)= 0.5
         PAR(2)= 0
         PAR(3)= EPS
         PAR(4)= EPS
         PAR(5)= R
         PAR(6)=-R
         PAR(7)= SIGN*V1
         PAR(8)= SIGN*V2
         PAR(9)= -SIGN*V1
         PAR(10)= SIGN*V2
         PAR(11)= PERIOD
         PAR(12)= SIGN
       ENDIF
C
C Specify exact solution as starting point :
C
       PERIOD=PAR(11)
       SIGN  =PAR(12)
C
       X=PERIOD*SIGN*(T-0.5)
       E=DEXP(X/DSQRT(2.D0))
       U(1)=E/(1+E)
       U(2)=SIGN*E/(1+E)**2/DSQRT(2.D0)
C
      RETURN
      END
C
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
C Local
      PARAMETER (NDM=2)
      DIMENSION V0(NDM),V1(NDM),G0(NDM),G1(NDM)
      DIMENSION DGDU0(NDM,NDM),DGDU1(NDM,NDM)
C
      V0(1)=U0(1) - PAR(3)*PAR(7)
      V0(2)=U0(2) - PAR(3)*PAR(8)
      V1(1)=U1(1) - PAR(4)*PAR(9)
      V1(2)=U1(2) - PAR(4)*PAR(10)
C
      CALL FFFF(NDM,V0,ICP,PAR,1,G0,DGDU0)
      CALL FFFF(NDM,V1,ICP,PAR,1,G1,DGDU1)
C
      FB(1)= DGDU0(1,1)*PAR(7) + DGDU0(1,2)*PAR(8) - PAR(5)*PAR(7)
      FB(2)= DGDU0(2,1)*PAR(7) + DGDU0(2,2)*PAR(8) - PAR(5)*PAR(8)
      FB(3)= DGDU1(1,1)*PAR(9) + DGDU1(1,2)*PAR(10)- PAR(6)*PAR(9)
      FB(4)= DGDU1(2,1)*PAR(9) + DGDU1(2,2)*PAR(10)- PAR(6)*PAR(10)
      FB(5)= PAR(7)**2 + PAR(8)**2 -1
      FB(6)= PAR(9)**2 + PAR(10)**2 -1
      FB(7)= G0(1)
      FB(8)= G0(2)
      FB(9)= G1(1)
      FB(10)=G1(2)
C
      RETURN
      END
C
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION U(NDIM),UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DIMENSION FI(NINT),ICP(*),PAR(*)
C Local
      PARAMETER (NDM=2)
      DIMENSION F(NDM),F0(NDM),DFDU(NDM,NDM)
C
      CALL FFFF(NDM,U   ,ICP,PAR,1,F ,DFDU)
      CALL FFFF(NDM,UOLD,ICP,PAR,0,F0,DUMMY)
C
       FI(1)= ( F(1) - F0(1) ) * ( DFDU(1,1)*F(1) + DFDU(1,2)*F(2) )
     *      + ( F(2) - F0(2) ) * ( DFDU(2,1)*F(1) + DFDU(2,2)*F(2) )
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
