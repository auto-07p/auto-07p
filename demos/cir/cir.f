C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   cir :    Homoclinic Bifurcation in an Electronic Circuit
C                (the same equations as in demo tor)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)
C
       rn=PAR(1) 
       be=PAR(2) 
       ga=PAR(3) 
       r =PAR(4) 
       a3=PAR(5) 
       b3=PAR(6) 
C
       x=U(1)
       y=U(2)
       z=U(3)
C
       F(1)= ( -(be+rn)*x + be*y - a3*x**3 + b3*(y-x)**3 )/r
       F(2)=  be*x - (be+ga)*y - z - b3*(y-x)**3
       F(3)= y
C
       IF(IJAC.EQ.0)RETURN
C 
       DFDU(1,1)=( -(be+rn) -3*a3*x**2 - 3*b3*(y-x)**2  )/r
       DFDU(1,2)=( be + 3*b3*(y-x)**2 )/r
       DFDU(1,3)=0
C 
       DFDU(2,1)=be + 3*b3*(y-x)**2
       DFDU(2,2)=-(be+ga) - 3*b3*(y-x)**2
       DFDU(2,3)=-1
C 
       DFDU(3,1)=0
       DFDU(3,2)=1
       DFDU(3,3)=0
C 
      IF(IJAC.EQ.1)RETURN 
C
C      *Parameter derivatives
       DFDP(1,1)=-x/r
       DFDP(2,1)=0
       DFDP(3,1)=0
C
       DFDP(1,2)=( -x + y )/r
       DFDP(2,2)=x-y
       DFDP(3,2)=0
C
       DFDP(1,3)=0
       DFDP(2,3)=-y
       DFDP(3,3)=0
C
       DFDP(1,4)=-F(1)/r
       DFDP(2,4)=0
       DFDP(3,4)=0
C
       DFDP(1,5)=x**3/r
       DFDP(2,5)=0
       DFDP(3,5)=0
C
       DFDP(1,6)=(y-x)**3 / r
       DFDP(2,6)=-(y-x)**3
       DFDP(3,6)=0
C
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*) 
C 
C COMMON block needed if IPS=9 (homoclinic bifurcations) :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB
C
C----------------------------------------------------------------------
C Problem parameters (only PAR(1-9) are available to the user) :
C
       PAR(1)=-0.721309D0         ! nu
       PAR(2)=0.6                 ! beta
       PAR(3)=0.0                 ! gamma
       PAR(4)=0.6                 ! r
       PAR(5)=0.328578            ! a_3
       PAR(6)=0.933578            ! b_3
C
       PAR(11)=36.13	! Truncated time interval 
C
C----------------------------------------------------------------------
C If IEQUIB >0 put initial equilibrium in PAR(11+i), i=1,...,NDIM :
C
      PAR(12) = 0.0
      PAR(13) = 0.0
      PAR(14) = 0.0
C 
      RETURN 
      END 
C
      SUBROUTINE PVLS(NDIM,U,PAR)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(*),PAR(*)
C Homoclinic bifurcations COMMON block needed here :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB
C
C If IEQUIB =0 put analytic equilibrium in PAR(11+i), i=1..NDIM
C
      IF(IEQUIB.EQ.0)THEN
        DO I=1,NDIM
          PAR(11+I)= 0.0
        ENDDO
      ENDIF
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








