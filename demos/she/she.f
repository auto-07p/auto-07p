C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   she : Heteroclinic Orbits In a Model of Shearing Instabilities
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DOUBLE PRECISION MUS,Q,PI2,P12CS,P01CS
      DIMENSION U(*),PAR(*),F(*),DFDU(NDIM,*),DFDP(NDIM,*)      
C
      P11SS=U(1)
      T02CS=U(2)
      P12CS=U(3)
      P01CS=U(4)
      A01CS=U(5)
C
      MUS=PAR(1)
      Q=PAR(2)
      SIGMA=PAR(3)
      ZETA=PAR(4)
      PI2=3.141592654D0*3.141592654D0
C
      F(1)=P11SS*T02CS+P11SS*MUS-P12CS*P01CS 
      F(2)=-(P11SS**2+T02CS)
      F(3)=(4.*SIGMA*P11SS*P01CS+4.*SIGMA*P12CS*MUS-9.
     . *SIGMA*P12CS+4.*P11SS*P01CS+4.*P12CS*MUS)/(4.*(
     . SIGMA+1.))
      F(4)=(-Q*SIGMA**2*A01CS-PI2*SIGMA**2*P01CS+3.*
     . PI2*SIGMA*P11SS*P12CS+3.*PI2*P11SS*P12CS)/(4.*
     . PI2*SIGMA)
      F(5)=(ZETA*(P01CS-A01CS))/4.
C
       IF(IJAC.EQ.0)RETURN
C     
      DO 100 I=1,5
        DO 100 J=1,5
          DFDU(I,J)=0.0D0
 100  CONTINUE
C
      DFDU(1,1)=T02CS+MUS    
      DFDU(1,2)=P11SS
      DFDU(1,3)=-P01CS
      DFDU(1,4)=-P12CS       
C
      DFDU(2,1)=-2.*P11SS
      DFDU(2,2)=(-1.)
C
      DFDU(3,1)=P01CS
      DFDU(3,3)=(4.*SIGMA*MUS-9.*SIGMA+4.*MUS)/(4.*(SIGMA+1.))
      DFDU(3,4)=P11SS
C
      DFDU(4,1)=(3.*P12CS*(SIGMA+1.))/(4.*SIGMA)
      DFDU(4,3)=(3.*P11SS*(SIGMA+1.))/(4.*SIGMA)
      DFDU(4,4)=(-SIGMA)/4.
      DFDU(4,5)=(-Q*SIGMA)/(4.*PI2)
C
      DFDU(5,4)=ZETA/4.
      DFDU(5,5)=(-ZETA)/4.
C     
      IF(IJAC.EQ.1)RETURN 
C
C no parameter derivatives are specified with this example
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
C----------------------------------------------------------------------
C Problem parameters (only PAR(1-9) are available to the user) :
C
       PAR(1)=0.163875d0          ! mu
       PAR(2)=0.0d0               ! q
       PAR(3)=0.5d0               ! sigma
       PAR(4)=0.2                 ! zeta
C
       PAR(11)=85.07	! Truncated time interval 
C
C----------------------------------------------------------------------
C If IEQUIB =-2 put initial approximations to equilibria in 
C   PAR(11+i), i=1,...,NDIM :        left-hand equilibrium
C   PAR(11+i), i=NDIM+1,...,2*NDIM   right-hand equilibrium                    
C
       PAR(12) = 0.4048147d0
       PAR(13) = -0.163875d0
       PAR(14) = 0.0
       PAR(15) = 0.0
       PAR(16) = 0.0
C
       PAR(17) = 0.0
       PAR(18) = 0.0
       PAR(19) = 0.0
       PAR(20) = 0.0
       PAR(21) = 0.0
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
C If IEQUIB =-1 put analytic expressions for equilibria in
C   PAR(11+i), i=1,..,NDIM         left-hand equilibrium
C   PAR(11+i), i=NDIM+1,...,2*NDIM right-hand equilibrium
      IF(IEQUIB.EQ.-1)THEN
        PAR(12)=DSQRT(PAR(1))
        PAR(13)=-PAR(1)
        PAR(14)=0.0d0
        PAR(15)=0.0d0
        PAR(16)=0.0d0
        DO I=NDIM+1,2*NDIM
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








