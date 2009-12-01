!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   she : Heteroclinic Orbits In a Model of Shearing Instabilities
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

      INTEGER I,J
      DOUBLE PRECISION P11SS,T02CS,P12CS,P01CS,A01CS,MUS,Q,SIGMA,ZETA,PI2

      P11SS=U(1)
      T02CS=U(2)
      P12CS=U(3)
      P01CS=U(4)
      A01CS=U(5)

      MUS=PAR(1)
      Q=PAR(2)
      SIGMA=PAR(3)
      ZETA=PAR(4)
      PI2=3.141592654D0*3.141592654D0

      F(1)=P11SS*T02CS+P11SS*MUS-P12CS*P01CS 
      F(2)=-(P11SS**2+T02CS)
      F(3)=P12CS*MUS+P11SS*P01CS-9*SIGMA*P12CS/(4*(SIGMA+1))
      F(4)=(-Q*SIGMA*A01CS/PI2-SIGMA*P01CS+(3*P11SS*P12CS*(1+1/SIGMA)))/4
      F(5)=ZETA*(P01CS-A01CS)/4

      IF(IJAC.EQ.0)RETURN

      DFDU(1,1)=T02CS+MUS    
      DFDU(1,2)=P11SS
      DFDU(1,3)=-P01CS
      DFDU(1,4)=-P12CS
      DFDU(1,5)=0

      DFDU(2,1)=-2*P11SS
      DFDU(2,2)=-1
      DFDU(2,3:5)=0

      DFDU(3,1)=P01CS
      DFDU(3,3)=MUS-9*SIGMA/(4*(SIGMA+1))
      DFDU(3,4)=P11SS
      DFDU(3,5)=0

      DFDU(4,1)=3*P12CS*(SIGMA+1)/(4*SIGMA)
      DFDU(4,2)=0
      DFDU(4,3)=3*P11SS*(SIGMA+1)/(4*SIGMA)
      DFDU(4,4)=-SIGMA/4
      DFDU(4,5)=-Q*SIGMA/(4*PI2)

      DFDU(5,1:3)=0
      DFDU(5,4)=ZETA/4
      DFDU(5,5)=-ZETA/4

      IF(IJAC.EQ.1)RETURN 

      DFDP(1,1)=P11SS
      DFDP(1,2:4)=0

      DFDP(3,1)=P12CS
      DFDP(3,2)=0
      DFDP(3,3)=-9*P12CS/(4*(SIGMA+1)**2)
      DFDP(3,4)=0

      DFDP(4,1)=0
      DFDP(4,2)=-SIGMA*A01CS/(4*PI2)
      DFDP(4,3)=(-Q*A01CS/PI2-P01CS-3*P11SS*P12CS/SIGMA**2)/4
      DFDP(4,4)=0

      DFDP(5,1:3)=0
      DFDP(5,4)=(P01CS-A01CS)/4

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T
! 
!----------------------------------------------------------------------
! Problem parameters (only PAR(1-9) are available to the user) :

       PAR(1)=0.163875d0          ! mu
       PAR(2)=0.0d0               ! q
       PAR(3)=0.5d0               ! sigma
       PAR(4)=0.2                 ! zeta

       PAR(11)=85.07              ! Truncated time interval 

!----------------------------------------------------------------------
! If IEQUIB =-2 put initial approximations to equilibria in 
!   PAR(11+i), i=1,...,NDIM :        left-hand equilibrium
!   PAR(11+i), i=NDIM+1,...,2*NDIM   right-hand equilibrium                    

       PAR(12) = 0.4048147d0
       PAR(13) = -0.163875d0
       PAR(14) = 0.0
       PAR(15) = 0.0
       PAR(16) = 0.0

       PAR(17) = 0.0
       PAR(18) = 0.0
       PAR(19) = 0.0
       PAR(20) = 0.0
       PAR(21) = 0.0

      END SUBROUTINE STPNT

      SUBROUTINE PVLS(NDIM,U,PAR)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
! Homoclinic bifurcations COMMON block needed here :
      COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      INTEGER ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
      INTEGER I

! If IEQUIB =-1 put analytic expressions for equilibria in
!   PAR(11+i), i=1,..,NDIM         left-hand equilibrium
!   PAR(11+i), i=NDIM+1,...,2*NDIM right-hand equilibrium
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

      END SUBROUTINE PVLS

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT









