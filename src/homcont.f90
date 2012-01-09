!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        Subroutines for Homoclinic Bifurcation Analysis
!       (A. R. Champneys, Yu. A. Kuznetsov, B. Sandstede,
!        B. E. Oldeman, E. J. Doedel)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE HOMCONT

  USE AUTO_CONSTANTS, ONLY : AUTOPARAMETERS,HCONST,NPARX,NEWCFILE
  USE BVP
  USE TOOLBOXBV

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: AUTOHO,INSTRHO

! This common block is also used by demos: don't remove it!!
! Also, don't use the common variables in FNHO and ICHO because 
! the MPI code does not transfer them.

  INTEGER ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
  COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV

  INTEGER, ALLOCATABLE, TARGET :: IREV(:),IFIXED(:),IPSI(:)
  LOGICAL, SAVE :: INITCNST=.FALSE.
  DOUBLE PRECISION, SAVE :: COMPZERO

  DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

CONTAINS

! ---------- ------
  SUBROUTINE AUTOHO(AP,ICP,ICU)

    USE AUTOMPI, ONLY : MPIIAM

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(INOUT) :: ICP(:)
    INTEGER, INTENT(IN) :: ICU(:)

    ! Redefine AUTO constants for homoclinic orbits
    ! (only on the first node for MPI)
    IF(MPIIAM()==0)THEN
       CALL INHO(AP,ICP)
    ENDIF

    CALL INITBVP(AP,ICP)
    IF(ABS(AP%ISW)==2.AND.(AP%ITP==5.OR.(ABS(AP%ITP)/10)==5))THEN
       IF(AP%ITP==5) THEN 
          ! ** Fold continuation (start).
          CALL AUTOBV(AP,ICP,ICU,FNHOL,BCHOL,ICHOL,STPNHO,FNCSHO)
       ELSE
          ! ** Fold continuation (restart).
          CALL AUTOBV(AP,ICP,ICU,FNHOL,BCHOL,ICHOL,STPNHO,FNCSHO)
       ENDIF
    ELSE
       CALL AUTOBV(AP,ICP,ICU,FNHO,BCHO,ICHO,STPNHO,FNCSHO)
    ENDIF

  END SUBROUTINE AUTOHO

! ---------- ----
  SUBROUTINE FNHO(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates the equations for homoclinic bifurcation analysis

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:),FF1(:)
    INTEGER NDM,NFPR,I,J,IT,IEQMAX
    DOUBLE PRECISION UMX,EP,P,UU

    NDM=AP%NDM
    NFPR=AP%NFPR

    ! Generate the function.

    IF(NDIM==NDM)THEN
       !    *Evaluate the R.-H. sides
       CALL FFHO(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,11)
       RETURN
    ENDIF

    ALLOCATE(DFU(NDM,NDM),DFP(NDM,AP%NPAR))

    IF(AP%NPARI>0)THEN
       !    Homoclinic branch switching
       DO J=0,NDIM-NDM,NDM
          IF(IJAC==2)THEN
             DO I=1,NFPR
                DFP(:,ICP(I))=0d0
             ENDDO
          ENDIF
          IF(J==0)THEN
             IT=10
          ELSEIF(J==NDIM-NDM)THEN
             IT=11
          ELSE
             IT=19+(J/NDM)*2
          ENDIF
          CALL FFHO(AP,NDM,U(J+1),UOLD(J+1),ICP,PAR,IJAC,F(J+1),DFU,DFP,IT)
          IF(IJAC/=0)THEN
             DFDU(J+1:J+NDM,J+1:J+NDM)=DFU(:,:)
          ENDIF
          IF(IJAC==2)THEN
             DO I=1,NFPR
                DFDP(J+1:J+NDM,ICP(I))=DFP(:,ICP(I))
             ENDDO
          ENDIF
       ENDDO
       DEALLOCATE(DFU,DFP)
       RETURN
    ENDIF


    ! Generate the function + adjoint variational equation for ITWIST=1

    IF(IJAC/=0)THEN

       ! Generate dF/du for F = - (Df(u))^T U + PAR(10)*f(u)

       UMX=0.d0
       DO I=1,NDIM
          IF(DABS(U(I))>UMX)UMX=DABS(U(I))
       ENDDO

       EP=HMACH*(1+UMX)

       DO I=1,NDM
          UU=U(I)
          U(I)=UU-EP
          CALL FFHO(AP,NDIM,U,UOLD,ICP,PAR,1,DFDU(1,I),DFU,DFP,11)
          U(I)=UU+EP
          CALL FFHO(AP,NDIM,U,UOLD,ICP,PAR,1,F,DFU,DFP,11)
          U(I)=UU
          DO J=NDM+1,NDIM
             DFDU(J,I)=(F(J)-DFDU(J,I))/(2*EP)
          ENDDO
       ENDDO
    ENDIF

    IF(IJAC==2)THEN
       DO I=1,NFPR
          DFP(:,ICP(I))=0d0
       ENDDO
    ENDIF

    CALL FFHO(AP,NDIM,U,UOLD,ICP,PAR,MAX(IJAC,1),F,DFU,DFP,11)
    IF(IJAC==0)THEN
       DEALLOCATE(DFU,DFP)
       RETURN
    ENDIF

    DFDU(1:NDM,1:NDM)=DFU(:,:)
    DFDU(1:NDM,NDM+1:NDIM)=0d0
    ! *Adjoint variational equations for normal vector
    ! *Set dF/dU for F = - (Df(u))^T U + PAR(10)*f(u)
    DO J=1,NDM
       DO I=1,NDM
          DFDU(NDM+J,NDM+I)=-DFU(I,J)
       ENDDO
    ENDDO

    IF(IJAC==1)THEN
       DEALLOCATE(DFU,DFP)
       RETURN
    ENDIF

    ALLOCATE(FF1(NDIM))
    DO I=1,NFPR
       DFDP(1:NDM,ICP(I))=DFP(:,ICP(I))
    ENDDO
    IF(IEQUIB<0)THEN
       ! heteroclinic equilibrium parameters
       IEQMAX=11+NDM*2
    ELSE
       ! homoclinic equilibrium parameters
       IEQMAX=11+NDM
    ENDIF
    DO I=1,NFPR
       IF(ICP(I)==10)THEN
          DFDP(NDM+1:NDIM,10)=F(:NDM)
       ELSEIF(ICP(I)==11)THEN
          DFDP(NDM+1:NDIM,11)=(F(NDM+1:NDIM)+PAR(10)*F(1:NDM))/PAR(11)
       ENDIF
       IF(ICP(I)>=10.AND.ICP(I)<=IEQMAX)CYCLE
       P=PAR(ICP(I))
       EP=HMACH*( 1 +ABS(P) )
       PAR(ICP(I))=P+EP
       CALL FFHO(AP,NDIM,U,UOLD,ICP,PAR,1,FF1,DFU,DFP,11)
       DO J=NDM+1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(FF1,DFU,DFP)

  END SUBROUTINE FNHO

! ---------- ----
  SUBROUTINE FFHO(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,IT)

    USE INTERFACES, ONLY:FUNI,FUNC

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC,IT
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(AP%NDM,AP%NDM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDP(AP%NDM,*)

    ! Local
    INTEGER I,J,NDM
    DOUBLE PRECISION DUM1,T

    NDM=AP%NDM

    ! Generate the function.

    CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    T=PAR(IT)
    IF(AP%NPARI==0)THEN
       IF(NDIM>NDM)THEN        
          !       *Adjoint variational equations for normal vector
          !       *Set F = - (Df)^T u
          DO J=1,NDM
             DUM1=0.0D0
             DO I=1,NDM
                DUM1=DUM1+DFDU(I,J)*U(NDM+I)
             ENDDO
             F(NDM+J) = -DUM1
          ENDDO
          !       *Set F =  F + PAR(10) * f
          DO J=1,NDM
             F(NDM+J) = F(NDM+J) + PAR(10) * F(J)
          ENDDO
       ENDIF
    ENDIF

    ! Scale by truncation interval T

    IF(IJAC/=0)THEN
       DFDU(:,:)=T*DFDU(:,:)
       IF(IJAC==2)THEN
          !       **Derivative to truncation interval
          DO J=1,AP%NFPR
             IF(ICP(J)==IT)THEN
                DFDP(:,IT)=F(:)
             ELSE
                DFDP(:,ICP(J))=T*DFDP(:,ICP(J))
             ENDIF
          ENDDO
       ENDIF
    ENDIF
    F(:)=T*F(:)

  END SUBROUTINE FFHO

! ---------- ----
  SUBROUTINE BCHO(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    ! Generates the boundary conditions for homoclinic bifurcation analysis

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: UU(:),FF1(:),FF2(:)
    INTEGER NFPR,I,J
    DOUBLE PRECISION UMX,EP,P

    NFPR=AP%NFPR

    ! Generate the function.

    CALL FBHO(AP,NDIM,PAR,ICP,NBC,.TRUE.,U0,U1,F)

    IF(IJAC==0)RETURN
    ALLOCATE(UU(NDIM),FF1(NBC),FF2(NBC))

    ! Derivatives with respect to U0.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U0(I))>UMX)UMX=DABS(U0(I))
    ENDDO
    EP=HMACH*(1+UMX)
    DO I=1,NDIM
       UU(I)=U0(I)
    ENDDO
    DO I=1,NDIM
       UU(I)=U0(I)-EP
       CALL FBHO(AP,NDIM,PAR,ICP,NBC,.FALSE.,UU,U1,FF1)
       UU(I)=U0(I)+EP
       CALL FBHO(AP,NDIM,PAR,ICP,NBC,.FALSE.,UU,U1,FF2)
       UU(I)=U0(I)
       DO J=1,NBC
          DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    ! Derivatives with respect to U1.

    UMX=0.d0
    DO I=1,NDIM
       IF(DABS(U1(I))>UMX)UMX=DABS(U1(I))
    ENDDO
    EP=HMACH*(1+UMX)
    DO I=1,NDIM
       UU(I)=U1(I)
    ENDDO
    DO I=1,NDIM
       UU(I)=U1(I)-EP
       CALL FBHO(AP,NDIM,PAR,ICP,NBC,.FALSE.,U0,UU,FF1)
       UU(I)=U1(I)+EP
       CALL FBHO(AP,NDIM,PAR,ICP,NBC,.FALSE.,U0,UU,FF2)
       UU(I)=U1(I)
       DO J=1,NBC
          DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
       ENDDO
    ENDDO

    DO I=1,NFPR
       P=PAR(ICP(I))
       PAR(ICP(I))=P+EP
       CALL FBHO(AP,NDIM,PAR,ICP,NBC,.FALSE.,U0,U1,FF2)
       DO J=1,NBC
          DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
       ENDDO
       PAR(ICP(I))=P
    ENDDO

    DEALLOCATE(FF1,FF2,UU)
  END SUBROUTINE BCHO

! ---------- ----
  SUBROUTINE FBHO(AP,NDIM,PAR,ICP,NBC,CSAVE,U0,U1,FB)

    USE SUPPORT, ONLY: PI
    USE BVP, ONLY: NRTN, IRTN
    USE INTERFACES, ONLY: FUNC,BCND,PVLS
    USE SUPPORT, ONLY: AUTOSTOP

    ! Generates the boundary conditions for homoclinic orbits.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NBC
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*),U0(NDIM),U1(NDIM)
    DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
    LOGICAL, INTENT(IN) :: CSAVE
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: VR(:,:,:),VT(:,:,:)
    DOUBLE PRECISION, ALLOCATABLE :: BOUND(:,:),RR(:,:),RI(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: XEQUIB1(:),XEQUIB2(:)
    DOUBLE PRECISION, ALLOCATABLE, SAVE :: UMAX(:)

    LOGICAL INEIG
    INTEGER NDM,NPAR,NBCN,JB,I,J,K,IP,KP
    DOUBLE PRECISION DUM,DUM1(1),DUM2(1)

    NDM=AP%NDM
    NPAR=AP%NPAR
    ALLOCATE(VR(NDM,NDM,2),VT(NDM,NDM,2),BOUND(NDM,NDM))
    ALLOCATE(RR(NDM,2),RI(NDM,2),XEQUIB1(NDM),XEQUIB2(NDM))

    ! *Initialization
    DO I=1,NBC
       FB(I) = 0.0D0
    ENDDO
    JB = 1

    IF ((IEQUIB==0).OR.(IEQUIB==-1)) THEN
       CALL PVLS(NDM,U0,PAR)
    ENDIF
    !          write(9,*) 'Xequib:'
    DO I=1,NDM
       XEQUIB1(I)=PAR(11+I)
       !          write(9,*) I,XEQUIB1(I)
    ENDDO
    ! ** Rotations */
    IF(IRTN/=0)THEN
       DO I=1,NDM
          XEQUIB2(I)=XEQUIB1(I)
          IF(NRTN(I)/=0)THEN
             IF(ISTART<0)THEN
                NRTN(I)=-ISTART*NRTN(I)
             ENDIF
             XEQUIB2(I)=XEQUIB2(I)+PI(2.d0)*NRTN(I)
          ENDIF
       ENDDO
    ELSEIF(IEQUIB.GE.0) THEN
       DO I=1,NDM
          XEQUIB2(I)=PAR(11+I)
       ENDDO
    ELSE
       DO I=1,NDM
          XEQUIB2(I)=PAR(NDM+11+I)
       ENDDO
    ENDIF

    ! **Regular Continuation**
    IF(ISTART/=3) THEN
       !    *Projection boundary conditions for the homoclinic orbit
       !    *NSTAB boundary conditions at t=0
       CALL PRJCTN(AP,BOUND,CSAVE,XEQUIB1,ICP,PAR,-1,1,1,NDM)
       DO I=1,NSTAB
          DO K=1,NDM
             FB(JB)=FB(JB)+(U0(K)-XEQUIB1(K))*BOUND(K,I)
          ENDDO
          JB = JB+1
       ENDDO

       !    *NUNSTAB boundary conditions at t=1
       IF(AP%NREV==0) THEN
          CALL PRJCTN(AP,BOUND,CSAVE,XEQUIB2,ICP,PAR,1,2,1,NDM)
          DO I=1,NUNSTAB
             DO K=1,NDM
                IF (ISTART.GE.0) THEN
                   FB(JB)=FB(JB)+(U1(K)-XEQUIB2(K))*BOUND(K,I)
                ELSE
                   FB(JB)=FB(JB)+(U1(NDIM-NDM+K)-XEQUIB2(K))*BOUND(K,I)
                   IF (ITWIST==0) THEN
                      !                 allow jump at end.
                      FB(JB)=FB(JB)+PAR(22)
                   ENDIF
                ENDIF
             ENDDO
             JB = JB+1
          ENDDO
       ELSE
          !     *NUNSTAB symmetric boundary conditions at t=1 if NREV=1

          DO I=1,NDIM
             IF(IREV(I)>0) THEN
                FB(JB)=U1(I)  
                JB=JB+1
             ENDIF
          ENDDO
       ENDIF
       INEIG=.FALSE.
       !    *NFIXED extra boundary conditions for the fixed conditions
       IF (NFIXED>0) THEN
          CALL EIGHO(AP,2,RR(1,1),RI(1,1),VR(1,1,1),XEQUIB1,ICP,PAR,NDM)
          IF(IEQUIB<0) THEN
             CALL EIGHO(AP,2,RR(1,2),RI(1,2),VR(1,1,2),XEQUIB2,ICP,PAR,NDM)
          ENDIF
          DO I=1,NFIXED
             IF((IFIXED(I)>10).AND..NOT.INEIG) THEN
                CALL EIGHO(AP,1,RR(1,1),RI(1,1),VT(1,1,1),XEQUIB1,ICP,PAR,NDM)
                INEIG=.TRUE.
                IF(IEQUIB<0) THEN
                   CALL EIGHO(AP,1,RR(1,2),RI(1,2),VT(1,1,2), &
                        XEQUIB2,ICP,PAR,NDM)
                ENDIF
             ENDIF
             FB(JB)=PSIHO(NDM,IFIXED(I),RR,RI,VR,VT,ICP,PAR,U0,U1)
             JB = JB+1 
          ENDDO
       ENDIF
       !    *extra boundary condition in the case of a saddle-node homoclinic
       IF (IEQUIB==2) THEN
          IF(.NOT.INEIG) THEN
             CALL EIGHO(AP,1,RR(1,1),RI(1,1),VT(1,1,1),XEQUIB1,ICP,PAR,NDM)
             INEIG=.TRUE.
          ENDIF
          FB(JB)=RR(NSTAB+1,1)
          JB=JB+1
       ENDIF
       !    *NDM initial conditions for the equilibrium if IEQUIB=1,2,-2
       IF ((IEQUIB/=0).AND.(IEQUIB/=-1)) THEN
          CALL FUNC(NDM,XEQUIB1,ICP,PAR,0,FB(JB),DUM1,DUM2)
          JB=JB+NDM
          !    *NDM extra initial conditions for the equilibrium if IEQUIB=-2
          IF (IEQUIB==-2) THEN
             CALL FUNC(NDM,XEQUIB2,ICP,PAR,0,FB(JB),DUM1,DUM2)
             JB=JB+NDM
          ENDIF
       ENDIF
       !    *boundary conditions for normal vector
       IF ((ISTART.GE.0).AND.(ITWIST==1)) THEN
          !       *-orthogonal to the unstable directions of A  at t=0
          CALL PRJCTN(AP,BOUND,CSAVE,XEQUIB1,ICP,PAR,1,1,2,NDM)
          DO I=1,NUNSTAB
             DUM=0.0
             DO K=1,NDM
                DUM=DUM+U0(NDM+K)*BOUND(K,I)
             ENDDO
             FB(JB)=DUM 
             JB = JB+1
          ENDDO
          !       *-orthogonal to the stable directions of A  at t=1
          CALL PRJCTN(AP,BOUND,CSAVE,XEQUIB2,ICP,PAR,-1,2,2,NDM)
          DO I=1,NSTAB
             DUM=0.0
             DO K=1,NDM
                DUM=DUM+U1(NDM+K)*BOUND(K,I)
             ENDDO
             FB(JB)=DUM 
             JB = JB+1
          ENDDO
          !  Branch switching to n-homoclinic orbits.
       ELSEIF(ISTART<0) THEN
          !     More boundary conditions: continuity+gaps
          DO K=0,NDIM/NDM-2
             DO I=1,NDM
                FB(JB)=U0(NDM*(K+1)+I)-U1(NDM*K+I)
                IF (ITWIST==1) THEN 
                   ! Lin(-Sandstede): PAR(20,22,...) contain the gap sizes,
                   ! PAR(NPAR-2*NDM+1...NPAR-NDM) contains the adjoint unit
                   ! vector at the gaps.
                   FB(JB)=FB(JB)-PAR(20+2*K)*PAR(NPAR-2*NDM+I)
                ENDIF
                JB = JB+1
             ENDDO
          ENDDO
          ! Poincare sections: <x-x_0,\dot x_0>=0
          ! PAR(NPAR-NDM+1...NPAR) contains the derivatives of the
          ! point x_0 in the original
          ! homoclinic orbit that is furthest from the equilibrium.
          ! x_0=umax is initialized at each run to an end point, and so
          ! is always in the Poincare section
          IF (.NOT.ALLOCATED(UMAX)) THEN
             ALLOCATE(UMAX(NDIM))
             DO I=1,NDIM
                UMAX(I) = U1(I)
             ENDDO
          ENDIF
          DO K=0,NDIM/NDM-2
             DO I=1,NDM
                FB(JB)=FB(JB)+(U1(K*NDM+I)-UMAX(K*NDM+I))*PAR(NPAR-NDM+I)
             ENDDO
             JB = JB + 1
          ENDDO
       ENDIF
    ELSE
       ! **Starting Solutions using Homotopy**
       IP=12
       IF(IEQUIB.GE.0) THEN 
          IP=IP+NDM
       ELSE
          IP=IP+2*NDM
       ENDIF
       KP=IP
       !    *Explicit boundary conditions for homoclinic orbit at t=0
       CALL EIGHO(AP,2,RR,RI,VR,XEQUIB1,ICP,PAR,NDM)
       JB=NDM+1
       IF(NUNSTAB>1) THEN
          FB(JB)=0.0
          KP=IP+NUNSTAB
          DO J=1,NUNSTAB
             DO I=1,NDM
                FB(I)=FB(I)+U0(I)-XEQUIB1(I)-PAR(IP+J)*VR(NDM-NUNSTAB+J,I,1)
             ENDDO
             FB(JB)=FB(JB)+PAR(IP+J)**2
          ENDDO
          FB(JB)=FB(JB)-PAR(IP)**2
          JB=JB+1
       ELSE
          KP=IP+1
          DO I=1,NDM
             FB(I)=U0(I)-XEQUIB1(I)-PAR(IP)*VR(NDM-NUNSTAB+1,I,1)
          ENDDO
       ENDIF
       !    *Projection boundary conditions for the homoclinic orbit at t=1
       CALL EIGHO(AP,1,RR,RI,VT,XEQUIB2,ICP,PAR,NDM)
       DO I=NDM-NUNSTAB+1,NDM
          DUM=0.0D0
          DO J=1,NDM
             DUM=DUM+(U1(J)-XEQUIB2(J))*VT(I,J,1)
          ENDDO
          KP=KP+1
          FB(JB)=DUM-PAR(KP)
          JB=JB+1
       ENDDO
       !    *NDM initial conditions for the equilibrium if IEQUIB=1,2,-2
       IF ((IEQUIB/=0).AND.(IEQUIB/=-1)) THEN
          CALL FUNC(NDM,XEQUIB1,ICP,PAR,0,FB(JB),DUM1,DUM2)
          JB=JB+NDM
          !    *NDM extra initial conditions for the equilibrium if IEQUIB=-2
          IF (IEQUIB==-2) THEN
             CALL FUNC(NDM,XEQUIB2,ICP,PAR,0,FB(JB),DUM1,DUM2)
             JB=JB+NDM
          ENDIF
       ENDIF
    ENDIF

    NBCN=NBC-JB+1
    !  write(9,*) NBCN,NBC
    ! *user defined extra boundary conditions
    IF (NBCN>0) THEN
       CALL BCND(NDIM,PAR,ICP,NBCN,U0,U1,FB(JB),0,DUM2)
    ELSEIF (NBCN<0) THEN
       PRINT*,'Evil BUG!: Negative number of boundary conditions left'
       CALL AUTOSTOP()
    END IF

    DEALLOCATE(VR,VT,BOUND,RR,RI,XEQUIB1,XEQUIB2)
  END SUBROUTINE FBHO

! ---------- ----
  SUBROUTINE ICHO(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Generates integral conditions for homoclinic bifurcation analysis

    USE INTERFACES
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    INTEGER NDM,NPAR,I,JB
    DOUBLE PRECISION, ALLOCATABLE :: DNT(:,:)
    DOUBLE PRECISION DUM,DUM1(1)

    NDM=AP%NDM
    NPAR=AP%NPAR
    JB=0

    ! Integral phase condition for homoclinic orbit

    IF(AP%NREV==0.AND.(AP%NPARI==0.OR.NDIM==NDM)) THEN
       DUM=0.d0
       DO I=1,NDM
          DUM=DUM+UPOLD(I)*(U(I)-UOLD(I))
       ENDDO
       JB=JB+1
       F(JB)=DUM
       IF(IJAC/=0)THEN
          DINT(JB,1:NDM)=UPOLD(1:NDM)
          DINT(JB,NDM+1:NDIM+NPAR)=0.d0
       ENDIF

       ! Integral phase condition for adjoint equation     

       IF (NDIM>NDM) THEN
          DUM=0.d0
          DO I=1,NDM
             DUM=DUM+UOLD(NDM+I)*(U(NDM+I)-UOLD(NDM+I))
          ENDDO
          JB=JB+1
          F(JB)=DUM
          IF(IJAC/=0)THEN
             DINT(JB,1:NDM)=0.d0
             DINT(JB,NDM+1:2*NDM)=UOLD(NDM+1:2*NDM)
             DINT(JB,2*NDM+1:NDIM+NPAR)=0.d0
          ENDIF
       ENDIF
    ENDIF

    ! User-defined integral constraints

    IF (JB<NINT) THEN
       IF(IJAC/=0)THEN
          ALLOCATE(DNT(NINT-JB,NDM))
          CALL ICNI(AP,NDM,PAR,ICP,NINT-JB,U,UOLD,UDOT,UPOLD,F(JB),IJAC,DNT)
          DINT(JB+1:NINT,1:NDM)=DNT(1:NINT-JB,1:NDM)
          DINT(JB+1:NINT,NDM+1:NDIM)=0d0
          DINT(JB+1:NINT,NDIM+1:NDIM+NPAR)=DNT(1:NINT-JB,1:NDM+NPAR)
          DEALLOCATE(DNT)
       ELSE
          CALL ICNI(AP,NDM,PAR,ICP,NINT-JB,U,UOLD,UDOT,UPOLD,F(JB),0,DUM1)
       ENDIF
    END IF

  END SUBROUTINE ICHO

! ---------- -----
  SUBROUTINE FNHOL(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

    ! Generates starting data for the continuation of folds.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

    CALL FNBLF(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP,FNHO)

  END SUBROUTINE FNHOL

! ---------- ----
  SUBROUTINE BCHOL(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

    ! Generates the boundary conditions for continuation of folds for
    ! homoclinic bifurcation analysis

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
    DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
    DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)

    CALL BCBLF(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC,BCHO)

  END SUBROUTINE BCHOL

! ---------- -----
  SUBROUTINE ICHOL(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

    ! Generates integral conditions for folds in homoclinic bifurcation analysis

    USE INTERFACES
    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
    DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
    DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
    DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
    DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

    CALL ICBLF(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT,ICHO)

  END SUBROUTINE ICHOL

! ---------- -------
  SUBROUTINE INSTRHO(KEYSTR,VALSTR,LISTLEN,IERR)

    CHARACTER(LEN=*), INTENT(IN) :: KEYSTR, VALSTR
    INTEGER, INTENT(IN) :: LISTLEN
    INTEGER, INTENT(OUT) :: IERR
    INTEGER NDIM,I

    ! read HomCont constants from a string

    IERR = 0
    IF(.NOT.INITCNST)THEN
       NUNSTAB=-1
       NSTAB=-1
       IEQUIB=1
       ITWIST=0
       ISTART=5 ! default is 1 or 4 depending on IPS from solution
       NREV=0
       NFIXED=0
       NPSI=0
       ALLOCATE(IREV(0),IFIXED(0),IPSI(0))
    ENDIF
    INITCNST = .TRUE.
    SELECT CASE(KEYSTR)
    CASE('NUNSTAB')
       READ(VALSTR,*,ERR=3)NUNSTAB
    CASE('NSTAB')
       READ(VALSTR,*,ERR=3)NSTAB
    CASE('IEQUIB')
       READ(VALSTR,*,ERR=3)IEQUIB
    CASE('ISTART')
       READ(VALSTR,*,ERR=3)ISTART
    CASE('ITWIST')
       READ(VALSTR,*,ERR=3)ITWIST
    CASE('IREV')
       NDIM=LISTLEN
       IF(ALLOCATED(IREV))DEALLOCATE(IREV)
       ALLOCATE(IREV(NDIM))
       READ(VALSTR,*,ERR=3)(IREV(I),I=1,NDIM)
       NREV=1
    CASE('IFIXED')
       NFIXED=LISTLEN
       IF(ALLOCATED(IFIXED))DEALLOCATE(IFIXED)
       ALLOCATE(IFIXED(NFIXED))
       READ(VALSTR,*,ERR=3)(IFIXED(I),I=1,NFIXED)
    CASE('IPSI')
       NPSI=LISTLEN
       IF(ALLOCATED(IPSI))DEALLOCATE(IPSI)
       ALLOCATE(IPSI(NPSI))
       READ(VALSTR,*,ERR=3)(IPSI(I),I=1,NPSI)
    CASE DEFAULT
       IERR = 1
    END SELECT
    RETURN
3   IERR = 3
  END SUBROUTINE INSTRHO

! ---------- ----
  SUBROUTINE INHO(AP,ICP)

    USE IO, ONLY: GETIPS3
    USE SUPPORT, ONLY: AUTOSTOP

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    DOUBLE PRECISION, PARAMETER :: HMACHHO=1.0d-13
    INTEGER, INTENT(INOUT) :: ICP(*)

    INTEGER NDIM,ISW,NBC,NINT,NDM,stat,I,ICORR,LINE,NBCPROJ,NFREE
    INTEGER NPARI,NPAR,IPS3,IRS

    ! Reads from fort.11 specific constants for homoclinic continuation.
    ! Sets up re-defined constants in AP. 
    ! Sets other constants in the module common blocks.

    ! set various constants 

    NDIM=AP%NDIM
    IRS=AP%IRS
    ISW=AP%ISW
    NBC=AP%NBC
    NINT=AP%NINT
    NDM=NDIM
    COMPZERO=HMACHHO

    OPEN(UNIT=12,FILE='fort.12',STATUS='OLD',ACCESS='sequential',IOSTAT=stat)
    IF(STAT/=0)THEN
       IF(.NOT.NEWCFILE)THEN
          WRITE(6,"(A)") &
           " Error: Using HomCont without fort.12, h., or new-style c. file."
          CALL AUTOSTOP()
       ENDIF
    ELSE
       LINE=1
       READ(12,*,ERR=1,END=2)NUNSTAB,NSTAB,IEQUIB,ITWIST,ISTART

       ! updated reading in of constants for reversible equations
       ! replaces location in datafile of compzero

       IF(ALLOCATED(IREV))DEALLOCATE(IREV)
       LINE=LINE+1
       READ(12,*,ERR=1,END=2)NREV
       IF(NREV>0)THEN
          ALLOCATE(IREV(NDM))
          LINE=LINE+1
          READ(12,*,ERR=1,END=2)(IREV(I),I=1,NDM)
       ELSE
          ALLOCATE(IREV(0))
       ENDIF

       LINE=LINE+1
       READ(12,*,ERR=1,END=2)NFIXED
       IF(ALLOCATED(IFIXED))DEALLOCATE(IFIXED)
       ALLOCATE(IFIXED(NFIXED))
       IF (NFIXED>0)THEN
          LINE=LINE+1
          READ(12,*,ERR=1,END=2)(IFIXED(I),I=1,NFIXED)
       ENDIF
       LINE=LINE+1
       READ(12,*,ERR=1,END=2)NPSI
       IF(ALLOCATED(IPSI))DEALLOCATE(IPSI)
       ALLOCATE(IPSI(NPSI))
       IF (NPSI>0)THEN
          LINE=LINE+1
          READ(12,*,ERR=1,END=2)(IPSI(I),I=1,NPSI)
       ENDIF
       CLOSE(UNIT=12,STATUS='KEEP')
    ENDIF
    NFREE=2+NFIXED-NREV+NINT+NBC
    IF (ISTART<0) THEN
       !    n-homoclinic branch switching
       NFREE=NFREE-ISTART-1
       NDIM=NDM*(-ISTART+1)
       ! make sure there is enough room for the Lin vector etc.
       NPARI=2*NDM
       AP%NPARI=NPARI

       ! Free parameter (artificial parameter for psi)
       ! nondegeneracy parameter of the adjoint

    ELSE
       IF (NPSI>0) THEN
          NPAR=AP%NPAR
          NPAR=MAX(20+MAXVAL(IPSI),NPAR)
          AP%NPAR=NPAR
       ENDIF
       IF (ITWIST==1) THEN
          NFREE = NFREE + 1
          ICP(NFREE) = 10
          NDIM=NDM*2
       ENDIF
    ENDIF

    ! Extra free parameters for equilibrium if iequib=1,2,-2

    IF ((IEQUIB/=0).AND.(IEQUIB/=-1)) THEN
       DO I=1,NDM
          ICP(NFREE+I)=11+I
       ENDDO
    ENDIF

    IF (IEQUIB==-2) THEN
       DO I=1,NDM
          ICP(NFREE+NDM+I)=11+NDM+I
       ENDDO
    ENDIF

    HCONST%NUNSTAB=NUNSTAB
    HCONST%NSTAB=NSTAB
    HCONST%IEQUIB=IEQUIB
    HCONST%ITWIST=ITWIST
    HCONST%ISTART=ISTART
    HCONST%IREV=>IREV
    HCONST%IFIXED=>IFIXED
    HCONST%IPSI=>IPSI
    IF (ISTART==5) THEN
       ISTART=1
       IF(IRS/=0)THEN
          IPS3=GETIPS3()
          IF(IPS3/=9.AND.IPS3/=0)THEN
             ISTART=4
          ENDIF
       ENDIF
    ENDIF
    IF(NSTAB==-1.OR.NUNSTAB==-1)THEN
       IF (IEQUIB==2) THEN
          NBCPROJ=NDM-1
       ELSE
          NBCPROJ=NDM
       ENDIF
    ELSE
       NBCPROJ=NSTAB+NUNSTAB
    ENDIF
    IF(NSTAB==-1.AND.NUNSTAB/=-1)THEN
       NSTAB=NBCPROJ-NUNSTAB
    ELSEIF(NUNSTAB==-1.AND.NSTAB/=-1)THEN
       NUNSTAB=NBCPROJ-NSTAB
    ENDIF

    IF (ISTART/=3) THEN
       ! *regular continuation
       IF (ISTART.GE.0) THEN
          NINT=NINT+ITWIST+1-NREV
       ENDIF
       IF (ISW==2.AND.AP%ITP/=5.AND.ABS(AP%ITP)/10/=5)THEN
          ICORR = 2
       ELSE
          ICORR = 1
       ENDIF
       NBC=NBC+NBCPROJ+NDIM-NDM+IEQUIB*NDM+NFREE-NINT-ICORR
       IF (IEQUIB==2) THEN
          NBC=NBC-NDM+1
       ENDIF
       IF (IEQUIB<0) THEN
          NBC=NBC-(3*IEQUIB+2)*NDM
       ENDIF
    ELSE
       ! *starting solutions using homotopy
       IF (ABS(NUNSTAB)==1) THEN
          NBC=NDM*(1+IEQUIB)+1
       ELSE
          NBC=NDM*(1+IEQUIB)+NUNSTAB+1
       ENDIF
       IF (IEQUIB==2.AND.AP%IID>0) THEN 
          WRITE(9,*)'WARNING: IEQUIB=2 NOT ALLOWED WITH ISTART=3'
       ENDIF
       IF (IEQUIB<0) THEN
          NBC=NBC-NDM*(3*IEQUIB+2)
       ENDIF
       NINT=0
    ENDIF

    ! write new constants into AP

    AP%NDIM=NDIM
    AP%NBC=NBC
    AP%NINT=NINT
    AP%NDM=NDM
    AP%NREV=NREV

    RETURN

1   WRITE(6,"(A,I2,A)") &
         " Error in fort.12 or h. file: bad integer on line ",LINE,"."
    GOTO 3
2   WRITE(6,"(A,I2,A)") &
         " Error in fort.12 or h. file: ends prematurely on line ",LINE,"."
3   CLOSE(UNIT=12,STATUS='KEEP')
    CALL AUTOSTOP()

  END SUBROUTINE INHO

! ---------- ------
  SUBROUTINE INTPHO(NDM,NCOLRS,TM,DTM,UPS,UDOTPS,T,DT,N,NDIM,J,J1)

    USE MESH, ONLY: INTWTS
    INTEGER, INTENT(IN) :: NDM,NCOLRS,N,NDIM,J,J1
    DOUBLE PRECISION, INTENT(IN) :: TM,DTM,T,DT
    DOUBLE PRECISION, INTENT(INOUT) :: UPS(NDIM,0:*),UDOTPS(NDIM,0:*)

    ! Local

    INTEGER I,L,JJ,K
    DOUBLE PRECISION D,Z,X(0:NCOLRS),W(0:NCOLRS)

    ! Finds interpolant (TM(.) , UPS(.), UDOTPS(.) ) on the new mesh
    ! at times TM,TM+DTM using the old mesh at times T,T+DT.

    ! Used by TRANHO to initiate branch switching to n-homoclinic orbits.

    D=DTM/NCOLRS
    DO L=0,NCOLRS
       X(L)=TM+L*D
    ENDDO
    DO I=0,NCOLRS-1
       Z=T+DT*I/NCOLRS
       CALL INTWTS(NCOLRS,Z,X,W)
       JJ=J1+I
       DO K=1,NDM
          UPS(N+K,JJ)=DOT_PRODUCT(W(:),UPS(N+K,J:J+NCOLRS))
          UDOTPS(N+K,JJ)=DOT_PRODUCT(W(:),UDOTPS(N+K,J:J+NCOLRS))
       ENDDO
    ENDDO

  END SUBROUTINE INTPHO

! ---------- ------
  SUBROUTINE TRANHO(NTSR,NCOLRS,NDM,NDIM,TM,UPS,UDOTPS,PAR,ICP,NPAR)

    ! Transform the data representation of the homoclinic orbit into
    ! an object suitable for homoclinic branch switching:

    ! dim|1...............NDM|NDM+1......NDIM-NDM|NDIM-NDM+1......NDIM|
    !    |                   |                   |                    |
    ! t=0|start of hom. orbit|maximum from equil.| maximum from equil.|
    !    |       :           |       :           |       :            |
    !    |       :           |end of hom. orbit  |       :            |
    !    |       :           |start of hom. orbit|       :            |
    !    |       :           |        :          |       :            |
    ! t=1|maximum from equil.|maximum from equil.| end of hom. orbit  |

    ! Called by PREHO

    USE BVP, ONLY: NRTN,IRTN
    USE SUPPORT, ONLY: PI
    USE INTERFACES, ONLY: FUNC
    INTEGER, INTENT(IN) :: NCOLRS,NDM,NDIM,ICP(*),NPAR
    INTEGER, INTENT(INOUT) :: NTSR
    DOUBLE PRECISION, INTENT(INOUT) :: TM(0:*), UPS(NDIM,0:*), &
         UDOTPS(NDIM,0:*), PAR(*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: TTM(:)
    INTEGER J2(3),I2L(1),I,I2,J,JJ,JMAX,K,K2,L,LL,LLL,IADDPH,NTNC
    DOUBLE PRECISION A(3),B(3),T(3),TT(3),DUM1(1),DUM2(1)
    DOUBLE PRECISION D1,DNORM,DTM,P,PHDIFF,TMMAX,UPSI,UPSMAX

    ALLOCATE(TTM(0:NTSR*2-1))

    ! First find maximum from the equilibrium

    UPSMAX=0
    JMAX=0
    DO J=0,NTSR
       UPSI=0
       DO I=1,NDM
          UPSI=UPSI+(UPS(I,J*NCOLRS)-PAR(11+I))**2
       ENDDO
       IF (UPSI>UPSMAX) THEN
          UPSMAX=UPSI
          JMAX=J
       ENDIF
    ENDDO
    IF(IRTN/=0)THEN
       ! Just use the point in the middle
       UPSMAX = 0
       DO I=1,NDM
          IF(NRTN(I)/=0)EXIT
       ENDDO
       DO J=0,NTSR
          D1=ABS(UPS(I,J*NCOLRS)-PAR(I+11))
          UPSI=ABS(UPS(I,J*NCOLRS)-(PAR(I+11)+PI(2.d0)*NRTN(I)))
          IF(D1<UPSI)UPSI=D1
          IF(UPSI>UPSMAX)THEN
             UPSMAX=UPSI
             JMAX=J
          ENDIF
       ENDDO
    ENDIF
    TMMAX=TM(JMAX)
    CALL FUNC(NDM,UPS(1,JMAX*NCOLRS),ICP,PAR,0,PAR(NPAR-NDM+1),DUM1,DUM2)

    ! PAR(NPAR-NDM+1...NPAR) contains the point furthest from
    ! the equilibrium.
    ! PAR(10)=the time for the unstable manifold tail.
    ! PAR(11)=the time for the stable manifold tail.
    ! PAR(20,22,...) contain the gap sizes.
    ! PAR(21,23,...) contain the times between Poincare sections

    PAR(10)=PAR(11)*TMMAX
    PAR(20)=0D0
    DO K=1,NDIM/NDM-2
       PAR(19+2*K)=PAR(11)
       PAR(20+2*K)=0D0
    ENDDO
    PAR(11)=PAR(11)*(1D0-TMMAX)

    ! Remember adjoint at maximum for applying Lin's method
    ! PAR(NPAR-2*NDM+1...NPAR-NDM) will contain the adjoint unit
    ! vector at the gaps.

    IF (ITWIST==1) THEN
       DNORM=0.D0
       DO I=1,NDM
          PAR(NPAR-2*NDM+I)=UPS(NDM+I,JMAX*NCOLRS)
          DNORM=DNORM+UPS(NDM+I,JMAX*NCOLRS)**2
       ENDDO
       DNORM=SQRT(DNORM)
       DO I=1,NDM
          PAR(NPAR-2*NDM+I)=PAR(NPAR-2*NDM+I)/DNORM
       ENDDO
    ENDIF

    ! Prepare the new NDIM*NCOLRS dimensional UPS matrix
    ! Move everything to the end in "middle part format"
    ! so that we can subsequently overwrite the beginning.

    PHDIFF=0
    IADDPH=1
    DO L=2*NTSR-1,NTSR-1,-1
       J=L-(2*NTSR-1)+JMAX
       ! move J=0...JMAX to
       !  L=2*NTSR-1-JMAX...2*NTSR-1
       ! move J=JMAX...NTSR-1 to
       !  L=NTSR-1...2*NTSR-2-JMAX
       IF (J<0) THEN
          J=J+NTSR
          IADDPH=0
       ENDIF
       TTM(L)=TM(J)-TMMAX
       IF (TTM(L)<0) TTM(L)=TTM(L)+1D0
       DO K=0,NCOLRS-1
          JJ=J*NCOLRS+K
          LL=L*NCOLRS+K
          DO I=1,NDM
             IF(IRTN/=0)THEN
                PHDIFF=0
                IF(IADDPH/=0)PHDIFF=PI(2.d0)*NRTN(MOD(I-1,NDM)+1)
             ENDIF
             UPS(I+NDM,LL)=UPS(I,JJ)+PHDIFF
             UDOTPS(I+NDM,LL)=UDOTPS(I,JJ)+PHDIFF
             UPS(I,LL)=UPS(I,JJ)
             UDOTPS(I,LL)=UDOTPS(I,JJ)
             IF (L<2*NTSR-JMAX) THEN
                ! move J=JMAX...NTSR to
                !  L+JMAX=NTSR-1+JMAX...2*NTSR-1
                IF(IRTN/=0)THEN
                   PHDIFF=PI(2.d0)*NRTN(MOD(I-1,NDM)+1)*(-ISTART-1)
                ENDIF
                LLL=LL+JMAX*NCOLRS
                IF(L==2*NTSR-JMAX-1)THEN
                   UPS(I+NDIM-NDM,LLL)=UPS(I,NTSR*NCOLRS)+PHDIFF
                   UDOTPS(I+NDIM-NDM,LLL)=UDOTPS(I,NTSR*NCOLRS)+PHDIFF
                ELSE
                   UPS(I+NDIM-NDM,LLL)=UPS(I,JJ)+PHDIFF
                   UDOTPS(I+NDIM-NDM,LLL)=UDOTPS(I,JJ)+PHDIFF
                ENDIF
             ENDIF
          ENDDO
       ENDDO
    ENDDO
    TTM(2*NTSR-1)=1D0

    ! create matching mesh
    ! merge TM(1..JMAX)/TMMAX, TM(JMAX..NTSR)-TMMAX,
    !       TM(1..JMAX)+1D0-TMMAX, 
    !       (TM(JMAX..NTSR)-TMMAX)/(1D0-TMMAX)

    J2(1) = 2*NTSR-JMAX
    J2(2) = NTSR
    J2(3) = NTSR
    A(1) = TMMAX-1D0
    A(2) = 0D0
    A(3) = 0D0
    B(1) = TMMAX
    B(2) = 1D0
    B(3) = 1D0-TMMAX
    NTSR=NTSR*2-2
    DO I=1,3
       T(I) = (TTM(J2(I))+A(I))/B(I)
       TT(I) = (TTM(J2(I)-1)+A(I))/B(I)
    ENDDO
    DO J=0,NTSR-1
       I2L=MINLOC(T)
       I2=I2L(1)
       TM(J+1)=T(I2)
       JJ=J*NCOLRS
       DTM=TM(J+1)-TM(J)

       ! Replace UPS and UDOTPS by its interpolant on the new mesh :

       CALL INTPHO(NDM,NCOLRS,TT(1),T(1)-TT(1),UPS,UDOTPS, &
            TM(J),DTM,0,NDIM,(J2(1)-1)*NCOLRS,JJ)

       ! Remesh middle part :

       CALL INTPHO(NDM,NCOLRS,TT(2),T(2)-TT(2),UPS,UDOTPS, &
            TM(J),DTM,NDM,NDIM,(J2(2)-1)*NCOLRS,JJ)

       ! Remesh last part :

       CALL INTPHO(NDM,NCOLRS,TT(3),T(3)-TT(3),UPS,UDOTPS, &
            TM(J),DTM,NDIM-NDM,NDIM,(J2(3)+JMAX-1)*NCOLRS,JJ)

       ! Copy middle parts, this applies only for 1->n switching
       ! where n>=3 and NDIM=(n+1)*NDM: (NDIM/NDM)-3 times.

       DO K2=NDM,NDIM-3*NDM,NDM
          DO K=0,NCOLRS-1
             DO I=NDM+1,2*NDM
                IF(IRTN/=0)THEN
                   PHDIFF=PI(2.d0)*NRTN(MOD(I-1,NDM)+1)*(K2/NDM)
                ENDIF
                UPS(I+K2,JJ+K)=UPS(I,JJ+K)+PHDIFF
                UDOTPS(I+K2,JJ+K)=UDOTPS(I,JJ+K)+PHDIFF
             ENDDO
          ENDDO
       ENDDO
       J2(I2)=J2(I2)+1
       TT(I2)=T(I2)
       IF(J<NTSR-1)THEN
          T(I2)=(TTM(J2(I2))+A(I2))/B(I2)
       ENDIF
    ENDDO

    ! Adjust end points

    NTNC=NTSR*NCOLRS
    DO I=1,NDM
       IF(IRTN/=0)PHDIFF=PI(2.d0)*NRTN(I)
       DO K2=I,NDIM-NDM,NDM
          P=PHDIFF*((K2-I)/NDM-1)
          UPS(K2,NTNC)=UPS(I+NDM,NTNC+NCOLRS)+P
          UDOTPS(K2,NTNC)=UDOTPS(I+NDM,NTNC+NCOLRS)+P
       ENDDO
       UPS(I+NDIM-NDM,NTNC)=UPS(I+NDIM-NDM,NTNC+NCOLRS)
       UDOTPS(I+NDIM-NDM,NTNC)=UDOTPS(I+NDIM-NDM,NTNC+NCOLRS)
    ENDDO

    ! Rotations: NRTN needs adjustment

    IF(IRTN/=0)THEN
       NRTN(1:NDM)=NRTN(1:NDM)*(-ISTART)
    ENDIF
    DEALLOCATE(TTM)
  END SUBROUTINE TRANHO

! ---------- ------
  SUBROUTINE CPBKHO(NTSR,NCOLRS,NAR,NDM,TM,UPS,UDOTPS,PAR)

    INTEGER, INTENT(IN) :: NCOLRS,NDM
    INTEGER, INTENT(INOUT) :: NTSR,NAR
    DOUBLE PRECISION, INTENT(INOUT) :: UPS(NAR,0:*), UDOTPS(NAR,0:*)
    DOUBLE PRECISION, INTENT(INOUT) :: TM(0:*), PAR(*)

    INTEGER NCOPY,I,J,K,M
    DOUBLE PRECISION TIME,TBASE

    ! Copy the homoclinic orbit back from the special representation 
    ! gotten from TRANHO to the usual representation.
    ! This is called from PREHO in order to perform normal continuation
    ! again once the branch switching is complete.

    NCOPY=NAR/NDM
    TIME=PAR(10)+PAR(11)
    DO K=1,NCOPY-2
       TIME=TIME+PAR(19+2*K)
    ENDDO
    TBASE=TIME-PAR(11)
    TM(NTSR*NCOPY)=1.0D0

    ! first init last point; otherwise it's overwritten

    DO K=1,NDM
       UPS(K,NTSR*NCOPY*NCOLRS)=UPS(K+(NCOPY-1)*NDM,NTSR*NCOLRS)
       UDOTPS(K,NTSR*NCOPY*NCOLRS)=UDOTPS(K+(NCOPY-1)*NDM,NTSR*NCOLRS)
    ENDDO
    DO K=NCOPY-1,0,-1
       DO J=NCOLRS*NTSR-1,0,-1
          I=J+NTSR*NCOLRS*K
          DO M=1,NDM
             UPS(M,I)=UPS(K*NDM+M,J)
             UDOTPS(M,I)=UDOTPS(K*NDM+M,J)
          ENDDO
       ENDDO
    ENDDO
    DO J=NTSR,1,-1
       TM(J+NTSR*(NCOPY-1))=(TBASE+TM(J)*PAR(11))/TIME
    ENDDO
    DO K=NCOPY-2,1,-1
       TBASE=TBASE-PAR(19+2*K)
       DO J=NTSR,1,-1
          TM(J+NTSR*K)=(TBASE+TM(J)*PAR(19+K*2))/TIME
       ENDDO
    ENDDO
    DO J=1,NTSR
       TM(J)=TM(J)*PAR(10)/TIME
    ENDDO
    NTSR=NTSR*NCOPY

    PAR(10)=0.D0
    PAR(11)=TIME
    NAR=NDM
  END SUBROUTINE CPBKHO

! ---------- -----
  SUBROUTINE PREHO(AP,PAR,ICP,NTSR,NAR,NCOLRS,UPS,UDOTPS,TM)

    ! Special homoclinic orbit preprocessing.

    USE BVP, ONLY: IRTN,NRTN,SETRTN
    USE SUPPORT, ONLY: PI
    USE INTERFACES, ONLY: FUNC

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NCOLRS
    INTEGER, INTENT(INOUT) :: NTSR,NAR
    DOUBLE PRECISION, INTENT(INOUT) :: UPS(AP%NDIM,0:NTSR*NCOLRS), &
         TM(0:NTSR),UDOTPS(AP%NDIM,0:NTSR*NCOLRS),PAR(*)

    ! Local

    DOUBLE PRECISION, ALLOCATABLE :: F(:)
    INTEGER NDIM,NDM,NPAR,II,IT,IST,I,J,J1,J2,JMIN,JR
    DOUBLE PRECISION T,TMMIN,UPSI,UPSMIN,DUM1(1),DUM2(1)

    NDIM=AP%NDIM
    NDM=AP%NDM
    NPAR=AP%NPAR

    IF (ISTART.GE.0.AND.NAR>2*NDM) THEN
       !    Use the usual representation again for normal continuation.
       CALL CPBKHO(NTSR,NCOLRS,NAR,NDM,TM,UPS,UDOTPS,PAR)
    ENDIF
    ! Look for rotations
    IF(IEQUIB.GE.0)THEN
       CALL SETRTN(NDM,NTSR*NCOLRS,NDIM,UPS)
    ENDIF
    IF (ISTART<0 .AND. .NOT.(NAR<NDIM .AND. NAR<3*NDM)) THEN
       !    Adjust rotations
       IF(IRTN==0)ALLOCATE(NRTN(NDM))
       IRTN=0
       DO I=1,NDM
          NRTN(I)=NINT( (UPS(NAR-NDM+I,NTSR*NCOLRS)-UPS(I,0)) / &
               (PI(2.d0) * (-ISTART)) )
          IF(NRTN(I)/=0)THEN
             IRTN=1
          ENDIF
       ENDDO
       IF(IRTN==0)DEALLOCATE(NRTN)
    ENDIF

    ! Shift phase if necessary if continuing from
    ! a periodic orbit into a homoclinic one

    IF (ISTART==4) THEN

       ! Try to find an approximate value for the equilibrium if it's not
       ! explicitely given. This is just the point where the speed is minimal.
       ! We hope that Newton's method will do the rest.

       IF (IEQUIB>0) THEN
          ALLOCATE(F(NDM))
          UPSMIN=HUGE(1.d0)
          JMIN=0
          DO J=0,NTSR*NCOLRS
             CALL FUNC(NDM,UPS(1,J),ICP,PAR,0,F,DUM1,DUM2)
             UPSI=0
             DO I=1,NDM
                UPSI=UPSI+F(I)*F(I)
             ENDDO
             IF (UPSI<UPSMIN) THEN
                JMIN = J
                UPSMIN = UPSI
             ENDIF
          ENDDO
          PAR(12:12+NDM-1)=UPS(:,JMIN)
          DEALLOCATE(F)
       ENDIF

       ! Find smallest value in norm

       UPSMIN=HUGE(1.d0)
       JMIN=0
       DO J=0,NTSR
          UPSI=0
          DO I=1,NDM
             UPSI=UPSI+(UPS(I,J*NCOLRS)-PAR(11+I))**2
          ENDDO
          IF (UPSI<UPSMIN) THEN
             UPSMIN=UPSI
             JMIN=J
          ENDIF
       ENDDO
       IF(UPSMIN<COMPZERO)THEN

          !  try to get time central value if all points within a range
          !  are within an epsilon neighbourhood of the equilibrium

          T=0
          J2=JMIN
          J1=J2
          J=JMIN-1
          DO WHILE(J/=JMIN+1)
             IF(J==-1)J=NTSR
             UPSI=0
             DO I=1,NDM
                UPSI=UPSI+(UPS(I,J*NCOLRS)-PAR(I+11))**2
             ENDDO
             IF(UPSI>COMPZERO)THEN
                J1=J+1
                IF(J1==NTSR+1)J1=0
                EXIT
             ENDIF
             J=J-1
          ENDDO
          J=JMIN+1
          DO WHILE(J/=J1)
             IF(J==NTSR+1)J=0
             UPSI=0
             DO I=1,NDM
                UPSI=UPSI+(UPS(I,J*NCOLRS)-PAR(I+11))**2
             ENDDO
             IF(UPSI>COMPZERO)THEN
                J2=J-1
                IF(J2==-1)J2=NTSR
                EXIT
             ENDIF
             J=J+1
          ENDDO
          T=(TM(J2)+TM(J1))/2
          IF(J1>J2)THEN
             T=(TM(J2)+TM(J1)+1)/2
             IF(T>=1)T=T-1
             IF(TM(J1)<=T)THEN
                J2=NTSR
             ELSE
                J1=0
             ENDIF
          ENDIF
          DO WHILE((TM(J1)<=T).AND.(J1<J2))
             J1=J1+1
          ENDDO
          JMIN=J1
          IF(JMIN==0)THEN
             IF(T-TM(NTSR)<TM(0)-T) JMIN=NTSR
          ELSE
             IF(T-TM(JMIN-1)<TM(JMIN)-T) JMIN=JMIN-1
          ENDIF
       ENDIF
       TMMIN=TM(JMIN)

       ! And then do the actual shift

       IF (JMIN/=0) THEN
          IST=-1
          J=NTSR*NCOLRS
          DO II=0,NTSR*NCOLRS-1
             IF (J==NTSR*NCOLRS) THEN
                IST=IST+1
                IF(MOD(IST,NCOLRS)==0)THEN
                   TM(NTSR)=TM(IST/NCOLRS)
                ENDIF
                UPS(:,J)=UPS(:,IST)
                UDOTPS(:,J)=UDOTPS(:,IST)
                J=IST
             ENDIF
             I=J
             J=J+JMIN*NCOLRS
             IF (J>=NTSR*NCOLRS) J=J-NTSR*NCOLRS
             IF (J==IST) J=NTSR*NCOLRS
             IF(MOD(I,NCOLRS)==0)THEN
                IT=I/NCOLRS
                TM(IT)=TM(J/NCOLRS)-TMMIN
                IF (TM(IT)<0) TM(IT)=TM(IT)+1.0D0
             ENDIF
             UPS(:,I)=UPS(:,J)
             UDOTPS(:,I)=UDOTPS(:,J)
          ENDDO

          ! Last equal to first

          TM(NTSR)=1.0D0
          UPS(:,NCOLRS*NTSR)=UPS(:,0)
          UDOTPS(:,NCOLRS*NTSR)=UDOTPS(:,0)

          ! Rotations

          IF(IRTN/=0)THEN
             JR=-1
             ntsrloop: DO J=0,NTSR*NCOLRS,NCOLRS
                DO I=1,NDM
                   IF(NRTN(I)/=0) THEN
                      IF(ABS((UPS(I,J+NCOLRS)-UPS(I,J))/NRTN(I))>ABS(PI(1d0)))&
                           THEN
                         JR=J+NCOLRS
                         EXIT ntsrloop
                      ENDIF
                   ENDIF
                ENDDO
             ENDDO ntsrloop
             IF(JR/=-1)THEN
                DO I=1,NDIM
                   IF (NRTN(I)/=0) THEN
                      DO J=JR,NTSR*NCOLRS
                         UPS(I,J)=UPS(I,J)+PI(2.d0)*NRTN(I)
                      ENDDO
                   ENDIF
                ENDDO
             ENDIF
          ENDIF

       ENDIF
    ENDIF

    ! If ISTART<0 we perform homoclinic branch switching and need
    ! to change the representation of the homoclinic orbit in UPS and
    ! UDOTPS.

    IF (ISTART<0 .AND. NAR<NDIM .AND. NAR<3*NDM) THEN
       CALL TRANHO(NTSR,NCOLRS,NDM,NDIM,TM,UPS,UDOTPS,PAR,ICP,NPAR)
    ELSEIF (ISTART<0 .AND. NAR<NDIM .AND. NAR.GE.3*NDM) THEN
       ! Copy forelast part
       DO J=0,NTSR*NCOLRS
          DO I=NDIM,NAR-NDM+1,-1
             UPS(I,J)=UPS(I-NDIM+NAR,J)
             UDOTPS(I,J)=UDOTPS(I-NDIM+NAR,J)
          ENDDO
       ENDDO
       DO I=1,(NDIM-NAR)/NDM
          PAR(16+2*(NAR/NDM+I))=PAR(16+2*NAR/NDM)
          PAR(15+2*(NAR/NDM+I))=PAR(15+2*NAR/NDM)
       ENDDO
       PAR(16+2*NAR/NDM)=(UPS(NAR-NDM+1,1)- &
            UPS(NAR-2*NDM+1,NTSR+1))/ PAR(NPAR-2*NDM+1)
    ENDIF

    ! Preprocesses (perturbs) restart data to enable 
    ! initial computation of the adjoint variable

    IF (NAR/=NDIM .AND. ISTART.GE.0 .AND. ITWIST==1) THEN
       PAR(10)= 0.0D0
       UPS(NAR+1:NDIM,:)=0.1d0
    ENDIF

  END SUBROUTINE PREHO

! ---------- ------
  SUBROUTINE STPNHO(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

    USE TOOLBOXBV, ONLY: STPNBV,STPNBV1,STPNBL
    USE BVP, ONLY: SETRTN
    USE MESH, ONLY: ADAPT2
    USE IO, ONLY: GETNDIM3
    USE INTERFACES, ONLY: PVLS

    ! Generates a starting point for the continuation of a branch of
    ! of solutions to general boundary value problems by calling the user
    ! supplied subroutine STPNT where an analytical solution is given.


    ! Generates a starting point for homoclinic continuation
    ! If ISTART=2 it calls STPNBV (STPNUB).
    ! If ISTART=3 it sets up the homotopy method.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
    INTEGER, INTENT(OUT) :: NODIR
    DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR), &
         UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: RR(:),RI(:),VR(:,:),VT(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
    INTEGER NDIM,IRS,NTST,NCOL,NDM,NDIM3,NDIMRD,NDIMU,NTSTCU
    INTEGER IP,KP,I,J,K
    DOUBLE PRECISION T,P

    NDIM=AP%NDIM
    IRS=AP%IRS
    NTST=AP%NTST
    NCOL=AP%NCOL
    NDM=AP%NDM

    IF(AP%ITP==5.AND.ABS(AP%ISW)==2) THEN 
       ! ** Fold continuation (start).
       CALL STPNBL(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)
       IF(IEQUIB.GE.0)THEN
          CALL SETRTN(NDM,NTSR*NCOLRS,NDIM,UPS)
       ENDIF
    ELSEIF(IRS>0)THEN

       ! Special case : Preprocess restart data in case of homoclinic
       ! continuation

       NTSTCU=2*NTSR*NCOLRS
       NDIMU=NDIM
       ! Autodetect special case when homoclinic branch switching is
       ! completed and the orbit's representation has to be
       ! changed.
       NDIM3=GETNDIM3()
       IF(NDIM3>(NDM*2).AND.NDIM3>NDIM)THEN
          NTSTCU=NTSR*(NDIM3/NDM)*NCOLRS
          NDIMU=NDIM3
       ENDIF
       ALLOCATE(UPSR(NDIMU,0:NTSTCU),UDOTPSR(NDIMU,0:NTSTCU),TMR(0:NTSTCU))
       CALL STPNBV1(AP,PAR,ICP,NDIMU,NTSR,NDIMRD,NCOLRS,RLDOT, &
            UPSR,UDOTPSR,TMR,NODIR)
       CALL PREHO(AP,PAR,ICP,NTSR,NDIMRD,NCOLRS,UPSR,UDOTPSR,TMR)
       CALL ADAPT2(NTSR,NCOLRS,NDIMU,NTST,NCOL,NDIM, &
            TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
       DEALLOCATE(TMR,UPSR,UDOTPSR)
    ELSE

       ! Generate the (initially uniform) mesh.

       CALL STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,UPS,UDOTPS,TM,NODIR)

       ! Initialize solution and additional parameters

       IF(IEQUIB.GE.0)THEN
          CALL SETRTN(NDM,NTSR*NCOLRS,NDIM,UPS)
       ENDIF
    ENDIF

    ALLOCATE(RR(NDM),RI(NDM),VR(NDM,NDM),VT(NDM,NDM))
    CALL PVLS(NDM,UPS,PAR)
    CALL EIGHO(AP,1,RR,RI,VT,PAR(12),ICP,PAR,NDM)
    CALL GETSTAB(NUNSTAB,RR,1)
    CALL EIGHO(AP,2,RR,RI,VR,PAR(12),ICP,PAR,NDM)
    CALL GETSTAB(NSTAB,RR,-1)

    IF (IRS>0.OR.ISTART/=3)THEN
       DEALLOCATE(RR,RI,VR,VT)
       RETURN
    ENDIF

    ! Set up artificial parameters at the left-hand end point of orbit

    IP=12
    IF(IEQUIB.GE.0) THEN 
       IP=IP+NDM
    ELSE
       IP=IP+2*NDM
    ENDIF
    KP=IP

    ! Parameters xi_1=eps_0, xi_i=0, i=2,NSTAB

    PAR(IP+1)=PAR(IP)
    DO I=2,NUNSTAB
       PAR(IP+I)=0.0
    ENDDO
    IP=IP+NUNSTAB

    ! Starting guess for homoclinic orbit in real principal unstable direction

    DO J=0,NTST*NCOL
       T=PAR(11)*J/(NTST*NCOL)
       DO K=1,NDIM
          UPS(K,J)=PAR(11+K)+VR(NSTAB+1,K)*PAR(KP)*EXP(RR(NSTAB+1)*T)
       ENDDO
       IF(AP%IID>0)THEN
          write(9,111)(ups(k,j),k=1,ndim)
       ENDIF
111    format('stpho : ',e20.10)
    ENDDO

    ! Artificial parameters at the right-hand end point of the orbit
    ! omega_i=<x(1)-x_o,w_i^*>

    DO I=1,NUNSTAB
       PAR(IP+I)=0.0
       DO J=1,NDM
          IF(IEQUIB.GE.0) THEN 
             P=0
          ELSE
             P=PAR(11+J)-PAR(11+NDM+J)
          ENDIF
          PAR(IP+I)=PAR(IP+I)+(P+VR(NSTAB+1,J)*PAR(KP)* &
               EXP(RR(NSTAB+1)*PAR(11)))*VT(NSTAB+I,J)
       ENDDO
    ENDDO
    IP=IP+NUNSTAB

    DEALLOCATE(RR,RI,VR,VT)
  END SUBROUTINE STPNHO

! ------ --------- --------
  DOUBLE PRECISION FUNCTION FNCSHO(AP,ICP,UPS,NDIM,PAR,ITEST,ATYPE)

    USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
    USE TOOLBOXBV, ONLY: FNCSBV
    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),NDIM
    DOUBLE PRECISION, INTENT(IN) :: UPS(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    INTEGER, INTENT(IN) :: ITEST
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: PU0(:),PU1(:),RR(:,:),RI(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: V(:,:,:),VT(:,:,:)
    INTEGER IID,NDM,NTST,NCOL,I,J
    LOGICAL INEIG
    DOUBLE PRECISION ORIENT

    FNCSHO=FNCSBV(AP,ICP,UPS,NDIM,PAR,ITEST,ATYPE)
    IF(ITEST>0)RETURN

    ALLOCATE(PU0(NDIM),PU1(NDIM))
    ALLOCATE(RR(NDIM,2),RI(NDIM,2),V(NDIM,NDIM,2),VT(NDIM,NDIM,2))

    IID=AP%IID
    NDM=AP%NDM
    NTST=AP%NTST
    NCOL=AP%NCOL

    !  *Compute eigenvalues
    INEIG=.FALSE.
    CALL EIGHO(AP,2,RR(1,1),RI(1,1),V(1,1,1),PAR(12),ICP,PAR,NDM)
    IF(IEQUIB<0)THEN
       CALL EIGHO(AP,2,RR(1,2),RI(1,2),V(1,1,2),PAR(12+NDM),ICP,PAR,NDM)
    ENDIF
    IF(IID.GE.3)THEN
       WRITE(9,*) 'EIGENVALUES'
       DO J=1,NDM
          WRITE(9,101) RR(J,1),RI(J,1)
       ENDDO
       IF(IEQUIB<0)THEN
          WRITE(9,*) 'EIGENVALUES of RHS equilibrium'
          DO J=1,NDM
             WRITE(9,101) RR(J,2),RI(J,2)
          ENDDO
       ENDIF
    ENDIF
    IF (((ITWIST==1).AND.(ISTART.GE.0)).OR.NPSI>0) THEN
       DO I=1,NDIM
          PU0(I)=UPS(I)
          PU1(I)=UPS(I+NTST*NCOL*NDIM)
       ENDDO
    ENDIF
    IF ((ITWIST==1).AND.(ISTART.GE.0)) THEN
       CALL EIGHO(AP,1,RR(1,1),RI(1,1),VT(1,1,1),PAR(12),ICP,PAR,NDM)
       IF(IEQUIB<0)THEN
          CALL EIGHO(AP,1,RR(1,2),RI(1,2),VT(1,1,2),PAR(12+NDM),ICP,PAR,NDM)
       ENDIF
       INEIG=.TRUE.
       ORIENT = PSIHO(NDM,0,RR,RI,V,VT,ICP,PAR,PU0,PU1)
       IF(IID.GE.3)THEN
          IF (ORIENT<0.0D0) THEN
             WRITE(9,102) ORIENT             
          ELSE
             WRITE(9,103) ORIENT   
          ENDIF
       ENDIF
    ENDIF

    DO I=1,NPSI
       IF((IPSI(I)>10).AND..NOT.INEIG) THEN
          CALL EIGHO(AP,1,RR(1,1),RI(1,1),VT(1,1,1),PAR(12),ICP,PAR,NDM)
          IF(IEQUIB<0)THEN
             CALL EIGHO(AP,1,RR(1,2),RI(1,2),VT(1,1,2),PAR(12+NDM),ICP,PAR,NDM)
          ENDIF
          INEIG=.TRUE.
       ENDIF
       PAR(20+IPSI(I))=PSIHO(NDM,IPSI(I),RR,RI,V,VT,ICP,PAR,PU0,PU1)
       IF(IID.GE.3)WRITE(9,104)IPSI(I),PAR(20+IPSI(I))
    ENDDO

    DEALLOCATE(PU0,PU1,RR,RI,V,VT)

101 FORMAT(1X,'(',F12.7,',',1X,F12.7,')')
102 FORMAT(1X,'Non-orientable',' (',E20.10,')')
103 FORMAT(1X,'orientable',' (',E20.10,')')      
104 FORMAT(1X,'PSI(',I2,')=',E20.10)

  END FUNCTION FNCSHO

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION PSIHO(NDM,IS,RR,RI,V,VT,ICP,PAR,PU0,PU1)

    ! The conditions for degenerate homoclinic orbits are given by PSI(IS)=0.

    ! RR and RI contain the real and imaginary parts of eigenvalues which are
    ! ordered with respect to their real parts (smallest first).    
    ! The (generalised) real eigenvectors are stored as the ROWS of V. 
    ! The (generalised) real left eigenvectors are in the ROWS of VT.
    ! In the block ENDPTS are stored the co-ordinates of the left (PU0)
    ! and right (PU1) endpoints of the solution (+  vector if that is computed)

    USE INTERFACES, ONLY: FUNC
    INTEGER, INTENT(IN) :: ICP(*),NDM,IS
    DOUBLE PRECISION, INTENT(IN) :: PAR(*),RR(NDM,*),RI(NDM,*), &
         V(NDM,NDM,*),VT(NDM,NDM,*),PU0(*),PU1(*)
    ! Local
    DOUBLE PRECISION, ALLOCATABLE :: F0(:),F1(:)
    INTEGER I,J
    DOUBLE PRECISION DROOT,DUM1(1),DUM2(1),F0NORM,F1NORM,S1,S2
    DOUBLE PRECISION U0NORM,U1NORM,VNORM1,VNORM2

    ALLOCATE(F0(NDM),F1(NDM))
    CALL FUNC(NDM,PU0,ICP,PAR,0,F0,DUM1,DUM2)
    CALL FUNC(NDM,PU1,ICP,PAR,0,F1,DUM1,DUM2)

    PSIHO=0.0D0

    !  Compute orientation

    IF (IS==0) THEN
       S1 = 0.0D0
       S2 = 0.0D0
       F0NORM = 0.0D0
       F1NORM = 0.0D0
       U0NORM = 0.0D0
       U1NORM = 0.0D0
       DO J=1,NDM
          S1 = S1 + F1(J)*PU0(NDM+J)
          S2 = S2 + F0(J)*PU1(NDM+J) 
          F0NORM=F0NORM+F0(J)**2
          F1NORM=F1NORM+F1(J)**2
          U0NORM=U0NORM+PU0(J+NDM)**2
          U1NORM=U1NORM+PU1(J+NDM)**2
       ENDDO
       DROOT=DSQRT(F0NORM*F1NORM*U0NORM*U1NORM)
       IF(DROOT/=0.d0)THEN
          PSIHO= - S1*S2/DROOT
       ELSE
          PSIHO=0.d0
       ENDIF
       RETURN
    ENDIF

    IF(IS/=11)DEALLOCATE(F1)
    IF(IS/=12)DEALLOCATE(F0)
    SELECT CASE(IS)

    CASE(1)
       ! Resonant eigenvalues (neutral saddle)
       PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)+RI(NSTAB,1)+RI(NSTAB+1,1)

    CASE(2)
       ! Double real leading eigenvalues (stable)
       !   (saddle, saddle-focus transition)
       IF (ABS(RI(NSTAB,1))>COMPZERO) THEN
          PSIHO=-(RI(NSTAB,1)-RI(NSTAB-1,1))**2
       ELSE
          PSIHO=(RR(NSTAB,1)-RR(NSTAB-1,1))**2
       ENDIF

    CASE(3)
       ! Double real positive eigenvalues (unstable)
       !   (saddle, saddle-focus transition)
       IF (ABS(RI(NSTAB+1,1))>COMPZERO) THEN
          PSIHO=-(RI(NSTAB+1,1)-RI(NSTAB+2,1))**2
       ELSE
          PSIHO=(RR(NSTAB+1,1)-RR(NSTAB+2,1))**2
       ENDIF

    CASE(4)
       ! Neutral saddle, saddle-focus or bi-focus (includes 1, above, also) 
       PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)

    CASE(5)
       ! Neutrally-divergent saddle-focus (stable eigenvalues complex)
       PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)+RR(NSTAB-1,1)

    CASE(6)
       ! Neutrally-divergent saddle-focus (unstable eigenvalues complex)
       PSIHO=RR(NSTAB,1)+RR(NSTAB+1,1)+RR(NSTAB+2,1)

    CASE(7)
       ! Three leading eigenvalues (stable)
       VNORM1 = 0D0
       VNORM2 = 0D0      
       DO I=1,NDM
          VNORM1 = VNORM1 + ABS(V(NSTAB,I,1))
          VNORM2 = VNORM2 + ABS(V(NSTAB-2,I,1))
       ENDDO
       IF (VNORM1>VNORM2) THEN
          PSIHO=RR(NSTAB,1)-RR(NSTAB-2,1)
       ELSE
          PSIHO=RR(NSTAB-2,1)-RR(NSTAB,1)
       ENDIF

    CASE(8)
       ! Three leading eigenvalues (unstable)
       VNORM1 = 0D0
       VNORM2 = 0D0      
       DO I=1,NDM
          VNORM1 = VNORM1 + ABS(V(NSTAB+1,I,1))
          VNORM2 = VNORM2 + ABS(V(NSTAB+3,I,1))
       ENDDO
       IF (VNORM1>VNORM2) THEN
          PSIHO=RR(NSTAB+1,1)-RR(NSTAB+3,1)
       ELSE
          PSIHO=RR(NSTAB+3,1)-RR(NSTAB+1,1)
       ENDIF

    CASE(9)
       ! Local bifurcation (zero eigenvalue or Hopf): NSTAB decreases
       !  (nb. the problem becomes ill-posed after a zero of 9 or 10)
       PSIHO=RR(NSTAB,1)

    CASE(10)
       ! Local bifurcation (zero eigenvalue or Hopf): NSTAB increases 
       PSIHO=RR(NSTAB+1,1) 

    CASE(11)
       ! Orbit flip (with respect to leading stable direction)
       ! e.g. 1D unstable manifold
       DO J=1,NDM
          PSIHO= PSIHO + F1(J)*VT(NSTAB,J,1)
       ENDDO
       PSIHO= PSIHO * EXP(-PAR(11)*RR(NSTAB,1)/2.0D0)
       DEALLOCATE(F1)

    CASE(12)
       ! Orbit flip (with respect to leading unstable direction)
       ! e.g. 1D stable manifold
       DO J=1,NDM
          PSIHO= PSIHO + F0(J)*VT(NSTAB+1,J,1)
       ENDDO
       PSIHO= PSIHO * EXP(PAR(11)*RR(NSTAB+1,1)/2.0D0)
       DEALLOCATE(F0)

    CASE(13)
       ! Inclination flip (critically twisted) with respect to stable manifold
       !   e.g. 1D unstable manifold   
       DO I=1,NDM
          PSIHO= PSIHO + PU0(NDM+I)*V(NSTAB,I,1)
       ENDDO
       PSIHO= PSIHO * EXP(-PAR(11)*RR(NSTAB,1)/2.0D0)

    CASE(14)
       ! Inclination flip (critically twisted) with respect to unstable manifold
       !   e.g. 1D stable manifold
       DO I=1,NDM
          PSIHO= PSIHO + PU1(NDM+I)*V(NSTAB+1,I,1)
       ENDDO
       PSIHO= PSIHO * EXP(PAR(11)*RR(NSTAB+1,1)/2.0D0)

    CASE(15)
       ! Non-central homoclinic to saddle-node (in stable manifold)
       DO I=1,NDM 
          PSIHO=PSIHO+(PAR(11+I)-PU1(I))*V(NSTAB+1,I,1)
       ENDDO

    CASE(16)
       ! Non-central homoclinic to saddle-node (in unstable manifold)
       DO I=1,NDM 
          PSIHO=PSIHO+(PAR(11+I)-PU0(I))*V(NSTAB+1,I,1)
       ENDDO

    END SELECT
  END FUNCTION PSIHO

! ---------- -------
  SUBROUTINE GETSTAB(N,RR,STAB)

    ! Determine number of stable (STAB==-1) or unstable (STAB==1)
    ! eigenvalues in RR

    INTEGER, INTENT(INOUT) :: N
    DOUBLE PRECISION, INTENT(IN) :: RR(:)
    INTEGER, INTENT(IN) :: STAB

    INTEGER I,IAMIN

    IF(N/=-1)RETURN

    IAMIN=0
    IF(IEQUIB==2)THEN
       !exclude eigenvalue closest to zero for saddle-node homs
       IAMIN=1
       DO I=1,SIZE(RR)
          IF(ABS(RR(I))<ABS(RR(IAMIN)))THEN
             IAMIN=I
          ENDIF
       ENDDO
    ENDIF
    N=0
    DO I=1,SIZE(RR)
       IF(I/=IAMIN.AND.RR(I)*STAB>0)THEN
          N=N+1
       ENDIF
    ENDDO
  END SUBROUTINE GETSTAB

! ---------- -----
  SUBROUTINE EIGHO(AP,ITRANS,RR,RI,VRET,XEQUIB,ICP,PAR,NDM)

    ! Uses LAPACK routine DGEEV to calculate the eigenvalues/eigenvectors
    ! of the linearization matrix a (obtained from DFHO) and orders them
    ! according to their real parts. Simple continuity with respect
    ! previous call with same value of ITRANS.

    ! input variables
    !         ITRANS = 1 use transpose of A (left eigenvectors)
    !                = 2 otherwise (right eigenvectors)

    ! output variables
    !         RR,RI real and imaginary parts of eigenvalues, ordered w.r.t
    !            real parts (largest first)
    !         VRET the rows of which are real parts of corresponding 
    !            eigenvectors 

    USE INTERFACES, ONLY:FUNI

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ITRANS,NDM
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*),XEQUIB(*)
    DOUBLE PRECISION, INTENT(OUT) :: RR(NDM),RI(NDM),VRET(NDM,NDM)
    ! Local
    LOGICAL, SAVE :: IEIGC(2) = (/.FALSE.,.FALSE./)
    CHARACTER(1) JOBVL,JOBVR
    DOUBLE PRECISION, ALLOCATABLE :: DFDU(:,:),DFDP(:,:),ZZ(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: F(:),WORK(:)
    DOUBLE PRECISION, SAVE, ALLOCATABLE :: VRPREV(:,:,:)
    INTEGER NPAR,IFAIL,LWORK,I,J,K,L,M
    DOUBLE PRECISION TMP,WORKINFO(1)
    INTEGER, EXTERNAL :: IDAMAX

    NPAR=AP%NPAR
    ALLOCATE(DFDU(NDM,NDM),DFDP(NDM,NPAR),ZZ(NDM,NDM),F(NDM))
    IFAIL=0

    CALL FUNI(AP,NDM,XEQUIB,XEQUIB,ICP,PAR,1,F,DFDU,DFDP)
    IF(PAR(11)<0)THEN
       ! reverse time: reverse eigenvalues/vectors
       DFDU(:,:)=-DFDU(:,:)
    ENDIF

    IF (ITRANS==1) THEN
       JOBVL='V'
       JOBVR='N'
    ELSE
       JOBVL='N'
       JOBVR='V'
    ENDIF

    ! LAPACK call for eigenvalues and eigenvectors
    CALL DGEEV(JOBVL,JOBVR,NDM,DFDU,NDM,RR,RI,ZZ,NDM,ZZ,NDM,WORKINFO,-1,IFAIL)
    LWORK=NINT(WORKINFO(1))
    ALLOCATE(WORK(LWORK))
    CALL DGEEV(JOBVL,JOBVR,NDM,DFDU,NDM,RR,RI,ZZ,NDM,ZZ,NDM,WORK,LWORK,IFAIL)
    DEALLOCATE(WORK)

    IF (IFAIL/=0.AND.AP%IID>0) THEN   
       WRITE(9,*) 'LAPACK EIGENVALUE ROUTINE FAILED !'
    ENDIF

    J=1
    DO WHILE(J<NDM)
       IF(RR(J)==RR(J+1).AND.RI(J)/=0.d0)THEN
          ! complex conjugate
          ZZ(:,J+1)=ZZ(:,J)
          J=J+1
       ENDIF
       J=J+1
    ENDDO

    ! Order the eigenvectors/values according size of real part of eigenvalue.
    ! (smallest first)

    DO I=1,NDM-1
       J=I
       DO L=I+1,NDM
          IF (RR(L)<RR(J)) THEN
             J=L
          ENDIF
       ENDDO
       IF(J>I)THEN
          TMP=RR(I)
          RR(I)=RR(J)
          RR(J)=TMP
          TMP=RI(I)
          RI(I)=RI(J)
          RI(J)=TMP
          DO K=1,NDM
             TMP=ZZ(K,I)
             ZZ(K,I)=ZZ(K,J)
             ZZ(K,J)=TMP
          ENDDO
       ENDIF
    ENDDO

    ! Choose sign of real part of eigenvectors to be 
    ! commensurate with that of the corresponding eigenvector 
    ! from the previous call with the same value of ITRANS

    IF (.NOT.IEIGC(ITRANS)) THEN
       IF(.NOT.ALLOCATED(VRPREV))ALLOCATE(VRPREV(NDM,NDM,2))
       ! normalize eigenvector so that the largest (in absolute value)
       ! component is positive
       DO I=1,NDM
          M=IDAMAX(NDM,ZZ(1,I),1)
          IF(ZZ(M,I)<0)THEN
             ZZ(:,I)=-ZZ(:,I)
          ENDIF
       ENDDO
       IEIGC(ITRANS)=.TRUE.
    ELSE
       DO I=1,NDM
          IF (DOT_PRODUCT(ZZ(:,I),VRPREV(:,I,ITRANS))<0.0D0) THEN
             ZZ(:,I)=-ZZ(:,I)
          ENDIF
       ENDDO
    ENDIF
    VRPREV(:,:,ITRANS)=ZZ(:,:)

    ! Send back the transpose of the matrix of real parts of eigenvectors

    DO I=1,NDM
       VRET(I,:)=ZZ(:,I)
    ENDDO

    DEALLOCATE(F,DFDU,DFDP,ZZ)

  END SUBROUTINE EIGHO

! ------- -------- ------
  LOGICAL FUNCTION SELPOS(ER, EI)
    DOUBLE PRECISION, INTENT(IN) :: ER,EI
    SELPOS = ER>0
  END FUNCTION SELPOS

! ------- -------- ------
  LOGICAL FUNCTION SELNEG(ER, EI)
    DOUBLE PRECISION, INTENT(IN) :: ER,EI
    SELNEG = ER<0
  END FUNCTION SELNEG

! ---------- ------
  SUBROUTINE PRJCTN(AP,BOUND,CSAVE,XEQUIB,ICP,PAR,IMFD,IS,ITRANS,NDM)

    ! Compute NUNSTAB (or NSTAB) projection boundary condition functions
    ! onto to the UNSTABLE (or STABLE) manifold of the appropriate equilibrium

    ! IMFD   = -1 stable eigenspace
    !        =  1 unstable eigenspace
    ! ITRANS =  1 use transpose of A
    !        =  2 otherwise
    ! IS     =  I (1 or 2) implies use the ith equilibrium in XEQUIB

    ! Use the normalization in Beyn 1990 (4.4) to ensure continuity 
    ! w.r.t parameters.
    ! For the purposes of this routine the "previous point on the
    ! branch" is at the values of PAR at which the routine was last
    ! called with the same values of IS and ITRANS.

    USE INTERFACES, ONLY:FUNI
    USE SUPPORT, ONLY: GEL

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),IMFD,IS,ITRANS,NDM
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*),XEQUIB(*)
    DOUBLE PRECISION, INTENT(OUT) :: BOUND(NDM,*)
    LOGICAL, INTENT(IN) :: CSAVE
    ! Local
    LOGICAL , SAVE :: IFLAG(2,2) = &
         RESHAPE((/.TRUE.,.TRUE.,.TRUE.,.TRUE./),(/2,2/))
    LOGICAL, ALLOCATABLE :: BWORK(:)
    DOUBLE PRECISION DDUM(1)
    DOUBLE PRECISION, ALLOCATABLE :: ER(:),EI(:),D(:,:),CPREV(:,:,:,:)
    DOUBLE PRECISION, ALLOCATABLE :: DUM1(:,:),DUM2(:,:),FDUM(:)
    DOUBLE PRECISION, ALLOCATABLE :: WORK(:),A(:,:),V(:,:)
    INTEGER MCOND,IFAIL,LWORK,ISDIM,I,J
    INTEGER LOC(1),IWORK(1)
    DOUBLE PRECISION TMP,DET,S,SEP

    SAVE CPREV

    ALLOCATE(A(NDM,NDM),FDUM(NDM))
    CALL FUNI(AP,NDM,XEQUIB,XEQUIB,ICP,PAR,1,FDUM,A,DDUM)
    IF(PAR(11)<0)THEN
       ! reverse time: reverse eigenvalues/vectors
       A(:,:)=-A(:,:)
    ENDIF
    DEALLOCATE(FDUM)

    ! Compute transpose of A if ITRANS=1
    IF (ITRANS==1) THEN
       DO I=1,NDM
          DO J=1,I-1
             TMP=A(I,J)
             A(I,J)=A(J,I)
             A(J,I)=TMP
          ENDDO
       ENDDO
    ENDIF

    IF (IMFD==1) THEN
       MCOND = NUNSTAB
    ELSE
       MCOND = NSTAB
    ENDIF

    ! Call LAPACK routine for the Schur decomposition of A

    ALLOCATE(ER(NDM),EI(NDM),BWORK(NDM))
    CALL DGEES('V', 'S', SELNEG, NDM, A, NDM, ISDIM, ER, &
         EI, BOUND, NDM, DDUM, -1, BWORK, IFAIL)
    LWORK = NINT(DDUM(1))
    ALLOCATE(WORK(LWORK))
    IF(IMFD==-1)THEN
       CALL DGEES('V', 'S', SELNEG, NDM, A, NDM, ISDIM, ER, &
            EI, BOUND, NDM, WORK, LWORK, BWORK, IFAIL)
    ELSE
       CALL DGEES('V', 'S', SELPOS, NDM, A, NDM, ISDIM, ER, &
            EI, BOUND, NDM, WORK, LWORK, BWORK, IFAIL)
    ENDIF
    IF (IFAIL==0.AND.ISDIM/=MCOND) THEN
       BWORK=.TRUE.

       ! Get orthonormal basis for the nstab lowest eigenvalues
       ! or nunstab highest eigenvalues (real part),
       ! which do not necessarily all have to be negative/positive

       DO I=1,MCOND
          IF (IMFD==-1) THEN
             LOC=MINLOC(ER,BWORK)
          ELSE
             LOC=MAXLOC(ER,BWORK)
          ENDIF
          BWORK(LOC(1))=.FALSE.
       ENDDO
       BWORK=.NOT.BWORK
       CALL DTRSEN('N', 'V', BWORK, NDM, A, NDM, BOUND, NDM, ER, EI, &
            ISDIM, S, SEP, WORK, LWORK, IWORK, 1, IFAIL)
       IF (IFAIL/=0.AND.AP%IID>0) THEN
          WRITE(9,*)'LAPACK SCHUR DECOMPOSITION ROUTINE FAILED !'
       ENDIF
    ENDIF
    DEALLOCATE(A,ER,EI,WORK,BWORK)

    ! Set previous matrix to be the present one if this is the first call
    IF (IFLAG(IS,ITRANS)) THEN
       IF (.NOT.ALLOCATED(CPREV))ALLOCATE(CPREV(NDM,NDM,2,2))
       CPREV(1:NDM,1:MCOND,IS,ITRANS)=BOUND(1:NDM,1:MCOND)
       IFLAG(IS,ITRANS)=.FALSE.
       RETURN
    ENDIF

    ! Calculate the BEYN matrix D and hence BOUND
    ! This amounts to solving (CPREV^T * V) * D = CPREV^T * CPREV
    ! and then calculating BOUND = V * D

    IF(MCOND==0)RETURN

    ALLOCATE(D(MCOND,MCOND),DUM1(MCOND,MCOND),DUM2(MCOND,MCOND))
    ! DUM1 = CPREV^T * BOUND
    CALL DGEMM('T','N',MCOND,MCOND,NDM,1.d0,CPREV(1,1,IS,ITRANS), &
         NDM,BOUND,NDM,0.d0,DUM1,MCOND)
    ! DUM2 = CPREV^T * CPREV
    CALL DGEMM('T','N',MCOND,MCOND,NDM,1.d0,CPREV(1,1,IS,ITRANS), &
         NDM,CPREV(1,1,IS,ITRANS),NDM,0.d0,DUM2,MCOND)

    ! D = DUM1^-1 * DUM2
    CALL GEL(MCOND,DUM1,MCOND,D,DUM2,DET)

    ! BOUND = BOUND * D
    ALLOCATE(V(NDM,MCOND))
    CALL DGEMM('N','N',NDM,MCOND,MCOND,1d0,BOUND,NDM,D,MCOND,0d0,V,NDM)
    BOUND(1:NDM,1:MCOND)=V(1:NDM,1:MCOND)
    DEALLOCATE(V,D,DUM1,DUM2)

    IF(CSAVE)THEN
       CPREV(1:NDM,1:MCOND,IS,ITRANS)=BOUND(1:NDM,1:MCOND)
    ENDIF

  END SUBROUTINE PRJCTN

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

END MODULE HOMCONT

BLOCK DATA
   INTEGER ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
   COMMON /BLHOM/ ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV
   DATA ITWIST,ISTART,IEQUIB,NFIXED,NPSI,NUNSTAB,NSTAB,NREV &
        /0,5,1,0,0,-1,-1,0/
END BLOCK DATA
