!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Algebraic Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE AE

  USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: AUTOAE

CONTAINS

! ---------- ------
  SUBROUTINE AUTOAE(AP,ICP,ICU,FUNI,STPNAEI,FNCI)

! This is the entry subroutine for algebraic systems.

    USE AUTOMPI

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ICU(*)

    include 'interfaces.h'

    IF(MPIIAM()>0)THEN
       IF(MPIWFI(.FALSE.))THEN
          RETURN
       ENDIF
    ENDIF
    CALL CNRLAE(AP,ICP,ICU,FUNI,STPNAEI,FNCI)

  END SUBROUTINE AUTOAE

! ---------- ------
  SUBROUTINE CNRLAE(AP,ICP,ICU,FUNI,STPNAEI,FNCI)

    USE IO
    USE MESH
    USE SUPPORT, ONLY: AA=>P0V, P1V, EVV, FNCS, STOPPED, INIT2, INIT3, &
         INITSTOPCNTS, PVLI, LBITP
    USE AUTO_CONSTANTS, ONLY: NPARX

! Controls the bifurcation analysis of algebraic problems

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ICU(*)
! Local
    DOUBLE PRECISION, ALLOCATABLE :: &
         PAR(:),VUZ(:),THU(:),THL(:), &
         U(:),UDOT(:),STUD(:,:),STU(:,:),TEST(:)
    INTEGER, ALLOCATABLE :: IUZ(:)
    LOGICAL IPOS
    INTEGER NDIM,IPS,IRS,ILP,IADS,ISP,ISW,NUZR,MXBF,NBIFS,NBFCS,ITPST
    INTEGER ITNW,ITP,I,ITEST,NINS,NBIF,NBFC,NODIR,NIT,NTOT,NTOP
    INTEGER NDM,IFOUND,ISTEPPED
    DOUBLE PRECISION DS,DSMAX,RDS,DSTEST,TMP
    LOGICAL ISTOP,STEPPED
    INTEGER STOPCNTS(-9:14)
    CHARACTER(4) ATYPE,ATYPEDUM

    CALL INIT2(AP,ICP,ICU)

    NDIM=AP%NDIM
    NDM=AP%NDM
    IPS=AP%IPS
    IRS=AP%IRS
    ILP=AP%ILP
    IADS=AP%IADS
    ISP=AP%ISP
    ISW=AP%ISW
    NUZR=AP%NUZR
    MXBF=AP%MXBF
    NBIFS=ABS(MXBF)
    IF(MXBF>0)THEN
       NBFCS=2*NBIFS
    ELSE
       NBFCS=NBIFS
    ENDIF
    ITPST=AP%ITPST

    DS=AP%DS

    ! allocate a minimum of NPARX so we can detect overflows 
    ! past NPAR gracefully
    ! set thu to 1 higher than NDIM for (u,par) representation
    ALLOCATE(PAR(MAX(AP%NPAR,NPARX)),THL(AP%NFPR),THU(NDIM+1))
    ALLOCATE(IUZ(NUZR),VUZ(NUZR))
    CALL INIT3(AP,ICP,PAR,THL,THU,IUZ,VUZ)
    THU(AP%NDIM+1)=THL(1)

    ALLOCATE(AA(NDIM+1,NDIM+1),U(NDIM+1),UDOT(NDIM+1))
    ALLOCATE(STUD(NBIFS,NDIM+1),STU(NBIFS,NDIM+1),TEST(AP%NTEST),EVV(NDM))
    ALLOCATE(P1V(NDIM+1,NDIM+1))

    NINS=0
    AP%NINS=NINS
    RDS=DS
    NBIF=0
    IPOS=.TRUE.

    DO I=1,NDIM
       U(I)=0.d0
       UDOT(I)=0.d0
    ENDDO
    ! init P1 to identity matrix so that GETMDMX in support.f90 makes sense
    P1V(:,:)=0.d0
    DO I=1,NDIM+1
       P1V(I,I)=1.d0
    ENDDO

! Generate the starting point

    NODIR=1
    CALL STPNAEI(AP,PAR,ICP,U,UDOT,NODIR)
    CALL PVLI(AP,ICP,U,NDIM,PAR,FNCI)

! Determine a suitable starting label and branch number

    CALL NEWLAB(AP)

! Write constants

    CALL STHD(AP,ICP)

    DO NBFC=0,NBFCS !bifurcation switch loop

       DO I=1,AP%NTEST
          TEST(I)=0.d0
       ENDDO

       NTOT=0
       AP%NTOT=NTOT
       CALL INITSTOPCNTS(ISP,ILP,ITPST,STOPCNTS)
       ISTOP=.FALSE.
       NIT=1

       IF(IRS.EQ.0) THEN
          ITP=9+10*ITPST
       ELSE
          ITP=0
       ENDIF
       AP%ITP=ITP
       U(NDIM+1)=PAR(ICP(1))

! Starting procedure  (to get direction vector) :

       IF(NODIR==1.AND.ISW>=0)THEN
          CALL STPRAE(AP,PAR,ICP,FUNI,U,UDOT,THU,0,AA)
       ELSEIF(IRS/=0.AND.ISW<0)THEN
          CALL STPRAE(AP,PAR,ICP,FUNI,U,UDOT,THU,1,AA)
       ELSEIF(ABS(IPS)==1.OR.IPS==11)THEN
          CALL STPRAE(AP,PAR,ICP,FUNI,U,UDOT,THU,-1,AA)
       ENDIF
       IF(ABS(IPS)==1.OR.IPS==11)THEN
          ! Get stability
          TMP=FNCI(AP,ICP,U,NDIM,PAR,6,ATYPEDUM)
       ENDIF

! Store plotting data for first point on the bifurcating branch
! or for the starting point

       CALL PVLI(AP,ICP,U,NDIM,PAR,FNCI)
       CALL STPLAE(AP,PAR,ICP,ICU,U,UDOT,NIT,ISTOP)

       IF(.NOT.ISTOP)THEN

! Provide initial approximation to the second point on the branch and
! determine the second point on the bifurcating or original branch
          CALL STEPAE(AP,PAR,ICP,FUNI,RDS,AA,U,UDOT,THU,NIT,ISW<0)
          IF(NIT>0)CALL PVLI(AP,ICP,U,NDIM,PAR,FNCI)

          IF(ISW<0.OR.NIT==0)THEN
             IF(ABS(IPS)==1.OR.IPS==11)THEN
                ! Get stability
                TMP=FNCI(AP,ICP,U,NDIM,PAR,6,ATYPEDUM)
             ENDIF
             ! Store plotting data for second point :
             CALL PVLI(AP,ICP,U,NDIM,PAR,FNCI)
             CALL STPLAE(AP,PAR,ICP,ICU,U,UDOT,NIT,ISTOP)
          ENDIF
       ENDIF

       DO WHILE(.NOT.ISTOP) ! branch computation loop
          ITP=0
          AP%ITP=ITP
          NINS=AP%NINS

! Find the next solution point on the branch
          CALL STEPAE(AP,PAR,ICP,FUNI,RDS,AA,U,UDOT,THU,NIT)
          ISTOP=NIT==0
          IF(.NOT.ISTOP)THEN
             CALL PVLI(AP,ICP,U,NDIM,PAR,FNCI)
          ENDIF
          DSTEST=RDS

          IFOUND=0
          IF(ISTOP)THEN
             ISTEPPED=AP%NTEST+1
          ELSE
             ISTEPPED=0
             DO ITEST=1,AP%NTEST
                ! Check for special points
                CALL LCSPAE(AP,DSTEST,PAR,ICP,ITEST,FUNI,FNCI,AA,&
                     U,UDOT,TEST(ITEST),THU,IUZ,VUZ,NIT,ATYPE,STEPPED)
                IF(STEPPED)ISTEPPED=ITEST
                IF(LEN_TRIM(ATYPE)>0)THEN
                   IFOUND=ITEST
                   AP%ITP=LBITP(ATYPE,.FALSE.)
                   AP%ITP=AP%ITP+SIGN(10,AP%ITP)*ITPST
                ENDIF
             ENDDO
          ENDIF

          DO ITEST=1,ISTEPPED-1
             ! evaluate the test functions for the next step
             TEST(ITEST)=FNCS(AP,ICP,U,PAR,ATYPEDUM,IUZ,VUZ,ITEST,FNCI)
          ENDDO

          ITP=AP%ITP
          IF(ITP/=0)THEN
             IF(STOPPED(IUZ,IFOUND,NUZR,ITP,STOPCNTS))THEN
                ISTOP=.TRUE. ! *Stop at the first found bifurcation
             ENDIF
             IF(MOD(ITP,10)==1)THEN
                ! Check for branch point, and if so store data :
                CALL STBIF(NDIM,NBIF,NBIFS,STUD,STU,U,UDOT)
             ENDIF
             IF(MOD(ITP,10)/=-4)THEN
                ! for plotter: use stability of previous point
                ! for bifurcation points
                AP%NINS=NINS
             ENDIF
          ENDIF

! Store plotting data on unit 7 :

          NTOT=AP%NTOT
          CALL PVLI(AP,ICP,U,NDIM,PAR,FNCI)
          CALL STPLAE(AP,PAR,ICP,ICU,U,UDOT,NIT,ISTOP)

! Adapt the stepsize along the branch

          IF(IADS.NE.0 .AND. MOD(NTOT,IADS).EQ.0 &
               .AND. ( MOD(ITP,10).EQ.0 .OR. MOD(ITP,10).EQ.4) )THEN
             ITNW=AP%ITNW
             NTOP=MOD(NTOT-1,9999)+1
             DSMAX=AP%DSMAX
             CALL ADPTDS(NIT,ITNW,AP%IBR,NTOP,AP%IID,DSMAX,RDS)
             AP%RDS=RDS
          ENDIF
       ENDDO !from branch computation loop

       IF(NBIF==0.OR.NBFC>=NBFCS)EXIT

       ! Initialize computation of the next bifurcating branch.

       CALL SWPNT(AP,DS,PAR,ICP,RDS,NBIF,NBIFS,STUD,STU,U,UDOT,IPOS)

       IF(IPOS)THEN
          NBIF=NBIF-1
       ENDIF

       IF(.NOT.IPOS .OR. MXBF.LT.0 )AP%IBR=AP%IBR+1

       ! IRS and ISW are for internal use: don't store in AP!
       IRS=1
       ISW=-1
    ENDDO !from bifurcation switch loop

    DEALLOCATE(PAR,THL,THU,IUZ,VUZ,EVV,AA,P1V,U,UDOT,STUD,STU,TEST)
  END SUBROUTINE CNRLAE

! ---------- ------
  SUBROUTINE STPRAE(AP,PAR,ICP,FUNI,U,UDOT,THU,IPERP,AA)

    USE SUPPORT

! Finds the second point on the initial solution branch.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*),IPERP
    DOUBLE PRECISION U(AP%NDIM+1),UDOT(AP%NDIM+1),THU(*),PAR(*)
    DOUBLE PRECISION AA(AP%NDIM+1,AP%NDIM+1)

! Local
    DOUBLE PRECISION, ALLOCATABLE :: AAA(:,:),F(:),UOLD(:),DFDU(:,:),DFDP(:,:)
    INTEGER NDIM,IID,NPAR,I,J
    DOUBLE PRECISION SS

    NDIM=AP%NDIM
    IID=AP%IID
    NPAR=AP%NPAR

    ALLOCATE(F(NDIM),UOLD(NDIM+1),DFDU(NDIM,NDIM),DFDP(NDIM,NPAR))
    UOLD(:)=U(:)
    DFDU(:,:)=0.d0
    DFDP(:,:)=0.d0

! Determine the direction of the branch at the starting point

    CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)

    IF(IPERP==1)THEN
       AA(:NDIM,:NDIM)=DFDU(:,:)
       AA(:NDIM,NDIM+1)=DFDP(:,ICP(1))
       AA(NDIM+1,:)=UDOT(:)
       IF(IID.GE.3)CALL WRJAC(NDIM+1,NDIM+1,AA,F)
       CALL NLVC(NDIM+1,NDIM+1,1,AA,UDOT)
    ELSEIF(IPERP==0)THEN
       ALLOCATE(AAA(NDIM,NDIM+1))
       AAA(:,1:NDIM)=DFDU(:,:)
       AAA(:,NDIM+1)=DFDP(:,ICP(1))
       IF(IID.GE.3)CALL WRJAC(NDIM,NDIM+1,AAA,F)
       CALL NLVC(NDIM,NDIM+1,1,AAA,UDOT)
       DEALLOCATE(AAA)
    ENDIF

! Scale and make sure that PAR(ICP(1))-dot is positive.
! If PAR(ICP(1))-dot is close to zero, then make sure
! that the first away-from-zero coordinate is positive.

    SS=0.d0
    DO I=1,NDIM+1
       SS=SS+THU(I)*UDOT(I)**2
    ENDDO
    UDOT(:)=UDOT(:)/SQRT(SS)

    IF(ABS(UDOT(NDIM+1))/(1.d0+ABS(U(NDIM+1)))>AP%EPSL)THEN
       IF(UDOT(NDIM+1)<0.d0)THEN
          UDOT(:)=-UDOT(:)
       ENDIF
    ELSE
       DO I=1,NDIM
          IF(I<AP%NFPR)THEN
             J=NDIM-AP%NFPR+I+1
          ELSE
             J=I-AP%NFPR+1
          ENDIF
          IF(ABS(UDOT(J))/(1.d0+ABS(U(J)))>AP%EPSU)THEN
             IF(UDOT(J)<0.d0)THEN
                UDOT(:)=-UDOT(:)
             ENDIF
             EXIT
          ENDIF
       ENDDO
    ENDIF

! Get the Jacobian for stability computation.
    AA(:NDIM,:NDIM)=DFDU(:,:)
    AA(:NDIM,NDIM+1)=DFDP(:,ICP(1))
    AA(NDIM+1,:)=UDOT(:)

    DEALLOCATE(F,DFDU,DFDP)

  END SUBROUTINE STPRAE

! ---------- ------
  SUBROUTINE STEPAE(AP,PAR,ICP,FUNI,RDS,AA,U,UDOT,THU,NIT,SW)

! This is the subroutine for computing solution branches. It solves
! the equations for finding the next point on the branch at distance RDS
! from the current point. RDS is adapted if there is no convergence.

! It also controls the computation of the second point on a bifurcating branch.
! This point is required to lie in a hyper-plane at distance DS from the
! branch point. This hyper-plane is parallel to the tangent of the
! known branch at the branch point.

    USE MESH
    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NIT
    DOUBLE PRECISION, INTENT(IN) :: THU(*)
    DOUBLE PRECISION, INTENT(OUT) :: AA(AP%NDIM+1,AP%NDIM+1)
    DOUBLE PRECISION, INTENT(INOUT) :: U(*),UDOT(*),PAR(*),RDS
    LOGICAL, OPTIONAL, INTENT(IN) :: SW
! Local
    INTEGER IADS,IID,ITNW,IBR,NTOT,NTOP
    LOGICAL BSW
    DOUBLE PRECISION DSMIN,DSMAX
    CHARACTER (LEN=7) FIXEDMINIMUM

    IADS=AP%IADS
    IID=AP%IID
    ITNW=AP%ITNW
    IBR=AP%IBR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    DSMIN=AP%DSMIN
    DSMAX=AP%DSMAX

    BSW=.FALSE.
    IF(PRESENT(SW))BSW=SW
    
    DO
       CALL SOLVAE(AP,PAR,ICP,FUNI,RDS,AA,U,UDOT,THU,NIT,BSW)
       IF(NIT>0)RETURN

! Maximum number of iterations has been reached.

       IF(IADS.EQ.0)EXIT

! Reduce stepsize and try again.

       CALL ADPTDS(ITNW,ITNW,IBR,NTOP,IID,DSMAX,RDS)
       AP%RDS=RDS
       IF(ABS(RDS).LT.DSMIN)EXIT
       IF(IID.GE.2)THEN
          WRITE(9,"(I4,I6,A)")IBR,NTOP,' NOTE:Retrying step'
       ENDIF
    ENDDO

! Minimum stepsize reached.

    IF(IADS==0)THEN
       FIXEDMINIMUM='fixed'
    ELSE
       FIXEDMINIMUM='minimum'
    ENDIF
    IF(IID>0)THEN
       IF(BSW)THEN
          WRITE(9,"(I4,I6,A,A,A)")&
               IBR,NTOP,' NOTE:No convergence when switching branches with ',&
               FIXEDMINIMUM,' step size'
       ELSE
          WRITE(9,"(I4,I6,A,A,A)")&
               IBR,NTOP,' NOTE:No convergence with ',FIXEDMINIMUM,' step size'
       ENDIF
    ENDIF
  END SUBROUTINE STEPAE

! ---------- ------
  SUBROUTINE SOLVAE(AP,PAR,ICP,FUNI,RDS,AA,U,UDOT,THU,NIT,BSW)

! This subroutine contains the main predictor-corrector loop
! Input: U, UDOT, PAR : extended solution
!        RDS: step-size
!        AP AP ICP, THU, FUNI: constants & function interface
!        BSW: if true, switch branches, else do normal continuation
! Output: NIT: number of iterations taken to converge
!         after successful convergence (NIT>0):
!           AP%DET contains the determinant 
!           AA contains the extended Jacobian matrix
!           U, UDOT, PAR are updated
!         else (NIT==0):
!           U, UDOT, PAR are not updated

    USE IO
    USE SUPPORT

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*)
    INTEGER, INTENT(OUT) :: NIT
    LOGICAL, INTENT(IN) :: BSW
    DOUBLE PRECISION, INTENT(IN) :: RDS,THU(*)
    DOUBLE PRECISION, INTENT(OUT) :: AA(AP%NDIM+1,AP%NDIM+1)
    DOUBLE PRECISION, INTENT(INOUT) :: U(AP%NDIM+1),UDOT(AP%NDIM+1),PAR(*)

! Local
    INTEGER NDIM,IID,ITNW,IBR,NTOT,NTOP,NIT1,NPAR,I,K,NDM
    LOGICAL CONVERGED
    DOUBLE PRECISION, ALLOCATABLE :: RHS(:),DU(:), &
         DFDU(:,:),DFDP(:,:),UOLD(:)
    DOUBLE PRECISION EPSL,EPSU,SS,UMX,DUMX,RDRLM,RDUMX,DELREF,ADU,AU,DET
    CHARACTER (LEN=*), PARAMETER :: O9 = & 
     "(' Branch ',I2,' N=',I5,1X,'IT=',I2,1X,'PAR(',I2,')=', &
        &ES11.3,1X,'U=',7ES11.3)"

    NDIM=AP%NDIM
    IID=AP%IID
    ITNW=AP%ITNW
    NDM=AP%NDM
    IBR=AP%IBR
    NPAR=AP%NPAR
    NTOT=AP%NTOT
    NTOP=MOD(NTOT-1,9999)+1

    EPSL=AP%EPSL
    EPSU=AP%EPSU

    ALLOCATE(RHS(NDIM+1),DU(NDIM+1),DFDU(NDIM,NDIM),DFDP(NDIM,NPAR))
    DFDU(:,:)=0.d0
    DFDP(:,:)=0.d0

    ALLOCATE(UOLD(NDIM+1))
    UOLD(:)=U(:)
    U(:)=UOLD(:)+RDS*UDOT(:)

! Write additional output on unit 9 if requested :

    IF(IID.GE.2)THEN
       CALL WRBAR("=",47)
       IF(BSW)THEN
          WRITE(9,O9)IBR,NTOP,0,ICP(1), &
               U(NDIM+1),(U(I),I=1,MIN(NDIM,6))
       ELSE
          WRITE(9,100)
          WRITE(9,101)IBR,NTOP+1,0,U(NDIM+1),RNRMV(NDM,U)
100       FORMAT(/,'  BR    PT  IT         PAR',11X,'L2-NORM')
101       FORMAT(I4,I6,I4,5X,2ES14.5)
       ENDIF
    ENDIF

! Call user-supplied FUNC to evaluate the right hand side of the
! differential equation and its derivatives :

    CONVERGED=.FALSE.
    DELREF=0
    DO NIT1=1,ITNW

       NIT=NIT1
       PAR(ICP(1))=U(NDIM+1)
       CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,2,RHS,DFDU,DFDP)

! Set up the Jacobian matrix and the right hand side :

       DO I=1,NDIM
          AA(I,NDIM+1)=DFDP(I,ICP(1))
          RHS(I)=-RHS(I)
          DO K=1,NDIM
             AA(I,K)=DFDU(I,K)
          ENDDO
       ENDDO
       SS=0.d0
       IF(BSW)THEN
          ! Branch switch
          DO K=1,NDIM+1
             AA(NDIM+1,K)=THU(K)*UDOT(K)
             SS=SS+THU(K)*(U(K)-UOLD(K)-RDS*UDOT(K))*UDOT(K)
          ENDDO
          RHS(NDIM+1)=-SS
       ELSE
          DO K=1,NDIM+1
             AA(NDIM+1,K)=2.d0*THU(K)*(U(K)-UOLD(K))/RDS
             SS=SS+THU(K)*(U(K)-UOLD(K))**2
          ENDDO
          RHS(NDIM+1)=RDS-SS/RDS
       ENDIF

! Use Gauss elimination with pivoting to solve the linearized system :

       IF(IID.GE.5)CALL WRJAC(NDIM+1,NDIM+1,AA,RHS)
       CALL GEL(NDIM+1,AA,1,DU,RHS,DET)
       AP%DET=DET

! Add the Newton increments :

       DO I=1,NDIM+1
          U(I)=U(I)+DU(I)
       ENDDO
       DUMX=0.d0
       UMX=0.d0
       DO I=1,NDIM
          ADU=ABS(DU(I))
          AU=ABS(U(I))
          IF(AU>UMX)UMX=AU
          IF(ADU>DUMX)DUMX=ADU
       ENDDO

       IF(IID.GE.2)THEN
          IF(BSW)THEN
             WRITE(9,O9)IBR,NTOP,NIT,ICP(1),U(NDIM+1),(U(I),I=1,MIN(NDIM,6))
          ELSE
             WRITE(9,101)IBR,NTOP+1,NIT,U(NDIM+1),RNRMV(NDM,U)
          ENDIF
       ENDIF

! Check whether relative error has reached user-supplied tolerance :

       RDRLM=ABS(DU(NDIM+1))/(1.d0+ABS(U(NDIM+1)))
       RDUMX=DUMX/(1.d0+UMX)
       IF(RDRLM.LE.EPSL.AND.RDUMX.LE.EPSU)THEN
! Recompute Jacobian for test functions
          PAR(ICP(1))=U(NDIM+1)
          CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,2,RHS,DFDU,DFDP)
          DO I=1,NDIM
             AA(I,NDIM+1)=DFDP(I,ICP(1))
             DO K=1,NDIM
                AA(I,K)=DFDU(I,K)
             ENDDO
          ENDDO
          DO K=1,NDIM+1
             AA(NDIM+1,K)=UDOT(K)
          ENDDO
          IF(IID.GE.2)WRITE(9,*)
! This subroutine determines an initial approximation to the next
! solution on a branch by extrapolating from the two preceding points.
          UDOT(:)=(U(:)-UOLD(:))/RDS
          CONVERGED=.TRUE.
          EXIT
       ENDIF

       IF(.NOT.BSW)THEN
          IF(NIT.EQ.1)THEN
             DELREF=20*MAX(RDRLM,RDUMX)
          ELSE
             IF(MAX(RDRLM,RDUMX).GT.DELREF)EXIT
          ENDIF
       ENDIF

    ENDDO
    IF(.NOT.CONVERGED)THEN
       DO I=1,NDIM+1
          U(I)=UOLD(I)
       ENDDO
       PAR(ICP(1))=U(NDIM+1)
       NIT=0
    ENDIF
    DEALLOCATE(RHS,DU,DFDU,DFDP,UOLD)

  END SUBROUTINE SOLVAE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!               Detection of Singular Points
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! ---------- ------
  SUBROUTINE LCSPAE(AP,DSTEST,PAR,ICP,ITEST,FUNI,FNCI,AA, &
       U,UDOT,Q,THU,IUZ,VUZ,NIT,ATYPE,STEPPED)

    USE SUPPORT

    DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

! This subroutine uses the secant method to accurately locate special
! points (branch points, folds, Hopf bifurcations, user zeroes).
! These are characterized as zeroes of the function FNCS.
! This subroutine calls CONT and STEPAE with varying stepsize RDS.
! The special point is assumed to have been found with sufficient
! accuracy if the ratio between RDS and the user supplied value of
! DS is less than the user-supplied toler EPSS.

    include 'interfaces.h'

    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),IUZ(*),ITEST
    INTEGER, INTENT(INOUT) :: NIT
    DOUBLE PRECISION, INTENT(IN) :: THU(*),VUZ(*)
    DOUBLE PRECISION, INTENT(INOUT) :: DSTEST,Q
    DOUBLE PRECISION, INTENT(INOUT) :: AA(AP%NDIM+1,AP%NDIM+1) 
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*),U(AP%NDIM+1)
    DOUBLE PRECISION, INTENT(INOUT) :: UDOT(AP%NDIM+1)
    CHARACTER(LEN=*), INTENT(OUT) :: ATYPE
    LOGICAL, INTENT(OUT) :: STEPPED

    INTEGER I,IID,ITMX,IBR,ITLCSP,NTOT,NITS
    DOUBLE PRECISION DS,DSMAX,EPSS,Q0,Q1,DQ,S0,S1,RDS,RRDS
    DOUBLE PRECISION DETS,DSTESTS
    DOUBLE PRECISION, ALLOCATABLE :: US(:),UDOTS(:),AAS(:,:)
    CHARACTER(4) ATYPEDUM

    IID=AP%IID
    ITMX=AP%ITMX
    IBR=AP%IBR
    NTOT=AP%NTOT

    DS=AP%DS
    DSMAX=AP%DSMAX
    EPSS=AP%EPSS

    STEPPED=.FALSE.

! Check whether FNCS has changed sign.

    Q0=Q
    Q1=FNCS(AP,ICP,U,PAR,ATYPE,IUZ,VUZ,ITEST,FNCI)
    ! disable detected potential bifurcations without stability changes
    I=LEN_TRIM(ATYPE)
    IF(ATYPE(I:I)=='0')ATYPE=''

    IF(AP%ITP/=0.AND.ABS((1.d0+HMACH)*Q1*DSTEST) < &
         EPSS*(1+SQRT(ABS(DS*DSMAX)))*ABS(Q0-Q1))THEN
       ! there could be multiple test functions going through zero
       ! at a point, for instance CP/LP and BP/CP.
       ! In general, use "first come, first served", but
       ! bifurcations override UZ, and CP overrides LP.
       IF((MOD(AP%ITP,10)==-4.AND.LEN_TRIM(ATYPE)>0.AND.TRIM(ATYPE)/='UZ').OR. &
          (AP%ITP==22.AND.TRIM(ATYPE)=='CP'))THEN
          Q=0.d0
          IF(IID>0)WRITE(9,102)RDS
          RETURN
       ENDIF
       Q1=0.d0
    ENDIF

    ! do not test via Q0*Q1 to avoid overflow.
    IF((Q0>=0.AND.Q1>=0) .OR. (Q0<=0.AND.Q1<=0) .OR. LEN_TRIM(ATYPE)==0)THEN
       ATYPE=''
       Q=Q1
       RETURN
    ENDIF

! Use the secant method for the first step:

    S0=0.d0
    S1=DSTEST
    DQ=Q0-Q1
    RDS=Q1/DQ*(S1-S0)

    RDS=(1.d0+HMACH)*RDS

! Return if relative tolerance has been met :

    RRDS=ABS(RDS)/(1+SQRT(ABS(DS*DSMAX)))
    IF(RRDS.LT.EPSS)THEN
       Q=0.d0
       IF(IID>0)WRITE(9,102)RDS
       RETURN
    ENDIF

    ALLOCATE(US(AP%NDIM+1),UDOTS(AP%NDIM+1),AAS(AP%NDIM+1,AP%NDIM+1))
    ! save state to restore in case of non-convergence or
    ! "possible special point"
    US(:)=U(:)
    UDOTS(:)=UDOT(:)
    AAS(:,:)=AA(:,:)
    DETS=AP%DET
    DSTESTS=DSTEST
    NITS=NIT

    DO ITLCSP=0,ITMX

! If requested write additional output on unit 9 :

       IF(IID.GE.2)THEN
          WRITE(9,101)ITLCSP,RDS
       ENDIF

       CALL STEPAE(AP,PAR,ICP,FUNI,RDS,AA,U,UDOT,THU,NIT)
       IF(NIT==0)EXIT

       STEPPED=.TRUE.

       CALL PVLI(AP,ICP,U,AP%NDIM,PAR,FNCI)

       Q=FNCS(AP,ICP,U,PAR,ATYPE,IUZ,VUZ,ITEST,FNCI)
       ! ignore stability changes
       I=LEN_TRIM(ATYPE)
       IF(ATYPE(I:I)=='0')ATYPE=ATYPE(1:I-1)

!        Use Mueller's method with bracketing for subsequent steps
       DSTEST=S1+RDS
       CALL MUELLER(Q0,Q1,Q,S0,S1,DSTEST,RDS)

       RDS=(1.d0+HMACH)*RDS

! Return if relative tolerance has been met :

       RRDS=ABS(RDS)/(1+SQRT(ABS(DS*DSMAX)))
       IF(RRDS.LT.EPSS)THEN
          Q=0.d0
          IF(IID>0)WRITE(9,102)RDS
          DEALLOCATE(US,UDOTS,AAS)
          STEPPED=.TRUE.
          RETURN
       ENDIF

    ENDDO

    IF(IID>0)WRITE(9,103)IBR,MOD(NTOT-1,9999)+1
    ATYPE=''
    ! set back to previous (converged) state
    U(:)=US(:)
    UDOT(:)=UDOTS(:)
    AA(:,:)=AAS(:,:)
    DO I=1,AP%NFPR
       PAR(ICP(I))=U(AP%NDIM+2-I)
    ENDDO
    AP%DET=DETS
    NIT=NITS
    DSTEST=DSTESTS
    CALL PVLI(AP,ICP,U,AP%NDIM,PAR,FNCI)
    Q=FNCS(AP,ICP,U,PAR,ATYPEDUM,IUZ,VUZ,ITEST,FNCI)
    DEALLOCATE(US,UDOTS,AAS)

101 FORMAT(' ==> Location of special point :  Iteration ',I3, &
         '  Step size = ',ES13.5)
102 FORMAT(' ==> Location of special point : ', &
         ' Convergence.   Step size = ',ES13.5)
103 FORMAT(I4,I6,' NOTE:Possible special point')
  END SUBROUTINE LCSPAE

!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                   Branch Switching for Algebraic Problems
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! ---------- -----
  SUBROUTINE STBIF(NDIM,NBIF,NBIFS,STUD,STU,U,UDOT)

    USE SUPPORT

! Stores branching data in the following arrays :
!        STU    ( the solution vector U | PAR(ICP(1)) )
!        STUD   ( U-dot | PAR(ICP(1))-dot )
! Here the vector ( U-dot, PAR(ICP(1))-dot ) is the direction vector of
! known branch at this point.

    INTEGER, INTENT(IN) :: NDIM,NBIFS
    INTEGER, INTENT(INOUT) :: NBIF
    DOUBLE PRECISION, INTENT(OUT) :: STUD(NBIFS,NDIM+1),STU(NBIFS,NDIM+1)
    DOUBLE PRECISION, INTENT(IN) :: U(NDIM+1),UDOT(NDIM+1)

! Keep track of the number of branch points stored.

    IF(NBIF==NBIFS)RETURN
    NBIF=NBIF+1

    STU(NBIF,:)=U(:)
    STUD(NBIF,:)=UDOT(:)

  END SUBROUTINE STBIF

! ---------- -----
  SUBROUTINE SWPNT(AP,DS,PAR,ICP,RDS,NBIF,NBIFS,STUD,STU,U,UDOT,IPOS)

! This subroutine retrieves the branching data U, U-dot, PAR(ICP(1)),
! PAR(ICP(1))-dot. If this initialization corresponds to the computation
! of the bifurcating branch in opposite direction, then only the sign of
!  the stepsize ( DS ) along the branch is reversed.

    TYPE(AUTOPARAMETERS) AP
    INTEGER ICP(*)
    INTEGER, INTENT(IN) :: NBIF,NBIFS
    DOUBLE PRECISION, INTENT(IN) :: DS
    DOUBLE PRECISION, INTENT(OUT) :: RDS
    DOUBLE PRECISION PAR(*),U(*),UDOT(*),STUD(NBIFS,*),STU(NBIFS,*)
    LOGICAL IPOS

    INTEGER NDIM,ISW,MXBF,I,I1

    NDIM=AP%NDIM
    ISW=AP%ISW
    MXBF=AP%MXBF

    RDS=DS
    IF(.NOT.IPOS)RDS=-DS
    DO I=1,NDIM+1
       U(I)=STU(1,I)
       UDOT(I)=STUD(1,I)
    ENDDO
    PAR(ICP(1))=U(NDIM+1)
    IF(ABS(ISW).EQ.2)PAR(ICP(2))=U(NDIM)

    IF(MXBF.GE.0)THEN
       IPOS=.NOT.IPOS
    ENDIF
    IF(.NOT.IPOS)RETURN

    DO I=1,NBIF-1
       DO I1=1,NDIM+1
          STU(I,I1)=STU(I+1,I1)
          STUD(I,I1)=STUD(I+1,I1)
       ENDDO
    ENDDO

  END SUBROUTINE SWPNT

! ---------- ------
  SUBROUTINE STPLAE(AP,PAR,ICP,ICU,U,UDOT,NIT,ISTOP)

    USE IO
    USE SUPPORT

! Stores the bifurcation diagram on unit 7 (Algebraic Problems).
! Every line written contains, in order, the following:
!
!  IBR    : The label of the branch.
!  NTOT   : The index of the point on the branch.
!           (Points are numbered consecutively along a branch).
!           If IPS=1, 11, or -1, then the sign of NTOT indicates stability :
!            - = stable , + = unstable, unknown, or not relevant.
!  ITP    : An integer indicating the type of point :
!
!             1  (BP)  :   Branch point.
!             2  (LP)  :   Fold.
!             3  (HB)  :   Hopf bifurcation point.
!             4  (  )  :   Output point (Every NPR steps along branch).
!            -4  (UZ)  :   Output point (Zero of user function).
!             9  (EP)  :   End point of branch, normal termination.
!            -9  (MX)  :   End point of branch, abnormal termination.
!
!  LAB        : The label of a special point.
!  PAR(ICP(1)): The principal parameter.
!  A          : The L2-norm of the solution vector, or other measure of
!               the solution (see the user-supplied parameter IPLT).
!  U          : The first few components of the solution vector.
!  PAR(ICP(*)): Further free parameters (if any).
!
    TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
    INTEGER, INTENT(IN) :: ICP(*),ICU(*),NIT
    DOUBLE PRECISION, INTENT(IN) :: U(*),UDOT(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
    LOGICAL, INTENT(INOUT) :: ISTOP

    INTEGER NDIM,IPS,ISW,IPLT,NMX,NPR,NDM,ITP,ITPST,IBR
    INTEGER NTOT,NTOTS,IAB,LABW,NINS
    DOUBLE PRECISION RL0,RL1,A0,A1,AMP

    NDIM=AP%NDIM
    IPS=AP%IPS
    ISW=AP%ISW
    IPLT=AP%IPLT
    NMX=AP%NMX
    NPR=AP%NPR
    NDM=AP%NDM
    ITP=AP%ITP
    ITPST=AP%ITPST
    IBR=AP%IBR

    RL0=AP%RL0
    RL1=AP%RL1
    A0=AP%A0
    A1=AP%A1

    NTOT=AP%NTOT
    NTOT=NTOT+1
    AP%NTOT=NTOT

! ITP is set to 4 every NPR steps along a branch, and the entire
! solution is written on unit 8.

    IF(NPR.NE.0)THEN
       IF(MOD(NTOT,NPR).EQ.0 .AND. MOD(ITP,10).EQ.0)ITP=4+10*ITPST
       AP%ITP=ITP
    ENDIF

! CHECK WHETHER LIMITS OF THE BIFURCATION DIAGRAM HAVE BEEN REACHED :

    IAB=ABS(IPLT)

    IF(IAB.LE.NDIM .AND. IAB.GT.0)THEN
       AMP=U(IAB)
    ELSE IF(IPLT.GT.NDIM.AND.IPLT.LE.2*NDIM)THEN
       AMP=U(IPLT-NDIM)
    ELSE IF(IPLT.GT.2*NDIM.AND.IPLT.LE.3*NDIM)THEN
       AMP=U(IPLT-2*NDIM)
    ELSE
       AMP=RNRMV(NDM,U)
    ENDIF

    IF(NIT==0)THEN
!        Maximum number of iterations reached somewhere.
       ISTOP=.TRUE.
       ITP=-9-10*ITPST
       AP%ITP=ITP
    ELSEIF(U(NDIM+1).LT.RL0.OR.U(NDIM+1).GT.RL1 &
         .OR. AMP.LT.A0.OR.AMP.GT.A1 &
         .OR. NTOT.EQ.NMX) THEN
       ISTOP=.TRUE.
       ITP=9+10*ITPST
       AP%ITP=ITP
    ENDIF

    LABW=0
    IF(MOD(ITP,10).NE.0)THEN
       LABW=AP%LAB
    ENDIF

! Determine stability and print output on units 6 and 7.

    NTOTS=NTOT
    NINS=AP%NINS
    IF((ABS(IPS)==1.OR.IPS==11) .AND. ABS(ISW)<=1)THEN
       IF(NINS.EQ.NDIM)NTOTS=-NTOT
    ENDIF
    CALL WRLINE(AP,PAR,ICU,IBR,NTOTS,LABW,AMP,U)

! Write restart information for multi-parameter analysis :

    IF(LABW.NE.0)THEN
       CALL WRTSP8(AP,PAR,ICP,LABW,U,UDOT)
       AP%LAB=AP%LAB+1
    ENDIF
!
  END SUBROUTINE STPLAE

! ---------- ------
  SUBROUTINE WRTSP8(AP,PAR,ICP,LAB,U,UDOT)

    USE COMPAT
    USE SUPPORT, ONLY: DIRECTION
    USE AUTO_CONSTANTS, ONLY: IPS, NDIMU => NDIM

! Write restart information on singular points, plotting points, etc.,
! on unit 8.

    TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
    INTEGER, INTENT(IN) :: ICP(*),LAB
    DOUBLE PRECISION, INTENT(IN) :: U(*),UDOT(*)
    DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

    INTEGER NDIM,ISW,ITP,IBR,NFPR,NPAR,NTOT,NROWPR,MTOT,NAR,NTPL,I,K
    INTEGER NPARI,NTST
    DOUBLE PRECISION T
    LOGICAL DIR

    NDIM=AP%NDIM
    ISW=AP%ISW
    NPARI=AP%NPARI
    ITP=AP%ITP
    IBR=AP%IBR
    NFPR=AP%NFPR
    NPAR=AP%NPAR
    NTOT=AP%NTOT

    NTPL=1
    NAR=NDIM+1

! skip direction info based on IIS and ITP
    DIR=DIRECTION(AP%IIS,ITP)
    NROWPR=NDIM/7 + 1 + (NPAR+6)/7
    NTST=0
    IF(DIR)THEN
       NROWPR=NROWPR + (NFPR+19)/20 + (NFPR+6)/7 + (NDIM+6)/7
       NTST=1
    ENDIF
    PAR(ICP(1))=U(NDIM+1)
    T=0.d0

    MTOT=MOD(NTOT-1,9999)+1
    WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,NTST,0,NPAR,&
         NPARI,NDIMU,IPS,0
    WRITE(8,102)T,(U(I),I=1,NDIM)
    IF(DIR)THEN
! Write the free parameter indices:
       WRITE(8,103)(ICP(I),I=1,NFPR)
! Write the direction of the branch:
       WRITE(8,102)UDOT(NDIM+1),(UDOT(NDIM-NFPR+I),I=2,NFPR)
       WRITE(8,102)(UDOT(K),K=1,NDIM)
    ENDIF
! Write the parameter values.
    WRITE(8,102)(PAR(I),I=1,NPAR)

101 FORMAT(6I6,I8,I6,I8,7I5)
102 FORMAT(4X,7ES19.10)
103 FORMAT(20I5)

    CALL AUTOFLUSH(8)
  END SUBROUTINE WRTSP8

! ---------- ------
  SUBROUTINE WRJAC(M,N,AA,RHS)

    INTEGER, INTENT(IN) :: M,N
    DOUBLE PRECISION, INTENT(IN) :: AA(M,N),RHS(M)
    INTEGER I,J

    WRITE(9,"(A)")' Residual vector :'
    WRITE(9,"(1X,12E10.3)")RHS(:),(0d0,I=M+1,N)
    WRITE(9,"(A)")' Jacobian matrix :'
    DO I=1,M
       WRITE(9,"(1X,12E10.3)")(AA(I,J),J=1,N)
    ENDDO
    DO I=M+1,N
       WRITE(9,"(1X,12E10.3)")(0d0,J=1,N)
    ENDDO

  END SUBROUTINE WRJAC

END MODULE AE
