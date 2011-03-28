!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!           Setting up of the Jacobian and right hand side
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      MODULE SOLVEBV

      USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

      IMPLICIT NONE
      PRIVATE

      PUBLIC ::SOLVBV

      INTERFACE
         INTEGER FUNCTION IDAMAX(N,DX,INCX)
            INTEGER, INTENT(IN) :: N,INCX
            DOUBLE PRECISION, INTENT(IN) :: DX(*)
         END FUNCTION IDAMAX
      END INTERFACE

      CONTAINS

!     ---------- ------
      SUBROUTINE SOLVBV(IFST,AP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDS,  &
       NLLV,RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM, &
       DUPS,DRL,P0,P1,THL,THU)

!$    USE OMP_LIB
      USE AUTOMPI
      USE SUPPORT, ONLY: AUTOSTOP

! Sets up and solves the linear equations for one Newton/Chord iteration

      include 'interfaces.h'
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER IFST,NLLV,ICP(*),NDIM
      DOUBLE PRECISION, INTENT(OUT) :: DET
      DOUBLE PRECISION PAR(*),RDS
      DOUBLE PRECISION RLOLD(AP%NFPR),RLCUR(AP%NFPR),RLDOT(AP%NFPR)
      DOUBLE PRECISION UPS(NDIM,0:*),UDOTPS(NDIM,0:*),UOLDPS(NDIM,0:*)
      DOUBLE PRECISION UPOLDP(NDIM,0:*),DUPS(NDIM,0:*),DTM(*)
      DOUBLE PRECISION DRL(AP%NFPR)
      DOUBLE PRECISION, INTENT(OUT) :: P0(*),P1(*)
      DOUBLE PRECISION THL(*),THU(*)

! Local
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: &
           A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:), &
           S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:),C2(:,:,:),     &
           CDBC(:,:),DD(:,:,:)
      INTEGER, ALLOCATABLE, SAVE :: &
           ICF(:,:),IRF(:,:),IPR(:,:),IPC(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: &
           FCFC(:,:),FAA(:,:),SOL(:,:),FC(:)
      INTEGER, ALLOCATABLE :: NP(:)
      INTEGER IAM,KWT,NTST,NCOL,NBC,NINT,IID,NFPR,NPAR
      INTEGER NRC,NFC,NROW,NCLM
      INTEGER NA,IT,NT,MNT,I,BASE
      INTEGER ISHAPE(8)

! Most of the required memory is allocated below

! This is an interesting section of code.  The main point
! is that setubv and conpar only get called when ifst
! is 1.  This is a optimization since you can solve
! the system using the previously factored jacobian.
! One thing to watch out for is that two seperate calls
! of solvbv_ talk to each other through these arrays,
! so it is only safe to get rid of them when ifst is
! 1 (since their entries will then be recreated in conpar
!  and setubv).


      IAM=MPIIAM()
      KWT=MPIKWT()

      NDIM=AP%NDIM
      NTST=AP%NTST
      NCOL=AP%NCOL
      NBC=AP%NBC
      NINT=AP%NINT
      IID=AP%IID
      NFPR=AP%NFPR
      NPAR=AP%NPAR
      NRC=NINT+1
      NFC=NRC+NBC
      NROW=NDIM*NCOL
      NCLM=NROW+NDIM

      ALLOCATE(NP(KWT))
      IF(KWT.GT.NTST)THEN
        PRINT*,'NTST is less than the number of nodes'
        CALL AUTOSTOP()
      ELSE
        CALL PARTITION(NTST,KWT,NP)
      ENDIF

!     NTST is the global one, NA is the local one.
!     The value of NTST may be different in different nodes.
      NA=NP(IAM+1)

      ALLOCATE(FC(NFC))
      MNT = 1
!$    MNT = OMP_GET_MAX_THREADS()
      IF(MNT.GT.NA)THEN
         MNT=NA
!$       CALL OMP_SET_NUM_THREADS(NA)
      ENDIF
      IF(IFST.EQ.1)THEN
         IF(ALLOCATED(A))THEN
!            !a sufficient check to see if array dimensions have changed:
            ISHAPE(1:3)=SHAPE(A)
            ISHAPE(4:6)=SHAPE(CC)
            ISHAPE(7:8)=SHAPE(CDBC)
            IF(ISHAPE(1)/=NCLM.OR.ISHAPE(2)/=NROW.OR.ISHAPE(3)/=NA    &
           .OR.ISHAPE(4)/=NDIM.OR.ISHAPE(5)/=NRC.OR.ISHAPE(6)/=NTST &
           .OR.ISHAPE(7)/=2*NDIM+NFPR.OR.ISHAPE(8)/=NBC)THEN
!              Free floating point arrays
               DEALLOCATE(A,B,C,D,A1,A2,S1,S2,BB,CC,C2,CDBC,DD)
!              Free integer arrays
               DEALLOCATE(ICF,IRF,IPR,IPC)
            ENDIF
         ENDIF

         IF(.NOT.ALLOCATED(A))THEN
            ALLOCATE(A(NCLM,NROW,NA),B(NFPR,NROW,NA))
            ALLOCATE(C(NCLM,NRC,NA),D(NFPR,NRC))
            ALLOCATE(A1(NDIM,NDIM,NTST),A2(NDIM,NDIM,NTST))
            ALLOCATE(S1(NDIM,NDIM,NTST-1),S2(NDIM,NDIM,NTST-1))
            ALLOCATE(BB(NFPR,NDIM,NTST),CC(NDIM,NRC,NTST))
            ALLOCATE(C2(NDIM,NRC,NTST),CDBC(2*NDIM+NFPR,NBC))
            ALLOCATE(DD(NFPR,NRC,NTST))

            ALLOCATE(ICF(NCLM,NA),IRF(NROW,NA),IPR(NDIM,NTST-1))
            ALLOCATE(IPC(NDIM,NTST-1))
         ENDIF
      ENDIF
      IF(IAM.EQ.0)THEN

         DO I=1,NFPR
            PAR(ICP(I))=RLCUR(I)
         ENDDO
         CALL SUBVBC(NDIM,NTST*NCOL,NBC,NFPR,BCNI, &
                 AP,PAR,NPAR,ICP,CDBC,FC,UPS,IFST)
         CALL SETFCDD(IFST,D,FC(NBC+1),NFPR,NINT)
         CALL SUBVPSA(NFPR,RDS,D(1,NRC),FC(NFC),RLCUR,RLOLD,RLDOT,THL,IFST)
         IF(KWT.GT.1)THEN
            CALL MPISBV(AP,PAR,ICP,NDIM,UPS,UOLDPS,RLOLD,UDOTPS, &
                 UPOLDP,DTM,THU,IFST,NLLV)
         ENDIF
      ENDIF
!     The matrices D and FC are unused in all nodes except the first.

      ALLOCATE(FCFC(NRC,NTST),FAA(NDIM,NTST),SOL(NDIM,NTST+1))
      BASE=(IAM*NTST+KWT-1)/KWT

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,IT,NT)

      IT = 0
!$    IT = OMP_GET_THREAD_NUM()
      NT = 1
!$    NT = OMP_GET_NUM_THREADS()
      IF(NLLV>=0.OR.IFST==1) &
        CALL SETUBV(NDIM,NA,NCOL,NINT,NFPR,NRC,NROW,NCLM,                &
         FUNI,ICNI,AP,PAR,NPAR,ICP,A,B,C,DD(1,1,BASE+1),DUPS,            &
         FCFC(1,BASE+1),UPS,UOLDPS,RLOLD,UDOTPS,UPOLDP,DTM,THU,          &
         IFST,IAM,IT,NT,IRF,ICF,IID,NLLV)

      I = (IT*NA+NT-1)/NT+1
      CALL BRBD(A(1,1,I),B(1,1,I),C(1,1,I),D,DD,DUPS(1,(I-1)*NCOL),FAA,FC,&
        FCFC,P0,P1,IFST,IID,NLLV,DET,NDIM,NTST,NA,NBC,NROW,NCLM,         &
        NFPR,NFC,A1,A2,BB,CC,C2,CDBC,                                    &
        SOL,S1,S2,IPR,IPC,IRF(1,I),ICF(1,I),IAM,KWT,IT,NT)

!$OMP END PARALLEL

      DEALLOCATE(FCFC,FAA,SOL)

      IF(KWT.GT.1)THEN
!        Global concatenation of the solution from each node.
        CALL MPIGAT(DUPS,NDIM*NCOL,NTST)
      ENDIF

      IF(IAM.EQ.0)THEN
         DUPS(:,NTST*NCOL)=FC(1:NDIM)
         DRL(:)=FC(NDIM+1:)
      ENDIF

      DEALLOCATE(NP,FC)
      END SUBROUTINE SOLVBV

!     ---------- -------
      SUBROUTINE SETFCDD(IFST,DD,FC,NCB,NRC)

      INTEGER, INTENT(IN) :: IFST,NCB,NRC
      DOUBLE PRECISION FC(*),DD(NCB,*)

      INTEGER I,J

      DO I=1,NRC
        IF(IFST.EQ.1)THEN
          DO J=1,NCB
            DD(J,I)=0.0D0
          ENDDO
        ENDIF
        FC(I)=0.0D0
      ENDDO

      END SUBROUTINE SETFCDD

!     ---------- ---------
      SUBROUTINE SUBVBC(NDIM,NTNC,NBC,NCB,BCNI, &
       AP,PAR,NPAR,ICP,CDBC,FC,UPS,IFST)

!     This subroutine handles a non-parallel part of SETUBV, that is,
!     * the boundary conditions (not much to parallelize here and
!       HomCont relies on non-parallel execution): the array CDBC
!       and parts of FC.

      include 'interfaces.h'

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER NDIM,NTNC,NBC,NCB,ICP(*),IFST,NPAR
      DOUBLE PRECISION CDBC(2*NDIM+NCB,NBC)
      DOUBLE PRECISION UPS(NDIM,0:NTNC),FC(*),PAR(*)

! Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,K

      ALLOCATE(UBC0(NDIM),UBC1(NDIM),FBC(NBC),DBC(NBC,2*NDIM+NPAR))
      DBC(:,:)=0.d0

!     Boundary conditions :

       IF(NBC.GT.0)THEN
         DO I=1,NDIM
            UBC0(I)=UPS(I,0)
            UBC1(I)=UPS(I,NTNC)
         ENDDO
         CALL BCNI(AP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,IFST*2,DBC)
         DO I=1,NBC
            FC(I)=-FBC(I)
            IF(IFST.EQ.1)THEN
               DO K=1,NDIM
                  CDBC(K,I)=DBC(I,K)
                  CDBC(NDIM+K,I)=DBC(I,NDIM+K)
               ENDDO
               DO K=1,NCB
                  CDBC(2*NDIM+K,I)=DBC(I,2*NDIM+ICP(K))
               ENDDO
            ENDIF
         ENDDO    
       ENDIF
       DEALLOCATE(UBC0,UBC1,FBC,DBC)
       END SUBROUTINE SUBVBC

!     ---------- -------
      SUBROUTINE SUBVPSA(NCB,RDS,DDPA,FCPA,RLCUR,RLOLD,RLDOT,THL,IFST)

      USE MESH

!     This subroutine handles a non-parallel part of SETUBV, that is,
!     * creating the parameter dependent pseudo-arclength parts of FC and D:
!       (the bottom element FCPA and row DDPA)

      INTEGER, INTENT(IN) :: NCB,IFST
      DOUBLE PRECISION, INTENT(IN) :: RDS,THL(NCB)
      DOUBLE PRECISION, INTENT(IN) :: RLCUR(NCB),RLOLD(NCB),RLDOT(NCB)
      DOUBLE PRECISION, INTENT(OUT) :: DDPA(NCB),FCPA

! Local
      INTEGER I
      DOUBLE PRECISION RLSUM

!     Pseudo-arclength equation :

       RLSUM=0.d0
       DO I=1,NCB
          IF(IFST.EQ.1)THEN
             DDPA(I)=THL(I)*RLDOT(I)
          ENDIF
          RLSUM=RLSUM+THL(I)*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
       FCPA=RDS-RLSUM

       END SUBROUTINE SUBVPSA

!     ---------- ------
      SUBROUTINE SETUBV(NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,         &
       ICNI,AP,PAR,NPAR,ICP,AA,BB,CC,DD,FA,FCFC,                        &
       UPS,UOLDPS,RLOLD,UDOTPS,UPOLDP,DTM,THU,IFST,IAM,IT,NT,IRF,ICF,IDB,NLLV)

      USE MESH

      INTEGER NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,IFST,IAM,IT,NT,NPAR
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER ICP(*),IRF(NRA,*),ICF(NCA,*),IDB,NLLV
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,NRC,*),FA(NRA,*),FCFC(NRC,*)
      DOUBLE PRECISION UPS(NDIM,0:*),UOLDPS(NDIM,0:*),RLOLD(NCB),UDOTPS(NDIM,0:*)
      DOUBLE PRECISION UPOLDP(NDIM,0:*),DTM(*),PAR(*),THU(*)

      include 'interfaces.h'

! Local
      DOUBLE PRECISION WI(0:NCOL),WP(0:NCOL,NCOL),WT(0:NCOL,NCOL)
      INTEGER I,N,II


      CALL WINT(NCOL,WI)
      CALL GENWTS(NCOL,WT,WP)

      I = (IT*NA+NT-1)/NT+1
      N = ((IT+1)*NA+NT-1)/NT+1-I
      II = (I-1)*NCOL
      CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,     &
           AP,PAR,NPAR,ICP,AA(1,1,I),BB(1,1,I),CC(1,1,I),         &
           DD(1,1,I),FA(1,I),FCFC(1,I),UPS(1,II),UOLDPS(1,II),    &
           RLOLD,UDOTPS(1,II),UPOLDP(1,II),DTM(I),THU,WI,WP,WT,   &
           IRF(1,I),ICF(1,I),IFST,NLLV)

      CONTAINS

!     ---------- ---------
      SUBROUTINE SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,  &
       ICNI,AP,PAR,NPAR,ICP,AA,BB,CC,DD,FA,FC,                  &
       UPS,UOLDPS,RLOLD,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT,IRF,ICF,IFST,NLLV)

!     This is the per-CPU parallelized part of SETUBV

      include 'interfaces.h'

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,ICP(*),NPAR
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,NRC,*),UPS(NDIM,0:*),UOLDPS(NDIM,0:*),RLOLD(NCB)
      DOUBLE PRECISION UDOTPS(NDIM,0:*),UPOLDP(NDIM,0:*),FA(NRA,*),FC(NRC,*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION WI(0:*),WP(0:NCOL,*),WT(0:NCOL,*)
      INTEGER IRF(NRA,*),ICF(NCA,*),IFST,NLLV

! Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U, &
        F,FICD,DICD,UID,UIP,PRM
      DOUBLE PRECISION WPLOC(0:NCOL)
      INTEGER I,J,JJ,K,IC,IC1,J1,K1,IB
      INTEGER, ALLOCATABLE :: IAMAX(:)

      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPAR),UOLD(2*NDIM+NCB),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT))
      ALLOCATE(DICD(NINT*(NDIM+NPAR)))
      ALLOCATE(UID(NDIM),UIP(NDIM),PRM(NPAR),IAMAX(NRA))

! Initialize to zero.

       DFDU(:)=0.d0
       DFDP(:)=0.d0
       DICD(:)=0.d0

       DO I=1,NPAR
          PRM(I)=PAR(I)
       ENDDO
       UOLD(:)=0.d0
       UOLD(NDIM+1:NDIM+NCB)=RLOLD(:)
       DO J=1,N

! Generate AA , BB and FA :

          DO IC=1,NCOL
             IC1=(IC-1)*NDIM+1
             JJ=(J-1)*NCOL
             DO IB=0,NCOL
                WPLOC(IB)=WP(IB,IC)/DTM(J)
             ENDDO
             CALL SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,AP,PRM,ICP,    &
                  AA(1,IC1,J),BB(1,IC1,J),FA(IC1,J),UPS(1,JJ), &
                  UOLDPS(1,JJ),UPOLDP(1,JJ),WPLOC,WT(0,IC),DFDU,DFDP, &
                  U,UOLD,F,IFST,NLLV)
             IF(IFST.EQ.1)THEN
                DO K=0,NDIM-1
                   IAMAX(IC1+K)=NDIM+IDAMAX(NRA-NDIM,AA(NDIM+1,IC1+K,J),1)
                ENDDO
             ENDIF
          ENDDO
!     
!     Generate CC, DD and FC :

         IF(IFST.EQ.1)DD(:,:,J)=0d0
         IF(NLLV.EQ.0)FC(:,J)=0d0
         DO K=0,NCOL
            J1=(J-1)*NCOL+K
            K1=K*NDIM+1
!     
!     Integral constraints+pseudo-arclength equation :
!     
            CALL SBVICN(NDIM,NINT,NCB,NCA,ICNI,AP,PRM,ICP,            &
                 CC(K1,1,J),DD(1,1,J),FC(1,J),UPS(1,J1),UOLDPS(1,J1), &
                 UDOTPS(1,J1),UPOLDP(1,J1),DTM(J),THU,WI(K),FICD,DICD,&
                 U,UOLD,UID,UIP,IFST,NLLV)
         ENDDO

!     debug: do condensation of parameters later after printing
         IF(IDB>4.AND.IAM==0)CYCLE

!      Condensation of parameters:
         IF(IFST.EQ.1)THEN
            CALL CONPAR(NDIM,NRA,NCA,AA(1,1,J),NCB,BB(1,1,J),NRC,      &
                 CC(1,1,J),DD(1,1,J),FA(1,J),FC(1,J),IRF(1,J),ICF(1,J),&
                 IAMAX,NLLV)
         ELSEIF(NLLV==0)THEN
            CALL CONRHS(NDIM,NRA,NCA,AA(1,1,J),NRC,                   &
                 CC(1,1,J),FA(1,J),FC(1,J),IRF(1,J))
         ENDIF
       ENDDO
!     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD)
       DEALLOCATE(UID,UIP,PRM,IAMAX)
      END SUBROUTINE SUBVPA

!     ---------- ---------
      SUBROUTINE SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,AP,PAR,ICP,            &
       AA,BB,FA,UPS,UOLDPS,UPOLDP,WPLOC,WT,DFDU,DFDP,U,UOLD,F,IFST,NLLV)

!     Does one call to FUNI and stores the result in AA, BB, and FA.

      include 'interfaces.h'

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,NCOL,NCB,NCA,ICP(*)
      INTEGER, INTENT(IN) :: IFST,NLLV
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: WT(0:NCOL),WPLOC(0:NCOL)
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NCOL)
      DOUBLE PRECISION, INTENT(IN) :: UOLDPS(NDIM,0:NCOL),UPOLDP(NDIM,0:NCOL)
      DOUBLE PRECISION, INTENT(OUT) :: AA(NCA,*),BB(NCB,*),FA(*),U(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UOLD(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*)

! Local
      DOUBLE PRECISION WTTMP
      INTEGER I,IB,IB1,J,K

      DO K=1,NDIM
         U(K)=DOT_PRODUCT(WT(:),UPS(K,:))
         UOLD(K)=DOT_PRODUCT(WT(:),UOLDPS(K,:))
         UOLD(NDIM+NCB+K)=DOT_PRODUCT(WT(:),UPOLDP(K,:))
      ENDDO
      CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,IFST*2,F,DFDU,DFDP)
      IF(IFST.EQ.1)THEN
         DO I=1,NDIM
!     use U instead of DFDU in inner loop to better utilize the CPU cache
            DO J=1,NDIM
               U(J)=DFDU(I,J)
            ENDDO
            DO IB=0,NCOL
               WTTMP=-WT(IB)
               IB1=IB*NDIM
               DO K=1,NDIM
                  AA(IB1+K,I)=WTTMP*U(K)
               ENDDO
               AA(IB1+I,I)=AA(IB1+I,I)+WPLOC(IB)
            ENDDO
         ENDDO
         DO I=1,NDIM
            DO K=1,NCB
               BB(K,I)=-DFDP(I,ICP(K))
            ENDDO
         ENDDO
      ENDIF
      IF(NLLV.EQ.0)THEN
         DO I=1,NDIM
            FA(I)=F(I)-DOT_PRODUCT(WPLOC(:),UPS(I,:))
         ENDDO
      ENDIF
      END SUBROUTINE SBVFUN

!     ---------- ------
      SUBROUTINE SBVICN(NDIM,NINT,NCB,NCA,ICNI,AP,PAR,ICP,CC,DD,FC,   &
       UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,FICD,DICD,UIC,UIO,UID,UIP, &
       IFST,NLLV)

!     Does one call to ICNI (integral constraints) and stores the
!     result in CC, DD and FC; and stores the pseudo-arclength
!     result too.

      include 'interfaces.h'
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,NINT,NCB,NCA,ICP(*),IFST,NLLV
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: UPS(*),UDOTPS(*)
      DOUBLE PRECISION, INTENT(IN) :: UOLDPS(*),UPOLDP(*),DTM,WI,THU(*)
      DOUBLE PRECISION, INTENT(OUT) :: CC(NCA,*),FICD(*),DICD(NINT,*)
      DOUBLE PRECISION, INTENT(OUT) :: UIC(*),UID(*),UIP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UIO(*),DD(NCB,*),FC(*)

! Local
      INTEGER I,M
      DOUBLE PRECISION DFCDU

      IF(NINT.GT.0)THEN
         DO I=1,NDIM
            UIC(I)=UPS(I)
            UIO(I)=UOLDPS(I)
            UID(I)=UDOTPS(I)
            UIP(I)=UPOLDP(I)
         ENDDO
         CALL ICNI(AP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,FICD,IFST*2,DICD)
         DO M=1,NINT
            IF(IFST.EQ.1)THEN
               DO I=1,NDIM
                  CC(I,M)=DTM*WI*DICD(M,I)
               ENDDO
               DO I=1,NCB
                  DD(I,M)=DD(I,M)+DTM*WI*DICD(M,NDIM+ICP(I))
               ENDDO
            ENDIF
            IF(NLLV.EQ.0)THEN
               FC(M)=FC(M)-DTM*WI*FICD(M)
            ENDIF
         ENDDO
      ENDIF
!     
!     Pseudo-arclength equation :
!     
      DO I=1,NDIM
         DFCDU=DTM*THU(I)*WI*UDOTPS(I)
         IF(IFST.EQ.1)THEN
            CC(I,NINT+1)=DFCDU
         ENDIF
         IF(NLLV.EQ.0)THEN
            FC(NINT+1)=FC(NINT+1)-DFCDU*(UPS(I)-UOLDPS(I))
         ENDIF
      ENDDO
      END SUBROUTINE SBVICN

      END SUBROUTINE SETUBV

!     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,DD,FA,FAA,FC,FCFC,P0,P1,IFST,  &
        IDB,NLLV,DET,NOV,NTST,NA,NBC,NRA,NCA,                &
        NCB,NFC,A1,A2,BB,CC,C2,CDBC,                         &
        SOL,S1,S2,IPR,IPC,IRF,ICF,IAM,KWT,IT,NT)

! Solves linear systems with matrix profile:

!     -----------------------------------------------
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !XXXXXXXXXX                                !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !        XXXXXXXXXX                        !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                XXXXXXXXXX                !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                        XXXXXXXXXX        !XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     !                                XXXXXXXXXX!XX!
!     -----------------------------------------------
!     !XX                                      XX!XX!
!     !XX                                      XX!XX!
!     !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX!XX!
!     !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX!XX!
!     !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX!XX!
!     -----------------------------------------------


! partioned as


!      ---------   ------   ------
!      !     ! !   !    !   !    !
!      !  A  !B!   ! XA !   ! FA !
!      !     ! ! . !    ! = !    !   .
!      !-----!-!   !----!   !----!
!      !  CDBC !   !    !   !    !
!      !-----!-!   ! XC !   ! FC !
!      !  C  !D!   !    !   !    !
!      !-----!-!   !----!   !----!


! Input parameters :

!   NA    number of blocks in A,
!   NRA   number of rows in each block of A,
!   NCA   number of columns in each block of A,
!   A     the matrix in the schematic representation above,

!   NCB   number of columns in each block of B,
!         (note that B is also three dimensional),
!   B     the matrix in the schema above,

!   NRC   the number of rows of the two dimensional matrix C,
!   C     the matrix C in the schema above,

!   D      the matrix D above,

!   NFC   the number of rows of the two dimensional matrices C+CDBC, XC and FC
!   C     the matrix C in the schema above,

!   CDBC  the matrix CDBC above,
!   NBC   the number of rows of the two dimensional matrix CDBC

!   FA     part of the right hand side vector,
!          (note that FA is also two dimensional),
!   FC     part of the right hand side vector.

!   IFST   = 1 on first call,
!          = 0  on subsequent calls with the same right hand side.

!   IDB   = 0..2 no debug output,
!         = 3    output of the residuals of the reduced system on unit 9,
!         = 4    output of the reduced Jacobian matrix and solution vector
!                on unit 9,
!         = 5    output of most matrices on unit 9 (see PRINT1),

!   IPR, IPC, ICF: Integer arrays

!   NLLV : If NLLV>0 then a null vector will be returned.
!          If NLLV = -1 then the system will be solved with zero right
!          hand side, except for the last equation, for which the right
!          hand side entry will be set to 1 (i.e., the last entry of FC
!          will be set to 1, otherwise FA and FC are zero).
!          If the linear system is the same as in the preceding call
!          then IFST=0 may be used even if NLLV is nonzero.

! Returned values :

!   FA     Part of solution vector corresponding to XA in the diagram.
!   FC     Part of solution vector corresponding to XC in the diagram.

! Notes: The number of columns of overlap for every two consecutive
!        blocks should be equal to the number NOV (NDIM).
!        Parts of the reduction are done in SUBVPA.

! Arguments
      INTEGER, INTENT(IN) :: IFST,IDB,NLLV,NOV,NTST,NA,NBC,NRA
      INTEGER, INTENT(IN) :: NCA,NCB,NFC,IAM,KWT,IT,NT
      DOUBLE PRECISION, INTENT(OUT) :: DET
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DOUBLE PRECISION D(NCB,*),DD(NCB,NFC-NBC,*)
      DOUBLE PRECISION FA(NRA,*),FAA(NOV,*),FC(*),FCFC(NFC-NBC,*)
      DOUBLE PRECISION, INTENT(OUT) :: P0(*),P1(*)
      DOUBLE PRECISION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NFC-NBC,*),C2(NOV,NFC-NBC,*)
      DOUBLE PRECISION CDBC(*),SOL(NOV,*),S1(*),S2(*)
      INTEGER   IPR(*),IPC(*),IRF(NRA,*),ICF(NCA,*)

! Local
      INTEGER I,J,K,II,N,NRC,BASE
      INTEGER, ALLOCATABLE :: IAMAX(:)
      DOUBLE PRECISION, ALLOCATABLE :: FCC(:),E(:,:),X(:)

      NRC=NFC-NBC
      BASE=(IAM*NTST+KWT-1)/KWT
      I = BASE+(IT*NA+NT-1)/NT+1
      N = BASE+((IT+1)*NA+NT-1)/NT+1-I

      IF(IDB.GT.4.and.IAM.EQ.0)THEN
!$OMP BARRIER
!$OMP MASTER
         CALL PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CDBC,D,DD, &
           FA,FC,FCFC,IFST,NLLV)
!$OMP END MASTER
!$OMP BARRIER
         IF(IFST.EQ.1.OR.NLLV>=0)THEN
            ALLOCATE(IAMAX(NRA))
            DO J=1,N
               IF(IFST.EQ.1)THEN
                  DO K=1,NRA
                     IAMAX(K)= NOV+IDAMAX(NRA-NOV,A(NOV+1,K,J),1)
                  ENDDO
                  CALL CONPAR(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,   &
                       C(1,1,J),DD(1,1,I+J-1),FA(1,J),FCFC(1,I+J-1),   &
                       IRF(1,J),ICF(1,J),IAMAX,NLLV)
               ELSEIF(NLLV==0)THEN
                  CALL CONRHS(NOV,NRA,NCA,A(1,1,J),NRC,                &
                       C(1,1,J),FA(1,J),FCFC(1,I+J-1),IRF(1,J))
               ENDIF
            ENDDO
            DEALLOCATE(IAMAX)
         ENDIF
      ENDIF

      IF(IFST.EQ.1)THEN
         CALL COPYCP(N,NOV,NRA,NCA,A,NCB,B,NRC,C,A1(1,1,I),A2(1,1,I),   &
             BB(1,1,I),CC(1,1,I),C2(1,1,I))
      ENDIF
      IF(NLLV.NE.0)THEN
         FA(:,:N)=0d0
         IF(IT.EQ.0)THEN
            FC(:NFC)=0d0
         ENDIF         
      ENDIF
      CALL CPYRHS(N,NOV,NRA,FAA(1,I),FA)

      CALL REDUCE(A1,A2,BB,CC,C2,DD,FAA,FCFC,                           &
           NTST,NOV,NCB,NRC,S1,S2,IPC,IPR,IFST,NLLV,IT,NT,IAM,KWT)

! Solve the system generated by REDUCE
! by Gauss elimination with complete pivoting.

! REDUCE already has a barrier.
!$OMP MASTER
      IF(IAM.EQ.0)THEN
         ! This is where we sum into the global copy of the d array
         DO J=1,NRC
            IF(IFST.EQ.1)THEN
               DO K=1,NCB
                  D(K,J)=D(K,J)+DD(K,J,NTST)
               ENDDO
            ENDIF
            IF(NLLV.EQ.0)THEN
               FC(NBC+J)=FC(NBC+J)+FCFC(J,NTST)
            ENDIF
         ENDDO
         ALLOCATE(FCC(NOV+NFC),E(NOV+NFC,NOV+NFC))
         CALL DIMRGE(E,CC,C2,CDBC,D,FC,                               &
           NTST,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,A1,A2,FAA,BB)
         DO II=1,NOV
            SOL(II,1)=FCC(II)
         ENDDO
         DEALLOCATE(FCC,E)
      ENDIF
!$OMP END MASTER

! Backsubstitution in the reduction process.

      CALL BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,IT,NT,IAM,KWT)

! Backsubstitution in the condensation of parameters process.

      ALLOCATE(X(NOV+1:NRA))
      CALL INFPAR(A,B,FA,SOL(1,I),FC,N,NOV,NRA,NCA,NCB,ICF,X)
      DEALLOCATE(X)

      END SUBROUTINE BRBD

!     ---------- ------
      SUBROUTINE CONPAR(NOV,NRA,NCA,A,NCB,B,NRC,C,D,FA,FC,IRF,ICF,IAMAX,NLLV)

! Arguments
      INTEGER, INTENT(IN) :: NOV,NRA,NCA,NCB,NRC,NLLV
      INTEGER, INTENT(OUT) :: ICF(NCA),IRF(NRA)
      DOUBLE PRECISION, INTENT(INOUT) :: A(NCA,NRA),B(NCB,NRA)
      DOUBLE PRECISION, INTENT(INOUT) :: C(NCA,NRC),D(NCB,NRC)
      DOUBLE PRECISION, INTENT(INOUT) :: FA(NRA),FC(NRC)
      INTEGER, INTENT(INOUT) :: IAMAX(NRA)
! Local
      INTEGER IC,IRP,IR,IPIV,JPIV,L
      DOUBLE PRECISION PIV,TPIV,RM,TMP

! Note that the summation of the adjacent overlapped part of C
! is delayed until REDUCE, in order to merge it with other communications.

! This is a per-CPU, per-element process function for
! Condensation of parameters (Elimination of local variables).

         DO IC=NOV+1,NCA-NOV
            IRP=IC-NOV
!           **Search for pivot (Complete pivoting)
            PIV = ABS(A(IAMAX(IRP),IRP))
            IPIV = IRP
            DO IR=IRP+1,NRA
               TPIV = ABS(A(IAMAX(IR),IR))
               IF(PIV.LT.TPIV)THEN
                  PIV = TPIV
                  IPIV = IR
               ENDIF
            ENDDO
!           **Move indices
            IRF(IRP)=IPIV
            JPIV=IAMAX(IPIV)
            IF(IRP.NE.IPIV)THEN
!              **Physically swap rows
               DO L=1,NCA
                  TMP=A(L,IPIV)
                  A(L,IPIV)=A(L,IRP)
                  A(L,IRP)=TMP
               ENDDO
               DO L=1,NCB
                  TMP=B(L,IPIV)
                  B(L,IPIV)=B(L,IRP)
                  B(L,IRP)=TMP
               ENDDO
               TMP=FA(IPIV)
               FA(IPIV)=FA(IRP)
               FA(IRP)=TMP
               IAMAX(IPIV)=IAMAX(IRP)
            ENDIF
            ICF(IC)=JPIV
            IF(IC.NE.JPIV)THEN
!              **Physically swap columns
               DO IR=1,IRP-1
                  TMP=A(JPIV,IR)
                  A(JPIV,IR)=A(IC,IR)
                  A(IC,IR)=TMP
               ENDDO
            ENDIF
!           **End of pivoting; elimination starts here
            PIV=A(JPIV,IRP)
            A(JPIV,IRP)=A(IC,IRP)
            A(IC,IRP)=PIV
            DO IR=IRP+1,NRA
!              **Swap columns of A physically
               RM=A(JPIV,IR)/PIV
               A(JPIV,IR)=A(IC,IR)
               A(IC,IR)=RM
               IF(RM.NE.0.0)THEN
                  CALL IMSBRA(NOV,NCA,NRA,A(1,IR),A(1,IRP),IC+1,IAMAX(IR),RM)
                  DO L=1,NCB
                     B(L,IR)=B(L,IR)-RM*B(L,IRP)
                  ENDDO
                  IF(NLLV.EQ.0)THEN
                     FA(IR)=FA(IR)-RM*FA(IRP)
                  ENDIF
               ELSEIF(IAMAX(IR).EQ.JPIV)THEN
                  IAMAX(IR)=IC+IDAMAX(NRA-IC,A(IC+1,IR),1)
               ELSEIF(IAMAX(IR).EQ.IC)THEN
                  IAMAX(IR)=JPIV
               ENDIF
            ENDDO
            DO IR=1,NRC
!              **Swap columns of C physically
               RM=C(JPIV,IR)/PIV
               C(JPIV,IR)=C(IC,IR)
               C(IC,IR)=RM
               IF(RM.NE.0.0)THEN
                  CALL SUBRAC(NOV,NCA,C(1,IR),A(1,IRP),IC+1,RM)
                  DO L=1,NCB
                     D(L,IR)=D(L,IR)-RM*B(L,IRP)
                  ENDDO
                  IF(NLLV.EQ.0)THEN
                     FC(IR)=FC(IR)-RM*FA(IRP)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO

      CONTAINS

!     ---------- ------
      SUBROUTINE IMSBRA(NOV,NCA,NRA,A,AP,ICP1,IAMAX,RM)

! Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: NOV,NRA,NCA,ICP1
      INTEGER, INTENT(OUT) :: IAMAX
! Local
      INTEGER L
      DOUBLE PRECISION PPIV,TPIV,V

      DO L=1,NOV
         A(L)=A(L)-RM*AP(L)
      ENDDO
      PPIV=0d0
      IAMAX=ICP1
      DO L=ICP1,NRA
         V=A(L)-RM*AP(L)
!     Also recalculate absolute maximum for current row
         A(L)=V
         TPIV=DABS(V)
         IF(PPIV.LT.TPIV)THEN
            PPIV=TPIV
            IAMAX=L
         ENDIF
      ENDDO
      DO L=NCA-NOV+1,NCA
         A(L)=A(L)-RM*AP(L)
      ENDDO
      END SUBROUTINE IMSBRA

!     ---------- ------
      SUBROUTINE SUBRAC(NOV,NCA,C,AP,ICP1,RM)
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: C(*)
      INTEGER, INTENT(IN) :: NOV,NCA,ICP1
! Local
      INTEGER L

      DO L=1,NOV
         C(L)=C(L)-RM*AP(L)
      ENDDO
      DO L=ICP1,NCA
         C(L)=C(L)-RM*AP(L)
      ENDDO
      END SUBROUTINE SUBRAC

      END SUBROUTINE CONPAR

!     ---------- ------
      SUBROUTINE CONRHS(NOV,NRA,NCA,A,NRC,C,FA,FC,IRF)

! Arguments
      INTEGER   NOV,NRA,NCA
      INTEGER   NRC,IRF(NRA)
      DOUBLE PRECISION A(NCA,NRA),C(NCA,NRC)
      DOUBLE PRECISION FA(NRA),FC(*)

! Local
      INTEGER   IC,IR,IPIV,IRP
      DOUBLE PRECISION RM,TMP

! Condensation of right hand side (one element).

      DO IR=1,NRA-NOV
         IPIV=IRF(IR)
         IF(IR.NE.IPIV)THEN
!     **Physically swap rows
            TMP=FA(IPIV)
            FA(IPIV)=FA(IR)
            FA(IR)=TMP
         ENDIF
      ENDDO
      DO IC=NOV+1,NCA-NOV
         IRP=IC-NOV
         RM=FA(IRP)
         IF(RM.NE.0.0)THEN
            DO IR=IRP+1,NRA
               FA(IR)=FA(IR)-RM*A(IC,IR)
            ENDDO
            DO IR=1,NRC
               FC(IR)=FC(IR)-RM*C(IC,IR)
            ENDDO
         ENDIF
      ENDDO

      END SUBROUTINE CONRHS

!     ---------- ------
      SUBROUTINE COPYCP(NA,NOV,NRA,NCA,A,NCB,B,NRC,C,A1,A2,BB,CC,C2)

! Arguments
      INTEGER, INTENT(IN) :: NA,NOV,NRA,NCA
      INTEGER, INTENT(IN) :: NCB,NRC
      DOUBLE PRECISION, INTENT(IN) :: A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DOUBLE PRECISION, INTENT(OUT) :: A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION, INTENT(OUT) :: BB(NCB,NOV,*),CC(NOV,NRC,*),C2(NOV,NRC,*)

!     DOUBLE PRECISION FA(NRA,*),FAA(NOV,*)

! Local
      INTEGER   I,IR,IR1,IC,IC1

! Copies the condensed sytem generated by CONPAR into workspace.

      DO I=1,NA
         DO IR=1,NOV
            IR1=NRA-NOV+IR
            DO IC=1,NOV
               IC1=NCA-NOV+IC
               A1(IC,IR,I)=A(IC,IR1,I)
               A2(IC,IR,I)=A(IC1,IR1,I)
            ENDDO     
            DO IC=1,NCB
               BB(IC,IR,I)=B(IC,IR1,I)
            ENDDO
         ENDDO
         DO IR=1,NRC
            DO IC=1,NOV
               CC(IC,IR,I)=C(IC,IR,I)
               C2(IC,IR,I)=C(NRA+IC,IR,I)
            ENDDO
         ENDDO
      ENDDO

      END SUBROUTINE COPYCP

!     ---------- ------
      SUBROUTINE CPYRHS(NA,NOV,NRA,FAA,FA)

      USE AUTOMPI

! Arguments
      INTEGER   NA,NOV,NRA

      DOUBLE PRECISION FA(NRA,*),FAA(NOV,*)

! Local
      INTEGER   I,IR

!     **Copy the RHS
      DO I=1,NA
         DO IR=1,NOV
            FAA(IR,I)=FA(NRA-NOV+IR,I)
         ENDDO         
      ENDDO

      END SUBROUTINE CPYRHS

!     ---------- ------
      SUBROUTINE REDUCE(A1,A2,BB,CC,C2,DD,FAA,FCFC,                &
           NTST,NOV,NCB,NRC,S1,S2,IPC,IPR,IFST,NLLV,IT,NT,IAM,KWT)

      USE AUTOMPI

! Arguments
      INTEGER, INTENT(IN) :: NTST,NOV,NCB,NRC,IFST,NLLV,IT,NT,IAM,KWT
      INTEGER, INTENT(INOUT) :: IPC(NOV,*),IPR(NOV,*)
      DOUBLE PRECISION, INTENT(INOUT) :: A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION, INTENT(INOUT) :: BB(NCB,NOV,*)
      DOUBLE PRECISION, INTENT(OUT) :: S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION, INTENT(INOUT) :: CC(NOV,NRC,*),C2(NOV,NRC,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DD(NCB,NRC,*),FAA(NOV,*),FCFC(NRC,*)

! Local 
      INTEGER IAMAX,PLO,PHI,NA,MPLO,MPHI
      LOGICAL DOMPI
      ALLOCATABLE IAMAX(:)

      ALLOCATE(IAMAX(2*NOV))

      MPLO = (IAM*NTST+KWT-1)/KWT+1
      MPHI = ((IAM+1)*NTST+KWT-1)/KWT
      NA = MPHI-MPLO+1
      PLO = MPLO+(IT*NA+NT-1)/NT
      PHI = MPLO+((IT+1)*NA+NT-1)/NT-1
      DOMPI = KWT>1.AND.NT==1
!     Reduce non-overlapping pieces
      CALL REDUCER(1,NTST)

!$OMP BARRIER
!$OMP MASTER

!     Reduce overlapping pieces
      IF(NT>1)THEN
         DOMPI = KWT>1
         PLO = MPLO
         PHI = MPHI
         CALL REDUCER(1,NTST)
      ENDIF
!$OMP END MASTER

      DEALLOCATE(IAMAX)

      CONTAINS

!      --------- ---------- -------
       RECURSIVE SUBROUTINE REDUCER(LO,HI)

! Arguments
       INTEGER, INTENT(IN) :: LO,HI

! Local 
       INTEGER IR,IC,I0,I1,I2,MID

       IF(HI<PLO.OR.LO>PHI)RETURN
! This is a check for the master reduction so it will stop as soon
! as there is no more overlap (already handled by nodes).
       IF(NT>1.AND.PHI-PLO==NA-1.AND.LO>=PLO)THEN
          IF((LO-PLO)*NT/NA==(HI-PLO)*NT/NA)RETURN
       ENDIF

! Use nested dissection for reduction; this is naturally a recursive
! procedure.

       MID=(LO+HI)/2

       IF(LO<MID) &
            CALL REDUCER(LO,MID)

       IF(MID+1<HI) &
            CALL REDUCER(MID+1,HI)

       IF(DOMPI)THEN
          CALL MPIREDUCE(A1,A2,BB,CC,C2,DD,FAA,FCFC,NTST,NOV,NCB,NRC,IFST,&
               NLLV,LO,HI)
       ELSE
          ! OpenMP parallel section without overlap
          IF(NT>1.AND.PHI-PLO<NA-1.AND.HI>PHI)RETURN
       ENDIF
       IF(LO<PLO)RETURN

! Initialization

       I0=LO
       I1=MID
       I2=HI
       IF(NLLV==0)THEN
          DO IR=1,NRC
             FCFC(IR,I2)=FCFC(IR,I2)+FCFC(IR,I1)
          ENDDO
       ENDIF
       IF(IFST.EQ.1)THEN
          DO IR=1,NOV
             DO IC=1,NOV
                S1(IC,IR,I1)=A1(IC,IR,I0)
             ENDDO
          ENDDO
          DO IR=1,NRC
             DO IC=1,NOV
                CC(IC,IR,I1+1)=CC(IC,IR,I1+1)+C2(IC,IR,I1)
             ENDDO
             DO IC=1,NCB
                DD(IC,IR,I2)=DD(IC,IR,I2)+DD(IC,IR,I1)
             ENDDO
          ENDDO

          CALL REDBLK(S1(1,1,I1),A2(1,1,I1),  S2(1,1,I1),BB(1,1,I1),FAA(1,I1), &
                      A1(1,1,I0),A1(1,1,I1+1),A2(1,1,I2),BB(1,1,I2),FAA(1,I2), &
                      CC(1,1,I0),CC(1,1,I1+1),C2(1,1,I2),DD(1,1,I2),FCFC(1,I2),&
                      IPC(1,I1),IPR(1,I1),IAMAX,NOV,NCB,NRC)
       ELSEIF(NLLV==0)THEN
          CALL REDRHSBLK(A2(1,1,I1),FAA(1,I1),    &
               A1(1,1,I1+1),FAA(1,I2),            &
               CC(1,1,I1+1),FCFC(1,I2),NOV,NRC,IPR(1,I1))
       ENDIF

       END SUBROUTINE REDUCER

!      ---------- ------
       SUBROUTINE REDBLK(S11,A21,S21,BB1,FAA1, &
                         A11,A12,A22,BB2,FAA2, &
                         CC1,CC2,CC3,DD,FC,  &
           IPC,IPR,IAMAX,NOV,NCB,NRC)

! Arguments
       INTEGER, INTENT(IN) :: NOV,NCB,NRC
       INTEGER, INTENT(OUT) :: IPC(NOV),IPR(NOV),IAMAX(NOV*2)
       DOUBLE PRECISION, INTENT(INOUT) :: S11(NOV,NOV),A21(NOV,NOV)
       DOUBLE PRECISION, INTENT(OUT)   :: S21(NOV,NOV),A11(NOV,NOV)
       DOUBLE PRECISION, INTENT(INOUT) :: A12(NOV,NOV),A22(NOV,NOV)
       DOUBLE PRECISION, INTENT(INOUT) :: CC1(NOV,NRC),CC2(NOV,NRC),CC3(NOV,NRC)
       DOUBLE PRECISION, INTENT(INOUT) :: BB1(NCB,NOV),BB2(NCB,NOV),DD(NCB,NRC)
       DOUBLE PRECISION, INTENT(INOUT) :: FAA1(NOV),FAA2(NOV),FC(*)

! Local
       INTEGER K1,K2,IR,IC,IPIV1,IPIV2,JPIV,JPIV1,JPIV2,ITMP
       DOUBLE PRECISION PIV1,PIV2,TPIV,TMP

         DO K1=1,NOV
            DO K2=1,NOV
               S21(K2,K1) = 0.0D0
               A11(K2,K1) = 0.0D0
            ENDDO
         ENDDO

         DO K1=1,NOV
            IAMAX(K1)=IDAMAX(NOV,A21(1,K1),1)
         ENDDO
         DO K1=1,NOV
            IAMAX(NOV+K1)=IDAMAX(NOV,A12(1,K1),1)
         ENDDO

         DO IC=1,NOV

! Complete pivoting; rows are swapped physically, columns also
            PIV1 = 0.d0
            IPIV1 = IC
            DO K1=IC,NOV
               TPIV=DABS(A21(IAMAX(K1),K1))
               IF(PIV1.LT.TPIV)THEN
                  PIV1   = TPIV
                  IPIV1  = K1
               ENDIF
            ENDDO
            JPIV1=IAMAX(IPIV1)

            PIV2 = 0.d0
            IPIV2 = 1
            DO K1=1,NOV
               TPIV=DABS(A12(IAMAX(NOV+K1),K1))
               IF(PIV2.LT.TPIV)THEN
                  PIV2   = TPIV
                  IPIV2  = K1
               ENDIF
            ENDDO
            JPIV2=IAMAX(NOV+IPIV2)

! rows and columns are swapped physically

            IF(PIV1.GE.PIV2)THEN
               JPIV        = JPIV1
               IPR(IC)     = IPIV1
               IF(IC.NE.IPIV1) &
                  CALL REDSWP(IC,NOV,NCB, &
                    S11(1,IC),S11(1,IPIV1),A21(1,IC),A21(1,IPIV1), &
                    S21(1,IC),S21(1,IPIV1),BB1(1,IC),BB1(1,IPIV1), &
                    FAA1(IC),FAA1(IPIV1))
            ELSE
               JPIV        = JPIV2
               IPR(IC)     = NOV+IPIV2
               CALL REDSWP(IC,NOV,NCB, &
                    S11(1,IC),A11(1,IPIV2),A21(1,IC),A12(1,IPIV2), &
                    S21(1,IC),A22(1,IPIV2),BB1(1,IC),BB2(1,IPIV2), &
                    FAA1(IC),FAA2(IPIV2))
            ENDIF
            IAMAX(IPR(IC)) = IAMAX(IC)
            IPC(IC) = JPIV
            IF(JPIV.NE.IC)THEN
               DO IR=1,NOV
                  TMP          = A12(IC,IR)
                  A12(IC,IR)   = A12(JPIV,IR)
                  A12(JPIV,IR) = TMP
                  TMP          = A21(IC,IR)
                  A21(IC,IR)   = A21(JPIV,IR)
                  A21(JPIV,IR) = TMP
               ENDDO
            ENDIF

! End of pivoting; Elimination starts here

            DO IR=IC+1,NOV
               CALL REDELIM(IC,NOV,NCB,IAMAX(IR),JPIV,         &
                    A21(1,IR),A21(1,IC),S11(1,IR),S11(1,IC),   &
                    S21(1,IR),S21(1,IC),BB1(1,IR),BB1(1,IC),   &
                    FAA1(IR),FAA1(IC))
            ENDDO

            DO IR=1,NOV
               CALL REDELIM(IC,NOV,NCB,IAMAX(NOV+IR),JPIV,     &
                    A12(1,IR),A21(1,IC),A11(1,IR),S11(1,IC),   &
                    A22(1,IR),S21(1,IC),BB2(1,IR),BB1(1,IC),   &
                    FAA2(IR),FAA1(IC))
            ENDDO

            DO IR=1,NRC
               TMP          = CC2(IC,IR)
               CC2(IC,IR)   = CC2(JPIV,IR)
               CC2(JPIV,IR) = TMP
               ITMP=0
               CALL REDELIM(IC,NOV,NCB,ITMP,JPIV,              &
                    CC2(1,IR),A21(1,IC),CC1(1,IR),S11(1,IC),   &
                    CC3(1,IR),S21(1,IC),DD(1,IR),BB1(1,IC),    &
                    FC(IR),FAA1(IC))
            ENDDO
         ENDDO

       END SUBROUTINE REDBLK

!      ---------- ------
       SUBROUTINE REDSWP(IC,NOV,NCB,S11,A11,A12,A21,S21,A22,BB1,BB2,F1,F2)

       INTEGER, INTENT(IN) :: IC,NOV,NCB
       DOUBLE PRECISION, INTENT(INOUT) :: S11(NOV),A11(NOV),A12(NOV),A21(NOV)
       DOUBLE PRECISION, INTENT(INOUT) :: S21(NOV),A22(NOV),BB1(NCB),BB2(NCB)
       DOUBLE PRECISION, INTENT(INOUT) :: F1,F2

       INTEGER L
       DOUBLE PRECISION TMP
! Swapping
       DO L=1,NOV
          TMP    = S11(L)
          S11(L) = A11(L)
          A11(L) = TMP                  
          IF(L.GE.IC)THEN
             TMP    = A21(L)
             A21(L) = A12(L)
             A12(L) = TMP
          ENDIF
          TMP    = S21(L)
          S21(L) = A22(L)
          A22(L) = TMP
       ENDDO
       DO L=1,NCB
          TMP    = BB1(L)
          BB1(L) = BB2(L)
          BB2(L) = TMP
       ENDDO
       IF(NLLV==0)THEN
          TMP = F1
          F1 = F2
          F2 = TMP
       ENDIF
       END SUBROUTINE REDSWP

!      ---------- -------
       SUBROUTINE REDELIM(IC,NOV,NCB,IAMAX,JPIV,A12,A21,A11,S11,A22,S21,&
            BB2,BB1,F2,F1)

       INTEGER, INTENT(IN) :: IC,NOV,NCB,JPIV
       INTEGER, INTENT(INOUT) :: IAMAX
       DOUBLE PRECISION, INTENT(INOUT) :: A12(NOV),A11(NOV),A22(NOV),BB2(NCB),F2
       DOUBLE PRECISION, INTENT(IN) :: A21(NOV),S11(NOV),S21(NOV),BB1(NCB),F1

       INTEGER L
       DOUBLE PRECISION RM,V,PPIV,TPIV

       RM = A12(IC)/A21(IC)
       A12(IC) = RM

       IF(RM.NE.0.0)THEN
          IF(IAMAX.EQ.0)THEN
             DO L=IC+1,NOV
                A12(L)=A12(L)-RM*A21(L)
             ENDDO
          ELSE
             PPIV=0d0
             IAMAX=IC+1
             DO L=IC+1,NOV
                V=A12(L)-RM*A21(L)
!     Also recalculate absolute maximum for current row
                A12(L)=V
                TPIV=DABS(V)
                IF(PPIV.LT.TPIV)THEN
                   PPIV=TPIV
                   IAMAX=L
                ENDIF
             ENDDO
          ENDIF
          DO L=1,NOV
             A11(L) = A11(L)-RM*S11(L)
             A22(L) = A22(L)-RM*S21(L)
          ENDDO
          DO L=1,NCB
             BB2(L) = BB2(L)-RM*BB1(L)
          ENDDO
          IF(NLLV==0)THEN
             F2 = F2-RM*F1
          ENDIF
       ELSEIF(IAMAX.EQ.JPIV)THEN
!     recalculate absolute maximum for current row
          IF(IC<NOV)THEN
             IAMAX = IC+IDAMAX(NOV-IC,A12(IC+1),1)
          ENDIF
       ELSEIF(IAMAX.EQ.IC)THEN
          IAMAX = JPIV
       ENDIF

       END SUBROUTINE REDELIM

!      ---------- ---------
       SUBROUTINE REDRHSBLK(A21,FAA1,A12,FAA2,CC,FC,NOV,NRC,IPR)

! Arguments
       INTEGER, INTENT(IN) :: NOV,NRC,IPR(NOV)
       DOUBLE PRECISION, INTENT(IN) :: A12(NOV,NOV),A21(NOV,NOV)
       DOUBLE PRECISION, INTENT(IN) :: CC(NOV,NRC)
       DOUBLE PRECISION, INTENT(INOUT) :: FAA1(NOV),FAA2(NOV),FC(*)

! Local
       INTEGER IC,IR,IPIV1,L1
       DOUBLE PRECISION RM

! Reduce with the right hand side for one block
       DO IC=1,NOV
         IPIV1 = IPR(IC)
         IF(IPIV1.LE.NOV)THEN
            RM          = FAA1(IPIV1)
            FAA1(IPIV1) = FAA1(IC)
         ELSE
            L1       = IPIV1-NOV
            RM       = FAA2(L1)
            FAA2(L1) = FAA1(IC)
         ENDIF
         FAA1(IC) = RM
         DO IR=IC+1,NOV
            FAA1(IR) = FAA1(IR)-A21(IC,IR)*RM
         ENDDO
         DO IR=1,NOV
            FAA2(IR) = FAA2(IR)-A12(IC,IR)*RM
         ENDDO
         DO IR=1,NRC
            FC(IR)= FC(IR)-CC(IC,IR)*RM
         ENDDO
       ENDDO

       END SUBROUTINE REDRHSBLK

      END SUBROUTINE REDUCE

!     ---------- ------
      SUBROUTINE DIMRGE(E,CC,C2,CDBC,D,FC, &
           NA,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,A1,A2,FAA,BB)

      USE SUPPORT
! Arguments
      INTEGER   NA,NFC,NBC,NOV,NCB,IDB,NLLV
      DOUBLE PRECISION E(NOV+NFC,*),CC(NOV,NFC-NBC,*),C2(NOV,NFC-NBC,*)
      DOUBLE PRECISION CDBC(2*NOV+NCB,NBC),D(NCB,*)
      DOUBLE PRECISION, INTENT(OUT) :: P0(NOV,*),P1(NOV,*)
      DOUBLE PRECISION A1(NOV,NOV,*),FAA(NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),FC(*),FCC(*)
      DOUBLE PRECISION, INTENT(OUT) :: DET

! Local
      INTEGER  I,J,NCR,NRC
      DOUBLE PRECISION, ALLOCATABLE :: XE(:)
      ALLOCATE(XE(NOV+NFC))

      NCR     = NFC+NOV
      NRC     = NFC-NBC

! Copy
      DO I=1,NOV
         DO J=1,NOV
            E(I,J)     = A1(J,I,1)
            P0(I,J)    = A1(J,I,1)
            E(I,NOV+J) = A2(J,I,NA)
            P1(I,J)    = A2(J,I,NA)
         ENDDO
         DO J=1,NCB
            E(I,2*NOV+J) = BB(J,I,NA)
         ENDDO
      ENDDO

      DO I=1,NBC
         DO J=1,2*NOV+NCB
            E(NOV+I,J)     = CDBC(J,I)
         ENDDO
      ENDDO
      DO I=1,NRC
         DO J=1,NOV
            E(NOV+NBC+I,J)       = CC(J,I,1)
            E(NOV+NBC+I,NOV+J)   = C2(J,I,NA)
         ENDDO
         DO J=1,NCB
            E(NOV+NBC+I,2*NOV+J) = D(J,I)
         ENDDO
      ENDDO

      DO I=1,NOV
         XE(I)=FAA(I,NA)
      ENDDO

      DO I=1,NFC
         XE(NOV+I) = FC(I)
      ENDDO
!     
      IF(IDB.GE.3)THEN
         WRITE(9,101)
         WRITE(9,100)(XE(I),I=1,NCR)
      ENDIF
!     
      IF(IDB.GE.4)THEN
         WRITE(9,102)
         DO I=1,NCR
            WRITE(9,100)(E(I,J),J=1,NCR)
         ENDDO
      ENDIF

! Solve for FCC
      IF(NLLV>0)THEN
         CALL NLVC(NCR,NCR,NLLV,E,FCC)
         DET=0.d0
      ELSE
         IF(NLLV<0)THEN
            DO I=1,NCR-1
               XE(I)=0.D0
            ENDDO
            XE(NCR)=1.D0
         ENDIF
         CALL GEL(NCR,E,1,FCC,XE,DET)
      ENDIF

      IF(IDB.GE.4)THEN
         WRITE(9,103)
         WRITE(9,100)(FCC(I),I=1,NCR)
      ENDIF


 100  FORMAT(1X,10E11.3)
 101  FORMAT(/,1X,'Residuals of reduced system:')
 102  FORMAT(/,1X,'Reduced Jacobian matrix:')
 103  FORMAT(/,1X,'Solution vector:')

      DO I=1,NFC
         FC(I)=FCC(NOV+I)
      ENDDO

      DEALLOCATE(XE)
      END SUBROUTINE DIMRGE

!     ---------- ------
      SUBROUTINE BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,IT,NT,IAM,KWT)

      USE AUTOMPI

! Arguments
      INTEGER, INTENT(IN) :: NTST,NOV,NCB,IPC(NOV,*),IT,NT,IAM,KWT
      DOUBLE PRECISION, INTENT(IN) :: S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION, INTENT(IN) :: A2(NOV,NOV,*),BB(NCB,NOV,*)
      DOUBLE PRECISION, INTENT(IN) :: FAA(NOV,*)
      DOUBLE PRECISION, INTENT(INOUT) :: FC(*)
      DOUBLE PRECISION, INTENT(OUT) :: SOL(NOV,*)

! Local
      INTEGER I,PLO,PHI,NA,MPLO,MPHI
      LOGICAL DOMPI

      MPLO = (IAM*NTST+KWT-1)/KWT+1
      MPHI = ((IAM+1)*NTST+KWT-1)/KWT
      NA = MPHI-MPLO+1
!$OMP MASTER
! do global backsubsitution until there is no overlap left
      IF(IAM==0)THEN
         DO I=1,NOV
            SOL(I,NTST+1) = FC(I)
         ENDDO
      ENDIF
      IF(KWT>1)THEN
         CALL MPIBCAST(FC,NOV+NCB)
      ENDIF
      IF(NT>1)THEN
         PLO = MPLO
         PHI = MPHI
         DOMPI = KWT>1
         CALL BCKSUBR(1,NTST)
      ENDIF
!$OMP END MASTER
!$OMP BARRIER
      PLO = MPLO+(IT*NA+NT-1)/NT
      PHI = MPLO+((IT+1)*NA+NT-1)/NT-1
      DOMPI = KWT>1.AND.NT==1
      CALL BCKSUBR(1,NTST)

      CONTAINS

!      Back substitution within the interval [MPLO,MPHI]; LO is in [PLO,PHI]
!      --------- ---------- -------
       RECURSIVE SUBROUTINE BCKSUBR(LO,HI)

! Arguments
       INTEGER, INTENT(IN) :: LO,HI

! Local
       INTEGER MID,I,I0,I1

       IF(LO>=HI.OR.HI<PLO.OR.LO>PHI)RETURN
! This is a check for the master reduction so it will stop as soon
! as there is no more overlap (already handled by nodes).
       IF(NT>1.AND.PHI-PLO==NA-1.AND.LO>=PLO)THEN
          IF((LO-PLO)*NT/NA==(HI-PLO)*NT/NA)RETURN
       ENDIF
       MID=(LO+HI)/2
       I=MID
       I0=LO
       I1=HI
       IF((PHI-PLO==NA-1.OR.HI<=PHI).AND.LO>=PLO)THEN
          CALL BCKSUB1(S1(1,1,I),A2(1,1,I),S2(1,1,I),BB(1,1,I),     &
               FAA(1,I),SOL(1,I0),SOL(1,I+1),SOL(1,I1+1),FC(NOV+1), &
               NOV,NCB,IPC(1,I))
       ENDIF
       IF(DOMPI)THEN
          CALL MPIBCKSUB(SOL,NTST,NOV,LO,HI)
       ENDIF
       CALL BCKSUBR(MID+1,HI)
       CALL BCKSUBR(LO,MID)

       END SUBROUTINE BCKSUBR

!      ---------- -------
       SUBROUTINE BCKSUB1(S1,A2,S2,BB,FAA,FCC,SOL1,SOL2,FC,NOV,NCB,IPC)

! Arguments
         INTEGER, INTENT(IN) :: NOV,NCB,IPC(NOV)
         DOUBLE PRECISION, INTENT(IN) :: S1(NOV,NOV),S2(NOV,NOV)
         DOUBLE PRECISION, INTENT(IN) :: A2(NOV,NOV),BB(NCB,NOV)
         DOUBLE PRECISION, INTENT(IN) :: SOL2(NOV),FAA(NOV),FC(*),FCC(*)
         DOUBLE PRECISION, INTENT(OUT) :: SOL1(NOV)

! Local
         INTEGER K,L
         DOUBLE PRECISION SM,TMP

! Backsubstitution process for 1 block row
         DO K=NOV,1,-1
            SM=FAA(K)
            DO L=1,NOV
               SM=SM-FCC(L)*S1(L,K)
               SM=SM-SOL2(L)*S2(L,K)
            ENDDO
            DO L=1,NCB
               SM=SM-FC(L)*BB(L,K)
            ENDDO
            DO L=K+1,NOV
               SM=SM-SOL1(L)*A2(L,K)
            ENDDO
            SOL1(K)=SM/A2(K,K)
         ENDDO
!     Revert column pivoting on SOL1
         DO K=NOV,1,-1
            TMP=SOL1(K)
            SOL1(K)=SOL1(IPC(K))
            SOL1(IPC(K))=TMP
         ENDDO

       END SUBROUTINE BCKSUB1

      END SUBROUTINE BCKSUB

!     ---------- ------
      SUBROUTINE INFPAR(A,B,FA,SOL,FC,NA,NOV,NRA,NCA,NCB,ICF,X)

!  Arguments
      INTEGER, INTENT(IN) :: NA,NOV,NRA,NCA,NCB,ICF(NCA,*)
      DOUBLE PRECISION, INTENT(IN) :: A(NCA,NRA,*),B(NCB,NRA,*),FC(*)
      DOUBLE PRECISION, INTENT(IN) :: SOL(NOV,*)
      DOUBLE PRECISION, INTENT(OUT) :: X(NOV+1:NRA),FA(NRA,*)

! Local
      INTEGER I,J,IR
      DOUBLE PRECISION SM,TMP

! Determine the local varables by backsubstitition.

! Backsubstitution in the condensation of parameters; no communication.
      DO I=1,NA
         DO IR=NRA-NOV,1,-1
            SM=FA(IR,I)
            DO J=1,NOV
               SM=SM-A(J,IR,I)*SOL(J,I)
               SM=SM-A(NRA+J,IR,I)*SOL(J,I+1)
            ENDDO
            DO J=1,NCB
               SM=SM-B(J,IR,I)*FC(NOV+J)
            ENDDO
            DO J=IR+1,NRA-NOV
               SM=SM-A(J+NOV,IR,I)*X(J+NOV)
            ENDDO
            X(NOV+IR)=SM/A(NOV+IR,IR,I)
         ENDDO    
!        **Copy SOL into FA 
         DO J=1,NOV
            FA(J,I)=SOL(J,I)
         ENDDO
!        **Undo pivots and copy X into FA
         DO J=NRA,NOV+1,-1
            TMP=X(ICF(J,I))
            X(ICF(J,I))=X(J)
            X(J)=TMP
         ENDDO
         DO J=NOV+1,NRA
            FA(J,I)=X(J)
         ENDDO
      ENDDO

      END SUBROUTINE INFPAR

!     ---------- ------
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CDBC,D,DD,FA, &
       FC,FCFC,IFST,NLLV)

      INTEGER, INTENT(IN) :: NA,NRA,NCA,NCB,NFC,NBC,IFST,NLLV
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DOUBLE PRECISION CDBC(2*(NCA-NRA)+NCB,NBC),D(NCB,*),DD(NCB,NFC-NBC,*)
      DOUBLE PRECISION FA(NRA,*),FC(*),FCFC(NFC-NBC,*)

      INTEGER I,IR,IC
      DOUBLE PRECISION, ALLOCATABLE :: D1(:)
      DOUBLE PRECISION FC1

       WRITE(9,101)
       DO I=1,NA
         WRITE(9,102)I
         DO IR=1,NRA
           IF(NLLV==0)THEN
              WRITE(9,103)(A(IC,IR,I),IC=1,NCA),(B(IC,IR,I),IC=1,NCB),FA(IR,I)
           ELSE
              WRITE(9,103)(A(IC,IR,I),IC=1,NCA),(B(IC,IR,I),IC=1,NCB),0d0
           ENDIF
         ENDDO
       ENDDO

       WRITE(9,104)
       DO I=1,NA
         WRITE(9,102)I
         DO IR=1,NFC
           IF(IR.GT.NBC)THEN
             WRITE(9,103)(C(IC,IR-NBC,I),IC=1,NCA)
           ELSEIF(I.EQ.1)THEN
             WRITE(9,103)(CDBC(IC,IR),IC=1,NCA-NRA)
           ELSEIF(I.EQ.NA)THEN
             WRITE(9,103)(CDBC(NCA-NRA+IC,IR),IC=1,NCA-NRA)
           ENDIF
         ENDDO
       ENDDO

       WRITE(9,105)
       DO IR=1,NBC
         WRITE(9,103)(CDBC(2*(NCA-NRA)+IC,IR),IC=1,NCB),FC(IR)
       ENDDO
       ALLOCATE(D1(NCB))
       DO IR=1,NFC-NBC
         DO IC=1,NCB
           D1(IC)=D(IC,IR)
           IF(IFST==1)THEN
              DO I=1,NA
                 D1(IC)=D1(IC)+DD(IC,IR,I)
              ENDDO
           ENDIF
         ENDDO
         IF(NLLV==0)THEN
            FC1=FC(NBC+IR)
            DO I=1,NA
               FC1=FC1+FCFC(IR,I)
            ENDDO
         ELSE
            FC1=0
         ENDIF
         WRITE(9,103)(D1(IC),IC=1,NCB),FC1
       ENDDO
       DEALLOCATE(D1)

 101   FORMAT(' AA , BB , FA (Full dimension) :')
 102   FORMAT(' I=',I3)
 103   FORMAT(1X,12E10.3)
 104   FORMAT(' CC (Full dimension) :')
 105   FORMAT(' DD , FC')

      END SUBROUTINE PRINT1

      END MODULE SOLVEBV
