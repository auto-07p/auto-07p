C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C           Setting up of the Jacobian and right hand side
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
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
C
C     ---------- ------
      SUBROUTINE SOLVBV(IFST,AP,DET,PAR,ICP,FUNI,BCNI,ICNI,RDS,
     * NLLV,RLCUR,RLOLD,RLDOT,NDIM,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,
     * DUPS,DRL,P0,P1,THL,THU)
C
C$    USE OMP_LIB
      USE AUTOMPI
C
C Sets up and solves the linear equations for one Newton/Chord iteration
C
      include 'interfaces.h'
      TYPE(AUTOPARAMETERS) AP
      INTEGER IFST,NLLV,ICP(*)
      DOUBLE PRECISION DET,PAR(*),RDS
      DOUBLE PRECISION RLOLD(AP%NFPR),RLCUR(AP%NFPR),RLDOT(AP%NFPR)
      DOUBLE PRECISION UPS(NDIM,0:*),UDOTPS(NDIM,0:*),UOLDPS(NDIM,0:*)
      DOUBLE PRECISION UPOLDP(NDIM,0:*),DUPS(NDIM,0:*),DTM(*)
      DOUBLE PRECISION DRL(AP%NFPR),P0(*),P1(*),THL(*),THU(*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE, SAVE ::
     *     A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:),
     *     S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:),CCBC(:,:,:),DDBC(:,:)
      INTEGER, ALLOCATABLE, SAVE ::
     *     ICF(:,:),IRF(:,:),IPR(:,:),IPC(:,:)
      DOUBLE PRECISION, ALLOCATABLE ::
     *     CCLO(:,:,:),DD(:,:,:),FCFC(:,:),FAA(:,:),SOL(:,:),FA(:,:),
     *     FC(:)
      INTEGER, ALLOCATABLE :: NP(:)
      INTEGER IAM,KWT,NDIM,NTST,NCOL,NBC,NINT,IID,NFPR,NPAR
      INTEGER NRC,NFC,NROW,NCLM
      INTEGER NA,NTSTNA,IT,NT,MNT,I,J
      INTEGER ISHAPE(8)
C
C Most of the required memory is allocated below
C
C This is an interesting section of code.  The main point
C is that setubv and conpar only get called when ifst
C is 1.  This is a optimization since you can solve
C the system using the previously factored jacobian.
C One thing to watch out for is that two seperate calls
C of solvbv_ talk to each other through these arrays,
C so it is only safe to get rid of them when ifst is
C 1 (since their entries will then be recreated in conpar
C  and setubv).
C
C
      IAM=MPIIAM()
      KWT=MPIKWT()
C
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
C
      ALLOCATE(NP(KWT))
      IF(KWT.GT.NTST)THEN
        PRINT*,'NTST is less than the number of nodes'
        STOP
      ELSE
        CALL PARTITION(NTST,KWT,NP)
      ENDIF
C
C     NTST is the global one, NA is the local one.
C     The value of NTST may be different in different nodes.
      NA=NP(IAM+1)
C
      NTSTNA=NA
      IF(IAM.EQ.0)NTSTNA=NTST
      ALLOCATE(FA(NROW,NTSTNA),FC(NFC))
      MNT = 1
C$    MNT = OMP_GET_MAX_THREADS()
      IF(MNT.GT.NA)THEN
         MNT=NA
C$       CALL OMP_SET_NUM_THREADS(NA)
      ENDIF
      IF(IFST.EQ.1)THEN
         IF(ALLOCATED(A))THEN
C            !a sufficient check to see if array dimensions have changed:
            ISHAPE(1:3)=SHAPE(A)
            ISHAPE(4:6)=SHAPE(CC)
            ISHAPE(7:8)=SHAPE(DDBC)
            IF(ISHAPE(1)/=NCLM.OR.ISHAPE(2)/=NROW.OR.ISHAPE(3)/=NA+1
     *     .OR.ISHAPE(4)/=NDIM.OR.ISHAPE(5)/=NRC.OR.ISHAPE(6)/=NTSTNA+1
     *     .OR.ISHAPE(7)/=NFPR.OR.ISHAPE(8)/=NBC)THEN
C              Free floating point arrays
               DEALLOCATE(A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,DDBC)
C              Free integer arrays
               DEALLOCATE(ICF,IRF,IPR,IPC)
            ENDIF
         ENDIF
C
         IF(.NOT.ALLOCATED(A))THEN
            ALLOCATE(A(NCLM,NROW,NA+1),B(NFPR,NROW,NA+1))
            ALLOCATE(C(NCLM,NRC,NA+1),D(NFPR,NRC))
            ALLOCATE(A1(NDIM,NDIM,NTSTNA+1),A2(NDIM,NDIM,NTSTNA+1))
            ALLOCATE(S1(NDIM,NDIM,NTSTNA+1),S2(NDIM,NDIM,NTSTNA+1))
            ALLOCATE(BB(NFPR,NDIM,NTSTNA+1),CC(NDIM,NRC,NTSTNA+1))
            ALLOCATE(CCBC(NDIM,NBC,2),DDBC(NFPR,NBC))
C
            ALLOCATE(ICF(NCLM,NA),IRF(NROW,NA),IPR(NDIM,NTSTNA-1))
            ALLOCATE(IPC(NDIM,NTSTNA-1))
         ENDIF
      ENDIF
      IF(IAM.EQ.0)THEN
C
         DO I=1,NFPR
            PAR(ICP(I))=RLCUR(I)
         ENDDO
         CALL SUBVBC(NDIM,NTST*NCOL,NBC,NFPR,BCNI,
     +        AP,PAR,NPAR,ICP,CCBC,DDBC,FC,UPS,IFST)
         CALL SUBVPSA(NFPR,RDS,D(1,NRC),FC(NFC),
     +        RLCUR,RLOLD,RLDOT,THL,IFST)
         IF(KWT.GT.1)THEN
            CALL MPISBV(AP,PAR,ICP,NDIM,UPS,UOLDPS,UDOTPS,
     +           UPOLDP,DTM,THU,IFST,NLLV)
         ENDIF
      ELSE
C     The matrix D and FC are set to zero for all nodes except the first.
C     zero pseudo-arclength part of matrices, rest is done in setubv()
         CALL SETFCDD(IFST,D(1,NRC),FC(NFC),NFPR,1)
      ENDIF
C
      IF(MNT.GT.1)THEN
C        CCLO avoids write overlap between threads
         ALLOCATE(DD(NFPR,NRC,MNT-1),FCFC(NRC,MNT-1),
     +        CCLO(NDIM,NRC,MNT-1))
      ENDIF
      ALLOCATE(FAA(NDIM,NTSTNA+1),SOL(NDIM,NTSTNA+1))
C
C$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,IT,NT)
C
      IT = 0
C$    IT = OMP_GET_THREAD_NUM()
      NT = 1
C$    NT = OMP_GET_NUM_THREADS()
      IF(NLLV>=0.OR.IFST==1)
     +  CALL SETUBV(NDIM,NA,NCOL,NINT,NFPR,NRC,NROW,NCLM,
     +   FUNI,ICNI,AP,PAR,NPAR,ICP,A,B,C,D,DD,FA,
     +   FC(NBC+1),FCFC,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,IAM,IT,NT,
     +   IRF,ICF,IID,NLLV)
C
      I = IT*NA/NT+1
      CALL BRBD(A(1,1,I),B(1,1,I),C(1,1,I),D,DD,FA(1,I),FAA,FC,
     +  FCFC,P0,P1,IFST,IID,NLLV,DET,NDIM,NTST,NA,NBC,NROW,NCLM,
     +  NFPR,NFC,A1,A2,BB,CC,CCLO,CCBC,DDBC,
     +  SOL,S1,S2,IPR,IPC,IRF(1,I),ICF(1,I),IAM,KWT,IT,NT)
C
C$OMP END PARALLEL
C
      IF(MNT.GT.1)DEALLOCATE(DD,FCFC,CCLO)
      DEALLOCATE(FAA,SOL)
C
      IF(KWT.GT.1)THEN
C        Global concatenation of the solution from each node.
        CALL MPIGAT(FA,NDIM*NCOL,NTST)
      ENDIF
C
      IF(IAM.EQ.0)THEN
         DO I=0,NTST-1
            DO J=0,NCOL-1
               DUPS(:,I*NCOL+J)=FA(J*NDIM+1:(J+1)*NDIM,I+1)
            ENDDO
         ENDDO
         DUPS(:,NTST*NCOL)=FC(1:NDIM)
         DRL(:)=FC(NDIM+1:)
      ENDIF
C
      DEALLOCATE(NP,FA,FC)
      RETURN
      END SUBROUTINE SOLVBV
C
C     ---------- -------
      SUBROUTINE SETFCDD(IFST,DD,FC,NCB,NRC)

      INTEGER, INTENT(IN) :: IFST,NCB,NRC
      DOUBLE PRECISION FC(*),DD(NCB,*)

      INTEGER I,J
C
      DO I=1,NRC
        IF(IFST.EQ.1)THEN
          DO J=1,NCB
            DD(J,I)=0.0D0
          ENDDO
        ENDIF
        FC(I)=0.0D0
      ENDDO
C
      RETURN
      END SUBROUTINE SETFCDD
C
C     ---------- ---------
      SUBROUTINE SUBVBC(NDIM,NTNC,NBC,NCB,BCNI,
     + AP,PAR,NPAR,ICP,CCBC,DDBC,FC,UPS,IFST)
C
C     This subroutine handles a non-parallel part of SETUBV, that is,
C     * the boundary conditions (not much to parallelize here and
C       HomCont relies on non-parallel execution): the arrays CCBC,
C       DDBC, and parts of FC.
C
      include 'interfaces.h'
C
      TYPE(AUTOPARAMETERS) AP
      INTEGER NDIM,NTNC,NBC,NCB,ICP(*),IFST,NPAR
      DOUBLE PRECISION CCBC(NDIM,NBC,*),DDBC(NCB,*)
      DOUBLE PRECISION UPS(NDIM,0:NTNC),FC(*),PAR(*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,K
C
      ALLOCATE(UBC0(NDIM),UBC1(NDIM),FBC(NBC),DBC(NBC,2*NDIM+NPAR))
      DBC(:,:)=0.d0
C
C     Boundary conditions :
C     
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
                  CCBC(K,I,1)=DBC(I,K)
                  CCBC(K,I,2)=DBC(I,NDIM+K)
               ENDDO
               DO K=1,NCB
                  DDBC(K,I)=DBC(I,2*NDIM+ICP(K))
               ENDDO
            ENDIF
         ENDDO    
       ENDIF
       DEALLOCATE(UBC0,UBC1,FBC,DBC)
       RETURN
       END SUBROUTINE SUBVBC
C
C     ---------- -------
      SUBROUTINE SUBVPSA(NCB,RDS,DDPA,FCPA,RLCUR,RLOLD,RLDOT,THL,IFST)
C
      USE MESH
C
C     This subroutine handles a non-parallel part of SETUBV, that is,
C     * creating the parameter dependent pseudo-arclength parts of FC and D:
C       (the bottom element FCPA and row DDPA)
C
      INTEGER, INTENT(IN) :: NCB,IFST
      DOUBLE PRECISION, INTENT(IN) :: RDS,THL(NCB)
      DOUBLE PRECISION, INTENT(IN) :: RLCUR(NCB),RLOLD(NCB),RLDOT(NCB)
      DOUBLE PRECISION, INTENT(OUT) :: DDPA(NCB),FCPA
C
C Local
      INTEGER I
      DOUBLE PRECISION RLSUM
C
C     Pseudo-arclength equation :
C
       RLSUM=0.d0
       DO I=1,NCB
          IF(IFST.EQ.1)THEN
             DDPA(I)=THL(I)*RLDOT(I)
          ENDIF
          RLSUM=RLSUM+THL(I)*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
       FCPA=RDS-RLSUM

       END SUBROUTINE SUBVPSA
C
C     ---------- ------
      SUBROUTINE SETUBV(NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,AP,PAR,NPAR,ICP,AA,BB,CC,DD,DDD,FA,FC,FCFC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,IAM,IT,NT,IRF,ICF,IDB,NLLV)
C
      USE MESH
C
      INTEGER NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,IFST,IAM,IT,NT,NPAR
      TYPE(AUTOPARAMETERS) AP
      INTEGER ICP(*),IRF(NRA,*),ICF(NCA,*),IDB,NLLV
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),UPS(NDIM,0:*),UOLDPS(NDIM,0:*)
      DOUBLE PRECISION UDOTPS(NDIM,0:*),UPOLDP(NDIM,0:*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION DDD(NCB,NRC,*),FCFC(NRC,*)
C
      include 'interfaces.h'
C
C Local
      DOUBLE PRECISION WI(0:NCOL),WP(0:NCOL,NCOL),WT(0:NCOL,NCOL)
      INTEGER I,N,II
C
C
      CALL WINT(NCOL,WI)
      CALL GENWTS(NCOL,WT,WP)
C
      I = IT*NA/NT+1
      N = (IT+1)*NA/NT+1-I
      IF(IT.EQ.0)THEN
         CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,
     +        AP,PAR,NPAR,ICP,AA,BB,CC,DD,FA,FC,UPS,
     +        UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT,IRF,ICF,IFST,NLLV)
      ELSE
         CALL SETFCDD(IFST,DDD(1,NRC,IT),FCFC(NRC,IT),NCB,1)
         II = (I-1)*NCOL
         CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,
     +        AP,PAR,NPAR,ICP,AA(1,1,I),BB(1,1,I),CC(1,1,I),
     +        DDD(1,1,IT),FA(1,I),FCFC(1,IT),UPS(1,II),UOLDPS(1,II),
     +        UDOTPS(1,II),UPOLDP(1,II),DTM(I),THU,WI,WP,WT,
     +        IRF(1,I),ICF(1,I),IFST,NLLV)
      ENDIF
C
      CONTAINS

C     ---------- ---------
      SUBROUTINE SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,AP,PAR,NPAR,ICP,AA,BB,CC,DD,FA,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT,IRF,ICF,IFST,NLLV)
C
C     This is the per-CPU parallelized part of SETUBV
C
      include 'interfaces.h'
C
      TYPE(AUTOPARAMETERS) AP
      INTEGER NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,ICP(*),NPAR
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),UPS(NDIM,0:*),UOLDPS(NDIM,0:*)
      DOUBLE PRECISION UDOTPS(NDIM,0:*),UPOLDP(NDIM,0:*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION WI(0:*),WP(0:NCOL,*),WT(0:NCOL,*)
      INTEGER IRF(NRA,*),ICF(NCA,*),IFST,NLLV
C
C Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U,
     +  F,FICD,DICD,UIC,UIO,UID,UIP,PRM
      DOUBLE PRECISION WPLOC(0:NCOL)
      INTEGER I,J,JJ,K,IC,IC1,J1,K1,IB
      INTEGER, ALLOCATABLE :: IAMAX(:)
C
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPAR),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT))
      ALLOCATE(DICD(NINT*(NDIM+NPAR)),UIC(NDIM),UIO(NDIM))
      ALLOCATE(UID(NDIM),UIP(NDIM),PRM(NPAR),IAMAX(NRA))
C
C Initialize to zero.
C
       DO I=1,NINT
         FC(I)=0.d0
         IF(IFST.EQ.1)THEN
            DO K=1,NCB
               DD(K,I)=0.d0
            ENDDO
         ENDIF
       ENDDO
       DFDU(:)=0.d0
       DFDP(:)=0.d0
       DICD(:)=0.d0

       DO I=1,NPAR
          PRM(I)=PAR(I)
       ENDDO
       DO J=1,N
C
C Generate AA , BB and FA :
C
          DO IC=1,NCOL
             IC1=(IC-1)*NDIM+1
             JJ=(J-1)*NCOL
             DO IB=0,NCOL
                WPLOC(IB)=WP(IB,IC)/DTM(J)
             ENDDO
             CALL SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,AP,PRM,ICP,
     +            AA(1,IC1,J),BB(1,IC1,J),FA(IC1,J),UPS(1,JJ),
     +            UOLDPS(1,JJ),WPLOC,WT(0,IC),DFDU,DFDP,
     +            U,UOLD,F,IFST,NLLV)
             IF(IFST.EQ.1)THEN
                DO K=0,NDIM-1
                   IAMAX(IC1+K)=
     +                  NDIM+IDAMAX(NRA-NDIM,AA(NDIM+1,IC1+K,J),1)
                ENDDO
             ENDIF
          ENDDO
C     
C     Generate CC, DD and FC :
C
         DO K=0,NCOL
            J1=(J-1)*NCOL+K
            K1=K*NDIM+1
C     
C     Integral constraints+pseudo-arclength equation :
C     
            CALL SBVICN(NDIM,NINT,NCB,NCA,ICNI,AP,PRM,ICP,
     +           CC(K1,1,J),DD,FC,UPS(1,J1),UOLDPS(1,J1),
     +           UDOTPS(1,J1),UPOLDP(1,J1),DTM(J),THU,WI(K),FICD,DICD,
     +           UIC,UIO,UID,UIP,IFST,NLLV)
         ENDDO

C     debug: do condensation of parameters later after printing
         IF(IDB>4.AND.IAM==0)CYCLE
C
C      Condensation of parameters:
         IF(IFST.EQ.1)THEN
            CALL CONPAR(NDIM,NRA,NCA,AA(1,1,J),NCB,BB(1,1,J),NRC,
     +           CC(1,1,J),DD,FA(1,J),FC,IRF(1,J),ICF(1,J),IAMAX,NLLV)
         ELSE
            CALL CONRHS(NDIM,NRA,NCA,AA(1,1,J),NRC,
     +           CC(1,1,J),FA(1,J),FC,IRF(1,J))
         ENDIF
       ENDDO
C     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD)
       DEALLOCATE(UIC,UIO,UID,UIP,PRM,IAMAX)
       RETURN
      END SUBROUTINE SUBVPA
C
C     ---------- ---------
      SUBROUTINE SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,AP,PAR,ICP,
     + AA,BB,FA,UPS,UOLDPS,WPLOC,WT,DFDU,DFDP,U,UOLD,F,IFST,NLLV)
C
C     Does one call to FUNI and stores the result in AA, BB, and FA.
C
      include 'interfaces.h'
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,NCOL,NCB,NCA,ICP(*)
      INTEGER, INTENT(IN) :: IFST,NLLV
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: WT(0:NCOL),WPLOC(0:NCOL)
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NCOL)
      DOUBLE PRECISION, INTENT(IN) :: UOLDPS(NDIM,0:NCOL)
      DOUBLE PRECISION, INTENT(OUT) :: AA(NCA,*),BB(NCB,*),FA(*),U(*)
      DOUBLE PRECISION, INTENT(OUT) :: UOLD(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*)
C
C Local
      DOUBLE PRECISION WTTMP,TMP
      INTEGER I,IB,IB1,J,K
C
      DO K=1,NDIM
         U(K)=DOT_PRODUCT(WT(:),UPS(K,:))
         UOLD(K)=DOT_PRODUCT(WT(:),UOLDPS(K,:))
      ENDDO
      CALL FUNI(AP,NDIM,U,UOLD,ICP,PAR,IFST*2,F,DFDU,DFDP)
      IF(IFST.EQ.1)THEN
         DO I=1,NDIM
C     use U instead of DFDU in inner loop to better utilize the CPU cache
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
C
C     ---------- ------
      SUBROUTINE SBVICN(NDIM,NINT,NCB,NCA,ICNI,AP,PAR,ICP,CC,DD,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,FICD,DICD,UIC,UIO,UID,UIP,
     + IFST,NLLV)
C
C     Does one call to ICNI (integral constraints) and stores the
C     result in CC, DD and FC; and stores the pseudo-arclength
C     result too.
C
      include 'interfaces.h'
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,NINT,NCB,NCA,ICP(*),IFST,NLLV
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: UPS(*),UDOTPS(*)
      DOUBLE PRECISION, INTENT(IN) :: UOLDPS(*),UPOLDP(*),DTM,WI,THU(*)
      DOUBLE PRECISION, INTENT(OUT) :: CC(NCA,*),FICD(*),DICD(NINT,*)
      DOUBLE PRECISION, INTENT(OUT) :: UIC(*),UIO(*),UID(*),UIP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DD(NCB,*),FC(*)
C
C Local
      INTEGER I,M
      DOUBLE PRECISION DFCDU
C
      IF(NINT.GT.0)THEN
         DO I=1,NDIM
            UIC(I)=UPS(I)
            UIO(I)=UOLDPS(I)
            UID(I)=UDOTPS(I)
            UIP(I)=UPOLDP(I)
         ENDDO
         CALL ICNI(AP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,
     +        FICD,IFST*2,DICD)
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
C     
C     Pseudo-arclength equation :
C     
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
C
      END SUBROUTINE SETUBV
C
C     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,DD,FA,FAA,FC,FCFC,P0,P1,IFST,
     +  IDB,NLLV,DET,NOV,NTST,NA,NBC,NRA,NCA,
     +  NCB,NFC,A1,A2,BB,CC,CCLO,CCBC,DDBC,
     +  SOL,S1,S2,IPR,IPC,IRF,ICF,IAM,KWT,IT,NT)
C
C Solves linear systems with matrix profile:
C
C     -----------------------------------------------
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !XXXXXXXXXX                                !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !        XXXXXXXXXX                        !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                XXXXXXXXXX                !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                        XXXXXXXXXX        !XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     !                                XXXXXXXXXX!XX!
C     -----------------------------------------------
C     !XX                                      XX!XX!
C     !XX                                      XX!XX!
C     !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX!XX!
C     !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX!XX!
C     !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX!XX!
C     -----------------------------------------------
C
C
C partioned as
C
C
C      ---------
C      !     ! !   !    !   !    !
C      !  A  !B!   ! XA !   ! FA !
C      !     ! ! . !    ! = !    !   .
C      !-----!-!   !----!   !----!
C      ! CCBC!DDBC !    !   !    !
C      !-----!-!   ! XC !   ! FC !
C      !  C  !D!   !    !   !    !
C      !-----!-!   !----!   !----!
C
C
C Input parameters :
C
C   NA    number of blocks in A,
C   NRA   number of rows in each block of A,
C   NCA   number of columns in each block of A,
C   A     the matrix in the schematic representation above,
C
C   NCB   number of columns in each block of B,
C         (note that B is also three dimensional),
C   B     the matrix in the schema above,
C
C   NRC   the number of rows of the two dimensional matrix C,
C   C     the matrix C in the schema above,
C
C   D      the matrix D above,
C
C   NFC   the number of rows of the two dimensional matrices C+CCBC, XC and FC
C   C     the matrix C in the schema above,
C
C   CCBC  the matrix CCBC above,
C   DDBC  the matrix CCBC above,
C   NBC   the number of rows of the two dimensional matrices CCBC and DDBC
C
C   FA     part of the right hand side vector,
C          (note that FA is also two dimensional),
C   FC     part of the right hand side vector.
C
C   IFST   = 1 on first call,
C          = 0  on subsequent calls with the same right hand side.
C
C   IDB   = 0..2 no debug output,
C         = 3    output of the residuals of the reduced system on unit 9,
C         = 4    output of the reduced Jacobian matrix and solution vector
C                on unit 9,
C         = 5    output of most matrices on unit 9 (see PRINT1),
C
C   IPR, IPC, ICF: Integer arrays
C
C   NLLV : If NLLV>0 then a null vector will be returned.
C          If NLLV = -1 then the system will be solved with zero right
C          hand side, except for the last equation, for which the right
C          hand side entry will be set to 1 (i.e., the last entry of FC
C          will be set to 1, otherwise FA and FC are zero).
C          If the linear system is the same as in the preceding call
C          then IFST=0 may be used even if NLLV is nonzero.
C
C Returned values :
C
C   FA     Part of solution vector corresponding to XA in the diagram.
C   FC     Part of solution vector corresponding to XC in the diagram.
C
C Notes: The number of columns of overlap for every two consecutive
C        blocks should be equal to the number NOV (NDIM).
C        Parts of the reduction are done in SUBVPA.
C
C Arguments
      INTEGER, INTENT(IN) :: IFST,IDB,NLLV,NOV,NTST,NA,NBC,NRA
      INTEGER, INTENT(IN) :: NCA,NCB,NFC,IAM,KWT,IT,NT
      DOUBLE PRECISION, INTENT(OUT) :: DET
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DOUBLE PRECISION D(NCB,*),DD(NCB,NFC-NBC,*)
      DOUBLE PRECISION FA(NRA,*),FAA(NOV,*),FC(*),FCFC(NFC-NBC,*),P0(*)
      DOUBLE PRECISION P1(*),A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NFC-NBC,*)
      DOUBLE PRECISION CCLO(NOV,NFC-NBC,*)
      DOUBLE PRECISION CCBC(*),DDBC(*),SOL(NOV,*),S1(*),S2(*)
      INTEGER   IPR(*),IPC(*),IRF(NRA,*),ICF(NCA,*)
C
C Local
      INTEGER I,J,K,II,N,NRC
      INTEGER, ALLOCATABLE :: IAMAX(:)
      DOUBLE PRECISION, ALLOCATABLE :: FCC(:),E(:,:),X(:)
C
      NRC=NFC-NBC
      I = IT*NA/NT+1
      N = (IT+1)*NA/NT+1-I
C
      IF(IDB.GT.4.and.IAM.EQ.0)THEN
C$OMP BARRIER
C$OMP MASTER
         CALL PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DD,DDBC,
     +     FA,FC,FCFC,NT,IFST,NLLV)
C$OMP END MASTER
C$OMP BARRIER
         IF(IFST.EQ.1.OR.NLLV>=0)THEN
            ALLOCATE(IAMAX(NRA))
            DO J=1,N
               IF(IFST.EQ.1)THEN
                  DO K=1,NRA
                     IAMAX(K)= NOV+IDAMAX(NRA-NOV,A(NOV+1,K,J),1)
                  ENDDO
                  IF(IT>0)THEN
                     CALL CONPAR(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,
     +                    C(1,1,J),DD(1,1,IT),FA(1,J),FCFC(1,IT),
     +                    IRF(1,J),ICF(1,J),IAMAX,NLLV)
                  ELSE
                     CALL CONPAR(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,
     +                    C(1,1,J),D,FA(1,J),FC(NBC+1),
     +                    IRF(1,J),ICF(1,J),IAMAX,NLLV)
                  ENDIF
               ELSE
                  IF(IT>0)THEN
                     CALL CONRHS(NOV,NRA,NCA,A(1,1,J),NRC,
     +                    C(1,1,J),FA(1,J),FCFC(1,IT),IRF(1,J))
                  ELSE
                     CALL CONRHS(NOV,NRA,NCA,A(1,1,J),NRC,
     +                    C(1,1,J),FA(1,J),FC(NBC+1),IRF(1,J))
                  ENDIF
               ENDIF
            ENDDO
            DEALLOCATE(IAMAX)
         ENDIF
      ENDIF

      IF(IFST.EQ.1)THEN
         CALL COPYCP(N,NOV,NRA,NCA,A,NCB,B,NRC,C,A1(1,1,I),A2(1,1,I),
     +       BB(1,1,I),CC(1,1,I),CCLO,IT)
      ENDIF
      IF(NLLV.NE.0)THEN
         IF(IT.EQ.0)THEN
            CALL SETZERO(FA,FC,N,NRA,NFC)
         ELSE
            CALL SETZERO(FA,FCFC(1,IT),N,NRA,NRC)
         ENDIF
      ENDIF
      CALL CPYRHS(N,NOV,NRA,FAA(1,I),FA)
C
      CALL REDUCE(A1,A2,BB,CC,CCLO,D,DD,FAA,FC(NBC+1),FCFC,
     +     NTST,NOV,NCB,NRC,S1,S2,IPC,IPR,IFST,NLLV,IT,NT,IAM,KWT)
C
C Solve the system generated by REDUCE
C by Gauss elimination with complete pivoting.
C
C REDUCE already has a barrier.
C$OMP MASTER
      IF(IAM.EQ.0)THEN
         ALLOCATE(FCC(NOV+NFC),E(NOV+NFC,NOV+NFC))
         CALL DIMRGE(E,CC,CCBC,D,DDBC,FC,
     +     NTST,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
         DO II=1,NOV
            SOL(II,1)=FCC(II)
         ENDDO
         DEALLOCATE(FCC,E)
      ENDIF
C$OMP END MASTER
C
C Backsubstitution in the reduction process.
C
      CALL BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,IT,NT,IAM,KWT)
C
C Backsubstitution in the condensation of parameters process.
C
      ALLOCATE(X(NOV+1:NRA))
      CALL INFPAR(A,B,FA,SOL(1,I),FC,N,NOV,NRA,NCA,NCB,ICF,X)
      DEALLOCATE(X)
C
      RETURN
      END SUBROUTINE BRBD
C
C     ---------- -------
      SUBROUTINE SETZERO(FA,FC,NA,NRA,NFC)
C
C Arguments
      INTEGER   NA,NRA,NFC
      DOUBLE PRECISION FA(NRA,*),FC(*)
C
C Local
      INTEGER    I,J
C
      DO I=1,NA
        DO J=1,NRA
          FA(J,I)=0.D0
        ENDDO
      ENDDO
C
      DO I=1,NFC
        FC(I)=0.D0
      ENDDO
C
      RETURN
      END SUBROUTINE SETZERO
C
C     ---------- ------
      SUBROUTINE CONPAR(NOV,NRA,NCA,A,NCB,B,NRC,C,D,FA,FC,IRF,ICF,IAMAX,
     +     NLLV)
C
C Arguments
      INTEGER, INTENT(IN) :: NOV,NRA,NCA,NCB,NRC,NLLV
      INTEGER, INTENT(OUT) :: ICF(NCA),IRF(NRA)
      DOUBLE PRECISION, INTENT(INOUT) :: A(NCA,NRA),B(NCB,NRA)
      DOUBLE PRECISION, INTENT(INOUT) :: C(NCA,NRC),D(NCB,NRC)
      DOUBLE PRECISION, INTENT(INOUT) :: FA(NRA),FC(NRC)
      INTEGER, INTENT(INOUT) :: IAMAX(NRA)
C Local
      INTEGER IC,IRP,IR,IPIV,JPIV,L
      DOUBLE PRECISION PIV,TPIV,RM,TMP
C
C Note that the summation of the adjacent overlapped part of C
C is delayed until REDUCE, in order to merge it with other communications.
C
C This is a per-CPU, per-element process function for
C Condensation of parameters (Elimination of local variables).
C
         DO IC=NOV+1,NCA-NOV
            IRP=IC-NOV
C           **Search for pivot (Complete pivoting)
            PIV = ABS(A(IAMAX(IRP),IRP))
            IPIV = IRP
            DO IR=IRP+1,NRA
               TPIV = ABS(A(IAMAX(IR),IR))
               IF(PIV.LT.TPIV)THEN
                  PIV = TPIV
                  IPIV = IR
               ENDIF
            ENDDO
C           **Move indices
            IRF(IRP)=IPIV
            JPIV=IAMAX(IPIV)
            IF(IRP.NE.IPIV)THEN
C              **Physically swap rows
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
C              **Physically swap columns
               DO IR=1,IRP-1
                  TMP=A(JPIV,IR)
                  A(JPIV,IR)=A(IC,IR)
                  A(IC,IR)=TMP
               ENDDO
            ENDIF
C           **End of pivoting; elimination starts here
            PIV=A(JPIV,IRP)
            A(JPIV,IRP)=A(IC,IRP)
            A(IC,IRP)=PIV
            DO IR=IRP+1,NRA
C              **Swap columns of A physically
               RM=A(JPIV,IR)/PIV
               A(JPIV,IR)=A(IC,IR)
               A(IC,IR)=RM
               IF(RM.NE.0.0)THEN
                  CALL IMSBRA(NOV,NCA,NRA,A(1,IR),A(1,IRP),
     +                 IC+1,IAMAX(IR),RM)
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
C              **Swap columns of C physically
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
      RETURN
C
      CONTAINS
C
C     ---------- ------
      SUBROUTINE IMSBRA(NOV,NCA,NRA,A,AP,ICP1,IAMAX,RM)
C
C Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: NOV,NRA,NCA,ICP1
      INTEGER, INTENT(OUT) :: IAMAX
C Local
      INTEGER L
      DOUBLE PRECISION PPIV,TPIV,V
C
      DO L=1,NOV
         A(L)=A(L)-RM*AP(L)
      ENDDO
      PPIV=0d0
      IAMAX=ICP1
      DO L=ICP1,NRA
         V=A(L)-RM*AP(L)
C     Also recalculate absolute maximum for current row
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
C
C     ---------- ------
      SUBROUTINE SUBRAC(NOV,NCA,C,AP,ICP1,RM)
C Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: C(*)
      INTEGER, INTENT(IN) :: NOV,NCA,ICP1
C Local
      INTEGER L
C
      DO L=1,NOV
         C(L)=C(L)-RM*AP(L)
      ENDDO
      DO L=ICP1,NCA
         C(L)=C(L)-RM*AP(L)
      ENDDO
      END SUBROUTINE SUBRAC
C
      END SUBROUTINE CONPAR
C
C     ---------- ------
      SUBROUTINE CONRHS(NOV,NRA,NCA,A,NRC,C,FA,FC,IRF)
C
C Arguments
      INTEGER   NOV,NRA,NCA
      INTEGER   NRC,IRF(NRA)
      DOUBLE PRECISION A(NCA,NRA),C(NCA,NRC)
      DOUBLE PRECISION FA(NRA),FC(*)
C
C Local
      INTEGER   IC,IR,IPIV,IRP
      DOUBLE PRECISION RM,TMP
C
C Condensation of right hand side (one element).
C
      DO IR=1,NRA-NOV
         IPIV=IRF(IR)
         IF(IR.NE.IPIV)THEN
C     **Physically swap rows
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
C
      RETURN
      END SUBROUTINE CONRHS
C
C     ---------- ------
      SUBROUTINE COPYCP(NA,NOV,NRA,NCA,A,
     +  NCB,B,NRC,C,A1,A2,BB,CC,CCLO,IT)
C
C Arguments
      INTEGER, INTENT(IN) :: NA,NOV,NRA,NCA
      INTEGER, INTENT(IN) ::  NCB,NRC,IT
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DOUBLE PRECISION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NRC,*),CCLO(NOV,NRC,*)
C
C     DOUBLE PRECISION FA(NRA,*),FAA(NOV,*)
C
C Local
      INTEGER   I,IR,IR1,IC,IC1,NAP1
C
C Copies the condensed sytem generated by CONPAR into workspace.
C
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
      ENDDO
C
      NAP1=NA+1
      DO I=1,NAP1
         DO IR=1,NRC
            DO IC=1,NOV
               IF(I.EQ.1)THEN
                  IF(IT.EQ.0)THEN
                     CC(IC,IR,I)=C(IC,IR,I)
                  ELSE
                     CCLO(IC,IR,IT)=C(IC,IR,I)
                  ENDIF
               ELSEIF(I.EQ.NAP1)THEN
                  CC(IC,IR,I)=C(NRA+IC,IR,I-1)
               ELSE
                  CC(IC,IR,I)=C(IC,IR,I)+C(NRA+IC,IR,I-1)
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C 
      RETURN
      END SUBROUTINE COPYCP
C
C     ---------- ------
      SUBROUTINE CPYRHS(NA,NOV,NRA,FAA,FA)
C
      USE AUTOMPI
C
C Arguments
      INTEGER   NA,NOV,NRA

      DOUBLE PRECISION FA(NRA,*),FAA(NOV,*)
C
C Local
      INTEGER   I,IR
C
C     **Copy the RHS
      DO I=1,NA
         DO IR=1,NOV
            FAA(IR,I)=FA(NRA-NOV+IR,I)
         ENDDO         
      ENDDO
C     
      RETURN
      END SUBROUTINE CPYRHS
C
C     ---------- ------
      SUBROUTINE REDUCE(A1,A2,BB,CC,CCLO,DD,DDD,FAA,FC,FCFC,
     +     NTST,NOV,NCB,NRC,S1,S2,IPC,IPR,IFST,NLLV,IT,NT,IAM,KWT)
C
      USE AUTOMPI
C
C Arguments
      INTEGER   NTST,NOV,NCB,NRC,IFST,NLLV,IT,NT,IAM,KWT
      INTEGER   IPC(NOV,*),IPR(NOV,*)
      DOUBLE PRECISION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NRC,*),CCLO(NOV,NRC,*)
      DOUBLE PRECISION DD(NCB,*),DDD(NCB,NRC,*)
      DOUBLE PRECISION FAA(NOV,*),FC(*),FCFC(NRC,*)
C
C Local 
      INTEGER IAMAX,I,II,J,K,PLO,PHI,BASE,NA
      ALLOCATABLE IAMAX(:)
C
      ALLOCATE(IAMAX(2*NOV))
C
      BASE=IAM*NTST/KWT
      NA=(IAM+1)*NTST/KWT-BASE
      IF(IT.EQ.0)THEN
C     Reduce non-overlapping 1st piece
         CALL REDUCER(1,NTST,BASE+1,BASE+NA/NT,CC,DD,FC)
      ELSE
         PLO = BASE+IT*NA/NT+1
         PHI = BASE+(IT+1)*NA/NT
C     Reduce non-overlapping pieces
         CALL REDUCER(1,NTST,PLO,PHI,
     +        CCLO(1,1,IT),DDD(1,1,IT),FCFC(1,IT))
      ENDIF
C
C$OMP BARRIER
C$OMP MASTER
C
C     Fix up boundaries between CC parts from COPYCP
C
      IF(IFST.EQ.1)THEN
         DO I=1,NT-1
            II=I*NA/NT+1
            DO J=1,NRC
               DO K=1,NOV
                  CC(K,J,II)=CC(K,J,II)+CCLO(K,J,I)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
C
C     Reduce overlapping pieces
      IF(NT.GT.1)
     +   CALL REDUCER(1,NTST,BASE+1,BASE+NA,CC,DD,FC)
C
C     This is where we sum into the global copy of the d array
      DO I=1,NT-1
         DO J=1,NRC
            IF(IFST.EQ.1)THEN
               DO K=1,NCB
                  DD(K,J)=DD(K,J)+DDD(K,J,I)
               ENDDO
            ENDIF
            IF(NLLV.EQ.0)THEN
               FC(J)=FC(J)+FCFC(J,I)
            ENDIF
         ENDDO
      ENDDO
C
      IF(KWT.GT.1)THEN
         CALL MPICON(S1,A1,A2,BB,CC,DD,FAA,FC,NTST,NOV,NCB,NRC,IFST)
         IF(IAM.EQ.0)
     +        CALL REDUCER(1,NTST,0,NTST,CC,DD,FC)
      ENDIF
C$OMP END MASTER
C
      DEALLOCATE(IAMAX)
      RETURN
C
      CONTAINS
C
C      --------- ---------- -------
       RECURSIVE SUBROUTINE REDUCER(LO,HI,PLO,PHI,CCLO,DD,FC)
C
C Arguments
       INTEGER   LO,HI,PLO,PHI
       DOUBLE PRECISION CCLO(NOV,NRC),DD(NCB,*),FC(*)
C
C Local 
       INTEGER IR,IC,I0,I1,I2,MID
C
       IF(HI.LT.PLO.OR.LO.GT.PHI)RETURN
C This is a check for the master reduction so it will stop as soon
C as there is no more overlap (already handled by workers).
       IF(PLO.EQ.0)THEN
          IF((LO*KWT-1)/NTST.EQ.(HI*KWT-1)/NTST)RETURN
       ELSEIF(NT.GT.1.AND.PHI-PLO.EQ.NA-1.AND.LO.GT.BASE)THEN
          IF(((LO-BASE)*NT-1)/NA.EQ.((HI-BASE)*NT-1)/NA)RETURN
       ENDIF
C
C Use nested dissection for reduction; this is naturally a recursive
C procedure.
C
       MID=(LO+HI)/2
C
       IF(LO.LT.MID)
     +    CALL REDUCER(LO,MID,PLO,PHI,CCLO,DD,FC)
C
       IF(MID+1.LT.HI)
     +    CALL REDUCER(MID+1,HI,PLO,PHI,CCLO,DD,FC)
C
C Thread is not in the [PLO,PHI] range: return
       IF(LO.LT.PLO.OR.HI.GT.PHI)RETURN
C
C Initialization
C
       I0=LO-BASE
       I1=MID-BASE
       I2=HI-BASE
       IF(IFST.EQ.1)THEN
          IF(LO.EQ.MID)THEN
             DO IR=1,NOV
                DO IC=1,NOV
                   S1(IC,IR,I1)=A1(IC,IR,I1)
                ENDDO
             ENDDO
          ENDIF
          IF(MID+1.LT.HI)THEN
             DO IR=1,NOV
                DO IC=1,NOV
                   A1(IC,IR,I1+1)=S1(IC,IR,I2)
                ENDDO
             ENDDO
          ENDIF
C
          IF(LO.EQ.PLO)THEN
             CALL REDBLK(S1(1,1,I1),A2(1,1,I1),S2(1,1,I1),BB(1,1,I1),
     +                   S1(1,1,I2),A1(1,1,I1+1),A2(1,1,I2),BB(1,1,I2),
     +                   CCLO,      CC(1,1,I1+1),CC(1,1,I2+1),DD,
     +                   IPC(1,I1),IPR(1,I1),IAMAX,NOV,NCB,NRC)
          ELSE
             CALL REDBLK(S1(1,1,I1),A2(1,1,I1),S2(1,1,I1),BB(1,1,I1),
     +                   S1(1,1,I2),A1(1,1,I1+1),A2(1,1,I2),BB(1,1,I2),
     +                   CC(1,1,I0),CC(1,1,I1+1),CC(1,1,I2+1),DD,
     +                   IPC(1,I1),IPR(1,I1),IAMAX,NOV,NCB,NRC)
          ENDIF
       ENDIF
       IF(NLLV.EQ.0)THEN
          CALL REDRHSBLK(A2(1,1,I1),FAA(1,I1),
     +         A1(1,1,I1+1),FAA(1,I2),
     +         CC(1,1,I1+1),FC,NOV,NRC,IPR(1,I1))
       ENDIF
C
       RETURN
       END SUBROUTINE REDUCER
C
C      ---------- ------
       SUBROUTINE REDBLK(S11,A21,S21,BB1,
     +                   S12,A12,A22,BB2,
     +                   CC1,CC2,CC3,DD,
     +     IPC,IPR,IAMAX,NOV,NCB,NRC)
C
C Arguments
       INTEGER   NOV,NCB,NRC
       INTEGER   IPC(NOV),IPR(NOV),IAMAX(NOV*2)
       DOUBLE PRECISION S11(NOV,NOV),A21(NOV,NOV),S21(NOV,NOV)
       DOUBLE PRECISION S12(NOV,NOV),A12(NOV,NOV),A22(NOV,NOV)
       DOUBLE PRECISION CC1(NOV,NRC),CC2(NOV,NRC),CC3(NOV,NRC)
       DOUBLE PRECISION BB1(NCB,NOV),BB2(NCB,NOV), DD(NCB,NRC)
C
C Local
       INTEGER K1,K2,IR,IC,ICP1,IPIV1,IPIV2,JPIV,JPIV1,JPIV2
       DOUBLE PRECISION PIV1,PIV2,TPIV,TMP
C
         DO K1=1,NOV
            DO K2=1,NOV
               S21(K2,K1) = 0.0D0
               S12(K2,K1) = 0.0D0
            ENDDO
         ENDDO
C
         DO K1=1,NOV
            IAMAX(K1)=IDAMAX(NOV,A21(1,K1),1)
         ENDDO
         DO K1=1,NOV
            IAMAX(NOV+K1)=IDAMAX(NOV,A12(1,K1),1)
         ENDDO
C
         DO IC=1,NOV
            ICP1=IC+1
C
C Complete pivoting; rows are swapped physically, columns also
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
C
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
C
C rows and columns are swapped physically
C
            IF(PIV1.GE.PIV2)THEN
               JPIV        = JPIV1
               IPR(IC)     = IPIV1
               IF(IC.NE.IPIV1)
     +            CALL REDSWP(IC,NOV,NCB,
     +              S11(1,IC),S11(1,IPIV1),A21(1,IC),A21(1,IPIV1),
     +              S21(1,IC),S21(1,IPIV1),BB1(1,IC),BB1(1,IPIV1))
            ELSE
               JPIV        = JPIV2
               IPR(IC)     = NOV+IPIV2
               CALL REDSWP(IC,NOV,NCB,
     +              S11(1,IC),S12(1,IPIV2),A21(1,IC),A12(1,IPIV2),
     +              S21(1,IC),A22(1,IPIV2),BB1(1,IC),BB2(1,IPIV2))
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
C
C End of pivoting; Elimination starts here
C
            DO IR=ICP1,NOV
               CALL REDELIM(IC,NOV,NCB,IAMAX(IR),JPIV,
     +              A21(1,IR),A21(1,IC),S11(1,IR),S11(1,IC),
     +              S21(1,IR),S21(1,IC),BB1(1,IR),BB1(1,IC))
            ENDDO
C     
            DO IR=1,NOV
               CALL REDELIM(IC,NOV,NCB,IAMAX(NOV+IR),JPIV,
     +              A12(1,IR),A21(1,IC),S12(1,IR),S11(1,IC),
     +              A22(1,IR),S21(1,IC),BB2(1,IR),BB1(1,IC))
            ENDDO
C     
            DO IR=1,NRC
               TMP          = CC2(IC,IR)
               CC2(IC,IR)   = CC2(JPIV,IR)
               CC2(JPIV,IR) = TMP
               CALL REDELIM(IC,NOV,NCB,0,JPIV,
     +              CC2(1,IR),A21(1,IC),CC1(1,IR),S11(1,IC),
     +              CC3(1,IR),S21(1,IC),DD(1,IR),BB1(1,IC))
            ENDDO
         ENDDO
C
       END SUBROUTINE REDBLK

C      ---------- ------
       SUBROUTINE REDSWP(IC,NOV,NCB,
     +     S11,S12,A12,A21,S21,A22,BB1,BB2)
C
       INTEGER IC,NOV,NCB
       DOUBLE PRECISION S11(NOV),S12(NOV),A12(NOV),A21(NOV)
       DOUBLE PRECISION S21(NOV),A22(NOV),BB1(NCB),BB2(NCB)
C
       INTEGER L
       DOUBLE PRECISION TMP
C Swapping
       DO L=1,NOV
          TMP    = S11(L)
          S11(L) = S12(L)
          S12(L) = TMP                  
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
       END SUBROUTINE REDSWP
C
C      ---------- -------
       SUBROUTINE REDELIM(IC,NOV,NCB,IAMAX,JPIV,
     +     A12,A21,S12,S11,A22,S21,BB2,BB1)
C
       INTEGER IC,NOV,NCB,IAMAX,JPIV
       DOUBLE PRECISION A12(NOV),A21(NOV),S12(NOV),S11(NOV)
       DOUBLE PRECISION A22(NOV),S21(NOV),BB1(NCB),BB2(NCB)
C
       INTEGER L
       DOUBLE PRECISION RM,V,PPIV,TPIV
C
       RM = A12(IC)/A21(IC)
       A12(IC) = RM
C
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
C     Also recalculate absolute maximum for current row
                A12(L)=V
                TPIV=DABS(V)
                IF(PPIV.LT.TPIV)THEN
                   PPIV=TPIV
                   IAMAX=L
                ENDIF
             ENDDO
          ENDIF
          DO L=1,NOV
             S12(L) = S12(L)-RM*S11(L)
             A22(L) = A22(L)-RM*S21(L)
          ENDDO
          DO L=1,NCB
             BB2(L) = BB2(L)-RM*BB1(L)
          ENDDO
       ELSEIF(IAMAX.EQ.JPIV)THEN
C     recalculate absolute maximum for current row
          IF(IC<NOV)THEN
             IAMAX = IC+IDAMAX(NOV-IC,A12(IC+1),1)
          ENDIF
       ELSEIF(IAMAX.EQ.IC)THEN
          IAMAX = JPIV
       ENDIF
C
       END SUBROUTINE REDELIM
C
      END SUBROUTINE REDUCE
C
C     ---------- ---------
      SUBROUTINE REDRHSBLK(A21,FAA1,A12,FAA2,CC,FC,NOV,NRC,IPR)
C
C Arguments
      INTEGER   NOV,NRC,IPR(NOV)
      DOUBLE PRECISION A12(NOV,NOV),A21(NOV,NOV)
      DOUBLE PRECISION CC(NOV,NRC)
      DOUBLE PRECISION FAA1(NOV),FAA2(NOV),FC(*)
C
C Local
      INTEGER IC,IR,IPIV1,L1
      DOUBLE PRECISION RM
C
C Reduce with the right hand side for one block
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
C
      RETURN
      END SUBROUTINE REDRHSBLK
C
C     ---------- ------
      SUBROUTINE DIMRGE(E,CC,CCBC,D,DDBC,FC,
     +  NA,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
C
      USE SUPPORT
C Arguments
      INTEGER   NA,NFC,NBC,NOV,NCB,IDB,NLLV
      DOUBLE PRECISION E(NOV+NFC,*),CC(NOV,NFC-NBC,*),CCBC(NOV,NBC,2)
      DOUBLE PRECISION D(NCB,*),DDBC(NCB,*),P0(NOV,*),P1(NOV,*)
      DOUBLE PRECISION S(NOV,NOV,*),FAA(NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),FC(*),FCC(*)
      DOUBLE PRECISION, INTENT(OUT) :: DET
C
C Local
      INTEGER  I,J,NCR,NRC
      DOUBLE PRECISION, ALLOCATABLE :: XE(:)
      ALLOCATE(XE(NOV+NFC))
C
      NCR     = NFC+NOV
      NRC     = NFC-NBC
C     
C Copy
      DO I=1,NOV
         DO J=1,NOV
            E(I,J)     = S(J,I,NA)
            P0(I,J)    = S(J,I,NA)
            E(I,NOV+J) = A2(J,I,NA)
            P1(I,J)    = A2(J,I,NA)
         ENDDO
         DO J=1,NCB
            E(I,2*NOV+J) = BB(J,I,NA)
         ENDDO
      ENDDO
C     
      DO I=1,NBC
         DO J=1,NOV
            E(NOV+I,J)     = CCBC(J,I,1)
            E(NOV+I,NOV+J) = CCBC(J,I,2)
         ENDDO
         DO J=1,NCB
            E(NOV+I,2*NOV+J) = DDBC(J,I)
         ENDDO
      ENDDO
      DO I=1,NRC
         DO J=1,NOV
            E(NOV+NBC+I,J)       = CC(J,I,1)
            E(NOV+NBC+I,NOV+J)   = CC(J,I,NA+1)
         ENDDO
         DO J=1,NCB
            E(NOV+NBC+I,2*NOV+J) = D(J,I)
         ENDDO
      ENDDO
C
      DO I=1,NOV
         XE(I)=FAA(I,NA)
      ENDDO
C
      DO I=1,NFC
         XE(NOV+I) = FC(I)
      ENDDO
C     
      IF(IDB.GE.3)THEN
         WRITE(9,101)
         WRITE(9,100)(XE(I),I=1,NCR)
      ENDIF
C     
      IF(IDB.GE.4)THEN
         WRITE(9,102)
         DO I=1,NCR
            WRITE(9,100)(E(I,J),J=1,NCR)
         ENDDO
      ENDIF
C
C Solve for FCC
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
C

 100  FORMAT(1X,10E11.3)
 101  FORMAT(/,1X,'Residuals of reduced system:')
 102  FORMAT(/,1X,'Reduced Jacobian matrix:')
 103  FORMAT(/,1X,'Solution vector:')
C     
      DO I=1,NFC
         FC(I)=FCC(NOV+I)
      ENDDO
C
      DEALLOCATE(XE)
      RETURN
      END SUBROUTINE DIMRGE
C
C     ---------- -------
      SUBROUTINE BCKSUB1(S1,A2,S2,BB,FAA,FCC,SOL1,SOL2,FC,NOV,NCB,IPC)
C
C Arguments
      INTEGER   NOV,NCB,IPC(NOV)
      DOUBLE PRECISION S1(NOV,NOV),S2(NOV,NOV)
      DOUBLE PRECISION A2(NOV,NOV),BB(NCB,NOV)
      DOUBLE PRECISION SOL1(NOV),SOL2(NOV),FAA(NOV),FC(*),FCC(*)
C
C Local
      INTEGER K,L
      DOUBLE PRECISION SM,TMP
C
C
C Backsubstitution process for 1 block row
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
C     Revert column pivoting on SOL1
      DO K=NOV,1,-1
         TMP=SOL1(K)
         SOL1(K)=SOL1(IPC(K))
         SOL1(IPC(K))=TMP
      ENDDO
C
      RETURN
      END SUBROUTINE BCKSUB1
C
C     ---------- ------
      SUBROUTINE BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,
     +     IT,NT,IAM,KWT)
C
      USE AUTOMPI
C
C Arguments
      INTEGER   NTST,NOV,NCB,IPC(NOV,*),IT,NT,IAM,KWT
      DOUBLE PRECISION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION A2(NOV,NOV,*),BB(NCB,NOV,*)
      DOUBLE PRECISION SOL(NOV,*),FAA(NOV,*),FC(*)
C
C Local
      INTEGER   I,PLO,PHI,BASE,NA
C
      BASE=IAM*NTST/KWT
      NA=(IAM+1)*NTST/KWT-BASE
C$OMP MASTER
C do global backsubsitution until there is no overlap left
      IF(IAM.EQ.0)THEN
         DO I=1,NOV
            SOL(I,NTST+1) = FC(I)
         ENDDO
      ENDIF
      IF(KWT.GT.1)THEN
         IF(IAM.EQ.0)
     +        CALL BCKSUBR(1,NTST,0,NTST)
         CALL MPIBCAST(FC,NOV+NCB)
         CALL MPISCAT(SOL,NOV,NTST,NOV)
      ENDIF
      IF(NT.GT.1)
     +     CALL BCKSUBR(1,NTST,BASE+1,BASE+NA)
C$OMP END MASTER
C$OMP BARRIER
      PLO=BASE+IT*NA/NT+1
      PHI=BASE+(IT+1)*NA/NT
      CALL BCKSUBR(1,NTST,PLO,PHI)
C
      RETURN
      CONTAINS

C      Back substitution within the interval [PLO,PHI]; no overlap
C      --------- ---------- -------
       RECURSIVE SUBROUTINE BCKSUBR(LO,HI,PLO,PHI)
C
C Arguments
       INTEGER   LO,HI,PLO,PHI
C
C Local
       INTEGER MID,I,I0,I1
C
       IF(LO.GE.HI.OR.HI.LT.PLO.OR.LO.GT.PHI)RETURN
       IF(PLO.EQ.0)THEN
          IF((LO*KWT-1)/NTST.EQ.(HI*KWT-1)/NTST)RETURN
       ELSEIF(NT.GT.1.AND.PHI-PLO.EQ.NA-1.AND.LO.GT.BASE)THEN
          IF(((LO-BASE)*NT-1)/NA.EQ.((HI-BASE)*NT-1)/NA)RETURN
       ENDIF
       MID=(LO+HI)/2
       I=MID-BASE
       I0=LO-BASE
       I1=HI-BASE
       IF(PLO.LE.LO.AND.HI.LE.PHI)THEN
          CALL BCKSUB1(S1(1,1,I),A2(1,1,I),S2(1,1,I),BB(1,1,I),
     +         FAA(1,I),SOL(1,I0),SOL(1,I+1),SOL(1,I1+1),FC(NOV+1),
     +         NOV,NCB,IPC(1,I))
       ENDIF
       CALL BCKSUBR(MID+1,HI,PLO,PHI)
       CALL BCKSUBR(LO,MID,PLO,PHI)
C     
       RETURN
       END SUBROUTINE BCKSUBR
C
      END SUBROUTINE BCKSUB
C
C     ---------- ------
      SUBROUTINE INFPAR(A,B,FA,SOL,FC,NA,NOV,NRA,NCA,NCB,ICF,X)
C
C  Arguments
      INTEGER   NA,NOV,NRA,NCA,NCB,ICF(NCA,*)
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION SOL(NOV,*),X(NOV+1:NRA)
C
C Local
      INTEGER I,J,IR
      DOUBLE PRECISION SM,TMP
C
C Determine the local varables by backsubstitition.
C
C Backsubstitution in the condensation of parameters; no communication.
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
C        **Copy SOL into FA 
         DO J=1,NOV
            FA(J,I)=SOL(J,I)
         ENDDO
C        **Undo pivots and copy X into FA
         DO J=NRA,NOV+1,-1
            TMP=X(ICF(J,I))
            X(ICF(J,I))=X(J)
            X(J)=TMP
         ENDDO
         DO J=NOV+1,NRA
            FA(J,I)=X(J)
         ENDDO
      ENDDO
C     
      RETURN
      END SUBROUTINE INFPAR
C           
C     ---------- ------
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DD,DDBC,FA,
     + FC,FCFC,NT,IFST,NLLV)
C
      INTEGER, INTENT(IN) :: NA,NRA,NCA,NCB,NFC,NBC,NT,IFST,NLLV
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DOUBLE PRECISION CCBC(NCA-NRA,NBC,*),D(NCB,*),DD(NCB,NFC-NBC,*)
      DOUBLE PRECISION DDBC(NCB,*),FA(NRA,*),FC(*),FCFC(NFC-NBC,*)

      INTEGER I,IR,IC
      DOUBLE PRECISION FC1,D1
C
       WRITE(9,101)
       DO I=1,NA
         WRITE(9,102)I
         DO IR=1,NRA
           IF(NLLV==0)THEN
              WRITE(9,103)(A(IC,IR,I),IC=1,NCA),(B(IC,IR,I),IC=1,NCB)
     *             ,FA(IR,I)
           ELSE
              WRITE(9,103)(A(IC,IR,I),IC=1,NCA),(B(IC,IR,I),IC=1,NCB)
     *             ,0d0
           ENDIF
         ENDDO
       ENDDO
C
       WRITE(9,104)
       DO I=1,NA
         WRITE(9,102)I
         DO IR=1,NFC
           IF(IR.GT.NBC)THEN
             WRITE(9,103)(C(IC,IR-NBC,I),IC=1,NCA)
           ELSEIF(I.EQ.1)THEN
             WRITE(9,103)(CCBC(IC,IR,1),IC=1,NCA-NRA)
           ELSEIF(I.EQ.NA)THEN
             WRITE(9,103)(CCBC(IC,IR,2),IC=1,NCA-NRA)
           ENDIF
         ENDDO
       ENDDO
C
       WRITE(9,105)
       DO IR=1,NBC
         WRITE(9,103)(DDBC(IC,IR),IC=1,NCB),FC(IR)
       ENDDO
       DO IR=1,NFC-NBC
         DO IC=1,NCB
           D1=D(IC,IR)
           IF(IFST==1)THEN
              DO I=1,NT-1
                 D1=D1+DD(IC,IR,I)
              ENDDO
           ENDIF
           WRITE(9,103)D1
         ENDDO
         IF(NLLV==0)THEN
            FC1=FC(NBC+IR)
            DO I=1,NT-1
               FC1=FC1+FCFC(IR,I)
            ENDDO
         ELSE
            FC1=0
         ENDIF
         WRITE(9,103)FC1
       ENDDO
C
 101   FORMAT(' AA , BB , FA (Full dimension) :')
 102   FORMAT(' I=',I3)
 103   FORMAT(1X,12E10.3)
 104   FORMAT(' CC (Full dimension) :')
 105   FORMAT(' DD , FC')
C
      RETURN
      END SUBROUTINE PRINT1

      END MODULE SOLVEBV
