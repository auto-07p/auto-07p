C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C           Setting up of the Jacobian and right hand side
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      MODULE SOLVEBV

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
      SUBROUTINE SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDS,
     * NLLV,RLCUR,RLOLD,RLDOT,NDX,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,
     * FA,FC,P0,P1,THL,THU)
C
C$    USE OMP_LIB
      USE AUTOMPI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Sets up and solves the linear equations for one Newton/Chord iteration
C
      EXTERNAL FUNI,BCNI,ICNI
      DIMENSION IAP(*),RAP(*),FC(*),PAR(*),ICP(*),RLOLD(*),RLCUR(*)
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),UOLDPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION FA(IAP(1)*IAP(6),*),DTM(*),RLDOT(*)
      DOUBLE PRECISION P0(*),P1(*),THL(*),THU(*)
C
C Local
      ALLOCATABLE A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:)
      ALLOCATABLE S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:)
      ALLOCATABLE CCLO(:,:,:),CCBC(:,:,:)
      ALLOCATABLE DDBC(:,:),DD(:,:,:),FCFC(:,:),FAA(:,:),SOL(:,:)
      ALLOCATABLE ICF(:,:),IRF(:,:),IPR(:,:),IPC(:,:),NP(:)
      SAVE A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,DDBC,ICF,IRF,IPR,IPC
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
      IAM=IAP(38)
      KWT=IAP(39)
C
      NDIM=IAP(1)
      NTST=IAP(5)
      NCOL=IAP(6)
      NBC=IAP(12)
      NINT=IAP(13)
      IID=IAP(18)
      NFPR=IAP(29)
      NPAR=IAP(31)
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
      MNT = 1
C$    MNT = OMP_GET_MAX_THREADS()
      IF(MNT.GT.NA)THEN
         MNT=NA
C$       CALL OMP_SET_NUM_THREADS(NA)
      ENDIF
      IF(IFST.EQ.1)THEN
         IF(ALLOCATED(A))THEN
C           Free floating point arrays
            DEALLOCATE(A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,DDBC)
C           Free integer arrays
            DEALLOCATE(ICF,IRF,IPR,IPC)
         ENDIF
C
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
      IF(IAM.EQ.0)THEN
C
         DO I=1,NFPR
            PAR(ICP(I))=RLCUR(I)
         ENDDO
C
C     ** Time evolution computations (parabolic systems)
         IPS=IAP(2)
         IF(IPS.EQ.14 .OR. IPS.EQ.16)RAP(15)=RLOLD(1)
         CALL SUBVBC(NDIM,NTST,NBC,NFPR,BCNI,NDX,
     +        IAP,RAP,PAR,NPAR,ICP,CCBC,DDBC,FC,UPS,IFST)
         CALL SUBVPSA(NFPR,RDS,D(1,NRC),FC(NFC),
     +        RLCUR,RLOLD,RLDOT,THL,IFST)
         IF(KWT.GT.1)THEN
            CALL MPISBV(IAP,RAP,PAR,ICP,NDX,UPS,UOLDPS,UDOTPS,
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
      CALL SETUBV(NDIM,NA,NCOL,NINT,NFPR,NRC,NROW,NCLM,
     +   FUNI,ICNI,NDX,IAP,RAP,PAR,NPAR,ICP,A,B,C,D,DD,FA,
     +   FC(NBC+1),FCFC,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,IT,NT,
     +   IRF,ICF,NLLV)
C
      I = IT*NA/NT+1
      CALL BRBD(A(1,1,I),B(1,1,I),C(1,1,I),D,DD,FA(1,I),FAA,FC,
     +  FCFC,P0,P1,IFST,IID,NLLV,DET,NDIM,NTST,NA,NBC,NROW,NCLM,
     +  NFPR,NFC,A1,A2,BB,CC,CCLO,CCBC,DDBC,
     +  SOL,S1,S2,IPR,IPC,ICF(1,I),IAM,KWT,IT,NT)
C
C$OMP END PARALLEL
C
      IF(MNT.GT.1)DEALLOCATE(DD,FCFC,CCLO)
      DEALLOCATE(FAA,SOL)
C
      IF(KWT.GT.1)THEN
C        Global concatenation of the solution from each node.
        CALL MPIGAT(FA,NDX,NTST)
      ENDIF
C
      IF(IAM.EQ.0)RAP(14)=DET
C
      DEALLOCATE(NP)
      RETURN
      END SUBROUTINE SOLVBV
C
C     ---------- -------
      SUBROUTINE SETFCDD(IFST,DD,FC,NCB,NRC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FC(*),DD(NCB,*)
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
      SUBROUTINE SUBVBC(NDIM,NTST,NBC,NCB,BCNI,NDX,
     + IAP,RAP,PAR,NPAR,ICP,CCBC,DDBC,FC,UPS,IFST)
C
      IMPLICIT NONE
C
C     This subroutine handles a non-parallel part of SETUBV, that is,
C     * the boundary conditions (not much to parallelize here and
C       HomCont relies on non-parallel execution): the arrays CCBC,
C       DDBC, and parts of FC.
C
      EXTERNAL BCNI
C
      INTEGER NDIM,NTST,NBC,NCB,NDX,IAP(*),ICP(*),IFST,NPAR
      DOUBLE PRECISION CCBC(NDIM,NBC,*),DDBC(NCB,*)
      DOUBLE PRECISION RAP(*),UPS(NDX,*),FC(*),PAR(*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,K
C
      ALLOCATE(UBC0(NDIM),UBC1(NDIM),FBC(NBC),DBC(NBC,2*NDIM+NPAR))
C
C     Boundary conditions :
C     
       IF(NBC.GT.0)THEN
         DO I=1,NDIM
            UBC0(I)=UPS(I,1)
            UBC1(I)=UPS(I,NTST+1)
         ENDDO
         CALL BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,IFST*2,DBC)
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
      IMPLICIT NONE
C
C     This subroutine handles a non-parallel part of SETUBV, that is,
C     * creating the parameter dependent pseudo-arclength parts of FC and D:
C       (the bottom element FCPA and row DDPA)
C
      INTEGER NCB,IFST
      DOUBLE PRECISION RDS,DDPA(*),FCPA
      DOUBLE PRECISION RLCUR(*),RLOLD(*),RLDOT(*),THL(*)
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
     + ICNI,NDX,IAP,RAP,PAR,NPAR,ICP,AA,BB,CC,DD,DDD,FA,FC,FCFC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,IAM,NT,IRF,ICF,NLLV)
C
      USE MESH
      IMPLICIT NONE
C
      INTEGER NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,NDX,IFST,IAM,NT,NPAR
      INTEGER IAP(*),ICP(*),IRF(NRA,*),ICF(NCA,*),NLLV
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION DDD(NCB,NRC,*),FCFC(NRC,*)
C
      EXTERNAL FUNI,ICNI
C
C Local
      DOUBLE PRECISION WI(NCOL+1),WP(NCOL+1,NCOL),WT(NCOL+1,NCOL)
      INTEGER I,N
C
C
      CALL WINT(NCOL+1,WI)
      CALL GENWTS(NCOL,NCOL+1,WT,WP)
C
      I = IAM*NA/NT+1
      N = (IAM+1)*NA/NT+1-I
      IF(IAM.EQ.0)THEN
         CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +        IAP,RAP,PAR,NPAR,ICP,AA,BB,CC,DD,FA,FC,UPS,
     +        UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT,IRF,ICF,IFST,NLLV)
      ELSE
         CALL SETFCDD(IFST,DDD(1,NRC,IAM),FCFC(NRC,IAM),NCB,1)
         CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +        IAP,RAP,PAR,NPAR,ICP,AA(1,1,I),BB(1,1,I),CC(1,1,I),
     +        DDD(1,1,IAM),FA(1,I),FCFC(1,IAM),UPS(1,I),UOLDPS(1,I),
     +        UDOTPS(1,I),UPOLDP(1,I),DTM(I),THU,WI,WP,WT,
     +        IRF(1,I),ICF(1,I),IFST,NLLV)
      ENDIF
C
      CONTAINS

C     ---------- ---------
      SUBROUTINE SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,NDX,IAP,RAP,PAR,NPAR,ICP,AA,BB,CC,DD,FA,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT,IRF,ICF,IFST,NLLV)
C
C     This is the per-CPU parallelized part of SETUBV
C
      EXTERNAL FUNI,ICNI
C
      INTEGER NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,IAP(*),ICP(*),NDX,NPAR
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION WI(0:*),WP(NCOL+1,*),WT(NCOL+1,*)
      INTEGER IRF(NRA,*),ICF(NCA,*),IFST,NLLV
C
C Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U,
     +  F,FICD,DICD,UIC,UIO,UID,UIP,PRM
      INTEGER I,J,K,IC,IC1,I1,J1,JP1,K1
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

       DO J=1,N
C
C Generate AA , BB and FA :
C
          DO IC=1,NCOL
             IC1=(IC-1)*NDIM+1
             DO I=1,NPAR
                PRM(I)=PAR(I)
             ENDDO
             CALL SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,NDX,IAP,RAP,PRM,ICP,
     +            AA(1,IC1,J),BB(1,IC1,J),FA(IC1,J),UPS(1,J),
     +            UOLDPS(1,J),DTM(J),WP(1,IC),WT(1,IC),DFDU,DFDP,
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
         JP1=J+1
         DO K=0,NCOL
            J1=J
            K1=K*NDIM+1
            I1=K1
            IF(K.EQ.NCOL)THEN
               J1=JP1
               I1=1
            ENDIF
C     
C     Integral constraints+pseudo-arclength equation :
C     
            DO I=1,NPAR
               PRM(I)=PAR(I)
            ENDDO
            CALL SBVICN(NDIM,NINT,NCB,NCA,ICNI,IAP,RAP,PRM,ICP,
     +           CC(K1,1,J),DD,FC,UPS(I1,J1),UOLDPS(I1,J1),
     +           UDOTPS(I1,J1),UPOLDP(I1,J1),DTM(J),THU,WI(K),FICD,DICD,
     +           UIC,UIO,UID,UIP,IFST,NLLV)
         ENDDO
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
      SUBROUTINE SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,NDX,IAP,RAP,PAR,ICP,
     + AA,BB,FA,UPS,UOLDPS,DTM,WP,WT,DFDU,DFDP,U,UOLD,F,IFST,NLLV)
C
C     Does one call to FUNI and stores the result in AA, BB, and FA.
C
      EXTERNAL FUNI
C
      INTEGER, INTENT(IN) :: NDIM,NCOL,NCB,NCA,NDX,IAP(*),ICP(*)
      INTEGER, INTENT(IN) :: IFST,NLLV
      DOUBLE PRECISION, INTENT(IN) :: DTM,PAR(*),WT(0:*),WP(0:*)
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION, INTENT(OUT) :: AA(NCA,*),BB(NCB,*),FA(*),U(*)
      DOUBLE PRECISION, INTENT(OUT) :: UOLD(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*),RAP(*)
C
C Local
      DOUBLE PRECISION WPLOC(0:NCOL),WTTMP,TMP
      INTEGER I,IB,IB1,J,K,L,L1
C
      DO K=1,NDIM
         U(K)=   WT(NCOL)*   UPS(K,2)
         UOLD(K)=WT(NCOL)*UOLDPS(K,2)
         DO L=0,NCOL-1
            L1=L*NDIM+K
            U(K)=U(K)        +WT(L)*   UPS(L1,1)
            UOLD(K) =UOLD(K) +WT(L)*UOLDPS(L1,1)
         ENDDO
      ENDDO
      CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PAR,IFST*2,F,DFDU,DFDP)
      DO IB=0,NCOL
         WPLOC(IB)=WP(IB)/DTM
      ENDDO
      IF(IFST.EQ.1)THEN
         DO I=1,NDIM
C     transpose DFDU for optimal access
            DO J=I+1,NDIM
               TMP=DFDU(I,J)
               DFDU(I,J)=DFDU(J,I)
               DFDU(J,I)=TMP
            ENDDO
            DO IB=0,NCOL
               WTTMP=-WT(IB)
               IB1=IB*NDIM
               DO K=1,NDIM
                  AA(IB1+K,I)=WTTMP*DFDU(K,I)
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
            FA(I)=F(I)-WPLOC(NCOL)*UPS(I,2)
            DO K=0,NCOL-1
               FA(I)=FA(I)-WPLOC(K)*UPS(K*NDIM+I,1)
            ENDDO
         ENDDO
      ENDIF
      END SUBROUTINE SBVFUN
C
C     ---------- ------
      SUBROUTINE SBVICN(NDIM,NINT,NCB,NCA,ICNI,IAP,RAP,PAR,ICP,CC,DD,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,FICD,DICD,UIC,UIO,UID,UIP,
     + IFST,NLLV)
C
      IMPLICIT NONE
C
C     Does one call to ICNI (integral constraints) and stores the
C     result in CC, DD and FC; and stores the pseudo-arclength
C     result too.
C
      EXTERNAL ICNI
      INTEGER, INTENT(IN) :: NDIM,NINT,NCB,NCA,IAP(*),ICP(*),IFST,NLLV
      DOUBLE PRECISION, INTENT(IN) :: RAP(*),PAR(*),UPS(*),UDOTPS(*)
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
         CALL ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,
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
     +  SOL,S1,S2,IPR,IPC,ICF,IAM,KWT,IT,NT)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   IFST,IDB,NLLV,NOV,NTST,NA,NBC,NRA
      INTEGER   NCA,NCB,NFC,IAM,KWT,IT,NT
      DOUBLE PRECISION, INTENT(OUT) :: DET
      DOUBLE PRECISION A(*),B(*),C(NCA,NFC-NBC,*),D(NCB,*)
      DOUBLE PRECISION DD(NCB,NFC-NBC,*)
      DOUBLE PRECISION FA(*),FAA(NOV,*),FC(*),FCFC(NFC-NBC,*),P0(*)
      DOUBLE PRECISION P1(*),A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NFC-NBC,*)
      DOUBLE PRECISION CCLO(NOV,NFC-NBC,*)
      DOUBLE PRECISION CCBC(*),DDBC(*),SOL(NOV,*),S1(*),S2(*)
      INTEGER   IPR(*),IPC(*),ICF(*)
C
C Local
      DOUBLE PRECISION FCC,E,X
      INTEGER I,II,N,NRC
      ALLOCATABLE FCC(:),E(:,:),X(:)
C
      NRC=NFC-NBC
C
      IF(IDB.GT.4.and.IAM.EQ.0.and.IT.EQ.0)
     +     CALL PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DD,DDBC,
     +     FA,FC,FCFC,NT)
C
      I = IT*NA/NT+1
      N = (IT+1)*NA/NT+1-I
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
C REDUCE already has a barrier.
C$OMP MASTER
C
      IF(IAM.EQ.0)THEN
         ALLOCATE(FCC(2*NOV+NFC+2*NOV*NOV+1),E(NOV+NFC,2*NOV+NFC))
C
         CALL DIMRGE(E,CC,CCBC,D,DDBC,FC,
     +     NTST,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
         DO II=1,NOV
            SOL(II,1)=FCC(II)
         ENDDO
C
         DEALLOCATE(FCC,E)
      ENDIF
C
C$OMP END MASTER
C
      CALL BCKSUB(S1,A2,S2,BB,FAA,SOL,FC,NTST,NOV,NCB,IPC,IT,NT,IAM,KWT)
C
      ALLOCATE(X(NRA))
      CALL INFPAR(A,B,FA,SOL(1,I),FC,N,NOV,NRA,NCA,NCB,ICF,X)
      DEALLOCATE(X)
C
      RETURN
      END SUBROUTINE BRBD
C
C     ---------- -------
      SUBROUTINE SETZERO(FA,FC,NA,NRA,NFC)
C
      IMPLICIT NONE
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
      IMPLICIT NONE
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
      IMPLICIT NONE
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
      IMPLICIT NONE
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
      IMPLICIT NONE
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NOV,NRA,NCA
      INTEGER   NCB,NRC,IT
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION BB(NCB,NOV,*),CC(NOV,NRC,*),CCLO(NOV,NRC,*)
C
C     DIMENSION FA(NRA,*),FAA(NOV,*)
C
C Local
      INTEGER   I,IR,IC
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NOV,NRA

      DIMENSION FA(NRA,*),FAA(NOV,*)
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
      IMPLICIT NONE
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
       IMPLICIT NONE
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
       IMPLICIT NONE
C
       INTEGER IC,NOV,NCB,IAMAX,JPIV
       DOUBLE PRECISION A12(NOV),A21(NOV),S12(NOV),S11(NOV)
       DOUBLE PRECISION A22(NOV),S21(NOV),BB1(NOV),BB2(NOV)
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
          IAMAX = IC+IDAMAX(NOV-IC,A12(IC+1),1)
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
      IMPLICIT NONE
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
      IMPLICIT NONE
C
C Arguments
      INTEGER   NA,NFC,NBC,NOV,NCB,IDB,NLLV
      DOUBLE PRECISION E(NOV+NFC,*),CC(NOV,NFC-NBC,*),CCBC(NOV,NBC,*)
      DOUBLE PRECISION D(NCB,*),DDBC(NCB,*),P0(NOV,*),P1(NOV,*)
      DOUBLE PRECISION S(NOV,NOV,*),FAA(NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),FC(*),FCC(*)
      DOUBLE PRECISION, INTENT(OUT) :: DET
C
C Local
      INTEGER  I,J,K,K1,K2,KC,KR,NAP1,NCR,NRC,NOVPI,NOVPJ,NOVPJ2
      DOUBLE PRECISION XE
      ALLOCATABLE XE(:)
      ALLOCATE(XE(NOV+NFC))
C
      NAP1    = NA+1
      NCR     = NFC+NOV
      NRC     = NFC-NBC
C     
C Copy
      DO I=1,NOV
         DO J=1,NOV
            NOVPJ      = NOV+J
            E(I,J)     = S(J,I,NA)
            P0(I,J)    = S(J,I,NA)
            E(I,NOVPJ) = A2(J,I,NA)
            P1(I,J)    = A2(J,I,NA)
         ENDDO
         DO J=1,NCB
            NOVPJ2      = 2*NOV+J
            E(I,NOVPJ2) = BB(J,I,NA)
         ENDDO
      ENDDO
C     
      DO I=1,NBC
         NOVPI=NOV+I
         DO J=1,NOV
            NOVPJ          = NOV+J
            E(NOVPI,J)     = CCBC(J,I,1)
            E(NOVPI,NOVPJ) = CCBC(J,I,2)
         ENDDO
         DO J=1,NCB
            NOVPJ2          = 2*NOV+J
            E(NOVPI,NOVPJ2) = DDBC(J,I)
         ENDDO
      ENDDO
      DO I=1,NRC
         NOVPI=NOV+NBC+I
         DO J=1,NOV
            NOVPJ          = NOV+J
            E(NOVPI,J)     = CC(J,I,1)
            E(NOVPI,NOVPJ) = CC(J,I,NAP1)
         ENDDO
         DO J=1,NCB
            NOVPJ2          = 2*NOV+J
            E(NOVPI,NOVPJ2) = D(J,I)
         ENDDO
      ENDDO
C
      DO I=1,NOV
         XE(I)=FAA(I,NA)
      ENDDO
C
      DO I=1,NFC
         NOVPI     = NOV+I
         XE(NOVPI) = FC(I)
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
      IF(NLLV.EQ.0)THEN
         CALL GEL(NCR,E,1,FCC,XE,DET)
      ELSEIF(NLLV.GT.0)THEN
         CALL NLVC(NCR,NCR,NLLV,E,FCC)
      ELSE
         DO I=1,NCR-1
            XE(I)=0.D0
         ENDDO
         XE(NCR)=1.D0
         CALL GEL(NCR,E,1,FCC,XE,DET)
      ENDIF

      IF(IDB.GE.4)THEN
         WRITE(9,103)
         WRITE(9,100)(FCC(I),I=1,NCR)
      ENDIF
C
      K1=NCR
      K2=K1+NOV**2
      DO KR=1,NOV
         DO KC=1,NOV
            K=(KR-1)*NOV+KC
            FCC(K1+K)=P0(KR,KC)
            FCC(K2+K)=P1(KR,KC)
         ENDDO
      ENDDO
      FCC(NCR+2*NOV**2+1)=DET
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
      K1=NCR
      K2=K1+NOV**2
      DO KR=1,NOV
         DO KC=1,NOV
            K=(KR-1)*NOV+KC
            P0(KR,KC) = FCC(K1+K)
            P1(KR,KC) = FCC(K2+K)
         ENDDO
      ENDDO
      DET=FCC(NCR+2*NOV**2+1)
C
      DEALLOCATE(XE)
      RETURN
      END SUBROUTINE DIMRGE
C
C     ---------- -------
      SUBROUTINE BCKSUB1(S1,A2,S2,BB,FAA,FCC,SOL1,SOL2,FC,NOV,NCB,IPC)
C
      IMPLICIT NONE
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
      IMPLICIT NONE
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
         CALL MPISCAT(SOL,NOV,NTST,1)
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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  Arguments
      INTEGER   NA,NOV,NRA,NCA,NCB,ICF(NCA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),FA(NRA,*),FC(*)
      DIMENSION SOL(NOV,*),X(*)
C
C Local
      DOUBLE PRECISION SM
C
C Determine the local varables by backsubstitition.
C
      NRAM=NRA-NOV
C
C Backsubstitution in the condensation of parameters; no communication.
      DO I=1,NA
         DO IR=NRAM,1,-1
            IRP1=IR+1
            SM=FA(IR,I)
            DO J=1,NOV
               NRAPJ=NRA+J
               SM=SM-A(J,IR,I)*SOL(J,I)
               SM=SM-A(NRAPJ,IR,I)*SOL(J,I+1)
            ENDDO
            DO J=1,NCB
               SM=SM-B(J,IR,I)*FC(NOV+J)
            ENDDO
            DO J=IRP1,NRAM
               J1=J+NOV
               SM=SM-A(J1,IR,I)*X(J1)
            ENDDO
            NOVPIR=NOV+IR
            X(NOVPIR)=SM/A(NOVPIR,IR,I)
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
C     
      RETURN
      END SUBROUTINE INFPAR
C           
C     ---------- ------
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DD,DDBC,FA,
     + FC,FCFC,NT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DIMENSION CCBC(NCA-NRA,NBC,*),D(NCB,*),DD(NCB,NFC-NBC,*)
      DIMENSION DDBC(NCB,*),FA(NRA,*),FC(*),FCFC(NFC-NBC,*)
C
       WRITE(9,101)
       DO I=1,NA
         WRITE(9,102)I
         DO IR=1,NRA
           WRITE(9,103)(A(IC,IR,I),IC=1,NCA),(B(IC,IR,I),IC=1,NCB)
     *     ,FA(IR,I)
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
           IF(NT.GT.1)THEN
             DO I=1,NT-1
               D1=D1+DD(IC,IR,I)
             ENDDO
           ENDIF
           WRITE(9,103)D1
         ENDDO
         FC1=FC(NBC+IR)
         IF(NT.GT.1)THEN
           DO I=1,NT-1
             FC1=FC1+FCFC(IR,I)
           ENDDO
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
