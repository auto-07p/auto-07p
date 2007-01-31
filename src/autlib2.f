C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C           Setting up of the Jacobian and right hand side
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ------
      SUBROUTINE SOLVBV(IFST,IAP,RAP,PAR,ICP,FUNI,BCNI,ICNI,RDS,
     * NLLV,RLCUR,RLOLD,RLDOT,NDX,UPS,DUPS,UOLDPS,UDOTPS,UPOLDP,DTM,
     * FA,FC,P0,P1,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Sets up and solves the linear equations for one Newton/Chord iteration
C
      EXTERNAL FUNI,BCNI,ICNI
      DIMENSION IAP(*),RAP(*),FC(*),PAR(*),ICP(*),RLOLD(*),RLCUR(*)
C
C Local
      ALLOCATABLE A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:)
      ALLOCATABLE S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:),CCBC(:,:,:)
      ALLOCATABLE DDBC(:,:)
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
     +        IAP,RAP,PAR,ICP,CCBC,DDBC,FC,UPS,IFST)
         CALL SUBVPSA(NDIM,NTST,NFPR,NDX,IAP,ICP,RDS,D(1,NRC),FC(NFC),
     +        RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU,IFST)
         IF(KWT.GT.1)THEN
            CALL MPISBV(IAP,RAP,PAR,ICP,RLDOT,NDX,UPS,UOLDPS,UDOTPS,
     +           UPOLDP,DTM,THU,IFST,NLLV)
         ENDIF
      ELSE
C     The matrix D and FC are set to zero for all nodes except the first.
C     zero pseudo-arclength part of matrices, rest is done in setubv()
         CALL SETFCDD(IFST,D(1,NRC),FC(NFC),NFPR,1)
      ENDIF
C
      CALL SETUBV(NDIM,NA,NCOL,NINT,NFPR,NRC,NROW,NCLM,
     +   FUNI,ICNI,NDX,IAP,RAP,PAR,ICP,A,B,C,D,FA,
     +   FC(NBC+1),UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST)
C
      CALL BRBD(A,B,C,D,FA,FC,P0,P1,IFST,
     +  IID,NLLV,DET,NDIM,NTST,NA,NBC,NROW,NCLM,
     +  NFPR,NFC,A1,A2,BB,CC,CCBC,DDBC,
     +  S1,S2,IPR,IPC,IRF,ICF,IAM,KWT)
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
      END
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
      END
C
C     ---------- ---------
      SUBROUTINE PARTITION(N,KWT,M)
C
C     Linear distribution of NTST over all nodes
      IMPLICIT NONE
      INTEGER N,KWT,M(KWT)
      INTEGER I,T,S
C     
        T = N / KWT
        S = MOD(N,KWT)
C     
        DO I=1,KWT
          M(I) = T
        ENDDO
C     
        DO I=1,S
          M(I) = M(I) + 1
        ENDDO
C     
      RETURN
      END
C
C     ---------- ---------
      SUBROUTINE SUBVBC(NDIM,NTST,NBC,NCB,BCNI,NDX,
     + IAP,RAP,PAR,ICP,CCBC,DDBC,FC,UPS,IFST)
C
      IMPLICIT NONE
      INTEGER NPARX,NBIFX,NIAP,NRAP
      include 'auto.h'
C
C     This subroutine handles a non-parallel part of SETUBV, that is,
C     * the boundary conditions (not much to parallelize here and
C       HomCont relies on non-parallel execution): the arrays CCBC,
C       DDBC, and parts of FC.
C
      EXTERNAL BCNI
C
      INTEGER NDIM,NTST,NBC,NCB,NDX,IAP(*),ICP(*),IFST
      DOUBLE PRECISION CCBC(NDIM,NBC,*),DDBC(NCB,*)
      DOUBLE PRECISION RAP(*),UPS(NDX,*),FC(*),PAR(*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,K
C
      ALLOCATE(UBC0(NDIM),UBC1(NDIM),FBC(NBC),DBC(NBC,2*NDIM+NPARX))
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
       END
C
C     ---------- -------
      SUBROUTINE SUBVPSA(NDIM,NTST,NCB,NDX,IAP,ICP,RDS,DDPA,FCPA,
     + RLCUR,RLOLD,RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU,IFST)
C
      IMPLICIT NONE
      INTEGER NPARX,NBIFX,NIAP,NRAP
      include 'auto.h'
C
C     This subroutine handles a non-parallel part of SETUBV, that is,
C     * creating the pseudo-arclength parts of FC and D: (the bottom
C       element FCPA and row DDPA)
C
      DOUBLE PRECISION RINPR
C
      INTEGER NDIM,NTST,NCB,NDX,IAP(*),ICP(*),IFST
      DOUBLE PRECISION RDS,DDPA(*),FCPA,DTM(*),UPS(NDX,*),DUPS(NDX,*)
      DOUBLE PRECISION UOLDPS(NDX,*),UDOTPS(NDX,*)
      DOUBLE PRECISION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      INTEGER I,J
      DOUBLE PRECISION RLSUM
C
C       Save difference :
C
       DO J=1,NTST+1
         DO I=1,NDX
            DUPS(I,J)=UPS(I,J)-UOLDPS(I,J)
          ENDDO
       ENDDO
C
C     Pseudo-arclength equation :
C
       RLSUM=0.d0
       DO I=1,NCB
          IF(IFST.EQ.1)THEN
             DDPA(I)=THL(ICP(I))*RLDOT(I)
          ENDIF
          RLSUM=RLSUM+THL(ICP(I))*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
C
       FCPA=RDS-RINPR(IAP,NDIM,NDX,UDOTPS,DUPS,DTM,THU)-RLSUM
C
       RETURN
       END
C
C     ---------- ------
      SUBROUTINE SETUBV(NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,NDX,IAP,RAP,PAR,ICP,AA,BB,CC,DD,FA,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST)
C
C$    USE OMP_LIB
      IMPLICIT NONE
      INTEGER NPARX,NBIFX,NIAP,NRAP
      INCLUDE 'auto.h'
C
      INTEGER NDIM,NA,NCOL,NINT,NCB,NRC,NRA,NCA,NDX,IFST
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
C
      EXTERNAL FUNI,ICNI
C
C Local
      DOUBLE PRECISION WI(NCOL+1),WP(NCOL+1,NCOL),WT(NCOL+1,NCOL)
      DOUBLE PRECISION, ALLOCATABLE :: DDD(:,:,:),FCFC(:,:)
      INTEGER I,J,K,IAM,N,NT
C
C
      CALL WINT(NCOL+1,WI)
      CALL GENWTS(NCOL,NCOL+1,WT,WP)
C
      NT = 1
C$    NT = OMP_GET_MAX_THREADS()
      IF(NT.GT.1)THEN
         ALLOCATE(DDD(NCB,NINT,NT-1),FCFC(NINT,NT-1))
      ENDIF
C
C$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,IAM,N)
      IAM = 0
C$    IAM = OMP_GET_THREAD_NUM()
      I = IAM*NA/NT+1
      N = (IAM+1)*NA/NT+1-I
      IF(IFST.EQ.1)THEN
         IF(IAM.EQ.0)THEN
            CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +           IAP,RAP,PAR,ICP,AA,BB,CC,DD,FA,FC,
     +           UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT)
         ELSE
            CALL SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +           IAP,RAP,PAR,ICP,AA(1,1,I),BB(1,1,I),CC(1,1,I),
     +           DDD(1,1,IAM),FA(1,I),FCFC(1,IAM),UPS(1,I),UOLDPS(1,I),
     +           UDOTPS(1,I),UPOLDP(1,I),DTM(I),THU,WI,WP,WT)
         ENDIF
      ELSE
         IF (IAM.EQ.0)THEN
            CALL SETRHS(NDIM,N,NCOL,NINT,NRA,FUNI,ICNI,NDX,IAP,
     +           RAP,PAR,ICP,FA,FC,UPS,UOLDPS,
     +           UDOTPS,UPOLDP,DTM,WI,WP,WT)
         ELSE
            CALL SETRHS(NDIM,N,NCOL,NINT,NRA,FUNI,ICNI,NDX,IAP,
     +           RAP,PAR,ICP,FA(1,I),FCFC(1,IAM),UPS(1,I),UOLDPS(1,I),
     +           UDOTPS(1,I),UPOLDP(1,I),DTM(I),WI,WP,WT)
         ENDIF
      ENDIF
C$OMP END PARALLEL
C
C     This is were we sum into the global copy of the d array
C
      IF(NT.GT.1)THEN
         DO I=1,NT-1
            DO J=1,NINT
               IF(IFST.EQ.1)THEN
                  DO K=1,NCB
                     DD(K,J)=DD(K,J)+DDD(K,J,I)
                  ENDDO
               ENDIF
               FC(J)=FC(J)+FCFC(J,I)
            ENDDO
         ENDDO
         DEALLOCATE(DDD,FCFC)
      ENDIF

      CONTAINS

C     ---------- ---------
      SUBROUTINE SUBVPA(NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,NDX,IAP,RAP,PAR,ICP,AA,BB,CC,DD,FA,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT)
C
C     This is the per-CPU parallelized part of SETUBV
C
      EXTERNAL FUNI,ICNI
C
      INTEGER NDIM,N,NCOL,NINT,NCB,NRC,NRA,NCA,IAP(*),ICP(*),NDX
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*)
      DOUBLE PRECISION DD(NCB,*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THU(*)
      DOUBLE PRECISION WI(*),WP(NCOL+1,*),WT(NCOL+1,*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U,
     +  F,FICD,DICD,UIC,UIO,UID,UIP
      INTEGER I,J,K,IC,IC1,NCP1,I1,J1,JP1,K1
C
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPARX),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT))
      ALLOCATE(DICD(NINT*(NDIM+NPARX)),UIC(NDIM),UIO(NDIM))
      ALLOCATE(UID(NDIM),UIP(NDIM))
C
C Generate AA , BB and FA :
C
       DO J=1,N
          DO IC=1,NCOL
             IC1=(IC-1)*NDIM+1
             CALL SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,NDX,IAP,RAP,PAR,ICP,
     +            AA(1,IC1,J),BB(1,IC1,J),FA(IC1,J),UPS(1,J),
     +            UOLDPS(1,J),DTM(J),WP(1,IC),WT(1,IC),DFDU,DFDP,
     +            U,UOLD,F)
          ENDDO
       ENDDO
C     
C     Generate CC, DD and FC :
C
C Initialize to zero.
C
       DO I=1,NINT
         FC(I)=0.d0
         DO K=1,NCB
           DD(K,I)=0.d0
         ENDDO
       ENDDO
C
       NCP1=NCOL+1
       DO J=1,N
         JP1=J+1
         DO K=1,NCP1
            J1=J
            K1=(K-1)*NDIM+1
            I1=K1
            IF(K.EQ.NCP1)THEN
               J1=JP1
               I1=1
            ENDIF
C     
C     Integral constraints+pseudo-arclength equation :
C     
            CALL SBVICN(NDIM,NINT,NCB,NCA,ICNI,IAP,RAP,PAR,ICP,
     +           CC(K1,1,J),DD,FC,UPS(I1,J1),UOLDPS(I1,J1),
     +           UDOTPS(I1,J1),UPOLDP(I1,J1),DTM(J),THU,WI(K),FICD,DICD,
     +           UIC,UIO,UID,UIP)
          ENDDO
       ENDDO
C     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD)
       DEALLOCATE(UIC,UIO,UID,UIP)
       RETURN
      END SUBROUTINE SUBVPA
C
C     ---------- ---------
      SUBROUTINE SBVFUN(NDIM,NCOL,NCB,NCA,FUNI,NDX,IAP,RAP,PAR,ICP,
     + AA,BB,FA,UPS,UOLDPS,DTM,WP,WT,DFDU,DFDP,U,UOLD,F)
C
C     Does one call to FUNI and stores the result in AA, BB, and FA.
C
      EXTERNAL FUNI
C
      INTEGER, INTENT(IN) :: NDIM,NCOL,NCB,NCA,NDX,IAP(*),ICP(*)
      DOUBLE PRECISION, INTENT(IN) :: DTM,PAR(*),WT(*),WP(*)
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION, INTENT(OUT) :: AA(NCA,*),BB(NCB,*),FA(*),U(*)
      DOUBLE PRECISION, INTENT(OUT) :: UOLD(*),DFDU(NDIM,*),DFDP(NDIM,*)
      DOUBLE PRECISION, INTENT(OUT) :: F(*),RAP(*)
C
C Local
      DOUBLE PRECISION PRM(NPARX),WPLOC(NCOL+1),WTTMP,TMP
      INTEGER I,IB,IB1,II,JJ,K,L,L1,NCP1
C
      NCP1=NCOL+1
      DO K=1,NDIM
         U(K)=   WT(NCP1)*   UPS(K,2)
         UOLD(K)=WT(NCP1)*UOLDPS(K,2)
         DO L=1,NCOL
            L1=(L-1)*NDIM+K
            U(K)=U(K)        +WT(L)*   UPS(L1,1)
            UOLD(K) =UOLD(K) +WT(L)*UOLDPS(L1,1)
         ENDDO
      ENDDO
      DO I=1,NPARX
         PRM(I)=PAR(I)
      ENDDO
      CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,2,F,DFDU,DFDP)
C     transpose DFDU for optimal access
      DO II=1,NDIM
         DO JJ=1,II-1
            TMP=DFDU(II,JJ)
            DFDU(II,JJ)=DFDU(JJ,II)
            DFDU(JJ,II)=TMP
         ENDDO
      ENDDO
      DO IB=1,NCP1
         WPLOC(IB)=WP(IB)/DTM
      ENDDO
      DO I=1,NDIM
         DO IB=1,NCP1
            WTTMP=-WT(IB)
            IB1=(IB-1)*NDIM
            DO K=1,NDIM
               AA(IB1+K,I)=WTTMP*DFDU(K,I)
            ENDDO
            AA(IB1+I,I)=AA(IB1+I,I)+WPLOC(IB)
         ENDDO
         DO K=1,NCB
            BB(K,I)=-DFDP(I,ICP(K))
         ENDDO
         FA(I)=F(I)-WPLOC(NCP1)*UPS(I,2)
         DO K=1,NCOL
            FA(I)=FA(I)-WPLOC(K)*UPS((K-1)*NDIM+I,1)
         ENDDO
      ENDDO
      END SUBROUTINE
C
C     ---------- ------
      SUBROUTINE SBVICN(NDIM,NINT,NCB,NCA,ICNI,IAP,RAP,PAR,ICP,CC,DD,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,FICD,DICD,UIC,UIO,UID,UIP)
C
      IMPLICIT NONE
C
C     Does one call to ICNI (integral constraints) and stores the
C     result in CC, DD and FC; and stores the pseudo-arclength
C     result too.
C
      EXTERNAL ICNI
      INTEGER, INTENT(IN) :: NDIM,NINT,NCB,NCA,IAP(*),ICP(*)
      DOUBLE PRECISION, INTENT(IN) :: RAP(*),PAR(*),UPS(*),UDOTPS(*)
      DOUBLE PRECISION, INTENT(IN) :: UOLDPS(*),UPOLDP(*),DTM,WI,THU(*)
      DOUBLE PRECISION, INTENT(OUT) :: CC(NCA,*),FICD(*),DICD(NINT,*)
      DOUBLE PRECISION, INTENT(OUT) :: UIC(*),UIO(*),UID(*),UIP(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DD(NCB,*),FC(*)
C
C Local
      INTEGER I,M
C
      IF(NINT.GT.0)THEN
         DO I=1,NDIM
            UIC(I)=UPS(I)
            UIO(I)=UOLDPS(I)
            UID(I)=UDOTPS(I)
            UIP(I)=UPOLDP(I)
         ENDDO
         CALL ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,
     +        FICD,2,DICD)
         DO M=1,NINT
            DO I=1,NDIM
               CC(I,M)=DTM*WI*DICD(M,I)
            ENDDO
            DO I=1,NCB
               DD(I,M)=DD(I,M)+DTM*WI*DICD(M,NDIM+ICP(I))
            ENDDO
            FC(M)=FC(M)-DTM*WI*FICD(M)
         ENDDO
      ENDIF
C     
C     Pseudo-arclength equation :
C     
      DO I=1,NDIM
         CC(I,NINT+1)=DTM*THU(I)*WI*UDOTPS(I)
      ENDDO
      END SUBROUTINE
C
C     ---------- ------
      SUBROUTINE SETRHS(NDIM,N,NCOL,NINT,NRA,FUNI,ICNI,
     + NDX,IAP,RAP,PAR,ICP,FA,FC,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,WI,WP,WT)
C
      EXTERNAL FUNI,ICNI
C
      INTEGER NDIM,N,NCOL,NINT,NRA,IAP(*),ICP(*),NDX
      DOUBLE PRECISION RAP(*),UPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),WI(*),WP(NCOL+1,*),WT(NCOL+1,*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DFDU,DFDP,UOLD,U,
     +  F,FICD,DICD,UIC,UIO,UID,UIP
      INTEGER I,J,K,L,M,IB,IC,IC1,NCP1,I1,J1,JP1,K1,L1
      DOUBLE PRECISION WPLOC(NCOL+1,NCOL),PRM(NPARX),DDT
C
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPARX),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT),DICD(NINT*(NDIM+NPARX)))
      ALLOCATE(UIC(NDIM),UIO(NDIM),UID(NDIM),UIP(NDIM))
C
C Initialize to zero.
       DO I=1,NINT
         FC(I)=0.d0
       ENDDO
       NCP1=NCOL+1
C
C Generate FA :
C
       DO 2 J=1,N
          JP1=J+1
          DDT=1.d0/DTM(J)
          DO IC=1,NCOL
             DO IB=1,NCP1
                WPLOC(IB,IC)=DDT*WP(IB,IC)
             ENDDO
          ENDDO
          DO 1 IC=1,NCOL
             DO K=1,NDIM
                U(K)   =WT(NCP1,IC)*UPS(K,JP1)
                UOLD(K)=WT(NCP1,IC)*UOLDPS(K,JP1)
                DO L=1,NCOL
                   L1=(L-1)*NDIM+K
                   U(K)   =U(K)   +WT(L,IC)*UPS(L1,J)
                   UOLD(K)=UOLD(K)+WT(L,IC)*UOLDPS(L1,J)
                ENDDO
             ENDDO
             DO I=1,NPARX
                PRM(I)=PAR(I)
             ENDDO
             CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,0,F,DFDU,DFDP)
             IC1=(IC-1)*NDIM
             DO I=1,NDIM
                FA(IC1+I,J)=F(I)-WPLOC(NCP1,IC)*UPS(I,JP1)
                DO K=1,NCOL
                   K1=(K-1)*NDIM+I
                   FA(IC1+I,J)=FA(IC1+I,J)-WPLOC(K,IC)*UPS(K1,J)
                ENDDO
             ENDDO
 1        CONTINUE
 2     CONTINUE
C     
C     Generate FC :
C
C     Integral constraints :     
       IF(NINT.GT.0)THEN
         DO J=1,NA
            JP1=J+1
            DO K=1,NCP1
               DO I=1,NDIM
                  I1=(K-1)*NDIM+I
                  J1=J
                  IF(K.EQ.NCP1)I1=I
                  IF(K.EQ.NCP1)J1=JP1
                  UIC(I)=UPS(I1,J1)
                  UIO(I)=UOLDPS(I1,J1)
                  UID(I)=UDOTPS(I1,J1)
                  UIP(I)=UPOLDP(I1,J1)
               ENDDO
               CALL ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,
     *              FICD,0,DICD)
               DO M=1,NINT
                  FC(M)=FC(M)-DTM(J)*WI(K)*FICD(M)
               ENDDO
            ENDDO
         ENDDO
       ENDIF
C     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD,UIC,UIO,UID,UIP)
       RETURN
       END SUBROUTINE
C
      END
C
C     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,FA,FC,P0,P1,IFST,
     +  IDB,NLLV,DET,NOV,NTST,NA,NBC,NRA,NCA,
     +  NCB,NFC,A1,A2,BB,CC,CCBC,DDBC,
     +  S1,S2,IPR,IPC,IRF,ICF,IAM,KWT)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   IFST,IDB,NLLV,NOV,NTST,NA,NBC,NRA
      INTEGER   NCA,NCB,NFC,IAM,KWT
      DOUBLE PRECISION DET
      DOUBLE PRECISION A(*),B(*),C(*),D(NCB,*),FA(*),FC(*),P0(*),P1(*)
      DOUBLE PRECISION A1(*),A2(*),BB(*),CC(*),CCBC(*),DDBC(*)
      DOUBLE PRECISION S1(*),S2(*)
      INTEGER   IPR(*),IPC(*),IRF(*),ICF(*)
C
C Local
      DOUBLE PRECISION SOL,FCC,E,X,FAA
      INTEGER IR,IC,NRC,NTSTNA
      ALLOCATABLE SOL(:,:),FCC(:),E(:,:),IR(:),IC(:),X(:),FAA(:,:)
C
      NTSTNA=NA
      IF(IAM.EQ.0)NTSTNA=NTST
      ALLOCATE(SOL(NOV,NTSTNA+1))
      ALLOCATE(FCC(2*NOV+NFC+2*NOV*NOV+1),E(NOV+NFC,2*NOV+NFC))
      ALLOCATE(IR(2*NOV+NFC+2*NOV*NOV+1),IC(2*NOV+NFC+2*NOV*NOV+1))
C
      NRC=NFC-NBC
C
      IF(IDB.GT.4.and.IAM.EQ.0)
     +     CALL PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DDBC,FA,FC)
C
      IF(IFST.EQ.1)THEN
         CALL CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF)
         CALL COPYCP(NA,NOV,NRA,NCA,A,NCB,B,NRC,C,A1,A2,BB,CC,IRF)
      ENDIF
C
      IF(NLLV.EQ.0)THEN
         CALL CONRHS(NOV,NA,NRA,NCA,A,NRC,C,FA,FC(NBC+1),IRF,ICF)
      ELSE
         CALL SETZERO(FA,FC,NA,NRA,NFC)
      ENDIF
      ALLOCATE(FAA(NOV,NTSTNA+1))
      CALL CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
C
      IF(KWT.GT.1)
     +     CALL MPICON(A1,A2,BB,CC,D,FAA,FC(NBC+1),
     +     NTST,NOV,NCB,NRC,IFST)
C
      IF(IAM.EQ.0)THEN
         IF(IFST.EQ.1)
     +     CALL REDUCE(A1,A2,BB,CC,D,
     +     NTST,NOV,NCB,NRC,S1,S2,IPC,IPR)
C
         IF(NLLV.EQ.0)
     +     CALL REDRHS(A1,A2,CC,
     +     FAA,FC(NBC+1),NTST,NOV,NRC,IPC,IPR)
C
         CALL DIMRGE(E,CC,CCBC,D,DDBC,FC,IR,IC,
     +     NTST,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
C
         CALL BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,SOL,NTST,NOV,NCB,IPC)
      ENDIF
      DEALLOCATE(FAA)
      IF(KWT.GT.1)THEN
         CALL MPIBCAST(FC,NOV+NCB)
         CALL MPISCAT(SOL,NOV,NTST,1)
      ENDIF
C
      ALLOCATE(X(NRA))
      CALL INFPAR(A,B,FA,SOL,FC,NA,NOV,NRA,NCA,NCB,IRF,ICF,X)
      DEALLOCATE(X)
C
      DEALLOCATE(SOL,FCC,E,IR,IC)
      RETURN
      END
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
      END
C
C     This is the per-CPU, per-element process function of CONPAR
C     ---------- ------
      SUBROUTINE CONPAP(NOV,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF,IAMAX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NOV,NRA,NCA
      INTEGER   NCB,NRC,ICF(*),IRF(*)
      DIMENSION A(NCA,*),B(NCB,*),C(NCA,*),D(NCB,*),IAMAX(*)
C
C Note that the summation of the adjacent overlapped part of C
C is delayed until REDUCE, in order to merge it with other communications.
C
      NEX=NCA-2*NOV
C
C Condensation of parameters (Elimination of local variables).
C
      M1    = NOV+1
      M2    = NOV+NEX
C     
      ZERO = 0.D0
C
         DO J=1,NRA
            IRF(J)=J
            IAMAX(J)=NOV+IDAMAX(NEX,A(NOV+1,J),1)
         ENDDO
         DO J=1,NCA
            ICF(J)=J
         ENDDO
         DO 1 IC=M1,M2
            IRP=IC-NOV
            IR1=IRP+1
            ICP1=IC+1
C           **Search for pivot (Complete pivoting)
            PIV = ZERO
            IPIV = IRP
            JPIV = IC
            DO K1=IRP,NRA
               IROW=IRF(K1)
               TPIV = DABS(A(ICF(IAMAX(IROW)),IROW))
               IF(PIV.LT.TPIV)THEN
                  PIV = TPIV
                  IPIV = K1
                  JPIV = IAMAX(IROW)
               ENDIF
            ENDDO
C           **Move indices
            ITMP        = ICF(IC)
            ICF(IC)     = ICF(JPIV)
            ICF(JPIV)   = ITMP
            ICFIC       = ICF(IC)
            ITMP        = IRF(IRP)
            IRF(IRP)    = IRF(IPIV)
            IRF(IPIV)   = ITMP
            IRFIRP      = IRF(IRP)
C           **End of pivoting; elimination starts here
            PIV=A(ICFIC,IRFIRP)
            NRAMIC=NRA-IC
            DO IR=IR1,NRA
               IRFIR=IRF(IR)
               RM=A(ICFIC,IRFIR)/PIV
               A(ICFIC,IRFIR)=RM
	       IF(RM.NE.0.0)THEN
                  CALL IMSBRA(NOV,NCA,NRAMIC,A(1,IRFIR),A(1,IRFIRP),
     +                 ICF(ICP1),IAMAX(IRFIR),RM)
                  IAMAX(IRFIR)=IAMAX(IRFIR)+IC
                  DO L=1,NCB
                     B(L,IRFIR)=B(L,IRFIR)-RM*B(L,IRFIRP)
                  ENDDO
               ELSEIF(IAMAX(IRFIR).EQ.IAMAX(IRFIRP))THEN
                  IAMAX(IRFIR)=IC+IMAXCF(NRAMIC,A,ICF(ICP1))
               ELSEIF(IAMAX(IRFIR).EQ.IC)THEN
                  IAMAX(IRFIR)=IAMAX(IRFIRP)
               ENDIF
            ENDDO
            DO IR=1,NRC
               RM=C(ICFIC,IR)/PIV
               C(ICFIC,IR)=RM
	       IF(RM.NE.0.0)THEN
                  CALL SUBRAC(NOV,NCA-IC,C(1,IR),A(1,IRFIRP),
     +                 ICF(ICP1),RM)
                  DO L=1,NCB
                     D(L,IR)=D(L,IR)-RM*B(L,IRFIRP)
                  ENDDO
	       ENDIF
            ENDDO

 1       CONTINUE
      RETURN
C
      CONTAINS
C
C     ---------- ------
      SUBROUTINE IMSBRA(NOV,NCA,N,A,AP,ICF,IAMAX,RM)
      IMPLICIT NONE
C Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
      INTEGER, INTENT(IN) :: NOV,N,NCA,ICF(*)
      INTEGER, INTENT(OUT) :: IAMAX
C Local
      INTEGER L
      DOUBLE PRECISION PPIV,TPIV,V
C
      DO L=1,NOV
         A(L)=A(L)-RM*AP(L)
      ENDDO
      PPIV=0d0
      IAMAX=1
      DO L=1,N
         V=A(ICF(L))-RM*AP(ICF(L))
C     Also recalculate absolute maximum for current row
         A(ICF(L))=V
         TPIV=DABS(V)
         IF(PPIV.LT.TPIV)THEN
            PPIV=TPIV
            IAMAX=L
         ENDIF
      ENDDO
      DO L=NCA-NOV+1,NCA
         A(L)=A(L)-RM*AP(L)
      ENDDO
      END SUBROUTINE
C
C     ---------- ------
      SUBROUTINE SUBRAC(NOV,N,C,AP,ICF,RM)
      IMPLICIT NONE
C Arguments
      DOUBLE PRECISION, INTENT(IN) :: AP(*),RM
      DOUBLE PRECISION, INTENT(INOUT) :: C(*)
      INTEGER, INTENT(IN) :: NOV,N,ICF(*)
C Local
      INTEGER L
C
      DO L=1,NOV
         C(L)=C(L)-RM*AP(L)
      ENDDO
      DO L=1,N
         C(ICF(L))=C(ICF(L))-RM*AP(ICF(L))
      ENDDO
      END SUBROUTINE
C
      END
C
C     ---------- ------
      SUBROUTINE CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF)
C
C$    USE OMP_LIB
      IMPLICIT NONE
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NCB,NRC,ICF(NCA,*),IRF(NRA,*)
      DOUBLE PRECISION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DOUBLE PRECISION D(NCB,*)
C Local
      INTEGER I,J,K,NB,NU,IAM,NT
      DOUBLE PRECISION, ALLOCATABLE :: DD(:,:,:)
      INTEGER, ALLOCATABLE :: IAMAX(:,:)
C
C Condensation of parameters (Elimination of local variables).
C NA is the local NTST.
C
      IF(NCA.EQ.2*NOV)RETURN
      NT = 1
C$    NT = OMP_GET_MAX_THREADS()
      ALLOCATE(IAMAX(NRA,NT))
      IF(NT.GT.1)THEN
        ALLOCATE(DD(NCB,NRC,NT-1))
      ENDIF
C$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IAM,J,K,NB,NU)
      IAM = 0
C$    IAM = OMP_GET_THREAD_NUM()
      NU = (IAM+1)*NA/NT
      IF(IAM.EQ.0)THEN
        DO J=1,NU
          CALL CONPAP(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,C(1,1,J),
     +          D,IRF(1,J),ICF(1,J),IAMAX(1,IAM+1))
        ENDDO
      ELSE
        DO J=1,NRC
          DO K=1,NCB
            DD(K,J,IAM)=0.0d0
          ENDDO
        ENDDO
        NB = IAM*NA/NT+1
        DO J=NB,NU
          CALL CONPAP(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,C(1,1,J),
     +          DD(1,1,IAM),IRF(1,J),ICF(1,J),IAMAX(1,IAM+1))
        ENDDO
      ENDIF
C$OMP END PARALLEL
      DEALLOCATE(IAMAX)
C
C     This is were we sum into the global copy of the d array
C
      IF(NT.GT.1)THEN
        DO I=1,NT-1
          DO J=1,NRC
            DO K=1,NCB
              D(K,J)=D(K,J)+DD(K,J,I)
            ENDDO
          ENDDO
        ENDDO
        DEALLOCATE(DD)
      ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE CONRHS(NOV,NA,NRA,NCA,A,NRC,C,FA,FC,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NRC,ICF(NCA,*),IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),C(NCA,NRC,*)
      DIMENSION FA(NRA,*),FC(*)
C
      NEX=NCA-2*NOV
      IF(NEX.EQ.0)RETURN
C
C Condensation of right hand side.
C
      M1    = NOV+1
      M2    = NOV+NEX
C
      DO I=1,NA
         DO IC=M1,M2
            IR1=IC-NOV+1
            IRP=IR1-1
            IRFIRP = IRF(IRP,I)
            ICFIC  = ICF(IC,I)
            DO IR=IR1,NRA
               IRFIR=IRF(IR,I)
               IF(A(ICFIC,IRFIR,I).NE.0.0)
     +            FA(IRFIR,I)=FA(IRFIR,I)-
     +            A(ICFIC,IRFIR,I)*FA(IRFIRP,I)
            ENDDO
            DO IR=1,NRC
               IF(C(ICFIC,IR,I).NE.0.0)
     +            FC(IR)=FC(IR)-C(ICFIC,IR,I)*FA(IRFIRP,I)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE COPYCP(NA,NOV,NRA,NCA,A,
     +  NCB,B,NRC,C,A1,A2,BB,CC,IRF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NOV,NRA,NCA
      INTEGER   NCB,NRC,IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION BB(NCB,NOV,*),CC(NOV,NRC,*)
C
C     DIMENSION FA(NRA,*),FAA(NOV,*)
C
C Local
      INTEGER   I,IR,IC
C
C Copies the condensed sytem generated by CONPAR into workspace.
C
      NAP1=NA+1
      DO I=1,NA
         DO IR=1,NOV
            IRFIR=IRF(NRA-NOV+IR,I)
            DO IC=1,NOV
               IC1=NCA-NOV+IC
               A1(IC,IR,I)=A(IC,IRFIR,I)
               A2(IC,IR,I)=A(IC1,IRFIR,I)
            ENDDO     
            DO IC=1,NCB
               BB(IC,IR,I)=B(IC,IRFIR,I)
            ENDDO
         ENDDO
      ENDDO
C
      DO I=1,NAP1
         DO IR=1,NRC
            DO IC=1,NOV
               IF(I.EQ.1)THEN
                  CC(IC,IR,I)=C(IC,IR,I)
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
      END
C
C     ---------- ------
      SUBROUTINE CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NOV,NRA
      INTEGER   IRF(NRA,*)

      DIMENSION FA(NRA,*),FAA(NOV,*)
C
C Local
      INTEGER   I,IR
C
C     **Copy the RHS
      DO I=1,NA
         DO IR=1,NOV
            IRFIR=IRF(NRA-NOV+IR,I)
            FAA(IR,I)=FA(IRFIR,I)
         ENDDO         
      ENDDO
C     
      RETURN
      END
C
C     ------- -------- ------
      INTEGER FUNCTION IMAXCF(N,A,ICFI)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER N
      DIMENSION A(*),ICFI(*)
C
      PIV = 0.0D0
      IMAXCF = 1
      DO K2=1,N
         TPIV=DABS(A(ICFI(K2)))
         IF(PIV.LT.TPIV)THEN
            PIV=TPIV
            IMAXCF=K2
         ENDIF
      ENDDO
C
      RETURN
      END
C
C
C     ---------- ------
      SUBROUTINE REDUCE(A1,A2,BB,CC,DD,NA,NOV,NCB,NRC,S1,S2,IPC,IPR)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   NA,NOV,NCB,NRC
      INTEGER   IPC(NOV,*),IPR(NOV,*)
      DOUBLE PRECISION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),CC(NOV,NRC,*)
      DOUBLE PRECISION DD(NCB,*)
C
C Local 
      INTEGER IAMAX,IR,IC,I1,I2,I3,NAM1
      ALLOCATABLE IAMAX(:)
      ALLOCATE(IAMAX(2*NOV))
C
      NAM1    = NA-1
C
C Initialization
C
      DO IR=1,NOV
         DO IC=1,NOV
            S1(IC,IR,1)=A1(IC,IR,1)
         ENDDO
      ENDDO
C
C The reduction process is done concurrently
      DO I1=1,NAM1
         I2=I1+1
         I3=I2+1
         CALL REDBLK(S1(1,1,I1),A2(1,1,I1),S2(1,1,I1),BB(1,1,I1),
     +               S1(1,1,I2),A1(1,1,I2),A2(1,1,I2),BB(1,1,I2),
     +               CC(1,1,1 ),CC(1,1,I2),CC(1,1,I3),DD,
     +        IPC(1,I1),IPR(1,I1),IAMAX,NOV,NCB,NRC)
      ENDDO
C
C Initialization
      DO IR=1,NOV
         DO IC=1,NOV
            S2(IC,IR,NA)=0.0D0
         ENDDO
      ENDDO
C
      DEALLOCATE(IAMAX)
      RETURN
C
      CONTAINS
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
       INTEGER K1,K2,IC,ICP1,IPIV1,IPIV2,ITMP,JPIV,JPIV1,JPIV2
       INTEGER IDAMAX
       DOUBLE PRECISION PIV1,PIV2,TPIV
C
         DO K1=1,NOV
            IPC(K1)       = K1
            IPR(K1)       = K1
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
C Complete pivoting; rows are swapped physically, columns swap indices
            PIV1 = 0.d0
            IPIV1 = IC
            DO K1=IC,NOV
               TPIV=DABS(A21(IPC(IAMAX(K1)),K1))
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
               TPIV=DABS(A12(IPC(IAMAX(NOV+K1)),K1))
               IF(PIV2.LT.TPIV)THEN
                  PIV2   = TPIV
                  IPIV2  = K1
               ENDIF
            ENDDO
            JPIV2=IAMAX(NOV+IPIV2)
C
C rows are swapped physically, columns swap indices
C
            IF(PIV1.GE.PIV2)THEN
               JPIV        = JPIV1
               IPR(IC)     = IPIV1
               CALL REDSWP(IC,NOV,NCB,IPC,
     +              S11(1,IC),S11(1,IPIV1),A21(1,IC),A21(1,IPIV1),
     +              S21(1,IC),S21(1,IPIV1),BB1(1,IC),BB1(1,IPIV1))
            ELSE
               JPIV        = JPIV2
               IPR(IC)     = NOV+IPIV2
               CALL REDSWP(IC,NOV,NCB,IPC,
     +              S11(1,IC),S12(1,IPIV2),A21(1,IC),A12(1,IPIV2),
     +              S21(1,IC),A22(1,IPIV2),BB1(1,IC),BB2(1,IPIV2))
            ENDIF
            IAMAX(IPR(IC)) = IAMAX(IC)
            ITMP           = IPC(IC)
            IPC(IC)        = IPC(JPIV)
            IPC(JPIV)      = ITMP
C
C End of pivoting; Elimination starts here
C
            DO IR=ICP1,NOV
               CALL REDELIM(IC,NOV,NCB,IPC,IAMAX(IR),JPIV,
     +              A21(1,IR),A21(1,IC),S11(1,IR),S11(1,IC),
     +              S21(1,IR),S21(1,IC),BB1(1,IR),BB1(1,IC))
            ENDDO
C     
            DO IR=1,NOV
               CALL REDELIM(IC,NOV,NCB,IPC,IAMAX(NOV+IR),JPIV,
     +              A12(1,IR),A21(1,IC),S12(1,IR),S11(1,IC),
     +              A22(1,IR),S21(1,IC),BB2(1,IR),BB1(1,IC))
            ENDDO
C     
            DO IR=1,NRC
               CALL REDELIM(IC,NOV,NCB,IPC,0,JPIV,
     +              CC2(1,IR),A21(1,IC),CC1(1,IR),S11(1,IC),
     +              CC3(1,IR),S21(1,IC),DD(1,IR),BB1(1,IC))
            ENDDO
         ENDDO
C
       END SUBROUTINE REDBLK

C      ---------- ------
       SUBROUTINE REDSWP(IC,NOV,NCB,IPC,
     +     S11,S12,A12,A21,S21,A22,BB1,BB2)
C
       IMPLICIT NONE
C
       INTEGER IC,NOV,NCB,IPC(*)
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
             TMP         = A21(IPC(L))
             A21(IPC(L)) = A12(IPC(L))
             A12(IPC(L)) = TMP
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
       SUBROUTINE REDELIM(IC,NOV,NCB,IPC,IAMAX,JPIV,
     +     A12,A21,S12,S11,A22,S21,BB2,BB1)
C
       IMPLICIT NONE
C
       INTEGER IC,NOV,NCB,IPC(*),IAMAX,JPIV
       DOUBLE PRECISION A12(NOV),A21(NOV),S12(NOV),S11(NOV)
       DOUBLE PRECISION A22(NOV),S21(NOV),BB1(NOV),BB2(NOV)
C
       INTEGER L,IMAXCF
       DOUBLE PRECISION RM,V,PPIV,TPIV
C
       RM = A12(IPC(IC))/A21(IPC(IC))
       A12(IPC(IC)) = RM
C
       IF(RM.NE.0.0)THEN
          IF(IAMAX.EQ.0)THEN
             DO L=IC+1,NOV
                A12(IPC(L))=A12(IPC(L))-RM*A21(IPC(L))
             ENDDO
          ELSE
             PPIV=0d0
             IAMAX=1
             DO L=IC+1,NOV
                V=A12(IPC(L))-RM*A21(IPC(L))
C     Also recalculate absolute maximum for current row
                A12(IPC(L))=V
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
          IAMAX = IC+IMAXCF(NOV-IC,A12,IPC(IC+1))
       ELSEIF(IAMAX.EQ.IC)THEN
          IAMAX = JPIV
       ENDIF
C
       END SUBROUTINE REDELIM
C
      END SUBROUTINE REDUCE
C
C     ---------- ------
      SUBROUTINE REDRHS(A1,A2,CC,FAA,FC,NA,NOV,NRC,IPC,IPR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NRC
      INTEGER   IPC(NOV,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION CC(NOV,NRC,*)
      DIMENSION FAA(NOV,*),FC(*)
      DIMENSION IPR(NOV,*)
C
C Local
      DOUBLE PRECISION RM
C
      NAP1    = NA+1
      NAM1    = NA-1
C
C Reduce concurrently in each node
      DO I1=1,NAM1
         I2=I1+1
         DO IC=1,NOV
            ICP1  = IC+1
            IPIV1 = IPR(IC,I1)
            IF(IPIV1.LE.NOV)THEN
               TMP           = FAA(IC,I1)
               FAA(IC,I1)    = FAA(IPIV1,I1)
               FAA(IPIV1,I1) = TMP
            ELSE        
               L1         = IPIV1-NOV
               TMP        = FAA(IC,I1)
               FAA(IC,I1) = FAA(L1,I2)
               FAA(L1,I2) = TMP
            ENDIF
            DO IR=ICP1,NOV
               L1=IPC(IC,I1)
               RM=A2(L1,IR,I1)
               FAA(IR,I1)=FAA(IR,I1)-RM*FAA(IC,I1)
            ENDDO
            DO IR=1,NOV
               L1=IPC(IC,I1)
               RM=A1(L1,IR,I2)
               FAA(IR,I2) = FAA(IR,I2)-RM*FAA(IC,I1)
            ENDDO
            DO IR=1,NRC
               L1=IPC(IC,I1)
               RM=CC(L1,IR,I2)
               FC(IR)= FC(IR)-RM*FAA(IC,I1)
            ENDDO
         ENDDO
      ENDDO            
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE DIMRGE(E,CC,CCBC,D,DDBC,FC,IR,IC,
     +  NA,NFC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   NA,NFC,NBC,NOV,NCB,IDB,NLLV
      INTEGER   IR(*),IC(*)
      DOUBLE PRECISION E(NOV+NFC,*),CC(NOV,NFC-NBC,*),CCBC(NOV,NBC,*)
      DOUBLE PRECISION D(NCB,*),DDBC(NCB,*),P0(NOV,*),P1(NOV,*)
      DOUBLE PRECISION S(NOV,NOV,*),FAA(NOV,*),A2(NOV,NOV,*)
      DOUBLE PRECISION BB(NCB,NOV,*),FC(*),FCC(*),DET
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
         CALL GE(0,NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
      ELSEIF(NLLV.GT.0)THEN
         CALL NLVC(NCR,NCR,NLLV,E,FCC,IR,IC)
      ELSE
         DO I=1,NCR-1
            XE(I)=0.D0
         ENDDO
         XE(NCR)=1.D0
         CALL GE(0,NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
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
      END
C
C     ---------- ------
      SUBROUTINE BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,SOL,NA,NOV,NCB,IPC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NCB,IPC(NOV,*)
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION A2(NOV,NOV,*),BB(NCB,NOV,*)
      DIMENSION SOL(NOV,*),FAA(NOV,*),FC(*),FCC(*)
C
C Local
      INTEGER I,K
      DOUBLE PRECISION SM
C
      DO L=1,NOV
         SOL(L,NA+1) = FC(L)
      ENDDO
C
C Backsubstitution process; concurrently in each node.
      NAM1=NA-1
      DO I=NAM1,1,-1
         DO K=NOV,1,-1
            SM=0.0D0
            DO L=1,NOV
               SM=SM+FCC(L)*S1(L,K,I)
               SM=SM+SOL(L,I+2)*S2(L,K,I)
            ENDDO
            DO L=1,NCB
               SM=SM+FC(NOV+L)*BB(L,K,I)
            ENDDO
            DO L=K+1,NOV
               L1=IPC(L,I)
               SM=SM+SOL(L1,I+1)*A2(L1,K,I)
            ENDDO
            L2=IPC(K,I)
            SOL(L2,I+1)=(FAA(K,I)-SM)/A2(L2,K,I)
         ENDDO
      ENDDO
C
      DO L=1,NOV
         SOL(L,1)=FCC(L)
      ENDDO

      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INFPAR(A,B,FA,SOL,FC,NA,NOV,NRA,NCA,NCB,IRF,ICF,X)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C  Arguments
      INTEGER   NA,NOV,NRA,NCA,NCB,IRF(NRA,*),ICF(NCA,*)
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
            SM=0.D0
            IRFIR=IRF(IR,I)
            DO J=1,NOV
               NRAPJ=NRA+J
               SM=SM+A(J,IRFIR,I)*SOL(J,I)
               SM=SM+A(NRAPJ,IRFIR,I)*SOL(J,I+1)
            ENDDO
            DO J=1,NCB
               NOVPJ=NOV+J
               SM=SM+B(J,IRFIR,I)*FC(NOVPJ)
            ENDDO
            DO J=IRP1,NRAM
               J1=J+NOV
               ICFJ1=ICF(J1,I)
               SM=SM+A(ICFJ1,IRFIR,I)*X(ICFJ1)
            ENDDO
            NOVPIR=NOV+IR
            ICFNOVPIR=ICF(NOVPIR,I)
            X(ICFNOVPIR)=(FA(IRFIR,I)-SM)/
     +           A(ICFNOVPIR,IRFIR,I)
         ENDDO    
C        **Copy SOL and X into FA 
         DO J=1,NOV
            FA(J,I)=SOL(J,I)
         ENDDO
         DO J=NOV+1,NRA 
            FA(J,I)=X(J)
         ENDDO        
      ENDDO
C
C     
      RETURN
      END
C           
C     ---------- ------
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NFC,NBC,A,B,C,CCBC,D,DDBC,FA,FC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NFC-NBC,*)
      DIMENSION CCBC(NCA-NRA,NBC,*),D(NCB,*),DDBC(NCB,*),FA(NRA,*),FC(*)
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
         WRITE(9,103)(D(IC,IR),IC=1,NCB),FC(NBC+IR)
       ENDDO
C
 101   FORMAT(' AA , BB , FA (Full dimension) :')
 102   FORMAT(' I=',I3)
 103   FORMAT(1X,12E10.3)
 104   FORMAT(' CC (Full dimension) :')
 105   FORMAT(' DD , FC')
C
      RETURN
      END


