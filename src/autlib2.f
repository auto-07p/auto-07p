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
      DIMENSION IAP(*),RAP(*),FC(*)
C
C Local
      ALLOCATABLE A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:)
      ALLOCATABLE S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:),CCBC(:,:,:)
      ALLOCATABLE FAA(:,:)
      ALLOCATABLE ICF(:,:),IRF(:,:),IPR(:,:),ICF1(:,:),ICF2(:,:)
      SAVE A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,FAA,ICF,IRF,IPR,ICF1,ICF2
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
      NDIM=IAP(1)
      NTST=IAP(5)
      NCOL=IAP(6)
      NBC=IAP(12)
      NINT=IAP(13)
      IID=IAP(18)
      NFPR=IAP(29)
      NRC=NINT+1
      NRD=NRC+NBC
      NROW=NDIM*NCOL
      NCLM=NROW+NDIM
      IF(IFST.EQ.1)THEN
         IF(ALLOCATED(A))THEN
C           Free floating point arrays
            DEALLOCATE(A,B,C,D,A1,A2,S1,S2,BB,CC,CCBC,FAA)
C           Free integer arrays
            DEALLOCATE(ICF,IRF,IPR,ICF1,ICF2)
         ENDIF
C
         ALLOCATE(A(NCLM,NROW,NTST+1),B(NFPR,NROW,NTST+1))
         ALLOCATE(C(NCLM,NRC,NTST+1),D(NFPR,NRD))
         ALLOCATE(A1(NDIM,NDIM,NTST+1),A2(NDIM,NDIM,NTST+1))
         ALLOCATE(S1(NDIM,NDIM,NTST+1),S2(NDIM,NDIM,NTST+1))
         ALLOCATE(BB(NFPR,NDIM,NTST+1),CC(NDIM,NINT+1,NTST+1))
         ALLOCATE(CCBC(NDIM,NBC,2),FAA(NDIM,NTST+1))
C
         ALLOCATE(ICF(NCLM,NTST+1),IRF(NROW,NTST+1),IPR(NDIM,NTST+1))
         ALLOCATE(ICF1(NDIM,NTST+1),ICF2(NDIM,NTST+1))
      ENDIF
C
      CALL SUBVSR(NDIM,NTST,NBC,NFPR,NRD,NROW,BCNI,NDX,
     +  IAP,RAP,PAR,ICP,RDS,CCBC,D,FC,RLCUR,RLOLD,
     +  RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU,IFST)
      CALL MPISBV(NDIM,NTST,NCOL,NINT,NFPR,NRC,NROW,NCLM,NDX,IAP,RAP,
     + PAR,ICP,RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST,NLLV,KWT)
      CALL SETUBV(NDIM,NTST/KWT,NCOL,NINT,NFPR,NRC,NROW,NCLM,
     +   FUNI,ICNI,NDX,IAP,RAP,PAR,ICP,A,B,C,D(1,NBC+1),FA,
     +   FC(NBC+1),UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,IFST)
C
      CALL BRBD(A,B,C,D,FA,FC,P0,P1,IFST,
     +  IID,NLLV,DET,NDIM,NTST,NBC,NROW,NCLM,
     +  NFPR,NRD,A1,A2,BB,CC,CCBC,FAA,
     +  S1,S2,IPR,ICF1,ICF2,IRF,ICF,KWT)
C
      RAP(14)=DET
C
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
      SUBROUTINE SUBVSR(NDIM,NTST,NBC,NCB,NRD,NRA,BCNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,CCBC,DD,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU,IFST)
C
      IMPLICIT NONE
      INTEGER NPARX,NBIFX,NIAP,NRAP
      include 'auto.h'
C
C     This subroutine handles the non-parallel part of SETUBV, that is,
C     * the boundary conditions (not much to parallelize here and
C       HomCont relies on non-parallel execution): the arrays CCBC,
C       DUPS, and parts of DD and FC.
C     * creating the pseudo-arclength parts of FC and DD
C
      EXTERNAL BCNI
      DOUBLE PRECISION RINPR
C
      INTEGER NDIM,NTST,NBC,NCB,NRD,NRA,NDX,IAP(*),ICP(*),IFST
      DOUBLE PRECISION RDS,CCBC(NDIM,NBC,*),DD(NCB,*)
      DOUBLE PRECISION RAP(*),UPS(NDX,*),DUPS(NDX,*)
      DOUBLE PRECISION UOLDPS(NDX,*),UDOTPS(NDX,*)
      DOUBLE PRECISION FC(*),DTM(*),PAR(*)
      DOUBLE PRECISION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,J,K,IPS
      DOUBLE PRECISION RLSUM
C
C Set constants.
       DO I=1,NCB
         PAR(ICP(I))=RLCUR(I)
       ENDDO
C
C     ** Time evolution computations (parabolic systems)
       IPS=IAP(2)
       IF(IPS.EQ.14 .OR. IPS.EQ.16)RAP(15)=RLOLD(1)
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
                  DD(K,I)=DBC(I,2*NDIM+ICP(K))
               ENDDO
            ENDIF
         ENDDO    
C       Save difference :
       ENDIF
       DO J=1,NTST+1
         DO I=1,NRA
            DUPS(I,J)=UPS(I,J)-UOLDPS(I,J)
          ENDDO
       ENDDO
C
C     Pseudo-arclength equation :
C
       RLSUM=0.d0
       DO I=1,NCB
          IF(IFST.EQ.1)THEN
             DD(I,NRD)=THL(ICP(I))*RLDOT(I)
          ENDIF
          RLSUM=RLSUM+THL(ICP(I))*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
C
       FC(NRD)=RDS-RINPR(IAP,NDIM,NDX,UDOTPS,DUPS,DTM,THU)-RLSUM
C
       DEALLOCATE(UBC0,UBC1,FBC,DBC)
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
     +  IDB,NLLV,DET,NOV,NA,NBC,NRA,NCA,
     +  NCB,NRD,A1,A2,BB,CC,CCBC,FAA,
     +  S1,S2,IPR,ICF1,ICF2,IRF,ICF,KWT)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   IFST,IDB,NLLV,NOV,NA,NBC,NRA
      INTEGER   NCA,NCB,NRD,KWT
      DOUBLE PRECISION DET
      DOUBLE PRECISION A(*),B(*),C(*),D(NCB,*),FA(*),FC(*),P0(*),P1(*)
      DOUBLE PRECISION A1(*),A2(*),BB(*),CC(*),CCBC(*),FAA(*)
      DOUBLE PRECISION S1(*),S2(*)
      INTEGER   IPR(*),ICF1(*),ICF2(*),IRF(*),ICF(*)      
C
C Local
      DOUBLE PRECISION SOL1,SOL2,SOL3,FCC,E,X
      INTEGER IR,IC,NRC
      ALLOCATABLE SOL1(:),SOL2(:),SOL3(:),FCC(:),E(:,:),IR(:),IC(:)
      ALLOCATABLE X(:)
      ALLOCATE(SOL1(NOV*(NA+1)),SOL2(NOV*(NA+1)),SOL3(NOV*(NA+1)))
      ALLOCATE(FCC(2*NOV+NRD+2*NOV*NOV+1),E(NOV+NRD,2*NOV+NRD))
      ALLOCATE(IR(2*NOV+NRD+2*NOV*NOV+1),IC(2*NOV+NRD+2*NOV*NOV+1))
C
      NRC=NRD-NBC
C
      IF(IDB.GT.4)
     +     CALL PRINT1(NA,NRA,NCA,NCB,NRD,NBC,A,B,C,CCBC,D,FA,FC)

      IF(IFST.EQ.1)THEN
         CALL CONPAR(NOV,NA/KWT,NRA,NCA,A,NCB,B,NRC,C,
     +        D(1,NBC+1),IRF,ICF)
      ENDIF
C
      IF(NLLV.EQ.0)THEN
         CALL CONRHS(NOV,NA/KWT,NRA,NCA,A,NRC,C,FA,FC(NBC+1),IRF,ICF)
      ELSE
         CALL SETZERO(FA,FC,NA/KWT,NRA,NRD)
      ENDIF
C
      IF(KWT.GT.1)THEN
         CALL MPICON(NA,NRA,NCA,A,NCB,B,NRC,C,
     +        D(1,NBC+1),FA,FC(NBC+1),IRF,ICF,IFST,KWT)
      ENDIF
C
      CALL CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
      IF(IFST.EQ.1)THEN
         CALL COPYCP(NA,NOV,NRA,NCA,A,NCB,B,NRC,C,A1,A2,BB,CC,IRF)
         CALL REDUCE(A1,A2,BB,CC,D(1,NBC+1),
     +     NA,NOV,NCB,NRC,S1,S2,ICF1,ICF2,IPR)
      ENDIF
C
      IF(NLLV.EQ.0)
     +     CALL REDRHS(A1,A2,CC,
     +     FAA,FC(NBC+1),NA,NOV,NCB,NRC,ICF1,ICF2,IPR)
C
      CALL DIMRGE(E,CC,CCBC,D,FC,IR,IC,IFST,
     +     NA,NRD,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
C
      CALL BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,
     +     SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2)
C
      ALLOCATE(X(NRA))
      CALL INFPAR(A,B,FA,SOL1,SOL2,FC,
     *     NA,NOV,NRA,NCA,NCB,IRF,ICF,X)
      DEALLOCATE(X)
C
      DEALLOCATE(SOL1,SOL2,SOL3,FCC,E,IR,IC)
      RETURN
      END
C
C     ---------- -------
      SUBROUTINE SETZERO(FA,FC,NA,NRA,NRD)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   NA,NRA,NRD
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
      DO I=1,NRD
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
      INTEGER, ALLOCATABLE :: IAMAX(:)
C
C Condensation of parameters (Elimination of local variables).
C NA is the local NTST.
C
      IF(NCA.EQ.2*NOV)RETURN
      NT = 1
C$    NT = OMP_GET_MAX_THREADS()
      IF(NT.GT.1)THEN
        ALLOCATE(DD(NCB,NRC,NT-1))
      ENDIF
C$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IAM,J,K,NB,NU,IAMAX)
      IAM = 0
C$    IAM = OMP_GET_THREAD_NUM()
      NU = (IAM+1)*NA/NT
      ALLOCATE(IAMAX(NRA))
      IF(IAM.EQ.0)THEN
        DO J=1,NU
          CALL CONPAP(NOV,NRA,NCA,A(1,1,J),NCB,B(1,1,J),NRC,C(1,1,J),
     +          D,IRF(1,J),ICF(1,J),IAMAX)
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
     +          DD(1,1,IAM),IRF(1,J),ICF(1,J),IAMAX)
        ENDDO
      ENDIF
      DEALLOCATE(IAMAX)
C$OMP END PARALLEL
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
      SUBROUTINE REDUCE(A1,A2,BB,CC,DD,NA,NOV,NCB,NRC,S1,S2,ICF1,ICF2,
     +  IPR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NCB,NRC
      INTEGER   ICF1(NOV,*),ICF2(NOV,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION BB(NCB,NOV,*),CC(NOV,NRC,*)
      DIMENSION DD(NCB,*)
      DIMENSION IPR(NOV,*)
C
C Local 
      DOUBLE PRECISION RM
      ALLOCATABLE IAMAX(:)
      ALLOCATE(IAMAX(2*NOV))
C
      ZERO    = 0.0D0
      NAP1    = NA+1
      NAM1    = NA-1
C
C Initialization
C
      DO I=1,NA
         DO K1=1,NOV
            ICF1(K1,I)     = K1
            ICF2(K1,I)     = K1
            IPR(K1,I)      = K1
            DO K2=1,NOV
               S2(K2,K1,I) = 0.0D0
               S1(K2,K1,I) = 0.0D0
            ENDDO
         ENDDO
      ENDDO
C
      DO IR=1,NOV
         DO IC=1,NOV
            S1(IC,IR,1)=A1(IC,IR,1)
         ENDDO
      ENDDO
C
C The reduction process is done concurrently
      DO 3 I1=1,NAM1
C
         I2=I1+1
         I3=I2+1
C
         DO K1=1,NOV
            IAMAX(K1)=IDAMAX(NOV,A2(1,K1,I1),1)
         ENDDO
         DO K1=1,NOV
            IAMAX(NOV+K1)=IDAMAX(NOV,A1(1,K1,I2),1)
         ENDDO
C
         DO 2 IC=1,NOV
            ICP1=IC+1
C
C Complete pivoting; rows are swapped physically, columns swap indices
            PIV1 = ZERO
            IPIV1 = IC
            DO K1=IC,NOV
               TPIV=DABS(A2(ICF2(IAMAX(K1),I1),K1,I1))
               IF(PIV1.LT.TPIV)THEN
                  PIV1   = TPIV
                  IPIV1  = K1
               ENDIF
            ENDDO
            JPIV1=IAMAX(IPIV1)
C
            PIV2 = ZERO
            IPIV2 = 1
            DO K1=1,NOV
               TPIV=DABS(A1(ICF1(IAMAX(NOV+K1),I2),K1,I2))
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
               IAMAX(IPIV1)      = IAMAX(IC)
               JPIV              = JPIV1
               IPR(IC,I1)        = IPIV1
               ITMP              = ICF2(IC,I1)
               ICF2(IC,I1)       = ICF2(JPIV1,I1)
               ICF2(JPIV1,I1)    = ITMP
               ITMP              = ICF1(IC,I2)
               ICF1(IC,I2)       = ICF1(JPIV1,I2)
               ICF1(JPIV1,I2)    = ITMP
C Swapping
               DO L=1,NOV
                  TMP            = S1(L,IC,I1)
                  S1(L,IC,I1)    = S1(L,IPIV1,I1)
                  S1(L,IPIV1,I1) = TMP
                  IF(L.GE.IC)THEN
                     TMP=A2(ICF2(L,I1),IC,I1)
                     A2(ICF2(L,I1),IC,I1)= 
     +                    A2(ICF2(L,I1),IPIV1,I1)
                     A2(ICF2(L,I1),IPIV1,I1)= TMP
                  ENDIF
                  TMP            = S2(L,IC,I1)
                  S2(L,IC,I1)    = S2(L,IPIV1,I1)
                  S2(L,IPIV1,I1) = TMP
               ENDDO
C
               DO L=1,NCB
                  TMP            = BB(L,IC,I1)
                  BB(L,IC,I1)    = BB(L,IPIV1,I1)
                  BB(L,IPIV1,I1) = TMP
               ENDDO
            ELSE
               IAMAX(NOV+IPIV2)  = IAMAX(IC)
               JPIV              = JPIV2
               IPR(IC,I1)        = NOV+IPIV2
               ITMP              = ICF2(IC,I1)
               ICF2(IC,I1)       = ICF2(JPIV2,I1)
               ICF2(JPIV2,I1)    = ITMP
               ITMP              = ICF1(IC,I2)
               ICF1(IC,I2)       = ICF1(JPIV2,I2)
               ICF1(JPIV2,I2)    = ITMP               
C Swapping
               DO L=1,NOV
                  IF(L.GE.IC)THEN
                     TMP  = A2(ICF2(L,I1),IC,I1)
                     A2(ICF2(L,I1),IC,I1)= 
     +                    A1(ICF2(L,I1),IPIV2,I2)
                     A1(ICF2(L,I1),IPIV2,I2) = TMP
                  ENDIF
                  TMP            = S2(L,IC,I1)
                  S2(L,IC,I1)    = A2(L,IPIV2,I2)
                  A2(L,IPIV2,I2) = TMP
                  TMP            = S1(L,IC,I1)
                  S1(L,IC,I1)    = S1(L,IPIV2,I2)
                  S1(L,IPIV2,I2) = TMP                  
               ENDDO
               DO L=1,NCB
                  TMP            = BB(L,IC,I1)
                  BB(L,IC,I1)    = BB(L,IPIV2,I2)
                  BB(L,IPIV2,I2) = TMP
               ENDDO
            ENDIF
C
C End of pivoting; Elimination starts here
C
            DO IR=ICP1,NOV
               RM = A2(ICF2(IC,I1),IR,I1)/
     +              A2(ICF2(IC,I1),IC,I1)
               A2(ICF2(IC,I1),IR,I1)   = RM
C
               IF(RM.NE.0.0)THEN
               DO L=ICP1,NOV
                  A2(ICF2(L,I1),IR,I1) =
     +                 A2(ICF2(L,I1),IR,I1)-RM*
     +                 A2(ICF2(L,I1),IC,I1)
               ENDDO
C
               DO L=1,NOV
                  S1(L,IR,I1) = S1(L,IR,I1)-RM*S1(L,IC,I1)
                  S2(L,IR,I1) = S2(L,IR,I1)-RM*S2(L,IC,I1)
               ENDDO
C
               DO L=1,NCB
                  BB(L,IR,I1) = BB(L,IR,I1)-RM*BB(L,IC,I1)
               ENDDO
	       ENDIF
               IF((RM.NE.0.0).OR.(IAMAX(IR).EQ.JPIV))THEN
C     recalculate absolute maximum for current row
                  IAMAX(IR) = IC+
     +                 IMAXCF(NOV-IC,A2(1,IR,I1),ICF2(ICP1,I1))
               ELSEIF(IAMAX(IR).EQ.IC)THEN
                  IAMAX(IR) = JPIV
               ENDIF
            ENDDO
C     
            DO IR=1,NOV
               RM = A1(ICF1(IC,I2),IR,I2)/
     +              A2(ICF2(IC,I1),IC,I1)
               A1(ICF1(IC,I2),IR,I2)   = RM
C
	       IF(RM.NE.0.0)THEN
                 DO L=ICP1,NOV
                    A1(ICF1(L,I2),IR,I2) = 
     +                 A1(ICF1(L,I2),IR,I2)-RM*
     +                 A2(ICF2(L,I1),IC,I1)
                 ENDDO
                 DO L=1,NOV
                   S1(L,IR,I2) = S1(L,IR,I2)-RM*S1(L,IC,I1)
                   A2(L,IR,I2) = A2(L,IR,I2)-RM*S2(L,IC,I1)
                 ENDDO
                 DO L=1,NCB
                   BB(L,IR,I2) = BB(L,IR,I2)-RM*BB(L,IC,I1)
                 ENDDO
	       ENDIF
               IF((RM.NE.0.0).OR.(IAMAX(NOV+IR).EQ.JPIV))THEN
C     recalculate absolute maximum for current row
                  IAMAX(NOV+IR) = IC+
     +                 IMAXCF(NOV-IC,A1(1,IR,I2),ICF1(ICP1,I2))
               ELSEIF(IAMAX(NOV+IR).EQ.IC)THEN
                  IAMAX(NOV+IR) = JPIV
               ENDIF
            ENDDO
C     
            DO IR=1,NRC
               RM = CC(ICF2(IC,I1),IR,I2)/
     +              A2(ICF2(IC,I1),IC,I1)
               CC(ICF2(IC,I1),IR,I2)   = RM                  
C
               IF(RM.NE.0.0)THEN
                 DO L=ICP1,NOV
                    CC(ICF2(L,I1),IR,I2) = 
     +                 CC(ICF2(L,I1),IR,I2)-RM*
     +                 A2(ICF2(L,I1),IC,I1)
                 ENDDO
                 DO L=1,NOV
                    CC(L,IR,1)  = CC(L,IR,1)-RM*S1(L,IC,I1)
                    CC(L,IR,I3) = CC(L,IR,I3)-RM*S2(L,IC,I1)
                 ENDDO
                 DO L=1,NCB
                    DD(L,IR)    = DD(L,IR)-RM*BB(L,IC,I1)
                 ENDDO
               ENDIF
             ENDDO           
C
 2       CONTINUE
 3    CONTINUE            
C
C Initialization
      DO I=1,NOV
         ICF2(I,NA)=I
      ENDDO
C
      DEALLOCATE(IAMAX)
      RETURN
      END
C   
C     ---------- ------
      SUBROUTINE REDRHS(A1,A2,CC,FAA,FC,NA,NOV,NCB,NRC,ICF1,ICF2,IPR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NRC
      INTEGER   ICF1(NOV,*),ICF2(NOV,*)
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
               L1=ICF2(IC,I1)
               RM=A2(L1,IR,I1)
               FAA(IR,I1)=FAA(IR,I1)-RM*FAA(IC,I1)
            ENDDO
            DO IR=1,NOV
               L1=ICF1(IC,I2)
               RM=A1(L1,IR,I2)
               FAA(IR,I2) = FAA(IR,I2)-RM*FAA(IC,I1)
            ENDDO
            DO IR=1,NRC
               L1=ICF2(IC,I1)
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
      SUBROUTINE DIMRGE(E,CC,CCBC,D,FC,IR,IC,IFST,
     +  NA,NRD,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NRD,NOV,NCB,IDB,NLLV
      INTEGER   IR(*),IC(*)
      DIMENSION E(NOV+NRD,*),CC(NOV,NRD-NBC,*),CCBC(NOV,NBC,*),D(NCB,*)
      DIMENSION P0(NOV,*),P1(NOV,*),S(NOV,NOV,*)
      DIMENSION FAA(NOV,*),A2(NOV,NOV,*),BB(NCB,NOV,*)
      DIMENSION FC(*),FCC(*)
C
C Local
      INTEGER  I,J
      ALLOCATABLE XE(:)
      ALLOCATE(XE(NOV+NRD))
C
      NAP1    = NA+1
      NCR     = NRD+NOV
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
      DO I=1,NRD
         NOVPI=NOV+I
         DO J=1,NOV
            NOVPJ          = NOV+J
            IF(I.LE.NBC)THEN
               E(NOVPI,J)     = CCBC(J,I,1)
               E(NOVPI,NOVPJ) = CCBC(J,I,2)
            ELSE
               E(NOVPI,J)     = CC(J,I-NBC,1)
               E(NOVPI,NOVPJ) = CC(J,I-NBC,NAP1)
            ENDIF
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
      DO I=1,NRD
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
      DO I=1,NRD
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
      SUBROUTINE BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,
     +           SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NCB,ICF2(NOV,*)
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION A2(NOV,NOV,*),BB(NCB,NOV,*)
      DIMENSION SOL1(NOV,*),SOL2(NOV,*),SOL3(NOV,*)
      DIMENSION FAA(NOV,*),FC(*),FCC(*)
C
C Local
      INTEGER I,K
      DOUBLE PRECISION SM
C
      DO L=1,NOV
         SOL1(L,NA) = FCC(L)
         SOL2(L,NA) = FC(L)
      ENDDO
C
      IF(NA.GT.1)THEN
         DO L=1,NOV
            SOL1(L,NA-1)=SOL1(L,NA)
            SOL3(L,NA-1)=SOL2(L,NA)
         ENDDO
      ENDIF
C
C Backsubstitution process; concurrently in each node.
      NAM1=NA-1
      DO I=NAM1,1,-1
         DO K=NOV,1,-1
            SM=0.0D0
            DO L=1,NOV
               SM=SM+SOL1(L,I)*S1(L,K,I)
               SM=SM+SOL3(L,I)*S2(L,K,I)
            ENDDO
            DO L=1,NCB
               SM=SM+FC(NOV+L)*BB(L,K,I)
            ENDDO
            DO L=K+1,NOV
               L1=ICF2(L,I)
               SM=SM+SOL2(L1,I)*A2(L1,K,I)
            ENDDO
            L2=ICF2(K,I)
            SOL2(L2,I)=(FAA(K,I)-SM)/A2(L2,K,I)
         ENDDO
         DO L=1,NOV
            SOL1(L,I+1)=SOL2(L,I)
            IF(I.GT.1)THEN
               SOL3(L,I-1)=SOL2(L,I)
               SOL1(L,I-1)=SOL1(L,I)            
            ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INFPAR(A,B,FA,SOL1,SOL2,FC,
     +  NA,NOV,NRA,NCA,NCB,IRF,ICF,X)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C  Arguments
      INTEGER   NA,NOV,NRA,NCA,NCB,IRF(NRA,*),ICF(NCA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),FA(NRA,*),FC(*)
      DIMENSION SOL1(NOV,*),SOL2(NOV,*),X(*)
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
               SM=SM+A(J,IRFIR,I)*SOL1(J,I)
               SM=SM+A(NRAPJ,IRFIR,I)*SOL2(J,I)
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
C        **Copy SOL1 and X into FA 
         DO J=1,NOV
            FA(J,I)=SOL1(J,I)
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
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NRD,NBC,A,B,C,CCBC,D,FA,FC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRD-NBC,*)
      DIMENSION CCBC(NCA-NRA,NBC,*),D(NCB,*),FA(NRA,*),FC(*)
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
         DO IR=1,NRD
           IF(IR.GT.NBC)THEN
             WRITE(9,103)(C(IC,IR-NBC,I),IC=1,NCA)
           ELSEIF(I.EQ.1)THEN
             WRITE(9,103)(CCBC(IC,IR,1),IC=1,NCA-NRA)
           ELSEIF(I.EQ.NA)THEN
             WRITE(9,103)(CCBC(IC,IR,2),IC=NRA+1,NCA)
           ENDIF
         ENDDO
       ENDDO
C
       WRITE(9,105)
       DO IR=1,NRD
         WRITE(9,103)(D(IC,IR),IC=1,NCB),FC(IR)
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


