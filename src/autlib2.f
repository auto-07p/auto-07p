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
      DIMENSION IAP(*),RAP(*)
C
C Local
      ALLOCATABLE A(:,:,:),B(:,:,:),C(:,:,:),D(:,:),A1(:,:,:),A2(:,:,:)
      ALLOCATABLE S1(:,:,:),S2(:,:,:),BB(:,:,:),CC(:,:,:),CCBC(:,:,:)
      ALLOCATABLE FAA(:,:), FT(:,:)
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
      IPS=IAP(2)
      NTST=IAP(5)
      NCOL=IAP(6)
      NBC=IAP(12)
      NINT=IAP(13)
      IID=IAP(18)
      NFPR=IAP(29)
      NRC=NBC+NINT+1
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
         ALLOCATE(C(NCLM,NINT+1,NTST+1),D(NFPR,NRC))
         ALLOCATE(A1(NDIM,NDIM,NTST+1),A2(NDIM,NDIM,NTST+1))
         ALLOCATE(S1(NDIM,NDIM,NTST+1),S2(NDIM,NDIM,NTST+1))
         ALLOCATE(BB(NFPR,NDIM,NTST+1),CC(NDIM,NINT+1,NTST+1))
         ALLOCATE(CCBC(NDIM,NBC,2),FAA(NDIM,NTST+1))
C
         ALLOCATE(ICF(NCLM,NTST+1),IRF(NROW,NTST+1),IPR(NDIM,NTST+1))
         ALLOCATE(ICF1(NDIM,NTST+1),ICF2(NDIM,NTST+1))
      ENDIF
      ALLOCATE(FT(NROW,NTST+1))
C
      IF(IFST.EQ.1)THEN
        CALL SETUBV(NDIM,IPS,NTST,NCOL,NBC,NINT,
     +   NFPR,NRC-NBC,NROW,NCLM,FUNI,BCNI,ICNI,NDX,
     +   IAP,RAP,PAR,ICP,RDS,A,B,C,CCBC,D,FT,FC,RLCUR,RLOLD,
     +   RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU)
      ELSE
        CALL SETRHS(NDIM,IPS,NTST,NCOL,NBC,NINT,
     +   NFPR,NRC,NROW,NCLM,FUNI,BCNI,ICNI,NDX,
     +   IAP,RAP,PAR,ICP,RDS,FT,FC,RLCUR,RLOLD,
     +   RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU)
      ENDIF
C
      CALL BRBD(A,B,C,D,FT,FC,P0,P1,IFST,
     +  IID,NLLV,DET,NDIM,NTST,NBC,NROW,NCLM,
     +  NFPR,NRC,A1,A2,BB,CC,CCBC,FAA,
     +  S1,S2,IPR,ICF1,ICF2,IRF,ICF)
C
      CALL FAFT(FT,FA,NTST,NROW,NDX)         
C
      RAP(14)=DET
C
      DEALLOCATE(FT)
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
C     ---------- ----
      SUBROUTINE FAFT(FF,FA,NTST,NROW,NDX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FA(NDX,*),FF(NROW,*)
C
      DO I=1,NTST
         DO J=1,NROW
            FA(I,J)=FF(J,I)
         ENDDO
      ENDDO
C
      RETURN
      END
C
C     ---------- ---------
      SUBROUTINE SUBVSR(NDIM,IPS,NA,NBC,NCB,NRC,NRA,BCNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,CCBC,DD,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU)
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
      INTEGER NDIM,IPS,NA,NBC,NCB,NRC,NRA,NDX,IAP(*),ICP(*)
      DOUBLE PRECISION RDS,CCBC(NDIM,NBC,*),DD(NCB,*)
      DOUBLE PRECISION RAP(*),UPS(NDX,*),DUPS(NDX,*)
      DOUBLE PRECISION UOLDPS(NDX,*),UDOTPS(NDX,*)
      DOUBLE PRECISION FC(*),DTM(*),PAR(*)
      DOUBLE PRECISION RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UBC0(:),UBC1(:),FBC(:),DBC(:,:)
      INTEGER I,J,K
      DOUBLE PRECISION RLSUM
C
C Set constants.
       DO I=1,NCB
         PAR(ICP(I))=RLCUR(I)
       ENDDO
C
C     ** Time evolution computations (parabolic systems)
       IF(IPS.EQ.14 .OR. IPS.EQ.16)RAP(15)=RLOLD(1)
C
      ALLOCATE(UBC0(NDIM),UBC1(NDIM),FBC(NBC),DBC(NBC,2*NDIM+NPARX))
C
C     Boundary conditions :
C     
       IF(NBC.GT.0)THEN
         DO I=1,NDIM
            UBC0(I)=UPS(1,I)
            UBC1(I)=UPS(NA+1,I)
         ENDDO
         CALL BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,2,DBC)     
         DO I=1,NBC
            FC(I)=-FBC(I)
            DO K=1,NDIM
               CCBC(K,I,1)=DBC(I,K)
               CCBC(K,I,2)=DBC(I,NDIM+K)
            ENDDO
            DO K=1,NCB
               DD(K,I)=DBC(I,2*NDIM+ICP(K))
            ENDDO
         ENDDO    
C       Save difference :
       ENDIF
       DO J=1,NA+1
         DO I=1,NRA
            DUPS(J,I)=UPS(J,I)-UOLDPS(J,I)
          ENDDO
       ENDDO
C
C     Pseudo-arclength equation :
C
       RLSUM=0.d0
       DO I=1,NCB
          DD(I,NBC+NRC)=THL(ICP(I))*RLDOT(I)
          RLSUM=RLSUM+THL(ICP(I))*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
C
       FC(NBC+NRC)=RDS-RINPR(IAP,NDIM,NDX,UDOTPS,DUPS,DTM,THU)-RLSUM
C
       DEALLOCATE(UBC0,UBC1,FBC,DBC)
       RETURN
       END
C
C     ---------- ---------
      SUBROUTINE SUBVPA(NDIM,NB,NA,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,
     + ICNI,NDX,IAP,RAP,PAR,ICP,AA,BB,CC,DD,FA,FC,
     + UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C     This is the per-CPU parallelized part of SETUBV
C
      EXTERNAL FUNI, BCNI, ICNI
C
      DIMENSION AA(NCA,NRA,*),BB(NCB,NRA,*),CC(NCA,NRC,*),DD(NCB,*)
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),UOLDPS(NDX,*),UDOTPS(NDX,*)
      DIMENSION UPOLDP(NDX,*),FA(NRA,*),FC(*),DTM(*),PAR(*)
      DIMENSION ICP(*),THU(*),WI(*),WP(NCOL+1,*),WT(NCOL+1,*)
C
C Local
      ALLOCATABLE DFDU(:),DFDP(:),UOLD(:),U(:),F(:)
      ALLOCATABLE FICD(:),DICD(:),UIC(:),UIO(:),UID(:)
      ALLOCATABLE UIP(:),WPLOC(:)
      DIMENSION PRM(NPARX)
C
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPARX),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),FICD(NINT))
      ALLOCATE(DICD(NINT*(NDIM+NPARX)),UIC(NDIM),UIO(NDIM))
      ALLOCATE(UID(NDIM),UIP(NDIM),WPLOC(NCOL+1))
C
C Set constants.
       NCP1=NCOL+1
C
C Generate AA , BB and FA :
C
       DO 2 J=NB,NA
          JP1=J+1
          DO 1 IC=1,NCOL
             DO K=1,NDIM
                U(K)=   WT(NCP1,IC)*   UPS(JP1,K)
                UOLD(K)=WT(NCP1,IC)*UOLDPS(JP1,K)
                DO L=1,NCOL
                   L1=(L-1)*NDIM+K
                   U(K)=U(K)        +WT(L,IC)*   UPS(J,L1)
                   UOLD(K) =UOLD(K) +WT(L,IC)*UOLDPS(J,L1)
                ENDDO
             ENDDO
             DO I=1,NPARX
                PRM(I)=PAR(I)
             ENDDO
             CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,2,F,DFDU,DFDP)
C     transpose DFDU for optimal access
             DO II=0,NDIM-1
                DO JJ=0,II-1
                   TMP=DFDU(1+II+JJ*NDIM)
                   DFDU(1+II+JJ*NDIM)=DFDU(1+JJ+II*NDIM)
                   DFDU(1+JJ+II*NDIM)=TMP
                ENDDO
             ENDDO
             IC1=(IC-1)*NDIM
             DO IB=1,NCP1
                WPLOC(IB)=WP(IB,IC)/DTM(J)
             ENDDO
             DO I=1,NDIM
                IB1=0
                IDFDU=(I-1)*NDIM
                DO IB=1,NCP1
                   WTTMP=-WT(IB,IC)
                   DO K=1,NDIM
                      AA(IB1+K,IC1+I,J)=WTTMP*DFDU(IDFDU+K)
                   ENDDO
                   AA(IB1+I,IC1+I,J)=AA(IB1+I,IC1+I,J)+WPLOC(IB)
                   IB1=IB1+NDIM
                ENDDO
                DO K=1,NCB
                   BB(K,IC1+I,J)=-DFDP((ICP(K)-1)*NDIM+I)
                ENDDO
                FA(IC1+I,J)=F(I)-WPLOC(NCP1)*UPS(JP1,I)
                DO K=1,NCOL
                   K1=(K-1)*NDIM+I
                   FA(IC1+I,J)=FA(IC1+I,J)-WPLOC(K)*UPS(J,K1)
                ENDDO
             ENDDO
 1        CONTINUE
 2     CONTINUE
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
C     Integral constraints :
C     
       IF(NINT.GT.0)THEN   
         DO J=NB,NA
            JP1=J+1
            DO K=1,NCP1
               DO I=1,NDIM
                  I1=(K-1)*NDIM+I
                  J1=J
                  IF(K.EQ.NCP1)I1=I
                  IF(K.EQ.NCP1)J1=JP1
                  UIC(I)=UPS(J1,I1)
                  UIO(I)=UOLDPS(J1,I1)
                  UID(I)=UDOTPS(J1,I1)
                  UIP(I)=UPOLDP(J1,I1)
               ENDDO
               CALL ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,
     *              FICD,2,DICD)
               DO M=1,NINT
                  DO I=1,NDIM
                     K1=(K-1)*NDIM+I
                     CC(K1,M,J)=DTM(J)*WI(K)*DICD((I-1)*NINT+M)
                  ENDDO
                  DO I=1,NCB
                     DD(I,M)=DD(I,M)
     *                    +DTM(J)*WI(K)*DICD((NDIM+ICP(I)-1)*NINT+M)
                  ENDDO
                  FC(M)=FC(M)-DTM(J)*WI(K)*FICD(M)
               ENDDO
            ENDDO
         ENDDO
       ENDIF
C     
C     Pseudo-arclength equation :
C     
       DO J=NB,NA
          JP1=J+1
          DO I=1,NDIM
             DO K=1,NCOL
                K1=(K-1)*NDIM+I
                CC(K1,NRC,J)=DTM(J)*THU(I)*WI(K)*UDOTPS(J,K1)
             ENDDO
             CC(NRA+I,NRC,J)=
     +            DTM(J)*THU(I)*WI(NCP1)*UDOTPS(JP1,I)
          ENDDO
       ENDDO
C     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,FICD,DICD)
       DEALLOCATE(UIC,UIO,UID,UIP,WPLOC)
       RETURN
       END
C
C     ---------- ---------
      SUBROUTINE SETUBV(NDIM,IPS,NA,NCOL,NBC,NINT,
     + NCB,NRC,NRA,NCA,FUNI,BCNI,ICNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,AA,BB,CC,CCBC,DD,FA,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU)
C
C$    USE OMP_LIB
      IMPLICIT NONE
C
      EXTERNAL FUNI, BCNI, ICNI
C
      INTEGER NDIM,IPS,NA,NCOL,NBC,NINT,NCB,NRC,NRA,NCA,NDX
      INTEGER IAP(*),ICP(*)
      DOUBLE PRECISION AA(NCA,NRA,*),BB(NCB,NRA,*)
      DOUBLE PRECISION CC(NCA,NRC,*),CCBC(NDIM,NBC,*),DD(NCB,*),RAP(*)
      DOUBLE PRECISION UPS(NDX,*),DUPS(NDX,*),UOLDPS(NDX,*)
      DOUBLE PRECISION UDOTPS(NDX,*),UPOLDP(NDX,*),FA(NRA,*),FC(*)
      DOUBLE PRECISION DTM(*),PAR(*),THL(*),THU(*)
      DOUBLE PRECISION RDS,RLCUR(*),RLOLD(*),RLDOT(*)
C
C Local
      ALLOCATABLE WI(:),WP(:,:),WT(:,:),DDD(:,:),FCFC(:)
      DOUBLE PRECISION WI,WP,WT,DDD,FCFC
      INTEGER NB,NU,J,K,IAM,NT
C
      ALLOCATE(WI(NCOL+1),WP(NCOL+1,NCOL),WT(NCOL+1,NCOL))
C
      CALL SUBVSR(NDIM,IPS,NA,NBC,NCB,NRC,NRA,BCNI,NDX,
     +     IAP,RAP,PAR,ICP,RDS,CCBC,DD,FC,RLCUR,RLOLD,
     +     RLDOT,UPS,UOLDPS,UDOTPS,DUPS,DTM,THL,THU)
C
      DO J=1,NINT
         FC(NBC+J)=0.d0
         DO K=1,NCB
            DD(K,NBC+J)=0.d0
         ENDDO
      ENDDO
C
      CALL WINT(NCOL+1,WI)
      CALL GENWTS(NCOL,NCOL+1,WT,WP)
C
C$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IAM,NT,DDD,FCFC,J,K,NB,NU)
      IAM = 0
      NT = 1
C$    IAM = OMP_GET_THREAD_NUM()
C$    NT = OMP_GET_NUM_THREADS()
      ALLOCATE(DDD(NCB,NINT),FCFC(NINT))
      NB = IAM*NA/NT+1
      NU = (IAM+1)*NA/NT
      CALL SUBVPA(NDIM,NB,NU,NCOL,NINT,NCB,NRC,NRA,NCA,FUNI,ICNI,NDX,
     +      IAP,RAP,PAR,ICP,AA,BB,CC,DDD,FA,FCFC,
     +      UPS,UOLDPS,UDOTPS,UPOLDP,DTM,THU,WI,WP,WT)
C
C     This is were we sum into the global copy of the d array
C
      DO J=1,NINT
         DO K=1,NCB
C$OMP ATOMIC
            DD(K,NBC+J)=DD(K,NBC+J)+DDD(K,J)
         ENDDO
C$OMP ATOMIC
         FC(NBC+J)=FC(NBC+J)+FCFC(J)
      ENDDO
      DEALLOCATE(DDD,FCFC)
C$OMP END PARALLEL
C
      DEALLOCATE(WI,WP,WT)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE SETRHS(NDIM,IPS,NA,NCOL,NBC,NINT,
     + NCB,NRC,NRA,NCA,FUNI,BCNI,ICNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,FA,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
      EXTERNAL FUNI, BCNI, ICNI
C
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),DUPS(NDX,*)
      DIMENSION UOLDPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION FA(NRA,*),FC(*),DTM(*),PAR(*)
      DIMENSION ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      ALLOCATABLE DFDU(:),DFDP(:),UOLD(:),U(:),F(:),UBC0(:),UBC1(:)
      ALLOCATABLE FBC(:),DBC(:),FICD(:),DICD(:),UIC(:),UIO(:),UID(:)
      ALLOCATABLE UIP(:),WPLOC(:,:),WI(:),WP(:,:),WT(:,:)
      DIMENSION PRM(NPARX)
C
      ALLOCATE(DFDU(NDIM*NDIM),DFDP(NDIM*NPARX),UOLD(NDIM),U(NDIM))
      ALLOCATE(F(NDIM),UBC0(NDIM),UBC1(NDIM),FBC(NBC))
      ALLOCATE(DBC(NBC*(2*NDIM+NPARX)),FICD(NINT))
      ALLOCATE(DICD(NINT*(NDIM+NPARX)),UIC(NDIM),UIO(NDIM))
      ALLOCATE(UID(NDIM),UIP(NDIM),WPLOC(NCOL+1,NCOL),WI(NCOL+1))
      ALLOCATE(WP(NCOL+1,NCOL),WT(NCOL+1,NCOL))
C
       CALL WINT(NCOL+1,WI)
       CALL GENWTS(NCOL,NCOL+1,WT,WP)
C
C Initialize to zero.
       DO I=1,NRC
         FC(I)=0.d0
       ENDDO
C
C Set constants.
       NCP1=NCOL+1
       DO I=1,NCB
         PAR(ICP(I))=RLCUR(I)
       ENDDO
C
C Generate FA :
C
       DO 2 J=1,NA
          JP1=J+1
          DT=DTM(J)
          DDT=1.d0/DT
          DO IC=1,NCOL
             DO IB=1,NCP1
                WPLOC(IB,IC)=DDT*WP(IB,IC)
             ENDDO
          ENDDO
          DO 1 IC=1,NCOL
             DO K=1,NDIM
                U(K)   =WT(NCP1,IC)*UPS(JP1,K)
                UOLD(K)=WT(NCP1,IC)*UOLDPS(JP1,K)
                DO L=1,NCOL
                   L1=(L-1)*NDIM+K
                   U(K)   =U(K)   +WT(L,IC)*UPS(J,L1)
                   UOLD(K)=UOLD(K)+WT(L,IC)*UOLDPS(J,L1)
                ENDDO
             ENDDO

C     ** Time evolution computations (parabolic systems)
             IF(IPS.EQ.14 .OR. IPS.EQ.16)RAP(15)=RLOLD(1)

             DO I=1,NPARX
                PRM(I)=PAR(I)
             ENDDO
             CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,0,F,DFDU,DFDP)
             IC1=(IC-1)*NDIM
             DO I=1,NDIM
                FA(IC1+I,J)=F(I)-WPLOC(NCP1,IC)*UPS(JP1,I)
                DO K=1,NCOL
                   K1=(K-1)*NDIM+I
                   FA(IC1+I,J)=FA(IC1+I,J)-WPLOC(K,IC)*UPS(J,K1)
                ENDDO
             ENDDO
 1        CONTINUE
 2     CONTINUE
C     
C     Generate FC :
C     
C     Boundary conditions :
C     
       IF(NBC.GT.0)THEN
         DO I=1,NDIM
            UBC0(I)=UPS(1,I)
            UBC1(I)=UPS(NA+1,I)
        ENDDO
         CALL BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,2,DBC)
         DO I=1,NBC
            FC(I)=-FBC(I)
         ENDDO
C       Save difference :
         DO J=1,NA+1
            DO I=1,NRA
               DUPS(J,I)=UPS(J,I)-UOLDPS(J,I)
            ENDDO
         ENDDO
       ENDIF
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
                  UIC(I)=UPS(J1,I1)
                  UIO(I)=UOLDPS(J1,I1)
                  UID(I)=UDOTPS(J1,I1)
                  UIP(I)=UPOLDP(J1,I1)
               ENDDO
               CALL ICNI(IAP,RAP,NDIM,PAR,ICP,NINT,UIC,UIO,UID,UIP,
     *              FICD,2,DICD)
               DO M=1,NINT
                  FC(NBC+M)=FC(NBC+M)-DTM(J)*WI(K)*FICD(M)
               ENDDO
            ENDDO
         ENDDO
       ENDIF
C     
C     Pseudo-arclength equation :
       RLSUM=0.d0
       DO I=1,NCB
          RLSUM=RLSUM+THL(ICP(I))*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
C     
       FC(NRC)=RDS-RINPR(IAP,NDIM,NDX,UDOTPS,DUPS,DTM,THU)-RLSUM
C     
       DEALLOCATE(DFDU,DFDP,UOLD,U,F,UBC0,UBC1,FBC,DBC,FICD,DICD)
       DEALLOCATE(UIC,UIO,UID,UIP,WPLOC,WI,WP,WT)
       RETURN
       END
C
C     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,FA,FC,P0,P1,IFST,
     +  IDB,NLLV,DET,NOV,NA,NBC,NRA,NCA,
     +  NCB,NRC,A1,A2,BB,CC,CCBC,FAA,
     +  S1,S2,IPR,ICF1,ICF2,IRF,ICF)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   IFST,IDB,NLLV,NOV,NA,NBC,NRA
      INTEGER   NCA,NCB,NRC
      DOUBLE PRECISION DET
      DOUBLE PRECISION A(*),B(*),C(*),D(NCB,*),FA(*),FC(*),P0(*),P1(*)
      DOUBLE PRECISION A1(*),A2(*),BB(*),CC(*),CCBC(*),FAA(*)
      DOUBLE PRECISION S1(*),S2(*)
      INTEGER   IPR(*),ICF1(*),ICF2(*),IRF(*),ICF(*)      
C
C Local
      DOUBLE PRECISION SOL1,SOL2,SOL3,FCC,E
      INTEGER IR,IC
      ALLOCATABLE SOL1(:),SOL2(:),SOL3(:),FCC(:),E(:,:),IR(:),IC(:)
      ALLOCATE(SOL1(NOV*(NA+1)),SOL2(NOV*(NA+1)),SOL3(NOV*(NA+1)))
      ALLOCATE(FCC(2*NOV+NRC+2*NOV*NOV+1),E(NOV+NRC,2*NOV+NRC))
      ALLOCATE(IR(2*NOV+NRC+2*NOV*NOV+1),IC(2*NOV+NRC+2*NOV*NOV+1))
C
      IF(IDB.GT.4)
     +     CALL PRINT1(NA,NRA,NCA,NCB,NRC,NBC,A,B,C,CCBC,D,FA,FC)

      IF(IFST.EQ.1)THEN
         CALL CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NRC-NBC,C,
     +        D(1,NBC+1),IRF,ICF)
         CALL COPYCP(NA,NOV,NRA,NCA,A,NCB,B,NRC-NBC,C,A1,A2,BB,CC,IRF)
      ENDIF
C
      IF(NLLV.EQ.0)THEN
         CALL CONRHS(NOV,NA,NRA,NCA,A,NRC-NBC,C,FA,FC(NBC+1),IRF,ICF)
      ELSE
         CALL SETZERO(FA,FC,NA,NRA,NRC)
      ENDIF
      CALL CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
C
      IF(IFST.EQ.1)
     +     CALL REDUCE(A1,A2,BB,CC,D(1,NBC+1),
     +     NA,NOV,NCB,NRC-NBC,S1,S2,ICF1,ICF2,IPR)
C
      IF(NLLV.EQ.0)
     +     CALL REDRHS(A1,A2,CC,
     +     FAA,FC(NBC+1),NA,NOV,NCB,NRC-NBC,ICF1,ICF2,IPR)
C
      CALL DIMRGE(E,CC,CCBC,D,FC,IR,IC,IFST,
     +     NA,NRC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
C
      CALL BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,
     +     SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2)
C
      CALL INFPAR(A,B,FA,SOL1,SOL2,FC,
     *     NA,NOV,NRA,NCA,NCB,IRF,ICF)
C
      DEALLOCATE(SOL1,SOL2,SOL3,FCC,E,IR,IC)
      RETURN
      END
C
C     ---------- -------
      SUBROUTINE SETZERO(FA,FC,NA,NRA,NRC)
C
      IMPLICIT NONE
C
C Arguments
      INTEGER   NA,NRA,NRC
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
      DO I=1,NRC
        FC(I)=0.D0
      ENDDO
C
      RETURN
      END
C
C     This is the per-CPU process function of CONPAR
C     ---------- ------
      SUBROUTINE CONPAP(NOV,NA,NRA,NCA,A,NCB,B,NRC,C,D,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NCB,NRC,ICF(NCA,*),IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION D(NCB,*)
C Local
      ALLOCATABLE IAMAX(:)
C
C Note that the summation of the adjacent overlapped part of C
C is delayed until REDUCE, in order to merge it with other communications.
C NA is the local NTST.
C
      NEX=NCA-2*NOV
      ALLOCATE(IAMAX(NRA))
C
C Condensation of parameters (Elimination of local variables).
C
      M1    = NOV+1
      M2    = NOV+NEX
C     
      ZERO = 0.D0
C
      DO 2 I=1,NA
         DO J=1,NRA
            IRF(J,I)=J
            IAMAX(J)=NOV+IDAMAX(NEX,A(M1,J,I),1)
         ENDDO
         DO J=1,NCA
            ICF(J,I)=J
         ENDDO
         DO 1 IC=M1,M2
            IR1=IC-NOV+1
            IRP=IR1-1
            ICP1=IC+1
C           **Search for pivot (Complete pivoting)
            PIV = ZERO
            IPIV = IRP
            JPIV = IC
            DO K1=IRP,NRA
               IROW=IRF(K1,I)
               TPIV = DABS(A(ICF(IAMAX(IROW),I),IROW,I))
               IF(PIV.LT.TPIV)THEN
                  PIV = TPIV
                  IPIV = K1
                  JPIV = IAMAX(IROW)
               ENDIF
            ENDDO
C           **Move indices
            ITMP        = ICF(IC,I)
            ICF(IC,I)   = ICF(JPIV,I)
            ICF(JPIV,I) = ITMP
            ICFIC       = ICF(IC,I)
            ITMP        = IRF(IRP,I)
            IRF(IRP,I)  = IRF(IPIV,I)
            IRF(IPIV,I) = ITMP
            IRFIRP      = IRF(IRP,I)
            PIV=A(ICF(IC,I),IRF(IRP,I),I)
C           **End of pivoting; elimination starts here
            DO IR=IR1,NRA
               IRFIR=IRF(IR,I)
               RM=A(ICFIC,IRFIR,I)/PIV
               A(ICFIC,IRFIR,I)=RM
	       IF(RM.NE.0.0)THEN
                  DO L=1,NOV
                     A(L,IRFIR,I)=A(L,IRFIR,I)-RM*A(L,IRFIRP,I)
                  ENDDO
                  DO L=ICP1,NCA
                     A(ICF(L,I),IRFIR,I)=
     +                    A(ICF(L,I),IRFIR,I)-RM*A(ICF(L,I),IRFIRP,I)
                  ENDDO
                  DO L=1,NCB
                     B(L,IRFIR,I)=B(L,IRFIR,I)-RM*B(L,IRFIRP,I)
                  ENDDO
	       ENDIF
               IF((RM.NE.0.0).OR.(IAMAX(IRFIR).EQ.JPIV))THEN
                  PPIV=0d0
                  JPPIV=ICP1
C     Recalculate absolute maximum for current row
                  DO L=ICP1,M2
                     TPIV=DABS(A(ICF(L,I),IRFIR,I))
                     IF(PPIV.LT.TPIV)THEN
                        PPIV=TPIV
                        JPPIV=L
                     ENDIF
                  ENDDO
                  IAMAX(IRFIR)=JPPIV
               ELSEIF(IAMAX(IRFIR).EQ.IC)THEN
                  IAMAX(IRFIR)=JPIV
               ENDIF
            ENDDO
            DO IR=1,NRC
               RM=C(ICF(IC,I),IR,I)/PIV
               C(ICF(IC,I),IR,I)=RM
	       IF(RM.NE.0.0)THEN
               DO L=1,NOV
                  C(L,IR,I)=C(L,IR,I)-RM*A(L,IRFIRP,I)
               ENDDO
               DO L=ICP1,NCA
                  C(ICF(L,I),IR,I)=C(ICF(L,I),IR,I)-RM*
     +                 A(ICF(L,I),IRFIRP,I)
               ENDDO
               DO L=1,NCB
                  D(L,IR)=D(L,IR)-RM*B(L,IRFIRP,I)
               ENDDO
	       ENDIF
            ENDDO
 1       CONTINUE
 2    CONTINUE
C
      DEALLOCATE(IAMAX)
      RETURN
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
      INTEGER I,J,K,N,IAM,NT
      DOUBLE PRECISION, ALLOCATABLE :: DD(:,:)
      IF(NCA.EQ.2*NOV)RETURN
C
C Condensation of parameters (Elimination of local variables).
C
C$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IAM,NT,DD,J,K,I,N)
      IAM = 0
      NT = 1
C$    IAM = OMP_GET_THREAD_NUM()
C$    NT = OMP_GET_NUM_THREADS()
      ALLOCATE(DD(NCB,NRC))
      DO J=1,NRC
         DO K=1,NCB
            DD(K,J)=0.0d0
         ENDDO
      ENDDO
      I = IAM*NA/NT+1
      N = (IAM+1)*NA/NT+1-I
      CALL CONPAP(NOV,N,NRA,NCA,A(1,1,I),NCB,B(1,1,I),NRC,C(1,1,I),DD,
     +     IRF(1,I),ICF(1,I))
C
C     This is were we sum into the global copy of the d array
C
      DO J=1,NRC
         DO K=1,NCB
C$OMP ATOMIC
            D(K,J)=D(K,J)+DD(K,J)
         ENDDO
      ENDDO
      DEALLOCATE(DD)
C$OMP END PARALLEL
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
     +  NA,NRC,NBC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NRC,NOV,NCB,IDB,NLLV
      INTEGER   IR(*),IC(*)
      DIMENSION E(NOV+NRC,*),CC(NOV,NRC-NBC,*),CCBC(NOV,NBC,*),D(NCB,*)
      DIMENSION P0(NOV,*),P1(NOV,*),S(NOV,NOV,*)
      DIMENSION FAA(NOV,*),A2(NOV,NOV,*),BB(NCB,NOV,*)
      DIMENSION FC(*),FCC(*)
C
C Local
      INTEGER  I,J
      ALLOCATABLE XE(:)
      ALLOCATE(XE(NOV+NRC))
C
      NAP1    = NA+1
      NCR     = NRC+NOV
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
      DO I=1,NRC
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
      DO I=1,NRC
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
      DO I=1,NRC
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
     +  NA,NOV,NRA,NCA,NCB,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C  Arguments
      INTEGER   NA,NOV,NRA,NCA,NCB,IRF(NRA,*),ICF(NCA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),FA(NRA,*),FC(*)
      DIMENSION SOL1(NOV,*),SOL2(NOV,*)
C
C Local
      DOUBLE PRECISION SM
      ALLOCATABLE X(:)
      ALLOCATE(X(NRA))
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
      DEALLOCATE(X)
      RETURN
      END
C           
C     ---------- ------
      SUBROUTINE PRINT1(NA,NRA,NCA,NCB,NRC,NBC,A,B,C,CCBC,D,FA,FC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
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
         DO IR=1,NRC
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
       DO IR=1,NRC
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


