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
      PARAMETER (NROWX=NDIMX*NCOLX,NCLMX=NROWX+NDIMX,
     *           NRCX=NBCX+NINTX+1,NCCX=NCLMX)
C
      PARAMETER (M1AA=NCLMX,M2AA=NROWX,M3AA=NTSTX,
     *           M1BB=NPARX,M2BB=NROWX,M3BB=NTSTX,
     *           M1CC=NCCX,M2CC=NRCX,M3CC=NTSTX,
     *           M1DD=NRCX,M2DD=NPARX)
      PARAMETER (NX=NDIMX,NX2=NX**2,NXP=NX*NPARX)
C
C     NAX is the local NTSTX, which is smaller than the global NTSTX.
C     NODES is the total number of nodes.
C
      PARAMETER (NAX=(NTSTX/NODES)+1)
C
C Sets up and solves the linear equations for one Newton/Chord iteration
C
      EXTERNAL FUNI,BCNI,ICNI
      DIMENSION IAP(*),RAP(*)
C
C Local
      LOGICAL IPAR
      DIMENSION FF(M2AA*(M3AA+1)),FT(M2AA*NAX)
C
C Most of the required memory is allocated below
      COMMON /BLAUT/ A(M1AA*M2AA*NAX),B(M1BB*M2BB*NAX),
     *       C(M1CC*M2CC*NAX),D(M1DD*M2DD), 
     *       A1((NDIMX**2)*NAX),A2((NDIMX**2)*NAX),
     *       S1((NDIMX**2)*NAX),S2((NDIMX**2)*NAX),
     *       BB(NDIMX*NPARX*NAX),
     *       CC(NRCX*NDIMX*(NAX+1)),
     *       FAA(NDIMX*NAX),
     *       CA1((NDIMX**2)*KREDO),
     *       ICF(NCLMX*NAX),IRF(NROWX*NAX),
     *       IPR(NDIMX*NAX),
     *       ICF11(NDIMX*KREDO),
     *       ICF1(NDIMX*NAX),ICF2(NDIMX*NAX),
     *       NP(2**KREDO)
       COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
C
      IAM=IAP(38)
      KWT=IAP(39)
      IF(KWT.GT.1)THEN
        IPAR=.TRUE.
      ELSE
        IPAR=.FALSE.
      ENDIF
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
C
      IF(KWT.GT.NTST)THEN
        PRINT*,'NTST is less than the number of nodes'
        STOP
      ELSE
        CALL PARTITION(NTST,KWT,NP)
      ENDIF
C
C     NTST0 is the global one, NTST is the local one.
C     The value of NTST may be different in different nodes.
      NTST0=NTST
      NTST=NP(IAM+1)
C
      IF(IFST.EQ.1)THEN
        CALL SETUBV(NDIM,IPS,NTST,NTST0,NP,NCOL,NBC,NINT,
     +   NFPR,NRC,NROW,NCLM,IAM,KWT,IPAR,FUNI,BCNI,ICNI,NDX,
     +   IAP,RAP,PAR,ICP,RDS,A,B,C,D,FT,FC,RLCUR,RLOLD,
     +   RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
      ELSE
        CALL SETRHS(NDIM,IPS,NTST,NTST0,NP,NCOL,NBC,NINT,
     +   NFPR,NRC,NROW,NCLM,IAM,KWT,IPAR,FUNI,BCNI,ICNI,NDX,
     +   IAP,RAP,PAR,ICP,RDS,FT,FC,RLCUR,RLOLD,
     +   RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
      ENDIF
C
C     The matrix D and FC are set to zero for all nodes except the first.
      IF(IAM.GT.0)CALL SETFCDD(IFST,D,FC,NFPR,NRC)
C
      CALL BRBD(A,B,C,D,FT,FC,P0,P1,IFST,
     +  IID,NLLV,DET,NDIM,NTST,NBC,NROW,NCLM,
     +  NFPR,NRC,IAM,KWT,IPAR,A1,A2,BB,CC,FAA,CA1,
     +  S1,S2,ICF11,IPR,ICF1,ICF2,IRF,ICF)
C
      LENFT=NTST*NROW*8
      LENFF=NTST0*NROW*8
      LENFF2=M2AA*(M3AA+1)*8
C
      IF(IPAR)THEN
C        Global concatenation of the solution from each node. 
         CALL GCOL(FT,LENFT,FF,LENFF2,LENFF)
         CALL FAFT(FF,FA,NTST0,NROW,NDX)
      ELSE
         CALL FAFT(FT,FA,NTST0,NROW,NDX)         
      ENDIF 
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
      SUBROUTINE PARTITION(N,KWT,M)
C
C     Linear distribution of NTST over all nodes
      INTEGER N,KWT,M(*)
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
C     ------- -------- ------
      INTEGER FUNCTION MYPART(IAM,NP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     Partition the mesh
C
      INTEGER IAM,NP(*)
C
        K=0
        DO I=1,IAM
          K=K+NP(I)
        ENDDO
        MYPART=K
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE SETUBV(NDIM,IPS,NA,NTST,NP,NCOL,NBC,NINT,
     + NCB,NRC,NRA,NCA,IAM,KWT,IPAR,FUNI,BCNI,ICNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,AA,BB,CC,DD,FA,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
      PARAMETER (M2BC=2*NDIMX+NPARX,M2INT=NDIMX+NPARX)
      PARAMETER (MCL1=NCOLX,MCL2=MCL1+1)
C
      LOGICAL  IPAR
      EXTERNAL FUNI, BCNI, ICNI
C
      DIMENSION AA(NCA,NRA,*),BB(NCB,NRA,*)
      DIMENSION CC(NCA,NRC,*),DD(NCB,*)
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),DUPS(NDX,*)
      DIMENSION UOLDPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION FA(NRA,*),FC(*),DTM(*),NP(*),PAR(*)
      DIMENSION ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      DIMENSION DFDU(NDIMX**2),DFDP(NDIMX*NPARX)
      DIMENSION UOLD(NDIMX),U(NDIMX),F(NDIMX),PRM(NPARX)
      DIMENSION UBC0(NDIMX),UBC1(NDIMX),FBC(NBCX),DBC(NBCX*M2BC)
      DIMENSION FICD(NINTX),DICD(NINTX*M2INT)
      DIMENSION UIC(NDIMX),UIO(NDIMX),UID(NDIMX),UIP(NDIMX)
      DIMENSION WPLOC(MCL2,MCL1),WI(MCL2),WP(MCL2,MCL1),WT(MCL2,MCL1)
C
       CALL WINT(NCOL+1,WI)
       CALL GENWTS(NCOL,MCL2,WT,WP)
C
C Initialize to zero.
C
       DO I=1,NRC
         FC(I)=0.d0
         DO K=1,NCB
           DD(K,I)=0.d0
         ENDDO
       ENDDO
C
C  NA is the local node's mesh interval number.
C
       DO I=1,NA
         DO J=1,NRA
           DO K=1,NCA
             AA(K,J,I)=0.d0
           ENDDO
         ENDDO
         DO J=1,NRA
           DO K=1,NCB
             BB(K,J,I)=0.d0
           ENDDO
         ENDDO
         DO J=1,NCA
           DO K=1,NRC
             CC(J,K,I)=0.d0
           ENDDO
         ENDDO
      ENDDO
C
C Set constants.
       NCP1=NCOL+1
       DO I=1,NCB
         PAR(ICP(I))=RLCUR(I)
       ENDDO
C
C Generate AA , BB and FA :
C
C      Partition the mesh intervals
C
       MPART=MYPART(IAM,NP)
C
       DO 2 JJ=1,NA
          J=JJ+MPART
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
                U(K)=   WT(NCP1,IC)*   UPS(JP1,K)
                UOLD(K)=WT(NCP1,IC)*UOLDPS(JP1,K)
                DO L=1,NCOL
                   L1=(L-1)*NDIM+K
                   U(K)=U(K)        +WT(L,IC)*   UPS(J,L1)
                   UOLD(K) =UOLD(K) +WT(L,IC)*UOLDPS(J,L1)
                ENDDO
             ENDDO
C
C     ** Time evolution computations (parabolic systems)
             IF(IPS.EQ.14 .OR. IPS.EQ.16)RAP(15)=RLOLD(1)
C
             DO I=1,NPARX
                PRM(I)=PAR(I)
             ENDDO
             CALL FUNI(IAP,RAP,NDIM,U,UOLD,ICP,PRM,2,F,DFDU,DFDP)
             IC1=(IC-1)*NDIM
             DO IB=1,NCP1
                IB1=(IB-1)*NDIM
                DO I=1,NDIM
                   AA(IB1+I,IC1+I,JJ)=WPLOC(IB,IC)
                   DO K=1,NDIM
                      AA(IB1+K,IC1+I,JJ)=AA(IB1+K,IC1+I,JJ)
     *                     -WT(IB,IC)*DFDU((K-1)*NDIM+I)
                   ENDDO
                ENDDO
             ENDDO
             DO I=1,NDIM
                DO K=1,NCB
                   BB(K,IC1+I,JJ)=-DFDP((ICP(K)-1)*NDIM+I)
                ENDDO
                FA(IC1+I,JJ)=F(I)-WPLOC(NCP1,IC)*UPS(JP1,I)
                DO K=1,NCOL
                   K1=(K-1)*NDIM+I
                   FA(IC1+I,JJ)=FA(IC1+I,JJ)-WPLOC(K,IC)*UPS(J,K1)
                ENDDO
             ENDDO
 1        CONTINUE
 2     CONTINUE
C     
C     Generate CC, DD and FC :
C     
C     Boundary conditions :
C     
       IF(NBC.GT.0)THEN
         DO I=1,NDIM
            UBC0(I)=UPS(1,I)
            UBC1(I)=UPS(NTST+1,I)
         ENDDO
         CALL BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,2,DBC)     
         DO I=1,NBC
            FC(I)=-FBC(I)
            DO K=1,NDIM
              IF(IAM.EQ.0)CC(K,I,1)=DBC((K-1)*NBC+I)
              IF(IAM.EQ.(KWT-1))CC(NRA+K,I,NA)=
     +                           DBC((NDIM+K-1)*NBC+I)
            ENDDO
            DO K=1,NCB
               DD(K,I)=DBC((2*NDIM+ICP(K)-1)*NBC+I)
            ENDDO
         ENDDO    
C       Save difference :    
         DO J=1,NTST+1
            DO I=1,NRA
               DUPS(J,I)=UPS(J,I)-UOLDPS(J,I)
            ENDDO
         ENDDO
       ENDIF
C     
C     Integral constraints :
C     
       IF(NINT.GT.0)THEN   
         DO JJ=1,NA
            J=JJ+MPART
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
                     CC(K1,NBC+M,JJ)=DTM(J)*WI(K)*DICD((I-1)*NINT+M)
                  ENDDO
                  DO I=1,NCB
                     DD(I,NBC+M)=DD(I,NBC+M)
     *                    +DTM(J)*WI(K)*DICD((NDIM+ICP(I)-1)*NINT+M)
                  ENDDO
                  FC(NBC+M)=FC(NBC+M)-DTM(J)*WI(K)*FICD(M)
               ENDDO
            ENDDO
         ENDDO
       ENDIF
C     
C     Pseudo-arclength equation :
C     
       DO JJ=1,NA
          J=JJ+MPART
          JP1=J+1
          DO I=1,NDIM
             DO K=1,NCOL
                K1=(K-1)*NDIM+I
                CC(K1,NRC,JJ)=DTM(J)*THU(I)*WI(K)*UDOTPS(J,K1)
             ENDDO
             CC(NRA+I,NRC,JJ)=
     +            DTM(J)*THU(I)*WI(NCP1)*UDOTPS(JP1,I)
          ENDDO
       ENDDO
C     
       RLSUM=0.d0
       DO I=1,NCB
          DD(I,NRC)=THL(ICP(I))*RLDOT(I)
          RLSUM=RLSUM+THL(ICP(I))*(RLCUR(I)-RLOLD(I))*RLDOT(I)
       ENDDO
C     
       FC(NRC)=RDS-RINPR(IAP,NDIM,NDX,UDOTPS,DUPS,DTM,THU)-RLSUM
C     
       RETURN
       END
C
C     ---------- ------
      SUBROUTINE SETRHS(NDIM,IPS,NA,NTST,NP,NCOL,NBC,NINT,
     + NCB,NRC,NRA,NCA,IAM,KWT,IPAR,FUNI,BCNI,ICNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,FA,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
      PARAMETER (M2BC=2*NDIMX+NPARX,M2INT=NDIMX+NPARX)
      PARAMETER (MCL1=NCOLX,MCL2=MCL1+1)
C
      LOGICAL  IPAR
      EXTERNAL FUNI, BCNI, ICNI
C
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),DUPS(NDX,*)
      DIMENSION UOLDPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION FA(NRA,*),FC(*),DTM(*),NP(*),PAR(*)
      DIMENSION ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      DIMENSION DFDU(NDIMX**2),DFDP(NDIMX*NPARX)
      DIMENSION UOLD(NDIMX),U(NDIMX),F(NDIMX),PRM(NPARX)
      DIMENSION UBC0(NDIMX),UBC1(NDIMX),FBC(NBCX),DBC(NBCX*M2BC)
      DIMENSION FICD(NINTX),DICD(NINTX*M2INT)
      DIMENSION UIC(NDIMX),UIO(NDIMX),UID(NDIMX),UIP(NDIMX)
      DIMENSION WPLOC(MCL2,MCL1),WI(MCL2),WP(MCL2,MCL1),WT(MCL2,MCL1)
C
       IAM=IAP(38)
       KWT=IAP(39)
       IF(KWT.GT.1)THEN
         IPAR=.TRUE.
       ELSE
         IPAR=.FALSE.
       ENDIF
C
       CALL WINT(NCOL+1,WI)
       CALL GENWTS(NCOL,MCL2,WT,WP)
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
C      Partition the mesh intervals.
       MPART=MYPART(IAM,NP)
C
       DO 2 JJ=1,NA
          J=JJ+MPART
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
                FA(IC1+I,JJ)=F(I)-WPLOC(NCP1,IC)*UPS(JP1,I)
                DO K=1,NCOL
                   K1=(K-1)*NDIM+I
                   FA(IC1+I,JJ)=FA(IC1+I,JJ)-WPLOC(K,IC)*UPS(J,K1)
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
            UBC1(I)=UPS(NTST+1,I)
        ENDDO
         CALL BCNI(IAP,RAP,NDIM,PAR,ICP,NBC,UBC0,UBC1,FBC,2,DBC)
         DO I=1,NBC
            FC(I)=-FBC(I)
         ENDDO
C       Save difference :
         DO J=1,NTST+1
            DO I=1,NRA
               DUPS(J,I)=UPS(J,I)-UOLDPS(J,I)
            ENDDO
         ENDDO
       ENDIF
C     
C     Integral constraints :     
       IF(NINT.GT.0)THEN
         DO JJ=1,NA
            J=JJ+MPART
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
       RETURN
       END
C
C     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,FA,FC,P0,P1,IFST,
     +  IDB,NLLV,DET,NOV,NA,NBC,NRA,NCA,
     +  NCB,NRC,IAM,KWT,PAR,A1,A2,BB,CC,FAA,CA1,
     +  S1,S2,ICF11,IPR,ICF1,ICF2,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Local Paramters
      PARAMETER (NTST = (NTSTX/NODES)+1)
      PARAMETER (NCR  = NDIMX+NPARX)
      PARAMETER (NCRE = NCR+NDIMX+2*NDIMX**2+1)
      PARAMETER (NBUF = 3*NDIMX+NPARX+10)
      PARAMETER (NE  = (2*NDIMX+NPARX)**2)
C
C Arguments
      INTEGER   IDB,NLLV,NOV,NA,NBC,NRA
      INTEGER   NCA,NCB,NRC,IAM,KWT
      DOUBLE PRECISION DET
      LOGICAL PAR
      DIMENSION A(*),B(*),C(*),D(*),FA(*),FC(*),P0(*),P1(*)
      DIMENSION A1(*),A2(*),BB(*),CC(*),FAA(*),CA1(*)
      DIMENSION S1(*),S2(*)
      INTEGER   ICF11(*),IPR(*)
      INTEGER   ICF1(*),ICF2(*),IRF(*),ICF(*)      
C
C Local 
      DIMENSION SOL1(NDIMX*NTST),SOL2(NDIMX*NTST)
      DIMENSION SOL3(NDIMX*NTST)
      DIMENSION FCC(NCRE),BUF(NBUF),E(NE)
      INTEGER   IR(NCRE),IC(NCRE)
C
      IF(IDB.GT.4.and.IAM.EQ.0)
     +     CALL PRINT1(NOV,NA,NRA,NCA,NCB,NRC,A,B,C,D,FA,FC)

      IF(IFST.EQ.1)THEN
         CALL CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NBC,NRC,C,D,IRF,ICF)
         CALL COPYCP(IAM,KWT,NA,NOV,NRA,NCA,A,NCB,B,
     +       NRC,C,A1,A2,BB,CC,IRF)
      ENDIF
C
      IF(NLLV.EQ.0)THEN
         CALL CONRHS(NOV,NA,NRA,NCA,A,NBC,NRC,C,FA,FC,IRF,ICF,IAM)
         CALL CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
      ELSE
         CALL SETZERO(FA,FC,NA,NRA,NRC)
         CALL CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
      ENDIF
C
      IF(IFST.EQ.1)
     +     CALL REDUCE(IAM,KWT,PAR,A1,A2,BB,CC,D,
     +     NA,NOV,NCB,NRC,S1,S2,CA1,ICF1,ICF2,ICF11,IPR,BUF,NBC)
C
      IF(NLLV.EQ.0)
     +     CALL REDRHS(IAM,KWT,PAR,A1,A2,CC,
     +     FAA,FC,NA,NOV,NCB,NRC,CA1,ICF1,ICF2,ICF11,IPR,NBC,BUF)
C
      CALL DIMRGE(IAM,KWT,PAR,E,CC,D,FC,BUF,IR,IC,IFST,
     +     NA,NRC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
C
      CALL BCKSUB(IAM,KWT,PAR,S1,S2,A2,BB,FAA,FC,FCC,
     +     SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2,BUF)
C
      CALL INFPAR(IAM,PAR,A,B,FA,SOL1,SOL2,FC,
     *     NA,NOV,NRA,NCA,NCB,IRF,ICF)
C
      RETURN
      END
C
C     ---------- -------
      SUBROUTINE SETZERO(FA,FC,NA,NRA,NRC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NRA,NRC
      DIMENSION FA(NRA,*),FC(*)
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
C     ---------- ------
      SUBROUTINE CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NBC,NRC,C,D,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NCB,NBC,NRC,ICF(NCA,*),IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION D(NCB,*)
C
C Note that the summation of the adjacent overlapped part of C
C is delayed until REDUCE, in order to merge it with other communications.
C NA is the local NTST.
C
      NEX=NCA-2*NOV
      IF(NEX.EQ.0)RETURN
C
C Condensation of parameters (Elimination of local variables).
C
      NBCP1 = NBC+1
      M1    = NOV+1
      M2    = NOV+NEX
C     
C     Initialization
      DO I=1,NA
         DO J=1,NRA
            IRF(J,I)=J
         ENDDO
         DO J=1,NCA
            ICF(J,I)=J
         ENDDO
      ENDDO
C
      ZERO = 0.D0
C
      DO 2 I=1,NA
         DO 1 IC=M1,M2
            IR1=IC-NOV+1
            IRP=IR1-1
            ICP1=IC+1
C           **Search for pivot (Complete pivoting)
            PIV = ZERO
            IPIV = IRP
            JPIV = IC
            DO K1=IRP,NRA
               DO K2=IC,M2
                  TPIV = A(ICF(K2,I),IRF(K1,I),I)
                  IF(TPIV.LT.ZERO)TPIV=-TPIV
                  IF(PIV.LT.TPIV)THEN
                     PIV = TPIV
                     IPIV = K1
                     JPIV = K2
                  ENDIF
               ENDDO
            ENDDO
C           **Move indices
            ITMP        = ICF(IC,I)
            ICF(IC,I)   = ICF(JPIV,I)
            ICF(JPIV,I) = ITMP
            ITMP        = IRF(IRP,I)
            IRF(IRP,I)  = IRF(IPIV,I)
            IRF(IPIV,I) = ITMP
C           **End of pivoting; elimination starts here
            DO IR=IR1,NRA
               RM=A(ICF(IC,I),IRF(IR,I),I)/
     +              A(ICF(IC,I),IRF(IRP,I),I)
               A(ICF(IC,I),IRF(IR,I),I)=RM
	       IF(RM.NE.0.0)THEN
               DO L=1,NOV
                  A(L,IRF(IR,I),I)=A(L,IRF(IR,I),I)-RM*
     +                 A(L,IRF(IRP,I),I)
               ENDDO
               DO L=ICP1,NCA
                  A(ICF(L,I),IRF(IR,I),I)=
     +                 A(ICF(L,I),IRF(IR,I),I)-RM*
     +                 A(ICF(L,I),IRF(IRP,I),I)
               ENDDO
               DO L=1,NCB
                  B(L,IRF(IR,I),I)=B(L,IRF(IR,I),I)-RM*
     +                 B(L,IRF(IRP,I),I)
               ENDDO
	       ENDIF
            ENDDO
            DO IR=NBC+1,NRC
               RM=C(ICF(IC,I),IR,I)/A(ICF(IC,I),IRF(IRP,I),I)
               C(ICF(IC,I),IR,I)=RM
	       IF(RM.NE.0.0)THEN
               DO L=1,NOV
                  C(L,IR,I)=C(L,IR,I)-RM*A(L,IRF(IRP,I),I)
               ENDDO
               DO L=ICP1,NCA
                  C(ICF(L,I),IR,I)=C(ICF(L,I),IR,I)-RM*
     +                 A(ICF(L,I),IRF(IRP,I),I)
                  ENDDO
               DO L=1,NCB
                  D(L,IR)=D(L,IR)-RM*B(L,IRF(IRP,I),I)
               ENDDO
	       ENDIF
            ENDDO
 1       CONTINUE
 2    CONTINUE
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE CONRHS(NOV,NA,NRA,NCA,A,
     +  NBC,NRC,C,FA,FC,IRF,ICF,IAM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA,IAM
      INTEGER   NBC,NRC,ICF(NCA,*),IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),C(NCA,NRC,*)
      DIMENSION FA(NRA,*),FC(*)
C
      NEX=NCA-2*NOV
      IF(NEX.EQ.0)RETURN
C
C Condensation of right hand side.
C
      NBCP1 = NBC+1
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
            DO IR=NBCP1,NRC
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
      SUBROUTINE COPYCP(IAM,KWT,NA,NOV,NRA,NCA,A,
     +  NCB,B,NRC,C,A1,A2,BB,CC,IRF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   IAM,KWT,NA,NOV,NRA,NCA
      INTEGER   NCB,NRC,IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION BB(NOV,NCB,*),CC(NOV,NRC,*)
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
               A1(IR,IC,I)=A(IC,IRFIR,I)
               A2(IR,IC,I)=A(IC1,IRFIR,I)
            ENDDO     
            DO IC=1,NCB
               BB(IR,IC,I)=B(IC,IRFIR,I)
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
C     ---------- ------
      SUBROUTINE REDUCE(IAM,KWT,PAR,A1,A2,BB,CC,DD,
     +  NA,NOV,NCB,NRC,S1,S2,CA1,ICF1,ICF2,
     +  ICF11,IPR,BUF,NBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   IAM,KWT,NA,NOV,NCB,NRC,NBC
      INTEGER   ICF1(NOV,*),ICF2(NOV,*),ICF11(NOV,*)
      LOGICAL   PAR
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION BB(NOV,NCB,*),CC(NOV,NRC,*),BUF(*)
      DIMENSION DD(NCB,*)
      DIMENSION CA1(NOV,NOV,*),IPR(NOV,*)
C
C Local 
      INTEGER ISM(KREDO),IRM(KREDO)
      INTEGER ISMM(KREDO),IRMM(KREDO)
      INTEGER ISMC(KREDO),IRMC(KREDO)
      INTEGER MYLEFT(KREDO),MYLEFTC(KREDO),MYRIGHT(KREDO)
      LOGICAL ODDC(KREDO),EVENC(KREDO),WORKER(KREDO)
      LOGICAL NOTSEND,MASTER(KREDO)
      DOUBLE PRECISION RM
      REAL*4 XKWT
C
      ZERO    = 0.0D0
      NBCP1   = NBC+1
      NAP1    = NA+1
      NAM1    = NA-1
      NRCMNBC = NRC-NBC
      LEN1    = 8*(NOV*(NRC-NBC))
      LEN2    = 8*(NOV+NRC-NBC+1)
      XKWT    = KWT
      NLEV    = NINT(LOG10(XKWT)/LOG10(2.0))
      NOTSEND = .TRUE.
C
C     FOR EACH REURSIVE LEVEL, CALCULATE THE MASTER(HOLDING THE
C     PIVOT ROW AFTER ROW SWAPPING) NODE WHICH WILL SEND THE 
C     PIVOT ROW TO THE CORRESPONDING WORKER NODE WHICH IS DISTANCED
C     2**(K-1) FROM THE MASTER WHERE K IS THE RECURSIVE LEVEL NUMBER. 
C     THE CORRESPONDING MESSAGE TYPE IN EACH RECURSIVE LEVEL IS
C     ALSO CALCULATED HERE.
C
C For each level in the recursion, determine the master node
C (holding the pivot row after row swapping), which will send the
C pivot row to the corresponding worker node at distance 2**(K-1)
C from the master. Here K is the level in the recursion.
C The message type at each level in the recursion is also determined.
C
      IF(PAR)THEN
C
         DO 1 I=1,NLEV
C
            ODDC(I)   = .FALSE.
            EVENC(I)  = .FALSE.
            MASTER(I) = .FALSE.  
            WORKER(I) = .FALSE.
            K1        = 2**(I-1)
            K2        = 2*K1
            NIAM      = IAM/K1
C
            IF(NOTSEND)THEN
C
               IF(MOD(NIAM,2).EQ.0)THEN
C
                  MASTER(I)  = .TRUE.
                  NOTSEND    = .FALSE.
                  ISM(I)     = I+IAM
                  IRM(I)     = ISM(I)+K1
                  MYRIGHT(I) = IAM+K1
                  IRMM(I)    = I+IAM+1+2*KWT
                  ISMC(I)    = I+IAM+KWT
                  MYLEFTC(I) = IAM-(K1-1)
C
               ELSE
C
                  WORKER(I)  = .TRUE.
                  ISM(I)     = I+IAM
                  IRM(I)     = ISM(I)-K1
                  MYLEFT(I)  = IAM-K1
C
               ENDIF
C
            ENDIF
C
            K=MOD(IAM,K2)
            IF(K.EQ.K1)THEN
               EVENC(I)=.TRUE.           
               ISMM(I)=I+IAM+2*KWT
            ENDIF
C
            IF(MOD(IAM,K2).EQ.0)THEN
               ODDC(I)       = .TRUE.
               IRMC(I)       = I+IAM+KWT+(K1-1)
            ENDIF
C
 1       CONTINUE
      ENDIF
C
C Initialization
C
      DO I=1,NA
         DO K1=1,NOV
            ICF1(K1,I)     = K1
            ICF2(K1,I)     = K1
            IPR(K1,I)      = K1
            DO K2=1,NOV
               S2(K1,K2,I) = 0.0D0
               S1(K1,K2,I) = 0.0D0
            ENDDO
         ENDDO
      ENDDO
C
      DO IR=1,NOV
         DO IC=1,NOV
            S1(IR,IC,1)=A1(IR,IC,1)
         ENDDO
      ENDDO
C
C The reduction process is done concurrently
      DO 3 I1=1,NAM1
C
         I2=I1+1
         I3=I2+1
C
         DO 2 IC=1,NOV
            ICP1=IC+1
C
C Complete pivoting; rows are swapped physically, columns swap indices
            PIV1 = ZERO
            IPIV1 = IC
            JPIV1 = IC
            DO K1=IC,NOV
               DO K2=IC,NOV
                  TPIV      = A2(K1,ICF2(K2,I1),I1)
                  IF(TPIV.LT.ZERO)TPIV=-TPIV
                  IF(PIV1.LT.TPIV)THEN
                     PIV1   = TPIV
                     IPIV1  = K1
                     JPIV1  = K2
                  ENDIF
               ENDDO
            ENDDO
C
            PIV2 = ZERO
            IPIV2 = 1
            JPIV2 = IC
            DO K1=1,NOV
               DO K2=IC,NOV
                  TPIV      = A1(K1,ICF1(K2,I2),I2)
                  IF(TPIV.LT.ZERO)TPIV=-TPIV
                  IF(PIV2.LT.TPIV)THEN
                     PIV2   = TPIV
                     IPIV2  = K1
                     JPIV2  = K2
                  ENDIF
               ENDDO
            ENDDO
C
            IF(PIV1.GE.PIV2)THEN
               IPR(IC,I1)        = IPIV1
               ITMP              = ICF2(IC,I1)
               ICF2(IC,I1)       = ICF2(JPIV1,I1)
               ICF2(JPIV1,I1)    = ITMP
               ITMP              = ICF1(IC,I2)
               ICF1(IC,I2)       = ICF1(JPIV1,I2)
               ICF1(JPIV1,I2)    = ITMP
C Swapping
               DO L=1,NOV
                  TMP            = S1(IC,L,I1)
                  S1(IC,L,I1)    = S1(IPIV1,L,I1)
                  S1(IPIV1,L,I1) = TMP
                  IF(L.GE.IC)THEN
                     TMP=A2(IC,ICF2(L,I1),I1)
                     A2(IC,ICF2(L,I1),I1)= 
     +                    A2(IPIV1,ICF2(L,I1),I1)
                     A2(IPIV1,ICF2(L,I1),I1)= TMP
                  ENDIF
                  TMP            = S2(IC,L,I1)
                  S2(IC,L,I1)    = S2(IPIV1,L,I1)
                  S2(IPIV1,L,I1) = TMP
               ENDDO
C
               DO L=1,NCB
                  TMP            = BB(IC,L,I1)
                  BB(IC,L,I1)    = BB(IPIV1,L,I1)
                  BB(IPIV1,L,I1) = TMP
               ENDDO
            ELSE
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
                     TMP  = A2(IC,ICF2(L,I1),I1)
                     A2(IC,ICF2(L,I1),I1)= 
     +                    A1(IPIV2,ICF2(L,I1),I2)
                     A1(IPIV2,ICF2(L,I1),I2) = TMP
                  ENDIF
                  TMP            = S2(IC,L,I1)
                  S2(IC,L,I1)    = A2(IPIV2,L,I2)
                  A2(IPIV2,L,I2) = TMP
                  TMP            = S1(IC,L,I1)
                  S1(IC,L,I1)    = S1(IPIV2,L,I2)
                  S1(IPIV2,L,I2) = TMP                  
               ENDDO
               DO L=1,NCB
                  TMP            = BB(IC,L,I1)
                  BB(IC,L,I1)    = BB(IPIV2,L,I2)
                  BB(IPIV2,L,I2) = TMP
               ENDDO
            ENDIF
C
C End of pivoting; Elimination starts here
C
            DO IR=ICP1,NOV
               RM = A2(IR,ICF2(IC,I1),I1)/
     +              A2(IC,ICF2(IC,I1),I1)
               A2(IR,ICF2(IC,I1),I1)   = RM
C
               IF(RM.NE.0.0)THEN
               DO L=ICP1,NOV
                  A2(IR,ICF2(L,I1),I1) =
     +                 A2(IR,ICF2(L,I1),I1)-RM*
     +                 A2(IC,ICF2(L,I1),I1)
               ENDDO
C
               DO L=1,NOV
                  S1(IR,L,I1) = S1(IR,L,I1)-RM*S1(IC,L,I1)
                  S2(IR,L,I1) = S2(IR,L,I1)-RM*S2(IC,L,I1)
               ENDDO
C
               DO L=1,NCB
                  BB(IR,L,I1) = BB(IR,L,I1)-RM*BB(IC,L,I1)
               ENDDO
	       ENDIF
            ENDDO
C     
            DO IR=1,NOV
               RM = A1(IR,ICF1(IC,I2),I2)/
     +              A2(IC,ICF2(IC,I1),I1)
               A1(IR,ICF1(IC,I2),I2)   = RM
C
	       IF(RM.NE.0.0)THEN
                 DO L=ICP1,NOV
                    A1(IR,ICF1(L,I2),I2) = 
     +                 A1(IR,ICF1(L,I2),I2)-RM*
     +                 A2(IC,ICF2(L,I1),I1)
                 ENDDO
                 DO L=1,NOV
                   S1(IR,L,I2) = S1(IR,L,I2)-RM*S1(IC,L,I1)
                   A2(IR,L,I2) = A2(IR,L,I2)-RM*S2(IC,L,I1)
                 ENDDO
                 DO L=1,NCB
                   BB(IR,L,I2) = BB(IR,L,I2)-RM*BB(IC,L,I1)
                 ENDDO
	       ENDIF
            ENDDO
C     
            DO IR=NBCP1,NRC
               RM = CC(ICF2(IC,I1),IR,I2)/
     +              A2(IC,ICF2(IC,I1),I1)
               CC(ICF2(IC,I1),IR,I2)   = RM                  
C
               IF(RM.NE.0.0)THEN
                 DO L=ICP1,NOV
                    CC(ICF2(L,I1),IR,I2) = 
     +                 CC(ICF2(L,I1),IR,I2)-RM*
     +                 A2(IC,ICF2(L,I1),I1)
                 ENDDO
                 DO L=1,NOV
                    CC(L,IR,1)  = CC(L,IR,1)-RM*S1(IC,L,I1)
                    CC(L,IR,I3) = CC(L,IR,I3)-RM*S2(IC,L,I1)
                 ENDDO
                 DO L=1,NCB
                    DD(L,IR)    = DD(L,IR)-RM*BB(IC,L,I1)
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
C     INTER NODES REDUCE IS DONE VIA COMMUNICATION 
C     BETWEEN MASTER NODES AND WORKER NODES.
C     THE SUMMATION OF THE OVERLAPED PART C IN THE
C     NEIGHBOR NODES IN THE CONDENSATION OF PARAMETER
C     ROUTINE IS DELAYED TO HERE TO SUM.
C
C Inter node reduction is done via communication between master node
C and worker nodes. The summation over the overlapped part C of
C neighboring nodes in the condensation of parameters is delayed until here.
      IF(PAR)THEN
C
         DO 4 I=1,NLEV
C
            IF(MASTER(I))THEN
               CALL CRECV(IRMM(I),BUF,LEN1)
               DO IR=NBCP1,NRC
                  IR1=IR-NBC
                  DO IC=1,NOV
                     L1=(IR1-1)*NOV+IC
                     CC(IC,IR,NAP1)=CC(IC,IR,NAP1)+BUF(L1)
                  ENDDO
               ENDDO   
               DO IR=1,NOV
                  DO IC=1,NOV
                     S2(IR,IC,NA)=0.0D0
                  ENDDO
               ENDDO            
            ENDIF
C
            IF(EVENC(I))THEN
               CALL CSEND(ISMM(I),CC(1,NBCP1,1),
     +                LEN1,IAM-1,0)
            ENDIF
C
            IF(WORKER(I))THEN
               DO IR=1,NOV
                  DO IC=1,NOV
                     CA1(IR,IC,I) = S1(IR,IC,NA)
                     S1(IR,IC,NA) = 0.0D0
                  ENDDO
               ENDDO
C
               DO L=1,NOV
                  ICF11(L,I)=L
               ENDDO
            ENDIF
C
            DO IC=1,NOV
C
               ICP1    = IC+1
               IPROW   = NOV-IC+1
               IPROWN  = IPROW+NOV
               IPROWN2 = IPROWN+NOV
               IB1     = IPROWN2+NCB+1
               IB2     = IB1+1
               IBUF    = 8*IB2
               IBUF1   = 8*(IB2+NRC-NBC)
C
               IF(MASTER(I))THEN
C
C PIVOTING (COMPLETE PIVOTING)
C
                  PIV1  = ZERO
                  IPIV1 = IC
                  JPIV1 = IC
                  DO K1=IC,NOV
                     DO K2=IC,NOV
                        K3        = ICF2(K2,NA)
                        TPIV      = A2(K1,K3,NA)
                        IF(TPIV.LT.ZERO)TPIV=-TPIV
                        IF(PIV1.LT.TPIV)THEN
                           PIV1   = TPIV
                           IPIV1  = K1
                           JPIV1  = K2
                        ENDIF
                     ENDDO
                  ENDDO
C
                  CALL CRECV(IRM(I),BUF,IBUF)
C
                  JPIV2 = NINT(BUF(IB1))
                  IPIV2 = NINT(BUF(IB2))
C
                  PIV2  = BUF(1)
                  IF(PIV2.LT.0.D0)PIV2=-PIV2
C
                  IF(PIV1.GE.PIV2)THEN
C
                     IPR(IC,NA)        = IPIV1
                     ITMP              = ICF2(IC,NA)
                     ICF2(IC,NA)       = ICF2(JPIV1,NA)
                     ICF2(JPIV1,NA)    = ITMP
C
C Send pivot row to worker
                     DO L=1,NOV
                        IF(L.GE.IC)THEN
                           L1      = L-IC+1
                           L2      = ICF2(L,NA)
                           BUF(L1) = A2(IPIV1,L2,NA)
                        ENDIF
                        L1      = IPROW+L
                        L2      = IPROWN+L
                        BUF(L1) = S1(IPIV1,L,NA)
                        BUF(L2) = S2(IPIV1,L,NA)
                     ENDDO
C
                     DO L=1,NCB
                        L1      = IPROWN2+L
                        BUF(L1) = BB(IPIV1,L,NA)
                     ENDDO
C
                     BUF(IB1)  = JPIV1
C
                     DO L=NBCP1,NRC
                        L1      = L-NBC
                        L2      = IB1+L1
                        L3      = ICF2(IC,NA)
                        BUF(L2) = CC(L3,L,NAP1)
                     ENDDO
C
                     L1      = IB2+NRCMNBC
                     BUF(L1) = 0.D0

                     CALL CSEND(ISM(I),BUF,IBUF1,MYRIGHT(I),0)
C
C Row swapping
                     DO L=1,NOV
                        TMP              = S1(IC,L,NA)
                        S1(IC,L,NA)      = S1(IPIV1,L,NA)
                        S1(IPIV1,L,NA)   = TMP
                        IF(L.GE.IC)THEN
                           L1             = ICF2(L,NA)
                           TMP            = A2(IC,L1,NA)
                           A2(IC,L1,NA)   = A2(IPIV1,L1,NA)
                           A2(IPIV1,L1,NA)=TMP
                        ENDIF
                        TMP              = S2(IC,L,NA)
                        S2(IC,L,NA)      = S2(IPIV1,L,NA)
                        S2(IPIV1,L,NA)   = TMP
                     ENDDO
C
                     DO L=1,NCB
                        TMP              = BB(IC,L,NA)
                        BB(IC,L,NA)      = BB(IPIV1,L,NA)
                        BB(IPIV1,L,NA)   = TMP
                     ENDDO
C
                  ELSE
C
                     IPR(IC,NA)        = NOV+IPIV2
                     JPIV1             = JPIV2
                     ITMP              = ICF2(IC,NA)
                     ICF2(IC,NA)       = ICF2(JPIV1,NA)
                     ICF2(JPIV1,NA)    = ITMP
C
                     DO L=1,NOV
                        IF(L.GE.IC)THEN
                           L1           = L-IC+1
                           L2           = ICF2(L,NA)
                           TMP          = BUF(L1)
                           BUF(L1)      = A2(IC,L2,NA)
                           A2(IC,L2,NA) = TMP
                        ENDIF
                        L1          = IPROW+L
                        L2          = IPROWN+L
                        TMP         = BUF(L1)
                        BUF(L1)     = S1(IC,L,NA)
                        S1(IC,L,NA) = TMP
                        TMP         = BUF(L2)
                        BUF(L2)     = S2(IC,L,NA)
                        S2(IC,L,NA) = TMP
                     ENDDO
C
                     DO L=1,NCB
                        L1          = IPROWN2+L
                        TMP         = BUF(L1)
                        BUF(L1)     = BB(IC,L,NA) 
                        BB(IC,L,NA) = TMP
                     ENDDO
C
                     BUF(IB1)  = JPIV2
C
                     DO L=NBCP1,NRC
                        L1      = L-NBC
                        L2      = ICF2(IC,NA)
                        L3      = IB1+L1
                        BUF(L3) = CC(L2,L,NAP1)
                     ENDDO
                     L1      = IB2+NRCMNBC
                     BUF(L1) = 1.D0
C
                     CALL CSEND(ISM(I),BUF,IBUF1,MYRIGHT(I),0)
C
C
                  ENDIF
C End pivoting in master
C
C Send data to worker nodes
                  DO L=1,NOV
                     BUF(L)          = S1(IC,L,NA)
                  ENDDO
C
                  DO L=NBCP1,NRC
                     L1              = L-NBC
                     L2              = ICF2(IC,NA)
                     L3              = NOV+L1
                     BUF(L3)         = CC(L2,L,NAP1)
                  ENDDO         
C
                  L2      = ICF2(IC,NA)
                  L1      = NOV+NRCMNBC+1
                  BUF(L1) = A2(IC,L2,NA)
C
                  CALL CSEND(ISMC(I),BUF,LEN2,MYLEFTC(I),0)
C     
C Elimination
                  DO IR=ICP1,NOV
                     L2 = ICF2(IC,NA)
                     RM = A2(IR,L2,NA)/A2(IC,L2,NA)
                     A2(IR,L2,NA) = RM
                     IF(RM.NE.ZERO)THEN
                        DO L=ICP1,NOV
                           L1 = ICF2(L,NA)
                           A2(IR,L1,NA) = A2(IR,L1,NA)-RM*
     +                          A2(IC,L1,NA)
                        ENDDO
                        DO L=1,NOV
                           S1(IR,L,NA) = S1(IR,L,NA)-RM*
     +                          S1(IC,L,NA)
                           S2(IR,L,NA) = S2(IR,L,NA)-RM*
     +                          S2(IC,L,NA)
                        ENDDO
                        DO L=1,NCB
                           BB(IR,L,NA) = BB(IR,L,NA)-RM*
     +                          BB(IC,L,NA)
                        ENDDO
                     ENDIF
                  ENDDO
C
                  DO IR=NBCP1,NRC
                     L2 = ICF2(IC,NA)
                     RM = CC(L2,IR,NAP1)/A2(IC,L2,NA)
                     CC(L2,IR,NAP1) = RM
                     IF(RM.NE.ZERO)THEN
                        DO L=ICP1,NOV
                           L1 = ICF2(L,NA)
                           CC(L1,IR,NAP1) = 
     +                          CC(L1,IR,NAP1)-RM*
     +                          A2(IC,L1,NA)
                        ENDDO
                        DO L=1,NCB
                           DD(L,IR) = DD(L,IR)-RM*BB(IC,L,NA)
                        ENDDO
                     ENDIF
                  ENDDO
C     
               ENDIF
C
               IF(WORKER(I))THEN
C
C Pivoting
                  PIV2  = ZERO
                  IPIV2 = 1
                  JPIV2 = IC
                  DO K1=1,NOV
                     DO K2=IC,NOV
                        K3  = ICF11(K2,I)
                        TPIV= CA1(K1,K3,I)
                        IF(TPIV.LT.ZERO)TPIV=-TPIV
                        IF(PIV2.LT.TPIV)THEN
                           PIV2   = TPIV
                           IPIV2  = K1
                           JPIV2  = K2
                        ENDIF
                     ENDDO
                  ENDDO  
C
                  ITMP             = ICF11(IC,I)
                  ICF11(IC,I)      = ICF11(JPIV2,I)
                  ICF11(JPIV2,I)   = ITMP 
C
                  DO L=1,NOV
                     IF(L.GE.IC)THEN
                        L1      = L-IC+1
                        L2      = ICF11(L,I)
                        BUF(L1) = CA1(IPIV2,L2,NA)
                     ENDIF
                     L1         = IPROW+L
                     L2         = L1+NOV
                     BUF(L1)    = S1(IPIV2,L,I)
                     BUF(L2)    = A2(IPIV2,L,NA)
                  ENDDO
C     
                  DO L=1,NCB
                     L1      = IPROWN2+L
                     BUF(L1) = BB(IPIV2,L,NA)
                  ENDDO
C     
                  BUF(IB1) = JPIV2
                  BUF(IB2) = IPIV2
C
                  CALL CSEND(ISM(I),BUF,IBUF,MYLEFT(I),0)
                  CALL CRECV(IRM(I),BUF,IBUF1)
C
                  L1   = IB2+NRCMNBC
                  INFO = NINT(BUF(L1))                    
C
                  IF(INFO.EQ.1)THEN
C Send pivot row to master
                     DO L=1,NOV
                        IF(L.GE.IC)THEN
                           L1              = L-IC+1
                           L2              = ICF11(L,I)
                           TMP             = CA1(IPIV2,L2,I) 
                           CA1(IPIV2,L2,I) = BUF(L1)
                           BUF(L1)         = TMP
                        ENDIF
                        L1             = IPROW+L
                        L2             = L1+NOV
                        TMP            = S1(IPIV2,L,NA)
                        S1(IPIV2,L,NA) = BUF(L1)
                        BUF(L1)        = TMP
                        TMP            = A2(IPIV2,L,NA)
                        A2(IPIV2,L,NA) = BUF(L2)
                        BUF(L2)        = TMP
                     ENDDO
                     DO L=1,NCB
                        L1             = IPROWN2+L
                        TMP            = BB(IPIV2,L,NA)
                        BB(IPIV2,L,NA) = BUF(L1)
                        BUF(L1)        = TMP
                     ENDDO
                  ELSE
C
                     ITMP             = ICF11(IC,I)
                     ICF11(IC,I)      = ICF11(JPIV2,I)
                     ICF11(JPIV2,I)   = ITMP 
C
                     JPIV2             = NINT(BUF(IB1))
                     ITMP              = ICF11(IC,I)
                     ICF11(IC,I)       = ICF11(JPIV2,I)
                     ICF11(JPIV2,I)    = ITMP  
                  ENDIF
C
C Elimination     
                  DO IR=1,NOV
                     L2 = ICF11(IC,I)
                     RM = CA1(IR,L2,I)/BUF(1)
                     CA1(IR,L2,I)   = RM
C
                     IF(RM.NE.ZERO)THEN
                        DO L=ICP1,NOV
                           L1=L-ICP1+2
                           L3=ICF11(L,I)
                           CA1(IR,L3,I) = 
     +                          CA1(IR,L3,I)-RM*BUF(L1)
                        ENDDO
                        DO L=1,NOV
                           L1 = IPROW+L
                           L2 = L1+NOV
                           S1(IR,L,NA) = S1(IR,L,NA)-RM*
     +                          BUF(L1)
                           A2(IR,L,NA) = A2(IR,L,NA)-RM*
     +                          BUF(L2)
                        ENDDO
                        DO L=1,NCB
                           L1 = IPROWN2+L
                           BB(IR,L,NA) = BB(IR,L,NA)-RM*
     +                          BUF(L1)
                        ENDDO
                     ENDIF
                  ENDDO
C     
                  DO IR=NBCP1,NRC
                     L1=IR-NBC
                     L2=IB1+L1
                     RM = BUF(L2)/BUF(1)
                     IF(RM.NE.ZERO)THEN
                        DO L=1,NOV
                           L3 = IPROWN+L
                           CC(L,IR,NAP1) = CC(L,IR,NAP1)-RM*
     +                          BUF(L3)
                        ENDDO
                     ENDIF
C     
                  ENDDO
C     
               ENDIF
C     
               IF(ODDC(I))THEN
                  CALL CRECV(IRMC(I),BUF,LEN2)
                  DO IR=NBCP1,NRC
                     IR1 = IR-NBC
                     L1  = NOV+1+NRCMNBC
                     L2  = NOV+IR1
                     RM  = BUF(L2)/BUF(L1)
                     IF(RM.NE.ZERO)THEN
                        DO L=1,NOV
                           CC(L,IR,1) = CC(L,IR,1)-RM*BUF(L)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
C
            ENDDO
C
 4       CONTINUE
C     
C Global sum for D by recursive doubling
         CALL RD0(IAM,KWT,DD(1,NBCP1),(NRC-NBC)*NCB)
C
      ENDIF     
C
      RETURN
      END
C   
C     ---------- ------
      SUBROUTINE REDRHS(IAM,KWT,PAR,A1,A2,CC,
     +  FAA,FC,NA,NOV,NCB,NRC,CA1,ICF1,ICF2,
     +  ICF11,IPR,NBC,TBUF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   IAM,KWT,NA,NOV,NRC,NBC
      INTEGER   ICF1(NOV,*),ICF2(NOV,*),ICF11(NOV,*)
      LOGICAL   PAR
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION CC(NOV,NRC,*),TBUF(*)
      DIMENSION FAA(NOV,*),FC(*)
      DIMENSION CA1(NOV,NOV,*),IPR(NOV,*)
C
C Local
      INTEGER ISM(KREDO),IRM(KREDO)
      INTEGER MYLEFT(KREDO),MYRIGHT(KREDO)
      LOGICAL NOTSEND,WORKER(KREDO),MASTER(KREDO)
      DOUBLE PRECISION RM
      DIMENSION BUF(2)
      REAL*4 XKWT    
C
      NBCP1   = NBC+1
      NAP1    = NA+1
      NAM1    = NA-1
      XKWT    = KWT
      NLEV    = NINT(LOG10(XKWT)/LOG10(2.0))
      NOTSEND = .TRUE.
C
C At each recursive level determine the master node (holding the pivot
C row after swapping), which will send the pivot row to the worker node
C at distance 2**(K-1) from the master. Here K is the recursion level.
C
      IF(PAR)THEN
         DO I=1,NLEV
            MASTER(I) = .FALSE.  
            WORKER(I) = .FALSE.
            K1        = 2**(I-1)
            NIAM      = IAM/K1
            IF(NOTSEND)THEN
               IF(MOD(NIAM,2).EQ.0)THEN
                  MASTER(I)  = .TRUE.
                  NOTSEND    = .FALSE.
                  ISM(I)     = I+IAM+10000
                  IRM(I)     = ISM(I)+K1
                  MYRIGHT(I) = IAM+K1
               ELSE
                  WORKER(I)  = .TRUE.
                  ISM(I)     = I+IAM+10000
                  IRM(I)     = ISM(I)-K1
                  MYLEFT(I)  = IAM-K1
               ENDIF
            ENDIF
         ENDDO
      ENDIF
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
               RM=A2(IR,L1,I1)
               FAA(IR,I1)=FAA(IR,I1)-RM*FAA(IC,I1)
            ENDDO
            DO IR=1,NOV
               L1=ICF1(IC,I2)
               RM=A1(IR,L1,I2)
               FAA(IR,I2) = FAA(IR,I2)-RM*FAA(IC,I1)
            ENDDO
            DO IR=NBCP1,NRC
               L1=ICF2(IC,I1)
               RM=CC(L1,IR,I2)
               FC(IR)= FC(IR)-RM*FAA(IC,I1)
            ENDDO
         ENDDO
      ENDDO            
C
C Inter-node reduction needs communication between nodes
      IF(PAR)THEN
         DO I=1,NLEV
            DO IC=1,NOV
               ICP1=IC+1
               IF(MASTER(I))THEN                  
                  IPIV1 = IPR(IC,NA)
                  IF(IPIV1.LE.NOV)THEN
                     BUF(1)        = FAA(IPIV1,NA)
                     FAA(IPIV1,NA) = FAA(IC,NA)
                     FAA(IC,NA)    = BUF(1)
                     BUF(2)        = -1.D0
                     CALL CSEND(ISM(I),BUF,16,MYRIGHT(I),0)
                  ELSE
                     BUF(1)             = FAA(IC,NA)
                     BUF(2)             = IPR(IC,NA)-NOV
                     CALL CSEND(ISM(I),BUF,16,MYRIGHT(I),0)
                     CALL CRECV(IRM(I),FAA(IC,NA),8)
                  ENDIF
C
                  DO IR=ICP1,NOV
                     L1=ICF2(IC,NA)
                     RM=A2(IR,L1,NA)
                     FAA(IR,NA)= FAA(IR,NA)-RM*FAA(IC,NA)
                  ENDDO
                  DO IR=NBCP1,NRC
                     L1=ICF2(IC,NA)
                     RM=CC(L1,IR,NAP1)
                     FC(IR)= FC(IR)-RM*FAA(IC,NA)
                  ENDDO
               ENDIF
C
               IF(WORKER(I))THEN
                  CALL CRECV(IRM(I),BUF,16)
                  IPIV2 = NINT(BUF(2))
                  IF(IPIV2.LT.0)THEN
                     TMP = BUF(1)
                  ELSE
                     TMP           = FAA(IPIV2,NA)
                     FAA(IPIV2,NA) = BUF(1)
                     CALL CSEND(ISM(I),TMP,8,MYLEFT(I),0)
                  ENDIF
C
                  DO IR=1,NOV
                     L1=ICF11(IC,I)
                     RM=CA1(IR,L1,I)
                     FAA(IR,NA) = FAA(IR,NA)-RM*TMP
                  ENDDO
               ENDIF
            ENDDO
C           **Synchronization at each recursion level among all nodes         
            CALL GSYNC()            
         ENDDO
C
         L1=NRC-NBC
         CALL GDSUM(FC(NBCP1),L1,TBUF)
C
      ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE DIMRGE(IAM,KWT,PAR,E,CC,D,FC,XE,IR,IC,IFST,
     +  NA,NRC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   IAM,KWT,NA,NRC,NOV,NCB,IDB,NLLV
      INTEGER   IR(*),IC(*)
      LOGICAL   PAR
      DIMENSION E(NOV+NRC,*),CC(NOV,NRC,*),D(NCB,*)
      DIMENSION P0(NOV,*),P1(NOV,*),S(NOV,NOV,*)
      DIMENSION FAA(NOV,*),A2(NOV,NOV,*),BB(NOV,NCB,*)
      DIMENSION FC(*),XE(*),FCC(*)
C
C Local
      INTEGER  I,J,MSGLEN1,MSGLEN2
C
C Define message length in bytes
C     
      NAP1    = NA+1
      MSGLEN1 = 8*NRC*NOV
      MSGLEN2 = 8*(NOV+NRC+2*NOV**2+1)
      NCR     = NRC+NOV
C
C Send CC(1:NOV,1:NRC,1) in node 0 to node KWT-1
C
      IF(PAR)THEN
         IF(IAM.EQ.0)CALL CSEND(100,CC(1,1,1),MSGLEN1,KWT-1,0)
         IF(IAM.EQ.(KWT-1))CALL CRECV(100,CC(1,1,1),MSGLEN1)
      ENDIF
C     
C Copy
      IF(IAM.EQ.(KWT-1))THEN
            DO I=1,NOV
               DO J=1,NOV
                  NOVPJ      = NOV+J
                  E(I,J)     = S(I,J,NA)
                  P0(I,J)    = S(I,J,NA)
                  E(I,NOVPJ) = A2(I,J,NA)
                  P1(I,J)    = A2(I,J,NA)
               ENDDO
               DO J=1,NCB
                  NOVPJ2      = 2*NOV+J
                  E(I,NOVPJ2) = BB(I,J,NA)
               ENDDO
            ENDDO
C     
            DO I=1,NRC
               NOVPI=NOV+I
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
         DO I=1,NRC
            NOVPI     = NOV+I
            XE(NOVPI) = FC(I)
         ENDDO
C     
         IF(IDB.GE.3)THEN
            IF(IAM.EQ.0)THEN
               WRITE(9,101)
               WRITE(9,100)(XE(I),I=1,NCR)
            ELSE
               WRITE(10,101)
               WRITE(10,100)(XE(I),I=1,NCR)
            ENDIF
         ENDIF
C     
         IF(IDB.GE.4)THEN
            IF(IAM.EQ.0)THEN
               WRITE(9,102)
               DO I=1,NCR
                  WRITE(9,100)(E(I,J),J=1,NCR)
               ENDDO
            ELSE
               WRITE(10,102)
               DO I=1,NCR
                  WRITE(10,100)(E(I,J),J=1,NCR)
               ENDDO
            ENDIF
         ENDIF
C
C Solve for FCC
         IF(NLLV.EQ.0)THEN
            CALL GE(IAM,NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
         ELSEIF(NLLV.GT.0)THEN
            CALL NLVC(IAM,NCR,NCR,NLLV,E,FCC,IR,IC)
         ELSE
            DO I=1,NCR-1
               XE(I)=0.D0
            ENDDO
            XE(NCR)=1.D0
            CALL GE(IAM,NCR,NCR,E,1,
     +           NCR,FCC,NCR,XE,IR,IC,DET)
         ENDIF

         IF(IDB.GE.4)THEN
            IF(IAM.EQ.0)THEN
               WRITE(9,103)
               WRITE(9,100)(FCC(I),I=1,NCR)
            ELSE
               WRITE(10,103)
               WRITE(10,100)(FCC(I),I=1,NCR)
            ENDIF
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
      ENDIF   

 100  FORMAT(1X,10E11.3)
 101  FORMAT(/,1X,'Residuals of reduced system:')
 102  FORMAT(/,1X,'Reduced Jacobian matrix:')
 103  FORMAT(/,1X,'Solution vector:')
C    
C Broadcast FCC from node KWT-1. The matrices P0 and P1 are 
C buffered in the tail of FCC so all nodes receive them.
      IF(PAR)THEN
         IF(IAM.EQ.(KWT-1))THEN
            CALL CSEND(200,FCC,MSGLEN2,-1,0)
         ELSE
            CALL CRECV(200,FCC,MSGLEN2)
         ENDIF
      ENDIF
C     
      DO I=1,NRC
         FC(I)=FCC(NOV+I)
      ENDDO
C
      IF(IAM.LT.(KWT-1))THEN
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
      ENDIF
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE BCKSUB(IAM,KWT,PAR,S1,S2,A2,BB,FAA,FC,FCC,
     +           SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2,BUF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   IAM,KWT,NA,NOV,NCB,ICF2(NOV,*)
      LOGICAL   PAR
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION A2(NOV,NOV,*),BB(NOV,NCB,*)
      DIMENSION SOL1(NOV,*),SOL2(NOV,*),SOL3(NOV,*)
      DIMENSION FAA(NOV,*),FC(*),FCC(*),BUF(*)
C
C Local
      INTEGER I,K
      INTEGER MSGLEN,RMSGTYPE,SMSGTYPE,MYLEFT,MYRIGHT
      INTEGER NLIST(2)
      LOGICAL HASLEFT,HASRIGHT,EVEN,ODD,MASTER(KREDO)
      DOUBLE PRECISION SM
      LOGICAL NOTSEND
C
      XKWT    = KWT
      NLEV    = NINT(LOG10(XKWT)/LOG10(2.0))
      NOV2    = 2*NOV
      NOV3    = 3*NOV
      IBUF    = 8*(NOV3+1)
C
C The backsubstitution in the reduction process is recursive.
      NOTSEND=.TRUE.
C
C At each recursion level determine the sender nodes (called MASTER here).
      IF(PAR)THEN
         DO I=1,NLEV
            MASTER(I) = .FALSE.  
            NIAM      = IAM/2**(I-1)
            IF(NOTSEND)THEN
               IF(MOD(NIAM,2).EQ.0)THEN
                  MASTER(I)  = .TRUE.
                  NOTSEND    = .FALSE.
               ENDIF
            ENDIF
         ENDDO
      ENDIF
C
      IF(PAR)THEN
C
C Initialization for the master or sender node at the last recursion level.
         IF(MASTER(NLEV))THEN
            DO L=1,NOV
               SOL1(L,NA) = FCC(L)
               SOL3(L,NA) = FC(L)
            ENDDO
         ENDIF
C     
         DO I=NLEV,1,-1
            IF(MASTER(I))THEN
               ISM=I+NLEV+4*KWT
               IRM=ISM+1
               K=2**(I-2)
C              **Compute the ID of the receiving node
               NLIST(1)=IAM-K
               NLIST(2)=IAM+K
C              **Receive solutions from previous level
               IF(I.LT.NLEV)THEN
                  CALL CRECV(IRM,BUF,IBUF)
                  NIAM=NINT(BUF(NOV3+1))
                  IF(IAM.LT.NIAM)THEN
                     DO L=1,NOV
                        SOL1(L,NA)=BUF(L)
                        SOL3(L,NA)=BUF(NOV+L)
                     ENDDO
                  ELSE
                     DO L=1,NOV
                        SOL1(L,NA)=BUF(NOV+L)
                        SOL3(L,NA)=BUF(NOV2+L)
                     ENDDO
                  ENDIF
               ENDIF    
C              **Backsubstitute
               DO K=NOV,1,-1
                  KP1=K+1
                  SM=0.D0
                  DO L=1,NOV
                     SM=SM+S1(K,L,NA)*SOL1(L,NA)
                     SM=SM+S2(K,L,NA)*SOL3(L,NA)
                  ENDDO
                  DO L=1,NCB
                     SM=SM+BB(K,L,NA)*FC(NOV+L)
                  ENDDO
                  DO L=KP1,NOV
                     L1=ICF2(L,NA)
                     SM=SM+SOL2(L1,NA)*A2(K,L1,NA)
                  ENDDO
                  L2=ICF2(K,NA)
                  SOL2(L2,NA)=(FAA(K,NA)-SM)/
     +                 A2(K,L2,NA)
               ENDDO     
C              **Send solutions to the next level
               IF(I.GT.1)THEN
                  DO L=1,NOV
                     BUF(L)=SOL1(L,NA)
                     BUF(NOV+L)=SOL2(L,NA)
                     BUF(NOV2+L)=SOL3(L,NA)
                  ENDDO
                  BUF(NOV3+1)=IAM
                  CALL GSENDX(ISM,BUF,IBUF,NLIST,2)
               ENDIF
            ENDIF
C           **Synchronization at each recursion level
            CALL GSYNC()
         ENDDO     
C     
C Define odd and even nodes    
         IF(MOD(IAM,2).EQ.0)THEN
            EVEN=.TRUE.
         ELSE
            ODD=.TRUE.
         ENDIF
C     
C Determine whether I have a right neighbor
         IF(IAM.EQ.(KWT-1))THEN
            HASRIGHT=.FALSE.
         ELSE
            HASRIGHT=.TRUE.
         ENDIF
C     
C Determine whether I have a left neighbor
         IF(IAM.EQ.0)THEN
            HASLEFT=.FALSE.
         ELSE
            HASLEFT=.TRUE.
         ENDIF
C     
C Define send message type
         SMSGTYPE=IAM+1000
C
C Define receive message type
         RMSGTYPE=SMSGTYPE-1
C
C Define my right neighbor
         MYLEFT=IAM-1
         MYRIGHT=IAM+1
         MSGLEN=8*NOV
C     
C May only need odd sends to even
         ITEST=0
         IF(ITEST.EQ.1)THEN
            IF(ODD.AND.HASRIGHT)THEN        
               CALL CSEND(SMSGTYPE,SOL2(1,NA),MSGLEN,MYRIGHT,0)
            ENDIF
            IF(EVEN.AND.HASLEFT)THEN
               CALL CRECV(RMSGTYPE,SOL1(1,NA),MSGLEN)               
            ENDIF
         ENDIF
C     
C Even nodes send and odd nodes receive    
         IF(EVEN.AND.HASRIGHT)THEN  
            CALL CSEND(SMSGTYPE,SOL2(1,NA),MSGLEN,MYRIGHT,0)
         ENDIF
         IF(ODD.AND.HASLEFT)THEN
            CALL CRECV(RMSGTYPE,SOL1(1,NA),MSGLEN)
         ENDIF
C     
      ELSE
C
         DO L=1,NOV
            SOL1(L,NA) = FCC(L)
            SOL2(L,NA) = FC(L)
         ENDDO
C     
      ENDIF
C
      IF(IAM.EQ.(KWT-1))THEN
         DO L=1,NOV
            SOL2(L,NA)=FC(L)
         ENDDO
      ENDIF
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
               SM=SM+SOL1(L,I)*S1(K,L,I)
               SM=SM+SOL3(L,I)*S2(K,L,I)
            ENDDO
            DO L=1,NCB
               SM=SM+FC(NOV+L)*BB(K,L,I)
            ENDDO
            DO L=K+1,NOV
               L1=ICF2(L,I)
               SM=SM+SOL2(L1,I)*A2(K,L1,I)
            ENDDO
            L2=ICF2(K,I)
            SOL2(L2,I)=(FAA(K,I)-SM)/A2(K,L2,I)
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
      SUBROUTINE INFPAR(IAM,PAR,A,B,FA,SOL1,SOL2,FC,
     +  NA,NOV,NRA,NCA,NCB,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C  Arguments
      INTEGER   IAM,NA,NOV,NRA,NCA,NCB,IRF(NRA,*),ICF(NCA,*)
      LOGICAL   PAR
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),FA(NRA,*),FC(*)
      DIMENSION SOL1(NOV,*),SOL2(NOV,*)
C
C Local
      DOUBLE PRECISION SM,X(NDIMX*NCOLX)
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
C     ---------- ---
      SUBROUTINE RD0(IAM,KWT,D,NRC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
      PARAMETER (NBUF=(NDIMX+NPARX)*(NDIMX+NPARX))
C
C     RECURSIVE DOUBLING PROCEDURE TO GET
C     THE GLOBAL SUM OF VECTORS FROM 
C     EACH NODE. THE GLOBAL SUM IS ONLY AVAILABLE
C     IN THE LAST NODE
C
C Arguments
      DIMENSION D(*)
C
C Local
      DIMENSION BUF(NBUF)
      INTEGER MYRIGHT(KREDO),SMTYPE(KREDO),RMTYPE(KREDO)
      INTEGER N,NREDO,NIAM,MSGLEN
      LOGICAL EVEN(KREDO),ODD(KREDO),NOTSEND
C
C Copying
      XKWT=KWT
C
C Determine the recursion level
      NREDO = NINT(LOG(XKWT)/LOG(2.))
C
C At each recursion level determine the odd and even nodes
      NOTSEND=.TRUE.
      DO N=1,NREDO
        SMTYPE(N) = 1000+N+IAM
        RMTYPE(N) = SMTYPE(N)-2**(N-1)
        MYRIGHT(N) = IAM + 2**(N-1)
        EVEN(N) = .FALSE.
        ODD(N) = .FALSE.
        NIAM=IAM/2**(N-1)
        IF(NOTSEND)THEN
           IF(MOD(NIAM,2).EQ.0)THEN
              EVEN(N)=.TRUE.
              NOTSEND=.FALSE.
           ELSE
              ODD(N)=.TRUE.
           ENDIF
        ENDIF
      ENDDO
C
      NIAM=NRC
      MSGLEN=8*NIAM
      DO N=1,NREDO
C        **Even nodes send and odd nodes receive from left to right
         IF (EVEN(N)) CALL CSEND(SMTYPE(N),D,MSGLEN,MYRIGHT(N),0)
         IF (ODD(N)) THEN
            CALL CRECV(RMTYPE(N),BUF,MSGLEN)
C          ** Accumulate the partial sum in the current receiving node
            DO I=1,NIAM
               D(I)=D(I)+BUF(I)
            ENDDO
         ENDIF
      ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE PRINT1(NOV,NA,NRA,NCA,NCB,NRC,A,B,C,D,FA,FC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION D(NCB,*),FA(NRA,*),FC(*)
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
           WRITE(9,103)(C(IC,IR,I),IC=1,NCA)
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
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C         Dummy Routines for the Sequential Version
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FUNCTION MYNODE()
      MYNODE=0
      RETURN
      END
      FUNCTION NUMNODES()
      NUMNODES=1
      RETURN
      END
      SUBROUTINE GSYNC()
      RETURN
      END
      FUNCTION DCLOCK()
      DCLOCK=0
      RETURN
      END
      SUBROUTINE CSEND(I1,R1,I2,I3,I4)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN
      END
      SUBROUTINE CRECV(I1,R1,I2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN
      END
      SUBROUTINE GDSUM(R1,I1,R2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN
      END
      SUBROUTINE GSENDX(I1,R1,I2,I3,I4)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN
      END
      SUBROUTINE GCOL(R1,I1,R2,I3,I4)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      RETURN
      END
      SUBROUTINE LED()
      RETURN
      END
      SUBROUTINE SETIOMODE()
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------


