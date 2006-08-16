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
      PARAMETER (NAX=NTSTX+1)
C
C Sets up and solves the linear equations for one Newton/Chord iteration
C
      EXTERNAL FUNI,BCNI,ICNI
      DIMENSION IAP(*),RAP(*)
C
C Local
      DIMENSION FT(M2AA*NAX)
C
C Most of the required memory is allocated below
      COMMON /BLAUT/ A(M1AA*M2AA*NAX),B(M1BB*M2BB*NAX),
     *       C(M1CC*M2CC*NAX),D(M1DD*M2DD), 
     *       A1((NDIMX**2)*NAX),A2((NDIMX**2)*NAX),
     *       S1((NDIMX**2)*NAX),S2((NDIMX**2)*NAX),
     *       BB(NDIMX*NPARX*NAX),
     *       CC(NRCX*NDIMX*(NAX+1)),
     *       FAA(NDIMX*NAX),
     *       CA1(NDIMX**2),
     *       ICF(NCLMX*NAX),IRF(NROWX*NAX),
     *       IPR(NDIMX*NAX),
     *       ICF11(NDIMX),
     *       ICF1(NDIMX*NAX),ICF2(NDIMX*NAX)
       COMMON /BLLOC/ DFU(NX2),DFP(NXP),UU1(NX),UU2(NX),FF1(NX),FF2(NX)
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
      IF(IFST.EQ.1)THEN
        CALL SETUBV(NDIM,IPS,NTST,NCOL,NBC,NINT,
     +   NFPR,NRC,NROW,NCLM,FUNI,BCNI,ICNI,NDX,
     +   IAP,RAP,PAR,ICP,RDS,A,B,C,D,FT,FC,RLCUR,RLOLD,
     +   RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
      ELSE
        CALL SETRHS(NDIM,IPS,NTST,NCOL,NBC,NINT,
     +   NFPR,NRC,NROW,NCLM,FUNI,BCNI,ICNI,NDX,
     +   IAP,RAP,PAR,ICP,RDS,FT,FC,RLCUR,RLOLD,
     +   RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
      ENDIF
C
      CALL BRBD(A,B,C,D,FT,FC,P0,P1,IFST,
     +  IID,NLLV,DET,NDIM,NTST,NBC,NROW,NCLM,
     +  NFPR,NRC,A1,A2,BB,CC,FAA,CA1,
     +  S1,S2,ICF11,IPR,ICF1,ICF2,IRF,ICF)
C
      CALL FAFT(FT,FA,NTST,NROW,NDX)         
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
      SUBROUTINE SETUBV(NDIM,IPS,NA,NCOL,NBC,NINT,
     + NCB,NRC,NRA,NCA,FUNI,BCNI,ICNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,AA,BB,CC,DD,FA,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
      PARAMETER (M2BC=2*NDIMX+NPARX,M2INT=NDIMX+NPARX)
      PARAMETER (MCL1=NCOLX,MCL2=MCL1+1)
C
      EXTERNAL FUNI, BCNI, ICNI
C
      DIMENSION AA(NCA,NRA,*),BB(NCB,NRA,*)
      DIMENSION CC(NCA,NRC,*),DD(NCB,*)
C
      DIMENSION IAP(*),RAP(*),UPS(NDX,*),DUPS(NDX,*)
      DIMENSION UOLDPS(NDX,*),UDOTPS(NDX,*),UPOLDP(NDX,*)
      DIMENSION FA(NRA,*),FC(*),DTM(*),PAR(*)
      DIMENSION ICP(*),RLCUR(*),RLOLD(*),RLDOT(*),THL(*),THU(*)
C
C Local
      DIMENSION DFDU(NDIMX**2),DFDP(NDIMX*NPARX)
      DIMENSION UOLD(NDIMX),U(NDIMX),F(NDIMX),PRM(NPARX)
      DIMENSION UBC0(NDIMX),UBC1(NDIMX),FBC(NBCX),DBC(NBCX*M2BC)
      DIMENSION FICD(NINTX),DICD(NINTX*M2INT)
      DIMENSION UIC(NDIMX),UIO(NDIMX),UID(NDIMX),UIP(NDIMX)
      DIMENSION WPLOC(MCL2),WI(MCL2),WP(MCL2,MCL1),WT(MCL2,MCL1)
C
       CALL WINT(NCOL+1,WI)
       CALL GENWTS(NCOL,MCL2,WT,WP)
C
C Set constants.
       NCP1=NCOL+1
       DO I=1,NCB
         PAR(ICP(I))=RLCUR(I)
       ENDDO
C
C Generate AA , BB and FA :
C
       DO 2 J=1,NA
          JP1=J+1
          DT=DTM(J)
          DDT=1.d0/DT
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
                WPLOC(IB)=DDT*WP(IB,IC)
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
       DO I=1,NRC
         FC(I)=0.d0
         DO K=1,NCB
           DD(K,I)=0.d0
         ENDDO
       ENDDO
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
              CC(K,I,1)=DBC((K-1)*NBC+I)
              CC(NRA+K,I,NA)=DBC((NDIM+K-1)*NBC+I)
            ENDDO
            DO K=1,NCB
               DD(K,I)=DBC((2*NDIM+ICP(K)-1)*NBC+I)
            ENDDO
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
C     
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
                  DO I=1,NDIM
                     K1=(K-1)*NDIM+I
                     CC(K1,NBC+M,J)=DTM(J)*WI(K)*DICD((I-1)*NINT+M)
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
       DO J=1,NA
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
      SUBROUTINE SETRHS(NDIM,IPS,NA,NCOL,NBC,NINT,
     + NCB,NRC,NRA,NCA,FUNI,BCNI,ICNI,NDX,
     + IAP,RAP,PAR,ICP,RDS,FA,FC,RLCUR,RLOLD,
     + RLDOT,UPS,UOLDPS,UDOTPS,UPOLDP,DUPS,DTM,THL,THU,P0,P1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
      PARAMETER (M2BC=2*NDIMX+NPARX,M2INT=NDIMX+NPARX)
      PARAMETER (MCL1=NCOLX,MCL2=MCL1+1)
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
       RETURN
       END
C
C     ---------- ----
      SUBROUTINE BRBD(A,B,C,D,FA,FC,P0,P1,IFST,
     +  IDB,NLLV,DET,NOV,NA,NBC,NRA,NCA,
     +  NCB,NRC,A1,A2,BB,CC,FAA,CA1,
     +  S1,S2,ICF11,IPR,ICF1,ICF2,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Local Paramters
      PARAMETER (NTST = NTSTX+1)
      PARAMETER (NCR  = NDIMX+NPARX)
      PARAMETER (NCRE = NCR+NDIMX+2*NDIMX**2+1)
      PARAMETER (NBUF = 3*NDIMX+NPARX+10)
      PARAMETER (NE  = (2*NDIMX+NPARX)**2)
C
C Arguments
      INTEGER   IDB,NLLV,NOV,NA,NBC,NRA
      INTEGER   NCA,NCB,NRC
      DOUBLE PRECISION DET
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
      IF(IDB.GT.4)
     +     CALL PRINT1(NOV,NA,NRA,NCA,NCB,NRC,A,B,C,D,FA,FC)

      IF(IFST.EQ.1)THEN
         CALL CONPAR(NOV,NA,NRA,NCA,A,NCB,B,NBC,NRC,C,D,IRF,ICF)
         CALL COPYCP(NA,NOV,NRA,NCA,A,NCB,B,NRC,C,A1,A2,BB,CC,IRF)
      ENDIF
C
      IF(NLLV.EQ.0)THEN
         CALL CONRHS(NOV,NA,NRA,NCA,A,NBC,NRC,C,FA,FC,IRF,ICF)
      ELSE
         CALL SETZERO(FA,FC,NA,NRA,NRC)
      ENDIF
      CALL CPYRHS(NA,NOV,NRA,FAA,FA,IRF)
C
      IF(IFST.EQ.1)
     +     CALL REDUCE(A1,A2,BB,CC,D,
     +     NA,NOV,NCB,NRC,S1,S2,CA1,ICF1,ICF2,ICF11,IPR,BUF,NBC)
C
      IF(NLLV.EQ.0)
     +     CALL REDRHS(A1,A2,CC,
     +     FAA,FC,NA,NOV,NCB,NRC,CA1,ICF1,ICF2,ICF11,IPR,NBC,BUF)
C
      CALL DIMRGE(E,CC,D,FC,BUF,IR,IC,IFST,
     +     NA,NRC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S1,A2,FAA,BB)
C
      CALL BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,
     +     SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2,BUF)
C
      CALL INFPAR(A,B,FA,SOL1,SOL2,FC,
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
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA
      INTEGER   NCB,NBC,NRC,ICF(NCA,*),IRF(NRA,*)
      DIMENSION A(NCA,NRA,*),B(NCB,NRA,*),C(NCA,NRC,*)
      DIMENSION D(NCB,*)
C Local
      DIMENSION IAMAX(NDIMX*NCOLX)
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
            DO IR=NBC+1,NRC
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
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE CONRHS(NOV,NA,NRA,NCA,A,
     +  NBC,NRC,C,FA,FC,IRF,ICF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NOV,NA,NRA,NCA
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
      SUBROUTINE REDUCE(A1,A2,BB,CC,DD,
     +  NA,NOV,NCB,NRC,S1,S2,CA1,ICF1,ICF2,
     +  ICF11,IPR,BUF,NBC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NCB,NRC,NBC
      INTEGER   ICF1(NOV,*),ICF2(NOV,*),ICF11(NOV,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION BB(NOV,NCB,*),CC(NOV,NRC,*),BUF(*)
      DIMENSION DD(NCB,*)
      DIMENSION CA1(NOV,NOV,*),IPR(NOV,*)
C
C Local 
      DOUBLE PRECISION RM
C
      ZERO    = 0.0D0
      NBCP1   = NBC+1
      NAP1    = NA+1
      NAM1    = NA-1
      NRCMNBC = NRC-NBC
      LEN1    = 8*(NOV*(NRC-NBC))
      LEN2    = 8*(NOV+NRC-NBC+1)
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
      RETURN
      END
C   
C     ---------- ------
      SUBROUTINE REDRHS(A1,A2,CC,
     +  FAA,FC,NA,NOV,NCB,NRC,CA1,ICF1,ICF2,
     +  ICF11,IPR,NBC,TBUF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NRC,NBC
      INTEGER   ICF1(NOV,*),ICF2(NOV,*),ICF11(NOV,*)
      DIMENSION A1(NOV,NOV,*),A2(NOV,NOV,*)
      DIMENSION CC(NOV,NRC,*),TBUF(*)
      DIMENSION FAA(NOV,*),FC(*)
      DIMENSION CA1(NOV,NOV,*),IPR(NOV,*)
C
C Local
      DOUBLE PRECISION RM
C
      NBCP1   = NBC+1
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
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE DIMRGE(E,CC,D,FC,XE,IR,IC,IFST,
     +  NA,NRC,NOV,NCB,IDB,NLLV,FCC,P0,P1,DET,S,A2,FAA,BB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Arguments
      INTEGER   NA,NRC,NOV,NCB,IDB,NLLV
      INTEGER   IR(*),IC(*)
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
C Copy
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
         CALL GE(NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
      ELSEIF(NLLV.GT.0)THEN
         CALL NLVC(NCR,NCR,NLLV,E,FCC,IR,IC)
      ELSE
         DO I=1,NCR-1
            XE(I)=0.D0
         ENDDO
         XE(NCR)=1.D0
         CALL GE(NCR,NCR,E,1,NCR,FCC,NCR,XE,IR,IC,DET)
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
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE BCKSUB(S1,S2,A2,BB,FAA,FC,FCC,
     +           SOL1,SOL2,SOL3,NA,NOV,NCB,ICF2,BUF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'auto.h'
C
C Arguments
      INTEGER   NA,NOV,NCB,ICF2(NOV,*)
      DIMENSION S1(NOV,NOV,*),S2(NOV,NOV,*)
      DIMENSION A2(NOV,NOV,*),BB(NOV,NCB,*)
      DIMENSION SOL1(NOV,*),SOL2(NOV,*),SOL3(NOV,*)
      DIMENSION FAA(NOV,*),FC(*),FCC(*),BUF(*)
C
C Local
      INTEGER I,K
      DOUBLE PRECISION SM
C
      NOV2    = 2*NOV
      NOV3    = 3*NOV
      IBUF    = 8*(NOV3+1)

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


