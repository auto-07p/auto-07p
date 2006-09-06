C     ------- ------
      PROGRAM AUTCON
C
C This program converts a data file into a labeled AUTO solution
C
      PARAMETER (NIAP=10,NPARX=36)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(NIAP),ICP(NPARX),PAR(NPARX)
      ALLOCATABLE U(:),TM(:),DTM(:),UPS(:,:),VPS(:,:)
C
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential')
       OPEN(3,FILE='fort.3',STATUS='unknown',ACCESS='sequential')
       OPEN(8,FILE='fort.8',STATUS='unknown',ACCESS='sequential')
C
        DO I=1,NPARX
          PAR(I)=0.d0
          ICP(I)=I
        ENDDO
C
        CALL INIT(IAP)
        NDIM=IAP(1)
        ALLOCATE(U(NDIM))
        NTST=IAP(5)
        NCOL=IAP(6)
C
        NOLD=0
 1      CONTINUE
          NOLD=NOLD+1
          READ(3,*,END=2)TEMP,(TEMP,I=1,NDIM)
        GOTO 1
C
 2      NOLD=NOLD-2
        NDX=NOLD+1
        IF(NTST+1.GT.NDX)NDX=NTST+1
        ALLOCATE(TM(NDX),DTM(NDX),UPS(NDX,NDIM*NCOL),VPS(NDX,NDIM*NCOL))
C
        REWIND 3
        DO J=1,NOLD+1
          READ(3,*)TM(J),(UPS(J,I),I=1,NDIM)
        ENDDO
C
        PERIOD=TM(NOLD+1)-TM(1)
        DO I=NOLD+1,1,-1
          TM(I)=(TM(I)-TM(1))/PERIOD
        ENDDO
        DO I=1,NOLD
          DTM(I)=TM(I+1)-TM(I)
        ENDDO
        CALL ADAPT(IAP,NOLD,1,NTST,NCOL,TM,DTM,NDX,UPS,VPS)
C
        ICP(1)=1
        RLDOT=1.d0
        PAR(11)=PERIOD
        CALL STPNT(NDIM,U,PAR,T)
        CALL WRTBV8(IAP,PAR,ICP,RLDOT,NDX,UPS,VPS,TM,DTM,NPARX)
C
        DEALLOCATE(U,TM,DTM,UPS,VPS)
C
      STOP
      END
C
C     ---------- ----
      SUBROUTINE INIT(IAP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Reads the file of continuation constants
C
      DIMENSION IAP(*)
C
      READ(2,*,END=1) NDIM,IPS
      READ(2,*,END=1) 
      READ(2,*,END=1) NTST,NCOL,IAD,ISP,ISW
C
      IAP(1)=NDIM
      IAP(2)=IPS
      IAP(5)=NTST
      IAP(6)=NCOL
      IAP(7)=IAD
      IAP(9)=ISP
      IAP(10)=ISW
      RETURN
C
 1    PRINT*,' Error : End of file encountered in AUTO constants-file'
      RETURN
      END
C
C     ---------- -----
      SUBROUTINE ADAPT(IAP,NOLD,NCOLD,NNEW,NCNEW,TM,DTM,NDX,UPS,VPS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Adapts the distribution of the mesh points so that the increase of the
C monotone function EQDF becomes approximately equidistributed over the
C intervals. The functions UPS and VPS are interpolated on new mesh.
C
      DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),TM(*),DTM(*)
C Local
      ALLOCATABLE TINT(:),TM2(:),UINT(:,:),ITM(:)
C
       NDIM=IAP(1)
       IPS=IAP(2)
       ISW=IAP(10)
C
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
       NRWNEW=NDIM*NCNEW
C
       ALLOCATE(TINT(NNEWP1),UINT(NNEWP1,NRWNEW))
       ALLOCATE(TM2(NNEWP1),ITM(NNEWP1))
C
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UINT(J,I)=0.d0
         ENDDO
       ENDDO
C
C For periodic boundary conditions extrapolate by periodicity.
C
       IF(IPS.EQ.2 .AND. IABS(ISW).NE.2) THEN
         IPER=1
       ELSE
         IPER=0
       ENDIF
C
C Generate the new mesh :
C
       CALL NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TM,DTM,NNEW,TINT,IPER)
C
C Replace UPS by its interpolant on the new mesh :
C
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,UPS,NNEWP1,NCNEW,
     *  TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           UPS(J,I)=UINT(J,I)
         ENDDO
       ENDDO
C
C Replace VPS by its interpolant on the new mesh :
C
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,VPS,NNEWP1,NCNEW,
     *  TINT,UINT,TM2,ITM)
       DO J=1,NNEWP1
         DO I=1,NRWNEW
           VPS(J,I)=UINT(J,I)
         ENDDO
       ENDDO
C
C Replace old mesh :
C
       TM(1)=0.d0
       DO J=1,NNEW
         DTM(J)=TINT(J+1)-TINT(J)
         TM(J+1)=TINT(J+1)
       ENDDO
C
       DEALLOCATE(UINT,TINT,TM2,ITM)
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INTERP(NDIM,N,NC,TM,NDX,UPS,N1,NC1,TM1,UPS1,TM2,ITM1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.
C
      DIMENSION TM(*),TM1(*),TM2(*),ITM1(*),UPS(NDX,*),UPS1(N1,*)
C Local
      DIMENSION W(NC+1),X(NC+1)
C
       NCP1=NC+1
       N1M1=N1-1
C
       DO I=1,NC1
         RI=I-1
         D=RI/NC1
         DO J1=1,N1M1
           TM2(J1)=TM1(J1)+D*( TM1(J1+1)-TM1(J1) )
         ENDDO
         CALL ORDR(N,TM,N1M1,TM2,ITM1)
         DO J1=1,N1M1
           J=ITM1(J1)
           Z=TM2(J1)
           D=( TM(J+1)-TM(J) )/NC
           DO L=1,NCP1
             X(L)=TM(J)+(L-1)*D
           ENDDO
           CALL INTWTS(NCP1,Z,X,W)
           DO K=1,NDIM
             K1=(I-1)*NDIM+K
             UPS1(J1,K1)=W(NCP1)*UPS(J+1,K)
             DO L=1,NC
               L1=K+(L-1)*NDIM
               UPS1(J1,K1)=UPS1(J1,K1)+W(L)*UPS(J,L1)
             ENDDO
           ENDDO
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         UPS1(N1,I)=UPS(N,I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TMOLD,DTMOLD,
     * NNEW,TMNEW,IPER)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Redistributes the mesh according to the function EQDF.
C
      DIMENSION TMOLD(*),DTMOLD(*),TMNEW(*)
C Local
      ALLOCATABLE EQF(:),UNEQ(:),IAL(:)
      ALLOCATE(IAL(NNEW+1),UNEQ(NNEW+1),EQF(NOLD+1))
C
C Put the values of the monotonely increasing function EQDF in EQF.
C
       CALL EQDF(NOLD,NDIM,NCOLD,DTMOLD,NDX,UPS,EQF,IPER)
C
C Uniformly divide the range of EQDF :
C
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
       DAL=EQF(NOLDP1)/NNEW
       DO J=1,NNEWP1
         UNEQ(J)=(J-1)*DAL
       ENDDO
C
       CALL ORDR(NOLDP1,EQF,NNEWP1,UNEQ,IAL)
C
C Generate the new mesh in TMNEW :
C
       DO J1=1,NNEW
         J=IAL(J1)
         X=(UNEQ(J1)-EQF(J))/(EQF(J+1)-EQF(J))
         TMNEW(J1)=(1.d0-X)*TMOLD(J)+X*TMOLD(J+1)
       ENDDO
C
C Assign TMNEW(NNEWP1) explicitly because of loss of precision
C problems when EQF(NOLDP1) and EQF(NOLD) are very close
C
       TMNEW(NNEWP1)=TMOLD(NOLDP1)
C
       DEALLOCATE(IAL,UNEQ,EQF)
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE ORDR(N,TM,N1,TM1,ITM1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C TM and TM1 are two ascending arrays with values in [0,1]. On exit the
C value of ITM1( i ) specifies the index of the TM-interval in which
C TM1(i) lies.
C
      DIMENSION TM(N),TM1(N1),ITM1(N1)
C
       K0=2
       DO J1=1,N1
         K1=K0
         DO J=K0,N
           K1=J
           IF(TM1(J1).LT.TM(J))GOTO 1
         ENDDO
 1       ITM1(J1)=K1-1
         K0=K1
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE INTWTS(N,Z,X,WTS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates weights for Lagrange interpolation.
C
      DIMENSION X(*),WTS(*)
C
       DO IB=1,N
         P=1.d0
         DENOM=1.d0
         DO K=1,N
           IF(K.NE.IB)THEN
             P=P*( Z-X(K) )
             DENOM=DENOM*( X(IB)-X(K) )
            ENDIF
         ENDDO
         WTS(IB)=P/DENOM
       ENDDO
C
      RETURN
      END
C
C     ---------- ----
      SUBROUTINE EQDF(NTST,NDIM,NCOL,DTM,NDX,UPS,EQF,IPER)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)
C
      DIMENSION UPS(NDX,*),EQF(*),DTM(*)
      LOGICAL SMALL
C Local
      DIMENSION WH(NCOL+1)
      ALLOCATABLE HD(:,:)
      ALLOCATE(HD(NTST+1,NCOL*NDIM))
C
C Compute approximation to NCOL-th derivative :
       CALL CNTDIF(NCOL,WH)
C
       SMALL=.TRUE.
       DO J=1,NTST
         JP1=J+1
         SC=1.d0/DTM(J)**NCOL
         DO I=1,NDIM
           HD(J,I)=WH(NCOL+1)*UPS(JP1,I)
           DO K=1,NCOL
             K1=I+(K-1)*NDIM
             HD(J,I)=HD(J,I)+WH(K)*UPS(J,K1)
           ENDDO
           HD(J,I)=SC*HD(J,I)
           IF(DABS(HD(J,I)).GT.HMACH)SMALL=.FALSE.
         ENDDO
       ENDDO
C
C Take care of "small derivative" case.
C
       IF(SMALL)THEN
         DO I=1,NTST+1
           EQF(I)=I-1
         ENDDO
         RETURN
       ENDIF
C
       IF(IPER.NE.1)GOTO 1
C
C Extend by periodicity :
C
       DO I=1,NDIM
         HD(NTST+1,I)=HD(1,I)
       ENDDO
       DTM(NTST+1)=DTM(1)
       GOTO 2
C
C Extend by extrapolation :
C
 1     DO I=1,NDIM
         HD(NTST+1,I)=2*HD(NTST,I)-HD(NTST-1,I)
       ENDDO
       DTM(NTST+1)=DTM(NTST)
C
C Compute approximation to (NCOL+1)-st derivative :
C
 2     DO J=1,NTST
         JP1=J+1
         DTAV=.5d0*(DTM(J)+DTM(J+1))
         SC=1.d0/DTAV
         DO I=1,NDIM
           HD(J,I)=SC*( HD(JP1,I)-HD(J,I) )
         ENDDO
       ENDDO
C
C Define the equidistribution function :
C
       PWR=1.d0/(NCOL+1.d0)
       EQF(1)=0.d0
       DO J=1,NTST
         E=0.d0
         DO I=1,NDIM
           E=E+DABS( HD(J,I) )**PWR
         ENDDO
         EQF(J+1)=EQF(J)+DTM(J)*E
       ENDDO
C
       DEALLOCATE(HD)
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE CNTDIF(N,D)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Generates the coefficients of the central difference formula for
C Nth derivative at uniformly spaced points
C
      DIMENSION D(*)
C
       D(1)=1.d0
       IF(N.EQ.0)RETURN
C
       DO I=1,N
         D(I+1)=0.d0
         DO K=1,I
           K1=I+2-K
           D(K1)=D(K1-1)-D(K1)
         ENDDO
         D(1)=-D(1)
       ENDDO
C
C Scale to [0,1]  :
C
       SC=N**N
       NP1=N+1
       DO I=1,NP1
         D(I)=SC*D(I)
       ENDDO
C
      RETURN
      END
C
C     ---------- ------
      SUBROUTINE WRTBV8(IAP,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,TM,DTM,NPARX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION IAP(*),UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLDOT(*)
C
       NDIM=IAP(1)
       NTST=IAP(5)
       NCOL=IAP(6)
       ISW=IAP(10)
       ITP=9
       NFPR=1
       IBR=1
       NTOT=1
       LAB=1
C
C Write information identifying the solution :
C
       NTPL=NCOL*NTST+1
       NAR=NDIM+1
       NRD=2+NDIM/7+(NDIM-1)/7
       NROWPR=NRD*(NCOL*NTST+1) + (NFPR-1)/7+1 + (NPARX-1)/7+1
     *                          + (NFPR-1)/20+1
C
       MTOT=MOD(NTOT,10000)
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,
     *             NTST,NCOL,NPARX
C
C Write the entire solution on unit 8 :
C
       DO J=1,NTST
         RN=1.d0/NCOL
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           T=TM(J)+(I-1)*RN*DTM(J)
           WRITE(8,102)T,(UPS(J,K),K=K1,K2)
         ENDDO
       ENDDO
       WRITE(8,102)TM(NTST+1),(UPS(NTST+1,I),I=1,NDIM)
C
C Write the free parameter indices:
C
       WRITE(8,103)(ICP(I),I=1,NFPR)
C
C Write the direction of the branch:
C
       WRITE(8,102)(RLDOT(I),I=1,NFPR)
       DO J=1,NTST
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           WRITE(8,102)(UDOTPS(J,K),K=K1,K2)
         ENDDO
       ENDDO
       WRITE(8,102)(UDOTPS(NTST+1,K),K=1,NDIM)
C
C Write the parameter values.
C
       WRITE(8,102)(PAR(I),I=1,NPARX)
C
 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,1P7E19.10)
 103   FORMAT(20I5)
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
C Dummy routine (in case GETP is called in the user routine PVLS)
      GETP=0.0d0
      RETURN
      END
