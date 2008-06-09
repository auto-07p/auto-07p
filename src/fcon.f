C     ------- ------
      PROGRAM AUTCON
C
C This program converts a data file into a labeled AUTO solution
C
      PARAMETER (NPARX=36)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ICP(1),RLDOT(1)
      ALLOCATABLE U(:),TM(:),DTM(:),UPS(:,:),VPS(:,:),PAR(:)
C
       OPEN(2,FILE='fort.2',STATUS='old',ACCESS='sequential')
       OPEN(3,FILE='fort.3',STATUS='unknown',ACCESS='sequential')
       OPEN(8,FILE='fort.8',STATUS='unknown',ACCESS='sequential')
C
        CALL INIT(NDIM,IPS,NTST,NCOL,ISW,NPAR)
        ALLOCATE(U(NDIM),PAR(NPAR))
        PAR(:)=0d0
C
        NOLD=0
        DO
          NOLD=NOLD+1
          READ(3,*,END=2)TEMP,(TEMP,I=1,NDIM)
        ENDDO
C
 2      NOLD=NOLD-2
        NTSTM=MAX(NOLD+1,NTST+1)
        NDX=NDIM*NCOL
        ALLOCATE(TM(NTSTM),DTM(NTSTM))
        ALLOCATE(UPS(NDX,NTSTM),VPS(NDX,NTSTM))
C
        REWIND 3
        DO J=1,NOLD+1
          READ(3,*)TM(J),UPS(1:NDIM,J)
        ENDDO
        VPS(:,:)=0d0
C
        PERIOD=TM(NOLD+1)-TM(1)
        DO I=NOLD+1,1,-1
          TM(I)=(TM(I)-TM(1))/PERIOD
        ENDDO
        DO I=1,NOLD
          DTM(I)=TM(I+1)-TM(I)
        ENDDO
        CALL ADAPT(NDIM,IPS,ISW,NOLD,1,NTST,NCOL,TM,DTM,NDX,UPS,VPS)
C
        ICP(1)=1
        RLDOT(1)=1.d0
        PAR(11)=PERIOD
        CALL STPNT(NDIM,U,PAR,T)
        CALL WRTBV8(NDIM,NTST,NCOL,ISW,PAR,ICP,RLDOT,NDX,UPS,VPS,
     *       TM,DTM,NPAR)
C
        DEALLOCATE(U,TM,DTM,UPS,VPS)
C
      STOP
      CONTAINS
C
C     ---------- ----
      SUBROUTINE INIT(NDIM,IPS,NTST,NCOL,ISW,NPAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER(LEN=20) STR
C
C Reads the file of continuation constants
C
      READ(2,'(A)') STR
      STR=ADJUSTL(STR)
      IF (STR(1:4)=='NPAR') THEN
         READ(STR(SCAN(STR,'=')+1:),*)NPAR
      ELSE
         NPAR=NPARX
         BACKSPACE 2
      ENDIF
      READ(2,*,END=1) NDIM,IPS
      READ(2,*,END=1) 
      READ(2,*,END=1) NTST,NCOL,ISW,ISW,ISW
C
      RETURN
C
 1    PRINT*,' Error : End of file encountered in AUTO constants-file'
      RETURN
      END SUBROUTINE INIT
C
C     ---------- -----
      SUBROUTINE ADAPT(NDIM,IPS,ISW,NOLD,NCOLD,NNEW,NCNEW,TM,DTM,NDX,
     *     UPS,VPS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Adapts the distribution of the mesh points so that the increase of the
C monotone function EQDF becomes approximately equidistributed over the
C intervals. The functions UPS and VPS are interpolated on new mesh.
C
      DIMENSION UPS(NDX,*),VPS(NDX,*),TM(*),DTM(*)
C Local
      ALLOCATABLE TINT(:),TM2(:),UINT(:,:),ITM(:)
C
       NOLDP1=NOLD+1
       NNEWP1=NNEW+1
C
       ALLOCATE(TINT(NNEWP1),UINT(NDIM*NCNEW,NNEWP1))
       ALLOCATE(TM2(NNEWP1),ITM(NNEWP1))
C
       UINT(:,:)=0.d0
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
       UPS(:,:NNEWP1)=UINT(:,:)
C
C Replace VPS by its interpolant on the new mesh :
C
       CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,VPS,NNEWP1,NCNEW,
     *  TINT,UINT,TM2,ITM)
       VPS(:,:NNEWP1)=UINT(:,:)
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
      END SUBROUTINE ADAPT
C
C     ---------- ------
      SUBROUTINE INTERP(NDIM,N,NC,TM,NDX,UPS,N1,NC1,TM1,UPS1,TM2,ITM1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.
C
      DIMENSION TM(*),TM1(*),TM2(*),ITM1(*),UPS(NDX,*),UPS1(NC1*NDIM,*)
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
             UPS1(K1,J1)=W(NCP1)*UPS(K,J+1)
             DO L=1,NC
               L1=K+(L-1)*NDIM
               UPS1(K1,J1)=UPS1(K1,J1)+W(L)*UPS(L1,J)
             ENDDO
           ENDDO
         ENDDO
       ENDDO
C
       DO I=1,NDIM
         UPS1(I,N1)=UPS(I,N)
       ENDDO
C
      RETURN
      END SUBROUTINE INTERP
C
C     ---------- ------
      SUBROUTINE NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TMOLD,DTMOLD,
     * NNEW,TMNEW,IPER)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C Redistributes the mesh according to the function EQDF.
C
      DIMENSION TMOLD(*),DTMOLD(*),TMNEW(*),UPS(NDX,*)
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
      END SUBROUTINE NEWMSH
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
      END SUBROUTINE ORDR
C
C     ---------- ------
      SUBROUTINE INTWTS(N,Z,X,WTS)
C
      IMPLICIT NONE
C
C Generates weights for Lagrange interpolation.
C
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(IN) :: Z, X(N)
      DOUBLE PRECISION, INTENT(OUT) :: WTS(N)

      INTEGER IB,K
      DOUBLE PRECISION P,DENOM

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
      END SUBROUTINE INTWTS
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
      ALLOCATE(HD(NCOL*NDIM,NTST+1))
C
C Compute approximation to NCOL-th derivative :
       CALL CNTDIF(NCOL,WH)
C
       SMALL=.TRUE.
       DO J=1,NTST
         JP1=J+1
         SC=1.d0/DTM(J)**NCOL
         DO I=1,NDIM
           HD(I,J)=WH(NCOL+1)*UPS(I,JP1)
           DO K=1,NCOL
             K1=I+(K-1)*NDIM
             HD(I,J)=HD(I,J)+WH(K)*UPS(K1,J)
           ENDDO
           HD(I,J)=SC*HD(I,J)
           IF(DABS(HD(I,J)).GT.HMACH)SMALL=.FALSE.
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
C Extend by periodicity :
C
       IF(IPER==1)THEN
          HD(:,NTST+1)=HD(:,1)
          DTM(NTST+1)=DTM(1)
       ELSE
C
C Extend by extrapolation :
C
          HD(:,NTST+1)=2*HD(:,NTST)-HD(:,NTST-1)
          DTM(NTST+1)=DTM(NTST)
       ENDIF
C
C Compute approximation to (NCOL+1)-st derivative :
C
       DO J=1,NTST
         DTAV=.5d0*(DTM(J)+DTM(J+1))
         SC=1.d0/DTAV
         DO I=1,NDIM
           HD(I,J)=SC*( HD(I,J+1)-HD(I,J) )
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
           E=E+DABS( HD(I,J) )**PWR
         ENDDO
         EQF(J+1)=EQF(J)+DTM(J)*E
       ENDDO
C
       DEALLOCATE(HD)
      END SUBROUTINE EQDF
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
      END SUBROUTINE CNTDIF
C
C     ---------- ------
      SUBROUTINE WRTBV8(NDIM,NTST,NCOL,ISW,PAR,ICP,RLDOT,NDX,UPS,UDOTPS,
     *  TM,DTM,NPAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION UPS(NDX,*),UDOTPS(NDX,*),TM(*),DTM(*)
      DIMENSION PAR(*),ICP(*),RLDOT(*)
C
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
       NRD=(NDIM+7)/7+(NDIM+6)/7
       NROWPR=NRD*(NCOL*NTST+1) + (NFPR+6)/7 + (NPAR+6)/7
     *                          + (NFPR+19)/20
C
       MTOT=MOD(NTOT,10000)
       WRITE(8,101)IBR,MTOT,ITP,LAB,NFPR,ISW,NTPL,NAR,NROWPR,
     *             NTST,NCOL,NPAR
C
C Write the entire solution on unit 8 :
C
       DO J=1,NTST
         RN=1.d0/NCOL
         DO I=1,NCOL
           K1=(I-1)*NDIM+1
           K2=I*NDIM
           T=TM(J)+(I-1)*RN*DTM(J)
           WRITE(8,102)T,(UPS(K,J),K=K1,K2)
         ENDDO
       ENDDO
       WRITE(8,102)TM(NTST+1),(UPS(I,NTST+1),I=1,NDIM)
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
           WRITE(8,102)(UDOTPS(K,J),K=K1,K2)
         ENDDO
       ENDDO
       WRITE(8,102)(UDOTPS(K,NTST+1),K=1,NDIM)
C
C Write the parameter values.
C
       WRITE(8,102)(PAR(I),I=1,NPAR)
C
 101   FORMAT(6I6,I8,I6,I8,3I5)
 102   FORMAT(4X,1P7E19.10)
 103   FORMAT(20I5)
C
      RETURN
      END SUBROUTINE WRTBV8
C
      DOUBLE PRECISION FUNCTION GETP(CODE,IC,UPS)
C Dummy routine (in case GETP is called in the user routine PVLS)
      GETP=0.0d0
      END FUNCTION GETP

      END PROGRAM AUTCON
