!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Mesh and Weight Generation
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE MESH

  PRIVATE
  PUBLIC :: MSH,GENWTS,WINT,ADPTDS,ADAPT,INTWTS,SCALEB
  PUBLIC :: RINPR,RNRMSQ,RINTG,RNRM2,RMXUPS,RMNUPS

CONTAINS

! ---------- ---
  SUBROUTINE MSH(NTST,TM)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! Generates a uniform mesh on [0,1].
!
    DIMENSION TM(*)

    TM(1)=0.d0
    DT=1.d0/NTST
    DO J=1,NTST
       TM(J+1)=J*DT
    ENDDO

  END SUBROUTINE MSH

! ---------- ------
  SUBROUTINE GENWTS(NCOL,N1,WT,WP)

    IMPLICIT NONE

! Generates weights of the collocation method. The user selected
! number of collocation points (ncol) must be one of { 2,...,7 }.
!
! The following weights are generated :
!
!         WT : for the function value,
!         WP : for the first derivative,

    INTEGER, INTENT(IN) :: NCOL, N1
    DOUBLE PRECISION, INTENT(OUT) :: WT(N1,*),WP(N1,*)   
! Local
    INTEGER NCP1,I,IB,IC,K,L
    DOUBLE PRECISION ZM(NCOL),XM(NCOL+1),D,DENOM,P,SUM

! Generate the collocation points :
    CALL CPNTS(NCOL,ZM)

    NCP1=NCOL+1
    D=1.d0/NCOL
    DO I=1,NCP1
       XM(I)=(I-1)*D
    ENDDO

! Generate weights :

    DO IB=1,NCP1
       DENOM=1.d0
       DO K=1,NCP1
          IF(K.NE.IB)DENOM=DENOM*( XM(IB)-XM(K) )
       ENDDO
       DO IC=1,NCOL
! Weights for the function values :
          P=1.d0
          DO K=1,NCP1
             IF(K.NE.IB)P=P*( ZM(IC)-XM(K) )
          ENDDO
          WT(IB,IC)=P/DENOM
! Weights for derivatives :
          SUM=0.d0
          DO L=1,NCP1
             IF(L.NE.IB)THEN
                P=1.d0
                DO K=1,NCP1
                   IF(K.NE.IB.AND.K.NE.L)P=P*( ZM(IC)-XM(K) )
                ENDDO
                SUM=SUM+P
             ENDIF
          ENDDO
          WP(IB,IC)=SUM/DENOM
       ENDDO
    ENDDO
  END SUBROUTINE GENWTS

! ---------- -----
  SUBROUTINE CPNTS(NCOL,ZM)

    IMPLICIT NONE

! Generates the collocation points with respect to [0,1].

    INTEGER, INTENT(IN) :: NCOL
    DOUBLE PRECISION, INTENT(OUT) :: ZM(NCOL)

    DOUBLE PRECISION C, C1, C2, C3, R

    GOTO (2,3,4,5,6,7)NCOL-1

2   C=.5d0/DSQRT(3.0d0)
    ZM(1)=.5d0-C
    ZM(2)=.5d0+C
    RETURN

3   C=.5d0*DSQRT(0.6d0)
    ZM(1)=.5d0-C
    ZM(2)=.5d0
    ZM(3)=.5d0+C
    RETURN

4   R=6.0d0/7.0d0
    C=.5d0*DSQRT(R**2-12.0d0/35.0d0)
    C1=.5d0*DSQRT(3.0d0/7.0d0+C)
    C2=.5d0*DSQRT(3.0d0/7.0d0-C)
    ZM(1)=.5d0-C1
    ZM(2)=.5d0-C2
    ZM(3)=.5d0+C2
    ZM(4)=.5d0+C1
    RETURN

5   C1=.5d0*0.90617984593866399280d0
    C2=.5d0*0.53846931010568309104d0
    ZM(1)=.5d0-C1
    ZM(2)=.5d0-C2
    ZM(3)=.5d0
    ZM(4)=.5d0+C2
    ZM(5)=.5d0+C1
    RETURN

6   C1=.5d0*0.93246951420315202781d0
    C2=.5d0*0.66120938646626451366d0
    C3=.5d0*0.23861918608319690863d0
    ZM(1)=.5d0-C1
    ZM(2)=.5d0-C2
    ZM(3)=.5d0-C3
    ZM(4)=.5d0+C3
    ZM(5)=.5d0+C2
    ZM(6)=.5d0+C1
    RETURN

7   C1=.5d0*0.949107991234275852452d0
    C2=.5d0*0.74153118559939443986d0
    C3=.5d0*0.40584515137739716690d0
    ZM(1)=.5d0-C1
    ZM(2)=.5d0-C2
    ZM(3)=.5d0-C3
    ZM(4)=.5d0
    ZM(5)=.5d0+C3
    ZM(6)=.5d0+C2
    ZM(7)=.5d0+C1
    RETURN
  END SUBROUTINE CPNTS

! ---------- ------
  SUBROUTINE CNTDIF(N,D)

    IMPLICIT NONE

! Generates the coefficients of the central difference formula for
! Nth derivative at uniformly spaced points
!              0 = x  < x  < ... < x  = 1.
!                   0    1          N
    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(OUT) :: D(N+1)

    INTEGER I,K,K1
    DOUBLE PRECISION SC

    D(1)=1.d0
    IF(N.EQ.0)RETURN

    DO I=1,N
       D(I+1)=0.d0
       DO K=1,I
          K1=I+2-K
          D(K1)=D(K1-1)-D(K1)
       ENDDO
       D(1)=-D(1)
    ENDDO

! Scale to [0,1]  :

    SC=N**N
    DO I=1,N+1
       D(I)=SC*D(I)
    ENDDO

  END SUBROUTINE CNTDIF

! ---------- ----
  SUBROUTINE WINT(N,WI)

    IMPLICIT NONE

! Generates the weights for the integration formula based on polynomial
! interpolation at N equally spaced points in [0,1].

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(OUT) :: WI(N)

    DOUBLE PRECISION C

    GOTO (3,4,5,6,7,8)N-2

3   C=1.d0/6.0d0
    WI(1)=C
    WI(2)=4.0d0*C
    WI(3)=C
    RETURN

4   C=1.d0/8.0d0
    WI(1)=C
    WI(2)=3.0d0*C
    WI(3)=WI(2)
    WI(4)=C
    RETURN

5   C=1.d0/90.0d0
    WI(1)=7.0d0*C
    WI(2)=32.0d0*C
    WI(3)=12.0d0*C
    WI(4)=WI(2)
    WI(5)=WI(1)
    RETURN

6   WI(1)=19.0d0/288.0d0
    WI(2)=25.0d0/96.0d0
    WI(3)=25.0d0/144.0d0
    WI(4)=WI(3)
    WI(5)=WI(2)
    WI(6)=WI(1)
    RETURN

7   WI(1)=41.0d0/840.0d0
    WI(2)=9.0d0/35.0d0
    WI(3)=9.0d0/280.0d0
    WI(4)=34.0d0/105.0d0
    WI(5)=WI(3)
    WI(6)=WI(2)
    WI(7)=WI(1)
    RETURN

8   WI(1)=751.0d0/17280.0d0
    WI(2)=3577.0d0/17280.0d0
    WI(3)=49.0d0/640.0d0
    WI(4)=2989.0d0/17280.0d0
    WI(5)=WI(4)
    WI(6)=WI(3)
    WI(7)=WI(2)
    WI(8)=WI(1)

  END SUBROUTINE WINT

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Stepsize and Mesh Adaption
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE ADPTDS(NIT,ITNW,IBR,NTOP,DSMAX,RDS)

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: NIT,ITNW,IBR,NTOP
    DOUBLE PRECISION, INTENT(IN) :: DSMAX
    DOUBLE PRECISION, INTENT(INOUT) :: RDS
! Local
    DOUBLE PRECISION ARDS

! The stepsize along the branch of solutions is adapted depending on the
! number of Newton iterations in the previous step (called if IADS > 0).

    SELECT CASE(NIT)
    CASE(0:1)
       RDS= 2.d0*RDS
    CASE(2)
       RDS= 1.5*RDS
    CASE(3:)
       IF(NIT<=ITNW/2)THEN
          RDS= 1.1*RDS
       ELSE IF(NIT>=ITNW)THEN
          RDS=.5d0*RDS
       ENDIF
    END SELECT

    ARDS= ABS(RDS)
    IF(ARDS>DSMAX)RDS=RDS*DSMAX/ARDS

    WRITE(9,"(/,I4,I6,8X,A,I3)")ABS(IBR),NTOP,' Iterations   : ',NIT
    WRITE(9,"(I4,I6,8X,A,ES13.5)")ABS(IBR),NTOP,' Next Step    : ',RDS

  END SUBROUTINE ADPTDS

! ---------- -----
  SUBROUTINE ADAPT(IAP,NOLD,NCOLD,NNEW,NCNEW,TM,DTM,NDX,UPS,VPS)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Adapts the distribution of the mesh points so that the increase of the
! monotone function EQDF becomes approximately equidistributed over the
! intervals. The functions UPS and VPS are interpolated on new mesh.

    DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),TM(*),DTM(*)
! Local
    ALLOCATABLE TINT(:),UINT(:,:),TM2(:),ITM(:)

    NDIM=IAP(1)
    IPS=IAP(2)
    ISW=IAP(10)

    NOLDP1=NOLD+1
    NNEWP1=NNEW+1
    NRWNEW=NDIM*NCNEW
    ALLOCATE(TINT(NNEWP1),UINT(NRWNEW,NNEWP1))
    ALLOCATE(TM2(NNEWP1),ITM(NNEWP1))

    DO J=1,NNEWP1
       DO I=1,NRWNEW
          UINT(I,J)=0.d0
       ENDDO
    ENDDO

! For periodic boundary conditions extrapolate by periodicity.

    IF(IPS.EQ.2 .AND. ABS(ISW).LE.1) THEN
       IPER=1
    ELSE
       IPER=0
    ENDIF

! Generate the new mesh :

    CALL NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TM,DTM,NNEW,TINT,IPER)

! Replace UPS by its interpolant on the new mesh :

    CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,UPS,NNEWP1,NCNEW, &
         TINT,UINT,TM2,ITM)
    DO J=1,NNEWP1
       DO I=1,NRWNEW
          UPS(I,J)=UINT(I,J)
       ENDDO
    ENDDO

! Replace VPS by its interpolant on the new mesh :

    CALL INTERP(NDIM,NOLDP1,NCOLD,TM,NDX,VPS,NNEWP1,NCNEW, &
         TINT,UINT,TM2,ITM)
    DO J=1,NNEWP1
       DO I=1,NRWNEW
          VPS(I,J)=UINT(I,J)
       ENDDO
    ENDDO

! Replace old mesh :

    TM(1)=0.d0
    DO J=1,NNEW
       DTM(J)=TINT(J+1)-TINT(J)
       TM(J+1)=TINT(J+1)
    ENDDO

    DEALLOCATE(TINT,UINT,TM2,ITM)
  END SUBROUTINE ADAPT

! ---------- ------
  SUBROUTINE INTERP(NDIM,N,NC,TM,NDX,UPS,N1,NC1,TM1,UPS1,TM2,ITM1)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.

    DIMENSION TM(*),TM1(*),TM2(*),ITM1(*),UPS(NDX,*),UPS1(NC1*NDIM,*)
! Local
    DIMENSION X(NC+1),W(NC+1)

    NCP1=NC+1
    N1M1=N1-1

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

    DO I=1,NDIM
       UPS1(I,N1)=UPS(I,N)
    ENDDO

  END SUBROUTINE INTERP

! ---------- ------
  SUBROUTINE NEWMSH(NDIM,NDX,UPS,NOLD,NCOLD,TMOLD,DTMOLD,NNEW,TMNEW,IPER)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Redistributes the mesh according to the function EQDF.

    DIMENSION TMOLD(*),DTMOLD(*),TMNEW(*),UPS(*)
! Local
    ALLOCATABLE EQF(:),UNEQ(:),IAL(:)
    ALLOCATE(EQF(NOLD+1),UNEQ(NNEW+1),IAL(NNEW+1))

! Put the values of the monotonely increasing function EQDF in EQF.

    CALL EQDF(NOLD,NDIM,NCOLD,DTMOLD,NDX,UPS,EQF,IPER)

! Uniformly divide the range of EQDF :

    NOLDP1=NOLD+1
    NNEWP1=NNEW+1
    DAL=EQF(NOLDP1)/NNEW
    DO J=1,NNEWP1
       UNEQ(J)=(J-1)*DAL
    ENDDO

    CALL ORDR(NOLDP1,EQF,NNEWP1,UNEQ,IAL)

! Generate the new mesh in TMNEW :

    DO J1=1,NNEW
       J=IAL(J1)
       X=(UNEQ(J1)-EQF(J))/(EQF(J+1)-EQF(J))
       TMNEW(J1)=(1.d0-X)*TMOLD(J)+X*TMOLD(J+1)
    ENDDO

! Assign TMNEW(NNEWP1) explicitly because of loss of precision
! problems when EQF(NOLDP1) and EQF(NOLD) are very close

    TMNEW(NNEWP1)=TMOLD(NOLDP1)

    DEALLOCATE(EQF,UNEQ,IAL)
  END SUBROUTINE NEWMSH

! ---------- ----
  SUBROUTINE ORDR(N,TM,N1,TM1,ITM1)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! TM and TM1 are two ascending arrays with values in [0,1]. On exit the
! value of ITM1( i ) specifies the index of the TM-interval in which
! TM1(i) lies.

    DIMENSION TM(N),TM1(N1),ITM1(N1)

    K0=2
    DO J1=1,N1
       K1=K0
       DO J=K0,N
          K1=J
          IF(TM1(J1).LT.TM(J))GOTO 1
       ENDDO
1      ITM1(J1)=K1-1
       K0=K1
    ENDDO
  END SUBROUTINE ORDR

! ---------- ------
  SUBROUTINE INTWTS(N,Z,X,WTS)

    IMPLICIT NONE

! Generates weights for Lagrange interpolation.

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

  END SUBROUTINE INTWTS

! ---------- ----
  SUBROUTINE EQDF(NTST,NDIM,NCOL,DTM,NDX,UPS,EQF,IPER)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    PARAMETER (HMACH=1.0d-7,RSMALL=1.0d-30,RLARGE=1.0d+30)

    DIMENSION UPS(NDX,*),EQF(*),DTM(*)
    LOGICAL SMALL
! Local
    DIMENSION WH(NCOL+1)
    ALLOCATABLE HD(:,:)
    ALLOCATE(HD(NDIM*NCOL,NTST+1))

! Compute approximation to NCOL-th derivative :
    CALL CNTDIF(NCOL,WH)

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
          IF(ABS(HD(I,J)).GT.HMACH)SMALL=.FALSE.
       ENDDO
    ENDDO

! Take care of "small derivative" case.

    IF(SMALL)THEN
       DO I=1,NTST+1
          EQF(I)=I-1
       ENDDO
       DEALLOCATE(HD)
       RETURN
    ENDIF

    IF(IPER.EQ.1)THEN
!        *Extend by periodicity :
       DO I=1,NDIM
          HD(I,NTST+1)=HD(I,1)
       ENDDO
       DTM(NTST+1)=DTM(1)
    ELSE
!        *Extend by extrapolation :
       DO I=1,NDIM
          HD(I,NTST+1)=2*HD(I,NTST)-HD(I,NTST-1)
       ENDDO
       DTM(NTST+1)=DTM(NTST)
    ENDIF

! Compute approximation to (NCOL+1)-st derivative :

    DO J=1,NTST
       JP1=J+1
       DTAV=.5d0*(DTM(J)+DTM(J+1))
       SC=1.d0/DTAV
       DO I=1,NDIM
          HD(I,J)=SC*( HD(I,JP1)-HD(I,J) )
       ENDDO
    ENDDO

! Define the equidistribution function :

    PWR=1.d0/(NCOL+1.d0)
    EQF(1)=0.d0
    DO J=1,NTST
       E=0.d0
       DO I=1,NDIM
          E=E+ABS( HD(I,J) )**PWR
       ENDDO
       EQF(J+1)=EQF(J)+DTM(J)*E
    ENDDO


    DEALLOCATE(HD)
  END SUBROUTINE EQDF

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RINPR(IAP,NDIM1,NDX,UPS,VPS,DTM,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Computes the L2 inner product of UPS and VPS.
! (Using the first NDIM1 components only.)

    DIMENSION IAP(*),UPS(NDX,*),VPS(NDX,*),DTM(*),THU(*)
! Local
    DIMENSION WI(IAP(6)+1)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

! Weights for the integration formulae :
    CALL WINT(NCOL+1,WI)

    S=0.d0
    DO J=1,NTST
       JP1=J+1
       SJ=0.d0
       DO I=1,NDIM1
          DO K=1,NCOL
             K1=(K-1)*NDIM+I
             SJ=SJ+WI(K)*THU(I)*UPS(K1,J)*VPS(K1,J)
          ENDDO
          SJ=SJ+WI(NCOL+1)*THU(I)*UPS(I,JP1)*VPS(I,JP1)
       ENDDO
       S=S+DTM(J)*SJ
    ENDDO

    RINPR=S

  END FUNCTION RINPR

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RNRMSQ(IAP,NDIM1,NDX,UPS,DTM,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    DIMENSION THU(*),IAP(*),UPS(*),DTM(*)

! Finds the norm-squared of UPS (first NDIM1 components are included only).

    RNRMSQ=RINPR(IAP,NDIM1,NDX,UPS,UPS,DTM,THU)

  END FUNCTION RNRMSQ

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RINTG(IAP,NDX,IC,UPS,DTM)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Computes the integral of the IC'th component of UPS.

    DIMENSION IAP(*),UPS(NDX,*),DTM(*)
! Local
    DIMENSION WI(IAP(6)+1)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

! Weights for the integration formulae :
    CALL WINT(NCOL+1,WI)

    S=0.d0
    DO J=1,NTST
       JP1=J+1
       SJ=0.d0
       DO K=1,NCOL
          K1=(K-1)*NDIM+IC
          SJ=SJ+WI(K)*UPS(K1,J)
       ENDDO
       SJ=SJ+WI(NCOL+1)*UPS(IC,JP1)
       S=S+DTM(J)*SJ
    ENDDO

    RINTG=S

  END FUNCTION RINTG

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RNRM2(IAP,NDX,IC,UPS,DTM)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Computes the L2-norm of the IC'th component of UPS.
 
    DIMENSION IAP(*),UPS(NDX,*),DTM(*)
! Local
    DIMENSION WI(IAP(6)+1)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

! Weights for the integration formulae :
    CALL WINT(NCOL+1,WI)

    S=0.d0
    DO J=1,NTST
       JP1=J+1
       SJ=0.d0
       DO K=1,NCOL
          K1=(K-1)*NDIM+IC
          SJ=SJ+WI(K)*UPS(K1,J)**2
       ENDDO
       SJ=SJ+WI(NCOL+1)*UPS(IC,JP1)**2
       S=S+DTM(J)*SJ
    ENDDO

    RNRM2=DSQRT(S)

  END FUNCTION RNRM2

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RMXUPS(IAP,NDX,I,UPS)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Computes the maximum of the I'th component of UPS.

    DIMENSION IAP(*),UPS(NDX,*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

    RMXUPS=UPS(I,1)

    DO J=1,NTST
       DO K=1,NCOL
          K1=(K-1)*NDIM+I
          IF(UPS(K1,J).GT.RMXUPS)RMXUPS=UPS(K1,J)
       ENDDO
    ENDDO
    IF(UPS(I,NTST+1).GT.RMXUPS)RMXUPS=UPS(I,NTST+1)

  END FUNCTION RMXUPS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RMNUPS(IAP,NDX,I,UPS)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Computes the minimum of the I'th component of UPS.

    DIMENSION IAP(*),UPS(NDX,*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)

    RMNUPS=UPS(I,1)

    DO J=1,NTST
       DO K=1,NCOL
          K1=(K-1)*NDIM+I
          IF(UPS(K1,J).LT.RMNUPS)RMNUPS=UPS(K1,J)
       ENDDO
    ENDDO
    IF(UPS(I,NTST+1).LT.RMNUPS)RMNUPS=UPS(I,NTST+1)

  END FUNCTION RMNUPS

! ---------- ------
  SUBROUTINE SCALEB(IAP,NDIM1,NDX,DVPS,RLD,DTM,THL,THU)

    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

! Scales the vector (DVPS,RLD) so its norm becomes 1.

    DIMENSION IAP(*),DVPS(NDX,*),DTM(*),RLD(*),THL(*),THU(*)

    NDIM=IAP(1)
    NTST=IAP(5)
    NCOL=IAP(6)
    NFPR=IAP(29)

    SS=RNRMSQ(IAP,NDIM1,NDX,DVPS,DTM,THU)

    DO I=1,NFPR
       SS=SS+THL(I)*RLD(I)**2
    ENDDO

    SC=1.d0/DSQRT(SS)

    DO J=1,NTST
       DO I=1,NCOL
          K1=(I-1)*NDIM
          DO K=K1+1,K1+NDIM1
             DVPS(K,J)=DVPS(K,J)*SC
          ENDDO
       ENDDO
    ENDDO

    DO I=1,NDIM1
       DVPS(I,NTST+1)=DVPS(I,NTST+1)*SC
    ENDDO

    DO I=1,NFPR
       RLD(I)=SC*RLD(I)
    ENDDO

  END SUBROUTINE SCALEB

END MODULE MESH
