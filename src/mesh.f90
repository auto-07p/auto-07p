!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!                    Mesh and Weight Generation
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
MODULE MESH

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MSH,GENWTS,WINT,ADPTDS,ADAPT,ADAPT2,INTWTS,SCALEB,SCALEBP
  PUBLIC :: RNRMSQ,RNRMSQP,RINTG,RINTGP,RNRM2,RNRM2P,RMXUPS,RMNUPS
  PUBLIC :: RMXUPST,RMNUPST,MXUPSP,RMXUPSP,RMNUPSP

CONTAINS

! ---------- ---
  SUBROUTINE MSH(NTST,TM)

! Generates a uniform mesh on [0,1].

    INTEGER, INTENT(IN) :: NTST
    DOUBLE PRECISION, INTENT(OUT) :: TM(0:NTST)

    INTEGER J

    DO J=0,NTST
       TM(J)=DBLE(J)/NTST
    ENDDO

  END SUBROUTINE MSH

! ---------- ------
  SUBROUTINE GENWTS(NCOL,WT,WP)

! Generates weights of the collocation method. The user selected
! number of collocation points (ncol) must be one of { 2,...,7 }.
!
! The following weights are generated :
!
!         WT : for the function value,
!         WP : for the first derivative,

    INTEGER, INTENT(IN) :: NCOL
    DOUBLE PRECISION, INTENT(OUT) :: WT(0:NCOL,NCOL),WP(0:NCOL,NCOL)
! Local
    INTEGER IB,IC,K,L,DENOM
    DOUBLE PRECISION ZM(NCOL),P,SUM

! Generate the collocation points :
    CALL CPNTS(NCOL,ZM)

! Generate weights :

    DO IB=0,NCOL
       DENOM=1
       DO K=0,NCOL
          IF(K/=IB)DENOM=DENOM*(IB-K)
       ENDDO
       DO IC=1,NCOL
! Weights for the function values :
          P=1.d0
          DO K=0,NCOL
             IF(K/=IB)P=P*( ZM(IC)*NCOL-K )
          ENDDO
          WT(IB,IC)=P/DENOM
! Weights for derivatives :
          SUM=0.d0
          DO L=0,NCOL
             IF(L/=IB)THEN
                P=1.d0
                DO K=0,NCOL
                   IF(K/=IB.AND.K/=L)P=P*( ZM(IC)*NCOL-K )
                ENDDO
                SUM=SUM+P*NCOL
             ENDIF
          ENDDO
          WP(IB,IC)=SUM/DENOM
       ENDDO
    ENDDO
  END SUBROUTINE GENWTS

! ---------- -----
  SUBROUTINE CPNTS(NCOL,ZM)

! Generates the collocation points with respect to [0,1].

    INTEGER, INTENT(IN) :: NCOL
    DOUBLE PRECISION, INTENT(OUT) :: ZM(NCOL)

    DOUBLE PRECISION, PARAMETER :: &
         C51 = .5d0*0.90617984593866399280d0, &
         C52 = .5d0*0.53846931010568309104d0, &
         C61 = .5d0*0.93246951420315202781d0, &
         C62 = .5d0*0.66120938646626451366d0, &
         C63 = .5d0*0.23861918608319690863d0, &
         C71 = .5d0*0.949107991234275852452d0, &
         C72 = .5d0*0.74153118559939443986d0, &
         C73 = .5d0*0.40584515137739716690d0, &
         R = 6.0d0/7.0d0

    DOUBLE PRECISION C21,C31,C4,C41,C42

    SELECT CASE(NCOL)
       CASE(2)
          C21 = .5d0/SQRT(3.0d0)
          ZM(1) = .5d0-C21
          ZM(2) = .5d0+C21
       CASE(3)
          C31 = .5d0*SQRT(0.6d0)
          ZM(1) = .5d0-C31
          ZM(2) = .5d0
          ZM(3) = .5d0+C31
       CASE(4)
          C4  = .5d0*SQRT(R**2-12.0d0/35.0d0)
          C41 = .5d0*SQRT(3.0d0/7.0d0+C4)
          C42 = .5d0*SQRT(3.0d0/7.0d0-C4)
          ZM(1) = .5d0-C41
          ZM(2) = .5d0-C42
          ZM(3) = .5d0+C42
          ZM(4) = .5d0+C41
       CASE(5)
          ZM(:) = (/ .5d0-C51, .5d0-C52, .5d0, .5d0+C52, .5d0+C51 /)
       CASE(6)
          ZM(:) = (/ .5d0-C61, .5d0-C62, .5d0-C63, &
                     .5d0+C63, .5d0+C62, .5d0+C61 /)
       CASE(7)
          ZM(:) = (/ .5d0-C71, .5d0-C72, .5d0-C73, .5d0, &
                     .5d0+C73, .5d0+C72, .5d0+C71  /)
    END SELECT
  END SUBROUTINE CPNTS

! ---------- ------
  SUBROUTINE CNTDIF(N,D)

! Generates the coefficients of the central difference formula for
! Nth derivative at uniformly spaced points
!              0 = x  < x  < ... < x  = 1.
!                   0    1          N
    INTEGER, INTENT(IN) :: N
    INTEGER, INTENT(OUT) :: D(0:N)

    INTEGER I,K

    D(0)=1
    IF(N.EQ.0)RETURN

    DO I=1,N
       D(I)=0
       DO K=I,1,-1
          D(K)=D(K-1)-D(K)
       ENDDO
       D(0)=-D(0)
    ENDDO

  END SUBROUTINE CNTDIF

! ---------- ----
  SUBROUTINE WINT(NCOL,WI)

! Generates the weights for the integration formula based on polynomial
! interpolation at NCOL+1 equally spaced points in [0,1].

    INTEGER, INTENT(IN) :: NCOL
    DOUBLE PRECISION, INTENT(OUT) :: WI(0:NCOL)

    DOUBLE PRECISION C

    SELECT CASE(NCOL)
    CASE(2)
       C=1.d0/6.0d0
       WI(0)=C
       WI(1)=4.0d0*C
       WI(2)=C
    CASE(3)
       C=1.d0/8.0d0
       WI(0)=C
       WI(1)=3.0d0*C
       WI(2)=WI(1)
       WI(3)=C
    CASE(4)
       C=1.d0/90.0d0
       WI(0)=7.0d0*C
       WI(1)=32.0d0*C
       WI(2)=12.0d0*C
       WI(3)=WI(1)
       WI(4)=WI(0)
    CASE(5)
       WI(0)=19.0d0/288.0d0
       WI(1)=25.0d0/96.0d0
       WI(2)=25.0d0/144.0d0
       WI(3)=WI(2)
       WI(4)=WI(1)
       WI(5)=WI(0)
    CASE(6)
       WI(0)=41.0d0/840.0d0
       WI(1)=9.0d0/35.0d0
       WI(2)=9.0d0/280.0d0
       WI(3)=34.0d0/105.0d0
       WI(4)=WI(2)
       WI(5)=WI(1)
       WI(6)=WI(0)
    CASE(7)
       WI(0)=751.0d0/17280.0d0
       WI(1)=3577.0d0/17280.0d0
       WI(2)=49.0d0/640.0d0
       WI(3)=2989.0d0/17280.0d0
       WI(4)=WI(3)
       WI(5)=WI(2)
       WI(6)=WI(1)
       WI(7)=WI(0)
    END SELECT

  END SUBROUTINE WINT

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Stepsize and Mesh Adaption
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! ---------- ------
  SUBROUTINE ADPTDS(NIT,ITNW,IBR,NTOP,IID,DSMAX,RDS)

    INTEGER, INTENT(IN) :: NIT,ITNW,IBR,NTOP,IID
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

    IF(IID>0)THEN
       WRITE(9,"(/,I4,I6,8X,A,I3)")ABS(IBR),NTOP,' Iterations   : ',NIT
       WRITE(9,"(I4,I6,8X,A,ES13.5)")ABS(IBR),NTOP,' Next Step    : ',RDS
    ENDIF

  END SUBROUTINE ADPTDS

! ---------- -----
  SUBROUTINE ADAPT(NTST,NTSTNA,NCOL,NDIM,TM,DTM,UPS,VPS,IPER)

    USE AUTOMPI

! Adapts the distribution of the mesh points so that the increase of the
! monotone function EQDF becomes approximately equidistributed over the
! intervals. The functions UPS and VPS are interpolated on new mesh.

    INTEGER, INTENT(IN) :: NTST,NTSTNA,NCOL,NDIM
! For periodic boundary conditions extrapolate by periodicity.
    LOGICAL, INTENT(IN) :: IPER
    DOUBLE PRECISION, INTENT(INOUT) :: UPS(NDIM,0:NTSTNA*NCOL), &
         VPS(NDIM,0:NTSTNA*NCOL),TM(0:NTSTNA),DTM(NTSTNA)
! Local
    INTEGER :: IAM
    DOUBLE PRECISION, ALLOCATABLE :: TINT(:),UINT(:,:),VINT(:,:)

! Generate the new mesh :

    IAM=MPIIAM()
    IF(IAM==0)THEN
       ALLOCATE(TINT(0:NTST),UINT(NDIM,0:NTST*NCOL),VINT(NDIM,0:NTST*NCOL))

       CALL NEWMSH(NDIM,NDIM,UPS,NTST,NCOL,TM,DTM,NTST,TINT,IPER)

       CALL MPISCAT(TINT,1,NTST,1)

! Replace UPS/VPS by their interpolants on the new mesh :

       CALL INTERP(NDIM,NTST,NCOL,TM,UPS,VPS,NDIM,NTST,NCOL,TINT, &
         UINT,VINT)

       CALL MPISCAT(UINT,NDIM*NCOL,NTST,NDIM)
       CALL MPISCAT(VINT,NDIM*NCOL,NTST,NDIM)

       UPS(:,:)=UINT(:,:)
       VPS(:,:)=VINT(:,:)

! Replace old mesh :

       DTM(:)=TINT(1:NTST)-TINT(0:NTST-1)
       TM(:)=TINT(:)

       DEALLOCATE(TINT,UINT)
    ELSE
       CALL MPISCAT(TM,1,NTST,1)
       DTM(:)=TM(1:NTSTNA)-TM(0:NTSTNA-1)
       CALL MPISCAT(UPS,NDIM*NCOL,NTST,NDIM)
       CALL MPISCAT(VPS,NDIM*NCOL,NTST,NDIM)
    ENDIF

  END SUBROUTINE ADAPT

! ---------- ------
  SUBROUTINE ADAPT2(NOLD,NCOLD,NDOLD,NNEW,NCNEW,NDIM, &
       TMR,UPSR,VPSR,TM,UPS,VPS,IPER)

! Adapts the distribution of the mesh points so that the increase of the
! monotone function EQDF becomes approximately equidistributed over the
! intervals. The functions UPS and VPS are interpolated on new mesh.

    INTEGER, INTENT(IN) :: NOLD,NCOLD,NDOLD,NNEW,NCNEW,NDIM
    LOGICAL, INTENT(IN) :: IPER
    DOUBLE PRECISION, INTENT(IN) :: UPSR(NDOLD,0:*),VPSR(NDOLD,0:*),TMR(0:NOLD)
    DOUBLE PRECISION, INTENT(OUT) :: UPS(NDIM,0:*),VPS(NDIM,0:*),TM(0:*)
! Local
    INTEGER J,NDM
    DOUBLE PRECISION, ALLOCATABLE :: DTM(:)

    NDM=MIN(NDOLD,NDIM)
    IF(NOLD==NNEW.AND.NCOLD==NCNEW)THEN
! Just copy
       DO J=0,NCNEW*NNEW
          UPS(1:NDM,J)=UPSR(1:NDM,J)
          VPS(1:NDM,J)=VPSR(1:NDM,J)
       ENDDO
       TM(0:NNEW)=TMR(0:NNEW)
       RETURN
    ENDIF

! Generate the new mesh :

    ALLOCATE(DTM(NOLD))
    DTM(:)=TMR(1:NOLD)-TMR(0:NOLD-1)
    CALL NEWMSH(NDM,NDOLD,UPSR,NOLD,NCOLD,TMR,DTM,NNEW,TM,IPER)
    DEALLOCATE(DTM)

! Replace UPS/VPS by their interpolants on the new mesh :

    CALL INTERP(NDIM,NOLD,NCOLD,TMR,UPSR,VPSR,NDOLD,NNEW,NCNEW, &
         TM,UPS,VPS)

  END SUBROUTINE ADAPT2

! ---------- ------
  SUBROUTINE INTERP(NDIM,N,NC,TM,UPS,VPS,NDOLD,N1,NC1,TM1,UPS1,VPS1)

! Finds interpolant (TM(.) , UPS(.) ) on new mesh TM1.

    INTEGER, INTENT(IN) :: NDIM,N,NC,N1,NC1,NDOLD
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:N*NC),VPS(NDOLD,0:N*NC)
    DOUBLE PRECISION, INTENT(IN) :: TM(0:N),TM1(0:N1)
    DOUBLE PRECISION, INTENT(OUT) :: UPS1(NDIM,0:N1*NC1),VPS1(NDIM,0:N1*NC1)
! Local
    INTEGER I,J,J1,K,NDM
    DOUBLE PRECISION W(0:NC),Z,D,DT

    J=1
    NDM=MIN(NDIM,NDOLD)
    DO J1=0,N1-1
       DT=TM1(J1+1)-TM1(J1)
       DO I=0,NC1-1
          Z=TM1(J1)+I*DT/NC1
          DO
             IF(J>N)EXIT
             IF(TM(J)>Z)EXIT
             J=J+1
          ENDDO
          J=J-1
          D=TM(J+1)-TM(J)
          CALL INTWTS(NC,NC*(Z-TM(J))/D,W)
          DO K=1,NDM
             UPS1(K,J1*NC1+I)=DOT_PRODUCT(W(:),UPS(K,J*NC:J*NC+NC))
             VPS1(K,J1*NC1+I)=DOT_PRODUCT(W(:),VPS(K,J*NC:J*NC+NC))
          ENDDO
       ENDDO
    ENDDO

    DO I=1,NDM
       UPS1(I,N1*NC1)=UPS(I,N*NC)
       VPS1(I,N1*NC1)=VPS(I,N*NC)
    ENDDO

  END SUBROUTINE INTERP

! ---------- ------
  SUBROUTINE NEWMSH(NDIM,NDOLD,UPS,NOLD,NCOLD,TMOLD,DTMOLD,NNEW,TMNEW,IPER)

! Redistributes the mesh according to the function EQDF.

    INTEGER, INTENT(IN) :: NDIM,NDOLD,NOLD,NCOLD,NNEW
    LOGICAL, INTENT(IN) :: IPER
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:*),TMOLD(0:NOLD),DTMOLD(NOLD)
    DOUBLE PRECISION, INTENT(OUT) :: TMNEW(0:NNEW)
! Local
    INTEGER J,J1
    DOUBLE PRECISION DAL,UNEQ,X1,X2
    DOUBLE PRECISION, ALLOCATABLE :: EQF(:),DEQF(:)
    ALLOCATE(EQF(0:NOLD),DEQF(NOLD))

! Put the values of the monotonely increasing function EQDF in EQF.

    CALL EQDF(NOLD,NDIM,NCOLD,DTMOLD,UPS,NDOLD,EQF,DEQF,IPER)

! Uniformly divide the range of EQDF :

    DAL=EQF(NOLD)/NNEW

! Generate the new mesh in TMNEW :

! Assign TMNEW(0) and TMNEW(NNEW) explicitly because of loss of precision
! problems when the corresponding EQF values are very close

    TMNEW(0)=TMOLD(0)
    J=1
    DO J1=1,NNEW-1

! EQF is an ascending array with values in [0,EQF(NOLD)]. Get the
! value of the index of the TM-interval in which EQF(i) lies.
       UNEQ=J1*DAL
       DO
          IF(J>NOLD)EXIT
          IF(EQF(J)>UNEQ)EXIT
          J=J+1
       ENDDO
       J=J-1
       X1=UNEQ-EQF(J)
       X2=EQF(J+1)-UNEQ
       IF(X1<=X2)THEN
          TMNEW(J1)=TMOLD(J)+X1/DEQF(J+1)
       ELSE
          ! More accurate FP rounding for UNEQ closer to EQF(J+1)
          TMNEW(J1)=TMOLD(J+1)-X2/DEQF(J+1)
       ENDIF
    ENDDO

    TMNEW(NNEW)=TMOLD(NOLD)

    DEALLOCATE(EQF,DEQF)
  END SUBROUTINE NEWMSH

! ---------- ------
  SUBROUTINE INTWTS(N,DT1DT,WTS)

! Generates weights for Lagrange interpolation.

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(IN) :: DT1DT
    DOUBLE PRECISION, INTENT(OUT) :: WTS(0:N)

    INTEGER IB,K,DENOM
    DOUBLE PRECISION P

    DENOM=1
    DO K=2,N
       DENOM=DENOM*K
    ENDDO
    DO IB=0,N
       P=1.d0
       DO K=0,N
          IF(K/=IB)THEN
             P=P*( K-DT1DT )
          ENDIF
       ENDDO
       WTS(IB)=P/DENOM
       IF(IB<N)DENOM=DENOM/(N-IB)*(-1-IB)
    ENDDO

  END SUBROUTINE INTWTS

! ---------- ----
  SUBROUTINE EQDF(NTST,NDIM,NCOL,DTM,UPS,NDOLD,EQF,DEQF,IPER)

    DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

    INTEGER, INTENT(IN) :: NTST,NDIM,NCOL,NDOLD
    LOGICAL, INTENT(IN) :: IPER
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDOLD,0:NTST*NCOL),DTM(NTST)
    DOUBLE PRECISION, INTENT(OUT) :: EQF(0:NTST),DEQF(NTST)

! Local
    DOUBLE PRECISION SC,E,PWR,DTAV,ND
    DOUBLE PRECISION, ALLOCATABLE :: HD(:)
    LOGICAL SMALL
    INTEGER I,J,WH(0:NCOL)

    ALLOCATE(HD(NDIM))
    CALL CNTDIF(NCOL,WH)

! Compute approximation to NCOL-th and (NCOL+1)-st derivative
! and define the equidistribution function :

    SMALL=.TRUE.
    PWR=1.d0/(NCOL+1.d0)
    EQF(0)=0.d0
    DO J=1,NTST
       E=0.d0
       SC=(NCOL/DTM(J))**NCOL
       DO I=1,NDIM
          ND=SC*DOT_PRODUCT(WH(:),UPS(I,(J-1)*NCOL:J*NCOL))
          IF(J>1)THEN
             E=E+ABS( 2*( ND-HD(I) )/(DTM(J-1)+DTM(J)) )**PWR
          ENDIF
          IF(ABS(ND)>HMACH)SMALL=.FALSE.
          HD(I)=ND
       ENDDO
       IF(J>1)THEN
          EQF(J-1)=EQF(J-2)+DTM(J-1)*E
          DEQF(J-1)=E
       ENDIF
    ENDDO

    E=0.d0
    IF(IPER)THEN
!      *Extend by periodicity :
       J=1
       DTAV=(DTM(NTST)+DTM(1))/2
    ELSE
!      *Extend by extrapolation :
       J=NTST-1
       DTAV=DTM(NTST)
    ENDIF
    SC=(NCOL/DTM(J))**NCOL
    DO I=1,NDIM
       ND=SC*DOT_PRODUCT(WH(:),UPS(I,(J-1)*NCOL:J*NCOL))
       E=E+ABS( (ND-HD(I))/DTAV )**PWR
       IF(ABS(ND)>HMACH)SMALL=.FALSE.
    ENDDO
    EQF(NTST)=EQF(NTST-1)+DTM(NTST)*E
    DEQF(NTST)=E

! Take care of "small derivative" case.

    IF(SMALL)THEN
       EQF(0)=0
       DO I=1,NTST
          EQF(I)=I
          DEQF(I)=1.0d0/DTM(I)
       ENDDO
    ENDIF

    DEALLOCATE(HD)
  END SUBROUTINE EQDF

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RNRMSQ(NTST,NCOL,NDIM,NDIM1,UPS,DTM,THU)

! Finds the norm-squared of UPS (first NDIM1 components are included only).

    INTEGER, INTENT(IN) :: NTST,NCOL,NDIM,NDIM1
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NCOL*NTST)
    DOUBLE PRECISION, INTENT(IN) :: DTM(*),THU(*)
! Local
    INTEGER I,J,K,KC
    DOUBLE PRECISION S,SJ,SK,WI(0:NCOL)

! Weights for the integration formulae :
    CALL WINT(NCOL,WI)

    S=0.d0
    K=0
    DO J=1,NTST
       SJ=0.d0
       DO KC=0,NCOL
          SK=0.d0
          DO I=1,NDIM1
             SK=SK+THU(I)*UPS(I,K+KC)**2
          ENDDO
          SJ=SJ+SK*WI(KC)
       ENDDO
       K=K+NCOL
       S=S+DTM(J)*SJ
    ENDDO

    RNRMSQ=S

  END FUNCTION RNRMSQ

! ------ --------- -------- -------
  DOUBLE PRECISION FUNCTION RNRMSQP(NTST,NCOL,NDIM,NDIM1,UPS,DTM,THU,NA)

    USE AUTOMPI

! Finds the norm-squared of UPS (first NDIM1 components are included only).
! Parallelized using MPI without MPI_Reduce so addition order is consistent.

    INTEGER, INTENT(IN) :: NTST,NCOL,NDIM,NDIM1,NA
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NCOL*NTST)
    DOUBLE PRECISION, INTENT(IN) :: DTM(*),THU(*)
! Local
    INTEGER I,J,K,KC
    DOUBLE PRECISION S,SJ,SK,WI(0:NCOL)
    DOUBLE PRECISION, ALLOCATABLE :: SS(:)

! Weights for the integration formulae :

    IF(MPIKWT()==1)THEN
       RNRMSQP=RNRMSQ(NTST,NCOL,NDIM,NDIM1,UPS,DTM,THU)
       RETURN
    ENDIF

    CALL WINT(NCOL,WI)

    ALLOCATE(SS(NTST))
    K=0
    DO J=1,NA
       SJ=0.d0
       DO KC=0,NCOL
          SK=0.d0
          DO I=1,NDIM1
             SK=SK+THU(I)*UPS(I,K+KC)**2
          ENDDO
          SJ=SJ+SK*WI(KC)
       ENDDO
       K=K+NCOL
       SS(J)=DTM(J)*SJ
    ENDDO

    CALL MPIGAT(SS,1,NTST)
    S=0.d0
    IF(MPIIAM()==0)THEN
       DO J=1,NTST
          S=S+SS(J)
       ENDDO
    ENDIF

    DEALLOCATE(SS)
    RNRMSQP=S

  END FUNCTION RNRMSQP

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RINTG(NTST,NCOL,NDIM,IC,UPS,DTM)

! Computes the integral of the IC'th component of UPS.

    INTEGER, INTENT(IN) :: IC,NDIM,NCOL,NTST
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL),DTM(NTST)
! Local
    DOUBLE PRECISION WI(0:NCOL)
    INTEGER J,K
    DOUBLE PRECISION S

! Weights for the integration formulae :
    CALL WINT(NCOL,WI)

    S=0.d0
    K=0
    DO J=1,NTST
       S=S+DTM(J)*DOT_PRODUCT(WI,UPS(IC,K:K+NCOL))
       K=K+NCOL
    ENDDO

    RINTG=S

  END FUNCTION RINTG

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RINTGP(NTST,NCOL,NDIM,IC,UPS,DTM,NA)

    USE AUTOMPI

! Computes the integral of the IC'th component of UPS.
! Parallelized using MPI without MPI_Reduce so addition order is consistent.

    INTEGER, INTENT(IN) :: IC,NDIM,NCOL,NTST,NA
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL),DTM(NTST)
! Local
    DOUBLE PRECISION WI(0:NCOL)
    INTEGER J,K
    DOUBLE PRECISION S
    DOUBLE PRECISION, ALLOCATABLE :: SS(:)

    IF(MPIKWT()==1)THEN
       RINTGP=RINTG(NTST,NCOL,NDIM,IC,UPS,DTM)
       RETURN
    ENDIF

! Weights for the integration formulae :
    CALL WINT(NCOL,WI)

    ALLOCATE(SS(NTST))
    K=0
    DO J=1,NA
       SS(J)=DTM(J)*DOT_PRODUCT(WI,UPS(IC,K:K+NCOL))
       K=K+NCOL
    ENDDO

    CALL MPIGAT(SS,1,NTST)
    S=0.d0
    IF(MPIIAM()==0)THEN
       DO J=1,NTST
          S=S+SS(J)
       ENDDO
    ENDIF

    DEALLOCATE(SS)
    RINTGP=S

  END FUNCTION RINTGP

! ------ --------- -------- -----
  DOUBLE PRECISION FUNCTION RNRM2(NTST,NCOL,NDIM,IC,UPS,DTM)

! Computes the L2-norm of the IC'th component of UPS.
 
    INTEGER, INTENT(IN) :: IC,NDIM,NCOL,NTST
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL),DTM(NTST)
! Local
    DOUBLE PRECISION WI(0:NCOL)
    INTEGER J,K
    DOUBLE PRECISION S

! Weights for the integration formulae :
    CALL WINT(NCOL,WI)

    S=0.d0
    K=0
    DO J=1,NTST
       S=S+DTM(J)*DOT_PRODUCT(WI,UPS(IC,K:K+NCOL)**2)
       K=K+NCOL
    ENDDO

    RNRM2=SQRT(S)

  END FUNCTION RNRM2

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RNRM2P(NTST,NCOL,NDIM,IC,UPS,DTM,NA)

    USE AUTOMPI

! Computes the L2-norm of the IC'th component of UPS.
! Parallelized using MPI without MPI_Reduce so addition order is consistent.

    INTEGER, INTENT(IN) :: IC,NDIM,NCOL,NTST,NA
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL),DTM(NTST)
! Local
    DOUBLE PRECISION WI(0:NCOL)
    INTEGER J,K
    DOUBLE PRECISION S
    DOUBLE PRECISION, ALLOCATABLE :: SS(:)

    IF(MPIKWT()==1)THEN
       RNRM2P=RNRM2(NTST,NCOL,NDIM,IC,UPS,DTM)
       RETURN
    ENDIF

! Weights for the integration formulae :
    CALL WINT(NCOL,WI)

    ALLOCATE(SS(NTST))
    K=0
    DO J=1,NA
       SS(J)=DTM(J)*DOT_PRODUCT(WI,UPS(IC,K:K+NCOL)**2)
       K=K+NCOL
    ENDDO

    CALL MPIGAT(SS,1,NTST)
    S=0.d0
    IF(MPIIAM()==0)THEN
       DO J=1,NTST
          S=S+SS(J)
       ENDDO
    ENDIF

    DEALLOCATE(SS)
    RNRM2P=SQRT(S)

  END FUNCTION RNRM2P

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RMXUPS(NTST,NCOL,NDIM,I,UPS)

! Computes the maximum of the I'th component of UPS.

    INTEGER, INTENT(IN) :: I,NDIM,NCOL,NTST
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL)

    RMXUPS=MAXVAL(UPS(I,:))

  END FUNCTION RMXUPS

! ------ --------- -------- -------
  DOUBLE PRECISION FUNCTION RMXUPSP(NA,NCOL,NDIM,I,UPS)

    USE AUTOMPI

! Computes the maximum of the I'th component of UPS (using MPI).

    INTEGER, INTENT(IN) :: I,NDIM,NCOL,NA
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NA*NCOL)

    DOUBLE PRECISION BUF(1)

    BUF(1)=MAXVAL(UPS(I,:))
    CALL MPIREDUCEMAX(BUF,1)
    RMXUPSP=BUF(1)

  END FUNCTION RMXUPSP

! ---------- ------
  SUBROUTINE MXUPSP(NA,NCOL,NDIM,NDM,UPS,UMX)

    USE AUTOMPI

! Computes the maximum of all components of UPS (using MPI).

    INTEGER, INTENT(IN) :: NDIM,NDM,NCOL,NA
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NA*NCOL)
    DOUBLE PRECISION, INTENT(OUT) :: UMX(NDIM)

    INTEGER I

    DO I=1,NDM
       UMX(I)=MAXVAL(UPS(I,:))
    ENDDO
    CALL MPIREDUCEMAX(UMX,NDM)

  END SUBROUTINE MXUPSP

! ------ --------- -------- -------
  DOUBLE PRECISION FUNCTION RMXUPST(NTST,NCOL,NDIM,I,UPS,DTM)

! Computes the t value for the maximum of the I'th component of UPS.

    INTEGER, INTENT(IN) :: I,NDIM,NCOL,NTST
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL),DTM(NTST)

    INTEGER LOC(1),J,L,M

    LOC=MAXLOC(UPS(I,:))
    L=LOC(1)-1
    J=L/NCOL
    RMXUPST=SUM(DTM(1:J))
    M=MOD(L,NCOL)
    IF(M>0)THEN
       RMXUPST=RMXUPST+DTM(J+1)*M/NCOL
    ENDIF

  END FUNCTION RMXUPST

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RMNUPS(NTST,NCOL,NDIM,I,UPS)

! Computes the minimum of the I'th component of UPS.

    INTEGER, INTENT(IN) :: I,NDIM,NCOL,NTST
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL)

    RMNUPS=MINVAL(UPS(I,:))

  END FUNCTION RMNUPS

! ------ --------- -------- ------
  DOUBLE PRECISION FUNCTION RMNUPSP(NA,NCOL,NDIM,I,UPS)

    USE AUTOMPI

! Computes the minimum of the I'th component of UPS (using MPI).

    INTEGER, INTENT(IN) :: I,NDIM,NCOL,NA
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NA*NCOL)

    DOUBLE PRECISION BUF(1)

    BUF(1)=MINVAL(UPS(I,:))
    CALL MPIREDUCEMIN(BUF,1)
    RMNUPSP=BUF(1)
    
  END FUNCTION RMNUPSP

! ------ --------- -------- -------
  DOUBLE PRECISION FUNCTION RMNUPST(NTST,NCOL,NDIM,I,UPS,DTM)

! Computes the t value for the minimum of the I'th component of UPS.

    INTEGER, INTENT(IN) :: I,NDIM,NCOL,NTST
    DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:NTST*NCOL),DTM(NTST)

    INTEGER LOC(1),J,L,M

    LOC=MINLOC(UPS(I,:))
    L=LOC(1)-1
    J=L/NCOL
    RMNUPST=SUM(DTM(1:J))
    M=MOD(L,NCOL)
    IF(M>0)THEN
       RMNUPST=RMNUPST+DTM(J+1)*M/NCOL
    ENDIF

  END FUNCTION RMNUPST

! ---------- ------
  SUBROUTINE SCALEB(NTST,NCOL,NDIM,NFPR,DVPS,RLD,DTM,THL,THU)

! Scales the vector (DVPS,RLD) so its norm becomes 1.

    INTEGER, INTENT(IN) :: NTST,NCOL,NDIM,NFPR
    DOUBLE PRECISION, INTENT(IN) :: DTM(*),THL(NFPR),THU(*)
    DOUBLE PRECISION, INTENT(INOUT) :: DVPS(NDIM,0:NCOL*NTST),RLD(NFPR)

    DOUBLE PRECISION SS

    SS=SQRT(RNRMSQ(NTST,NCOL,NDIM,NDIM,DVPS,DTM,THU)+ &
         DOT_PRODUCT(THL(:),RLD(:)**2))

    DVPS(:,:)=DVPS(:,:)/SS
    RLD(:)=RLD(:)/SS

  END SUBROUTINE SCALEB

! ---------- -------
  SUBROUTINE SCALEBP(NTST,NCOL,NDIM,NFPR,DVPS,RLD,DTM,THL,THU,NA)

    USE AUTOMPI

! Scales the vector (DVPS,RLD) so its norm becomes 1.
! Parallelized using MPI

    INTEGER, INTENT(IN) :: NTST,NCOL,NDIM,NFPR,NA
    DOUBLE PRECISION, INTENT(IN) :: DTM(*),THL(NFPR),THU(*)
    DOUBLE PRECISION, INTENT(INOUT) :: DVPS(NDIM,0:NCOL*NTST),RLD(NFPR)

    DOUBLE PRECISION SS

    SS=RNRMSQP(NTST,NCOL,NDIM,NDIM,DVPS,DTM,THU,NA)

    IF(MPIIAM()==0)THEN
       SS=SQRT(SS+DOT_PRODUCT(THL(:),RLD(:)**2))
    ENDIF

    CALL MPIBCAST1(SS)

    DVPS(:,0:NCOL*NA)=DVPS(:,0:NCOL*NA)/SS
    IF(MPIIAM()==0.AND.NTST>NA)THEN
       DVPS(:,NCOL*NTST)=DVPS(:,NCOL*NTST)/SS
    ENDIF
    RLD(:)=RLD(:)/SS

  END SUBROUTINE SCALEBP

END MODULE MESH
