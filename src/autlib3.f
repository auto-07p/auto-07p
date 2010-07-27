      MODULE INTERFACES

      USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS
      USE BVP

      IMPLICIT NONE
      PRIVATE

      PUBLIC :: AUTOBVP
      PUBLIC :: FUNI,BCNI,ICNI ! Interface subroutines

      PUBLIC :: FUNC,STPNT,BCND,ICND,PVLS ! User subroutines

      INTERFACE
         SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
         INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
         DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
         DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
         DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
         END SUBROUTINE FUNC

         SUBROUTINE STPNT(NDIM,U,PAR,T)
         INTEGER, INTENT(IN) :: NDIM
         DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
         DOUBLE PRECISION, INTENT(IN) :: T
         END SUBROUTINE STPNT

         SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
         INTEGER, INTENT(IN) :: NDIM, ICP(*), NBC, IJAC
         DOUBLE PRECISION, INTENT(IN) :: PAR(*), U0(NDIM), U1(NDIM)
         DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
         DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
         END SUBROUTINE BCND

         SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJ,DINT)
         INTEGER, INTENT(IN) :: NDIM, ICP(*), NINT, IJ
         DOUBLE PRECISION, INTENT(IN) :: PAR(*),U(NDIM),UOLD(NDIM)
         DOUBLE PRECISION, INTENT(IN) :: UDOT(NDIM),UPOLD(NDIM)
         DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
         DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
         END SUBROUTINE ICND

         SUBROUTINE PVLS(NDIM,U,PAR)
         INTEGER, INTENT(IN) :: NDIM
         DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
         DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
         END SUBROUTINE PVLS
      END INTERFACE

      DOUBLE PRECISION, PARAMETER :: HMACH=1.0d-7

      CONTAINS

!     ---------- -------
      SUBROUTINE AUTOBVP(AP,ICP,ICU)

      TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
      INTEGER, INTENT(INOUT) :: ICP(:)
      INTEGER, INTENT(IN) :: ICU(:)

      INTEGER I, ISW, ITP, NDIM, NBC, NINT, NFPR, NXP, NPAR
      ISW = AP%ISW
      ITP = AP%ITP
      NDIM = AP%NDIM
      NBC = AP%NBC
      NINT = AP%NINT
      NPAR = AP%NPAR

      ! Two-Parameter Continuation for IPS=4 or IPS=7.
      IF(ABS(ISW)==2.AND.(ITP==5.OR.(ABS(ITP)/10)==5))THEN
         ! ** Continuation of folds (BVP; start/restart)
         NDIM=2*NDIM
         NBC=2*NBC
         NINT=2*NINT+1
         NFPR=NBC+NINT-NDIM+1
         NXP=NFPR/2-1
         IF(NXP.GT.0)THEN
            DO I=1,NXP
               ICP(NFPR/2+I+1)=NPAR+I
            ENDDO
         ENDIF
         ! PAR(NPAR+NFPR/2) contains a norm
         AP%NPARI=NFPR/2
         IF(ITP==5)THEN
            ! ** Continuation of folds (BVP; start)
            ICP(NFPR/2+1)=NPAR+NFPR/2
            AP%ISW=-2
         ENDIF
      ENDIF
      IF(ABS(ISW)>=2.AND.(ITP==6.OR.ABS(ITP)/10==6)) THEN
         ! ** BP cont (BVP, start and restart) (by F. Dercole).
         NXP=NBC+NINT-NDIM+1
         IF(ITP==6)THEN
            ! ** BP cont (BVP; start)
            NDIM=4*NDIM
            NBC=3*NBC+NDIM/2+NXP
            NINT=3*NINT+NXP+5
            ICP(NXP+1)=NPAR+3*NXP+NDIM/4 ! a
            ICP(NXP+2)=NPAR+3*NXP+NDIM/4+1 ! b
            DO I=1,NXP
               ICP(NXP+I+2)=NPAR+I ! q
               ICP(2*NXP+I+2)=NPAR+NXP+I ! r
               ICP(4*NXP+NDIM/4+I+3)=NPAR+3*NXP+NDIM/4+3+I ! d
            ENDDO
            DO I=1,NXP+NDIM/4-1
               ICP(3*NXP+I+2)=NPAR+2*NXP+I ! psi^*_2,psi^*_3
            ENDDO
            ICP(4*NXP+NDIM/4+2)=NPAR+3*NXP+NDIM/4+2 ! c1
            ICP(4*NXP+NDIM/4+3)=NPAR+3*NXP+NDIM/4+3 ! c2
            AP%NPARI=4*NXP+NDIM/4+3

            AP%ISW=-ABS(ISW)
         ELSE
            ! ** BP cont (BVP; restart 1 or 2)
            NDIM=2*NDIM
            NBC=NBC+NDIM+NXP
            NINT=NINT+NXP+1
            IF(ABS(ISW)==2)THEN
               ! ** Non-generic case
               ICP(NXP+2)=NPAR+3*NXP+NDIM/2+1 ! b
            ENDIF
            DO I=1,NXP+NDIM/2-1
               ICP(NXP+I+2)=NPAR+2*NXP+I ! psi^*_2,psi^*_3
            ENDDO
            DO I=1,NXP
               ICP(2*NXP+NDIM/2+I+1)=NPAR+3*NXP+NDIM/2+3+I ! d
            ENDDO
            AP%NPARI=4*NXP+NDIM/2+3
         ENDIF
      ENDIF
      AP%NDIM = NDIM
      AP%NBC = NBC
      AP%NINT = NINT
      AP%NFPR = NBC+NINT-NDIM+1

      IF(ABS(ISW)<=1)THEN
         ! ** Boundary value problems (here IPS=4)
         CALL AUTOBV(AP,ICP,ICU,FUNI,BCNI,ICNI,STPNBV,FNCSBV)
      ELSE
         ! Two-Parameter Continuation for IPS=4 or IPS=7.
         IF(ABS(ISW)==2)THEN
            IF(ITP==5)THEN
               ! ** Continuation of folds (BVP, start).
               CALL AUTOBV(AP,ICP,ICU,FNBL,BCBL,ICBL,STPNBL,FNCSBV)
            ELSE IF(ABS(ITP)/10==5) THEN
               ! ** Continuation of folds (BVP, restart).
               CALL AUTOBV(AP,ICP,ICU,FNBL,BCBL,ICBL,STPNBV,FNCSBV)
            ENDIF
         ENDIF
         IF((ITP==6.OR.(ABS(ITP)/10)==6)) THEN
            ! ** BP cont (BVP, start and restart) (by F. Dercole).
            CALL AUTOBV(AP,ICP,ICU,FNBBP,BCBBP,ICBBP,STPNBBP,FNCSBV)
         ENDIF
      ENDIF

      END SUBROUTINE AUTOBVP
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C        Subroutines for the Continuation of Folds for BVP.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FNBL(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
C Generates the equations for the 2-parameter continuation
C of folds (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
C Local
      DOUBLE PRECISION, ALLOCATABLE :: DFU(:,:),DFP(:,:),FF1(:),FF2(:)
      INTEGER NDM,NFPR,NFPX,NPAR,I,J
      DOUBLE PRECISION UU,UMX,EP,P
C
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPAR=AP%NPAR
C
C Generate the function.
C
      ALLOCATE(DFU(NDM,NDM),DFP(NDM,NPAR))
      CALL FFBL(AP,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
C
      IF(IJAC.EQ.0)THEN
        DEALLOCATE(DFU,DFP)
        RETURN
      ENDIF
C
C Generate the Jacobian.
C
      DFDU(1:NDM,1:NDM)=DFU(:,:)
      DFDU(1:NDM,NDM+1:NDIM)=0d0
      DFDU(NDM+1:NDIM,NDM+1:NDIM)=DFU(:,:)
      IF(IJAC==2)THEN
         NFPX=NFPR/2-1
         DO I=1,NFPR-NFPX
            DFDP(1:NDM,ICP(I))=DFP(:,ICP(I))
         ENDDO
         DO I=1,NFPX
            DFDP(1:NDM,ICP(NFPR-NFPX+I))=0d0
            DFDP(NDM+1:NDIM,ICP(NFPR-NFPX+I))=DFP(:,ICP(I+1))
         ENDDO
      ENDIF

      UMX=0.d0
      DO I=1,NDIM
        IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
      ENDDO
C
      EP=HMACH*(1+UMX)
C
      ALLOCATE(FF1(NDIM),FF2(NDIM))
      DO I=1,NDM
        UU=U(I)
        U(I)=UU-EP
        CALL FFBL(AP,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        U(I)=UU+EP
        CALL FFBL(AP,U,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
        U(I)=UU
        DO J=NDM+1,NDIM
          DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
        ENDDO
      ENDDO
C
      DEALLOCATE(FF2)
      IF (IJAC.EQ.1)THEN
        DEALLOCATE(DFU,DFP,FF1)
        RETURN
      ENDIF
C
      NFPX=NFPR/2-1
      DO I=1,NFPR-NFPX
        P=PAR(ICP(I))
        PAR(ICP(I))=P+EP
        CALL FFBL(AP,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
        DO J=1,NDIM
          DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
        ENDDO
        PAR(ICP(I))=P
      ENDDO
C
      DEALLOCATE(DFU,DFP,FF1)
      RETURN
      END SUBROUTINE FNBL
C
C     ---------- ----
      SUBROUTINE FFBL(AP,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDM
      DOUBLE PRECISION, INTENT(IN) :: UOLD(2*NDM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(2*NDM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(2*NDM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)

      INTEGER NFPR,NFPX,I,J
C
       NFPR=AP%NFPR
C
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
C
       NFPX=NFPR/2-1
       DO I=1,NDM
         F(NDM+I)=0.d0
         DO J=1,NDM
           F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
         ENDDO
         DO J=1,NFPX
            F(NDM+I)=F(NDM+I)
     *           + DFDP(I,ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
         ENDDO
       ENDDO
C
      RETURN
      END SUBROUTINE FFBL
C
C     ---------- ----
      SUBROUTINE BCBL(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
C Generates the boundary conditions for the 2-parameter continuation
C of folds (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
      DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UU1(:),UU2(:),FF1(:),FF2(:),
     *     DFU(:,:)
      INTEGER NDM,NBC0,NFPR,NPAR,I,J
      DOUBLE PRECISION UMX,EP,P
C
       NDM=AP%NDM
       NBC0=NBC/2
       NFPR=AP%NFPR
       NPAR=AP%NPAR
       ALLOCATE(DFU(NBC0,2*NDM+NPAR))
C
C Generate the function.
C
       CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,F,DFU)
C
       IF(IJAC.EQ.0)THEN
          DEALLOCATE(DFU)
          RETURN
       ENDIF
C
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NBC),FF2(NBC))
C
C Derivatives with respect to U0.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U0(J)
           UU2(J)=U0(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBL(AP,NDIM,PAR,ICP,NBC0,UU1,U1,FF1,DFU)
         CALL FBBL(AP,NDIM,PAR,ICP,NBC0,UU2,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
C Derivatives with respect to U1.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U1(J)
           UU2(J)=U1(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,UU1,FF1,DFU)
         CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,UU2,FF2,DFU)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(FF1,UU1,UU2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF2,DFU)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         P=PAR(ICP(I))
         PAR(ICP(I))=P+EP
         CALL FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF2(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
      DEALLOCATE(DFU,FF2)
      RETURN
      END SUBROUTINE BCBL
C
C     ---------- ----
      SUBROUTINE FBBL(AP,NDIM,PAR,ICP,NBC0,U0,U1,F,DBC)
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC0
      DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NBC0+AP%NDM+AP%NFPR/2-1)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC0,*)
C
      INTEGER NDM,NFPR,NFPX,I,J
C
       NDM=AP%NDM
       NFPR=AP%NFPR
C
       NFPX=NFPR/2-1
       CALL BCNI(AP,NDM,PAR,ICP,NBC0,U0,U1,F,2,DBC)
       DO I=1,NBC0
         F(NBC0+I)=0.d0
         DO J=1,NDM
           F(NBC0+I)=F(NBC0+I)+DBC(I,J)*U0(NDM+J)
           F(NBC0+I)=F(NBC0+I)+DBC(I,NDM+J)*U1(NDM+J)
         ENDDO
         DO J=1,NFPX
            F(NBC0+I)=F(NBC0+I)
     *       + DBC(I,NDIM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
         ENDDO
       ENDDO
C
      RETURN
      END SUBROUTINE FBBL
C
C     ---------- ----
      SUBROUTINE ICBL(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
C Generates integral conditions for the 2-parameter continuation of
C folds (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
C Local
      DOUBLE PRECISION, ALLOCATABLE :: FF1(:),FF2(:),DFU(:,:)
      INTEGER NDM,NNT0,NFPR,NPAR,I,J,NFPX
      DOUBLE PRECISION UMX,EP,P,UU
C
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPAR=AP%NPAR
C
C Note that PAR(NPAR) is used to keep the norm of the null vector
C
       F(NINT)=-PAR(NPAR)
       DO I=1,NDM
         F(NINT)=F(NINT)+U(NDM+I)*U(NDM+I)
       ENDDO
       IF(IJAC/=0)THEN
         DINT(NINT,NDM+1:NDIM)=2*U(NDM+1:NDIM)
         IF(IJAC/=1.AND.ICP(NFPR/2+1)==NPAR)THEN
           DINT(NINT,NDIM+NPAR)=-1
         ENDIF
       ENDIF

       IF(NINT==1)RETURN

       NNT0=(NINT-1)/2
       NFPX=NFPR/2-1
       ALLOCATE(DFU(NNT0,NDM+NPAR))
C
C Generate the function.
C
       CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,UPOLD,F,DFU)
       DO I=1,NFPX
         F(NINT)=F(NINT)+PAR(ICP(NFPR-NFPX+I))**2
       ENDDO
C
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
C
C Generate the Jacobian.
C
       DINT(1:NNT0,1:NDM)=DFU(1:NNT0,1:NDM)
       DINT(NNT0+1:NINT-1,NDM+1:NDIM)=DFU(1:NNT0,1:NDM)
       IF(IJAC/=1)THEN
         DO I=1,NFPR
           DINT(1:NNT0,NDIM+ICP(I))=DFU(1:NNT0,NDM+ICP(I))
           IF(I>=NFPR-NFPX+1)THEN
             DINT(NNT0+1:NINT-1,NDIM+ICP(I))=
     *             DFU(1:NNT0,NDM+ICP(I-NFPX-1))
             DINT(NINT,NDIM+ICP(I))=2*PAR(ICP(I))
           ENDIF
         ENDDO
       ENDIF
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
C
       EP=HMACH*(1+UMX)
C
       ALLOCATE(FF1(NINT),FF2(NINT))
       DO I=1,NDM
         UU=U(I)
         U(I)=UU-EP
         CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         U(I)=UU+EP
         CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,FF2,DFU)
         U(I)=UU
         DO J=NNT0+1,NINT-1
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
C
       DO I=1,NFPR-NFPX
         P=PAR(ICP(I))
         PAR(ICP(I))=P+EP
         CALL FIBL(AP,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         DO J=NNT0+1,NINT-1
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
      DEALLOCATE(FF1,DFU)
      RETURN
      END SUBROUTINE ICBL
C
C     ---------- ----
      SUBROUTINE FIBL(AP,PAR,ICP,NINT,NNT0,
     * U,UOLD,UDOT,UPOLD,F,DINT)
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NINT,NNT0
      DOUBLE PRECISION, INTENT(IN) :: UOLD(*),UDOT(*),UPOLD(*)
      DOUBLE PRECISION, INTENT(INOUT) :: U(*),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NNT0,*)
      INTEGER NDM,NFPR,NFPX,I,J
C
       NDM=AP%NDM
       NFPR=AP%NFPR
       NFPX=NFPR/2-1
C
         CALL ICNI(AP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,F,2,DINT)
         DO I=1,NNT0
           F(NNT0+I)=0.d0
           DO J=1,NDM
             F(NNT0+I)=F(NNT0+I)+DINT(I,J)*U(NDM+J)
           ENDDO
           DO J=1,NFPX
             F(NNT0+I)=F(NNT0+I)
     *         + DINT(I,NDM+ICP(1+J))*PAR(ICP(NFPR-NFPX+J))
           ENDDO
         ENDDO
C
      RETURN
      END SUBROUTINE FIBL
C
C     ---------- ------
      SUBROUTINE STPNBL(AP,PAR,ICP,NTSR,NCOLRS,
     * RLDOT,UPS,UDOTPS,TM,NODIR)
C
      USE IO
      USE MESH
C
C
C Generates starting data for the 2-parameter continuation of folds.
C (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
      INTEGER, INTENT(IN) :: ICP(*)
      INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
      INTEGER, INTENT(OUT) :: NODIR
      DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR),
     *     UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
C Local
      INTEGER, ALLOCATABLE :: ICPRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: RLDOTRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
      INTEGER NDIM,NCOL,NDM,NFPR,NFPR0,NFPX,NTST,NDIMRD,ITPRS,I,J,NPAR
C
       NDIM=AP%NDIM
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPAR=AP%NPAR
C
       ALLOCATE(ICPRS(NFPR),RLDOTRS(NFPR))
       ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),UDOTPSR(NDIM,0:NCOLRS*NTSR),
     *      TMR(0:NTSR))
       CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPSR,
     *      UDOTPSR,TMR,ITPRS,NDIM)
C
       NFPR0=NFPR/2
       DO I=1,NFPR0
         RLDOT(I)=RLDOTRS(I)
       ENDDO
       DEALLOCATE(ICPRS,RLDOTRS)
C
       DO J=0,NTSR*NCOLRS
          UPSR(NDM+1:NDIM,J)=0.d0
          UDOTPSR(NDM+1:NDIM,J)=0.d0
       ENDDO
C
       NFPX=NFPR/2-1
       IF(NFPX.GT.0) THEN
         DO I=1,NFPX
           PAR(ICP(NFPR0+1+I))=0.d0
           RLDOT(NFPR0+I+1)=0.d0
         ENDDO
       ENDIF
C Initialize the norm of the null vector
       PAR(NPAR)=0.
       RLDOT(NFPR0+1)=0.d0
C
       NODIR=0
       NTST=AP%NTST
       NCOL=AP%NCOL
       CALL ADAPT2(NTSR,NCOLRS,NDIM,NTST,NCOL,NDIM,
     *      TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
       DEALLOCATE(TMR,UPSR,UDOTPSR)
C
      RETURN
      END SUBROUTINE STPNBL
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   Subroutines for BP cont (BVPs) (by F. Dercole)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- -----
      SUBROUTINE FNBBP(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
C Generates the equations for the 2-parameter continuation
C of BP (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

C Local
      DOUBLE PRECISION, ALLOCATABLE :: DFU(:),DFP(:),UU1(:),UU2(:),
     *     FF1(:),FF2(:)
      INTEGER NDM,NFPR,NPAR,I,J
      DOUBLE PRECISION UMX,EP,P
C
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPAR=AP%NPAR
C
C Generate the function.
C
       ALLOCATE(DFU(NDM*NDM),DFP(NDM*NPAR))
       CALL FFBBP(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFU,DFP)
C
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU,DFP)
         RETURN
       ENDIF
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NDIM),FF2(NDIM))
C
C Generate the Jacobian.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
C
       EP=HMACH*(1+UMX)
C
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FFBBP(AP,NDIM,UU1,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         CALL FFBBP(AP,NDIM,UU2,UOLD,ICP,PAR,FF2,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDU(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(UU1,UU2,FF2)
       IF (IJAC.EQ.1)THEN
         DEALLOCATE(DFU,DFP,FF1)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         P=PAR(ICP(I))
         PAR(ICP(I))=P+EP
         CALL FFBBP(AP,NDIM,U,UOLD,ICP,PAR,FF1,NDM,DFU,DFP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
       DEALLOCATE(DFU,DFP,FF1)
C
      RETURN
      END SUBROUTINE FNBBP
C
C     ---------- -----
      SUBROUTINE FFBBP(AP,NDIM,U,UOLD,ICP,PAR,F,NDM,DFDU,DFDP)
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,NDM
      DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDM,NDM),DFDP(NDM,*)
C Local
      DOUBLE PRECISION, ALLOCATABLE :: FI(:),DINT(:,:)
      DOUBLE PRECISION DUM(1),UPOLD(NDM)
      INTEGER ISW,NBC,NINT,NBC0,NNT0,NFPX,I,J,NPARU
C
       ISW=AP%ISW
       NBC=AP%NBC
       NINT=AP%NINT
       NPARU=AP%NPAR-AP%NPARI ! real - internal
C
       IF(ISW.LT.0) THEN
C        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
C        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
C        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
C
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFDU,DFDP)
       IF(NNT0.GT.0) THEN
         ALLOCATE(FI(NNT0),DINT(NNT0,NDM))
         CALL FUNC(NDM,UOLD,ICP,PAR,0,UPOLD,DUM,DUM)
         CALL ICNI(AP,NDM,PAR,ICP,NNT0,U,UOLD,DUM,UPOLD,FI,1,DINT)
       ENDIF
C
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
C        ** Non-generic and/or start
         DO I=1,NDM
           F(I)=F(I)-PAR(NPARU+3*NFPX+NDM+1)*U(NDIM-NDM+I)
         ENDDO
       ENDIF
C
       IF(ISW.GT.0) THEN
C        ** restart 1 or 2
         DO I=1,NDM
           F(NDM+I)=0.d0
           DO J=1,NDM
             F(NDM+I)=F(NDM+I)-DFDU(J,I)*U(NDM+J)
           ENDDO
           DO J=1,NNT0
             F(NDM+I)=F(NDM+I)+DINT(J,I)*PAR(NPARU+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ELSE
C        ** start
         DO I=1,NDM
           F(NDM+I)=0.d0
           F(2*NDM+I)=0.d0
           F(3*NDM+I)=PAR(NPARU+3*NFPX+NDM+2)*U(NDM+I)+
     *       PAR(NPARU+3*NFPX+NDM+3)*U(2*NDM+I)
           DO J=1,NDM
             F(NDM+I)=F(NDM+I)+DFDU(I,J)*U(NDM+J)
             F(2*NDM+I)=F(2*NDM+I)+DFDU(I,J)*U(2*NDM+J)
             F(3*NDM+I)=F(3*NDM+I)-DFDU(J,I)*U(3*NDM+J)
           ENDDO
           DO J=1,NFPX
             F(NDM+I)=F(NDM+I)+DFDP(I,ICP(J))*PAR(NPARU+J)
             F(2*NDM+I)=F(2*NDM+I)+DFDP(I,ICP(J))*PAR(NPARU+NFPX+J)
           ENDDO
           DO J=1,NNT0
             F(3*NDM+I)=F(3*NDM+I)+DINT(J,I)*PAR(NPARU+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ENDIF
       IF(NNT0.GT.0) THEN
         DEALLOCATE(FI,DINT)
       ENDIF
C
      RETURN
      END SUBROUTINE FFBBP
C
C     ---------- -----
      SUBROUTINE BCBBP(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
C Generates the boundary conditions for the 2-parameter continuation
C of BP (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
      DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UU1(:),UU2(:),FF1(:),FF2(:),
     *     DFU(:,:)
      INTEGER ISW,NINT,NDM,NFPR,NPAR,NBC0,I,J
      DOUBLE PRECISION UMX,EP,P
C
       ISW=AP%ISW
       NINT=AP%NINT
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPAR=AP%NPAR
C
       IF(ISW.LT.0) THEN
C        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
       ELSE IF(ISW.EQ.2) THEN
C        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ELSE
C        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ENDIF
C
C Generate the function.
C
       ALLOCATE(DFU(NBC0,2*NDM+NPAR))
       CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,F,DFU)
C
       IF(IJAC.EQ.0)THEN
          DEALLOCATE(DFU)
          RETURN
       ENDIF
C
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NBC),FF2(NBC))
C
C Derivatives with respect to U0.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U0(I)).GT.UMX)UMX=DABS(U0(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U0(J)
           UU2(J)=U0(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,UU1,U1,FF1,DFU)
         CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,UU2,U1,FF2,DFU)
         DO J=1,NBC
           DBC(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
C Derivatives with respect to U1.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U1(I)).GT.UMX)UMX=DABS(U1(I))
       ENDDO
       EP=HMACH*(1+UMX)
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U1(J)
           UU2(J)=U1(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,UU1,FF1,DFU)
         CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,UU2,FF2,DFU)
         DO J=1,NBC
           DBC(J,NDIM+I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         P=PAR(ICP(I))
         PAR(ICP(I))=P+EP
         CALL FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FF1,DFU)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
       DEALLOCATE(FF1,DFU)
C
      RETURN
      END SUBROUTINE BCBBP
C
C     ---------- -----
      SUBROUTINE FBBBP(AP,NDIM,PAR,ICP,NBC,NBC0,U0,U1,FB,DBC)
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,NBC0
      DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: FB(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC0,*)

      INTEGER ISW,NINT,NDM,NNT0,NFPX,I,J,NPARU
C
       ISW=AP%ISW
       NINT=AP%NINT
       NDM=AP%NDM
       NPARU=AP%NPAR-AP%NPARI
C
       IF(ISW.LT.0) THEN
C        ** start
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
C        ** Non-generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
C        ** generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
C
       CALL BCNI(AP,NDM,PAR,ICP,NBC0,U0,U1,FB,2,DBC)
C
       IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
C        ** Non-generic and/or start
         DO I=1,NBC0
           FB(I)=FB(I)+PAR(NPARU+3*NFPX+NDM+1)*PAR(NPARU+2*NFPX+I)
         ENDDO
       ENDIF
C
       IF(ISW.GT.0) THEN
C        ** restart 1 or 2
         DO I=1,NDM
           FB(NBC0+I)=-U0(NDM+I)
           FB(NBC0+NDM+I)=U1(NDM+I)
           DO J=1,NBC0
             FB(NBC0+I)=FB(NBC0+I)+DBC(J,I)*PAR(NPARU+2*NFPX+J)
             FB(NBC0+NDM+I)=FB(NBC0+NDM+I)+
     *            DBC(J,NDM+I)*PAR(NPARU+2*NFPX+J)
           ENDDO
         ENDDO
         DO I=1,NFPX
           FB(NBC0+2*NDM+I)=PAR(NPARU+3*NFPX+NDM+3+I)
           DO J=1,NBC0
             FB(NBC0+2*NDM+I)=FB(NBC0+2*NDM+I)+
     *         DBC(J,2*NDM+ICP(I))*PAR(NPARU+2*NFPX+J)
           ENDDO
         ENDDO
       ELSE
C        ** start
         DO I=1,NBC0
           FB(NBC0+I)=0.d0
           FB(2*NBC0+I)=0.d0
           DO J=1,NDM
             FB(NBC0+I)=FB(NBC0+I)+DBC(I,J)*U0(NDM+J)
             FB(NBC0+I)=FB(NBC0+I)+DBC(I,NDM+J)*U1(NDM+J)
             FB(2*NBC0+I)=FB(2*NBC0+I)+DBC(I,J)*U0(2*NDM+J)
             FB(2*NBC0+I)=FB(2*NBC0+I)+DBC(I,NDM+J)*U1(2*NDM+J)
           ENDDO
           DO J=1,NFPX
             FB(NBC0+I)=FB(NBC0+I)+DBC(I,2*NDM+ICP(J))*PAR(NPARU+J)
             FB(2*NBC0+I)=FB(2*NBC0+I)+
     *         DBC(I,2*NDM+ICP(J))*PAR(NPARU+NFPX+J)
           ENDDO
         ENDDO
         DO I=1,NDM
           FB(3*NBC0+I)=-U0(3*NDM+I)
           FB(3*NBC0+NDM+I)=U1(3*NDM+I)
           DO J=1,NBC0
             FB(3*NBC0+I)=FB(3*NBC0+I)+DBC(J,I)*PAR(NPARU+2*NFPX+J)
             FB(3*NBC0+NDM+I)=FB(3*NBC0+NDM+I)+
     *         DBC(J,NDM+I)*PAR(NPARU+2*NFPX+J)
           ENDDO
         ENDDO
         DO I=1,NFPX
           FB(3*NBC0+2*NDM+I)=PAR(NPARU+3*NFPX+NDM+3+I)
           DO J=1,NBC0
             FB(3*NBC0+2*NDM+I)=FB(3*NBC0+2*NDM+I)+
     *         DBC(J,2*NDM+ICP(I))*PAR(NPARU+2*NFPX+J)
           ENDDO
         ENDDO
       ENDIF
C
      RETURN
      END SUBROUTINE FBBBP
C
C     ---------- -----
      SUBROUTINE ICBBP(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
C Generates integral conditions for the 2-parameter continuation
C of BP (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
C Local
      DOUBLE PRECISION, ALLOCATABLE :: UU1(:),UU2(:),FF1(:),FF2(:),
     *     DFU(:)
      INTEGER ISW,NBC,NDM,NFPR,NPAR,NNT0,I,J
      DOUBLE PRECISION UMX,EP,P
C
       ISW=AP%ISW
       NBC=AP%NBC
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPAR=AP%NPAR
C
       IF(ISW.LT.0) THEN
C        ** start
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
C        ** Non-generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
C        ** generic case
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
C
C Generate the function.
C
       IF(NNT0.GT.0) THEN
         ALLOCATE(DFU(NNT0*(NDM+NPAR)))
       ELSE
         ALLOCATE(DFU(1))
       ENDIF
       CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *   UPOLD,F,DFU)
C
       IF(IJAC.EQ.0)THEN
         DEALLOCATE(DFU)
         RETURN
       ENDIF
C
       ALLOCATE(UU1(NDIM),UU2(NDIM),FF1(NINT),FF2(NINT))
C
C Generate the Jacobian.
C
       UMX=0.d0
       DO I=1,NDIM
         IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
       ENDDO
C
       EP=HMACH*(1+UMX)
C
       DO I=1,NDIM
         DO J=1,NDIM
           UU1(J)=U(J)
           UU2(J)=U(J)
         ENDDO
         UU1(I)=UU1(I)-EP
         UU2(I)=UU2(I)+EP
         CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,UU1,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,UU2,UOLD,UDOT,
     *    UPOLD,FF2,DFU)
         DO J=1,NINT
           DINT(J,I)=(FF2(J)-FF1(J))/(2*EP)
         ENDDO
       ENDDO
C
       DEALLOCATE(UU1,UU2,FF2)
       IF(IJAC.EQ.1)THEN
         DEALLOCATE(FF1,DFU)
         RETURN
       ENDIF
C
       DO I=1,NFPR
         P=PAR(ICP(I))
         PAR(ICP(I))=P+EP
         CALL FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,U,UOLD,UDOT,
     *    UPOLD,FF1,DFU)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(FF1(J)-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
       DEALLOCATE(FF1,DFU)
C
      RETURN
      END SUBROUTINE ICBBP
C
C     ---------- -----
      SUBROUTINE FIBBP(AP,NDIM,PAR,ICP,NINT,NNT0,
     * U,UOLD,UDOT,UPOLD,FI,DINT)
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,NNT0
      DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: FI(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NNT0,*)
C
C Local
      DOUBLE PRECISION, ALLOCATABLE :: F(:),DFU(:,:),DFP(:,:)
      INTEGER ISW,NBC,NDM,NBC0,NFPX,NPAR,I,J,NPARU
C
       ISW=AP%ISW
       NBC=AP%NBC
       NDM=AP%NDM
       NPAR=AP%NPAR
       NPARU=NPAR-AP%NPARI
C
       IF(ISW.LT.0) THEN
C        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
       ELSE IF(ISW.EQ.2) THEN
C        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ELSE
C        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
C
       ALLOCATE(F(NDM),DFU(NDM,NDM),DFP(NDM,NPAR))
       CALL FUNI(AP,NDM,U,UOLD,ICP,PAR,2,F,DFU,DFP)
       IF(NNT0.GT.0) THEN
         CALL ICNI(AP,NDM,PAR,ICP,NNT0,U,UOLD,UDOT,UPOLD,FI,2,DINT)
C
         IF((ISW.EQ.2).OR.(ISW.LT.0)) THEN
C          ** Non-generic and/or start
           DO I=1,NNT0
             FI(I)=FI(I)+PAR(NPARU+3*NFPX+NDM+1)*
     *             PAR(NPARU+2*NFPX+NBC0+I)
           ENDDO
         ENDIF
       ENDIF
C
       IF(ISW.GT.0) THEN
C        ** restart 1 or 2
         DO I=1,NFPX
           FI(NNT0+I)=-PAR(NPARU+3*NFPX+NDM+3+I)
           DO J=1,NDM
             FI(NNT0+I)=FI(NNT0+I)-DFP(J,ICP(I))*U(NDM+J)
           ENDDO
           DO J=1,NNT0
             FI(NNT0+I)=FI(NNT0+I)+
     *         DINT(J,NDM+ICP(I))*PAR(NPARU+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ELSE
C        ** start
         DO I=1,NNT0
           FI(NNT0+I)=0.d0
           FI(2*NNT0+I)=0.d0
           DO J=1,NDM
             FI(NNT0+I)=FI(NNT0+I)+DINT(I,J)*U(NDM+J)
             FI(2*NNT0+I)=FI(2*NNT0+I)+DINT(I,J)*U(2*NDM+J)
           ENDDO
           DO J=1,NFPX
             FI(NNT0+I)=FI(NNT0+I)+DINT(I,NDM+ICP(J))*PAR(NPARU+J)
             FI(2*NNT0+I)=FI(2*NNT0+I)+
     *            DINT(I,NDM+ICP(J))*PAR(NPARU+NFPX+J)
           ENDDO
         ENDDO
         FI(3*NNT0+1)=-1.d0
         FI(3*NNT0+2)=-1.d0
         FI(3*NNT0+3)=0.d0
         FI(3*NNT0+4)=0.d0
         DO I=1,NDM
           FI(3*NNT0+1)=FI(3*NNT0+1)+U(NDM+I)*UOLD(NDM+I)
           FI(3*NNT0+2)=FI(3*NNT0+2)+U(2*NDM+I)*UOLD(2*NDM+I)
           FI(3*NNT0+3)=FI(3*NNT0+3)+U(NDM+I)*UOLD(2*NDM+I)
           FI(3*NNT0+4)=FI(3*NNT0+4)+U(2*NDM+I)*UOLD(NDM+I)
         ENDDO
         DO I=1,NFPX
           FI(3*NNT0+1)=FI(3*NNT0+1)+PAR(NPARU+I)**2
           FI(3*NNT0+2)=FI(3*NNT0+2)+PAR(NPARU+NFPX+I)**2
           FI(3*NNT0+3)=FI(3*NNT0+3)+PAR(NPARU+I)*PAR(NPARU+NFPX+I)
           FI(3*NNT0+4)=FI(3*NNT0+4)+PAR(NPARU+I)*PAR(NPARU+NFPX+I)
           FI(3*NNT0+4+I)=-PAR(NPARU+3*NFPX+NDM+3+I)+
     *       PAR(NPARU+3*NFPX+NDM+2)*PAR(NPARU+I)+
     *       PAR(NPARU+3*NFPX+NDM+3)*PAR(NPARU+NFPX+I)
           DO J=1,NDM
             FI(3*NNT0+4+I)=FI(3*NNT0+4+I)-DFP(J,ICP(I))*U(3*NDM+J)
           ENDDO
           DO J=1,NNT0
             FI(3*NNT0+4+I)=FI(3*NNT0+4+I)+DINT(J,NDM+ICP(I))*
     *         PAR(NPARU+2*NFPX+NBC0+J)
           ENDDO
         ENDDO
       ENDIF
       DEALLOCATE(F,DFU,DFP)
C
       FI(NINT)=-PAR(NPARU+3*NFPX+NDM)
       DO I=1,NDM
         FI(NINT)=FI(NINT)+U(NDIM-NDM+I)**2
       ENDDO
       DO I=1,NBC0+NNT0
         FI(NINT)=FI(NINT)+PAR(NPARU+2*NFPX+I)**2
       ENDDO
C
      RETURN
      END SUBROUTINE FIBBP
C
C     ---------- -------
      SUBROUTINE STPNBBP(AP,PAR,ICP,NTSR,NCOLRS,
     * RLDOT,UPS,UDOTPS,TM,NODIR)
C
      USE SOLVEBV
      USE IO
      USE MESH
C
C Generates starting data for the 2-parameter continuation
C of BP (BVP).
C
      TYPE(AUTOPARAMETERS), INTENT(INOUT) :: AP
      INTEGER, INTENT(IN) :: ICP(*)
      INTEGER, INTENT(INOUT) :: NTSR,NCOLRS
      INTEGER, INTENT(OUT) :: NODIR
      DOUBLE PRECISION, INTENT(OUT) :: PAR(*),RLDOT(AP%NFPR),
     *     UPS(AP%NDIM,0:*),UDOTPS(AP%NDIM,0:*),TM(0:*)
C Local
      INTEGER, ALLOCATABLE :: ICPRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: VPS(:,:),VDOTPS(:,:),RVDOT(:)
      DOUBLE PRECISION, ALLOCATABLE :: THU1(:),THL1(:)
      DOUBLE PRECISION, ALLOCATABLE :: P0(:,:),P1(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: U(:),RLDOTRS(:),RLCUR(:)
      DOUBLE PRECISION, ALLOCATABLE :: DTM(:),UPST(:,:),UDOTPST(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: VDOTPST(:,:),UPOLDPT(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: UPSR(:,:),UDOTPSR(:,:),TMR(:)
      INTEGER NDIM,NTST,NCOL,ISW,NBC,NINT,NFPR,NDIM3,I,J,IFST,NLLV
      INTEGER ITPRS,NDM,NBC0,NNT0,NFPX,NDIMRD,NPARU
      DOUBLE PRECISION DUM(1),DET,RDSZ
C
       NDIM=AP%NDIM
       NTST=AP%NTST
       NCOL=AP%NCOL
       ISW=AP%ISW
       NBC=AP%NBC
       NINT=AP%NINT
       NDM=AP%NDM
       NFPR=AP%NFPR
       NPARU=AP%NPAR-AP%NPARI
C
       NDIM3=GETNDIM3()
C
       IF(NDIM.EQ.NDIM3) THEN
C        ** restart 2
         CALL STPNBV(AP,PAR,ICP,NTSR,NCOLRS,RLDOT,
     *     UPS,UDOTPS,TM,NODIR)
         RETURN
       ENDIF
C
       ALLOCATE(UPSR(NDIM,0:NCOLRS*NTSR),
     *      UDOTPSR(NDIM,0:NCOLRS*NTSR),TMR(0:NTSR))
       IF(ISW.LT.0) THEN
C        ** start
         NBC0=(4*NBC-NINT-5*NDM+2)/15
         NNT0=(-NBC+4*NINT+5*NDM-23)/15
       ELSE IF(ISW.EQ.2) THEN
C        ** Non-generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ELSE
C        ** generic case
         NBC0=(2*NBC-NINT-3*NDM)/3
         NNT0=(-NBC+2*NINT+3*NDM-3)/3
       ENDIF
       NFPX=NBC0+NNT0-NDM+1
C
       ALLOCATE(ICPRS(NFPR),RLCUR(NFPR),RLDOTRS(NFPR))
       IF(ISW.LT.0) THEN
C
C Start
C
C        ** allocation
         ALLOCATE(UPST(NDM,0:NTSR*NCOLRS),UDOTPST(NDM,0:NTSR*NCOLRS))
         ALLOCATE(UPOLDPT(NDM,0:NTSR*NCOLRS))
         ALLOCATE(VDOTPST(NDM,0:NTSR*NCOLRS),RVDOT(NFPX))
         ALLOCATE(THU1(NDM),THL1(NFPX))
         ALLOCATE(P0(NDM,NDM),P1(NDM,NDM))
         ALLOCATE(U(NDM),DTM(NTSR))
C
C        ** read the std branch
         CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,UPST,
     *     UDOTPST,TMR,ITPRS,NDM)
C
         DO I=1,NTSR
           DTM(I)=TMR(I)-TMR(I-1)
         ENDDO
C
         DO I=1,NFPX
           RLCUR(I)=PAR(ICPRS(I))
         ENDDO
C
C Compute the second null vector
C
C        ** redefine IAP
         AP%NDIM=NDM
         AP%NTST=NTSR
         AP%NCOL=NCOLRS
         AP%NBC=NBC0
         AP%NINT=NNT0
         AP%NFPR=NFPX
C
C        ** compute UPOLDP
         IF(NNT0.GT.0) THEN
            DO J=0,NTSR*NCOLRS
               U(:)=UPST(:,J)
               CALL FUNI(AP,NDM,U,U,ICPRS,PAR,0,
     *              UPOLDPT(1,J),DUM,DUM)
            ENDDO
         ENDIF
C
C        ** unit weights
         THL1(1:NFPX)=1.d0
         THU1(1:NDM)=1.d0
C
C        ** call SOLVBV
         RDSZ=0.d0
         NLLV=1
         IFST=1
         CALL SOLVBV(IFST,AP,DET,PAR,ICPRS,FUNI,BCNI,ICNI,RDSZ,NLLV,
     *     RLCUR,RLCUR,RLDOTRS,NDM,UPST,UPST,UDOTPST,UPOLDPT,
     *     DTM,VDOTPST,RVDOT,P0,P1,THL1,THU1)
C
C        ** normalization
         CALL SCALEB(NTSR,NCOLRS,NDM,NFPX,UDOTPST,RLDOTRS,DTM,THL1,THU1)
         CALL SCALEB(NTSR,NCOLRS,NDM,NFPX,VDOTPST,RVDOT,DTM,THL1,THU1)
C
C        ** restore IAP
         AP%NDIM=NDIM
         AP%NTST=NTST
         AP%NCOL=NCOL
         AP%NBC=NBC
         AP%NINT=NINT
         AP%NFPR=NFPR

C        ** init UPS,PAR
         UPSR(1:NDM,:)=UPST(:,:)
         UPSR(NDM+1:2*NDM,:)=UDOTPST(:,:)
         UPSR(2*NDM+1:3*NDM,:)=VDOTPST(:,:)
         UPSR(3*NDM+1:4*NDM,:)=0.d0
         UDOTPSR(:,:)=0.d0
C
         DO I=1,NFPX
           PAR(NPARU+I)=RLDOTRS(I)
           PAR(NPARU+NFPX+I)=RVDOT(I)
           RLDOT(I)=0.d0
           RLDOT(NFPX+I+2)=0.d0
           RLDOT(2*NFPX+I+2)=0.d0
         ENDDO
C
C        ** init psi^*2,psi^*3
         DO I=1,NBC0+NNT0
           PAR(NPARU+2*NFPX+I)=0.d0
           RLDOT(3*NFPX+I+2)=0.d0
         ENDDO
C
C        ** init a,b,c1,c1,d
         PAR(NPARU+3*NFPX+NDM:NPARU+3*NFPX+NDM+3)=0.d0
         RLDOT(NFPX+1)=0.d0
         RLDOT(NFPX+2)=1.d0
         RLDOT(4*NFPX+NDM+2)=0.d0
         RLDOT(4*NFPX+NDM+3)=0.d0
         DO I=1,NFPX
           PAR(NPARU+3*NFPX+NDM+3+I)=0.d0
           RLDOT(4*NFPX+NDM+I+3)=0.d0
         ENDDO
C
         DEALLOCATE(UPST,UPOLDPT,UDOTPST,VDOTPST,RVDOT)
         DEALLOCATE(THU1,THL1)
         DEALLOCATE(P0,P1)
         DEALLOCATE(U,DTM)
C
         NODIR=0
C
       ELSE
C
C Restart 1
C
         ALLOCATE(VPS(2*NDIM,0:NTSR*NCOLRS),
     *         VDOTPS(2*NDIM,0:NTSR*NCOLRS))
C
C        ** read the std branch
         CALL READBV(AP,PAR,ICPRS,NTSR,NCOLRS,NDIMRD,RLDOTRS,VPS,
     *     VDOTPS,TMR,ITPRS,2*NDIM)
C
         UPSR(1:NDM,:)=VPS(1:NDM,:)
         UPSR(NDM+1:2*NDM,:)=VPS(3*NDM+1:4*NDM,:)
C
         DEALLOCATE(VPS,VDOTPS)
C
         NODIR=1
C
       ENDIF
       DEALLOCATE(ICPRS,RLDOTRS,RLCUR)
C
       CALL ADAPT2(NTSR,NCOLRS,NDIM,NTST,NCOL,NDIM,
     *      TMR,UPSR,UDOTPSR,TM,UPS,UDOTPS,.FALSE.)
       DEALLOCATE(TMR,UPSR,UDOTPSR)
      RETURN
      END SUBROUTINE STPNBBP
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C          Routines for Interface with User Supplied Routines
C  (To generate Jacobian by differencing, if not supplied analytically)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     ---------- ----
      SUBROUTINE FUNI(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)
C
C Interface subroutine to user supplied FUNC.
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)
C
      INTEGER JAC,I,J,NFPR,IJC
      DOUBLE PRECISION UMX,EP,UU,P
C
       JAC=AP%JAC
C
C Generate the function.
C
C
C if the user specified the Jacobian but not the
C parameter derivatives we do not generate the Jacobian here
C
       IF(JAC.EQ.0.AND.IJAC.NE.0)THEN
C
C Generate the Jacobian by differencing.
C
         UMX=0.d0
         DO I=1,NDIM
           IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
         ENDDO
C
         EP=HMACH*(1+UMX)
C
         DO I=1,NDIM
           UU=U(I)
           U(I)=UU-EP
           CALL FUNC(NDIM,U,ICP,PAR,0,F,DFDU,DFDP)
           U(I)=UU+EP
           CALL FUNC(NDIM,U,ICP,PAR,0,DFDU(1,I),DFDU,DFDP)
           U(I)=UU
           DO J=1,NDIM
             DFDU(J,I)=(DFDU(J,I)-F(J))/(2*EP)
           ENDDO
         ENDDO
C
       ENDIF
C
       IF(JAC==0.OR.IJAC==0)THEN
         IJC=0
       ELSEIF(JAC==1)THEN
         IJC=IJAC
       ELSE
         IJC=1
       ENDIF
       CALL FUNC(NDIM,U,ICP,PAR,IJC,F,DFDU,DFDP)
       IF(JAC==1.OR.IJAC/=2)RETURN
       NFPR=AP%NFPR
       DO I=1,NFPR
         P=PAR(ICP(I))
         EP=HMACH*( 1 +ABS(P) )
         PAR(ICP(I))=P+EP
         CALL FUNC(NDIM,U,ICP,PAR,0,DFDP(1,ICP(I)),DFDU,DFDP)
         DO J=1,NDIM
           DFDP(J,ICP(I))=(DFDP(J,ICP(I))-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
      RETURN
      END SUBROUTINE FUNI
C
C     ---------- ----
      SUBROUTINE BCNI(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)
C
C Interface subroutine to the user supplied BCND.
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
      DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
C Local
      DOUBLE PRECISION EP,UMX,UU,P
      INTEGER IJC,I,J,JAC,NFPR
C
       JAC=AP%JAC
C
C Generate the function.
C
       IF(JAC==0 .AND. IJAC/=0)THEN
C
C Generate the Jacobian by differencing.
C
          UMX=0.d0
          DO I=1,NDIM
             IF(ABS(U0(I)).GT.UMX)UMX=ABS(U0(I))
          ENDDO
C
          EP=HMACH*(1+UMX)
C
          DO I=1,NDIM
             UU=U0(I)
             U0(I)=UU-EP
             CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F,0,DBC)
             U0(I)=UU+EP
             CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,DBC(1,I),0,DBC)
             U0(I)=UU
             DO J=1,NBC
                DBC(J,I)=(DBC(J,I)-F(J))/(2*EP)
             ENDDO
          ENDDO
C
          UMX=0.d0
          DO I=1,NDIM
             IF(ABS(U1(I)).GT.UMX)UMX=ABS(U1(I))
          ENDDO
C
          EP=HMACH*(1+UMX)
C
          DO I=1,NDIM
             UU=U1(I)
             U1(I)=UU-EP
             CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F,0,DBC)
             U1(I)=UU+EP
             CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,DBC(1,NDIM+I),0,DBC)
             U1(I)=UU
             DO J=1,NBC
                DBC(J,NDIM+I)=(DBC(J,NDIM+I)-F(J))/(2*EP)
             ENDDO
          ENDDO
       ENDIF
C
       IF(JAC==0.OR.IJAC==0)THEN
         IJC=0
       ELSEIF(JAC==1)THEN
         IJC=IJAC
       ELSE
         IJC=1
       ENDIF
       CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJC,DBC)
       IF(JAC==1 .OR. IJAC/=2)RETURN
C
       NFPR=AP%NFPR
       DO I=1,NFPR
         P=PAR(ICP(I))
         EP=HMACH*( 1 +ABS(P))
         PAR(ICP(I))=P+EP
         CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,DBC(1,2*NDIM+ICP(I)),0,DBC)
         DO J=1,NBC
           DBC(J,2*NDIM+ICP(I))=(DBC(J,2*NDIM+ICP(I))-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
      RETURN
      END SUBROUTINE BCNI
C
C     ---------- ----
      SUBROUTINE ICNI(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     * F,IJAC,DINT)
C
C Interface subroutine to user supplied ICND.
C
      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)
C
      INTEGER JAC,I,J,NFPR,IJC
      DOUBLE PRECISION UMX,EP,UU,P
C
       JAC=AP%JAC
C
C Generate the integrand.
C
       IF(JAC==0 .AND. IJAC/=0)THEN
C
C Generate the Jacobian by differencing.
C
          UMX=0.d0
          DO I=1,NDIM
             IF(ABS(U(I)).GT.UMX)UMX=ABS(U(I))
          ENDDO
C
          EP=HMACH*(1+UMX)
C
          DO I=1,NDIM
             UU=U(I)
             U(I)=UU-EP
             CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,0,DINT)
             U(I)=UU+EP
             CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,DINT(1,I),0,
     *            DINT)
             U(I)=UU
             DO J=1,NINT
                DINT(J,I)=(DINT(J,I)-F(J))/(2*EP)
             ENDDO
          ENDDO
       ENDIF
C
       IF(JAC==0.OR.IJAC==0)THEN
         IJC=0
       ELSEIF(JAC==1)THEN
         IJC=IJAC
       ELSE
         IJC=1
       ENDIF
       CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJC,DINT)
       IF(JAC==1 .OR. IJAC/=2)RETURN
C
       NFPR=AP%NFPR
       DO I=1,NFPR
         P=PAR(ICP(I))
         EP=HMACH*( 1 +ABS(P) )
         PAR(ICP(I))=P+EP
         CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,
     *        DINT(1,NDIM+ICP(I)),0,DINT)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(DINT(J,NDIM+ICP(I))-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO
C
      RETURN
      END SUBROUTINE ICNI

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      END MODULE INTERFACES
