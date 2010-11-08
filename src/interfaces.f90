!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          Routines for Interface with User Supplied Routines
!  (To generate Jacobian by differencing, if not supplied analytically)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      MODULE INTERFACES

      USE AUTO_CONSTANTS, ONLY: AUTOPARAMETERS

      IMPLICIT NONE
      PRIVATE

      PUBLIC :: FUNI,BCNI,ICNI,PVLSI ! Interface subroutines

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

!     ---------- ----
      SUBROUTINE FUNI(AP,NDIM,U,UOLD,ICP,PAR,IJAC,F,DFDU,DFDP)

! Interface subroutine to user supplied FUNC.

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(*)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM),DFDP(NDIM,*)

      INTEGER JAC,I,J,NFPR,IJC
      DOUBLE PRECISION UMX,EP,UU,P

       JAC=AP%JAC

! Generate the function.


! if the user specified the Jacobian but not the
! parameter derivatives we do not generate the Jacobian here

       IF(JAC.EQ.0.AND.IJAC.NE.0)THEN

! Generate the Jacobian by differencing.

         UMX=0.d0
         DO I=1,NDIM
           IF(DABS(U(I)).GT.UMX)UMX=DABS(U(I))
         ENDDO

         EP=HMACH*(1+UMX)

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

       ENDIF

       IF(JAC==0.OR.IJAC==0)THEN
         IJC=0
       ELSE
         IJC=1
         IF(JAC==1)THEN
            IJC=IJAC
         ENDIF
         IF(AP%NDIM>NDIM)THEN
            ! zero initialize allocated matrices for extended systems
            DFDU(:,:)=0d0
            IF(JAC==1.AND.IJAC==2)THEN
               DO I=1,AP%NFPR
                  DFDP(:,ICP(I))=0d0
               ENDDO
            ENDIF
         ENDIF
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

      END SUBROUTINE FUNI

!     ---------- ----
      SUBROUTINE BCNI(AP,NDIM,PAR,ICP,NBC,U0,U1,F,IJAC,DBC)

! Interface subroutine to the user supplied BCND.

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM,ICP(*),NBC,IJAC
      DOUBLE PRECISION, INTENT(INOUT) :: U0(NDIM),U1(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NBC)
      DOUBLE PRECISION, INTENT(INOUT) :: DBC(NBC,*)
! Local
      DOUBLE PRECISION EP,UMX,UU,P
      INTEGER IJC,I,J,JAC,NFPR

       JAC=AP%JAC

! Generate the function.

       IF(JAC==0 .AND. IJAC/=0)THEN

! Generate the Jacobian by differencing.

          UMX=0.d0
          DO I=1,NDIM
             IF(ABS(U0(I)).GT.UMX)UMX=ABS(U0(I))
          ENDDO

          EP=HMACH*(1+UMX)

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

          UMX=0.d0
          DO I=1,NDIM
             IF(ABS(U1(I)).GT.UMX)UMX=ABS(U1(I))
          ENDDO

          EP=HMACH*(1+UMX)

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

       IF(JAC==0.OR.IJAC==0)THEN
         IJC=0
       ELSE
         IJC=1
         IF(JAC==1)THEN
            IJC=IJAC
         ENDIF
         IF(AP%NDIM>NDIM.OR.AP%NBC>NBC)THEN
            ! zero initialize allocated matrices for extended systems
            IF(JAC==1.AND.IJAC==2)THEN
               DO I=1,AP%NFPR
                  DBC(:,2*NDIM+ICP(I))=0d0
               ENDDO
            ENDIF
            DBC(:,:2*NDIM)=0d0
         ENDIF
       ENDIF
       CALL BCND(NDIM,PAR,ICP,NBC,U0,U1,F,IJC,DBC)
       IF(JAC==1 .OR. IJAC/=2)RETURN

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

      END SUBROUTINE BCNI

!     ---------- ----
      SUBROUTINE ICNI(AP,NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJAC,DINT)

! Interface subroutine to user supplied ICND.

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: ICP(*),NDIM,NINT,IJAC
      DOUBLE PRECISION, INTENT(IN) :: UOLD(NDIM),UDOT(NDIM),UPOLD(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NINT)
      DOUBLE PRECISION, INTENT(INOUT) :: DINT(NINT,*)

      INTEGER JAC,I,J,NFPR,IJC
      DOUBLE PRECISION UMX,EP,UU,P

       JAC=AP%JAC

! Generate the integrand.

       IF(JAC==0 .AND. IJAC/=0)THEN

! Generate the Jacobian by differencing.

          UMX=0.d0
          DO I=1,NDIM
             IF(ABS(U(I)).GT.UMX)UMX=ABS(U(I))
          ENDDO

          EP=HMACH*(1+UMX)

          DO I=1,NDIM
             UU=U(I)
             U(I)=UU-EP
             CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,0,DINT)
             U(I)=UU+EP
             CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,DINT(1,I),0,DINT)
             U(I)=UU
             DO J=1,NINT
                DINT(J,I)=(DINT(J,I)-F(J))/(2*EP)
             ENDDO
          ENDDO
       ENDIF

       IF(JAC==0.OR.IJAC==0)THEN
         IJC=0
       ELSE
         IJC=1
         IF(JAC==1)THEN
            IJC=IJAC
         ENDIF
         IF(AP%NDIM>NDIM.OR.AP%NINT>NINT)THEN
            ! zero initialize allocated matrices for extended systems
            IF(JAC==1.AND.IJAC==2)THEN
               DO I=1,AP%NFPR
                  DINT(:,NDIM+ICP(I))=0d0
               ENDDO
            ENDIF
            DINT(:,:NDIM)=0d0
         ENDIF
       ENDIF
       CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,F,IJC,DINT)
       IF(JAC==1 .OR. IJAC/=2)RETURN

       NFPR=AP%NFPR
       DO I=1,NFPR
         P=PAR(ICP(I))
         EP=HMACH*( 1 +ABS(P) )
         PAR(ICP(I))=P+EP
         CALL ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD, &
              DINT(1,NDIM+ICP(I)),0,DINT)
         DO J=1,NINT
           DINT(J,NDIM+ICP(I))=(DINT(J,NDIM+ICP(I))-F(J))/EP
         ENDDO
         PAR(ICP(I))=P
       ENDDO

      END SUBROUTINE ICNI

! ---------- -----
      SUBROUTINE PVLSI(AP,UPS,NDIM,PAR)

      USE AUTO_CONSTANTS, ONLY : NPARX
      USE SUPPORT, ONLY: AUTOSTOP

      TYPE(AUTOPARAMETERS), INTENT(IN) :: AP
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(IN) :: UPS(NDIM,0:*)
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)

      INTEGER NDM,i

      NDM=AP%NDM
      CALL PVLS(NDM,UPS,PAR)

      DO i=NPARX,AP%NPAR+1,-1
         IF(PAR(i)/=0)THEN
            WRITE(6,"(A,I4)")'NPAR should be at least ',i
            CALL AUTOSTOP()
         ENDIF
      ENDDO

      END SUBROUTINE PVLSI

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      END MODULE INTERFACES
