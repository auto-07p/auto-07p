!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
!     brc  :  A parabolic PDE (the Brusselator)
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! (Discretized in space by polynomial collocation at Chebyshev points)
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! NOTE: The value of the constant NE is defined in the module brc below.
!
!      NE  :  the dimension of the PDE system
!
!      NN  :  the number of Chebyshev collocation points in space is
!             determined by the AUTO-constant NDIM:
!             NN = NDIM/NE
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      MODULE brc
        SAVE
        INTEGER, PARAMETER :: NE=2
        DOUBLE PRECISION, ALLOCATABLE :: D2(:,:)
      END MODULE brc

      SUBROUTINE FF(NE,U,PAR,F) 
!     ---------- -- 
!     Define the nonlinear term

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NE
      DOUBLE PRECISION, INTENT(IN) :: U(NE),PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NE)

      DOUBLE PRECISION X,Y,A,B

        X=U(1)
        Y=U(2)
        A=PAR(1)
        B=PAR(2)

        F(1)= X**2*Y - (B+1)*X + A
        F(2)=-X**2*Y + B*X

      END SUBROUTINE FF

      SUBROUTINE SETDC(NE,DC,PAR) 
!     ---------- ----- 
!     Set the diffusion constants (constant, or in terms of PAR)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NE
      DOUBLE PRECISION, INTENT(OUT) :: DC(NE)
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)

        DC(1)=PAR(3)/PAR(5)**2
        DC(2)=PAR(4)/PAR(5)**2

      END SUBROUTINE SETDC

      SUBROUTINE SETBC(NE,PAR,U0,U1) 
!     ---------- ----- 
! Set the boundary values (to be kept fixed in time)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NE
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: U0(NE),U1(NE)

      DOUBLE PRECISION A,B

        A=PAR(1)
        B=PAR(2)

        U0(1)=A
        U0(2)=B/A
        U1(1)=A
        U1(2)=B/A

      END SUBROUTINE SETBC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 
! Define the starting stationary solution on the spatial mesh

      USE brc
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM/NE,NE),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      INTEGER I,NN
      DOUBLE PRECISION A,B,Dx,Dy,RL

! Set the parameter values
        A=2.d0
        B=5.45d0
        Dx=0.008d0
        Dy=0.004d0
        RL=0.4

        PAR(1)=A
        PAR(2)=B
        PAR(3)=Dx
        PAR(4)=Dy
        PAR(5)=RL

! Set the starting solution at the Chebyshev collocation points
        NN=NDIM/NE
        DO I=1,NN
          U(I,1)=A
          U(I,2)=B/A
        ENDDO

      END SUBROUTINE STPNT
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!                Problem-independent subroutines
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      USE brc
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM/NE,NE), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM/NE,NE)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION W(NE),FW(NE),DC(NE),U0(NE),U1(NE)
      INTEGER I,J,K,NN,NP

! Problem-independent initialization :
        NN=NDIM/NE
        NP=NN+1

        CALL SETDC(NE,DC,PAR)
        CALL SETBC(NE,PAR,U0,U1)

        DO I=1,NN
          DO K=1,NE
            W(K)=U(I,K)
          ENDDO
          CALL FF(NE,W,PAR,FW)
          DO J=1,NE
            F(I,J)=FW(J) + DC(J)*(U0(J)*D2(I,0)+U1(J)*D2(I,NP))
            DO K=1,NN
              F(I,J)=F(I,J)+DC(J)*D2(I,K)*U(K,J)
            ENDDO
          ENDDO
        ENDDO

      END SUBROUTINE FUNC

      SUBROUTINE GENCF(PAR,NN)
!     ---------- -----

      USE brc
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NN
      DOUBLE PRECISION, INTENT(IN) :: PAR(*)

      DOUBLE PRECISION, ALLOCATABLE :: X(:),XX(:,:),CC(:,:),RI(:,:)
      INTEGER, ALLOCATABLE :: IR(:),IC(:)

      DOUBLE PRECISION pi,C,DET
      INTEGER I,J,K,M,NP

        NP=NN+1
        M=NN+2
        ALLOCATE(D2(NN,0:NP))
        ALLOCATE(X(M),XX(M,M),CC(M,0:NP),RI(M,M),IR(M),IC(M))

        pi=4*ATAN(1.d0)
        X(1)=0.d0
        DO K=2,NP
          C=COS( (2*K-3)*pi/(2*NN) )
          X(K)=(1+C)/2
        ENDDO
        X(M)=1.d0

        DO I=1,M
          DO J=1,M
            RI(I,J)=0.d0
            XX(I,J)=X(I)**(J-1)
          ENDDO
          RI(I,I)=1.d0
        ENDDO

        CALL GE(0,M,M,XX,M,M,CC,M,RI,IR,IC,DET) 

        DO I=1,NN
          DO J=0,NP
            D2(I,J)=0.d0
            DO K=2,M-1
              D2(I,J)=D2(I,J)+CC(K+1,J)*K*(K-1)*X(I+1)**(K-2)
            ENDDO
          ENDDO
        ENDDO
        DEALLOCATE(X,XX,CC,RI,IR,IC)

      END SUBROUTINE GENCF

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE PVLS(NDIM,U,PAR)
!     ---------- ----

      USE brc
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
      LOGICAL, SAVE :: ifrst = .TRUE.

! Problem-independent initialization :
      IF(ifrst)THEN
         CALL GENCF(PAR,NDIM/NE)
         ifrst=.FALSE.
      ENDIF

      END SUBROUTINE PVLS
