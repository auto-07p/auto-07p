!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
!     brc  :  A parabolic PDE (the Brusselator)
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! (Discretized in space by polynomial collocation at Chebyshev points)
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! NOTE: The values of the constants NE and NN are defined in the file
!       brc.inc. If they are changed then the equations-file brc.f must
!       be rewritten with an editor or with the GUI Write button.
!
!      NE  :  the dimension of the PDE system
!      NN  :  the number of Chebyshev collocation points in space 
!
! The AUTO-constant NDIM must be set equal to the value of NE*NN
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

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

      IMPLICIT NONE
      INCLUDE 'brc.inc'
      INTEGER, PARAMETER :: NP=NN+1
      DOUBLE PRECISION D2
      COMMON /BLPPDE/ D2(NN,0:NP)
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NN,NE),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

      INTEGER I
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

      IMPLICIT NONE
      INCLUDE 'brc.inc'
      INTEGER, PARAMETER :: NP=NN+1
      LOGICAL, SAVE :: ifrst = .TRUE.
      DOUBLE PRECISION D2
      COMMON /BLPPDE/ D2(NN,0:NP)
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NN,NE), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NN,NE)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION W(NE),FW(NE),DC(NE),U0(NE),U1(NE)
      INTEGER I,J,K

! Problem-independent initialization :
        IF(ifrst)THEN
          CALL GENCF(PAR)
          ifrst=.FALSE.
        ENDIF

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

      SUBROUTINE GENCF(PAR)
!     ---------- -----

      IMPLICIT NONE
      INCLUDE 'brc.inc'
      INTEGER, PARAMETER :: NP=NN+1, M=NN+2
      DOUBLE PRECISION D2
      COMMON /BLPPDE/ D2(NN,M)
      DOUBLE PRECISION X(M),XX(M,M),CC(M,M),RI(M,M),PAR(*),pi,C,DET
      INTEGER IR(M),IC(M),I,J,K

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
          DO J=1,M
            D2(I,J)=0.d0
            DO K=2,M-1
              D2(I,J)=D2(I,J)+CC(K+1,J)*K*(K-1)*X(I+1)**(K-2)
            ENDDO
          ENDDO
        ENDDO

      END SUBROUTINE GENCF

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
