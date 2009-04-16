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
! 
      SUBROUTINE FF(NE,U,PAR,F) 
!     ---------- -- 
!     Define the nonlinear term
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NE),F(NE),PAR(*)
! 
        X=U(1)
        Y=U(2)
        A=PAR(1)
        B=PAR(2)
!
        F(1)= X**2*Y - (B+1)*X + A
        F(2)=-X**2*Y + B*X
! 
      RETURN 
      END 
!
      SUBROUTINE SETDC(NE,DC,PAR) 
!     ---------- ----- 
!     Set the diffusion constants (constant, or in terms of PAR)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION DC(NE),PAR(*)
! 
        DC(1)=PAR(3)/PAR(5)**2
        DC(2)=PAR(4)/PAR(5)**2
! 
      RETURN 
      END 
!
      SUBROUTINE SETBC(NE,PAR,U0,U1) 
!     ---------- ----- 
! Set the boundary values (to be kept fixed in time)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),U0(NE),U1(NE)
!
        A=PAR(1)
        B=PAR(2)
!
        U0(1)=A
        U0(2)=B/A
        U1(1)=A
        U1(2)=B/A
!
      RETURN 
      END 
! 
      SUBROUTINE STPNT(NDIM,U,PAR) 
!     ---------- ----- 
! Define the starting stationary solution on the spatial mesh
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'brc.inc'
      PARAMETER ( NP=NN+1 )
      COMMON /BLPPDE/ D2(NN,0:NP)
      DIMENSION U(NN,NE),PAR(*)
!
! Set the parameter values
        A=2.d0
        B=5.45d0
        Dx=0.008d0
        Dy=0.004d0
        RL=0.4
!
        PAR(1)=A
        PAR(2)=B
        PAR(3)=Dx
        PAR(4)=Dy
        PAR(5)=RL
!
! Set the starting solution at the Chebyshev collocation points
        DO 1 I=1,NN
          U(I,1)=A
          U(I,2)=B/A
 1      CONTINUE
! 
      RETURN 
      END 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!                Problem-independent subroutines
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 
! 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'brc.inc'
      PARAMETER ( NP=NN+1 )
      COMMON /BLPPDE/ D2(NN,0:NP)
      COMMON /BLPPFR/ ifrst
      DIMENSION U(NN,NE),F(NN,NE),PAR(*)
      DIMENSION W(NE),FW(NE),DC(NE),U0(NE),U1(NE)
!
! Problem-independent initialization :
        IF(ifrst.NE.1234)THEN
          CALL GENCF(PAR)
          ifrst=1234
        ENDIF
!
        CALL SETDC(NE,DC,PAR)
        CALL SETBC(NE,PAR,U0,U1)
!
        DO 4 I=1,NN
          DO 1 K=1,NE
            W(K)=U(I,K)
 1        CONTINUE
          CALL FF(NE,W,PAR,FW)
          DO 3 J=1,NE
            F(I,J)=FW(J) + DC(J)*(U0(J)*D2(I,0)+U1(J)*D2(I,NP))
            DO 2 K=1,NN
              F(I,J)=F(I,J)+DC(J)*D2(I,K)*U(K,J)
 2          CONTINUE
 3        CONTINUE
 4      CONTINUE
! 
      RETURN 
      END 
!
      SUBROUTINE GENCF(PAR)
!     ---------- -----
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      INCLUDE 'brc.inc'
      PARAMETER ( NP=NN+1, M=NN+2)
      COMMON /BLPPDE/ D2(NN,M)
      DIMENSION X(M),XX(M,M),CC(M,M),RI(M,M),PAR(*)
      DIMENSION IR(M),IC(M)
!
        pi=4*DATAN(1.d0)
        X(1)=0.d0
        DO 1 K=2,NP
          C=COS( (2*K-3)*pi/(2*NN) )
          X(K)=(1+C)/2
 1      CONTINUE
        X(M)=1.d0
!
        DO 3 I=1,M
          DO 2 J=1,M
            RI(I,J)=0.d0
            XX(I,J)=X(I)**(J-1)
 2        CONTINUE
          RI(I,I)=1.d0
 3      CONTINUE
!
        CALL GE(0,M,M,XX,M,M,CC,M,RI,IR,IC,DET) 
!  
        DO 6 I=1,NN
          DO 5 J=1,M
            D2(I,J)=0.d0
            DO 4 K=2,M-1
              D2(I,J)=D2(I,J)+CC(K+1,J)*K*(K-1)*X(I+1)**(K-2)
 4          CONTINUE
 5        CONTINUE
 6      CONTINUE 
!
      RETURN
      END
! 
      SUBROUTINE BCND 
      RETURN 
      END 
! 
      SUBROUTINE ICND 
      RETURN 
      END 
! 
      SUBROUTINE FOPT 
      RETURN 
      END 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! 
      SUBROUTINE PVLS
      RETURN 
      END 
