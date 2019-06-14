C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   ab :            The A --> B reaction 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
C Evaluates the algebraic equations or ODE right hand side
C
C Input arguments :
C      NDIM   :   Dimension of the ODE system 
C      U      :   State variables
C      ICP    :   Array indicating the free parameter(s)
C      PAR    :   Equation parameters
C
C Values to be returned :
C      F      :   ODE right hand side values
C
C Normally unused Jacobian arguments : IJAC, DFDU, DFDP (see manual)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*), F(NDIM), ICP(*)
C
       U1=U(1)
       U2=U(2)
       E=DEXP(U2)
C
       F(1)=-U1 + PAR(1)*(1-U1)*E
       F(2)=-U2 + PAR(1)*PAR(2)*(1-U1)*E - PAR(3)*U2
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE STPNT(NDIM,U,PAR)
C     ---------- -----
C
C Input arguments :
C      NDIM   :   Dimension of the ODE system 
C
C Values to be returned :
C      U      :   A starting solution vector
C      PAR    :   The corresponding equation-parameter values
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*)
C
C Initialize the equation parameters
       PAR(1)=0.
       PAR(2)=14.
       PAR(3)=2.
C
C Initialize the solution
       U(1)=0.
       U(2)=0.
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C The following subroutines are not used here,
C but they must be supplied as dummy routines
C
      SUBROUTINE BCND 
      RETURN 
      END 
C 
      SUBROUTINE ICND 
      RETURN 
      END 
C 
      SUBROUTINE FOPT 
      RETURN 
      END 
C 
      SUBROUTINE PVLS
      RETURN 
      END 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
