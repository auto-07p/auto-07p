C----------------------------------------------------------------------
C----------------------------------------------------------------------
C   aut.f :       Model AUTO-equations file
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
C     ---------- ----
C
C Evaluates the algebraic equations or ODE right hand side
C
C Input arguments :
C      NDIM   :   Dimension of the algebraic or ODE system 
C      U      :   State variables
C      ICP    :   Array indicating the free parameter(s)
C      PAR    :   Equation parameters
C
C Values to be returned :
C      F      :   Equation or ODE right hand side values
C
C Normally unused Jacobian arguments : IJAC, DFDU, DFDP (see manual)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*), F(NDIM), ICP(*)
C
       F(1)= ....
       F(2)= ....
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
C      NDIM   :   Dimension of the algebraic or ODE system 
C
C Values to be returned :
C      U      :   A starting solution vector
C      PAR    :   The corresponding equation-parameter values
C
C Note : For time- or space-dependent solutions this subroutine has
C        arguments (NDIM,U,PAR,T), where the scalar input parameter T
C        contains the varying time or space variable value.

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), PAR(*)
C
C Initialize the equation parameters
       PAR(1)= ....
       PAR(2)= ....
C
C Initialize the solution
       U(1)= ....
       U(2)= ....
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC)
C     ---------- ----
C
C Boundary Conditions
C
C Input arguments :
C      NDIM   :   Dimension of the ODE system 
C      PAR    :   Equation parameters
C      ICP    :   Array indicating the free parameter(s)
C      NBC    :   Number of boundary conditions
C      U0     :   State variable values at the left boundary
C      U1     :   State variable values at the right boundary
C
C Values to be returned :
C      FB     :   The values of the boundary condition functions 
C
C Normally unused Jacobian arguments : IJAC, DBC (see manual)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PAR(*), ICP(*), U0(NDIM), U1(NDIM), FB(NBC)
C
CXXX   FB(1)=
CXXX   FB(2)=
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE ICND(NDIM,PAR,ICP,NINT,U,UOLD,UDOT,UPOLD,FI,IJAC,DINT)
C     ---------- ----
C
C Integral Conditions
C
C Input arguments :
C      NDIM   :   Dimension of the ODE system 
C      PAR    :   Equation parameters
C      ICP    :   Array indicating the free parameter(s)
C      NINT   :   Number of integral conditions
C      U      :   Value of the vector function U at `time' t
C
C The following input arguments, which are normally not needed,
C correspond to the preceding point on the solution branch
C      UOLD   :   The state vector at 'time' t
C      UDOT   :   Derivative of UOLD with respect to arclength
C      UPOLD  :   Derivative of UOLD with respect to `time'
C
C Normally unused Jacobian arguments : IJAC, DINT
C
C Values to be returned :
C      FI     :   The value of the vector integrand 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM), UOLD(NDIM), UDOT(NDIM), UPOLD(NDIM)
      DIMENSION FI(NINT), ICP(*), PAR(*)
C
CXXX   FI(1)=
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
      SUBROUTINE FOPT(NDIM,U,ICP,PAR,IJAC,FS,DFDU,DFDP)
C     ---------- ----
C
C Defines the objective function for algebraic optimization problems
C
C Supplied variables :
C      NDIM   :   Dimension of the state equation
C      U      :   The state vector
C      ICP    :   Indices of the control parameters
C      PAR    :   The vector of control parameters
C
C Values to be returned :
C      FS      :   The value of the objective function
C
C Normally unused Jacobian argument : IJAC, DFDP
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),ICP(*),PAR(*),DFDU(NDIM),DFDP(*)
C
CXXX   FS=
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C 
      SUBROUTINE PVLS(NDIM,U,PAR)
C     ---------- ----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(NDIM),PAR(*)
C
C---------------------------------------------------------------------- 
C NOTE : 
C Parameters set in this subroutine should be considered as ``solution 
C measures'' and be used for output purposes only.
C 
C They should never be used as `true'' continuation parameters. 
C
C They may, however, be added as ``over-specified parameters'' in the 
C parameter list associated with the AUTO-Constant NICP, in order to 
C print their values on the screen and in the ``p.xxx file.
C
C They may also appear in the list associated with AUTO-Constant NUZR.
C
C---------------------------------------------------------------------- 
C For algebraic problems the argument U is, as usual, the state vector.
C For differential equations the argument U represents the approximate 
C solution on the entire interval [0,1]. In this case its values must 
C be accessed indirectly by calls to GETP, as illustrated below.
C---------------------------------------------------------------------- 
C
C Set PAR(2) equal to the L2-norm of U(1)
CXX       PAR(2)=GETP('NRM',1,U)
C
C Set PAR(3) equal to the minimum of U(2)
CXX       PAR(3)=GETP('MIN',2,U)
C
C Set PAR(4) equal to the value of U(2) at the left boundary.
CXX       PAR(4)=GETP('BV0',2,U)
C
C Set PAR(5) equal to the pseudo-arclength step size used.
CXX       PAR(5)=GETP('STP',1,U)
C
C---------------------------------------------------------------------- 
C The first argument of GETP may be one of the following:
C        'NRM' (L2-norm),     'MAX' (maximum),
C        'INT' (integral),    'BV0 (left boundary value),
C        'MIN' (minimum),     'BV1' (right boundary value).
C
C Also available are
C   'STP' (Pseudo-arclength step size used).
C   'FLD' (`Fold function', which vanishes at folds).
C   'BIF' (`Bifurcation function', which vanishes at singular points).
C   'HBF' (`Hopf function'; which vanishes at Hopf points).
C   'SPB' ( Function which vanishes at secondary periodic bifurcations).
C---------------------------------------------------------------------- 
C
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
