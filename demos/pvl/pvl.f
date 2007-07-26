C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C   pvl :  Setting functional parameter values (for Bratu's equation)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C 
      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*),F(NDIM)
C 
       E=EXP(U(1)) 
       F(1)=U(2) 
       F(2)=-PAR(1)*E 
C 
      RETURN 
      END 
C 
      SUBROUTINE STPNT(NDIM,U,PAR,T) 
C     ---------- ----- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION U(NDIM),PAR(*) 
C
       PAR(1)=0 
       U(1)=0.0 
       U(2)=0.0 
C 
      RETURN 
      END 
C 
      SUBROUTINE BCND(NDIM,PAR,ICP,NBC,U0,U1,FB,IJAC,DBC) 
C     ---------- ---- 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      DIMENSION PAR(*),ICP(*),U0(NDIM),U1(NDIM),FB(NBC)
C 
       FB(1)=U0(1) 
       FB(2)=U1(1) 
C 
      RETURN 
      END 
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
C They may also appear in the list associated with AUTO-constant NUZR.
C
C---------------------------------------------------------------------- 
C For algebraic problems the argument U is, as usual, the state vector.
C For differential equations the argument U represents the approximate 
C solution on the entire interval [0,1]. In this case its values must 
C be accessed indirectly by calls to GETP, as illustrated below.
C---------------------------------------------------------------------- 
C
C Set PAR(2) equal to the L2-norm of U(1)
       PAR(2)=GETP('NRM',1,U)
C
C Set PAR(3) equal to the minimum of U(2)
       PAR(3)=GETP('MIN',2,U)
C
C Set PAR(4) equal to the value of U(2) at the left boundary.
       PAR(4)=GETP('BV0',2,U)
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
C 
      SUBROUTINE ICND
      RETURN 
      END 
C
      SUBROUTINE FOPT 
      RETURN 
      END 
