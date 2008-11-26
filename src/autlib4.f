C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    Floquet Multiplier Computation (Tom Fairgrieve, U. of Toronto)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  References:
C  T. F. Fairgrieve, PhD Thesis, University of Toronto, 1994.

C  T. F. Fairgrieve, A. D. Jepson, O.K. Floquet multipliers,
C  SIAM J. Numer. Anal. 28. No. 5, 1991, 1446-1462.
C
C  Please inform Tom Fairgrieve (tff@na.utoronto.ca) of any 
C  modifications to or errors in these routines.
C  Mailing Address: T.F. Fairgrieve, Department of Computer Science,
C  University of Toronto, Toronto, Ontario, CANADA  M5S 1A4C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  Routines included in this file:
C
C  subroutine flowkm : new routine to compute floquet multipliers
C  subroutine dhhpr  : compute a Householder matrix
C  subroutine dhhap  : appy a Householder matrix
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  Required library routines (included in the file eispack.f) :
C
C  subroutine qzhes  : QZ reduction to Hessenberg form             (EISPACK)
C  subroutine qzit   : QZ reduction to quasi-upper triangular form (EISPACK)
C  subroutine qzval  : QZ calculation of eigenvalues               (EISPACK)
C  function   epslon : machine constant routine                    (EISPACK)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  function   dnrm2  : compute l2-norm of a vector                 (BLAS-1)
C  function   ddot   : dot product of two vectors                  (BLAS-1)
C  subroutine dscal  : scale a vector by a constant                (BLAS-1)
C  function   idamax : find index of element with max abs value    (BLAS-1)
C  subroutine daxpy  : constant times a vector plus a vector       (BLAS-1)
C  subroutine drot   : apply a plane rotation                      (BLAS-1)
C  subroutine dswap  : swap two vectors                            (BLAS-1)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  subroutine xerbla : BLAS error handling routine                 (BLAS-2)
C  function   lsame  : compare character strings                   (BLAS-2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  subroutine dgemm  : matrix-matrix multiply                      (BLAS-3)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  subroutines ezsvd, ndrotg, ndsvd, prse, sig22, sigmin, sndrtg :
C                      Demmel-Kahan svd routines 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      MODULE FLOQUET

      IMPLICIT NONE

      PRIVATE

      PUBLIC :: FLOWKM

      CONTAINS

      SUBROUTINE FLOWKM (NDIM, P0, P1, IID, EV)
C
C  Subroutine to compute Floquet multipliers via the "deflated circuit 
C  pencil" method. This routine is called by the AUTO routine FNSPBV
C
C  Parameter declarations:
C
      INTEGER           NDIM, IID
      DOUBLE PRECISION  P0(NDIM, NDIM), P1(NDIM, NDIM)
      COMPLEX(KIND(1.0D0)) EV(*)
C
C  Local declarations:
C
C
C  Maximum order of problem.  **** (To avoid changing too many AUTO'86
C  routines, I need to declare some local array storage here )
C
      INTEGER           LEFT, RIGHT
      PARAMETER         (LEFT = 1, RIGHT = 2)
C
      INTEGER           I, J, NDIMM1
      DOUBLE PRECISION  X(:),BETA,V(:),C0(:,:),C1(:,:),RWORK(:,:),
     &                  NRMC0X, NRMC1X, CONST
      LOGICAL           INFEV
C
C  storage for SVD computations
C
      INTEGER           SVDINF, SVDLWRK
      DOUBLE PRECISION  SVDS(:), SVDE(:), SVDWRK(:), SVDV(:,:)
      ALLOCATABLE       C0,C1,RWORK,X,V,SVDS,SVDE,SVDWRK,SVDV
C  compute right singular vectors only
      CHARACTER(1), PARAMETER :: SVDJOBA = 'G',
     &     SVDJOBU = 'N', SVDJOBV = 'V'
C
C  storage for generalized eigenvalue computations
C
      INTEGER           QZIERR
      DOUBLE PRECISION  QZZ(1, 1), QZEPS1, QZALFR(:),
     &                  QZALFI(:), QZBETA(:)
      ALLOCATABLE       QZALFR,QZALFI,QZBETA
      LOGICAL           QZMATZ
C  don't want to accumulate the transforms --- vectors not needed
      PARAMETER         (QZMATZ = .FALSE.)
      PARAMETER         (QZEPS1 = 0.0D0)
C
C  BLAS routines
C
      DOUBLE PRECISION  DNRM2
      EXTERNAL          DNRM2, DGEMM
C
C  routines from LAPACK
C
      EXTERNAL          DGESVJ, DGGEV
C
C  builtin F77 functions
C
      INTRINSIC         MAX, ABS, CMPLX
C
      SVDLWRK = MAX(6,2*NDIM)
      ALLOCATE(C0(NDIM,NDIM),C1(NDIM,NDIM),RWORK(NDIM,NDIM))
      ALLOCATE(SVDE(NDIM),SVDS(NDIM+1),SVDV(NDIM,NDIM),V(NDIM),X(NDIM))
      ALLOCATE(QZALFI(NDIM),QZBETA(NDIM),QZALFR(NDIM),SVDWRK(SVDLWRK))
C
C Change sign of P1 so that we get the sign of the multipliers right.
C
      C0=P0
      C1=-P1
C
C  Print the undeflated circuit pencil (C0, C1).
C
       IF(IID.GT.4) THEN
           WRITE(9,101)
           WRITE(9,102)
           DO I=1,NDIM
               WRITE(9,104)(C0(I,J),J=1,NDIM)
           ENDDO
           WRITE(9,103)
           DO I=1,NDIM
               WRITE(9,104)(C1(I,J),J=1,NDIM)
           ENDDO
       END IF
C
C  PART I:
C  =======
C
C  Deflate the Floquet multiplier at +1.0 so that the deflated 
C  circuit pencil is not defective at periodic branch turning points.
C
C  The matrix (C0 - C1) should be (nearly) singular.  Find an approximation 
C  to the right null vector (call it X).  This will be our approximation 
C  to the eigenvector corresponding to the fixed multiplier at +1.0.  
C  
C  There are many ways to get this approximation.  We could use
C    1) p'(0) = f(p(0))
C    2) AUTO'86 routine NLVC applied to C0-C1
C    3) the right singular vector corresponding to the smallest 
C       singular value of C0-C1
C
C  I've chosen option 3) because it should introduce as little roundoff
C  error as possible.  Although it is more expensive, this is insignificant
C  relative to the rest of the AUTO computations. Also, the SVD does give a
C  version of the Householder matrix which we would have to compute
C  anyways.  But note that it gives V = ( X perp | X ) and not (X | Xperp),
C  which the Householder routine would give.  This will permute the deflated
C  circuit pencil, so that the part to be deflated is in the last column,
C  not it the first column, as was shown in the paper.
C
      RWORK = C0 - C1
C
      CALL DGESVJ( SVDJOBA, SVDJOBU, SVDJOBV, NDIM, NDIM, RWORK, NDIM,
     &     SVDS, 0, SVDV, NDIM, SVDWRK, SVDLWRK, SVDINF )
      IF (SVDINF .NE. 0) WRITE (9, 901) SVDINF
C
C  Apply a Householder matrix (call it H1) based on the null vector
C  to (C0, C1) from the right.  H1 = SVDV = ( Xperp | X ), where X
C  is the null vector.
C
      CALL DGEMM ( 'n', 'n', NDIM, NDIM, NDIM, 1.0d0, C0, NDIM, 
     &             SVDV, NDIM, 0.0d0, RWORK, NDIM )
      C0 = RWORK
      CALL DGEMM ( 'n', 'n', NDIM, NDIM, NDIM, 1.0d0, C1, NDIM, 
     &             SVDV, NDIM, 0.0d0, RWORK, NDIM )
      C1 = RWORK
C
C  Apply a Householder matrix (call it H2) based on 
C  (C0*X/||C0*X|| + C1*X/||C1*X||) / 2 
C  to (C0*H1, C1*H1) from the left.
C
      NRMC0X = DNRM2( NDIM, C0(1,NDIM), 1)
      NRMC1X = DNRM2( NDIM, C1(1,NDIM), 1)
      DO I = 1, NDIM
          X(I) = ((C0(I,NDIM)/NRMC0X)+(C1(I,NDIM)/NRMC1X)) / 2.0D0 
      ENDDO
      CALL DHHPR ( 1, NDIM, NDIM, X, 1, BETA, V )
      CALL DHHAP ( 1, NDIM, NDIM, NDIM, BETA, V, LEFT, C0, NDIM)
      CALL DHHAP ( 1, NDIM, NDIM, NDIM, BETA, V, LEFT, C1, NDIM)
C
C  Rescale so that (H2^T)*C0*(H1)(1,NDIM) ~= (H2^T)*C1*(H1)(1,NDIM) ~= 1.0
C
      CONST = MAX(ABS(C0(1,NDIM)), ABS(C1(1,NDIM)))
      DO J = 1, NDIM
          DO I = 1, NDIM
              C0(I,J) = C0(I,J) / CONST
              C1(I,J) = C1(I,J) / CONST
          ENDDO
      ENDDO
C
C  Finished the deflation process! Print the deflated circuit pencil.
C
       IF(IID.GT.4) THEN
           WRITE(9,105)
           WRITE(9,106)
           DO I=1,NDIM
               WRITE(9,104)(C0(I,J),J=1,NDIM)
           ENDDO
           WRITE(9,107)
           DO I=1,NDIM
               WRITE(9,104)(C1(I,J),J=1,NDIM)
            ENDDO
       END IF
C
C  At this point we have
C
C     (C0Bar, C1Bar) 
C ::= (H2^T)*(C0, C1)*(H1).
C
C     (( B0^T     | Beta0  )  ( B1^T     | Beta1  ))  1
C   = (( ----------------- ), ( ----------------- ))
C     (( C0BarDef | Delta0 )  ( C1BarDef | Delta1 )) NDIM-1
C
C         NDIM-1      1          NDIM-1      1
C
C  and approximations to the Floquet multipliers are 
C  (Beta0/Beta1) union the eigenvalues of the deflated pencil 
C  (C0BarDef, C1BarDef).
C
C  PART II:
C  ========
C
C  Compute the eigenvalues of the deflated circuit pencil 
C  (C0BarDef, C1BarDef)
C  by using the QZ routines from EISPACK.
C
      NDIMM1 = NDIM - 1
C
C  compute the eigenvalues for the generalized eigenvalue problem
C
      CALL DGGEV('N', 'N', NDIMM1, C0(2,1), NDIM, C1(2,1), NDIM, QZALFR,
     &     QZALFI, QZBETA, QZZ, 1, QZZ, 1, SVDWRK, -1, QZIERR)
      SVDLWRK = SVDWRK(1)
      DEALLOCATE(SVDWRK)
      ALLOCATE(SVDWRK(SVDLWRK))
      CALL DGGEV('N', 'N', NDIMM1, C0(2,1), NDIM, C1(2,1), NDIM, QZALFR,
     &     QZALFI, QZBETA, QZZ, 1, QZZ, 1, SVDWRK, SVDLWRK, QZIERR)
      IF (QZIERR .NE. 0) WRITE (9, 902) QZIERR
C
C  Pack the eigenvalues into complex form.
C
      EV(1) = CMPLX( C0(1,NDIM) / C1(1,NDIM), 0.0D0, KIND(1.0D0) )
      INFEV = .FALSE.
      DO J = 1, NDIMM1
          IF (QZBETA(J) .NE. 0.0D0) THEN
              EV(J+1) = CMPLX( QZALFR(J)/QZBETA(J), 
     &                         QZALFI(J)/QZBETA(J), KIND(1.0D0) )
          ELSE
              EV(J+1) = ( 1.0D+30, 1.0D+30 )
              INFEV = .TRUE.
          END IF
      ENDDO
      IF (INFEV) WRITE (9, 903) 
C
C  Done!
C
      DEALLOCATE(C0,C1,RWORK,X,V,SVDS,SVDE,SVDWRK,SVDV,
     *     QZALFR,QZALFI,QZBETA)
      RETURN
C
C  Format statements
C
 101  FORMAT(' Undeflated circuit pencil (C0, C1) ')
 102  FORMAT('   C0 : ')
 103  FORMAT('   C1 : ')
 104  FORMAT(1X,6E23.16)
 105  FORMAT(' Deflated cicuit pencil (H2^T)*(C0, C1)*(H1) ')
 106  FORMAT('   (H2^T)*C0*(H1) : ')
 107  FORMAT('   (H2^T)*C1*(H1) : ')
 901  FORMAT(' NOTE : Warning from subroutine FLOWKM : ',
     &     /,'        SVD routine returned SVDINF = ', I4,
     &     /,'        Floquet multiplier calculations may be wrong')
 902  FORMAT(' NOTE : Warning from subroutine FLOWKM : ',
     &     /,'        QZ routine returned QZIERR = ', I4,
     &     /,'        Floquet multiplier calculations may be wrong ')
 903  FORMAT(' NOTE : Warning from subroutine FLOWKM : ',
     &     /,'        Infinite Floquet multiplier represented by ',
     &     /,'        CMPLX( 1.0D+30, 1.0D+30 )')
C
      END SUBROUTINE FLOWKM
C
C **************************
C *  Householder routines  *
C **************************
C
C
C  Subroutines for performing Householder plane rotations.
C
C  DHHPR: for computing Householder transformations and
C  DHHAP: for applying them.
C
C  Ref: Golub and van Loan, Matrix Calcualtions, 
C       First Edition, Pages 38-43
C
      SUBROUTINE DHHPR ( K, J, N, X, INCX, BETA, V )
C     .. Scalar Arguments ..
      INTEGER            J, K, N, INCX
      DOUBLE PRECISION, INTENT(OUT) :: BETA
C     .. Array Arguments ..
      DOUBLE PRECISION   X( * ), V( * )
C     ..
C
C  Purpose
C  =======
C
C  DHHPR  computes a Householder Plane Rotation (G&vL Alg. 3.3-1)
C  defined by v and beta.
C  (I - beta v vt) * x is such that x_i = 0 for i=k+1 to j.
C
C  Parameters
C  ==========
C
C  K      - INTEGER.
C           On entry, K specifies that the K+1st entry of X 
C           be the first to be zeroed.
C           K must be at least one.
C           Unchanged on exit.
C
C  J      - INTEGER.
C           On entry, J specifies the last entry of X to be zeroed.
C           J must be >= K and <= N.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the (logical) length of X.
C           Unchanged on exit.
C
C  X      - DOUBLE PRECISION array of DIMENSION at least
C           ( 1 + ( N - 1 )*abs( INCX ) ). 
C           On entry, X specifies the vector to be (partially) zeroed.
C           Unchanged on exit.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must be > zero. If X represents part of a matrix, 
C           then use INCX = 1 if a column vector is being zeroed and 
C           INCX = NDIM if a row vector is being zeroed.
C           Unchanged on exit.
C
C  BETA   - DOUBLE PRECISION.
C           BETA specifies the scalar beta. (see pg. 40 of G and v.L.)
C
C  V      - DOUBLE PRECISION array of DIMENSION at least n.
C           Is updated to be the appropriate Householder vector for
C           the given problem. (Note: space for the implicit zeroes is
C           assumed to be present. Will save on time for index translation.)
C
C  -- Written by Tom Fairgrieve,
C                Department of Computer Science,
C                University of Toronto,
C                Toronto, Ontario CANADA  M5S 1A4 
C
C     .. Local Scalars ..
      DOUBLE PRECISION   ALPHA, M
      INTEGER            I, L, JMKP1, IEND, ISTART
C     .. External Functions from BLAS ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DNRM2
      EXTERNAL           IDAMAX, DNRM2
C     .. External Subroutines from BLAS ..
      EXTERNAL           DSCAL
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
C
C     .. Executable Statements ..
C
C  Test the input parameters.
C
      IF ((K .LT. 1) .OR. (K .GT. J)) THEN
          WRITE (9,*) 'Domain error for K in DHHPR'
          STOP
      END IF
      IF (J .GT. N) THEN
          WRITE (9,*) 'Domain error for J in DHHPR'
          STOP
      END IF
      IF (INCX .LT. 1) THEN
          WRITE (9,*) 'Domain error for INCX in DHHPR'
          STOP
      END IF
C
C  Number of potential non-zero elements in V.
C
      JMKP1 = J - K + 1
C
C  Find M := max{ |x_k|, ... , |x_j| }
C
      M = ABS( X( IDAMAX( JMKP1, X(K), INCX ) ) )
C
C  alpha := 0
C  For i = k to j
C      v_i = x_i / m
C      alpha := alpha + v_i^2    (i.e. alpha = vtv)
C  End For
C  alpha :=  sqrt( alpha )
C
C  Copy X(K)/M, ... , X(J)/M to V(K), ... , V(J)
C
      IF (INCX .EQ. 1) THEN
          DO I = K, J
              V(I) = X(I)/M
          ENDDO
      ELSE
          IEND = JMKP1*INCX
          ISTART = (K-1)*INCX + 1
          L = K
          DO I = ISTART, IEND, INCX
              V(L) = X(I)/M
              L = L + 1
          ENDDO
      END IF
C 
C  Compute alpha
C
      ALPHA = DNRM2(JMKP1, V(K), 1)
C
C  beta := 1/(alpha(alpha + |V_k|))
C
      BETA = 1.0D0 / (ALPHA*(ALPHA+ABS(V(K))))
C
C  v_k := v_k + sign(v_k)*alpha
C
      V(K) = V(K) + SIGN(1.0D0,V(K))*ALPHA
C
C  Done !
C
      RETURN
C
C     End of DHHPR.
C
      END SUBROUTINE DHHPR
      SUBROUTINE DHHAP ( K, J, N, Q, BETA, V, JOB, A, LDA )
C     .. Scalar Arguments ..
      INTEGER            J, K, N, Q, JOB, LDA
      DOUBLE PRECISION   BETA
C     .. Array Arguments ..
      DOUBLE PRECISION   V( * ), A( LDA, * )
C     ..
C
C  Purpose
C  =======
C
C  DHHAP applies a Householder Plane Rotation defined by v and beta
C  to the matrix A.  If JOB = 1 then A := (I - beta*v*vt)A and if 
C  JOB = 2 then A := A(I - beta*v*vt). (See Golub and van Loan
C  Alg. 3.3-2.)
C
C  Parameters
C  ==========
C
C  K      - INTEGER.
C           On entry, K specifies that the V(K) may be the first 
C           non-zero entry of V. 
C           K must be at least one.
C           Unchanged on exit.
C
C  J      - INTEGER.
C           On entry, J specifies the last non-zero entry of V.
C           J must be >= K and <= N.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the row dimension of A.
C           Unchanged on exit.
C
C  Q      - INTEGER.
C           On entry, Q specifies the column dimension of A.
C           Unchanged on exit.
C
C  BETA   - DOUBLE PRECISION.
C           BETA specifies the scalar beta. (see pg. 40 of G and v.L.)
C           Unchanged on exit.
C
C  V      - DOUBLE PRECISION array of DIMENSION at least n.
C           Householder vector v.
C           Unchanged on exit.
C
C  JOB    - INTEGER.
C           On entry, JOB specifies the order of the Householder application.
C           If JOB = 1 then A := (I - beta*v*vt)A and if JOB = 2 then
C           A := A(I - beta*v*vt)
C           Unchanged on exit.
C
C  A      - DOUBLE PRECISION array of DIMENSION at least
C           ( LDA, Q ).
C           On entry, A specifies the matrix to be transformed.
C           On exit, A specifies the transformed matrix.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the declared leading dimension of A.
C           Unchanged on exit.
C
C  -- Written by Tom Fairgrieve,
C                Department of Computer Science,
C                University of Toronto,
C                Toronto, Ontario CANADA  M5S 1A4 
C
C     .. Local Scalars ..
      DOUBLE PRECISION  S
      INTEGER           JMKP1, ROW, COL
C     .. External Functions from BLAS ..
      DOUBLE PRECISION  DDOT
      EXTERNAL          DDOT
C
C     .. Executable Statements ..
C
C  Test the input parameters.
C
      IF ((JOB .NE. 1) .AND. (JOB .NE. 2)) THEN
          WRITE (9,*) 'Domain error for JOB in DHHAP'
          STOP
      END IF
      IF ((K .LT. 1) .OR. (K .GT. J)) THEN
          WRITE (9,*) 'Domain error for K in DHHAP'
          STOP
      END IF
      IF (JOB .EQ. 1) THEN
          IF (J .GT. N) THEN
              WRITE (9,*) 'Domain error for J in DHHAP'
              STOP
          END IF
      ELSE
          IF (J .GT. Q) THEN
              WRITE (9,*) 'Domain error for J in DHHAP'
              STOP
          END IF
      END IF
C
C  Minimum {row,col} dimension of update.
C
      JMKP1 = J - K + 1
C
C  If (JOB = 1) then
C      For p = 1, ... , q
C          s := beta*(v_k*a_k,p + ... + v_j*a_j,p)
C          For i = k, ..., j
C              a_i,p := a_i,p - s*v_i
C          End For
C      End For
C  Else % JOB=2
C      For p = 1, ... , n
C          s := beta*(v_k*a_p,k + ... + v_j*a_p,j)
C          For i = k, ..., j
C              a_p,i := a_p,i - s*v_i
C          End For
C      End For
C  End If
C
      IF (JOB .EQ. 1) THEN
          DO COL = 1, Q
              S = BETA * DDOT(JMKP1, V(K), 1, A(K, COL), 1)
              DO ROW = K, J
                  A(ROW,COL) = A(ROW,COL) - S*V(ROW)
              ENDDO
          ENDDO
      ELSE
          DO ROW = 1, N
              S = BETA * DDOT(JMKP1, V(K), 1, A(ROW, K), LDA)
              DO COL = K, J
                  A(ROW,COL) = A(ROW,COL) - S*V(COL)
              ENDDO
          ENDDO
      END IF
C
C  Done !
C
      RETURN
C
C     End of DHHAP.
C
      END SUBROUTINE DHHAP
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      END MODULE FLOQUET
