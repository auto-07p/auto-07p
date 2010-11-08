!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Floquet Multiplier Computation (Tom Fairgrieve, U. of Toronto)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  References:
!  T. F. Fairgrieve, PhD Thesis, University of Toronto, 1994.
!
!  T. F. Fairgrieve, A. D. Jepson, O.K. Floquet multipliers,
!  SIAM J. Numer. Anal. 28. No. 5, 1991, 1446-1462.
!
!  Please inform Tom Fairgrieve (tff@na.utoronto.ca) of any 
!  modifications to or errors in these routines.
!  Mailing Address: T.F. Fairgrieve, Department of Computer Science,
!  University of Toronto, Toronto, Ontario, CANADA  M5S 1A4C
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Routines included in this file:
!
!  subroutine flowkm : new routine to compute floquet multipliers
!  subroutine dhhpr  : compute a Householder matrix
!  subroutine dhhap  : appy a Householder matrix
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  Required library routines (included in the file eispack.f) :
!
!  subroutine qzhes  : QZ reduction to Hessenberg form             (EISPACK)
!  subroutine qzit   : QZ reduction to quasi-upper triangular form (EISPACK)
!  subroutine qzval  : QZ calculation of eigenvalues               (EISPACK)
!  function   epslon : machine constant routine                    (EISPACK)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  function   dnrm2  : compute l2-norm of a vector                 (BLAS-1)
!  function   ddot   : dot product of two vectors                  (BLAS-1)
!  subroutine dscal  : scale a vector by a constant                (BLAS-1)
!  function   idamax : find index of element with max abs value    (BLAS-1)
!  subroutine daxpy  : constant times a vector plus a vector       (BLAS-1)
!  subroutine drot   : apply a plane rotation                      (BLAS-1)
!  subroutine dswap  : swap two vectors                            (BLAS-1)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  subroutine xerbla : BLAS error handling routine                 (BLAS-2)
!  function   lsame  : compare character strings                   (BLAS-2)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  subroutine dgemm  : matrix-matrix multiply                      (BLAS-3)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  subroutines ezsvd, ndrotg, ndsvd, prse, sig22, sigmin, sndrtg :
!                      Demmel-Kahan svd routines 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE FLOQUET

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: FLOWKM

CONTAINS

  SUBROUTINE FLOWKM (NDIM, P0, P1, IID, EV)

    !  Subroutine to compute Floquet multipliers via the "deflated circuit 
    !  pencil" method. This routine is called by the AUTO routine FNSPBV

    !  Parameter declarations:

    INTEGER, INTENT(IN) :: NDIM, IID
    DOUBLE PRECISION, INTENT(IN) :: P0(NDIM, NDIM), P1(NDIM, NDIM)
    COMPLEX(KIND(1.0D0)), INTENT(OUT) :: EV(NDIM)

    !  Local declarations:


    !  Maximum order of problem.  **** (To avoid changing too many AUTO'86
    !  routines, I need to declare some local array storage here )

    INTEGER           LEFT, RIGHT
    PARAMETER         (LEFT = 1, RIGHT = 2)

    INTEGER           I, J, NDIMM1
    DOUBLE PRECISION  X(:),BETA,V(:),C0(:,:),C1(:,:),RWORK(:,:), &
                      NRMC0X, NRMC1X, CONST
    LOGICAL           INFEV

    !  storage for SVD computations

    INTEGER           SVDINF, SVDLWRK
    DOUBLE PRECISION  SVDS(:), SVDE(:), SVDWRK(:), SVDV(:,:)
    ALLOCATABLE       C0,C1,RWORK,X,V,SVDS,SVDE,SVDWRK,SVDV
    !  compute right singular vectors only
    CHARACTER(1), PARAMETER :: SVDJOBA = 'G', SVDJOBU = 'N', SVDJOBV = 'V'

    !  storage for generalized eigenvalue computations

    INTEGER           QZIERR
    DOUBLE PRECISION  QZZ(1, 1), QZEPS1, QZALFR(:), QZALFI(:), QZBETA(:)
    ALLOCATABLE       QZALFR,QZALFI,QZBETA
    LOGICAL           QZMATZ
    !  don't want to accumulate the transforms --- vectors not needed
    PARAMETER         (QZMATZ = .FALSE.)
    PARAMETER         (QZEPS1 = 0.0D0)

    !  BLAS routines

    DOUBLE PRECISION  DNRM2
    EXTERNAL          DNRM2, DGEMM

    !  routines from LAPACK

    EXTERNAL          DGESVJ, DGGEV

    !  builtin F77 functions

    INTRINSIC         MAX, ABS, CMPLX

    SVDLWRK = MAX(6,2*NDIM)
    ALLOCATE(C0(NDIM,NDIM),C1(NDIM,NDIM),RWORK(NDIM,NDIM))
    ALLOCATE(SVDE(NDIM),SVDS(NDIM+1),SVDV(NDIM,NDIM),V(NDIM),X(NDIM))
    ALLOCATE(QZALFI(NDIM),QZBETA(NDIM),QZALFR(NDIM),SVDWRK(SVDLWRK))

    ! Change sign of P1 so that we get the sign of the multipliers right.

    C0=P0
    C1=-P1

    !  Print the undeflated circuit pencil (C0, C1).

    IF(IID>4) THEN
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

    !  PART I:
    !  =======

    !  Deflate the Floquet multiplier at +1.0 so that the deflated 
    !  circuit pencil is not defective at periodic branch turning points.

    !  The matrix (C0 - C1) should be (nearly) singular.  Find an approximation 
    !  to the right null vector (call it X).  This will be our approximation 
    !  to the eigenvector corresponding to the fixed multiplier at +1.0.  
    !  
    !  There are many ways to get this approximation.  We could use
    !    1) p'(0) = f(p(0))
    !    2) AUTO'86 routine NLVC applied to C0-C1
    !    3) the right singular vector corresponding to the smallest 
    !       singular value of C0-C1

    !  I've chosen option 3) because it should introduce as little roundoff
    !  error as possible.  Although it is more expensive, this is insignificant
    !  relative to the rest of the AUTO computations. Also, the SVD does give a
    !  version of the Householder matrix which we would have to compute
    !  anyways.  But note that it gives V = ( X perp | X ) and not (X | Xperp),
    !  which the Householder routine would give.  This will permute the deflated
    !  circuit pencil, so that the part to be deflated is in the last column,
    !  not it the first column, as was shown in the paper.

    RWORK = C0 - C1

    CALL DGESVJ( SVDJOBA, SVDJOBU, SVDJOBV, NDIM, NDIM, RWORK, NDIM, &
         SVDS, 0, SVDV, NDIM, SVDWRK, SVDLWRK, SVDINF )
    IF (SVDINF /= 0) WRITE (9, 901) SVDINF

    !  Apply a Householder matrix (call it H1) based on the null vector
    !  to (C0, C1) from the right.  H1 = SVDV = ( Xperp | X ), where X
    !  is the null vector.

    CALL DGEMM ( 'n', 'n', NDIM, NDIM, NDIM, 1.0d0, C0, NDIM, &
         SVDV, NDIM, 0.0d0, RWORK, NDIM )
    C0 = RWORK
    CALL DGEMM ( 'n', 'n', NDIM, NDIM, NDIM, 1.0d0, C1, NDIM, &
         SVDV, NDIM, 0.0d0, RWORK, NDIM )
    C1 = RWORK

    !  Apply a Householder matrix (call it H2) based on 
    !  (C0*X/||C0*X|| + C1*X/||C1*X||) / 2 
    !  to (C0*H1, C1*H1) from the left.

    NRMC0X = DNRM2( NDIM, C0(1,NDIM), 1)
    NRMC1X = DNRM2( NDIM, C1(1,NDIM), 1)
    DO I = 1, NDIM
       X(I) = ((C0(I,NDIM)/NRMC0X)+(C1(I,NDIM)/NRMC1X)) / 2.0D0 
    ENDDO
    CALL DHHPR ( 1, NDIM, NDIM, X, 1, BETA, V )
    CALL DHHAP ( 1, NDIM, NDIM, NDIM, BETA, V, LEFT, C0, NDIM)
    CALL DHHAP ( 1, NDIM, NDIM, NDIM, BETA, V, LEFT, C1, NDIM)

    !  Rescale so that (H2^T)*C0*(H1)(1,NDIM) ~= (H2^T)*C1*(H1)(1,NDIM) ~= 1.0

    CONST = MAX(ABS(C0(1,NDIM)), ABS(C1(1,NDIM)))
    DO J = 1, NDIM
       DO I = 1, NDIM
          C0(I,J) = C0(I,J) / CONST
          C1(I,J) = C1(I,J) / CONST
       ENDDO
    ENDDO

    !  Finished the deflation process! Print the deflated circuit pencil.

    IF(IID>4) THEN
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

    !  At this point we have

    !     (C0Bar, C1Bar) 
    ! ::= (H2^T)*(C0, C1)*(H1).

    !     (( B0^T     | Beta0  )  ( B1^T     | Beta1  ))  1
    !   = (( ----------------- ), ( ----------------- ))
    !     (( C0BarDef | Delta0 )  ( C1BarDef | Delta1 )) NDIM-1

    !         NDIM-1      1          NDIM-1      1

    !  and approximations to the Floquet multipliers are 
    !  (Beta0/Beta1) union the eigenvalues of the deflated pencil 
    !  (C0BarDef, C1BarDef).

    !  PART II:
    !  ========

    !  Compute the eigenvalues of the deflated circuit pencil 
    !  (C0BarDef, C1BarDef)
    !  by using the QZ routines from EISPACK.

    NDIMM1 = NDIM - 1

    !  compute the eigenvalues for the generalized eigenvalue problem

    CALL DGGEV('N', 'N', NDIMM1, C0(2,1), NDIM, C1(2,1), NDIM, QZALFR, &
         QZALFI, QZBETA, QZZ, 1, QZZ, 1, SVDWRK, -1, QZIERR)
    SVDLWRK = NINT(SVDWRK(1))
    DEALLOCATE(SVDWRK)
    ALLOCATE(SVDWRK(SVDLWRK))
    CALL DGGEV('N', 'N', NDIMM1, C0(2,1), NDIM, C1(2,1), NDIM, QZALFR, &
         QZALFI, QZBETA, QZZ, 1, QZZ, 1, SVDWRK, SVDLWRK, QZIERR)
    IF (QZIERR /= 0) WRITE (9, 902) QZIERR

    !  Pack the eigenvalues into complex form.

    EV(1) = CMPLX( C0(1,NDIM) / C1(1,NDIM), 0.0D0, KIND(1.0D0) )
    INFEV = .FALSE.
    DO J = 1, NDIMM1
       IF (QZBETA(J) /= 0.0D0) THEN
          EV(J+1) = CMPLX( QZALFR(J)/QZBETA(J), QZALFI(J)/QZBETA(J), &
               KIND(1.0D0) )
       ELSE
          EV(J+1) = CMPLX( HUGE(1.0D0), HUGE(1.0D0), KIND(1.0D0) )
          INFEV = .TRUE.
       END IF
    ENDDO
    IF (INFEV) WRITE (9, 903) 

    !  Done!

    DEALLOCATE(C0,C1,RWORK,X,V,SVDS,SVDE,SVDWRK,SVDV,QZALFR,QZALFI,QZBETA)

    !  Format statements

101 FORMAT(' Undeflated circuit pencil (C0, C1) ')
102 FORMAT('   C0 : ')
103 FORMAT('   C1 : ')
104 FORMAT(1X,6E23.16)
105 FORMAT(' Deflated circuit pencil (H2^T)*(C0, C1)*(H1) ')
106 FORMAT('   (H2^T)*C0*(H1) : ')
107 FORMAT('   (H2^T)*C1*(H1) : ')
901 FORMAT(' NOTE : Warning from subroutine FLOWKM : ', &
          /,'        SVD routine returned SVDINF = ', I4, &
          /,'        Floquet multiplier calculations may be wrong')
902 FORMAT(' NOTE : Warning from subroutine FLOWKM : ', &
          /,'        QZ routine returned QZIERR = ', I4, &
          /,'        Floquet multiplier calculations may be wrong ')
903 FORMAT(' NOTE : Warning from subroutine FLOWKM : ', &
          /,'        Infinite Floquet multiplier represented by ', &
          /,'        CMPLX( HUGE(1.0D0), HUGE(1.0D0) )')

  END SUBROUTINE FLOWKM

  ! **************************
  ! *  Householder routines  *
  ! **************************


  !  Subroutines for performing Householder plane rotations.

  !  DHHPR: for computing Householder transformations and
  !  DHHAP: for applying them.

  !  Ref: Golub and van Loan, Matrix Calcualtions, 
  !       First Edition, Pages 38-43

  SUBROUTINE DHHPR ( K, J, N, X, INCX, BETA, V )
    USE SUPPORT, ONLY: AUTOSTOP
    !     .. Scalar Arguments ..
    INTEGER, INTENT(IN) :: J, K, N, INCX
    DOUBLE PRECISION, INTENT(OUT) :: BETA
    !     .. Array Arguments ..
    DOUBLE PRECISION, INTENT(IN) :: X( * )
    DOUBLE PRECISION, INTENT(INOUT) :: V( * )
    !     ..

    !  Purpose
    !  =======

    !  DHHPR  computes a Householder Plane Rotation (G&vL Alg. 3.3-1)
    !  defined by v and beta.
    !  (I - beta v vt) * x is such that x_i = 0 for i=k+1 to j.

    !  Parameters
    !  ==========

    !  K      - INTEGER.
    !           On entry, K specifies that the K+1st entry of X 
    !           be the first to be zeroed.
    !           K must be at least one.
    !           Unchanged on exit.

    !  J      - INTEGER.
    !           On entry, J specifies the last entry of X to be zeroed.
    !           J must be >= K and <= N.
    !           Unchanged on exit.

    !  N      - INTEGER.
    !           On entry, N specifies the (logical) length of X.
    !           Unchanged on exit.

    !  X      - DOUBLE PRECISION array of DIMENSION at least
    !           ( 1 + ( N - 1 )*abs( INCX ) ). 
    !           On entry, X specifies the vector to be (partially) zeroed.
    !           Unchanged on exit.

    !  INCX   - INTEGER.
    !           On entry, INCX specifies the increment for the elements of
    !           X. INCX must be > zero. If X represents part of a matrix, 
    !           then use INCX = 1 if a column vector is being zeroed and 
    !           INCX = NDIM if a row vector is being zeroed.
    !           Unchanged on exit.

    !  BETA   - DOUBLE PRECISION.
    !           BETA specifies the scalar beta. (see pg. 40 of G and v.L.)

    !  V      - DOUBLE PRECISION array of DIMENSION at least n.
    !           Is updated to be the appropriate Householder vector for
    !           the given problem. (Note: space for the implicit zeroes is
    !           assumed to be present. Will save on time for index translation.)

    !  -- Written by Tom Fairgrieve,
    !                Department of Computer Science,
    !                University of Toronto,
    !                Toronto, Ontario CANADA  M5S 1A4 

    !     .. Local Scalars ..
    DOUBLE PRECISION   ALPHA, M
    INTEGER            I, L, JMKP1, IEND, ISTART
    !     .. External Functions from BLAS ..
    INTEGER            IDAMAX
    DOUBLE PRECISION   DNRM2
    EXTERNAL           IDAMAX, DNRM2
    !     .. External Subroutines from BLAS ..
    EXTERNAL           DSCAL
    !     .. Intrinsic Functions ..
    INTRINSIC          ABS, SIGN

    !     .. Executable Statements ..

    !  Test the input parameters.

    IF ((K < 1) .OR. (K > J)) THEN
       WRITE (9,*) 'Domain error for K in DHHPR'
       CALL AUTOSTOP()
    END IF
    IF (J > N) THEN
       WRITE (9,*) 'Domain error for J in DHHPR'
       CALL AUTOSTOP()
    END IF
    IF (INCX < 1) THEN
       WRITE (9,*) 'Domain error for INCX in DHHPR'
       CALL AUTOSTOP()
    END IF

    !  Number of potential non-zero elements in V.

    JMKP1 = J - K + 1

    !  Find M := max{ |x_k|, ... , |x_j| }

    M = ABS( X( IDAMAX( JMKP1, X(K), INCX ) ) )

    !  alpha := 0
    !  For i = k to j
    !      v_i = x_i / m
    !      alpha := alpha + v_i^2    (i.e. alpha = vtv)
    !  End For
    !  alpha :=  sqrt( alpha )

    !  Copy X(K)/M, ... , X(J)/M to V(K), ... , V(J)

    IF (INCX == 1) THEN
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

    !  Compute alpha

    ALPHA = DNRM2(JMKP1, V(K), 1)

    !  beta := 1/(alpha(alpha + |V_k|))

    BETA = 1.0D0 / (ALPHA*(ALPHA+ABS(V(K))))

    !  v_k := v_k + sign(v_k)*alpha

    V(K) = V(K) + SIGN(1.0D0,V(K))*ALPHA

    !  Done !
    !     End of DHHPR.

  END SUBROUTINE DHHPR

  SUBROUTINE DHHAP ( K, J, N, Q, BETA, V, JOB, A, LDA )
    USE SUPPORT, ONLY: AUTOSTOP
    !     .. Scalar Arguments ..
    INTEGER, INTENT(IN) :: J, K, N, Q, JOB, LDA
    DOUBLE PRECISION, INTENT(IN) :: BETA
    !     .. Array Arguments ..
    DOUBLE PRECISION, INTENT(IN) :: V( * )
    DOUBLE PRECISION, INTENT(INOUT) :: A( LDA, * )
    !     ..

    !  Purpose
    !  =======

    !  DHHAP applies a Householder Plane Rotation defined by v and beta
    !  to the matrix A.  If JOB = 1 then A := (I - beta*v*vt)A and if 
    !  JOB = 2 then A := A(I - beta*v*vt). (See Golub and van Loan
    !  Alg. 3.3-2.)

    !  Parameters
    !  ==========

    !  K      - INTEGER.
    !           On entry, K specifies that the V(K) may be the first 
    !           non-zero entry of V. 
    !           K must be at least one.
    !           Unchanged on exit.

    !  J      - INTEGER.
    !           On entry, J specifies the last non-zero entry of V.
    !           J must be >= K and <= N.
    !           Unchanged on exit.

    !  N      - INTEGER.
    !           On entry, N specifies the row dimension of A.
    !           Unchanged on exit.

    !  Q      - INTEGER.
    !           On entry, Q specifies the column dimension of A.
    !           Unchanged on exit.

    !  BETA   - DOUBLE PRECISION.
    !           BETA specifies the scalar beta. (see pg. 40 of G and v.L.)
    !           Unchanged on exit.

    !  V      - DOUBLE PRECISION array of DIMENSION at least n.
    !           Householder vector v.
    !           Unchanged on exit.

    !  JOB    - INTEGER.
    !           On entry, JOB specifies the order of the Householder application.
    !           If JOB = 1 then A := (I - beta*v*vt)A and if JOB = 2 then
    !           A := A(I - beta*v*vt)
    !           Unchanged on exit.

    !  A      - DOUBLE PRECISION array of DIMENSION at least
    !           ( LDA, Q ).
    !           On entry, A specifies the matrix to be transformed.
    !           On exit, A specifies the transformed matrix.

    !  LDA    - INTEGER.
    !           On entry, LDA specifies the declared leading dimension of A.
    !           Unchanged on exit.

    !  -- Written by Tom Fairgrieve,
    !                Department of Computer Science,
    !                University of Toronto,
    !                Toronto, Ontario CANADA  M5S 1A4 

    !     .. Local Scalars ..
    DOUBLE PRECISION  S
    INTEGER           JMKP1, ROW, COL
    !     .. External Functions from BLAS ..
    DOUBLE PRECISION  DDOT
    EXTERNAL          DDOT

    !     .. Executable Statements ..

    !  Test the input parameters.

    IF ((JOB /= 1) .AND. (JOB /= 2)) THEN
       WRITE (9,*) 'Domain error for JOB in DHHAP'
       CALL AUTOSTOP()
    END IF
    IF ((K < 1) .OR. (K > J)) THEN
       WRITE (9,*) 'Domain error for K in DHHAP'
       CALL AUTOSTOP()
    END IF
    IF (JOB == 1) THEN
       IF (J > N) THEN
          WRITE (9,*) 'Domain error for J in DHHAP'
          CALL AUTOSTOP()
       END IF
    ELSE
       IF (J > Q) THEN
          WRITE (9,*) 'Domain error for J in DHHAP'
          CALL AUTOSTOP()
       END IF
    END IF

    !  Minimum {row,col} dimension of update.

    JMKP1 = J - K + 1

    ! If (JOB = 1) then
    !     For p = 1, ... , q
    !         s := beta*(v_k*a_k,p + ... + v_j*a_j,p)
    !         For i = k, ..., j
    !             a_i,p := a_i,p - s*v_i
    !         End For
    !     End For
    ! Else % JOB=2
    !     For p = 1, ... , n
    !         s := beta*(v_k*a_p,k + ... + v_j*a_p,j)
    !         For i = k, ..., j
    !             a_p,i := a_p,i - s*v_i
    !         End For
    !     End For
    ! End If

    IF (JOB == 1) THEN
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

    ! Done !
    ! End of DHHAP.

  END SUBROUTINE DHHAP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
END MODULE FLOQUET
