      SUBROUTINE DGETF2( M, N, A, LDA, IPIV, INFO )
*
*  -- LAPACK ROUTINE (VERSION 1.0B) --
*     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
*     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
*     JUNE 30, 1992
*
*     .. SCALAR ARGUMENTS ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. ARRAY ARGUMENTS ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  PURPOSE
*  =======
*
*  DGETF2 COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A
*  USING PARTIAL PIVOTING WITH ROW INTERCHANGES.
*
*  THE FACTORIZATION HAS THE FORM
*     A = P * L * U
*  WHERE P IS A PERMUTATION MATRIX, L IS LOWER TRIANGULAR WITH UNIT
*  DIAGONAL ELEMENTS (LOWER TRAPEZOIDAL IF M > N), AND U IS UPPER
*  TRIANGULAR (UPPER TRAPEZOIDAL IF M < N).
*
*  THIS IS THE RIGHT-LOOKING LEVEL 2 BLAS VERSION OF THE ALGORITHM.
*
*  ARGUMENTS
*  =========
*
*  M       (INPUT) INTEGER
*          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
*
*  N       (INPUT) INTEGER
*          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
*
*  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
*          ON ENTRY, THE M BY N MATRIX TO BE FACTORED.
*          ON EXIT, THE FACTORS L AND U FROM THE FACTORIZATION
*          A = P*L*U; THE UNIT DIAGONAL ELEMENTS OF L ARE NOT STORED.
*
*  LDA     (INPUT) INTEGER
*          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
*
*  IPIV    (OUTPUT) INTEGER ARRAY, DIMENSION (MIN(M,N))
*          THE PIVOT INDICES; FOR 1 <= I <= MIN(M,N), ROW I OF THE
*          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
*
*  INFO    (OUTPUT) INTEGER
*          = 0: SUCCESSFUL EXIT
*          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
*          > 0: IF INFO = K, U(K,K) IS EXACTLY ZERO. THE FACTORIZATION
*               HAS BEEN COMPLETED, BUT THE FACTOR U IS EXACTLY
*               SINGULAR, AND DIVISION BY ZERO WILL OCCUR IF IT IS USED
*               TO SOLVE A SYSTEM OF EQUATIONS.
*
*  =====================================================================
*
*     .. PARAMETERS ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. LOCAL SCALARS ..
      INTEGER            J, JP
*     ..
*     .. EXTERNAL FUNCTIONS ..
      INTEGER            IDAMAX
      EXTERNAL           IDAMAX
*     ..
*     .. EXTERNAL SUBROUTINES ..
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
*     ..
*     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
*     ..
*     .. EXECUTABLE STATEMENTS ..
*
*     TEST THE INPUT PARAMETERS.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETF2', -INFO )
         RETURN
      END IF
*
*     QUICK RETURN IF POSSIBLE
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
      DO 10 J = 1, MIN( M, N )
*
*        FIND PIVOT AND TEST FOR SINGULARITY.
*
         JP = J - 1 + IDAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
*
*           APPLY THE INTERCHANGE TO COLUMNS 1:N.
*
            IF( JP.NE.J )
     $         CALL DSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
*
*           COMPUTE ELEMENTS J+1:M OF J-TH COLUMN.
*
            IF( J.LT.M )
     $         CALL DSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
*
         ELSE IF( INFO.EQ.0 ) THEN
*
            INFO = J
         END IF
*
         IF( J.LT.MIN( M, N ) ) THEN
*
*           UPDATE TRAILING SUBMATRIX.
*
            CALL DGER( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,
     $                 A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
*
*     END OF DGETF2
*
      END
