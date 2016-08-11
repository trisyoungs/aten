      SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
*
*  -- LAPACK ROUTINE (VERSION 1.0B) --
*     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
*     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
*     FEBRUARY 29, 1992
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
*  DGETRF COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A
*  USING PARTIAL PIVOTING WITH ROW INTERCHANGES.
*
*  THE FACTORIZATION HAS THE FORM
*     A = P * L * U
*  WHERE P IS A PERMUTATION MATRIX, L IS LOWER TRIANGULAR WITH UNIT
*  DIAGONAL ELEMENTS (LOWER TRAPEZOIDAL IF M > N), AND U IS UPPER
*  TRIANGULAR (UPPER TRAPEZOIDAL IF M < N).
*
*  THIS IS THE RIGHT-LOOKING LEVEL 3 BLAS VERSION OF THE ALGORITHM.
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
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. LOCAL SCALARS ..
      INTEGER            I, IINFO, J, JB, NB
*     ..
*     .. EXTERNAL SUBROUTINES ..
      EXTERNAL           DGEMM, DGETF2, DLASWP, DTRSM, XERBLA
*     ..
*     .. EXTERNAL FUNCTIONS ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
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
         CALL XERBLA( 'DGETRF', -INFO )
         RETURN
      END IF
*
*     QUICK RETURN IF POSSIBLE
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
*     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
*
      NB = ILAENV( 1, 'DGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
*
*        USE UNBLOCKED CODE.
*
         CALL DGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
*
*        USE BLOCKED CODE.
*
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
*
*           FACTOR DIAGONAL AND SUBDIAGONAL BLOCKS AND TEST FOR EXACT
*           SINGULARITY.
*
            CALL DGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
*
*           ADJUST INFO AND THE PIVOT INDICES.
*
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
*
*           APPLY INTERCHANGES TO COLUMNS 1:J-1.
*
            CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
*
            IF( J+JB.LE.N ) THEN
*
*              APPLY INTERCHANGES TO COLUMNS J+JB:N.
*
               CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
*
*              COMPUTE BLOCK ROW OF U.
*
               CALL DTRSM( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               IF( J+JB.LE.M ) THEN
*
*                 UPDATE TRAILING SUBMATRIX.
*
                  CALL DGEMM( 'NO TRANSPOSE', 'NO TRANSPOSE', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     END OF DGETRF
*
      END
