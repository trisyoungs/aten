      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
*
*  -- LAPACK ROUTINE (VERSION 1.0B) --
*     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
*     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
*     FEBRUARY 29, 1992
*
*     .. SCALAR ARGUMENTS ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  PURPOSE
*  =======
*
*  DTRTRI COMPUTES THE INVERSE OF A REAL UPPER OR LOWER TRIANGULAR
*  MATRIX A.
*
*  THIS IS THE LEVEL 3 BLAS VERSION OF THE ALGORITHM.
*
*  ARGUMENTS
*  =========
*
*  UPLO    (INPUT) CHARACTER*1
*          SPECIFIES WHETHER THE MATRIX A IS UPPER OR LOWER TRIANGULAR.
*          = 'U':  UPPER TRIANGULAR
*          = 'L':  LOWER TRIANGULAR
*
*  DIAG    (INPUT) CHARACTER*1
*          SPECIFIES WHETHER OR NOT THE MATRIX A IS UNIT TRIANGULAR.
*          = 'N':  NON-UNIT TRIANGULAR
*          = 'U':  UNIT TRIANGULAR
*
*  N       (INPUT) INTEGER
*          THE ORDER OF THE MATRIX A.  N >= 0.
*
*  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
*
*          ON ENTRY, THE TRIANGULAR MATRIX A.  IF UPLO = 'U', THE
*          LEADING N BY N UPPER TRIANGULAR PART OF THE ARRAY A CONTAINS
*          THE UPPER TRIANGULAR MATRIX, AND THE STRICTLY LOWER
*          TRIANGULAR PART OF A IS NOT REFERENCED.  IF UPLO = 'L', THE
*          LEADING N BY N LOWER TRIANGULAR PART OF THE ARRAY A CONTAINS
*          THE LOWER TRIANGULAR MATRIX, AND THE STRICTLY UPPER
*          TRIANGULAR PART OF A IS NOT REFERENCED.  IF DIAG = 'U', THE
*          DIAGONAL ELEMENTS OF A ARE ALSO NOT REFERENCED AND ARE
*          ASSUMED TO BE 1.
*
*          ON EXIT, THE (TRIANGULAR) INVERSE OF THE ORIGINAL MATRIX, IN
*          THE SAME STORAGE FORMAT.
*
*  LDA     (INPUT) INTEGER
*          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
*
*  INFO    (OUTPUT) INTEGER
*          = 0: SUCCESSFUL EXIT
*          > 0: IF INFO = K, A(K,K) IS EXACTLY ZERO.  THE TRIANGULAR
*               MATRIX IS SINGULAR AND ITS INVERSE CAN NOT BE COMPUTED.
*          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
*
*  =====================================================================
*
*     .. PARAMETERS ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. LOCAL SCALARS ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J, JB, NB, NN
*     ..
*     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. EXTERNAL SUBROUTINES ..
      EXTERNAL           DTRMM, DTRSM, DTRTI2, XERBLA
*     ..
*     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
*     ..
*     .. EXECUTABLE STATEMENTS ..
*
*     TEST THE INPUT PARAMETERS.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTRI', -INFO )
         RETURN
      END IF
*
*     QUICK RETURN IF POSSIBLE
*
      IF( N.EQ.0 )
     $   RETURN
*
*     CHECK FOR SINGULARITY IF NON-UNIT.
*
      IF( NOUNIT ) THEN
         DO 10 INFO = 1, N
            IF( A( INFO, INFO ).EQ.ZERO )
     $         RETURN
   10    CONTINUE
         INFO = 0
      END IF
*
*     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
*
      NB = ILAENV( 1, 'DTRTRI', UPLO // DIAG, N, -1, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
*
*        USE UNBLOCKED CODE
*
         CALL DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
      ELSE
*
*        USE BLOCKED CODE
*
         IF( UPPER ) THEN
*
*           COMPUTE INVERSE OF UPPER TRIANGULAR MATRIX
*
            DO 20 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
*
*              COMPUTE ROWS 1:J-1 OF CURRENT BLOCK COLUMN
*
               CALL DTRMM( 'LEFT', 'UPPER', 'NO TRANSPOSE', DIAG, J-1,
     $                     JB, ONE, A, LDA, A( 1, J ), LDA )
               CALL DTRSM( 'RIGHT', 'UPPER', 'NO TRANSPOSE', DIAG, J-1,
     $                     JB, -ONE, A( J, J ), LDA, A( 1, J ), LDA )
*
*              COMPUTE INVERSE OF CURRENT DIAGONAL BLOCK
*
               CALL DTRTI2( 'UPPER', DIAG, JB, A( J, J ), LDA, INFO )
   20       CONTINUE
         ELSE
*
*           COMPUTE INVERSE OF LOWER TRIANGULAR MATRIX
*
            NN = ( ( N-1 ) / NB )*NB + 1
            DO 30 J = NN, 1, -NB
               JB = MIN( NB, N-J+1 )
               IF( J+JB.LE.N ) THEN
*
*                 COMPUTE ROWS J+JB:N OF CURRENT BLOCK COLUMN
*
                  CALL DTRMM( 'LEFT', 'LOWER', 'NO TRANSPOSE', DIAG,
     $                        N-J-JB+1, JB, ONE, A( J+JB, J+JB ), LDA,
     $                        A( J+JB, J ), LDA )
                  CALL DTRSM( 'RIGHT', 'LOWER', 'NO TRANSPOSE', DIAG,
     $                        N-J-JB+1, JB, -ONE, A( J, J ), LDA,
     $                        A( J+JB, J ), LDA )
               END IF
*
*              COMPUTE INVERSE OF CURRENT DIAGONAL BLOCK
*
               CALL DTRTI2( 'LOWER', DIAG, JB, A( J, J ), LDA, INFO )
   30       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     END OF DTRTRI
*
      END
