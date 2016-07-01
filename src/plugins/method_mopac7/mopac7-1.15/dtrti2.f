      SUBROUTINE DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
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
*  DTRTI2 COMPUTES THE INVERSE OF A REAL UPPER OR LOWER TRIANGULAR
*  MATRIX.
*
*  THIS IS THE LEVEL 2 BLAS VERSION OF THE ALGORITHM.
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
*          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
*
*  =====================================================================
*
*     .. PARAMETERS ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. LOCAL SCALARS ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J
      DOUBLE PRECISION   AJJ
*     ..
*     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. EXTERNAL SUBROUTINES ..
      EXTERNAL           DSCAL, DTRMV, XERBLA
*     ..
*     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
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
         CALL XERBLA( 'DTRTI2', -INFO )
         RETURN
      END IF
*
      IF( UPPER ) THEN
*
*        COMPUTE INVERSE OF UPPER TRIANGULAR MATRIX.
*
         DO 10 J = 1, N
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
*
*           COMPUTE ELEMENTS 1:J-1 OF J-TH COLUMN.
*
            CALL DTRMV( 'UPPER', 'NO TRANSPOSE', DIAG, J-1, A, LDA,
     $                  A( 1, J ), 1 )
            CALL DSCAL( J-1, AJJ, A( 1, J ), 1 )
   10    CONTINUE
      ELSE
*
*        COMPUTE INVERSE OF LOWER TRIANGULAR MATRIX.
*
         DO 20 J = N, 1, -1
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
            IF( J.LT.N ) THEN
*
*              COMPUTE ELEMENTS J+1:N OF J-TH COLUMN.
*
               CALL DTRMV( 'LOWER', 'NO TRANSPOSE', DIAG, N-J,
     $                     A( J+1, J+1 ), LDA, A( J+1, J ), 1 )
               CALL DSCAL( N-J, AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
*
      RETURN
*
*     END OF DTRTI2
*
      END
