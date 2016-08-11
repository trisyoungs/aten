      SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
*
*  -- LAPACK ROUTINE (VERSION 1.0B) --
*     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
*     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
*     JUNE 30, 1992
*
*     .. SCALAR ARGUMENTS ..
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. ARRAY ARGUMENTS ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), WORK( LWORK )
*     ..
*
*  PURPOSE
*  =======
*
*  DGETRI COMPUTES THE INVERSE OF A MATRIX USING THE LU FACTORIZATION
*  COMPUTED BY DGETRF.
*
*  THIS METHOD INVERTS U AND THEN COMPUTES INV(A) BY SOLVING THE SYSTEM
*  INV(A)*L = INV(U) FOR INV(A).
*
*  ARGUMENTS
*  =========
*
*  N       (INPUT) INTEGER
*          THE ORDER OF THE MATRIX A.  N >= 0.
*
*  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
*          ON ENTRY, THE FACTORS L AND U FROM THE FACTORIZATION
*          A = P*L*U AS COMPUTED BY DGETRF.
*          ON EXIT, IF INFO = 0, THE INVERSE OF THE ORIGINAL MATRIX A.
*
*  LDA     (INPUT) INTEGER
*          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
*
*  IPIV    (INPUT) INTEGER ARRAY, DIMENSION (N)
*          THE PIVOT INDICES FROM DGETRF; FOR 1<=I<=N, ROW I OF THE
*          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
*
*  WORK    (WORKSPACE) DOUBLE PRECISION ARRAY, DIMENSION (LWORK)
*          IF INFO RETURNS 0, THEN WORK(1) RETURNS N*NB, THE MINIMUM
*          VALUE OF LWORK REQUIRED TO USE THE OPTIMAL BLOCKSIZE.
*
*  LWORK   (INPUT) INTEGER
*          THE DIMENSION OF THE ARRAY WORK.  LWORK >= MAX(1,N).
*          FOR OPTIMAL PERFORMANCE LWORK SHOULD BE AT LEAST N*NB,
*          WHERE NB IS THE OPTIMAL BLOCKSIZE RETURNED BY ILAENV.
*
*  INFO    (OUTPUT) INTEGER
*          = 0:  SUCCESSFUL EXIT
*          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
*          > 0: IF INFO = K, U(K,K) IS EXACTLY ZERO; THE MATRIX IS
*               SINGULAR AND ITS INVERSE COULD NOT BE COMPUTED.
*
*  =====================================================================
*
*     .. PARAMETERS ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. LOCAL SCALARS ..
      INTEGER            I, IWS, J, JB, JJ, JP, LDWORK, NB, NBMIN, NN
*     ..
*     .. EXTERNAL FUNCTIONS ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. EXTERNAL SUBROUTINES ..
      EXTERNAL           DGEMM, DGEMV, DSWAP, DTRSM, DTRTRI, XERBLA
*     ..
*     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
*     ..
*     .. EXECUTABLE STATEMENTS ..
*
*     TEST THE INPUT PARAMETERS.
*
      INFO = 0
      WORK( 1 ) = MAX( N, 1 )
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -3
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRI', -INFO )
         RETURN
      END IF
*
*     QUICK RETURN IF POSSIBLE
*
      IF( N.EQ.0 )
     $   RETURN
*
*     FORM INV(U).  IF INFO > 0 FROM DTRTRI, THEN U IS SINGULAR,
*     AND THE INVERSE IS NOT COMPUTED.
*
      CALL DTRTRI( 'UPPER', 'NON-UNIT', N, A, LDA, INFO )
      IF( INFO.GT.0 )
     $   RETURN
*
*     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
*
      NB = ILAENV( 1, 'DGETRI', ' ', N, -1, -1, -1 )
      NBMIN = 2
      LDWORK = N
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
         IWS = MAX( LDWORK*NB, 1 )
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DGETRI', ' ', N, -1, -1, -1 ) )
         END IF
      ELSE
         IWS = N
      END IF
*
*     SOLVE THE EQUATION INV(A)*L = INV(U) FOR INV(A).
*
      IF( NB.LT.NBMIN .OR. NB.GE.N ) THEN
*
*        USE UNBLOCKED CODE.
*
         DO 20 J = N, 1, -1
*
*           COPY CURRENT COLUMN OF L TO WORK AND REPLACE WITH ZEROS.
*
            DO 10 I = J + 1, N
               WORK( I ) = A( I, J )
               A( I, J ) = ZERO
   10       CONTINUE
*
*           COMPUTE CURRENT COLUMN OF INV(A).
*
            IF( J.LT.N )
     $         CALL DGEMV( 'NO TRANSPOSE', N, N-J, -ONE, A( 1, J+1 ),
     $                     LDA, WORK( J+1 ), 1, ONE, A( 1, J ), 1 )
   20    CONTINUE
      ELSE
*
*        USE BLOCKED CODE.
*
         NN = ( ( N-1 ) / NB )*NB + 1
         DO 50 J = NN, 1, -NB
            JB = MIN( NB, N-J+1 )
*
*           COPY CURRENT BLOCK COLUMN OF L TO WORK AND REPLACE WITH
*           ZEROS.
*
            DO 40 JJ = J, J + JB - 1
               DO 30 I = JJ + 1, N
                  WORK( I+( JJ-J )*LDWORK ) = A( I, JJ )
                  A( I, JJ ) = ZERO
   30          CONTINUE
   40       CONTINUE
*
*           COMPUTE CURRENT BLOCK COLUMN OF INV(A).
*
            IF( J+JB.LE.N )
     $         CALL DGEMM( 'NO TRANSPOSE', 'NO TRANSPOSE', N, JB,
     $                     N-J-JB+1, -ONE, A( 1, J+JB ), LDA,
     $                     WORK( J+JB ), LDWORK, ONE, A( 1, J ), LDA )
            CALL DTRSM( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', N, JB,
     $                  ONE, WORK( J ), LDWORK, A( 1, J ), LDA )
   50    CONTINUE
      END IF
*
*     APPLY COLUMN INTERCHANGES.
*
      DO 60 J = N - 1, 1, -1
         JP = IPIV( J )
         IF( JP.NE.J )
     $      CALL DSWAP( N, A( 1, J ), 1, A( 1, JP ), 1 )
   60 CONTINUE
*
      WORK( 1 ) = IWS
      RETURN
*
*     END OF DGETRI
*
      END
