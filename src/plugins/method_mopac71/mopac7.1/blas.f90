!*MODULE BLAS1   *DECK DASUM
      real(kind(0.0d0)) function dasum (n, dx, incx) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      real(double) , intent(in) :: dx(incx*n) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, m, mp1, nincx 
      real(double) :: dtemp 
!-----------------------------------------------
!
!     TAKES THE SUM OF THE ABSOLUTE VALUES.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
!
      dasum = 0.0D+00 
      dtemp = 0.0D+00 
      if (n <= 0) return  
      if (incx /= 1) then 
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
        nincx = n*incx 
        do i = 1, nincx, incx 
          dtemp = dtemp + abs(dx(i)) 
        end do 
        dasum = dtemp 
        return  
      endif 
!
!        CODE FOR INCREMENT EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
      m = mod(n,6) 
      if (m == 0) go to 40 
      do i = 1, m 
        dtemp = dtemp + abs(dx(i)) 
      end do 
      if (n < 6) go to 60 
   40 continue 
      mp1 = m + 1 
      do i = mp1, n, 6 
        dtemp = dtemp + abs(dx(i)) + abs(dx(i+1)) + abs(dx(i+2)) + abs(dx(i+3))&
           + abs(dx(i+4)) + abs(dx(i+5)) 
      end do 
   60 continue 
      dasum = dtemp 
      return  
      end function dasum 


!*MODULE BLAS1   *DECK DAXPY
      subroutine daxpy(n, da, dx, incx, dy, incy) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      integer , intent(in) :: incy 
      real(double) , intent(in) :: da 
      real(double) , intent(in) :: dx(*) 
      real(double) , intent(inout) :: dy(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ix, iy, m, mp1 
!-----------------------------------------------
!
!     CONSTANT TIMES A VECTOR PLUS A VECTOR.
!           DY(I) = DY(I) + DA * DX(I)
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      if (n <= 0) return  
      if (da == 0.0D+00) return  
      if (incx/=1 .or. incy/=1) then 
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
        ix = 1 
        iy = 1 
        if (incx < 0) ix = ((-n) + 1)*incx + 1 
        if (incy < 0) iy = ((-n) + 1)*incy + 1 
        dy(iy:(n-1)*incy+iy:incy) = dy(iy:(n-1)*incy+iy:incy) + da*dx(ix:(n-1)*&
          incx+ix:incx) 
        return  
      endif 
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
      m = mod(n,4) 
      if (m /= 0) then 
        dy(:m) = dy(:m) + da*dx(:m) 
        if (n < 4) return  
      endif 
      mp1 = m + 1 
      dy(mp1:((n-mp1+4)/4)*4-1+mp1) = dy(mp1:((n-mp1+4)/4)*4-1+mp1) + da*dx(mp1&
        :((n-mp1+4)/4)*4-1+mp1) 
      return  
      end subroutine daxpy 


      subroutine dcopy(n, dx, incx, dy, incy) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      integer , intent(in) :: incy 
      real(double) , intent(in) :: dx(*) 
      real(double) , intent(out) :: dy(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ix, iy, m, mp1 
!-----------------------------------------------
!
!     COPIES A VECTOR, X, TO A VECTOR, Y.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
!
      if (n <= 0) return  
      if (incx/=1 .or. incy/=1) then 
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
        ix = 1 
        iy = 1 
        if (incx < 0) ix = ((-n) + 1)*incx + 1 
        if (incy < 0) iy = ((-n) + 1)*incy + 1 
        dy(iy:(n-1)*incy+iy:incy) = dx(ix:(n-1)*incx+ix:incx) 
        return  
      endif 
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
      m = mod(n,7) 
      if (m /= 0) then 
        dy(:m) = dx(:m) 
        if (n < 7) return  
      endif 
      mp1 = m + 1 
      dy(mp1:((n-mp1+7)/7)*7-1+mp1) = dx(mp1:((n-mp1+7)/7)*7-1+mp1) 
      return  
      end subroutine dcopy 


!*MODULE BLAS1   *DECK DDOT
      real(kind(0.0d0)) function ddot (n, dx, incx, dy, incy) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      integer , intent(in) :: incy 
      real(double) , intent(in) :: dx(*) 
      real(double) , intent(in) :: dy(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ix, iy, m, mp1 
      real(double) :: dtemp 
!-----------------------------------------------
!
!     FORMS THE DOT PRODUCT OF TWO VECTORS.
!           DOT = DX(I) * DY(I)
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      ddot = 0.0D+00 
      dtemp = 0.0D+00 
      if (n <= 0) return  
      if (incx/=1 .or. incy/=1) then 
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
        ix = 1 
        iy = 1 
        if (incx < 0) ix = ((-n) + 1)*incx + 1 
        if (incy < 0) iy = ((-n) + 1)*incy + 1 
        dtemp = dot_product(dx(ix:(n-1)*incx+ix:incx),dy(iy:(n-1)*incy+iy:incy)&
          ) 
        ddot = dtemp 
        return  
      endif 
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
      m = mod(n,5) 
      if (m == 0) go to 40 
      dtemp = dot_product(dx(:m),dy(:m)) 
      if (n < 5) go to 60 
   40 continue 
      mp1 = m + 1 
      dtemp = dtemp + dot_product(dx(mp1:((n-mp1+5)/5)*5-1+mp1),dy(mp1:((n-mp1+&
        5)/5)*5-1+mp1)) 
   60 continue 
      ddot = dtemp 
      return  
      end function ddot 


      subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c&
        , ldc) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: m 
      integer , intent(in) :: n 
      integer , intent(in) :: k 
      integer , intent(in) :: lda 
      integer , intent(in) :: ldb 
      integer , intent(in) :: ldc 
      real(double) , intent(in) :: alpha 
      real(double) , intent(in) :: beta 
      character  :: transa*(*) 
      character  :: transb*(*) 
      real(double) , intent(in) :: a(lda,*) 
      real(double) , intent(in) :: b(ldb,*) 
      real(double) , intent(inout) :: c(ldc,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, info, j, l, nrowa, nrowb 
      real(double) :: temp 
      logical :: nota, notb 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!     .. SCALAR ARGUMENTS ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DGEMM  PERFORMS ONE OF THE MATRIX-MATRIX OPERATIONS
!
!     C := ALPHA*OP( A )*OP( B ) + BETA*C,
!
!  WHERE  OP( X ) IS ONE OF
!
!     OP( X ) = X   OR   OP( X ) = X',
!
!  ALPHA AND BETA ARE SCALARS, AND A, B AND C ARE MATRICES, WITH OP( A )
!  AN M BY K MATRIX,  OP( B )  A  K BY N MATRIX AND  C AN M BY N MATRIX.
!
!  PARAMETERS
!  ==========
!
!  TRANSA - CHARACTER*1.
!           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
!           THE MATRIX MULTIPLICATION AS FOLLOWS:
!
!              TRANSA = 'N' OR 'N',  OP( A ) = A.
!
!              TRANSA = 'T' OR 'T',  OP( A ) = A'.
!
!              TRANSA = 'C' OR 'C',  OP( A ) = A'.
!
!           UNCHANGED ON EXIT.
!
!  TRANSB - CHARACTER*1.
!           ON ENTRY, TRANSB SPECIFIES THE FORM OF OP( B ) TO BE USED IN
!           THE MATRIX MULTIPLICATION AS FOLLOWS:
!
!              TRANSB = 'N' OR 'N',  OP( B ) = B.
!
!              TRANSB = 'T' OR 'T',  OP( B ) = B'.
!
!              TRANSB = 'C' OR 'C',  OP( B ) = B'.
!
!           UNCHANGED ON EXIT.
!
!  M      - INTEGER.
!           ON ENTRY,  M  SPECIFIES  THE NUMBER  OF ROWS  OF THE  MATRIX
!           OP( A )  AND OF THE  MATRIX  C.  M  MUST  BE AT LEAST  ZERO.
!           UNCHANGED ON EXIT.
!
!  N      - INTEGER.
!           ON ENTRY,  N  SPECIFIES THE NUMBER  OF COLUMNS OF THE MATRIX
!           OP( B ) AND THE NUMBER OF COLUMNS OF THE MATRIX C. N MUST BE
!           AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  K      - INTEGER.
!           ON ENTRY,  K  SPECIFIES  THE NUMBER OF COLUMNS OF THE MATRIX
!           OP( A ) AND THE NUMBER OF ROWS OF THE MATRIX OP( B ). K MUST
!           BE AT LEAST  ZERO.
!           UNCHANGED ON EXIT.
!
!  ALPHA  - DOUBLE PRECISION.
!           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
!           UNCHANGED ON EXIT.
!
!  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, KA ), WHERE KA IS
!           K  WHEN  TRANSA = 'N' OR 'N',  AND IS  M  OTHERWISE.
!           BEFORE ENTRY WITH  TRANSA = 'N' OR 'N',  THE LEADING  M BY K
!           PART OF THE ARRAY  A  MUST CONTAIN THE MATRIX  A,  OTHERWISE
!           THE LEADING  K BY M  PART OF THE ARRAY  A  MUST CONTAIN  THE
!           MATRIX A.
!           UNCHANGED ON EXIT.
!
!  LDA    - INTEGER.
!           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
!           IN THE CALLING (SUB) PROGRAM. WHEN  TRANSA = 'N' OR 'N' THEN
!           LDA MUST BE AT LEAST  MAX( 1, M ), OTHERWISE  LDA MUST BE AT
!           LEAST  MAX( 1, K ).
!           UNCHANGED ON EXIT.
!
!  B      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDB, KB ), WHERE KB IS
!           N  WHEN  TRANSB = 'N' OR 'N',  AND IS  K  OTHERWISE.
!           BEFORE ENTRY WITH  TRANSB = 'N' OR 'N',  THE LEADING  K BY N
!           PART OF THE ARRAY  B  MUST CONTAIN THE MATRIX  B,  OTHERWISE
!           THE LEADING  N BY K  PART OF THE ARRAY  B  MUST CONTAIN  THE
!           MATRIX B.
!           UNCHANGED ON EXIT.
!
!  LDB    - INTEGER.
!           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
!           IN THE CALLING (SUB) PROGRAM. WHEN  TRANSB = 'N' OR 'N' THEN
!           LDB MUST BE AT LEAST  MAX( 1, K ), OTHERWISE  LDB MUST BE AT
!           LEAST  MAX( 1, N ).
!           UNCHANGED ON EXIT.
!
!  BETA   - DOUBLE PRECISION.
!           ON ENTRY,  BETA  SPECIFIES THE SCALAR  BETA.  WHEN  BETA  IS
!           SUPPLIED AS ZERO THEN C NEED NOT BE SET ON INPUT.
!           UNCHANGED ON EXIT.
!
!  C      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDC, N ).
!           BEFORE ENTRY, THE LEADING  M BY N  PART OF THE ARRAY  C MUST
!           CONTAIN THE MATRIX  C,  EXCEPT WHEN  BETA  IS ZERO, IN WHICH
!           CASE C NEED NOT BE SET ON ENTRY.
!           ON EXIT, THE ARRAY  C  IS OVERWRITTEN BY THE  M BY N  MATRIX
!           ( ALPHA*OP( A )*OP( B ) + BETA*C ).
!
!  LDC    - INTEGER.
!           ON ENTRY, LDC SPECIFIES THE FIRST DIMENSION OF C AS DECLARED
!           IN  THE  CALLING  (SUB)  PROGRAM.   LDC  MUST  BE  AT  LEAST
!           MAX( 1, M ).
!           UNCHANGED ON EXIT.
!
!
!  LEVEL 3 BLAS ROUTINE.
!
!  -- WRITTEN ON 8-FEBRUARY-1989.
!     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
!     IAIN DUFF, AERE HARWELL.
!     JEREMY DU CROZ, NUMERICAL ALGORITHMS GROUP LTD.
!     SVEN HAMMARLING, NUMERICAL ALGORITHMS GROUP LTD.
!
!
!     .. EXTERNAL FUNCTIONS ..
!     .. EXTERNAL SUBROUTINES ..
!     .. INTRINSIC FUNCTIONS ..
!     .. LOCAL SCALARS ..
!     .. PARAMETERS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     SET  NOTA  AND  NOTB  AS  TRUE IF  A  AND  B  RESPECTIVELY ARE NOT
!     TRANSPOSED AND SET  NROWA, NCOLA AND  NROWB  AS THE NUMBER OF ROWS
!     AND  COLUMNS OF  A  AND THE  NUMBER OF  ROWS  OF  B  RESPECTIVELY.
!
      nota = lsame(transa,'N') 
      notb = lsame(transb,'N') 
      if (nota) then 
        nrowa = m 
      else 
        nrowa = k 
      endif 
      if (notb) then 
        nrowb = k 
      else 
        nrowb = n 
      endif 
!
!     TEST THE INPUT PARAMETERS.
!
      info = 0 
      if (.not.nota .and. .not.lsame(transa,'C') .and. .not.lsame(transa,'T')) &
        then 
        info = 1 
      else if (.not.notb .and. .not.lsame(transb,'C') .and. .not.lsame(transb,&
          'T')) then 
        info = 2 
      else if (m < 0) then 
        info = 3 
      else if (n < 0) then 
        info = 4 
      else if (k < 0) then 
        info = 5 
      else if (lda < max(1,nrowa)) then 
        info = 8 
      else if (ldb < max(1,nrowb)) then 
        info = 10 
      else if (ldc < max(1,m)) then 
        info = 13 
      endif 
      if (info /= 0) then 
        call xerbla ('DGEMM ', info) 
        return  
      endif 
!
!     QUICK RETURN IF POSSIBLE.
!
      if (m==0 .or. n==0 .or. (Abs(alpha) < 1.d-20 .or. k==0) .and. Abs(beta - 1.d0) < 1.d-20) return  
!
!     AND IF  ALPHA.EQ.ZERO.
!
      if (Abs(alpha) < 1.d-20) then 
        if (Abs(beta - 1.d0) < 1.d-20) then 
          c(:m,:n) = zero 
        else 
          c(:m,:n) = beta*c(:m,:n) 
        endif 
        return  
      endif 
!
!     START THE OPERATIONS.
!
      if (notb) then 
        if (nota) then 
!
!           FORM  C := ALPHA*A*B + BETA*C.
!
          do j = 1, n 
            if (beta == zero) then 
              c(:m,j) = zero 
            else if (Abs(beta - 1.d0) > 1.d-20) then 
              c(:m,j) = beta*c(:m,j) 
            endif 
            do l = 1, k 
              if (b(l,j) == zero) cycle  
              temp = alpha*b(l,j) 
              c(:m,j) = c(:m,j) + temp*a(:m,l) 
            end do 
          end do 
        else 
!
!           FORM  C := ALPHA*A'*B + BETA*C
!
          do j = 1, n 
            do i = 1, m 
              temp = zero 
              temp = temp + sum(a(:k,i)*b(:k,j)) 
              if (beta == zero) then 
                c(i,j) = alpha*temp 
              else 
                c(i,j) = alpha*temp + beta*c(i,j) 
              endif 
            end do 
          end do 
        endif 
      else 
        if (nota) then 
!
!           FORM  C := ALPHA*A*B' + BETA*C
!
          do j = 1, n 
            if (beta == zero) then 
              c(:m,j) = zero 
            else if (Abs(beta-1.d0) > 1.d-20) then 
              c(:m,j) = beta*c(:m,j) 
            endif 
            do l = 1, k 
              if (b(j,l) == zero) cycle  
              temp = alpha*b(j,l) 
              c(:m,j) = c(:m,j) + temp*a(:m,l) 
            end do 
          end do 
        else 
!
!           FORM  C := ALPHA*A'*B' + BETA*C
!
          do j = 1, n 
            do i = 1, m 
              temp = zero 
              temp = temp + sum(a(:k,i)*b(j,:k)) 
              if (beta == zero) then 
                c(i,j) = alpha*temp 
              else 
                c(i,j) = alpha*temp + beta*c(i,j) 
              endif 
            end do 
          end do 
        endif 
      endif 
!
      return  
!
!     END OF DGEMM .
!
      end subroutine dgemm 


      subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: m 
      integer , intent(in) :: n 
      integer , intent(in) :: lda 
      integer , intent(in) :: incx 
      integer , intent(in) :: incy 
      real(double) , intent(in) :: alpha 
      real(double) , intent(in) :: beta 
      character  :: trans*(*) 
      real(double) , intent(in) :: a(lda,*) 
      real(double) , intent(in) :: x(*) 
      real(double) , intent(inout) :: y(*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: info, ix, iy, j, jx, jy, kx, ky, lenx, leny 
      real(double) :: temp 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!     .. SCALAR ARGUMENTS ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DGEMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
!
!     Y := ALPHA*A*X + BETA*Y,   OR   Y := ALPHA*A'*X + BETA*Y,
!
!  WHERE ALPHA AND BETA ARE SCALARS, X AND Y ARE VECTORS AND A IS AN
!  M BY N MATRIX.
!
!  PARAMETERS
!  ==========
!
!  TRANS  - CHARACTER*1.
!           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
!           FOLLOWS:
!
!              TRANS = 'N' OR 'N'   Y := ALPHA*A*X + BETA*Y.
!
!              TRANS = 'T' OR 'T'   Y := ALPHA*A'*X + BETA*Y.
!
!              TRANS = 'C' OR 'C'   Y := ALPHA*A'*X + BETA*Y.
!
!           UNCHANGED ON EXIT.
!
!  M      - INTEGER.
!           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
!           M MUST BE AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  N      - INTEGER.
!           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
!           N MUST BE AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  ALPHA  - DOUBLE PRECISION.
!           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
!           UNCHANGED ON EXIT.
!
!  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, N ).
!           BEFORE ENTRY, THE LEADING M BY N PART OF THE ARRAY A MUST
!           CONTAIN THE MATRIX OF COEFFICIENTS.
!           UNCHANGED ON EXIT.
!
!  LDA    - INTEGER.
!           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
!           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
!           MAX( 1, M ).
!           UNCHANGED ON EXIT.
!
!  X      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
!           ( 1 + ( N - 1 )*ABS( INCX ) ) WHEN TRANS = 'N' OR 'N'
!           AND AT LEAST
!           ( 1 + ( M - 1 )*ABS( INCX ) ) OTHERWISE.
!           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE
!           VECTOR X.
!           UNCHANGED ON EXIT.
!
!  INCX   - INTEGER.
!           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
!           X. INCX MUST NOT BE ZERO.
!           UNCHANGED ON EXIT.
!
!  BETA   - DOUBLE PRECISION.
!           ON ENTRY, BETA SPECIFIES THE SCALAR BETA. WHEN BETA IS
!           SUPPLIED AS ZERO THEN Y NEED NOT BE SET ON INPUT.
!           UNCHANGED ON EXIT.
!
!  Y      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
!           ( 1 + ( M - 1 )*ABS( INCY ) ) WHEN TRANS = 'N' OR 'N'
!           AND AT LEAST
!           ( 1 + ( N - 1 )*ABS( INCY ) ) OTHERWISE.
!           BEFORE ENTRY WITH BETA NON-ZERO, THE INCREMENTED ARRAY Y
!           MUST CONTAIN THE VECTOR Y. ON EXIT, Y IS OVERWRITTEN BY THE
!           UPDATED VECTOR Y.
!
!  INCY   - INTEGER.
!           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
!           Y. INCY MUST NOT BE ZERO.
!           UNCHANGED ON EXIT.
!
!
!  LEVEL 2 BLAS ROUTINE.
!
!  -- WRITTEN ON 22-OCTOBER-1986.
!     JACK DONGARRA, ARGONNE NATIONAL LAB.
!     JEREMY DU CROZ, NAG CENTRAL OFFICE.
!     SVEN HAMMARLING, NAG CENTRAL OFFICE.
!     RICHARD HANSON, SANDIA NATIONAL LABS.
!
!
!     .. PARAMETERS ..
!     .. LOCAL SCALARS ..
!     .. EXTERNAL FUNCTIONS ..
!     .. EXTERNAL SUBROUTINES ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      info = 0 
      if (.not.lsame(trans,'N') .and. .not.lsame(trans,'T') .and. .not.lsame(&
        trans,'C')) then 
        info = 1 
      else if (m < 0) then 
        info = 2 
      else if (n < 0) then 
        info = 3 
      else if (lda < max(1,m)) then 
        info = 6 
      else if (incx == 0) then 
        info = 8 
      else if (incy == 0) then 
        info = 11 
      endif 
      if (info /= 0) then 
        call xerbla ('DGEMV ', info) 
        return  
      endif 
!
!     QUICK RETURN IF POSSIBLE.
!
      if (m==0 .or. n==0 .or. Abs(alpha) < 1.d-20 .and. Abs(beta - 1.d0) < 1.d-20) return  
!
!     SET  LENX  AND  LENY, THE LENGTHS OF THE VECTORS X AND Y, AND SET
!     UP THE START POINTS IN  X  AND  Y.
!
      if (lsame(trans,'N')) then 
        lenx = n 
        leny = m 
      else 
        lenx = m 
        leny = n 
      endif 
      if (incx > 0) then 
        kx = 1 
      else 
        kx = 1 - (lenx - 1)*incx 
      endif 
      if (incy > 0) then 
        ky = 1 
      else 
        ky = 1 - (leny - 1)*incy 
      endif 
!
!     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
!     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
!
!     FIRST FORM  Y := BETA*Y.
!
      if (Abs(beta - 1.d0) > 1.d-20) then 
        if (incy == 1) then 
          if (beta == zero) then 
            y(:leny) = zero 
          else 
            y(:leny) = beta*y(:leny) 
          endif 
        else 
          iy = ky 
          if (Abs(beta) < 1.d-20) then 
            y(iy:(leny-1)*incy+iy:incy) = zero 
          else 
            y(iy:(leny-1)*incy+iy:incy) = beta*y(iy:(leny-1)*incy+iy:incy) 
          endif 
        endif 
      endif 
      if (Abs(alpha) < 1.d-20) return  
      if (lsame(trans,'N')) then 
!
!        FORM  Y := ALPHA*A*X + Y.
!
        jx = kx 
        if (incy == 1) then 
          do j = 1, n 
            if (x(jx) /= zero) then 
              temp = alpha*x(jx) 
              y(:m) = y(:m) + temp*a(:m,j) 
            endif 
            jx = jx + incx 
          end do 
        else 
          do j = 1, n 
            if (x(jx) /= zero) then 
              temp = alpha*x(jx) 
              iy = ky 
              y(iy:(m-1)*incy+iy:incy) = y(iy:(m-1)*incy+iy:incy) + temp*a(:m,j&
                ) 
            endif 
            jx = jx + incx 
          end do 
        endif 
      else 
!
!        FORM  Y := ALPHA*A'*X + Y.
!
        jy = ky 
        if (incx == 1) then 
          do j = 1, n 
            temp = zero 
            temp = temp + sum(a(:m,j)*x(:m)) 
            y(jy) = y(jy) + alpha*temp 
            jy = jy + incy 
          end do 
        else 
          do j = 1, n 
            temp = zero 
            ix = kx 
            temp = temp + sum(a(:m,j)*x(ix:(m-1)*incx+ix:incx)) 
            y(jy) = y(jy) + alpha*temp 
            jy = jy + incy 
          end do 
        endif 
      endif 
!
      return  
!
!     END OF DGEMV .
!
      end subroutine dgemv 


      subroutine dger(m, n, alpha, x, incx, y, incy, a, lda) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: m 
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      integer , intent(in) :: incy 
      integer , intent(in) :: lda 
      real(double) , intent(in) :: alpha 
      real(double) , intent(in) :: x(*) 
      real(double) , intent(in) :: y(*) 
      real(double) , intent(inout) :: a(lda,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: info, ix, j, jy, kx 
      real(double) :: temp 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!     .. SCALAR ARGUMENTS ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DGER   PERFORMS THE RANK 1 OPERATION
!
!     A := ALPHA*X*Y' + A,
!
!  WHERE ALPHA IS A SCALAR, X IS AN M ELEMENT VECTOR, Y IS AN N ELEMENT
!  VECTOR AND A IS AN M BY N MATRIX.
!
!  PARAMETERS
!  ==========
!
!  M      - INTEGER.
!           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
!           M MUST BE AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  N      - INTEGER.
!           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
!           N MUST BE AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  ALPHA  - DOUBLE PRECISION.
!           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
!           UNCHANGED ON EXIT.
!
!  X      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
!           ( 1 + ( M - 1 )*ABS( INCX ) ).
!           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE M
!           ELEMENT VECTOR X.
!           UNCHANGED ON EXIT.
!
!  INCX   - INTEGER.
!           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
!           X. INCX MUST NOT BE ZERO.
!           UNCHANGED ON EXIT.
!
!  Y      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
!           ( 1 + ( N - 1 )*ABS( INCY ) ).
!           BEFORE ENTRY, THE INCREMENTED ARRAY Y MUST CONTAIN THE N
!           ELEMENT VECTOR Y.
!           UNCHANGED ON EXIT.
!
!  INCY   - INTEGER.
!           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
!           Y. INCY MUST NOT BE ZERO.
!           UNCHANGED ON EXIT.
!
!  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, N ).
!           BEFORE ENTRY, THE LEADING M BY N PART OF THE ARRAY A MUST
!           CONTAIN THE MATRIX OF COEFFICIENTS. ON EXIT, A IS
!           OVERWRITTEN BY THE UPDATED MATRIX.
!
!  LDA    - INTEGER.
!           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
!           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
!           MAX( 1, M ).
!           UNCHANGED ON EXIT.
!
!
!  LEVEL 2 BLAS ROUTINE.
!
!  -- WRITTEN ON 22-OCTOBER-1986.
!     JACK DONGARRA, ARGONNE NATIONAL LAB.
!     JEREMY DU CROZ, NAG CENTRAL OFFICE.
!     SVEN HAMMARLING, NAG CENTRAL OFFICE.
!     RICHARD HANSON, SANDIA NATIONAL LABS.
!
!
!     .. PARAMETERS ..
!     .. LOCAL SCALARS ..
!     .. EXTERNAL SUBROUTINES ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      info = 0 
      if (m < 0) then 
        info = 1 
      else if (n < 0) then 
        info = 2 
      else if (incx == 0) then 
        info = 5 
      else if (incy == 0) then 
        info = 7 
      else if (lda < max(1,m)) then 
        info = 9 
      endif 
      if (info /= 0) then 
        call xerbla ('DGER  ', info) 
        return  
      endif 
!
!     QUICK RETURN IF POSSIBLE.
!
      if (m==0 .or. n==0 .or. alpha==zero) return  
!
!     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
!     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
!
      if (incy > 0) then 
        jy = 1 
      else 
        jy = 1 - (n - 1)*incy 
      endif 
      if (incx == 1) then 
        do j = 1, n 
          if (y(jy) /= zero) then 
            temp = alpha*y(jy) 
            a(:m,j) = a(:m,j) + x(:m)*temp 
          endif 
          jy = jy + incy 
        end do 
      else 
        if (incx > 0) then 
          kx = 1 
        else 
          kx = 1 - (m - 1)*incx 
        endif 
        do j = 1, n 
          if (y(jy) /= zero) then 
            temp = alpha*y(jy) 
            ix = kx 
            a(:m,j) = a(:m,j) + x(ix:(m-1)*incx+ix:incx)*temp 
          endif 
          jy = jy + incy 
        end do 
      endif 
!
      return  
!
!     END OF DGER  .
!
      end subroutine dger 


!*MODULE BLAS1   *DECK DNRM2
      real(kind(0.0d0)) function dnrm2 (n, dx, incx) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      real(double) , intent(in) :: dx(n) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: next, j, nn, i 
      real(double) :: cutlo, cuthi, hitest, sum, xmax, zero, one 

      save cutlo, cuthi, zero, one 
!-----------------------------------------------
      data zero, one/ 0.0D+00, 1.0D+00/  
!
!     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
!     INCREMENT INCX .
!     IF    N .LE. 0 RETURN WITH RESULT = 0.
!     IF N .GE. 1 THEN INCX MUST BE .GE. 1
!
!           C.L.LAWSON, 1978 JAN 08
!
!     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
!     HOPEFULLY APPLICABLE TO ALL MACHINES.
!         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES.
!         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES.
!     WHERE
!         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
!         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
!         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
!
!     BRIEF OUTLINE OF ALGORITHM..
!
!     PHASE 1    SCANS ZERO COMPONENTS.
!     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
!     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
!     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
!     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
!
!     VALUES FOR CUTLO AND CUTHI..
!     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
!     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
!     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
!                   UNIVAC AND DEC AT 2**(-103)
!                   THUS CUTLO = 2**(-51) = 4.44089E-16
!     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
!                   THUS CUTHI = 2**(63.5) = 1.30438E19
!     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
!                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
!     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D+19
!     DATA CUTLO, CUTHI / 8.232D-11,  1.304D+19 /
!     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
      data cutlo, cuthi/ 8.232D-11, 1.304D+19/  
      data j/ 0/  
!
      if (n <= 0) then 
        dnrm2 = zero 
      else 
!
        next = 30 
        sum = zero 
        nn = n*incx 
!                                                 BEGIN MAIN LOOP
        i = 1 
   20   continue 
        if (next == 30) go to 30 
        if (next == 40) go to 40 
        if (next == 70) go to 70 
        if (next == 80) go to 80 
   30   continue 
        if (abs(dx(i)) > cutlo) go to 110 
        next = 40 
        xmax = zero 
!
!                        PHASE 1.  SUM IS ZERO
!
   40   continue 
        if (Abs(dx(i)) < 1.d-20) go to 130 
        if (abs(dx(i)) > cutlo) go to 110 
!
!                                PREPARE FOR PHASE 2.
        next = 70 
        go to 60 
!
!                                PREPARE FOR PHASE 4.
!
   50   continue 
        i = j 
        next = 80 
        sum = (sum/dx(i))/dx(i) 
   60   continue 
        xmax = abs(dx(i)) 
        go to 90 
!
!                   PHASE 2.  SUM IS SMALL.
!                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
!
   70   continue 
        if (abs(dx(i)) > cutlo) go to 100 
!
!                     COMMON CODE FOR PHASES 2 AND 4.
!                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
!
   80   continue 
        if (abs(dx(i)) <= xmax) go to 90 
        sum = one + sum*(xmax/dx(i))**2 
        xmax = abs(dx(i)) 
        go to 130 
!
   90   continue 
        sum = sum + (dx(i)/xmax)**2 
        go to 130 
!
!
!                  PREPARE FOR PHASE 3.
!
  100   continue 
        sum = (sum*xmax)*xmax 
!
!
!     FOR REAL OR D.P. SET HITEST = CUTHI/N
!     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
!
  110   continue 
        hitest = cuthi/dble(n) 
!
!                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
!
        do j = i, nn, incx 
          if (abs(dx(j)) >= hitest) go to 50 
          sum = sum + dx(j)**2 
        end do 
        dnrm2 = sqrt(sum) 
        go to 140 
!
  130   continue 
        i = i + incx 
        if (i <= nn) go to 20 
!
!              END OF MAIN LOOP.
!
!              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
!
        dnrm2 = xmax*sqrt(sum) 
      endif 
  140 continue 
      return  
      end function dnrm2 


      subroutine dscal(n, da, dx, incx) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      real(double) , intent(in) :: da 
      real(double) , intent(inout) :: dx(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: m, mp1, nincx 
!-----------------------------------------------
!
!     SCALES A VECTOR BY A CONSTANT.
!     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
!
      if (n <= 0) return  
      if (incx /= 1) then 
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
        nincx = n*incx 
        dx(:nincx:incx) = da*dx(:nincx:incx) 
        return  
      endif 
!
!        CODE FOR INCREMENT EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
      m = mod(n,5) 
      if (m /= 0) then 
        dx(:m) = da*dx(:m) 
        if (n < 5) return  
      endif 
      mp1 = m + 1 
      dx(mp1:((n-mp1+5)/5)*5-1+mp1) = da*dx(mp1:((n-mp1+5)/5)*5-1+mp1) 
      return  
      end subroutine dscal 


      subroutine dswap(n, dx, incx, dy, incy) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      integer , intent(in) :: incy 
      real(double) , intent(inout) :: dx(*) 
      real(double) , intent(inout) :: dy(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, ix, iy, m, mp1 
      real(double) :: dtemp 
!-----------------------------------------------
!
!     INTERCHANGES TWO VECTORS.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
!
      if (n <= 0) return  
      if (incx/=1 .or. incy/=1) then 
!
!       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
!         TO 1
!
        ix = 1 
        iy = 1 
        if (incx < 0) ix = ((-n) + 1)*incx + 1 
        if (incy < 0) iy = ((-n) + 1)*incy + 1 
        do i = 1, n 
          dtemp = dx(ix) 
          dx(ix) = dy(iy) 
          dy(iy) = dtemp 
          ix = ix + incx 
          iy = iy + incy 
        end do 
        return  
      endif 
!
!       CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!       CLEAN-UP LOOP
!
      m = mod(n,3) 
      if (m /= 0) then 
        do i = 1, m 
          dtemp = dx(i) 
          dx(i) = dy(i) 
          dy(i) = dtemp 
        end do 
        if (n < 3) return  
      endif 
      mp1 = m + 1 
      do i = mp1, n, 3 
        dtemp = dx(i) 
        dx(i) = dy(i) 
        dy(i) = dtemp 
        dtemp = dx(i+1) 
        dx(i+1) = dy(i+1) 
        dy(i+1) = dtemp 
        dtemp = dx(i+2) 
        dx(i+2) = dy(i+2) 
        dy(i+2) = dtemp 
      end do 
      return  
      end subroutine dswap 


      subroutine dtrmm(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: m 
      integer , intent(in) :: n 
      integer , intent(in) :: lda 
      integer , intent(in) :: ldb 
      real(double) , intent(in) :: alpha 
      character  :: side*(*) 
      character  :: uplo*(*) 
      character  :: transa*(*) 
      character  :: diag*(*) 
      real(double) , intent(in) :: a(lda,*) 
      real(double) , intent(inout) :: b(ldb,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, info, j, k, nrowa 
      real(double) :: temp 
      logical :: lside, nounit, upper 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!     .. SCALAR ARGUMENTS ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DTRMM  PERFORMS ONE OF THE MATRIX-MATRIX OPERATIONS
!
!     B := ALPHA*OP( A )*B,   OR   B := ALPHA*B*OP( A ),
!
!  WHERE  ALPHA  IS A SCALAR,  B  IS AN M BY N MATRIX,  A  IS A UNIT, OR
!  NON-UNIT,  UPPER OR LOWER TRIANGULAR MATRIX  AND  OP( A )  IS ONE  OF
!
!     OP( A ) = A   OR   OP( A ) = A'.
!
!  PARAMETERS
!  ==========
!
!  SIDE   - CHARACTER*1.
!           ON ENTRY,  SIDE SPECIFIES WHETHER  OP( A ) MULTIPLIES B FROM
!           THE LEFT OR RIGHT AS FOLLOWS:
!
!              SIDE = 'L' OR 'L'   B := ALPHA*OP( A )*B.
!
!              SIDE = 'R' OR 'R'   B := ALPHA*B*OP( A ).
!
!           UNCHANGED ON EXIT.
!
!  UPLO   - CHARACTER*1.
!           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX A IS AN UPPER OR
!           LOWER TRIANGULAR MATRIX AS FOLLOWS:
!
!              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
!
!              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
!
!           UNCHANGED ON EXIT.
!
!  TRANSA - CHARACTER*1.
!           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
!           THE MATRIX MULTIPLICATION AS FOLLOWS:
!
!              TRANSA = 'N' OR 'N'   OP( A ) = A.
!
!              TRANSA = 'T' OR 'T'   OP( A ) = A'.
!
!              TRANSA = 'C' OR 'C'   OP( A ) = A'.
!
!           UNCHANGED ON EXIT.
!
!  DIAG   - CHARACTER*1.
!           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT TRIANGULAR
!           AS FOLLOWS:
!
!              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
!
!              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
!                                  TRIANGULAR.
!
!           UNCHANGED ON EXIT.
!
!  M      - INTEGER.
!           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF B. M MUST BE AT
!           LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  N      - INTEGER.
!           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF B.  N MUST BE
!           AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  ALPHA  - DOUBLE PRECISION.
!           ON ENTRY,  ALPHA SPECIFIES THE SCALAR  ALPHA. WHEN  ALPHA IS
!           ZERO THEN  A IS NOT REFERENCED AND  B NEED NOT BE SET BEFORE
!           ENTRY.
!           UNCHANGED ON EXIT.
!
!  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, K ), WHERE K IS M
!           WHEN  SIDE = 'L' OR 'L'  AND IS  N  WHEN  SIDE = 'R' OR 'R'.
!           BEFORE ENTRY  WITH  UPLO = 'U' OR 'U',  THE  LEADING  K BY K
!           UPPER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE UPPER
!           TRIANGULAR MATRIX  AND THE STRICTLY LOWER TRIANGULAR PART OF
!           A IS NOT REFERENCED.
!           BEFORE ENTRY  WITH  UPLO = 'L' OR 'L',  THE  LEADING  K BY K
!           LOWER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE LOWER
!           TRIANGULAR MATRIX  AND THE STRICTLY UPPER TRIANGULAR PART OF
!           A IS NOT REFERENCED.
!           NOTE THAT WHEN  DIAG = 'U' OR 'U',  THE DIAGONAL ELEMENTS OF
!           A  ARE NOT REFERENCED EITHER,  BUT ARE ASSUMED TO BE  UNITY.
!           UNCHANGED ON EXIT.
!
!  LDA    - INTEGER.
!           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
!           IN THE CALLING (SUB) PROGRAM.  WHEN  SIDE = 'L' OR 'L'  THEN
!           LDA  MUST BE AT LEAST  MAX( 1, M ),  WHEN  SIDE = 'R' OR 'R'
!           THEN LDA MUST BE AT LEAST MAX( 1, N ).
!           UNCHANGED ON EXIT.
!
!  B      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDB, N ).
!           BEFORE ENTRY,  THE LEADING  M BY N PART OF THE ARRAY  B MUST
!           CONTAIN THE MATRIX  B,  AND  ON EXIT  IS OVERWRITTEN  BY THE
!           TRANSFORMED MATRIX.
!
!  LDB    - INTEGER.
!           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
!           IN  THE  CALLING  (SUB)  PROGRAM.   LDB  MUST  BE  AT  LEAST
!           MAX( 1, M ).
!           UNCHANGED ON EXIT.
!
!
!  LEVEL 3 BLAS ROUTINE.
!
!  -- WRITTEN ON 8-FEBRUARY-1989.
!     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
!     IAIN DUFF, AERE HARWELL.
!     JEREMY DU CROZ, NUMERICAL ALGORITHMS GROUP LTD.
!     SVEN HAMMARLING, NUMERICAL ALGORITHMS GROUP LTD.
!
!
!     .. EXTERNAL FUNCTIONS ..
!     .. EXTERNAL SUBROUTINES ..
!        EXTERNAL           XRBLA
!     .. INTRINSIC FUNCTIONS ..
!     .. LOCAL SCALARS ..
!     .. PARAMETERS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      lside = lsame(side,'L') 
      if (lside) then 
        nrowa = m 
      else 
        nrowa = n 
      endif 
      nounit = lsame(diag,'N') 
      upper = lsame(uplo,'U') 
!
      info = 0 
      if (.not.lside .and. .not.lsame(side,'R')) then 
        info = 1 
      else if (.not.upper .and. .not.lsame(uplo,'L')) then 
        info = 2 
      else if (.not.lsame(transa,'N') .and. .not.lsame(transa,'T') .and. .not.&
          lsame(transa,'C')) then 
        info = 3 
      else if (.not.lsame(diag,'U') .and. .not.lsame(diag,'N')) then 
        info = 4 
      else if (m < 0) then 
        info = 5 
      else if (n < 0) then 
        info = 6 
      else if (lda < max(1,nrowa)) then 
        info = 9 
      else if (ldb < max(1,m)) then 
        info = 11 
      endif 
      if (info /= 0) then 
        call xerbla ('DTRMM ', info) 
        return  
      endif 
!
!     QUICK RETURN IF POSSIBLE.
!
      if (n == 0) return  
!
!     AND WHEN  ALPHA.EQ.ZERO.
!
      if (alpha == zero) then 
        b(:m,:n) = zero 
        return  
      endif 
!
!     START THE OPERATIONS.
!
      if (lside) then 
        if (lsame(transa,'N')) then 
!
!           FORM  B := ALPHA*A*B.
!
          if (upper) then 
            do j = 1, n 
              do k = 1, m 
                if (b(k,j) == zero) cycle  
                temp = alpha*b(k,j) 
                b(:k-1,j) = b(:k-1,j) + temp*a(:k-1,k) 
                if (nounit) temp = temp*a(k,k) 
                b(k,j) = temp 
              end do 
            end do 
          else 
            do j = 1, n 
              do k = m, 1, -1 
                if (b(k,j) == zero) cycle  
                temp = alpha*b(k,j) 
                b(k,j) = temp 
                if (nounit) b(k,j) = b(k,j)*a(k,k) 
                b(k+1:m,j) = b(k+1:m,j) + temp*a(k+1:m,k) 
              end do 
            end do 
          endif 
        else 
!
!           FORM  B := ALPHA*B*A'.
!
          if (upper) then 
            do j = 1, n 
              do i = m, 1, -1 
                temp = b(i,j) 
                if (nounit) temp = temp*a(i,i) 
                temp = temp + sum(a(:i-1,i)*b(:i-1,j)) 
                b(i,j) = alpha*temp 
              end do 
            end do 
          else 
            do j = 1, n 
              do i = 1, m 
                temp = b(i,j) 
                if (nounit) temp = temp*a(i,i) 
                temp = temp + sum(a(i+1:m,i)*b(i+1:m,j)) 
                b(i,j) = alpha*temp 
              end do 
            end do 
          endif 
        endif 
      else 
        if (lsame(transa,'N')) then 
!
!           FORM  B := ALPHA*B*A.
!
          if (upper) then 
            do j = n, 1, -1 
              temp = alpha 
              if (nounit) temp = temp*a(j,j) 
              b(:m,j) = temp*b(:m,j) 
              do k = 1, j - 1 
                if (a(k,j) == zero) cycle  
                temp = alpha*a(k,j) 
                b(:m,j) = b(:m,j) + temp*b(:m,k) 
              end do 
            end do 
          else 
            do j = 1, n 
              temp = alpha 
              if (nounit) temp = temp*a(j,j) 
              b(:m,j) = temp*b(:m,j) 
              do k = j + 1, n 
                if (a(k,j) == zero) cycle  
                temp = alpha*a(k,j) 
                b(:m,j) = b(:m,j) + temp*b(:m,k) 
              end do 
            end do 
          endif 
        else 
!
!           FORM  B := ALPHA*B*A'.
!
          if (upper) then 
            do k = 1, n 
              do j = 1, k - 1 
                if (a(j,k) == zero) cycle  
                temp = alpha*a(j,k) 
                b(:m,j) = b(:m,j) + temp*b(:m,k) 
              end do 
              temp = alpha 
              if (nounit) temp = temp*a(k,k) 
              if (Abs(temp - 1.d0) < 1.d-20) cycle  
              b(:m,k) = temp*b(:m,k) 
            end do 
          else 
            do k = n, 1, -1 
              do j = k + 1, n 
                if (a(j,k) == zero) cycle  
                temp = alpha*a(j,k) 
                b(:m,j) = b(:m,j) + temp*b(:m,k) 
              end do 
              temp = alpha 
              if (nounit) temp = temp*a(k,k) 
              if (Abs(temp - 1.d0) < 1.d-20) cycle  
              b(:m,k) = temp*b(:m,k) 
            end do 
          endif 
        endif 
      endif 
!
      return  
!
!     END OF DTRMM .
!
      end subroutine dtrmm 


      subroutine dtrmv(uplo, trans, diag, n, a, lda, x, incx) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: lda 
      integer , intent(in) :: incx 
      character  :: uplo*(*) 
      character  :: trans*(*) 
      character  :: diag*(*) 
      real(double) , intent(in) :: a(lda,*) 
      real(double) , intent(inout) :: x(*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: info, ix, j, jx, kx 
      real(double) :: temp 
      logical :: nounit 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!     .. SCALAR ARGUMENTS ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DTRMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
!
!     X := A*X,   OR   X := A'*X,
!
!  WHERE X IS AN N ELEMENT VECTOR AND  A IS AN N BY N UNIT, OR NON-UNIT,
!  UPPER OR LOWER TRIANGULAR MATRIX.
!
!  PARAMETERS
!  ==========
!
!  UPLO   - CHARACTER*1.
!           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX IS AN UPPER OR
!           LOWER TRIANGULAR MATRIX AS FOLLOWS:
!
!              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
!
!              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
!
!           UNCHANGED ON EXIT.
!
!  TRANS  - CHARACTER*1.
!           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
!           FOLLOWS:
!
!              TRANS = 'N' OR 'N'   X := A*X.
!
!              TRANS = 'T' OR 'T'   X := A'*X.
!
!              TRANS = 'C' OR 'C'   X := A'*X.
!
!           UNCHANGED ON EXIT.
!
!  DIAG   - CHARACTER*1.
!           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT
!           TRIANGULAR AS FOLLOWS:
!
!              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
!
!              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
!                                  TRIANGULAR.
!
!           UNCHANGED ON EXIT.
!
!  N      - INTEGER.
!           ON ENTRY, N SPECIFIES THE ORDER OF THE MATRIX A.
!           N MUST BE AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, N ).
!           BEFORE ENTRY WITH  UPLO = 'U' OR 'U', THE LEADING N BY N
!           UPPER TRIANGULAR PART OF THE ARRAY A MUST CONTAIN THE UPPER
!           TRIANGULAR MATRIX AND THE STRICTLY LOWER TRIANGULAR PART OF
!           A IS NOT REFERENCED.
!           BEFORE ENTRY WITH UPLO = 'L' OR 'L', THE LEADING N BY N
!           LOWER TRIANGULAR PART OF THE ARRAY A MUST CONTAIN THE LOWER
!           TRIANGULAR MATRIX AND THE STRICTLY UPPER TRIANGULAR PART OF
!           A IS NOT REFERENCED.
!           NOTE THAT WHEN  DIAG = 'U' OR 'U', THE DIAGONAL ELEMENTS OF
!           A ARE NOT REFERENCED EITHER, BUT ARE ASSUMED TO BE UNITY.
!           UNCHANGED ON EXIT.
!
!  LDA    - INTEGER.
!           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
!           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
!           MAX( 1, N ).
!           UNCHANGED ON EXIT.
!
!  X      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
!           ( 1 + ( N - 1 )*ABS( INCX ) ).
!           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE N
!           ELEMENT VECTOR X. ON EXIT, X IS OVERWRITTEN WITH THE
!           TRANFORMED VECTOR X.
!
!  INCX   - INTEGER.
!           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
!           X. INCX MUST NOT BE ZERO.
!           UNCHANGED ON EXIT.
!
!
!  LEVEL 2 BLAS ROUTINE.
!
!  -- WRITTEN ON 22-OCTOBER-1986.
!     JACK DONGARRA, ARGONNE NATIONAL LAB.
!     JEREMY DU CROZ, NAG CENTRAL OFFICE.
!     SVEN HAMMARLING, NAG CENTRAL OFFICE.
!     RICHARD HANSON, SANDIA NATIONAL LABS.
!
!
!     .. PARAMETERS ..
!     .. LOCAL SCALARS ..
!     .. EXTERNAL FUNCTIONS ..
!     .. EXTERNAL SUBROUTINES ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      info = 0 
      if (.not.lsame(uplo,'U') .and. .not.lsame(uplo,'L')) then 
        info = 1 
      else if (.not.lsame(trans,'N') .and. .not.lsame(trans,'T') .and. .not.&
          lsame(trans,'C')) then 
        info = 2 
      else if (.not.lsame(diag,'U') .and. .not.lsame(diag,'N')) then 
        info = 3 
      else if (n < 0) then 
        info = 4 
      else if (lda < max(1,n)) then 
        info = 6 
      else if (incx == 0) then 
        info = 8 
      endif 
      if (info /= 0) then 
        call xerbla ('DTRMV ', info) 
        return  
      endif 
!
!     QUICK RETURN IF POSSIBLE.
!
      if (n == 0) return  
!
      nounit = lsame(diag,'N') 
!
!     SET UP THE START POINT IN X IF THE INCREMENT IS NOT UNITY. THIS
!     WILL BE  ( N - 1 )*INCX  TOO SMALL FOR DESCENDING LOOPS.
!
      if (incx <= 0) then 
        kx = 1 - (n - 1)*incx 
      else if (incx /= 1) then 
        kx = 1 
      endif 
!
!     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
!     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
!
      if (lsame(trans,'N')) then 
!
!        FORM  X := A*X.
!
        if (lsame(uplo,'U')) then 
          if (incx == 1) then 
            do j = 1, n 
              if (x(j) == zero) cycle  
              temp = x(j) 
              x(:j-1) = x(:j-1) + temp*a(:j-1,j) 
              if (.not.nounit) cycle  
              x(j) = x(j)*a(j,j) 
            end do 
          else 
            jx = kx 
            do j = 1, n 
              if (x(jx) /= zero) then 
                temp = x(jx) 
                ix = kx 
                x(ix:(j-2)*incx+ix:incx) = x(ix:(j-2)*incx+ix:incx) + temp*a(:j&
                  -1,j) 
                if (nounit) x(jx) = x(jx)*a(j,j) 
              endif 
              jx = jx + incx 
            end do 
          endif 
        else 
          if (incx == 1) then 
            do j = n, 1, -1 
              if (x(j) == zero) cycle  
              temp = x(j) 
              x(n:1+j:(-1)) = x(n:1+j:(-1)) + temp*a(n:1+j:(-1),j) 
              if (.not.nounit) cycle  
              x(j) = x(j)*a(j,j) 
            end do 
          else 
            kx = kx + (n - 1)*incx 
            jx = kx 
            do j = n, 1, -1 
              if (x(jx) /= zero) then 
                temp = x(jx) 
                ix = kx 
                x(ix:incx*(j-n+1)+ix:(-incx)) = x(ix:incx*(j-n+1)+ix:(-incx))&
                   + temp*a(n:1+j:(-1),j) 
                if (nounit) x(jx) = x(jx)*a(j,j) 
              endif 
              jx = jx - incx 
            end do 
          endif 
        endif 
      else 
!
!        FORM  X := A'*X.
!
        if (lsame(uplo,'U')) then 
          if (incx == 1) then 
            do j = n, 1, -1 
              temp = x(j) 
              if (nounit) temp = temp*a(j,j) 
              temp = temp + sum(a(j-1:1:(-1),j)*x(j-1:1:(-1))) 
              x(j) = temp 
            end do 
          else 
            jx = kx + (n - 1)*incx 
            do j = n, 1, -1 
              temp = x(jx) 
              ix = jx 
              if (nounit) temp = temp*a(j,j) 
              temp = temp + sum(a(j-1:1:(-1),j)*x(ix-incx:incx*(1-j)+ix:(-incx)&
                )) 
              x(jx) = temp 
              jx = jx - incx 
            end do 
          endif 
        else 
          if (incx == 1) then 
            do j = 1, n 
              temp = x(j) 
              if (nounit) temp = temp*a(j,j) 
              temp = temp + sum(a(j+1:n,j)*x(j+1:n)) 
              x(j) = temp 
            end do 
          else 
            jx = kx 
            do j = 1, n 
              temp = x(jx) 
              ix = jx 
              if (nounit) temp = temp*a(j,j) 
              temp = temp + sum(a(j+1:n,j)*x(ix+incx:(n-j)*incx+ix:incx)) 
              x(jx) = temp 
              jx = jx + incx 
            end do 
          endif 
        endif 
      endif 
!
      return  
!
!     END OF DTRMV .
!
      end subroutine dtrmv 


      subroutine dtrsm(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: m 
      integer , intent(in) :: n 
      integer , intent(in) :: lda 
      integer , intent(in) :: ldb 
      real(double) , intent(in) :: alpha 
      character  :: side*(*) 
      character  :: uplo*(*) 
      character  :: transa*(*) 
      character  :: diag*(*) 
      real(double) , intent(in) :: a(lda,*) 
      real(double) , intent(inout) :: b(ldb,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: one = 1.0D+0 
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, info, j, k, nrowa 
      real(double) :: temp 
      logical :: lside, nounit, upper 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!     .. SCALAR ARGUMENTS ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DTRSM  SOLVES ONE OF THE MATRIX EQUATIONS
!
!     OP( A )*X = ALPHA*B,   OR   X*OP( A ) = ALPHA*B,
!
!  WHERE ALPHA IS A SCALAR, X AND B ARE M BY N MATRICES, A IS A UNIT, OR
!  NON-UNIT,  UPPER OR LOWER TRIANGULAR MATRIX  AND  OP( A )  IS ONE  OF
!
!     OP( A ) = A   OR   OP( A ) = A'.
!
!  THE MATRIX X IS OVERWRITTEN ON B.
!
!  PARAMETERS
!  ==========
!
!  SIDE   - CHARACTER*1.
!           ON ENTRY, SIDE SPECIFIES WHETHER OP( A ) APPEARS ON THE LEFT
!           OR RIGHT OF X AS FOLLOWS:
!
!              SIDE = 'L' OR 'L'   OP( A )*X = ALPHA*B.
!
!              SIDE = 'R' OR 'R'   X*OP( A ) = ALPHA*B.
!
!           UNCHANGED ON EXIT.
!
!  UPLO   - CHARACTER*1.
!           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX A IS AN UPPER OR
!           LOWER TRIANGULAR MATRIX AS FOLLOWS:
!
!              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
!
!              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
!
!           UNCHANGED ON EXIT.
!
!  TRANSA - CHARACTER*1.
!           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
!           THE MATRIX MULTIPLICATION AS FOLLOWS:
!
!              TRANSA = 'N' OR 'N'   OP( A ) = A.
!
!              TRANSA = 'T' OR 'T'   OP( A ) = A'.
!
!              TRANSA = 'C' OR 'C'   OP( A ) = A'.
!
!           UNCHANGED ON EXIT.
!
!  DIAG   - CHARACTER*1.
!           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT TRIANGULAR
!           AS FOLLOWS:
!
!              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
!
!              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
!                                  TRIANGULAR.
!
!           UNCHANGED ON EXIT.
!
!  M      - INTEGER.
!           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF B. M MUST BE AT
!           LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  N      - INTEGER.
!           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF B.  N MUST BE
!           AT LEAST ZERO.
!           UNCHANGED ON EXIT.
!
!  ALPHA  - DOUBLE PRECISION.
!           ON ENTRY,  ALPHA SPECIFIES THE SCALAR  ALPHA. WHEN  ALPHA IS
!           ZERO THEN  A IS NOT REFERENCED AND  B NEED NOT BE SET BEFORE
!           ENTRY.
!           UNCHANGED ON EXIT.
!
!  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, K ), WHERE K IS M
!           WHEN  SIDE = 'L' OR 'L'  AND IS  N  WHEN  SIDE = 'R' OR 'R'.
!           BEFORE ENTRY  WITH  UPLO = 'U' OR 'U',  THE  LEADING  K BY K
!           UPPER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE UPPER
!           TRIANGULAR MATRIX  AND THE STRICTLY LOWER TRIANGULAR PART OF
!           A IS NOT REFERENCED.
!           BEFORE ENTRY  WITH  UPLO = 'L' OR 'L',  THE  LEADING  K BY K
!           LOWER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE LOWER
!           TRIANGULAR MATRIX  AND THE STRICTLY UPPER TRIANGULAR PART OF
!           A IS NOT REFERENCED.
!           NOTE THAT WHEN  DIAG = 'U' OR 'U',  THE DIAGONAL ELEMENTS OF
!           A  ARE NOT REFERENCED EITHER,  BUT ARE ASSUMED TO BE  UNITY.
!           UNCHANGED ON EXIT.
!
!  LDA    - INTEGER.
!           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
!           IN THE CALLING (SUB) PROGRAM.  WHEN  SIDE = 'L' OR 'L'  THEN
!           LDA  MUST BE AT LEAST  MAX( 1, M ),  WHEN  SIDE = 'R' OR 'R'
!           THEN LDA MUST BE AT LEAST MAX( 1, N ).
!           UNCHANGED ON EXIT.
!
!  B      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDB, N ).
!           BEFORE ENTRY,  THE LEADING  M BY N PART OF THE ARRAY  B MUST
!           CONTAIN  THE  RIGHT-HAND  SIDE  MATRIX  B,  AND  ON EXIT  IS
!           OVERWRITTEN BY THE SOLUTION MATRIX  X.
!
!  LDB    - INTEGER.
!           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
!           IN  THE  CALLING  (SUB)  PROGRAM.   LDB  MUST  BE  AT  LEAST
!           MAX( 1, M ).
!           UNCHANGED ON EXIT.
!
!
!  LEVEL 3 BLAS ROUTINE.
!
!
!  -- WRITTEN ON 8-FEBRUARY-1989.
!     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
!     IAIN DUFF, AERE HARWELL.
!     JEREMY DU CROZ, NUMERICAL ALGORITHMS GROUP LTD.
!     SVEN HAMMARLING, NUMERICAL ALGORITHMS GROUP LTD.
!
!
!     .. EXTERNAL FUNCTIONS ..
!     .. EXTERNAL SUBROUTINES ..
!     .. INTRINSIC FUNCTIONS ..
!     .. LOCAL SCALARS ..
!     .. PARAMETERS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      lside = lsame(side,'L') 
      if (lside) then 
        nrowa = m 
      else 
        nrowa = n 
      endif 
      nounit = lsame(diag,'N') 
      upper = lsame(uplo,'U') 
!
      info = 0 
      if (.not.lside .and. .not.lsame(side,'R')) then 
        info = 1 
      else if (.not.upper .and. .not.lsame(uplo,'L')) then 
        info = 2 
      else if (.not.lsame(transa,'N') .and. .not.lsame(transa,'T') .and. .not.&
          lsame(transa,'C')) then 
        info = 3 
      else if (.not.lsame(diag,'U') .and. .not.lsame(diag,'N')) then 
        info = 4 
      else if (m < 0) then 
        info = 5 
      else if (n < 0) then 
        info = 6 
      else if (lda < max(1,nrowa)) then 
        info = 9 
      else if (ldb < max(1,m)) then 
        info = 11 
      endif 
      if (info /= 0) then 
        call xerbla ('DTRSM ', info) 
        return  
      endif 
!
!     QUICK RETURN IF POSSIBLE.
!
      if (n == 0) return  
!
!     AND WHEN  ALPHA.EQ.ZERO.
!
      if (alpha == zero) then 
        b(:m,:n) = zero 
        return  
      endif 
!
!     START THE OPERATIONS.
!
      if (lside) then 
        if (lsame(transa,'N')) then 
!
!           FORM  B := ALPHA*INV( A )*B.
!
          if (upper) then 
            do j = 1, n 
              if (Abs(alpha-1.d0) > 1.d-20) then 
                b(:m,j) = alpha*b(:m,j) 
              endif 
              do k = m, 1, -1 
                if (b(k,j) == zero) cycle  
                if (nounit) b(k,j) = b(k,j)/a(k,k) 
                b(:k-1,j) = b(:k-1,j) - b(k,j)*a(:k-1,k) 
              end do 
            end do 
          else 
            do j = 1, n 
              if (Abs(alpha-1.d0) > 1.d-20) then 
                b(:m,j) = alpha*b(:m,j) 
              endif 
              do k = 1, m 
                if (b(k,j) == zero) cycle  
                if (nounit) b(k,j) = b(k,j)/a(k,k) 
                b(k+1:m,j) = b(k+1:m,j) - b(k,j)*a(k+1:m,k) 
              end do 
            end do 
          endif 
        else 
!
!           FORM  B := ALPHA*INV( A' )*B.
!
          if (upper) then 
            do j = 1, n 
              do i = 1, m 
                temp = alpha*b(i,j) 
                temp = temp - sum(a(:i-1,i)*b(:i-1,j)) 
                if (nounit) temp = temp/a(i,i) 
                b(i,j) = temp 
              end do 
            end do 
          else 
            do j = 1, n 
              do i = m, 1, -1 
                temp = alpha*b(i,j) 
                temp = temp - sum(a(i+1:m,i)*b(i+1:m,j)) 
                if (nounit) temp = temp/a(i,i) 
                b(i,j) = temp 
              end do 
            end do 
          endif 
        endif 
      else 
        if (lsame(transa,'N')) then 
!
!           FORM  B := ALPHA*B*INV( A ).
!
          if (upper) then 
            do j = 1, n 
              if (Abs(alpha-1.d0) > 1.d-20) then 
                b(:m,j) = alpha*b(:m,j) 
              endif 
              do k = 1, j - 1 
                if (a(k,j) == zero) cycle  
                b(:m,j) = b(:m,j) - a(k,j)*b(:m,k) 
              end do 
              if (.not.nounit) cycle  
              temp = one/a(j,j) 
              b(:m,j) = temp*b(:m,j) 
            end do 
          else 
            do j = n, 1, -1 
              if (Abs(alpha-1.d0) > 1.d-20) then 
                b(:m,j) = alpha*b(:m,j) 
              endif 
              do k = j + 1, n 
                if (a(k,j) == zero) cycle  
                b(:m,j) = b(:m,j) - a(k,j)*b(:m,k) 
              end do 
              if (.not.nounit) cycle  
              temp = one/a(j,j) 
              b(:m,j) = temp*b(:m,j) 
            end do 
          endif 
        else 
!
!           FORM  B := ALPHA*B*INV( A' ).
!
          if (upper) then 
            do k = n, 1, -1 
              if (nounit) then 
                temp = one/a(k,k) 
                b(:m,k) = temp*b(:m,k) 
              endif 
              do j = 1, k - 1 
                if (a(j,k) == zero) cycle  
                temp = a(j,k) 
                b(:m,j) = b(:m,j) - temp*b(:m,k) 
              end do 
              if (Abs(alpha-1.d0) < 1.d-20) cycle  
              b(:m,k) = alpha*b(:m,k) 
            end do 
          else 
            do k = 1, n 
              if (nounit) then 
                temp = one/a(k,k) 
                b(:m,k) = temp*b(:m,k) 
              endif 
              do j = k + 1, n 
                if (a(j,k) == zero) cycle  
                temp = a(j,k) 
                b(:m,j) = b(:m,j) - temp*b(:m,k) 
              end do 
              if (Abs(alpha-1.d0) < 1.d-20) cycle  
              b(:m,k) = alpha*b(:m,k) 
            end do 
          endif 
        endif 
      endif 
!
      return  
!
!     END OF DTRSM .
!
      end subroutine dtrsm 


      integer function idamax (n, dx, incx) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer , intent(in) :: incx 
      real(double) , intent(in) :: dx(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, ix 
      real(double) :: dmax 
!-----------------------------------------------
!
!     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!     MODIFIED TO CORRECT PROBLEM WITH NEGATIVE INCREMENT, 8/21/90.
!
!
      idamax = 0 
      if (n < 1) return  
      idamax = 1 
      if (n == 1) return  
      if (incx /= 1) then 
!
!        CODE FOR INCREMENT NOT EQUAL TO 1
!
        ix = 1 
        if (incx < 0) ix = ((-n) + 1)*incx + 1 
        dmax = dabs(dx(ix)) 
        ix = ix + incx 
        do i = 2, n 
          if (dabs(dx(ix)) > dmax) then 
            idamax = i 
            dmax = dabs(dx(ix)) 
          endif 
          ix = ix + incx 
        end do 
        return  
      endif 
!
!        CODE FOR INCREMENT EQUAL TO 1
!
      dmax = dabs(dx(1)) 
      do i = 2, n 
        if (dabs(dx(i)) <= dmax) cycle  
        idamax = i 
        dmax = dabs(dx(i)) 
      end do 
      return  
      end function idamax 


      logical function lsame (ca, cb) 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character , intent(in) :: ca 
      character , intent(in) :: cb 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: inta, intb, zcode 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC ichar 
!-----------------------------------------------
!
!  -- LAPACK AUXILIARY ROUTINE (VERSION 1.0) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  LSAME RETURNS .TRUE. IF CA IS THE SAME LETTER AS CB REGARDLESS OF
!  CASE.
!
!  ARGUMENTS
!  =========
!
!  CA      (INPUT) CHARACTER*1
!  CB      (INPUT) CHARACTER*1
!          CA AND CB SPECIFY THE SINGLE CHARACTERS TO BE COMPARED.
!
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. LOCAL SCALARS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST IF THE CHARACTERS ARE EQUAL
!
      lsame = ca == cb 
      if (lsame) return  
!
!     NOW TEST FOR EQUIVALENCE IF BOTH CHARACTERS ARE ALPHABETIC.
!
      zcode = ichar('Z') 
!
!     USE 'Z' RATHER THAN 'A' SO THAT ASCII CAN BE DETECTED ON PRIME
!     MACHINES, ON WHICH ICHAR RETURNS A VALUE WITH BIT 8 SET.
!     ICHAR('A') ON PRIME MACHINES RETURNS 193 WHICH IS THE SAME AS
!     ICHAR('A') ON AN EBCDIC MACHINE.
!
      inta = ichar(ca) 
      intb = ichar(cb) 
!
      if (zcode==90 .or. zcode==122) then 
!
!        ASCII IS ASSUMED - ZCODE IS THE ASCII CODE OF EITHER LOWER OR
!        UPPER CASE 'Z'.
!
        if (inta>=97 .and. inta<=122) inta = inta - 32 
        if (intb>=97 .and. intb<=122) intb = intb - 32 
!
      else if (zcode==233 .or. zcode==169) then 
!
!        EBCDIC IS ASSUMED - ZCODE IS THE EBCDIC CODE OF EITHER LOWER OR
!        UPPER CASE 'Z'.
!
        if (inta>=129 .and. inta<=137 .or. inta>=145 .and. inta<=153 .or. inta&
          >=162 .and. inta<=169) inta = inta + 64 
        if (intb>=129 .and. intb<=137 .or. intb>=145 .and. intb<=153 .or. intb&
          >=162 .and. intb<=169) intb = intb + 64 
!
      else if (zcode==218 .or. zcode==250) then 
!
!        ASCII IS ASSUMED, ON PRIME MACHINES - ZCODE IS THE ASCII CODE
!        PLUS 128 OF EITHER LOWER OR UPPER CASE 'Z'.
!
        if (inta>=225 .and. inta<=250) inta = inta - 32 
        if (intb>=225 .and. intb<=250) intb = intb - 32 
      endif 
      lsame = inta == intb 
      return  
!
!     RETURN
!
!     END OF LSAME
!
      end function lsame 


      subroutine xerbla(srname, info) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
     USE chanel_C, only : iw
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:48:56  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: info 
      character , intent(in) :: srname*6 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!
!  -- LAPACK AUXILIARY ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!     .. SCALAR ARGUMENTS ..
! MOPAC CHANGE
      write (iw, fmt=10) srname, info 
! END OF MOPAC CHANGE
!
      call mopend ('Error in BLAS') 
      return  
!
   10 format(' ** ON ENTRY TO ',a6,' PARAMETER NUMBER ',i2,' HAD ',&
        'AN ILLEGAL VALUE') 
!
!     END OF XERBLA
!
      end subroutine xerbla 
