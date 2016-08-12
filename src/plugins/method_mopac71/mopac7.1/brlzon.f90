      subroutine brlzon(fmatrx, n3,mono3, step, mode) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use funcon_C, only : fpc_10, fpc_8
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:02  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use cdiag_I   
      use dofs_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer, intent(in)  :: mono3, n3
      integer , intent(in) :: mode 
      real(double) , intent(in) :: step 
      real(double) , intent(in) :: fmatrx((n3*(n3 + 1))/2) 
      real(double)  :: fmat2d(n3, n3) 
      real(double), allocatable  :: b(:,:) 
      complex  :: sec(mono3,mono3) 
      complex  :: vec(mono3,mono3) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ncells, k, i, j, m, loop, ii, iii, jj
      real , dimension(mono3) :: eigs 
      real :: cay 
      real(double) :: fact, c, twopi, ri, bottom, top 
      complex :: phase 
!-----------------------------------------------
!**********************************************************************
!
!   IF MODE IS 1 THEN
!   BRLZON COMPUTES THE PHONON SPECTRUM OF A LINEAR POLYMER GIVEN
!   THE WEIGHTED HESSIAN MATRIX.
!   IF MODE IS 2 THEN
!   BRLZON COMPUTES THE ELECTRONIC ENERGY BAND STRUCTURE OF A LINEAR
!   POLYMER GIVEN THE FOCK MATRIX.
!
!                 ON INPUT
!
!   IF MODE IS 1 THEN
!         FMATRX IS THE MASS-WEIGHTED HESSIAN MATRIX, PACKED LOWER
!                   HALF TRIANGLE
!         N3     IS 3*(NUMBER OF ATOMS IN UNIT CELL) = SIZE OF FMATRX
!         MONO3  IS 3*(NUMBER OF ATOMS IN PRIMITIVE UNIT CELL)
!         FMAT2D, SEC, VEC ARE SCRATCH ARRAYS
!   IF MODE IS 2 THEN
!         FMATRX IS THE FOCK MATRIX, PACKED LOWER HALF TRIANGLE
!         N3     IS NUMBER OF ATOMIC ORBITALS IN SYSTEM = SIZE OF FMATRX
!         MONO3  IS NUMBER OF ATOMIC ORBITALS IN FUNDAMENTAL UNIT CELL
!         FMAT2D, SEC, VEC ARE SCRATCH ARRAYS
!
!**********************************************************************
      fact = fpc_10 
      c = fpc_8 
      twopi = 2.D0*3.14159265358979D0 
!
!  NCELLS IS THE NUMBER OF PRIMITIVE UNIT CELLS IN THE UNIT CELL
!
      ncells = n3/mono3 
!
!  PUT THE ENERGY MATRIX INTO SQUARE MATRIX FORM
!
      k = 0 
      do i = 1, n3 
        if (i > 0) then 
          fmat2d(i,:i) = fmatrx(k+1:i+k) 
          k = i + k 
        endif 
      end do 
!
!   STEP IS THE STEP SIZE IN THE BRILLOUIN ZONE (BOUNDARIES: 0.0 - 0.5),
!   THERE ARE M OF THESE.
!   MONO3 IS THE SIZE OF ONE MER (MONOMERIC UNIT)
!
      m = int(0.5D0/step + 1) 
      allocate(b(mono3,m))
      do loop = 1, m 
        sec(:mono3,:mono3) = cmplx(0.0,0.0) 
        cay = real((loop - 1)*step) 
        do i = 1, n3 - 1, mono3 
          ri = dble((i - 1)/mono3) 
!
! IF THE PRIMITIVE UNIT CELL IS MORE THAN HALF WAY ACROSS THE UNIT CELL,
! CONSIDER IT AS BEING LESS THAN HALF WAY ACROSS, BUT IN THE OPPOSITE
! DIRECTION.
!
          if (ri > 0.5*ncells) ri = ri - ncells 
!
!  PHASE IS THE COMPLEX PHASE EXP(I.K.R(I)/(2PI))
!
          phase = exp(sqrt(cmplx(-1.0,0.0))*cay*real(ri*twopi)) 
          do ii = 1, mono3 
            iii = ii + i - 1 
            do jj = 1, ii 
              sec(ii,jj) = sec(ii,jj) + real(fmat2d(iii,jj))*phase 
            end do 
          end do 
        end do 
        call cdiag (sec, eigs, vec, mono3) 
        if (mode == 1) then 
!
!  CONVERT INTO RECIPRICAL CENTIMETERS
!
          do i = 1, mono3 
            b(i,loop) = dble(sign(sqrt(real(fact)*abs(eigs(i)*1.E5))/real(c*&
              twopi),eigs(i))) 
          end do 
        else 
          do i = 1, mono3 
            b(i,loop) = dble(eigs(i)) 
          end do 
        endif 
      end do 
      bottom = 1.D6 
      top = -1.D6 
      do i = 1, mono3 
        do j = 1, m 
          bottom = min(bottom,b(i,j)) 
          top = max(top,b(i,j)) 
        end do 
      end do 
      if (mode == 1) then 
        write (iw, '(2/,A,F6.3,/)') &
          ' FREQUENCIES IN CM(-1) FOR PHONON SPECTRUM ACROSS BRILLOUIN ZONE' 
        do i = 1, mono3 
!#            WRITE(IW,'(/,A,I4,/)')'  BAND:',I
          write (iw, '(1(F6.3,F7.1))') ((j - 1)*step,b(i,j),j=1,m) 
        end do 
        return  
      else 
        write (iw, '(2/,A,F6.3,/)') &
          ' ENERGIES (IN EV) OF ELECTRONIC BANDS IN BAND STRUCTURE' 
        do i = 1, mono3, 2 
          write (iw, '(2F10.3)') ((j - 1)*step,b(i,j),j=1,m) 
          if (i + 1 > mono3) cycle  
          write (iw, '(2F10.3)') ((j - 1)*step,b(i+1,j),j=m,1,(-1)) 
        end do 
      endif 
      call dofs (b, mono3, m, fmat2d, 500, bottom, top) 
      return  
      end subroutine brlzon 
      subroutine cdiag(a, value, vec, n) 
!
!  Complex diagonalizer.  This works by expanding the complex secular matrix "a"
!  into a real matrix, "ar", in which the real parts of "a" are in the 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: n 
      real  :: value(*) 
      complex  :: a(n,*) 
      complex  :: vec(n,*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k, i2
      double precision, dimension(2*n, 2*n) :: ar 
      double precision, dimension((2*n*(2*n + 1))/2) :: arlin
      double precision, dimension(2*n, 2*n) :: vecr
      double precision, dimension(2*n) :: value_r
      ar = 0.d0
      do i = 1, n
        do j = 1, i
          ar(    i,     j) = real(a(i,j), kind(0.d0))
          ar(n + i, n + j) = ar(i,j)
          ar(n + i,     j) = imag(a(i,j))
          ar(n + j,     i) = -imag(a(i,j))
        end do
        ar(n + i, i) = 0.d0
      end do
      k = 0
      do i = 1,2*n
        do j = 1,i
          k = k + 1
          arlin(k) = ar(i,j)
        end do
      end do
      call rsp(arlin, 2*n, 2*n, value_r, vecr) 
      do i = 2,2*n,2
        i2 = i/2
        value(i2) = real(value_r(i), kind(0.0))
        do j = 1,n
          vec(j,i2) = cmplx(vecr(j,i), vecr(n + j,i), kind=4)
        end do
      end do 
      return  
      end subroutine cdiag 

