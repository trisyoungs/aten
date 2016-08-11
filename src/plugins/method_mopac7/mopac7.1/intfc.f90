      subroutine intfc(fmatrx, xparam, georef, nar, nbr, ncr) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : natoms, nvar, numat
      use permanent_arrays, only : na, nb, nc, geo, loc, labels, nat
      use chanel_C, only : iw
      use elemts_C, only : elemnt
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:21  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use jcarin_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nar(natoms) 
      integer , intent(in) :: nbr(natoms) 
      integer , intent(in) :: ncr(natoms) 
      real(double) , intent(in) :: fmatrx(*) 
      real(double)  :: xparam(nvar) 
      real(double) , intent(in) :: georef(3,natoms) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j, i, ilim, n3, k, ncol
      real(double), dimension(3,natoms) :: fcint 
      real(double) :: step, stepi, sum, dumy(3*numat)
      logical :: precis 
!-----------------------------------------------
      nvar = 0 
      numat = 0 
      na(:natoms) = nar 
      nb(:natoms) = nbr 
      nc(:natoms) = ncr 
      fcint(:,:natoms) = 0.D0 
      geo(:,:natoms) = georef 
      na(1) = 0 
      do i = 1, natoms 
        if (labels(i) == 99) cycle  
        numat = numat + 1 
        ilim = min(3,i - 1) 
        do j = 1, ilim 
          nvar = nvar + 1 
          loc(1,nvar) = i 
          loc(2,nvar) = j 
          xparam(nvar) = geo(j,i) 
        end do 
      end do 
      n3 = 3*numat 
!
!   Calculate force constants over internal coordinates
!
      step = 1.D-7 
      stepi = 0.5D0/step 
      precis = .TRUE. 
      write (iw, '(/,10X,A,/)') &
        ' FORCE CONSTANT IN INTERNAL COORDINATES (Millidynes/A)' 
      write (iw, '(A)') &
        '    ATOM   CHEMICAL  BOND LENGTH      BOND ANGLE     TWIST ANGLE' 
      write (iw, '(A)') &
        '   NUMBER   SYMBOL  FORCE CONSTANT  FORCE CONSTANT  FORCE CONSTANT' 
      write (iw, '(A)') 
      do i = 1, nvar 
        j = i 
        call jcarin (xparam, step, precis, dumy, ncol, i, j) 
        dumy(:n3) = dumy(:n3)*stepi 
!
!   Calculate internal force constant
!
        sum = 0.D0 
        do j = 1, n3 
          do k = 1, n3 
            if (j >= k) then 
              sum = sum + dumy(j)*fmatrx((j*(j-1))/2+k)*dumy(k) 
            else 
              sum = sum + dumy(j)*fmatrx((k*(k-1))/2+j)*dumy(k) 
            endif 
          end do 
        end do 
!
!   1.D-5 is to correct the multiplier in FREQCY of the force constants
!
        fcint(loc(2,i),loc(1,i)) = sum*5.D-6 
      end do 
      if (ncol < 0) return ! dummy statement, just to use ncol
      write (iw, '(I7,6X,A2,3F16.6)') (i,elemnt(nat(i)),(fcint(j,i),j=1,3),i=1,&
        numat) 
      return  
      end subroutine intfc 
