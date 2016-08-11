      subroutine chrge(p, q) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only:  nfirst, nlast
      use molkst_C, only: numat, norbs
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:44:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
     
      real(double) , intent(out) :: q(numat) 
      real(double) , intent(in) :: p((norbs*(norbs+1))/2) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: k, i, ia, ib, j 
!-----------------------------------------------
!***********************************************************************
!
!      CHRGE STORES IN Q THE TOTAL ELECTRON DENSITIES ON THE ATOMS
!
!      ON INPUT P      = DENSITY MATRIX
!
!      ON OUTPUT Q     = ATOM ELECTRON DENSITIES
!
!***********************************************************************
      k = 0 
      do i = 1, numat 
        ia = nfirst(i) 
        ib = nlast(i) 
        q(i) = 0.D0 
        do j = ia, ib 
          k = k + j 
          q(i) = q(i) + p(k) 
        end do 
      end do 
      return  
      end subroutine chrge 
