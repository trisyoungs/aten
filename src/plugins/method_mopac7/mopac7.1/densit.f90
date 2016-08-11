      subroutine densit(c, mdim, norbs, ndubl, nsingl, fract, p, mode) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:06  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mdim 
      integer , intent(in) :: norbs 
      integer , intent(in) :: ndubl 
      integer , intent(inout) :: nsingl 
      integer , intent(in) :: mode 
      real(double) , intent(in) :: fract 
      real(double) , intent(in) :: c(mdim,mdim)
      real(double) , intent(inout) :: p((norbs*(norbs + 1))/2) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: norbs2, nl2, nu2, nl1, nu1, l, i, j 
      real(double) :: sign, frac, const, sum2, sum1 
!-----------------------------------------------
!***********************************************************************
!
!   DENSIT COMPUTES THE DENSITY MATRIX GIVEN THE EIGENVECTOR MATRIX, AND
!          INFORMATION ABOUT THE M.O. OCCUPANCY.
!
!  INPUT:  C     = SQUARE EIGENVECTOR MATRIX, C IS OF SIZE MDIM BY MDIM
!                  AND THE EIGENVECTORS ARE STORED IN THE TOP LEFT-HAND
!                  CORNER.
!          NORBS = NUMBER OF ORBITALS
!          NDUBL = NUMBER OF DOUBLY-OCCUPIED M.O.S ( =0 IF UHF)
!          NSINGL= NUMBER OF SINGLY OR FRACTIONALLY OCCUPIED M.O.S.
!          MODE  = 2 IF POSITRON EQUIVALENT IS NOT TO BE USED
!
!   ON EXIT: P   = DENSITY MATRIX
!
!***********************************************************************
!
! SET UP LIMITS FOR SUMS
!  NL1 = BEGINING OF ONE ELECTRON SUM
!  NU1 = END OF SAME
!  NL2 = BEGINING OF TWO ELECTRON SUM
!  NU2 = END OF SAME
!
      norbs2 = norbs/2 
      nsingl = max(ndubl,nsingl) 
      if (ndubl/=0 .and. nsingl>norbs2 .and. mode/=2) then 
!
!    TAKE POSITRON EQUIVALENT
!
        sign = -1.D0 
        frac = 2.D0 - fract 
        const = 2.D0 
        nl2 = nsingl + 1 
        nu2 = norbs 
        nl1 = ndubl + 1 
        nu1 = nsingl 
      else 
!
!    TAKE ELECTRON EQUIVALENT
!
        sign = 1.D0 
        frac = fract 
        const = 0.D0 
        nl2 = 1 
        nu2 = ndubl 
        nl1 = ndubl + 1 
        nu1 = nsingl 
      endif 
      l = 0 
      do i = 1, norbs 
        do j = 1, i 
          l = l + 1 
          sum1 = 0.D0 
          sum2 = sum(c(i,nl2:nu2)*c(j,nl2:nu2)) 
          sum2 = sum2*2.D0 
          sum1 = sum(c(i,nl1:nu1)*c(j,nl1:nu1)) 
          p(l) = (sum2 + sum1*frac)*sign 
        end do 
        p(l) = const + p(l) 
      end do 
      return  
      end subroutine densit 
