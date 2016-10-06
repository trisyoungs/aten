      subroutine set(s1, s2, na, nb, rab, ii) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE overlaps_C, only : isp, ips, sa, sb
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:35:52  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use aintgs_I 
      use bintgs_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: na 
      integer , intent(in) :: nb 
      integer , intent(in) :: ii 
      real(double) , intent(in) :: s1 
      real(double) , intent(in) :: s2 
      real(double) , intent(in) :: rab 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j, jcall 
      real(double) :: alpha, beta 
!-----------------------------------------------
!***********************************************************************
!
!     SET IS PART OF THE OVERLAP CALCULATION, CALLED BY OVERLP.
!         IT CALLS AINTGS AND BINTGS
!
!***********************************************************************
      if (na <= nb) then 
        isp = 1 
        ips = 2 
        sa = s1 
        sb = s2 
      else 
        isp = 2 
        ips = 1 
        sa = s2 
        sb = s1 
      endif 
      j = ii + 2 
      if (ii > 3) j = j - 1 
      alpha = 0.5D00*rab*(sa + sb) 
      beta = 0.5D00*rab*(sb - sa) 
      jcall = j - 1 
      call aintgs (alpha, jcall) 
      call bintgs (beta, jcall) 
      return  
!
      end subroutine set 
