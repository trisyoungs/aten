      subroutine aintgs(x, k) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE overlaps_C, only : a
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:27:09  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: k 
      real(double) , intent(in) :: x 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i 
      real(double) :: c 
!-----------------------------------------------
!***********************************************************************
!
!    AINTGS FORMS THE "A" INTEGRALS FOR THE OVERLAP CALCULATION.
!
!***********************************************************************
      c = exp((-x)) 
      a(1) = c/x 
      do i = 1, k 
        a(i+1) = (a(i)*i+c)/x 
      end do 
      return  
!
      end subroutine aintgs 
