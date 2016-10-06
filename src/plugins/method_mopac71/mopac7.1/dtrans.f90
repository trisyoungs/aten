      subroutine dtrans(d, h, ioper, first, r) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE symmetry_C, only : nclass, elem 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:33:49  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dtran2_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ioper 
      logical , intent(inout) :: first 
      real(double) , intent(inout) :: d(5) 
      real(double) , intent(inout) :: h(5) 
      real(double) , intent(in) :: r(3,3) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, k 
      real(double), dimension(5,5,12) :: t1 
      real(double), dimension(3,3) :: s 
!-----------------------------------------------
      if (first) then 
        first = .FALSE. 
        s = r 
        t1 = 0.d0
        call dtran2 (s, t1, 1) 
        do k = 2, nclass 
          s = elem(:,:,k) 
          call dtran2 (s, t1, k) 
        end do 
      endif 
      do i = 1, 5 
        h(i) = 0.D0 
        h(i) = h(i) + sum(t1(i,:,1)*d) 
      end do 
      do i = 1, 5 
        d(i) = 0.D0 
        d(i) = d(i) + sum(t1(:,i,ioper)*h) 
      end do 
      return  
      end subroutine dtrans 
