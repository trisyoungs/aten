      subroutine timer(a) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE chanel_C, only : iw 
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:36:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use second_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character , intent(in) :: a*(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(double) :: t0, t1, t2 
      logical :: first 

      save first, t0, t1 
!-----------------------------------------------
      data first/ .TRUE./  
      if (first) then 
!
!  DEFINE THE ZERO OF TIME
!
        t0 = second(1) 
        t1 = t0 
        first = .FALSE. 
      endif 
      t2 = second(1) 
      if (index(a,'BEF')==0 .and. a/=' ') write (iw, '(2X,A,A,F7.2,A,F8.2)') a&
        , ' INTERVAL:', t2 - t1, ' INTEGRAL:', t2 - t0 
      t1 = t2 + 0.026D0 
      return  
      end subroutine timer 
