      subroutine mopend(txt) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE molkst_C, only : moperr, errtxt 
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:53:50  03/08/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character , intent(in) :: txt*(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
      moperr = .TRUE. 
      errtxt = txt 
      return  
      end subroutine mopend 
