      subroutine makopr(numat, coord, ierror, r) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use symmetry_C, only : ielem, jy, nclass
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:23  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use symopr_I 
      use bldsym_I 
      use chi_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: numat 
      integer , intent(out) :: ierror 
      real(double)  :: coord(3,numat) 
      real(double)  :: r(3,3) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, iqual 
      real(double) :: toler 
!-----------------------------------------------
!*********************************************************************
!
!   MAKOPR builds the operations based on the point group
!          of the system.  A check is made to verify that the
!          operations are valid.
!
!*********************************************************************
      call symopr (numat, coord, 1, r) 
      if (nclass < 2) return  
!
!   NCLASS is the number of Classes in the Group.
!   Construct the Operations corresponding to the Classes
!   These are stored in ELEM.
!
      do i = 2, nclass 
        call bldsym (jy(i), i) 
      end do 
!
!   Use a more tolerant criterion for recognizing operations because
!   the point-group has already been identified.
!
      toler = 0.2D0 
      do i = 2, nclass 
        call chi (toler, coord, i, iqual) 
        if (ielem(i) >= 1) cycle  
        ierror = 5 
      end do 
      call symopr (numat, coord, -1, r) 
      if (iqual < 0) return ! dummy use of iqual
      return  
      end subroutine makopr 
