      subroutine chi(toler, coord, ioper, iqual) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use symmetry_C, only : ielem, elem, jelem
      use molkst_C, only : numat
      use permanent_arrays, only : nat
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:03  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ioper 
      integer , intent(out) :: iqual 
      real(double) , intent(in) :: toler 
      real(double) , intent(in) :: coord(3,numat) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: iresul, i, j 
      real(double), dimension(3) :: help 
!-----------------------------------------------
!***********************************************************************
!
!   CHI RETURNS A '1' IN IELEM(IOPER) IF THE SYMMETRY OPERATION ELEM(IOP
!       LEAVES THE SYSTEM UNCHANGED.  OTHERWISE CHI RETURNS '0' IN IELEM
!
!   ON INPUT: COORD        = CARTESIAN COORDINATES
!             IOPER        = SYMMETRY OPERATION TO BE PERFORMED
!             ELEM         = SYMMETRY OPERATORS AS 3*3 MATRICES
!   ON OUTPUT IELEM(IOPER) = 1 OR 0.
!
!***********************************************************************
      iresul = 1 
      iqual = 0 
      l20: do i = 1, numat 
        help(1) = coord(1,i)*elem(1,1,ioper) + coord(2,i)*elem(1,2,ioper) + &
          coord(3,i)*elem(1,3,ioper) 
        help(2) = coord(1,i)*elem(2,1,ioper) + coord(2,i)*elem(2,2,ioper) + &
          coord(3,i)*elem(2,3,ioper) 
        help(3) = coord(1,i)*elem(3,1,ioper) + coord(2,i)*elem(3,2,ioper) + &
          coord(3,i)*elem(3,3,ioper) 
        do j = 1, numat 
          if (nat(i) /= nat(j)) cycle  
          if (abs(coord(1,j)-help(1)) > toler) cycle  
          if (abs(coord(2,j)-help(2)) > toler) cycle  
          if (abs(coord(3,j)-help(3)) > toler) cycle  
          jelem(ioper,i) = j 
          if (i == j) iqual = iqual + 1 
          cycle  l20 
        end do 
        iresul = 0 
      end do l20 
      ielem(ioper) = iresul 
      return  
      end subroutine chi 
