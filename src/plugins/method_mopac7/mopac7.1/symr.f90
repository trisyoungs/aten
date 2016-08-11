      subroutine symr 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat
      use permanent_arrays, only : coord 
      use chanel_C, only : iw
      use symmetry_C, only : nclass, r, ipo, elem, nsym, nent
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:03  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      use symp_I 
      implicit none
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: maxent = 6 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: nvalue, i, ndflt, k, j, n 
      real(double) :: x, y, z, xa, ya, za, dist 
      logical :: prob 
!-----------------------------------------------
!
!     R(9,*):   The 9 elements of each record are a packed 3 by 3
!          array of a given symmetry operations.
!    IPO(n,*):  A vector that contains the symmetry mapping of atomic ce
!
      if (allocated(ipo)) deallocate(ipo)
      allocate(ipo(numat,120))
      prob = .FALSE. 
      nvalue = 0 
!  Get the symmetry functions: (NOTE: THE FIRST IS ALWAYS E)
      r(1,1) = 1.D0 
      r(2,1) = 0.D0 
      r(3,1) = 0.D0 
      r(4,1) = 0.D0 
      r(5,1) = 1.D0 
      r(6,1) = 0.D0 
      r(7,1) = 0.D0 
      r(8,1) = 0.D0 
      r(9,1) = 1.D0 
!
!  CENTER THE MOLECULE
!
      x = 0.D0 
      y = 0.D0 
      z = 0.D0 
      do i = 1, numat 
        x = x + coord(1,i) 
        y = y + coord(2,i) 
        z = z + coord(3,i) 
        ipo(i,1) = i 
      end do 
      xa = x/dble(numat) 
      ya = y/dble(numat) 
      za = z/dble(numat) 
      coord(1,:numat) = (-xa) + coord(1,:numat) 
      coord(2,:numat) = (-ya) + coord(2,:numat) 
      coord(3,:numat) = (-za) + coord(3,:numat) 
!
      nent = 1 
      nsym = 0 
      ndflt = 1 
   30 continue 
      nsym = nsym + 1 
      ndflt = ndflt + 1 
      if (ndflt <= nclass) then 
!
!   Copy Symmetry Operation from ELEM
!
        k = 0 
        nvalue = 1 
        do i = 1, 3 
          r(k+1:3+k,1+nent) = elem(i,:,ndflt) 
          k = 3 + k 
        end do 
!  NOW, TO CALCULATE THE IPO OF THIS FUNCTION
        nent = 1 + nent 
        n = nent 
!  Now, to initialize IPO(n) and
!  Perform R on each atomic center and determine where it maps to.
        l80: do i = 1, numat 
          x = coord(1,i)*r(1,n) + coord(2,i)*r(2,n) + coord(3,i)*r(3,n) 
          y = coord(1,i)*r(4,n) + coord(2,i)*r(5,n) + coord(3,i)*r(6,n) 
          z = coord(1,i)*r(7,n) + coord(2,i)*r(8,n) + coord(3,i)*r(9,n) 
          ipo(i,n) = 0 
          do j = 1, numat 
            dist = abs(x - coord(1,j)) + abs(y - coord(2,j)) + abs(z - coord(3,&
              j)) 
            if (dist >= 0.6D0) cycle  
            if (ipo(i,n) == 0) then 
              ipo(i,n) = j 
            else 
              write (iw, 50) 
              prob = .TRUE. 
              exit  l80 
   50         format('  ONE ATOM MAPS ONTO TWO DIFFERENT ATOMIC C','ENTERS',/,&
                '  ADD KEYWORD '' NOSYM'' AND RE-RUN') 
            endif 
          end do 
          if (ipo(i,n) /= 0) cycle  l80 
          write (iw, 70) 
   70     format('  ONE ATOM MAPS ONTO NO OTHER ATOM ',/,&
            '  ADD KEYWORD '' NOSYM'' AND RE-RUN') 
          prob = .TRUE. 
          exit  l80 
        end do l80 
!
!
      endif 
      if (nvalue/=0 .and. nsym<maxent) go to 30 
!
!  If a problem exists.  Stop the program.
!
      if (prob) then 
        write (iw, *) ' PROBLEM IN SYMR' 
        call mopend ('PROBLEM IN SYMR') 
        return  
      endif 
      nsym = nent 
!
!  NEXT, EXPAND THE EXISTING OPERATORS TO THE FULL SET
!
      call symp 
!
      return  
      end subroutine symr 
