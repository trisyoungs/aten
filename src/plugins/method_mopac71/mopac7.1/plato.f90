      subroutine plato(coord, numat, r) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:33  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use symopr_I 
      use mopend_I 
      use bangle_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: numat 
      real(double)  :: coord(3,numat) 
      real(double)  :: r(3,3) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(numat) :: near 
      integer , dimension(3) :: ipoly 
      integer ::  index, i, l, j, ii, k, i1, j1, m, i3, i2 
      real(double), dimension(3) :: allr 
      real(double), dimension(3,3) :: xyz 
      real(double) :: toler, xmin, dist, r2j, angle, sum, buff, buff1 

      save toler 
!-----------------------------------------------
!***********************************************************************
!
!    PLATO  is part of the SYMMETRY package.  It generates the
!           unitary transform which will orientate the system
!           so that the 'Z' axis is an axis of rotation.
!
!    PLATO is called when the system belongs to a cubic point group.
!          Atoms nearest to the center of symmetry are identified.
!          If there are 4, 6, 8, 12, or 20, then they outline one
!          of the Platonic solids (tetrahedron, octahedron, cube,
!          icosahedron, or pentagonal dodecahedron).  In that case,
!          each atom lies on a 3, 4, or 5 fold symmetry axis.
!
!          In other cases the axis of symmetry lies goes through a
!          regular polygon, either an equilateral triangle, a square,
!          or a regular pentagon.
!
!          The unitary transform is returned through R.
!
!**********************************************************************
      data toler/ 0.1D0/  
      call symopr (numat, coord, -1, r) 
      index = 0 
      do i = 1, numat 
        xmin = coord(1,i)**2 + coord(2,i)**2 + coord(3,i)**2 
        if (xmin < toler) cycle  
        exit  
      end do 
      l = 0 
      do i = 1, numat 
        dist = coord(1,i)**2 + coord(2,i)**2 + coord(3,i)**2 
        if (dist < toler) cycle  
        if (dist > xmin + toler) cycle  
        if (abs(dist - xmin) < toler) then 
          l = l + 1 
          near(l) = i 
        else 
          l = 0 
        endif 
        index = i 
        xmin = dist 
      end do 
      if (l == 0) then 
        write (iw, *) ' Geometry in PLATO is unrecognizable!' 
        call mopend ('Geometry in PLATO is unrecognizable!') 
        return  
      endif 
      if (l==4 .or. l==6 .or. l==8 .or. l==12 .or. l==20) then 
!
!   How many near neighbors has atom NEAR(1) got?
!
        xmin = 100.D0 
        j = near(1) 
        do ii = 2, l 
          i = near(ii) 
          dist = (coord(1,j)-coord(1,i))**2 + (coord(2,j)-coord(2,i))**2 + (&
            coord(3,j)-coord(3,i))**2 
          xmin = min(dist,xmin) 
        end do 
        j = near(1) 
        k = 0 
        do ii = 2, l 
          i = near(ii) 
          dist = (coord(1,j)-coord(1,i))**2 + (coord(2,j)-coord(2,i))**2 + (&
            coord(3,j)-coord(3,i))**2 
          if (abs(dist - xmin) >= toler) cycle  
          k = k + 1 
        end do 
        if (l==4 .and. k==3 .or. l==6 .and. k==4 .or. l==8 .and. k==3 .or. l==&
          12 .and. k==5 .or. l==20 .and. k==3) then 
!
!     The system is a Platonic solid.  This is the simplest case.
!     An atom lies on a high-symmetry axis.
!
          r(1,3) = coord(1,index)/dist 
          r(2,3) = coord(2,index)/dist 
          r(3,3) = coord(3,index)/dist 
          go to 160 
        endif 
      endif 
      i1 = near(1) 
      do i = 1, 3 
        xmin = 100.D0 
        ipoly(i) = 0 
        l70: do j1 = 2, l 
          j = near(j1) 
          do m = 1, i - 1 
            if (ipoly(m) /= j) cycle  
            cycle  l70 
          end do 
          r2j = (coord(1,i1)-coord(1,j))**2 + (coord(2,i1)-coord(2,j))**2 + (&
            coord(3,i1)-coord(3,j))**2 
          if (xmin <= r2j) cycle  l70 
          xmin = r2j 
          ipoly(i) = j 
          allr(i) = r2j 
        end do l70 
      end do 
      do i = 1, 3 
        allr(i) = sqrt(allr(i)) 
      end do 
!
!   Identify the two atoms in the regular polygon.
!
      l100: do i = 1, 2 
        do j = i + 1, 3 
          if (abs(allr(i)-allr(j)) >= 0.01D0) cycle  
          exit  l100 
        end do 
      end do l100 
      xyz(:,3) = coord(:,ipoly(i)) 
      xyz(:,2) = coord(:,ipoly(j)) 
      xyz(:,1) = coord(:,i1) 
      i3 = ipoly(i) 
      i2 = ipoly(j) 
      ipoly(1) = i3 
      ipoly(2) = i2 
      call bangle (xyz, 3, 1, 2, angle) 
      if (abs(angle - 1.0472D0) < 0.1D0) then 
!
!   It's a triangle!
!
        r(:,3) = coord(:,i1) + coord(:,i2) + coord(:,i3) 
      else if (abs(angle - 1.5707963D0) < 0.1D0) then 
!
!   It's a square!
!
        r(:,3) = coord(:,i2) + coord(:,i3) 
      else if (abs(angle - 1.885D0) < 0.1D0) then 
!
!  It's a pentagon!
!
        r(:,3) = 1.6180341D0*(coord(:,i2)+coord(:,i3)) - coord(:,i1) 
      endif 
  160 continue 
      sum = sqrt(r(1,3)**2+r(2,3)**2+r(3,3)**2) 
      r(:,3) = r(:,3)/sum 
      buff = sqrt(r(1,3)**2+r(2,3)**2) 
      buff1 = sqrt(r(1,3)**2+r(3,3)**2) 
      if (buff <= buff1) then 
        r(1,1) = r(3,3)/buff1 
        r(2,1) = 0.D0 
        r(3,1) = -r(1,3)/buff1 
      else 
        r(1,1) = r(2,3)/buff 
        r(2,1) = -r(1,3)/buff 
        r(3,1) = 0.D0 
      endif 
      r(1,2) = r(2,3)*r(3,1) - r(2,1)*r(3,3) 
      r(2,2) = r(3,3)*r(1,1) - r(3,1)*r(1,3) 
      r(3,2) = r(1,3)*r(2,1) - r(1,1)*r(2,3) 
      call symopr (numat, coord, 1, r) 
!
!   The molecule is now orientated along one of the high-symmetry axes
!
      return  
      end subroutine plato 
