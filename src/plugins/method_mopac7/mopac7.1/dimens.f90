      subroutine dimens(coord, iw) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat
      use permanent_arrays, only : nat
      use elemts_C, only : elemnt
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:09  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use symopr_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iw 
      real(double)  :: coord(3,numat) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(3,2) :: ij 
      integer :: l, loop, j, k, kmax, lmax, i, kk, ll
      real(double), dimension(3,3) :: c 
      real(double), dimension(3) :: dim 
      real(double) :: x1, y1, z1, rabmax, rmax, r, xy, ca, cb, sa, sb, ymin, &
        ymax 
!-----------------------------------------------
    if (numat == 1) return
    if (numat > 20) then
      !
      !    Pick any atom.  For convenience, let this be atom 1
      !
      l = 1
      x1 = coord(1, l)
      y1 = coord(2, l)
      z1 = coord(3, l)
      rabmax = 0.d0
      loop = 0
      do
        loop = loop + 1
         !
         !    Find the atom most distant from the current atom (atom L)
         !
        rmax = 0.d0
        do j = 1, numat
          r = (x1-coord(1, j)) ** 2 + (y1-coord(2, j)) ** 2 &
               & + (z1-coord(3, j)) ** 2
          if (r > rmax) then
            rmax = r
            k = j
          end if
        end do
         !
         !   Atom K is most distant
         !
         !  If the interatomic separation is a maximum, then that is the
         !  first dimension.
         !
        if (Abs (rmax-rabmax) < 1.d-5) exit
        rabmax = Max (rmax, rabmax)
        kmax = k
        lmax = l
         !
         !   We don't know if RMAX is the maximum, therefore go to the
         !   mid-point, and repeat the test.
         !
         !   Find the mid-point between the two atoms
         !
        x1 = 0.5d0 * (x1+coord(1, k))
        y1 = 0.5d0 * (y1+coord(2, k))
        z1 = 0.5d0 * (z1+coord(3, k))
         !
         !    Find the atom most distant from the mid-point
         !
        rmax = 0.d0
        do j = 1, numat
          r = (x1-coord(1, j)) ** 2 + (y1-coord(2, j)) ** 2 + (z1-coord(3, j)) &
         & ** 2
          if (r > rmax) then
            rmax = r
            l = j
          end if
        end do
        x1 = coord(1, l)
        y1 = coord(2, l)
        z1 = coord(3, l)
         !
         !  Atom L is most distant.  Now loop back and find the atom most
         !  distant from L.
         !
        if (loop >= 10) exit
      end do
    else
      !
      !  Search for absolute largest distance.
      !
      rmax = 0.d0
      do k = 1, numat
        x1 = coord(1, k)
        y1 = coord(2, k)
        z1 = coord(3, k)
        do l = 1, k - 1
          r = (x1-coord(1, l)) ** 2 + (y1-coord(2, l)) ** 2 &
               & + (z1-coord(3, l)) ** 2
          if (r > rmax) then
            rmax = r
            kmax = k
            lmax = l
          end if
        end do
      end do
    end if
    k = kmax
    l = lmax
   !
   !   Determine vector joining most distant atoms.
   !
    x1 = coord(1, k) - coord(1, l)
    y1 = coord(2, k) - coord(2, l)
    z1 = coord(3, k) - coord(3, l)
   !
   !  Rotate the system so that the most distant atoms have the same
   !  "Y" and "Z" coordinates.
   !
    xy = x1 ** 2 + y1 ** 2
    r = Sqrt (xy+z1**2)
    xy = Sqrt (xy)
    if (xy < 1.d-10) then
      if (z1 < 0.0d0) then
        ca = -1.d0
        cb = -1.d0
        sa = 0.d0
        sb = 0.d0
      else if (z1 > 0.0d0) then
        ca = 1.d0
        cb = 1.d0
        sa = 0.d0
        sb = 0.d0
      else
        ca = 0.d0
        cb = 0.d0
        sa = 0.d0
        sb = 0.d0
      end if
    else
      ca = x1 / xy
      cb = z1 / r
      sa = y1 / xy
      sb = xy / r
    end if
    c(1, 3) = ca * cb
    c(1, 2) = -sa
    c(1, 1) = ca * sb
    c(2, 3) = sa * cb
    c(2, 2) = ca
    c(2, 1) = sa * sb
    c(3, 3) = -sb
    c(3, 2) = 0.d0
    c(3, 1) = cb
    call symopr (numat, coord, 1, c)
!    dim(1) = Sqrt (rmax)
    dim(1) = r
    ij(1, 1) = k
    ij(1, 2) = l
    dim_loop: do
     !
     !   The longest dimension is now "X"
     !   Find the most distant atom in "Y-Z" plane'
     !
      y1 = coord(2, l)
      z1 = coord(3, l)
      rabmax = 0.d0
      loop = 0
      do
        loop = loop + 1
        if (loop > 10) then
          k = kk
          l = ll
          exit
        else
         !
         !    Find the atom most distant from atom "L"
         !
          rmax = 0.d0
          do j = 1, numat
            r = (y1-coord(2, j)) ** 2 + (z1-coord(3, j)) ** 2
            if (r > rmax) then
              rmax = r
              k = j
            end if
          end do
         !
         !   Atom K is most distant from L in the Y-Z plane.
         !
          if (Abs (rmax-rabmax) < 1.d-5) exit
          if (rmax > rabmax) then
            rabmax = rmax
            kk = k
            ll = l
          end if
         !
         !   We don't know if RMAX is the maximum, therefore go to the
         !   mid-point, and repeat the test.
         !
         !   Now find the mid-point between the two atoms
         !
          y1 = 0.5d0 * (y1+coord(2, k))
          z1 = 0.5d0 * (z1+coord(3, k))
         !
         !    Find the atom most distant
         !
          rmax = 0.d0
          do j = 1, numat
            r = (y1-coord(2, j)) ** 2 + (z1-coord(3, j)) ** 2
            if (r > rmax) then
              rmax = r
              l = j
            end if
          end do
          y1 = coord(2, l)
          z1 = coord(3, l)
        end if
      end do
     !
     !   Determine vector joining most distant atoms (K and L).
     !
      y1 = coord(2, k) - coord(2, l)
      z1 = coord(3, k) - coord(3, l)
      r = Sqrt (y1**2 + z1**2 + 1.d-20)
      ca = y1 / r
      sa = z1 / r
     !
     !  Rotate system so that atoms K and L have the same Y coordinate.
     !  We only need to calculate the Z coordinate for the next test.
     !
      do i = 1, numat
        coord(3, i) = -sa * coord(2, i) + ca * coord(3, i)
      end do
      if (r > dim(1)+Epsilon(r)) then
        dim(1) = r
        ij(1, 1) = k
        ij(1, 2) = l
        cycle dim_loop
      else
!        dim(2) = Sqrt (rmax)
        dim(2) = r
        ij(2, 1) = k
        ij(2, 2) = l
        exit dim_loop
      end if
    end do dim_loop
   !
   !   The longest dimension is now "X", the second longest dimension
   !   is "Y".
   !   Find the largest dimension in the "Z" direction
   !
    ymin = 1.d16
    ymax = -1.d16
    do i = 1, numat
      if (coord(3, i) > ymax) then
        k = i
        ymax = coord(3, i)
      end if
      if (coord(3, i) < ymin) then
        l = i
        ymin = coord(3, i)
      end if
    end do
    dim(3) = ymax - ymin
    ij(3, 1) = k
    if (k == l) then
      if (l == 1) then
        l = 2
      else
        l = 1
      end if
    end if
    ij(3, 2) = l
    write (iw, "(/,9X,A,/)") " MOLECULAR DIMENSIONS (Angstroms)"
    write (iw, "(9X,A)") "   Atom       Atom       Distance"
    write (iw, "(11X,A2,I6,3X,A2,I6,F12.5)") ((elemnt(nat(ij(j, i))), &
             & ij(j, i), i=1, 2), dim(j), j=1, 3)
end subroutine dimens
