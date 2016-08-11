      subroutine molsym(coord, ierror, r) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:29  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use bldsym_I, only :  
      use rsp_I, only :  
      use symopr_I, only :  
      use plato_I, only :  
      use chi_I, only :  
      use rotmol_I, only :  
      use orient_I, only :  
      use cartab_I, only :  
      use permanent_arrays, only : nat, atmass
      use symmetry_C, only : ielem, cub, name
      use chanel_C, only : iw
      use molkst_C, only : numat, keywrd, moperr
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: ierror 
      real(double)  :: coord(3,numat) 
      real(double)  :: r(3,3) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(6) :: icyc 
      integer :: i, j, k, ij, iturn, iqual, kndex, icheck, naxes, iz, ix, iy 
      real(double), dimension(6) :: f 
      real(double), dimension(3) :: ew, help 
      real(double), dimension(3,3) :: rhelp 
      real(double), dimension(3) :: shift 
      real(double) :: toler, wmol, sum, rxy, tole, distxy, rmin, sina, &
        cosa, theta, total 
      logical :: linear, cubic, axis, sphere, debug, reorie 
!-----------------------------------------------
!***************************************************************
!                                                              *
!     Credits not yet written                                  *
!                                                              *
!***************************************************************
!
!    MOLSYM CALCULATES THE MOLECULAR SYMMETRY AS A POINT-GROUP SYMBOL
!
      debug = index(keywrd,' MOLSYM') /= 0 
!
!   REORIE IS NORMALLY .TRUE.   IF SET .FALSE., THEN THE ORIENTATION
!   SUPPLIED WILL BE USED IN DETERMINING SYMMETRY
!
      reorie = index(keywrd,' NOREOR') == 0 
      toler = 0.1D0 
      ierror = 0 
      name = '????' 
      do i = 1, 3 
        cub(i,:) = 0.D0 
        cub(i,i) = 1.D0 
      end do 
!
!  Now to put all the symmetry operations into the array ELEM
!   In order, the operations in ELEM are
!
!   1 C2(X)       6 Sigma(YZ)   11 C6    16 S8
!   2 C2(Y)       7 inversion   12 C7    17 S10
!   3 C2(Z)       8 C3          13 C8    18 S12
!   4 Sigma(XY)   9 C4          14 S4    19 1 if cubic, 0 otherwise.
!   5 Sigma(XZ)  10 C5          15 S6    20 1 if infinite, 0 otherwise.
!
!
      do i = 1, 18 
        call bldsym (i, i) 
        ielem(i) = 0 
      end do 
      ielem(19) = 0 
      ielem(20) = 0 
!
!   Calculate Moments of Inertia and Axes of Inertia.
!   First, center the molecule.
!
      shift = 0.D0 
      wmol = 0.D0 
      do i = 1, numat 
        wmol = wmol + atmass(i) 
        shift = shift + atmass(i)*coord(:,i) 
      end do 
      ij = 0 
      do i = 1, 3 
        shift(i) = shift(i)/wmol 
        coord(i,:numat) = coord(i,:numat) - shift(i) 
        do j = 1, i 
          ij = ij + 1 
          f(ij) = ij*1.D-8 
          do k = 1, numat 
            f(ij) = f(ij) + atmass(k)*coord(i,k)*coord(j,k) 
          end do 
        end do 
      end do 
      if (.not.reorie) then 
!
!     USE ORIENTATION AS SUPPLIED
!
!     FOR SYMMETRY, ORIENTATION IS: Z=PRINCIPAL AXIS, X,Y=SECONDARY
!
!      WRITE(IW,*)'H IN MOLSYM'
        sum = 0.D0 
        do i = 1, 5 
          sum = sum + abs(f(i)) 
        end do 
        linear = sum < 0.01D0 
        sphere = linear .and. abs(f(6))<0.01D0 
        cubic = abs(f(1)-f(3))<0.01D0 .and. abs(f(1)-f(6))<0.01D0 .and. abs(f(2&
          ))+abs(f(4))+abs(f(5))<0.01D0 
        do i = 1, 3 
          r(i,:) = 0.D0 
          r(i,i) = 1.D0 
        end do 
      else 
        call rsp (f, 3, 3, ew, r) 
        r(1,3) = r(2,1)*r(3,2) - r(3,1)*r(2,2) 
        r(2,3) = r(3,1)*r(1,2) - r(1,1)*r(3,2) 
        r(3,3) = r(1,1)*r(2,2) - r(2,1)*r(1,2) 
!
!   Determine if molecule belongs to a special group
!
        linear = ew(2) < 1.D-2 
        sphere = ew(3) < 1.D-2 
        cubic = ew(3) - ew(1) < 5.D-3*max(ew(3),40.D0) 
      endif 
      if (sphere) then 
!
!   Set flags 8 and 12 simultaneously to 1 - impossible for
!   non-spherical systems.
!
        ielem(7) = 1 
        ielem(8) = 1 
        ielem(10) = 1 
        ielem(20) = 1 
!
!  Make sure that C5 axis is different from C3 - this eliminates
!  accidental degeneracy.
!
        cub = 0.D0 
        cub(1,2) = 1.D0 
        cub(2,3) = 1.D0 
        cub(3,1) = 1.D0 
        go to 270 
      else if (linear) then 
!
!   Molecule is C-infinity-v or D-infinity-h
!
        call symopr (numat, coord, 1, r) 
        ielem(20) = 1 
        go to 250 
      endif 
      if (reorie) then 
        if (.not.cubic .and. ew(3)-ew(2)<1.D-2*ew(3)) then 
!
!   Molecule has two-fold degeneracy ( Cn, Cnv, Dn, Dnh, Sn, n>2)
!
          do i = 1, 3 
            rxy = -r(i,1) 
            r(i,1) = r(i,3) 
            r(i,3) = rxy 
          end do 
          rxy = ew(1) 
          ew(1) = ew(3) 
          ew(3) = rxy 
        endif 
        axis = abs(ew(1)-ew(2)) < 0.01D0*ew(2) 
      else 
        axis = abs(f(1)-f(3)) < 0.01D0*f(6) 
      endif 
!
!   Is there a plane of symmetry perpendicular to the Z axis?
!
      call symopr (numat, coord, 1, r) 
      if (cubic) then 
        ielem(19) = 1 
        call plato (coord, numat, r) 
        if (moperr) return  
      endif 
      if (axis) then 
!
!  Molecule has degeneracy.  At this point, the Z-axis is
!  defined.  The molecule is Cn Cnv Cnh Dn, etc, or a special group,
!  e.g. Td, Oh, Ih.
!
  130   continue 
        iturn = 7 
        j = 0 
!
!   Check for the existance of a Cn(2<n<9) or Sn(1<0.5*n<7)
!
        do i = 8, 18 
!
!   Make certain that TOLE gets tighter as the n in Cn increases.
!
          if (i < 14) then 
            tole = toler*9.D0/(i - 5)**2 
          else 
            tole = toler*16.D0/(2*i - 24)**2 
          endif 
          call chi (tole, coord, i, iqual) 
!
!   If Cn, then set ITURN = n  (Remember ELEM(8) is C3, therefore offset
!                               by 5)
!
          if (ielem(i)/=1 .or. i>=14) cycle  
          if (iturn > 9) then 
            toler = toler*0.5D0 
            go to 130 
          endif 
          iturn = i 
        end do 
        if (ielem(14) + ielem(15) + ielem(17)>1 .or. ielem(15)+ielem(16)+ielem(&
          17)>1 .or. ielem(16)+ielem(17)+ielem(18)>1) then 
          toler = toler*0.5D0 
          go to 130 
        endif 
        iturn = iturn - 5 
        if (debug) then 
          write (iw, '(A)') ' after checking 8-18' 
          write (iw, '(20I3)') ielem 
        endif 
!
!  Now use two adjacent equivalent atoms, not on the
!  Z axis, to define the X-axis.
!
        do i = 1, numat 
          distxy = coord(1,i)**2 + coord(2,i)**2 
          if (distxy < toler) cycle  
!
!   Atom I is the first atom.
!
          rmin = 1000.D0 
          kndex = 0 
          do j = i + 1, numat 
            if (abs(abs(coord(3,i))-abs(coord(3,j))) > 0.2D0) cycle  
            rxy = coord(1,j)**2 + coord(2,j)**2 
            if (abs(rxy - distxy)>toler .or. nat(i)/=nat(j)) cycle  
            rxy = (coord(1,i)-coord(1,j))**2 + (coord(2,i)-coord(2,j))**2 
            if (rxy > rmin) cycle  
            kndex = j 
            rmin = rxy 
          end do 
!
!   Atom KNDEX is the second, adjacent, atom, equivalent to I.
!
          exit  
        end do 
        if (kndex < 1) then 
!
!  System does not have a Cn axis!  Go back and treat it as an Abelian
!  system.
!
          axis = .FALSE. 
          go to 190 
        endif 
        help(1) = coord(1,i) + coord(1,kndex) 
        help(2) = coord(2,i) + coord(2,kndex) 
        distxy = sqrt(help(1)**2+help(2)**2) 
        sina = help(2)/distxy 
        cosa = help(1)/distxy 
        call rotmol (numat, coord, sina, cosa, 1, 2, r) 
!
!   Is there a Sigma(XZ) plane of symmetry?
!
        call chi (toler, coord, 5, iqual) 
        if (ielem(5) /= 1) then 
!
!    Is there a C2(X) axis of rotation?
!
          call chi (toler, coord, 1, iqual) 
          if (ielem(1) /= 0) then 
!
!    Check for an improper axis of rotation.
!
            theta = 1.5707963268D0/dble(iturn) 
            sina = sin(theta) 
            cosa = cos(theta) 
            icheck = 0 
  180       continue 
            call rotmol (numat, coord, sina, cosa, 1, 2, r) 
            if (icheck > 0) go to 190 
            call chi (toler, coord, 5, iqual) 
            if (ielem(5) > 0) go to 190 
            icheck = 1 
            sina = -sina 
            go to 180 
          endif 
        endif 
      endif 
  190 continue 
      if (cubic) call orient (numat, coord, r) 
      if (debug) then 
        write (iw, '(A)') ' C2 and sigma-v     ' 
        write (iw, '(20I3)') ielem 
      endif 
      if (.not.axis) then 
        toler = 0.2D0 
!
!   Molecule belongs to one of the 8 Abelian groups
!   (C1, C2, Ci, Cs, C2v, D2, C2h, or D2h)
!
        do i = 1, 6 
          call chi (toler, coord, i, iqual) 
          icyc(i) = (1 + iqual)*ielem(i) 
        end do 
        if (reorie) then 
          naxes = ielem(1) + ielem(2) + ielem(3) 
!
!    Special handling for the Abelian Groups:  Determine
!    the principal axis by atom counts.
!
          if (naxes <= 1) then 
            iz = 1 
            if (ielem(1) == 1) go to 220 
            iz = 2 
            if (ielem(2) == 1) go to 220 
            iz = 3 
            if (ielem(3) == 1) go to 220 
            if (icyc(5) > icyc(4)) iz = 2 
            if (icyc(6) > icyc(7-iz)) iz = 1 
            go to 220 
          endif 
          iz = 1 
          if (icyc(2) > icyc(1)) iz = 2 
          if (icyc(3) > icyc(iz)) iz = 3 
  220     continue 
          icyc(7-iz) = -1 
          ix = 1 
          if (icyc(5) > icyc(6)) ix = 2 
          if (icyc(4) > icyc(7-ix)) ix = 3 
          iy = 6 - ix - iz 
!
!  Whew!   Now to re-orient the molecule so the the principal
!  axis is 'Z'
!
          rhelp(:,1) = r(:,ix) 
          rhelp(:,2) = r(:,iy) 
          rhelp(1,3) = r(2,ix)*r(3,iy) - r(3,ix)*r(2,iy) 
          rhelp(2,3) = r(3,ix)*r(1,iy) - r(1,ix)*r(3,iy) 
          rhelp(3,3) = r(1,ix)*r(2,iy) - r(2,ix)*r(1,iy) 
          call symopr (numat, coord, -1, r) 
          r = rhelp 
          call symopr (numat, coord, 1, r) 
        endif 
!
!   And re-calculate the first 7 Characters.
!   (C2(X), C2(Y), C2(Z), Sigma(XY), Sigma(XZ), Sigma(YZ), i)
!
      endif 
  250 continue 
      do i = 1, 7 
        call chi (toler, coord, i, iqual) 
      end do 
      if (debug) then 
        write (iw, '(A)') ' After re-doing 1-7' 
        write (iw, '(20I3)') ielem 
      endif 
  270 continue 
      call symopr (numat, coord, -1, r) 
      total = ew(1) + ew(2) + ew(3) 
      ew = total - ew 
      call cartab 
      return  
      end subroutine molsym 
