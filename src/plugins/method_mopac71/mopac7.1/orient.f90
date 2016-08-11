      subroutine orient(numat, coord, r) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use symmetry_C, only : cub, ielem
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:31  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rotmol_I 
      use chi_I 
      use mult33_I 
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
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, jota, iqual, j 
      real(double), dimension(2) :: wink 
      real(double) :: toler, wink2, sina, cosa, sinb, cosb 

      save wink, toler 
!-----------------------------------------------
!***********************************************************************
!
!    ORIENT is part of the SYMMETRY package
!           In cubic systems the three moments of inertia are identical.
!           Therefore, use the nearest atom to the center to determine
!           the principal axis (Z)
!
!**********************************************************************
      data toler/ 0.1D0/  
      data wink(1), wink(2)/ 0.955316618125D0, 0.65235813978437D0/  
!
!   WINK(1) rotates a cubic molecule through half the tetrahedral angle.
!   WINK(2) does something similar for an icosahedral system.
!
!   WINK(1)=ACOS(SQRT(1/3))
!   WINK(2)=ACOS((47+21*SQRT(5))/(75+33*SQRT(5)))
!           = angle from a vertex of an icosahedron to the center and th
!             to the middle of the triangular face
!           = 37.377366 degrees
!
!
      wink2 = 0.D0 
      if (ielem(8) >= 1) then 
!
!   Check for a S4 or C5 axis
!
        do i = 1, 2 
          jota = 18 - 4*i 
          wink2 = wink(i) 
          sina = sin(wink2) 
          cosa = cos(wink2) 
          call rotmol (numat, coord, sina, cosa, 1, 3, r) 
          call chi (toler, coord, jota, iqual) 
          if (ielem(jota) > 0) exit  
          if (i == 1) then 
            call chi (toler, coord, 3, iqual) 
            if (ielem(3) == 1) exit  
          endif 
          wink2 = -wink2 
          sinb = sin(2.D0*wink2) 
          cosb = cos(2.D0*wink2) 
          call rotmol (numat, coord, sinb, cosb, 1, 3, r) 
          call chi (toler, coord, jota, iqual) 
          if (ielem(jota) > 0) exit  
          if (i == 1) then 
            call chi (toler, coord, 3, iqual) 
            if (ielem(3) == 1) exit  
          endif 
          call rotmol (numat, coord, sina, cosa, 1, 3, r) 
        end do 
        call chi (toler, coord, 9, iqual) 
!
!   Check on all IELEM registers
        if (ielem(10) > 0) call chi (toler, coord, 17, iqual) 
      else 
!
!   No C3 axis, therefore not T, Td, Th, O, or Oh.
!
        wink2 = -wink(1) 
        if (ielem(10) > 0) wink2 = -wink(2) 
        sina = -sin(wink2) 
        cosa = cos(wink2) 
        call rotmol (numat, coord, sina, cosa, 1, 3, r) 
        call chi (toler, coord, 8, iqual) 
        call rotmol (numat, coord, (-sina), cosa, 1, 3, r) 
        if (ielem(8) <= 0) then 
          if (ielem(9) <= 0) then 
            wink2 = -wink2 
          else 
            call rotmol (numat, coord, 0.707106781186D0, 0.707106781186D0, 1, 2&
              , r) 
          endif 
        endif 
      endif 
      j = sum(ielem(:17)) 
      if (j==2 .and. ielem(1)+ielem(8)==2) return  
      cub(1,1) = cos(wink2) 
      cub(3,3) = cub(1,1) 
      cub(1,3) = sin(wink2) 
      cub(3,1) = -cub(1,3) 
      call mult33 (cub, 8) 
      call mult33 (cub, 15) 
      call chi (toler, coord, 8, iqual) 
      j = iqual ! dummy use of iqual
      call chi (toler, coord, 15, iqual) 
      if (j + iqual == 0) return ! dummy use of iqual
      return  
      end subroutine orient 
