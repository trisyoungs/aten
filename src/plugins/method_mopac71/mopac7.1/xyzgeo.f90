      subroutine xyzgeo(xyz, numat, na, nb, nc, degree, geo) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use bangle_I 
      use dihed_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: numat 
      real(double) , intent(in) :: degree 
      integer , intent(in) :: na(numat) 
      integer , intent(in) :: nb(numat) 
      integer , intent(inout) :: nc(numat) 
      real(double)  :: xyz(3,numat) 
      real(double)  :: geo(3,numat) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k, l, ii, i1 
      real(double) :: angl, tol, sum, r 
!-----------------------------------------------
!**********************************************************************
!
!   XYZGEO CONVERTS COORDINATES FROM CARTESIAN TO INTERNAL.
!
!     ON INPUT XYZ  = ARRAY OF CARTESIAN COORDINATES
!              NUMAT= NUMBER OF ATOMS
!              NA   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED
!                     BY DISTANCE
!              NB   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED
!                     BY ANGLE
!              NC   = NUMBERS OF ATOM TO WHICH ATOMS ARE RELATED
!                     BY DIHEDRAL
!
!    ON OUTPUT GEO  = INTERNAL COORDINATES IN ANGSTROMS, RADIANS,
!                     AND RADIANS
!
!**********************************************************************
      do i = 2, numat 
        j = na(i) 
        k = nb(i) 
        l = nc(i) 
        if (i >= 3) then 
          ii = i 
          call bangle (xyz, ii, j, k, geo(2,i)) 
          geo(2,i) = geo(2,i)*degree 
          if (i >= 4) then 
!
!   MAKE SURE DIHEDRAL IS MEANINGLFUL
!
            call bangle (xyz, j, k, l, angl) 
            tol = 0.2617993878D0 
            if (angl>3.14159265358979D0 - tol .or. angl<tol) then 
!
!  ANGLE IS UNSATISFACTORY, LET'S SEARCH FOR ANOTHER ATOM FOR
!  DEFINING THE DIHEDRAL.
   10         continue 
              sum = 100.D0 
              do i1 = 1, ii - 1 
                r = (xyz(1,i1)-xyz(1,k))**2 + (xyz(2,i1)-xyz(2,k))**2 + (xyz(3,&
                  i1)-xyz(3,k))**2 
                if (.not.(r<sum .and. i1/=j .and. i1/=k)) cycle  
                call bangle (xyz, j, k, i1, angl) 
                if (angl>=3.14159265358979D0 - tol .or. angl<=tol) cycle  
                sum = r 
                l = i1 
                nc(ii) = l 
              end do 
              if (sum>99.D0 .and. tol>0.1D0) then 
!
! ANYTHING WITHIN 5 DEGREES?
!
!  0.087266D0 IS 0.0174532925199432957D0*5.D0
!
                tol = 0.087266462599716478D0 
                go to 10 
              endif 
            endif 
            call dihed (xyz, ii, j, k, l, geo(3,i)) 
            geo(3,i) = geo(3,i)*degree 
          endif 
        endif 
        geo(1,i) = sqrt((xyz(1,i)-xyz(1,j))**2+(xyz(2,i)-xyz(2,j))**2+(xyz(3,i)&
          -xyz(3,j))**2) 
      end do 
      geo(1,1) = 0.D0 
      geo(2,1) = 0.D0 
      geo(3,1) = 0.D0 
      if (numat == 1) return  
      geo(2,2) = 0.D0 
      geo(3,2) = 0.D0 
      if (numat == 2) return  
      geo(3,3) = 0.D0 
      return  
      end subroutine xyzgeo 
