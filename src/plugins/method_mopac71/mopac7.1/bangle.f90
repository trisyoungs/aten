      subroutine bangle(xyz, i, j, k, angle) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double
      use molkst_C, only : numat 
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:33:12  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: i 
      integer , intent(in) :: j 
      integer , intent(in) :: k 
      real(double) , intent(out) :: angle 
      real(double) , intent(in) :: xyz(3,numat) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(double) :: d2ij, d2jk, d2ik, xy, temp 
!-----------------------------------------------
!********************************************************************
!
! BANGLE CALCULATES THE ANGLE BETWEEN ATOMS I,J, AND K. THE
!        CARTESIAN COORDINATES ARE IN XYZ.
!
!********************************************************************
      d2ij = (xyz(1,i)-xyz(1,j))**2 + (xyz(2,i)-xyz(2,j))**2 + (xyz(3,i)-xyz(3,&
        j))**2 
      d2jk = (xyz(1,j)-xyz(1,k))**2 + (xyz(2,j)-xyz(2,k))**2 + (xyz(3,j)-xyz(3,&
        k))**2 
      d2ik = (xyz(1,i)-xyz(1,k))**2 + (xyz(2,i)-xyz(2,k))**2 + (xyz(3,i)-xyz(3,&
        k))**2 
      xy = sqrt(d2ij*d2jk) 
      temp = 0.5D0*(d2ij + d2jk - d2ik)/xy 
      temp = min(1.0D0,temp) 
      temp = dmax1(-1.0D0,temp) 
      angle = acos(temp) 
      return  
      end subroutine bangle 
