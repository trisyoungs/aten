      subroutine xyzint(xyz, numat, na, nb, nc, degree, geo) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numcal
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:04:59  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dihed_I 
      use bangle_I 
      use xyzgeo_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: degree 
      integer :: numat
      integer  :: na(numat) 
      integer  :: nb(numat) 
      integer  :: nc(numat) 
      real(double)  :: xyz(3,numat) 
      real(double)  :: geo(3,numat) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, j, im1, k 
      real(double) :: sum, r 

      save icalcn 
!-----------------------------------------------
!**********************************************************************
!
! XYZINT WORKS OUT THE INTERNAL COORDINATES OF A MOLECULE.
!        THE "RULES" FOR THE CONNECTIVITY ARE AS FOLLOWS:
!        ATOM I IS DEFINED AS BEING AT A DISTANCE FROM THE NEAREST
!        ATOM J, ATOM J ALREADY HAVING BEEN DEFINED.
!        ATOM I MAKES AN ANGLE WITH ATOM J AND THE ATOM K, WHICH HAS
!        ALREADY BEEN DEFINED, AND IS THE NEAREST ATOM TO J
!        ATOM I MAKES A DIHEDRAL ANGLE WITH ATOMS J, K, AND L. L HAVING
!        BEEN DEFINED AND IS THE NEAREST ATOM TO K, AND J, K AND L
!        HAVE A CONTAINED ANGLE IN THE RANGE 15 TO 165 DEGREES,
!        IF POSSIBLE.
!
!        IF(NA(2).EQ.1 THEN THE ORIGINAL CONNECTIVITY IS USED.
!
!        NOTE THAT GEO AND XYZ MUST NOT BE THE SAME IN THE CALL.
!
!   ON INPUT XYZ    = CARTESIAN ARRAY OF NUMAT ATOMS
!            DEGREE = 1 IF ANGLES ARE TO BE IN RADIANS
!            DEGREE = 57.29578 IF ANGLES ARE TO BE IN DEGREES
!
!**********************************************************************
      data icalcn/ 0/  
      if (.not.icalcn/=numcal .and. na(2)==(-1)) then 
        na(2) = 1 
        do i = 2, numat 
          j = na(i) 
          if (i > 3) call dihed (xyz, i, j, nb(i), nc(i), geo(3,i)) 
          if (i > 2) call bangle (xyz, i, j, nb(i), geo(2,i)) 
          geo(1,i) = sqrt((xyz(1,i)-xyz(1,j))**2+(xyz(2,i)-xyz(2,j))**2+(xyz(3,&
            i)-xyz(3,j))**2) 
        end do 
      else 
        if (na(2) == (-1)) icalcn = numcal 
        do i = 1, numat 
          na(i) = 2 
          nb(i) = 3 
          nc(i) = 4 
          im1 = i - 1 
          if (im1 == 0) cycle  
          sum = 1.D30 
          do j = 1, im1 
            r = (xyz(1,i)-xyz(1,j))**2 + (xyz(2,i)-xyz(2,j))**2 + (xyz(3,i)-xyz&
              (3,j))**2 
            if (.not.(r<sum .and. na(j)/=j .and. nb(j)/=j)) cycle  
            sum = r 
            k = j 
          end do 
!
!   ATOM I IS NEAREST TO ATOM K
!
          na(i) = k 
          if (i > 2) nb(i) = na(k) 
          if (i <= 3) cycle  
          nc(i) = nb(k) 
!
!   FIND ANY ATOM TO RELATE TO NA(I)
!
        end do 
      endif 
      na(1) = 0 
      nb(1) = 0 
      nc(1) = 0 
      if (numat > 1) then 
        nb(2) = 0 
        nc(2) = 0 
        if (numat > 2) nc(3) = 0 
      endif 
      call xyzgeo (xyz, numat, na, nb, nc, degree, geo) 
      return  
      end subroutine xyzint
