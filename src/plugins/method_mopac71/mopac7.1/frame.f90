      subroutine frame(fmat, numat, mode) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : coord, atmass
      use euler_C, only : id
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:15  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use axis_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: numat 
      integer  :: mode 
      real(double) , intent(inout) :: fmat(*)    
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k, n3, l 
      real(double), dimension(6,3*numat) :: vib 
      real(double), dimension(3,3) :: rot 
      real(double) :: shift(6) 
      real(double), dimension(3,numat) :: coord1 
      real(double) :: sum, wtmass, x, y, z, sum1, sum2, sum3, sum4, sum5&
        , sum6 
!-----------------------------------------------
!**********************************************************************
!
!   FRAME APPLIES AN RIGID ORIENTATION TO THE MOLECULE IN A FORCE
!         CALCULATION. THE TRANSLATIONS ARE GIVEN A 'FORCE CONSTANT'
!         OF T(X)=500 MILLIDYNES/ANGSTROM
!            T(Y)=600 MILLIDYNES/ANGSTROM
!            T(Z)=700 MILLIDYNES/ANGSTROM
!         AND THE ROTATIONS ARE GIVEN A 'FORCE CONSTANT' OF
!            R(X)=800 MILLIDYNES/ANGSTROM
!            R(Y)=900 MILLIDYNES/ANGSTROM
!            R(Z)=1000 MILLIDYNES/ANGSTROM,
!    THE ROTATIONS ARE MADE ABOUT AXES DETERMINED BY THE MOMENTS
!    OF INERTIA, WHICH IN TURN DEPEND ON THE ISOTOPIC MASSES. FOR
!    THE NORMAL FREQUENCY CALCULATION THESE ARE THE REAL MASSES,
!    FOR THE FORCE CALCULATION THEY ARE ALL UNITY.
!**********************************************************************
      call axis (coord, numat, sum, x, y, z, mode, rot) 
      do i = 1, numat 
        do j = 1, 3 
          sum = 0.D0 
          do k = 1, 3 
            sum = sum + coord(k,i)*rot(k,j) 
          end do 
          coord1(j,i) = sum 
        end do 
      end do 
      n3 = numat*3 
      j = 0 
      wtmass = 1.D0 
      if (mode == 1) then 
        do i = 1, numat 
          wtmass = sqrt(atmass(i)) 
          j = j + 1 
          vib(1,j) = wtmass 
          vib(2,j) = 0.D0 
          vib(3,j) = 0.D0 
          vib(4,j) = 0.D0 
          vib(5,j) = coord1(3,i)*wtmass 
          vib(6,j) = coord1(2,i)*wtmass 
          j = j + 1 
          vib(1,j) = 0.D0 
          vib(2,j) = wtmass 
          vib(3,j) = 0.D0 
          vib(4,j) = coord1(3,i)*wtmass 
          vib(5,j) = 0.D0 
          vib(6,j) = -coord1(1,i)*wtmass 
          j = j + 1 
          vib(1,j) = 0.D0 
          vib(2,j) = 0.D0 
          vib(3,j) = wtmass 
          vib(4,j) = -coord1(2,i)*wtmass 
          vib(5,j) = -coord1(1,i)*wtmass 
          vib(6,j) = 0.D0 
        end do 
      else 
        vib(1,j+1:numat*3-2+j:3) = wtmass 
        vib(2,j+1:numat*3-2+j:3) = 0.D0 
        vib(3,j+1:numat*3-2+j:3) = 0.D0 
        vib(4,j+1:numat*3-2+j:3) = 0.D0 
        vib(5,j+1:numat*3-2+j:3) = coord1(3,:numat)*wtmass 
        vib(6,j+1:numat*3-2+j:3) = coord1(2,:numat)*wtmass 
        vib(1,j+2:numat*3-1+j:3) = 0.D0 
        vib(2,j+2:numat*3-1+j:3) = wtmass 
        vib(3,j+2:numat*3-1+j:3) = 0.D0 
        vib(4,j+2:numat*3-1+j:3) = coord1(3,:numat)*wtmass 
        vib(5,j+2:numat*3-1+j:3) = 0.D0 
        vib(6,j+2:numat*3-1+j:3) = -coord1(1,:numat)*wtmass 
        vib(1,j+3:numat*3+j:3) = 0.D0 
        vib(2,j+3:numat*3+j:3) = 0.D0 
        vib(3,j+3:numat*3+j:3) = wtmass 
        vib(4,j+3:numat*3+j:3) = -coord1(2,:numat)*wtmass 
        vib(5,j+3:numat*3+j:3) = -coord1(1,:numat)*wtmass 
        vib(6,j+3:numat*3+j:3) = 0.D0 
      endif 
      j = 1 
      do i = 1, numat 
        do k = 4, 6 
          x = vib(k,j) 
          y = vib(k,j+1) 
          z = vib(k,j+2) 
          vib(k,j) = x*rot(1,1) + y*rot(1,2) + z*rot(1,3) 
          vib(k,j+1) = x*rot(2,1) + y*rot(2,2) + z*rot(2,3) 
          vib(k,j+2) = x*rot(3,1) + y*rot(3,2) + z*rot(3,3) 
        end do 
        j = j + 3 
      end do 
      sum1 = 0.D0 
      sum2 = 0.D0 
      sum3 = 0.D0 
      sum4 = 0.D0 
      sum5 = 0.D0 
      sum6 = 0.D0 
      do i = 1, n3 
        sum1 = sum1 + vib(1,i)**2 
        sum2 = sum2 + vib(2,i)**2 
        sum3 = sum3 + vib(3,i)**2 
        sum4 = sum4 + vib(4,i)**2 
        sum5 = sum5 + vib(5,i)**2 
        sum6 = sum6 + vib(6,i)**2 
      end do 
      if (sum1 > 1.D-5) sum1 = sqrt(1.D0/sum1) 
      if (sum2 > 1.D-5) sum2 = sqrt(1.D0/sum2) 
      if (sum3 > 1.D-5) sum3 = sqrt(1.D0/sum3) 
      if (sum4 > 1.D-5) sum4 = sqrt(1.D0/sum4) 
      if (sum5 > 1.D-5) sum5 = sqrt(1.D0/sum5) 
      if (sum6 > 1.D-5) sum6 = sqrt(1.D0/sum6) 
      if (id /= 0) then 
        sum4 = 0.D0 
        sum5 = 0.D0 
        sum6 = 0.D0 
      endif 
      vib(1,:n3) = vib(1,:n3)*sum1 
      vib(2,:n3) = vib(2,:n3)*sum2 
      vib(3,:n3) = vib(3,:n3)*sum3 
      vib(4,:n3) = vib(4,:n3)*sum4 
      vib(5,:n3) = vib(5,:n3)*sum5 
      vib(6,:n3) = vib(6,:n3)*sum6 
      do i = 1, 6 
        shift(i) = 40000.D0 + i*100.D0 
      end do 
      l = 0 
      do i = 1, n3 
        do j = 1, i 
          l = l + 1 
          sum1 = 0.D0 
          do k = 1, 6 
            sum1 = sum1 + vib(k,i)*shift(k)*vib(k,j) 
          end do 
          fmat(l) = fmat(l) + sum1 
        end do 
      end do 
      return  
      end subroutine frame 
