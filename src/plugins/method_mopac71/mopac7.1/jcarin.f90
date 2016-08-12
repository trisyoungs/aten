      subroutine jcarin(xparam, step, preci, b, ncol, il, iu) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat, nvar, ndep
      use permanent_arrays, only : geo, coord, loc
      use euler_C, only : id, tvec, l1l, l1u, l2l, l2u, l3l, l3u
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:22  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use symtry_I 
      use gmetry_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: ncol 
      integer , intent(in) :: il 
      integer , intent(in) :: iu 
      real(double) , intent(in) :: step 
      logical , intent(in) :: preci 
      real(double) , intent(in) :: xparam(3*numat) 
      real(double) , intent(inout) :: b(iu - il + 1,numat*3**(id + 1)) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ivar, jvar, j, ij, ii, im, jl, kl, j1, j2
      real(double), dimension(3,numat*27) :: coold 
!-----------------------------------------------
!     JACOBIAN dCARTESIAN/dINTERNAL, WORKED OUT BY FINITE DIFFERENCE.
!  INPUT
!     XPARAM(*) : INTERNAL COORDINATES
!     STEP      : STEP SIZE FOR FINITE DIFFERENCE METHOD
!     PRECI     : .TRUE. IF 2-POINTS FINITE DIFFERENCES TO BE USED,
!                 .FALSE. OTHERWISE.
!  OUTPUT
!     B(NVAR,NCOL) : JACOBIAN, STEP TIME TOO LARGE.
!
!
      write(0,*) "Dimensions of b INPUT:", iu-il+1, numat*3**(id + 1)
      write(0,*) "COORD SIZE: ", size(coord, 1), size(coord, 2)
      ncol = 3*numat 
      if (id /= 0) ncol = ncol*(l1u - l1l + 1)*(l2u - l2l + 1)*(l3u - l3l + 1) 
!
!     INTERNAL OF CENTRAL POINT
      do ivar = 1, nvar 
        geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) 
      end do 
!
      if (id == 0) then 
!
!        MOLECULAR SYSTEM
!        ----------------
        jvar = 0 
        do ivar = il, iu 
          jvar = jvar + 1 
!        STEP FORWARD
          geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) + step 
          if (ndep /= 0) call symtry 
          call gmetry (geo, coord) 
          j = 0
          do j1 = 1, numat
            do j2 = 1, 3
              j = j + 1
            write(0,*) "J2J1", j2, j1
              b(jvar, j) = coord(j2, j1)
            end do
          end do
          geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) 
        end do 
        if (preci) then 
          jvar = 0 
          do ivar = il, iu 
            jvar = jvar + 1 
!           STEP BACKWARD
            geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) - step 
            if (ndep /= 0) call symtry 
            call gmetry (geo, coord) 
            j = 0
            do j1 = 1, numat
              do j2 = 1, 3
                j = j + 1
                b(jvar, j) = b(jvar, j) - coord(j2, j1)
              end do
            end do
            geo(loc(2, ivar), loc(1, ivar)) = xparam(ivar)
          end do
        else 
!           CENTRAL POINT
          if (ndep /= 0) call symtry 
          call gmetry (geo, coord) 
          jvar = 0 
          do ivar = il, iu
            jvar = jvar + 1
            j = 0
            do j1 = 1, numat
              do j2 = 1, 3
                j = j + 1
        write(0,*) " indices = ", jvar, j
                b(jvar, j) = b(jvar, j) - coord(j2, j1)
              end do
            end do
          end do
        endif 
      else 
!
!        SOLID STATE
!        -----------
        jvar = 0 
        do ivar = il, iu 
          jvar = jvar + 1 
!        STEP FORWARD
          geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) + step 
          if (ndep /= 0) call symtry 
          call gmetry (geo, coord) 
          ij = 0 
          do ii = 1, numat 
            do im = l1l, l1u 
              do jl = l2l, l2u 
                do kl = l3l, l3u 
                  b(jvar,ij+1:3+ij) = coord(:,ii) + tvec(:,1)*im + tvec(:,2)*jl&
                     + tvec(:,3)*kl 
                  ij = 3 + ij 
                end do 
              end do 
            end do 
          end do 
          geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) 
        end do 
        if (preci) then 
          jvar = 0 
          do ivar = il, iu 
            jvar = jvar + 1 
!           STEP BACKWARD
            geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) - step 
            if (ndep /= 0) call symtry 
            call gmetry (geo, coord) 
            ij = 0 
            do ii = 1, numat 
              do im = l1l, l1u 
                do jl = l2l, l2u 
                  do kl = l3l, l3u 
                    b(jvar,ij+1:3+ij) = b(jvar,ij+1:3+ij) - coord(:,ii) - tvec(&
                      :,1)*im - tvec(:,2)*jl - tvec(:,3)*kl 
                    ij = 3 + ij 
                  end do 
                end do 
              end do 
            end do 
            geo(loc(2,ivar),loc(1,ivar)) = xparam(ivar) 
          end do 
        else 
!           CENTRAL POINT
          if (ndep /= 0) call symtry 
          call gmetry (geo, coord) 
          ij = 0 
          do ii = 1, numat 
            do im = l1l, l1u 
              do jl = l2l, l2u 
                do kl = l3l, l3u 
                  ij = ij + 1 
                  coold(:,ij) = coord(:,ii) + tvec(:,1)*im + tvec(:,2)*jl + &
                    tvec(:,3)*kl 
                end do 
              end do 
            end do 
          end do 
          jvar = 0 
!vocl loop,scalar
          do ivar = il, iu 
            jvar = jvar + 1 
            b(jvar,:ncol) = b(jvar,:ncol) - coold(:ncol,1) 
          end do 
        endif 
      endif 
      return  
      end subroutine jcarin 
