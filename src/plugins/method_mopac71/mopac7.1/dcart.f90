      subroutine dcart(coord, dxyz) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : nfirst, nlast, nat, p, pa, pb
      use euler_C, only : tvec, id, l1l, l1u, l2l, l2u, l3l, l3u 
      USE molkst_C, only : numat, numcal, keywrd
      USE molmec_C, only : nnhco, nhco, htype
      USE funcon_C, only : fpc_9 
      USE chanel_C, only : iw, irot 
      USE elemts_C, only : elemnt 
      USE cosmo_C, only : useps
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:54:27  03/14/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use analyt_I 
      use dhc_I 
      use dihed_I 
      use diegrd_I 
   !   use xyzcry_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: coord(3,numat) 
      real(double)  :: dxyz(3,numat)  
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, ncells, icuc, numtot, i, j, ii, iii, im1, if, il, jj, &
        jjj, jf, jl, kkkk, ik, jk, kl, l, ij, k, loop
      real(double), dimension(3,numat) :: work2 
      real(double), dimension(171) :: pdi, padi, pbdi 
      real(double), dimension(3,2) :: cdi 
      real(double), dimension(3) :: eng 
      integer :: ndi(2)
      real(double) :: chnge, chnge2, const, aa, ee, deriv, del, angle, refh, &
        heat, sum, sumx, sumy, sumz 
      logical :: debug, force, makep, anader, large 

      save debug, force, anader, large, chnge, chnge2, icalcn 
!-----------------------------------------------
!***********************************************************************
!
!    DCART CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
!          CARTESIAN COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
!
!    THE MAIN ARRAYS IN DCART ARE:
!        DXYZ   ON EXIT CONTAINS THE CARTESIAN DERIVATIVES.
!
!*********************************************************************** 
      data icalcn/ 0/  
      data chnge/ 1.D-4/  
      chnge2 = chnge*0.5D0 
!
! CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
! CHNGE2=CHNGE/2
!
      if (icalcn /= numcal) then 
        icalcn = numcal 
        const = fpc_9
        large = index(keywrd,'LARGE') /= 0 
        anader = index(keywrd,'ANALYT') /= 0 
        debug = index(keywrd,'DCART') /= 0 
        force = index(keywrd,'PREC') + index(keywrd,'FORCE') /= 0 
      endif 
      ncells = (l1u - l1l + 1)*(l2u - l2l + 1)*(l3u - l3l + 1) 
      icuc = (ncells + 1)/2 
      numtot = numat*ncells 
      dxyz(:,:numtot) = 0.D0 
      if (anader) rewind irot 
      do ii = 1, numat 
        iii = ncells*(ii - 1) 
        im1 = ii - 1 
        if = nfirst(ii) 
        il = nlast(ii) 
        ndi(2) = nat(ii) 
        cdi(:,2) = coord(:,ii) 
        do jj = 1, im1 
          jjj = ncells*(jj - 1) 
!  FORM DIATOMIC MATRICES
          jf = nfirst(jj) 
          jl = nlast(jj) 
!   GET FIRST ATOM
          ndi(1) = nat(jj) 
          makep = .TRUE. 
          kkkk = 0 
          do ik = l1l, l1u 
            do jk = l2l, l2u 
              do kl = l3l, l3u 
                kkkk = kkkk + 1 
                cdi(:,1) = coord(:,jj) + tvec(:,1)*ik + tvec(:,2)*jk + tvec(:,3&
                  )*kl 
                if (makep) then 
                  makep = .FALSE. 
                  ij = 0 
                  do i = jf, jl 
                    k = i*(i - 1)/2 + jf - 1 
                    if (i - jf + 1 > 0) then 
                      padi(ij+1:i-jf+1+ij) = pa(k+1:i-jf+1+k) 
                      pbdi(ij+1:i-jf+1+ij) = pb(k+1:i-jf+1+k) 
                      pdi(ij+1:i-jf+1+ij) = p(k+1:i-jf+1+k) 
                      ij = i - jf + 1 + ij 
                    endif 
                  end do 
! GET SECOND ATOM FIRST ATOM INTERSECTION
                  do i = if, il 
                    l = i*(i - 1)/2 
                    k = l + jf - 1 
                    if (jl - jf + 1 > 0) then 
                      padi(ij+1:jl-jf+1+ij) = pa(k+1:jl-jf+1+k) 
                      pbdi(ij+1:jl-jf+1+ij) = pb(k+1:jl-jf+1+k) 
                      pdi(ij+1:jl-jf+1+ij) = p(k+1:jl-jf+1+k) 
                      ij = jl - jf + 1 + ij 
                    endif 
                    k = l + if - 1 
                    if (i - if + 1 > 0) then 
                      padi(ij+1:i-if+1+ij) = pa(k+1:i-if+1+k) 
                      pbdi(ij+1:i-if+1+ij) = pb(k+1:i-if+1+k) 
                      pdi(ij+1:i-if+1+ij) = p(k+1:i-if+1+k) 
                      ij = i - if + 1 + ij 
                    endif 
                  end do 
                endif 
                if (anader) then 
                  call analyt (pdi, padi, pbdi, cdi, ndi, jf, jl, if, il, eng) 
                  dxyz(:,iii+icuc) = dxyz(:,iii+icuc) - eng 
                  dxyz(:,jjj+kkkk) = dxyz(:,jjj+kkkk) + eng 
                else 
                  if (.not.force) then 
                    cdi(1,1) = cdi(1,1) + chnge2 
                    cdi(2,1) = cdi(2,1) + chnge2 
                    cdi(3,1) = cdi(3,1) + chnge2 
                    call dhc (pdi, padi, pbdi, cdi, ndi, jf, jl, if, il, aa, 1) 
                  endif 
                  do k = 1, 3 
                    if (force) then 
                      cdi(k,2) = cdi(k,2) - chnge2 
                      call dhc (pdi, padi, pbdi, cdi, ndi, jf, jl, if, il, aa, 1) 
                    endif 
                    cdi(k,2) = cdi(k,2) + chnge 
                    call dhc (pdi, padi, pbdi, cdi, ndi, jf, jl, if, il, ee, 2) 
                    cdi(k,2) = cdi(k,2) - chnge2 
                    if (.not.force) cdi(k,2) = cdi(k,2) - chnge2 
                    deriv = (aa - ee)*const/chnge 
                    dxyz(k,iii+icuc) = dxyz(k,iii+icuc) - deriv 
                    dxyz(k,jjj+kkkk) = dxyz(k,jjj+kkkk) + deriv 
!      ENDIF
                  end do 
                endif 
              end do 
            end do 
          end do 
        end do 
      end do 
      if (nnhco /= 0) then 
!
!   NOW ADD IN MOLECULAR-MECHANICS CORRECTION TO THE H-N-C=O TORSION
!
        del = 1.D-8 
        do i = 1, nnhco 
          do j = 1, 4 
            do k = 1, 3 
              coord(k,nhco(j,i)) = coord(k,nhco(j,i)) - del 
              call dihed (coord, nhco(1,i), nhco(2,i), nhco(3,i), nhco(4,i), &
                angle) 
              refh = htype*sin(angle)**2 
              coord(k,nhco(j,i)) = coord(k,nhco(j,i)) + del*2.D0 
              call dihed (coord, nhco(1,i), nhco(2,i), nhco(3,i), nhco(4,i), &
                angle) 
              coord(k,nhco(j,i)) = coord(k,nhco(j,i)) - del 
              heat = htype*sin(angle)**2 
              sum = (refh - heat)/(2.D0*del) 
              dxyz(k,nhco(j,i)) = dxyz(k,nhco(j,i)) - sum 
            end do 
          end do 
        end do 
      endif 
! COSMO change A. Klamt
! analytic calculation of the gradient of the dielectric energy A.Klamt
      if (useps) call diegrd () 
!     DO 170 I=1,6
! 170 LSTOR1(I)=LSTOR2(I)
      if (.not.debug) return  
      write (iw, &
      '(2/10X,''CARTESIAN COORDINATE DERIVATIVES'',2/3X,       ''NUMBER  ATOM '&
      &',5X,''X'',12X,''Y'',12X,''Z'',/)') 
      if (ncells == 1) then 
        write (iw, '(2I6,F13.6,2F13.6)') (i,nat(i),(dxyz(j,i),j=1,3),i=1,numtot&
          ) 
      else if (large) then 
        write (iw, '(2I6,F13.6,2F13.6)') (i,nat((i-1)/ncells+1),(dxyz(j,i),j=1,&
          3),i=1,numtot) 
      else 
        write (iw, '(2I6,F13.6,2F13.6)') (i,nat((i-1)/ncells+1),(dxyz(j,i) + &
          dxyz(j,i+1) + dxyz(j,i+2),j=1,3),i=1,numtot,3) 
      endif 
      if (anader) rewind irot 
! end of COSMO (A. Klamt) changes
      if (.not.debug) return  
      write (iw, &
      '(2/10X,''CARTESIAN COORDINATE DERIVATIVES'', /2X,              ''NO. AT.&
      &'',5X,''X'',12X,''Y'',12X,''Z'',/)') 
      if (ncells == 1) then 
        write (iw, '(I6,A2,3F13.6)') (i,elemnt(nat(i)),(dxyz(j,i),j=1,3),i=1,&
          numtot) 
      else if (large) then 
        loop = 0 
        do i = 1, numat 
          sumx = 0.D0 
          sumy = 0.D0 
          sumz = 0.D0 
          do ik = l1l, l1u 
            do jk = l2l, l2u 
              do kl = l3l, l3u 
                loop = loop + 1 
                sumx = sumx + dxyz(1,loop) 
                sumy = sumy + dxyz(2,loop) 
                sumz = sumz + dxyz(3,loop) 
                if (abs(dxyz(1,loop)) + abs(dxyz(2,loop)) + abs(dxyz(3,loop))&
                   <= 1.D-5) cycle  
                write (iw, '(I6,A2,F13.6,2F13.6,3I4)') i, elemnt(nat(i)), (dxyz&
                  (k,loop),k=1,3), ik, jk, kl 
              end do 
            end do 
          end do 
          work2(1,i) = sumx 
          work2(2,i) = sumy 
          work2(3,i) = sumz 
        end do 
        write (iw, *) ' Central Unit Cell Derivatives' 
        write (iw, '(I6,A2,F13.6,2F13.6)') (i,elemnt(nat(i)),(work2(j,i),j=1,3)&
          ,i=1,numat) 
        if (id == 3) call xyzcry (tvec, numat, work2, iw) 
      else 
        write (iw, '(I6,A2,F13.6,2F13.6)') (i,elemnt(nat((i-1)/ncells+1)),(dxyz&
          (j,i) + dxyz(j,i+1) + dxyz(j,i+2),j=1,3),i=1,numtot,3) 
      endif 
      if (anader) rewind irot 
      return  
      end subroutine dcart 
