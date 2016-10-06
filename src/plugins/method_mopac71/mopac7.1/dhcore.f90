      subroutine dhcore(coord, h, w, ww, enuclr, nati, natx, step) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numcal, norbs, numat, lm6, &
      method_dorbs
      use permanent_arrays, only : nat, nfirst, nlast, nw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:08  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use h1elec_I 
      use rotate_I 
      use rotatd_I 
      use elenuc_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nati 
      integer , intent(in) :: natx 
      real(double) , intent(out) :: enuclr 
      real(double) , intent(in) :: step 
      real(double)  :: coord(3,numat) 
      real(double) , intent(inout) :: h(*) 
      real(double) , intent(out) :: w(*) 
      real(double) , intent(out) :: ww(lm6,45) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(0:8) :: nb 
      integer :: icalcn, i, kr, ia, ib, ni, nband2, j, ja, jb, nj, i2, i1, ij, &
        kro, li, lj, nwjj, iad, ibd, jad, jbd, la, lb, ip, jp, ik, ii 
      real(double), dimension(45) :: e1b, de1b, e2a, de2a 
      real(double), dimension(9,9) :: di, ddi 
      real(double), dimension(101) :: wjd, dwjd 
      real(double), dimension(90,90) :: dwjdd, wjdd 
      real(double), dimension(171) :: en, den 
      real(double) :: cutoff, csave, enuc, denuc 

      save nb, cutoff, icalcn 
!-----------------------------------------------
!
!  DHCORE GENERATES THE 1-ELECTRON  AND 2-ELECTRON INTEGRALS DERIVATIVES
!         WITH RESPECT TO THE CARTESIAN COORDINATE COORD (NATX,NATI).
!
!  INPUT
!      COORD     : CARTESIAN  COORDINATES OF THE MOLECULE.
!      NATI,NATX : INDICES OF THE MOVING COORDINATE.
!      STEP      : STEP SIZE OF THE 2-POINTS FINITE DIFFERENCE.
!  OUTPUT
!      H         : 1-ELECTRON INTEGRALS DERIVATIVES (PACKED CANONICAL).
!      W         : 2-ELECTRON INTEGRALS DERIVATIVES (ORDERED AS REQUIRED
!                             IN DFOCK2 AND DIJKL1).
!      ENUCLR    : NUCLEAR ENERGY DERIVATIVE.
!
!-----------------------------------------------
      data nb/ 1, 0, 0, 10, 0, 0, 0, 0, 45/  
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        cutoff = 1.D10 
        icalcn = numcal 
      endif 
      h(:norbs*(norbs+1)/2) = 0 
      enuclr = 0.D0 
      kr = 1 
      i = nati 
      csave = coord(natx,nati) 
      ia = nfirst(nati) 
      ib = nlast(nati) 
      ni = nat(nati) 
      nband2 = 0 
      do j = 1, numat 
        if (j == nati) cycle  
        ja = nfirst(j) 
        jb = nlast(j) 
        nj = nat(j) 
        coord(natx,nati) = csave + step 
        call h1elec (ni, nj, coord(1,nati), coord(1,j), di) 
        coord(natx,nati) = csave - step 
        call h1elec (ni, nj, coord(1,nati), coord(1,j), ddi) 
!
!     FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX.
!
        i2 = 0 
        if (ia > ja) then 
          do i1 = ia, ib 
            ij = i1*(i1 - 1)/2 + ja - 1 
            i2 = i2 + 1 
            h(ij+1:jb-ja+1+ij) = h(ij+1:jb-ja+1+ij) + (di(i2,:jb-ja+1)-ddi(i2,:&
              jb-ja+1)) 
          end do 
        else 
          do i1 = ja, jb 
            ij = i1*(i1 - 1)/2 + ia - 1 
            i2 = i2 + 1 
            h(ij+1:ib-ia+1+ij) = h(ij+1:ib-ia+1+ij) + (di(:ib-ia+1,i2)-ddi(:ib-&
              ia+1,i2)) 
          end do 
        endif 
!
!     CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERM
!     E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
!
        kro = kr 
        nband2 = nband2 + nb(nlast(j)-nfirst(j)) 
        if (method_dorbs) then 
          coord(natx,nati) = csave + step 
          li = ib - ia + 1 
          lj = jb - ja + 1 
          nwjj = nw(j) - 1 
          iad = 1 
          ibd = li 
          jad = ibd + 1 
          jbd = jad + lj - 1 
          la = (li*(li + 1))/2 
          lb = (lj*(lj + 1))/2 
          ip = 1 + la 
          jp = 1 
!
!     Two-electron one and two center terms.
!
          call rotatd (ip, jp, iad, ibd, jad, jbd, ni, nj, coord(1,nati), coord&
            (1,j), wjdd, 90, enuc) 
!
!   Electron-nuclear attraction terms
!
          en = 0.D0 
          call elenuc (iad, ibd, jad, jbd, en) 
          ik = 0 
          do i = iad, ibd 
            if (i - iad + 1 > 0) then 
              e1b(ik+1:i-iad+1+ik) = en(i*(i-1)/2+iad:i+i*(i-1)/2) 
              ik = i - iad + 1 + ik 
            endif 
          end do 
          ik = 0 
          do i = jad, jbd 
            if (i - jad + 1 > 0) then 
              e2a(ik+1:i-jad+1+ik) = en(i*(i-1)/2+jad:i+i*(i-1)/2) 
              ik = i - jad + 1 + ik 
            endif 
          end do 
          coord(natx,nati) = csave + step*(-1.D0) 
          call rotatd (ip, jp, iad, ibd, jad, jbd, ni, nj, coord(1,nati), coord&
            (1,j), dwjdd, 90, denuc) 
!
!   Electron-nuclear attraction terms
!
          den = 0.D0 
          call elenuc (iad, ibd, jad, jbd, den) 
          ik = 0 
          do i = iad, ibd 
            if (i - iad + 1 > 0) then 
              de1b(ik+1:i-iad+1+ik) = den(i*(i-1)/2+iad:i+i*(i-1)/2) 
              ik = i - iad + 1 + ik 
            endif 
          end do 
          ik = 0 
          do i = jad, jbd 
            if (i - jad + 1 > 0) then 
              de2a(ik+1:i-jad+1+ik) = den(i*(i-1)/2+jad:i+i*(i-1)/2) 
              ik = i - jad + 1 + ik 
            endif 
          end do 
          ww(1+nwjj:lb+nwjj,:la) = transpose(wjdd(1+la:la*2,:lb)-dwjdd(1+la:la*&
            2,:lb)) 
        else 
          coord(natx,nati) = csave + step 
          call rotate (ni, nj, coord(1,nati), coord(1,j), wjd, kr, e1b, e2a, &
            enuc, cutoff) 
          kr = kro 
          coord(natx,nati) = csave + step*(-1.D0) 
          call rotate (ni, nj, coord(1,nati), coord(1,j), dwjd, kr, de1b, de2a&
            , denuc, cutoff) 
          if (kr > kro) then 
            wjd(:kr-kro+1) = wjd(:kr-kro+1) - dwjd(:kr-kro+1) 
            w(kro:kr) = wjd(:kr-kro+1) 
          endif 
        endif 
        coord(natx,nati) = csave 
        enuclr = enuclr + enuc - denuc 
!
!   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
!
        i2 = 0 
        do i1 = ia, ib 
          ii = i1*(i1 - 1)/2 + ia - 1 
          if (i1 - ia + 1 > 0) then 
            h(ii+1:i1-ia+1+ii) = h(ii+1:i1-ia+1+ii) + e1b(i2+1:i1-ia+1+i2) - &
              de1b(i2+1:i1-ia+1+i2) 
            i2 = i1 - ia + 1 + i2 
          endif 
        end do 
!
!   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
!
        i2 = 0 
        do i1 = ja, jb 
          ii = i1*(i1 - 1)/2 + ja - 1 
          if (i1 - ja + 1 > 0) then 
            h(ii+1:i1-ja+1+ii) = h(ii+1:i1-ja+1+ii) + e2a(i2+1:i1-ja+1+i2) - &
              de2a(i2+1:i1-ja+1+i2) 
            i2 = i1 - ja + 1 + i2 
          endif 
        end do 
      end do 
      return  
      end subroutine dhcore 
