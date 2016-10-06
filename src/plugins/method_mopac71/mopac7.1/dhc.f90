      subroutine dhc(p, pa, pb, xi, nat, if, il, jf, jl, dener, mode) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE overlaps_C, only : cutof1, cutof2
      use molkst_C, only : numcal, uhf, method_dorbs
      use euler_C, only : id
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:07  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use h1elec_I 
      use rotatd_I 
      use elenuc_I 
      use fockd2_I 
      use rotate_I 
      use fock2_I 
      use helect_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: if 
      integer , intent(in) :: il 
      integer , intent(in) :: jf 
      integer , intent(in) :: jl 
      integer , intent(in) :: mode 
      real(double) , intent(out) :: dener 
      integer , intent(in) :: nat(2) 
      real(double)  :: p(171) 
      real(double)  :: pa(171) 
      real(double)  :: pb(171) 
      real(double)  :: xi(3,2) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(2) :: nfirst, nlast 
      integer :: icalcn 
      integer , dimension(2) :: nw 
      integer :: lmw, linear, ia, ib, ic, ja, jb, jc, j, nj, ni, k, j1, jj, &
        i1, jp, ip, ii, kr, i2 
      real(double), dimension(8100) :: wjs, wks, w, wj, wk 
      real(double) :: wlim 
      real(double), dimension(171) :: h 
      real(double), dimension(9,9) :: shmat 
      real(double), dimension(171) :: f 
      real(double), dimension(10) :: e1b, e2a 
      real(double) :: rij, enuc, enuclr, ee 
      logical :: ignor1,  cutoff, ignor2

      save wjs, wks, w, wj, wk, ignor1, icalcn, wlim 
!-----------------------------------------------
!***********************************************************************
!
!  DHC CALCULATES THE ENERGY CONTRJBUTIONS FROM THOSE PAIRS OF ATOMS
!         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV.
!
!***********************************************************************
      data icalcn/ 0/  
      data wjs/ 8100*0.D0/  
      data wks/ 8100*0.D0/  
      data w/ 8100*0.D0/  
      data wj/ 8100*0.D0/  
      data wk/ 8100*0.D0/  
      data lmw/ 90/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
        ignor1 = .FALSE. 
        ignor2 = .FALSE. 
        wlim = 4.D0 
        if (id == 0) wlim = 0.D0 
      endif 
      if (mode==1 .and. id/=0) then 
        rij = (xi(1,1)-xi(1,2))**2 + (xi(2,1)-xi(2,2))**2 + (xi(3,1)-xi(3,2))**&
          2 
        ignor1 = rij > cutof1**2 
        ignor2 = rij > cutof2**2 
        if (ignor1 .and. ignor2) then 
          dener = 0.D0 
          return  
        endif 
      endif 
      if (mode==2 .and. id/=0 .and. ignor2 .and. ignor1) then 
        dener = 0.D0 
        return  
      endif 
      nfirst(1) = 1 
      nlast(1) = il - if + 1 
      nfirst(2) = nlast(1) + 1 
      nlast(2) = nfirst(2) + jl - jf 
      linear = (nlast(2)*(nlast(2)+1))/2 
      f(:linear) = 0.D0 
      h(:linear) = 0.0D00 
      ia = nfirst(2) 
      ib = nlast(2) 
      ic = nlast(2) 
      ja = nfirst(1) 
      jb = nlast(1) 
      jc = nlast(1) 
      j = 1 
      nj = nat(1) 
      ni = nat(2) 
      if (.not.ignor1) then 
!
!   Evaluate the one-electron term
!
        call h1elec (nj, ni, xi(1,1), xi(1,2), shmat) 
        if (nat(1)==102 .or. nat(2)==102) then 
          k = (ib*(ib + 1))/2 
          h(:k) = 0.D0 
        else 
          j1 = 0 
          do j = ia, ib 
            jj = j*(j - 1)/2 
            j1 = j1 + 1 
            i1 = 0 
            h(jj+1:jb-ja+1+jj) = shmat(:jb-ja+1,j1) 
            f(jj+1:jb-ja+1+jj) = shmat(:jb-ja+1,j1) 
          end do 
        endif 
      endif 
      if (.not.ignor2) then 
!
!   Evaluate the two-electron term
!
        if (method_dorbs) then 
!
!     Two-electron one and two center terms.
!
          jp = 1 
          ip = 1 + ((nlast(1)-nfirst(1)+1)*(nlast(1)-nfirst(1)+2))/2 
          call rotatd (ip, jp, ia, ib, ja, jb, ni, nj, xi(1,2), xi(1,1), w, lmw&
            , enuc) 
!
!   Electron-nuclear attraction terms
!
          call elenuc (ia, ib, ja, jb, h) 
          f = h 
!
!   Nuclear-nuclear repulsions
!
          enuclr = enuc 
          nw(1) = 1 
          nw(2) = ip 
          call fockd2 (f, p, pa, w, lmw, wj, wk, 2, nfirst, nlast, nw) 
        else 
          kr = 1 
          call rotate (ni, nj, xi(1,2), xi(1,1), w, kr, e2a, e1b, enuclr, &
            cutof2*1.1D0) 
          if (id /= 0) then 
            wj(:kr-1) = w(:kr-1) 
            wk(:kr-1) = w(:kr-1) 
            if (mode == 1) cutoff = wj(1) < wlim 
            if (cutoff) then 
              wk(:kr-1) = 0.D0 
            endif 
            wjs(:kr-1) = wj(:kr-1) 
            wks(:kr-1) = wk(:kr-1) 
          endif 
!
!    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS.
!
          i2 = 0 
          do i1 = ja, jc 
            ii = i1*(i1 - 1)/2 + ja - 1 
            if (i1 - ja + 1 > 0) then 
              h(ii+1:i1-ja+1+ii) = h(ii+1:i1-ja+1+ii) + e1b(i2+1:i1-ja+1+i2) 
              f(ii+1:i1-ja+1+ii) = f(ii+1:i1-ja+1+ii) + e1b(i2+1:i1-ja+1+i2) 
              i2 = i1 - ja + 1 + i2 
            endif 
          end do 
          do i1 = jc + 1, jb 
            ii = (i1*(i1 + 1))/2 
            f(ii) = f(ii) + e1b(1) 
            h(ii) = h(ii) + e1b(1) 
          end do 
          i2 = 0 
          do i1 = ia, ic 
            ii = i1*(i1 - 1)/2 + ia - 1 
            if (i1 - ia + 1 > 0) then 
              h(ii+1:i1-ia+1+ii) = h(ii+1:i1-ia+1+ii) + e2a(i2+1:i1-ia+1+i2) 
              f(ii+1:i1-ia+1+ii) = f(ii+1:i1-ia+1+ii) + e2a(i2+1:i1-ia+1+i2) 
              i2 = i1 - ia + 1 + i2 
            endif 
          end do 
          do i1 = ic + 1, ib 
            ii = (i1*(i1 + 1))/2 
            f(ii) = f(ii) + e2a(1) 
            h(ii) = h(ii) + e2a(1) 
          end do 
          call fock2 (f, p, pa, w, wjs, wks, 2, nfirst, nlast) 
        endif 
      else 
        enuclr = 0.D0 
      endif 
      ee = helect(nlast(2),pa,h,f) 
      if (uhf) then 
        f(:linear) = h(:linear) 
        if (method_dorbs) then 
          nw(1) = 1 
          nw(2) = ip 
          call fockd2 (f, p, pb, w, lmw, wj, wk, 2, nfirst, nlast, nw) 
        else 
          call fock2 (f, p, pb, w, wjs, wks, 2, nfirst, nlast) 
        endif 
        ee = ee + helect(nlast(2),pb,h,f) 
      else 
        ee = ee*2.D0 
      endif 
      dener = ee + enuclr 
      return  
!
      end subroutine dhc 
