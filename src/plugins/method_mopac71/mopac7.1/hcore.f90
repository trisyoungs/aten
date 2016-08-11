      subroutine hcore() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE molkst_C, only : numcal, numat, norbs, &
      & keywrd, enuclr, n2elec, efield
      use permanent_arrays, only : nfirst, nlast, nat, uspd, &
      & coord, h, w, wj, wk
      use cosmo_C, only : useps
      USE euler_C, only : tvec, id, l1l, l1u, l2l, l2u, l3l, l3u
      USE funcon_C, only : a0, ev 
      USE parameters_C, only : tore, dd
      use overlaps_C, only : cutof1, cutof2
      USE chanel_C, only : iw
     
!...Translated by Pacific-Sierra Research 77to90  4.4G  15:30:15  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use h1elec_I 
      use rotate_I 
      use solrot_I 
      use addhcr_I 
      use addnuc_I 
      use vecprt_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, ione 
      integer :: i, j, k, kr, ia, ib, ni, i1, i2, j1, io1, jo1, im1, &
        ja, jb, nj, ii, jj, kro 
      real(double), dimension(10) :: e1b, e2a 
      real(double), dimension(9,9) :: di 
      real(double), dimension(100) :: wjd, wkd 
      real(double), dimension(3) :: xj 
      real(double), dimension(9,9) :: dibits 
      real(double) ::  c2ref, c1ref, xf, yf, zf, const, fldcon, r, hterme, &
        fnuc, half, enuc 
      logical :: fldon, first, debug 
      character :: tmpkey*241 

      save first, debug, icalcn, ione
!***********************************************************************
!
!   HCORE GENERATES THE ONE-ELECTRON MATRIX AND TWO ELECTRON INTEGRALS
!         FOR A GIVEN MOLECULE WHOSE GEOMETRY IS DEFINED IN CARTESIAN
!         COORDINATES.
!
!  ON INPUT  COORD   = COORDINATES OF THE MOLECULE.
!
!  ON OUTPUT  H      = ONE-ELECTRON MATRIX.
!             W      = TWO-ELECTRON INTEGRALS.
!             ENUCLR = NUCLEAR ENERGY
!***********************************************************************
      data icalcn/ 0/  
      first = icalcn /= numcal 
      icalcn = numcal 
      if (first) then 
        ione = 1  
        cutof2 = 1.D10 
        cutof1 = 1.D10 
        c2ref = 0.D0 
        if (id == 1) then
          c2ref = 1000.D0
          if (allocated(wj)) deallocate (wj)
          if (allocated(wk)) deallocate (wk)
          allocate(wj(n2elec), wk(n2elec))
        end if
        l1l = 0
        l1u = 0
        l2l = 0
        l2u = 0
        l3l = 0
        l3u = 0
        if(id > 0) then
          l1l = -1
          l1u =  1
        end if
        if(id > 1) then
          l2l = -1
          l2u =  1
        end if
        if(id > 2) then
          l3l = -1
          l3u =  1
        end if
        i = index(keywrd,' CUTOFF2') 
        if (i /= 0) c2ref = reada(keywrd,i + 9)**2 
        if (index(keywrd,' CUTOFF1') /= 0) c1ref = reada(keywrd,index(keywrd,&
          ' CUTOFF1') + 9)**2 
        if (id /= 0) ione = 0 
        debug = index(keywrd,'HCORE') /= 0 
        xf = 0.D0 
        yf = 0.D0 
        zf = 0.D0 
        tmpkey = keywrd 
        i = index(tmpkey,' FIELD(') + index(tmpkey,' FIELD=(') 
        if (i /= 0) then 
!
!   ERASE ALL TEXT FROM TMPKEY EXCEPT FIELD DATA
!
          tmpkey(:i) = ' ' 
          tmpkey(index(tmpkey,')'):) = ' ' 
!
!   READ IN THE EFFECTIVE FIELD IN X,Y,Z COORDINATES
!
          xf = reada(tmpkey,i) 
          i = index(tmpkey,',') 
          if (i /= 0) then 
            tmpkey(i:i) = ' ' 
            yf = reada(tmpkey,i) 
            i = index(tmpkey,',') 
            if (i /= 0) then 
              tmpkey(i:i) = ' ' 
              zf = reada(tmpkey,i) 
            endif 
          endif 
          write (iw, &
      '(/10X,''THE ELECTRIC FIELD IS'',3F10.5,              '' VOLTS/ANGSTROM''&
      &,/)') xf, yf, zf 
        endif 
        const = a0/(8.D0*ev) 
!
        efield(1) = -xf*const 
        efield(2) = -yf*const 
        efield(3) = -zf*const 
      endif 
      fldon = .FALSE. 
      if (efield(1)/=0.0D00 .or. efield(2)/=0.0D00 .or. efield(3)/=0.0D00) then 
!
!   FLDCON = h/Ao
!
        fldcon = ev/a0 
        fldon = .TRUE. 
      endif 
      if (id /= 0) then 
        if (c2ref /= 0.D0) then 
          cutof2 = c2ref 
        else 
!
!   CALCULATE CUTOF2
!
          cutof2 = 1.D9 
          do i = l1l, l1u 
            do j = l2l, l2u 
              do k = l3l, l3u 
                r = (tvec(1,1)*i+tvec(1,2)*j+tvec(1,3)*k)**2 + (tvec(2,1)*i+&
                  tvec(2,2)*j+tvec(2,3)*k)**2 + (tvec(3,1)*i+tvec(3,2)*j+tvec(3&
                  ,3)*k)**2 
                if (r<=0.1D0 .or. r>=cutof2) cycle  
                cutof2 = r 
              end do 
            end do 
          end do 
          cutof2 = cutof2*0.2499D0 
        endif 
        if (c1ref /= 0.D0) then 
          cutof1 = c1ref 
        else 
          cutof1 = max(25.D0,min(cutof2*0.5D0,49.D0)) 
        endif 
        cutof2 = sqrt(cutof2) 
        if (first) then 
          write (iw, *) ' Cutoff for 2-electron integrals:', cutof2 
          write (iw, '(A,F12.6)') ' Overlap Cutoff Distance:', sqrt(cutof1) 
        endif 
      endif 
      enuclr = 0.d0
      h = 0.d0
      kr = 1 
      do i = 1, numat 
        ia = nfirst(i) 
        ib = nlast(i) 
        ni = nat(i) 
!
! FIRST WE FILL THE DIAGONALS, AND OFF-DIAGONALS ON THE SAME ATOM
!
        if (.not.fldon) then 
          do i1 = ia, ib 
            i2 = i1*(i1 - 1)/2 + ia - 1 
            if (i1 - ia + 1 > 0) then 
              h(i2+1:i1-ia+1+i2) = 0.D0 
              i2 = i1 - ia + 1 + i2 
            endif 
            h(i2) = uspd(i1) 
            cycle  
          end do 
        else 
          do i1 = ia, ib 
            i2 = i1*(i1 - 1)/2 + ia - 1 
            do j1 = ia, i1 
              i2 = i2 + 1 
              h(i2) = 0.D0 
              io1 = i1 - ia 
              jo1 = j1 - ia 
              if (jo1==0 .and. io1==1) then 
                hterme = -a0*dd(ni)*efield(1)*fldcon 
                h(i2) = hterme 
              endif 
              if (jo1==0 .and. io1==2) then 
                hterme = -a0*dd(ni)*efield(2)*fldcon 
                h(i2) = hterme 
              endif 
              if (jo1/=0 .or. io1/=3) cycle  
              hterme = -a0*dd(ni)*efield(3)*fldcon 
              h(i2) = hterme 
            end do 
            h(i2) = uspd(i1) 
            fnuc = -(efield(1)*coord(1,i)+efield(2)*coord(2,i)+efield(3)*coord(&
              3,i))*fldcon 
            h(i2) = h(i2) + fnuc 
          end do 
        endif 
        if (fldon) enuclr = enuclr - fnuc*tore(nat(i)) 
!
!   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX<PSI(LAMBDA)|PSI(SIGMA)>
!
        im1 = i - ione 
        do j = 1, im1 
          half = 1.D0 
          if (i == j) half = 0.5D0 
          ja = nfirst(j) 
          jb = nlast(j) 
          nj = nat(j) 
          if (id == 0) then 
            call h1elec (ni, nj, coord(1,i), coord(1,j), di) 
          else 
            di = 0.D0 
            do ii = l1l, l1u 
              do jj = l2l, l2u 
                do k = l3l, l3u 
                  xj = coord(:,j) + tvec(:,1)*ii + tvec(:,2)*jj + tvec(:,3)*k 
                  call h1elec (ni, nj, coord(1,i), xj, dibits) 
                  di = di + dibits 
                end do 
              end do 
            end do 
          endif 
          i2 = 0 
          do i1 = ia, ib 
            ii = i1*(i1 - 1)/2 + ja - 1 
            i2 = i2 + 1 
            jj = min(i1,jb) 
            h(ii+1:jj-ja+1+ii) = h(ii+1:jj-ja+1+ii) + di(i2,:jj-ja+1) 
          end do 
!
!   CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERMS
!   E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
!
          if (id == 0) then 
            call rotate (ni, nj, coord(1,i), coord(1,j), w(kr), kr, e1b, e2a, &
              enuc, cutof2) 
          else 
            kro = kr 
            call solrot (ni, nj, coord(1,i), coord(1,j), wjd, wkd, kr, e1b, e2a&
              , enuc, cutof2, i) 
            jj = 0 
            wj(kro:kr-1) = wjd(:kr-kro) 
            wk(kro:kr-1) = wkd(:kr-kro) 
          endif 
          enuclr = enuclr + enuc 
!
!   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
!
          i2 = 0 
          do i1 = ia, ib 
            ii = i1*(i1 - 1)/2 + ia - 1 
            if (i1 - ia + 1 > 0) then 
              h(ii+1:i1-ia+1+ii) = h(ii+1:i1-ia+1+ii) + e1b(i2+1:i1-ia+1+i2)*&
                half 
              i2 = i1 - ia + 1 + i2 
            endif 
          end do 
          do i1 = ib + 1, ib 
            ii = (i1*(i1 + 1))/2 
            h(ii) = h(ii) + e1b(1)*half 
          end do 
!
!   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
!
          i2 = 0 
          do i1 = ja, jb 
            ii = i1*(i1 - 1)/2 + ja - 1 
            if (i1 - ja + 1 > 0) then 
              h(ii+1:i1-ja+1+ii) = h(ii+1:i1-ja+1+ii) + e2a(i2+1:i1-ja+1+i2)*&
                half 
              i2 = i1 - ja + 1 + i2 
            endif 
          end do 
        end do 
      end do 
      if (useps) then 
! The following routine adds the dielectric correction for the electron-
! interaction to the diagonal elements of H
        call addhcr () 
! In the following routine the dielectric correction to the core-core-
! interaction is added to ENUCLR
        call addnuc () 
      endif 
! end of COSMO change
      if (debug) then 
        write (iw, '(2/10X,''ONE-ELECTRON MATRIX FROM HCORE'')') 
        call vecprt (h, norbs) 
        j = min(400,kr - 1) 
        j = kr - 1 
        if (id == 0) then 
          write (iw, '(2/10X,''TWO-ELECTRON MATRIX IN HCORE''/)') 
          write (iw, 200) (w(i),i=1,j) 
        else 
          write (iw, '(2/10X,''TWO-ELECTRON J MATRIX IN HCORE''/)') 
          write (iw, 200) (wj(i),i=1,j) 
          write (iw, '(2/10X,''TWO-ELECTRON K MATRIX IN HCORE''/)') 
          write (iw, 200) (wk(i),i=1,j) 
        endif 
  200   format(10f8.4) 
      endif 
      return  
      end subroutine hcore 
