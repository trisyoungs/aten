      subroutine addfck(p)
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE cosmo_C, only : nden, nps, nps2, abcmat      
      use molkst_C, only : numat, mpack
      use permanent_arrays, only : nfirst, nlast, f
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
      real(double), dimension (mpack), intent (in) :: p
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i0, iden, i, ia, idel, im, ic, id, jden, j, ja, jdel, jm, jc, &
        jd, kden, i1 
      real(double) :: fim 
!-----------------------------------------------
      i0 = nps2 + nden*nps 
      iden = 0 
      do i = 1, numat 
        ia = nfirst(i) 
        idel = nlast(i) - ia 
        im = (ia*(ia + 1))/2 - 1 
        do ic = 0, idel 
          do id = 0, ic 
            im = im + 1 
            iden = iden + 1 
            fim = 0.D0 
            jden = 0 
            do j = 1, numat 
              ja = nfirst(j) 
              jdel = nlast(j) - ja 
              jm = (ja*(ja + 1))/2 - 1 
              do jc = 0, jdel 
                do jd = 0, jc 
                  jm = jm + 1 
                  jden = jden + 1 
                  kden = max(iden,jden) 
                  i1 = (kden*(kden - 3))/2 + iden + jden + i0 
                  fim = fim + abcmat(i1)*p(jm) 
                end do 
                jm = jm + ja - 1 
              end do 
            end do 
            f(im) = f(im) + 2*fim 
          end do 
          f(im) = f(im) + 2*fim 
          im = im + ia - 1 
        end do 
      end do 
!#      WRITE(26,*)' F After ADDFCK'
!#      CALL VECPRT(F,NLAST(NUMAT))
      return  
      end subroutine addfck 


      subroutine addhcr
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : numat
      use permanent_arrays, only : nfirst, nlast, nat, h
      USE parameters_C, only : tore 
      USE cosmo_C, only : abcmat, nps2, nden, nps
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: iden, i0, i, ia, idel, id, im, ic,  jden, j, ja, jdel, kden, i1 
      real(double) :: him 
!-----------------------------------------------
      iden = 0 
      i0 = nps2 + nden*nps 
!#      WRITE(26,*)' H Before ADDHCR'
!#      CALL VECPRT(H,NORBS)
      do i = 1, numat 
        ia = nfirst(i) 
        idel = nlast(i) - ia 
        im = (ia*(ia + 1))/2 - 1 
        do ic = 0, idel 
          do id = 0, ic 
            im = im + 1 
            iden = iden + 1 
            him = 0.D0 
            jden = 1 
            do j = 1, numat 
              ja = nfirst(j) 
              jdel = nlast(j) - ja 
              kden = max(iden,jden) 
              i1 = (kden*(kden - 3))/2 + iden + jden + i0 
              him = him - abcmat(i1)*tore(nat(j)) 
              jden = jden + jdel**2 + 1 
            end do 
            h(im) = h(im) + him 
          end do 
          h(im) = h(im) + him 
          im = im + ia - 1 
        end do 
      end do 
!#      WRITE(26,*)' H After ADDHCR'
!#      CALL VECPRT(H,NORBS)
      return  
      end subroutine addhcr 


      subroutine addnuc 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use cosmo_C, only : abcmat, nps, nden, nps2
      use molkst_C, only : enuclr, numat
      use permanent_arrays, only : nfirst, nlast, nat
      use parameters_C, only : tore
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i0, iden, i, ia, idel, i1, j, ja, jdel 
      real(double) :: enclr, corei 
!-----------------------------------------------
      enclr = 0.D0 
      i0 = nps2 + nden*nps 
      iden = 0 
      do i = 1, numat 
        ia = nfirst(i) 
        idel = nlast(i) - ia 
        i1 = i0 + (iden*(iden + 1))/2 
        corei = tore(nat(i)) 
        do j = 1, i - 1 
          ja = nfirst(j) 
          jdel = nlast(j) - ja 
          i1 = i1 + 1 
          enclr = enclr + 2*corei*abcmat(i1)*tore(nat(j)) 
          i1 = i1 + jdel**2 
        end do 
        i1 = i1 + 1 
        enclr = enclr + corei*abcmat(i1)*corei 
        iden = iden + 1 + idel**2 
      end do 
      enuclr = enuclr + enclr 
      return  
      end subroutine addnuc 


      subroutine btoc
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE parameters_C, only : dd, qq 
      USE funcon_C, only : a0, ev
      use molkst_C, only : numat
      use cosmo_C, only : iatsp, nps, srad, rds, cosurf, fepsi, nps2, &
      & nden, bh, abcmat
      use permanent_arrays, only : nfirst, nlast, nat, coord
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, i0, iden, ia, idel, nati, ips, i1, k, kk2, l 
      real(double), dimension(3) :: xx, xa 
      real(double) ::  ri, ddi, qqi2, dist, rm1, rm3, rm5, fact, bhk, cij, &
        rm 
!-----------------------------------------------
      do i = 1, nps 
        j = iatsp(i) 
        ri = srad(j) - rds 
        cosurf(:,i) = cosurf(:,i)*ri + coord(:,j) 
      end do 
! FILLING B-MATRIX
      i0 = nps2 - nden 
      iden = 0 
      do i = 1, numat 
        ia = nfirst(i) 
        idel = nlast(i) - ia 
        idel = min(3,idel) 
        nati = nat(i) 
        ddi = dd(nati)*2*a0 
        qqi2 = (a0*qq(nati))**2 
        xx = coord(:,i) 
        do ips = 1, nps 
          i1 = i0 + ips*nden 
          xa = cosurf(:,ips) - xx 
          dist = sum(xa**2) 
          rm1 = 1.D0/dsqrt(dist) 
          abcmat(iden+1+i1) = rm1 
          if (idel == 0) cycle  
          rm3 = rm1**3 
          rm5 = rm1**5 
          abcmat(iden+3+i1) = rm1 + 3*xa(1)**2*qqi2*rm5 - qqi2*rm3 
          abcmat(iden+6+i1) = rm1 + 3*xa(2)**2*qqi2*rm5 - qqi2*rm3 
          abcmat(iden+10+i1) = rm1 + 3*xa(3)**2*qqi2*rm5 - qqi2*rm3 
          abcmat(iden+2+i1) = xa(1)*ddi*rm3 
          abcmat(iden+4+i1) = xa(2)*ddi*rm3 
          abcmat(iden+7+i1) = xa(3)*ddi*rm3 
          abcmat(iden+5+i1) = 6*xa(1)*xa(2)*qqi2*rm5 
          abcmat(iden+8+i1) = 6*xa(1)*xa(3)*qqi2*rm5 
          abcmat(iden+9+i1) = 6*xa(3)*xa(2)*qqi2*rm5 
        end do 
        iden = iden + 1 + idel**2 
      end do 
      i1 = nps2 + nden*nps 
!  FILLING C-MATRIX
      fact = -0.25D0*2*ev*a0*fepsi 
      do i = 1, nden 
        do k = 1, nps 
          bhk = 0.D0 
          kk2 = (k*(k - 1))/2 
          bhk = sum(abcmat(i+nden+i0:k*nden+i+i0:nden)*abcmat(kk2+1:k+kk2)) 
          do l = k + 1, nps 
            bhk = bhk + abcmat(i+l*nden+i0)*abcmat((l*(l-1))/2+k) 
          end do 
          bh(k) = bhk 
        end do 
        do j = 1, i 
          cij = 0.D0 
          cij = sum(bh(:nps)*abcmat(j+nden+i0:nps*nden+j+i0:nden)) 
          i1 = i1 + 1 
          abcmat(i1) = fact*cij 
        end do 
      end do 
      do i = 1, nps 
        j = iatsp(i) 
        rm = srad(j) - rds 
        cosurf(:,i) = (cosurf(:,i)-coord(:,j))/rm 
      end do 
      return  
      end subroutine btoc 


      subroutine ciint(c34, pq34) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use cosmo_C, only : nps2, nden, nps, abcmat
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(in) :: c34(*) 
      real(double) , intent(inout) :: pq34(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i0, i
!-----------------------------------------------
      i0 = nps2 + nden*nps 
      pq34(:nden) = 0.D0 
!
!  CIINT should not be used if NPS is negative
!
      if (i0 < 1) return  
      do i = 1, nden 
        if (i - 1 > 0) then 
          pq34(:i-1) = pq34(:i-1) + abcmat(i0+1:i-1+i0)*c34(i) 
          pq34(i) = pq34(i) + sum(abcmat(i0+1:i-1+i0)*c34(:i-1)) 
          i0 = i - 1 + i0 
        endif 
        i0 = i0 + 1 
        pq34(i) = pq34(i) + abcmat(i0)*c34(i) 
      end do 
      return  
      end subroutine ciint 


      subroutine consts
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use cosmo_C, only : nps, nps2, cosurf, nppa, tm, iatsp, &
      & nspa, srad, rds, dirtm, nar, area1, disex2, lenabc, nn, &
      & dirvec, abcmat, nset, xsp, area
      USE molkst_C, only : numat
      use permanent_arrays, only : nat, coord
      USE chanel_C, only : iw
! THIS ROUTINE CONSTRUCTS OR UPDATES THE SOLVENT-ACCESSIBLE
! SURFACE (SAS)
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      use dgetrf_I 
      use dgetri_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(lenabc) :: nsetf 
      integer , dimension(2) :: n0 
      integer , dimension(lenabc) :: ipiv 
      integer :: maxnps, nps3, i, inset, nps0, ips, j, nn1, nn2&
        , nn3, narea, k, i0, jmax, ipm, jps, nara, nari, nsetfi, j1, l, j2, &
        narj, nsetfj, ii 
      real(double), dimension(3) :: xx, xa, xi, xj 
      real(double) :: sdis, fdiag, ds, c2ds, r, ri, dist1, dist2, dist3, &
        dist, sp, sininv, sdis0, spm, x1, x2, x3, aa, rj, aij, y1, y2, y3 
      logical , dimension(nppa) :: din 
      logical :: isup 
!-----------------------------------------------
      isup = nps > 0 
      n0(1) = nps2 
      n0(2) = -nps 
      maxnps = lenabc
      if (isup) then 
        nps3 = lenabc - nps 
        do i = nps, 1, -1 
          iatsp(nps3+i) = iatsp(i) 
          cosurf(:,nps3+i) = cosurf(:,i) 
        end do 
        nps3 = nps3 + 1 
      endif 
      sdis = 0.D0 
      fdiag = 1.05D0*sqrt(nppa + 0.D0) 
      inset = 1 
      iatsp(lenabc+1) = 0 
      nps = 0 
      area = 0.D0 
      do i = 1, numat 
        ds = sqrt(4.D0/nspa) 
        if (nat(i) == 1) ds = 2*ds 
        c2ds = cos(2.D0*ds) 
        r = srad(i) 
        ri = r - rds 
        xa = coord(:,i) 
        nps0 = nps + 1 
        if (isup) then 
          if (nps >= nps3) then 
            write (iw, *) 'NPS .GT. NPS3 IN CONSTS IN COSMO' 
            call mopend ('NPS .GT. NPS3 IN CONSTS IN COSMO') 
            return  
          endif 
          nps2 = nps3 
          do ips = nps2, lenabc + 1 
            if (iatsp(ips) == i) cycle  
            exit  
          end do 
          nps3 = ips 
! TRANSFORM COSURF ACCORDING TO TM(INV)
          do j = nps2, nps3 - 1 
            xx(1) = cosurf(1,j) 
            xx(2) = cosurf(2,j) 
            xx(3) = cosurf(3,j) 
            cosurf(1,j) = xx(1)*tm(1,1,i) + xx(2)*tm(1,2,i) + xx(3)*tm(1,3,i) 
            cosurf(2,j) = xx(1)*tm(2,1,i) + xx(2)*tm(2,2,i) + xx(3)*tm(2,3,i) 
            cosurf(3,j) = xx(1)*tm(3,1,i) + xx(2)*tm(3,2,i) + xx(3)*tm(3,3,i) 
          end do 
          nn1 = nn(1,i) 
          nn2 = nn(2,i) 
          nn3 = nn(3,i) 
        else 
! SEARCH FOR 3 NEAREST NEIGHBOR ATOMS
          dist1 = 1.D20 
          dist2 = 1.D20 
          dist3 = 1.D20 
          nn1 = 0 
          nn2 = 0 
          nn3 = 0 
          do j = 1, numat 
            if (j == i) cycle  
            dist = sum((xa - coord(:,j))**2) 
            if (dist + 0.05D0 < dist3) then 
              dist3 = dist 
              nn3 = j 
            endif 
            if (dist3 + 0.05D0 < dist2) then 
              dist = dist2 
              dist2 = dist3 
              dist3 = dist 
              nn3 = nn2 
              nn2 = j 
            endif 
            if (dist2 + 0.05D0 >= dist1) cycle  
            dist = dist1 
            dist1 = dist2 
            dist2 = dist 
            nn2 = nn1 
            nn1 = j 
          end do 
          nn(1,i) = nn1 
          nn(2,i) = nn2 
          nn(3,i) = nn3 
        endif 
! BUILD NEW TRANSFORMATION MATRIX
        if (nn1 == 0) then 
          tm(1,1,i) = 1.D0 
          tm(1,2,i) = 0.D0 
          tm(1,3,i) = 0.D0 
        else 
          dist1 = sum((xa - coord(:,nn1))**2) 
          dist = 1.D0/sqrt(dist1) 
          tm(1,1,i) = (coord(1,nn1)-xa(1))*dist 
          tm(1,2,i) = (coord(2,nn1)-xa(2))*dist 
          tm(1,3,i) = (coord(3,nn1)-xa(3))*dist 
        endif 
   90   continue 
        if (nn2 == 0) then 
          tm(2,1,i) = -tm(1,2,i) 
          tm(2,2,i) = tm(1,1,i) 
          tm(2,3,i) = 0.D0 
        else 
          dist2 = sum((xa - coord(:,nn2))**2) 
          dist = 1.D0/sqrt(dist2) 
          xx(1) = (coord(1,nn2)-xa(1))*dist 
          xx(2) = (coord(2,nn2)-xa(2))*dist 
          xx(3) = (coord(3,nn2)-xa(3))*dist 
          sp = xx(1)*tm(1,1,i) + xx(2)*tm(1,2,i) + xx(3)*tm(1,3,i) 
          if (sp*sp > 0.99D0) then 
            nn2 = nn3 
            nn3 = 0 
            dist2 = dist3 
            go to 90 
          endif 
          sininv = 1.D0/sqrt(1.D0 - sp*sp) 
          tm(2,1,i) = (xx(1)-sp*tm(1,1,i))*sininv 
          tm(2,2,i) = (xx(2)-sp*tm(1,2,i))*sininv 
          tm(2,3,i) = (xx(3)-sp*tm(1,3,i))*sininv 
        endif 
        tm(3,1,i) = tm(1,2,i)*tm(2,3,i) - tm(2,2,i)*tm(1,3,i) 
        tm(3,2,i) = tm(1,3,i)*tm(2,1,i) - tm(2,3,i)*tm(1,1,i) 
        tm(3,3,i) = tm(1,1,i)*tm(2,2,i) - tm(2,1,i)*tm(1,2,i) 
! TRANSFORM DIRVEC ACCORDING TO TM
        do j = 1, nppa 
          xx(1) = dirvec(1,j) 
          xx(2) = dirvec(2,j) 
          xx(3) = dirvec(3,j) 
          dirtm(:,j) = xx(1)*tm(1,:,i) + xx(2)*tm(2,:,i) + xx(3)*tm(3,:,i) 
        end do 
! FIND THE POINTS OF THE BASIC GRID ON THE SAS
        narea = 0 
        l150: do j = 1, nppa 
          din(j) = .FALSE. 
          xx = xa + dirtm(:,j)*r 
          do k = 1, numat 
            if (k == i) cycle  
            dist = sum((xx - coord(:,k))**2) 
            dist = sqrt(dist) - srad(k) 
            if (dist >= 0) cycle  
            cycle  l150 
          end do 
          narea = narea + 1 
          din(j) = .TRUE. 
        end do l150 
        if (narea == 0) cycle  
        area = area + narea*ri*ri 
        if (isup) then 
          do j = nps2, nps3 - 1 
            nps = nps + 1 
            iatsp(nps) = i 
            xx(1) = cosurf(1,j) 
            xx(2) = cosurf(2,j) 
            xx(3) = cosurf(3,j) 
            cosurf(1,nps) = xx(1)*tm(1,1,i) + xx(2)*tm(2,1,i) + xx(3)*tm(3,1,i) 
            cosurf(2,nps) = xx(1)*tm(1,2,i) + xx(2)*tm(2,2,i) + xx(3)*tm(3,2,i) 
            cosurf(3,nps) = xx(1)*tm(1,3,i) + xx(2)*tm(2,3,i) + xx(3)*tm(3,3,i) 
          end do 
        else 
          i0 = 1 + 1/nat(i) 
          jmax = n0(i0) 
          i0 = 3*(i0 - 1)*nppa - 3 
          do j = 1, jmax 
            nps = nps + 1 
            if (nps > maxnps) then 
              write (iw, *) 'NPS IS GREATER THAN MAXNPS-USE SMALLER NSPA' 
              write (iw, '(A,I6)') 'NUMBER OF ATOMS IN SYSTEM: ', numat 
              write (iw, '(A,I6)') 'NUMBER OF ATOMS CALCULATED:', i 
              write (iw, *) 
              write (iw, *) &
                ' TRY INCREASING LENABC IN FILE SIZES AND RE-COMPLILE MOPAC.' 
              write (iw, *) ' NPS:', nps 
              write (iw, *) ' MAXNPS:', maxnps 
              call mopend ('NPS IS GREATER THAN MAXNPS-USE SMALLER NSPA') 
              return  
            endif 
            iatsp(nps) = i 
            xx(1) = abcmat(i0+j*3+1) 
            xx(2) = abcmat(i0+j*3+2) 
            xx(3) = abcmat(i0+j*3+3) 
            cosurf(1,nps) = xx(1)*tm(1,1,i) + xx(2)*tm(2,1,i) + xx(3)*tm(3,1,i) 
            cosurf(2,nps) = xx(1)*tm(1,2,i) + xx(2)*tm(2,2,i) + xx(3)*tm(3,2,i) 
            cosurf(3,nps) = xx(1)*tm(1,3,i) + xx(2)*tm(2,3,i) + xx(3)*tm(3,3,i) 
          end do 
        endif 
  180   continue 
        sdis0 = sdis 
        nar(nps0:nps) = 0 
        xsp(1,nps0:nps) = 0.D0 
        xsp(2,nps0:nps) = 0.D0 
        xsp(3,nps0:nps) = 0.D0 
        do j = 1, nppa 
          if (.not.din(j)) cycle  
          spm = -1.D0 
          x1 = dirtm(1,j) 
          x2 = dirtm(2,j) 
          x3 = dirtm(3,j) 
          do ips = nps0, nps 
            sp = x1*cosurf(1,ips) + x2*cosurf(2,ips) + x3*cosurf(3,ips) 
            if (sp < spm) cycle  
            spm = sp 
            ipm = ips 
          end do 
          if (spm < c2ds) then 
            nps = nps + 1 
            if (nps > maxnps) then 
              write (iw, *) 'NPS IS GREATER THAN MAXNPS-USE SMALLER NSPA' 
              write (iw, *) ' NPS:', nps 
              write (iw, *) ' MAXNPS:', maxnps 
              call mopend ('NPS IS GREATER THAN MAXNPS-USE SMALLER NSPA') 
              return  
            endif 
            cosurf(:,nps) = dirtm(:,j) 
            iatsp(nps) = i 
            go to 180 
          endif 
          nar(ipm) = nar(ipm) + 1 
          xsp(:,ipm) = xsp(:,ipm) + dirtm(:,j) 
        end do 
        sdis = 0.D0 
        ips = nps0 - 1 
        if (nps < ips) go to 180 
  240   continue 
        ips = ips + 1 
  250   continue 
        if (nar(ips) == 0) then 
          nps = nps - 1 
          if (nps < ips) go to 180 
          nar(ips:nps) = nar(ips+1:nps+1) 
          xsp(1,ips:nps) = xsp(1,ips+1:nps+1) 
          xsp(2,ips:nps) = xsp(2,ips+1:nps+1) 
          xsp(3,ips:nps) = xsp(3,ips+1:nps+1) 
          go to 250 
        endif 
        dist = dot_product(xsp(:,ips),xsp(:,ips)) 
        sdis = sdis + dist 
        dist = 1.D0/sqrt(dist) 
        cosurf(:,ips) = xsp(:,ips)*dist 
        if (ips < nps) go to 240 
        if (abs(sdis - sdis0) > 1.D-5) go to 180 
        do ips = nps0, nps 
          nsetf(ips) = inset 
          inset = inset + nar(ips) 
          nar(ips) = 0 
          xsp(:,ips) = xa + cosurf(:,ips)*ri 
        end do 
        do j = 1, nppa 
          if (.not.din(j)) cycle  
          spm = -1.D0 
          x1 = dirtm(1,j) 
          x2 = dirtm(2,j) 
          x3 = dirtm(3,j) 
          do ips = nps0, nps 
            sp = x1*cosurf(1,ips) + x2*cosurf(2,ips) + x3*cosurf(3,ips) 
            if (sp < spm) cycle  
            spm = sp 
            ipm = ips 
          end do 
          if (spm < c2ds) cycle  
          nara = nar(ipm) 
          nset(nsetf(ipm)+nara) = j 
          nar(ipm) = nara + 1 
        end do 
      end do 
      if (area /= 0.D0) area1 = area*4.D0*3.14159D0/nppa 
! FILLING AAMAT
      do ips = 1, nps 
        i = iatsp(ips) 
        ri = srad(i) - rds 
        nari = nar(ips) 
        nsetfi = nsetf(ips) 
        aa = 0.D0 
        do k = nsetfi, nsetfi + nari - 1 
          j1 = nset(k) 
          aa = aa + fdiag 
          x1 = dirvec(1,j1) 
          x2 = dirvec(2,j1) 
          x3 = dirvec(3,j1) 
          do l = nsetfi, k - 1 
            j2 = nset(l) 
            aa = aa + 2.D0/sqrt((x1 - dirvec(1,j2))**2+(x2-dirvec(2,j2))**2+(x3&
              -dirvec(3,j2))**2) 
          end do 
        end do 
        aa = aa/ri/nari**2 
        abcmat(ips+(ips-1)*nps) = aa 
        xi = coord(:,i) 
        xa = xsp(:,ips) 
        do jps = ips + 1, nps 
          narj = nar(jps) 
          nsetfj = nsetf(jps) 
          j = iatsp(jps) 
          xj = coord(:,j) - xi 
          dist = sum((xsp(:,jps)-xa)**2) 
          if (dist < disex2) then 
            rj = srad(j) - rds 
            aij = 0.D0 
            do k = nsetfi, nsetfi + nari - 1 
              j1 = nset(k) 
              xx = dirvec(:,j1)*ri 
              if (i /= j) then 
                x1 = xx(1)*tm(1,1,i) + xx(2)*tm(2,1,i) + xx(3)*tm(3,1,i) - xj(1&
                  ) 
                x2 = xx(1)*tm(1,2,i) + xx(2)*tm(2,2,i) + xx(3)*tm(3,2,i) - xj(2&
                  ) 
                x3 = xx(1)*tm(1,3,i) + xx(2)*tm(2,3,i) + xx(3)*tm(3,3,i) - xj(3&
                  ) 
                do l = nsetfj, nsetfj + narj - 1 
                  j2 = nset(l) 
                  xx = dirvec(:,j2)*rj 
                  y1 = xx(1)*tm(1,1,j) + xx(2)*tm(2,1,j) + xx(3)*tm(3,1,j) - x1 
                  y2 = xx(1)*tm(1,2,j) + xx(2)*tm(2,2,j) + xx(3)*tm(3,2,j) - x2 
                  y3 = xx(1)*tm(1,3,j) + xx(2)*tm(2,3,j) + xx(3)*tm(3,3,j) - x3 
                  aij = aij + 1.D0/sqrt(y1*y1 + y2*y2 + y3*y3) 
                end do 
              else 
!                  AA=((DIRVEC(1,J2)*RJ-XX(1))**2+(DIRVEC(2,J2)*RJ
!     &                   -XX(2))**2+(DIRVEC(3,J2)*RJ-XX(3))**2)
                aij = aij + sum(((dirvec(1,nset(nsetfj:narj-1+nsetfj))*rj-xx(1)&
                  )**2+(dirvec(2,nset(nsetfj:narj-1+nsetfj))*rj-xx(2))**2+(&
                  dirvec(3,nset(nsetfj:narj-1+nsetfj))*rj-xx(3))**2)**(-.5D0)) 
              endif 
            end do 
            aij = aij/nari/narj 
          else 
            aij = 1.D0/sqrt(dist) 
          endif 
          abcmat(ips+(jps-1)*nps) = aij 
          abcmat(jps+(ips-1)*nps) = aij 
        end do 
      end do 
! INVERT A-MATRIX
      call dgetrf (nps, nps, abcmat, nps, ipiv) 
      call dgetri (nps, abcmat, nps, ipiv, xsp, 3*lenabc) 
!  STORE INV. A-MATRIX AS LOWER TRIANGLE
      ii = 0 
      do i = 1, nps 
        do j = 1, i 
          ii = ii + 1 
          abcmat(ii) = abcmat(j+(i-1)*nps) 
        end do 
      end do 
      nps2 = ii 
      return  
      end subroutine consts 


      subroutine cqden
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      use permanent_arrays, only : p, nfirst, nlast, nat
      use molkst_C, only : numat
      use parameters_C, only : tore
      use cosmo_C, only : qden
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: iden, i, ia, idel, im, ic
!-----------------------------------------------
      iden = 0 
      do i = 1, numat 
        ia = nfirst(i) 
        idel = nlast(i) - ia 
        idel = min(3,idel) 
        im = (ia*(ia + 1))/2 
        iden = iden + 1 
        qden(iden) = tore(nat(i)) - p(im) 
        do ic = 1, idel 
          im = im + ia - 1 
          if (ic + 1 > 0) then 
            qden(iden+1:ic+1+iden) = -p(im+1:ic+1+im) 
            im = ic + 1 + im 
            iden = ic + 1 + iden 
          endif 
        end do 
      end do 
      return  
      end subroutine cqden 


      subroutine dgetf2(m, n, a, lda, ipiv) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use idamax_I 
      use dger_I 
      use dscal_I 
      use dswap_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: m 
      integer  :: n 
      integer  :: lda 
      integer , intent(out) :: ipiv(*) 
      real(double)  :: a(lda,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: one = 1.0D+0 
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j, jp 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC min 
!-----------------------------------------------
!
!  -- LAPACK ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     JUNE 30, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DGETF2 COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A
!  USING PARTIAL PIVOTING WITH ROW INTERCHANGES.
!
!  THE FACTORIZATION HAS THE FORM
!     A = P * L * U
!  WHERE P IS A PERMUTATION MATRIX, L IS LOWER TRIANGULAR WITH UNIT
!  DIAGONAL ELEMENTS (LOWER TRAPEZOIDAL IF M > N), AND U IS UPPER
!  TRIANGULAR (UPPER TRAPEZOIDAL IF M < N).
!
!  THIS IS THE RIGHT-LOOKING LEVEL 2 BLAS VERSION OF THE ALGORITHM.
!
!  ARGUMENTS
!  =========
!
!  M       (INPUT) INTEGER
!          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
!
!  N       (INPUT) INTEGER
!          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
!
!  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
!          ON ENTRY, THE M BY N MATRIX TO BE FACTORED.
!          ON EXIT, THE FACTORS L AND U FROM THE FACTORIZATION
!          A = P*L*U; THE UNIT DIAGONAL ELEMENTS OF L ARE NOT STORED.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
!
!  IPIV    (OUTPUT) INTEGER ARRAY, DIMENSION (MIN(M,N))
!          THE PIVOT INDICES; FOR 1 <= I <= MIN(M,N), ROW I OF THE
!          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
!
!  =====================================================================
!
!     .. PARAMETERS ..
!     ..
!     .. LOCAL SCALARS ..
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXTERNAL SUBROUTINES ..
!     ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     QUICK RETURN IF POSSIBLE
!
      if (m==0 .or. n==0) return  
!
      do j = 1, min(m,n) 
!
!        FIND PIVOT AND TEST FOR SINGULARITY.
!
        jp = j - 1 + idamax(m - j + 1,a(j,j),1) 
        ipiv(j) = jp 
        if (a(jp,j) /= zero) then 
!
!           APPLY THE INTERCHANGE TO COLUMNS 1:N.
!
          if (jp /= j) call dswap (n, a(j,1), lda, a(jp,1), lda) 
!
!           COMPUTE ELEMENTS J+1:M OF J-TH COLUMN.
!
          if (j < m) call dscal (m - j, one/a(j,j), a(j+1,j), 1) 
        endif 
!
        if (j >= min(m,n)) cycle  
!
!           UPDATE TRAILING SUBMATRIX.
!
        call dger (m - j, n - j, (-one), a(j+1,j), 1, a(j,j+1), lda, a(j+1,j+1)&
          , lda) 
      end do 
      return  
!
!     END OF DGETF2
!
      end subroutine dgetf2 


      subroutine dgetrf(m, n, a, lda, ipiv) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dgemm_I 
      use dgetf2_I 
      use dlaswp_I 
      use dtrsm_I 
      use ilaenv_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: m 
      integer  :: n 
      integer  :: lda 
      integer  :: ipiv(*) 
      real(double)  :: a(lda,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: one = 1.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j, jb, nb 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC min 
!-----------------------------------------------
!
!  -- LAPACK ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DGETRF COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A
!  USING PARTIAL PIVOTING WITH ROW INTERCHANGES.
!
!  THE FACTORIZATION HAS THE FORM
!     A = P * L * U
!  WHERE P IS A PERMUTATION MATRIX, L IS LOWER TRIANGULAR WITH UNIT
!  DIAGONAL ELEMENTS (LOWER TRAPEZOIDAL IF M > N), AND U IS UPPER
!  TRIANGULAR (UPPER TRAPEZOIDAL IF M < N).
!
!  THIS IS THE RIGHT-LOOKING LEVEL 3 BLAS VERSION OF THE ALGORITHM.
!
!  ARGUMENTS
!  =========
!
!  M       (INPUT) INTEGER
!          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
!
!  N       (INPUT) INTEGER
!          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
!
!  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
!          ON ENTRY, THE M BY N MATRIX TO BE FACTORED.
!          ON EXIT, THE FACTORS L AND U FROM THE FACTORIZATION
!          A = P*L*U; THE UNIT DIAGONAL ELEMENTS OF L ARE NOT STORED.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
!
!  IPIV    (OUTPUT) INTEGER ARRAY, DIMENSION (MIN(M,N))
!          THE PIVOT INDICES; FOR 1 <= I <= MIN(M,N), ROW I OF THE
!          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
!
!  =====================================================================
!
!     .. PARAMETERS ..
!     ..
!     .. LOCAL SCALARS ..
!     ..
!     .. EXTERNAL SUBROUTINES ..
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
!
!     QUICK RETURN IF POSSIBLE
!
      if (m==0 .or. n==0) return  
!
!     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
!
      nb = ilaenv(1,'DGETRF',m,n,-1) 
      if (nb<=1 .or. nb>=min(m,n)) then 
!
!        USE UNBLOCKED CODE.
!
        call dgetf2 (m, n, a, lda, ipiv) 
      else 
!
!        USE BLOCKED CODE.
!
        do j = 1, min(m,n), nb 
          jb = min(min(m,n) - j + 1,nb) 
!
!           FACTOR DIAGONAL AND SUBDIAGONAL BLOCKS AND TEST FOR EXACT
!           SINGULARITY.
!
          call dgetf2 (m - j + 1, jb, a(j,j), lda, ipiv(j)) 
!
!           ADJUST THE PIVOT INDICES.
!
          ipiv(j:min(m,j+jb-1)) = j - 1 + ipiv(j:min(m,j+jb-1)) 
!
!           APPLY INTERCHANGES TO COLUMNS 1:J-1.
!
          call dlaswp (j - 1, a, lda, j, j + jb - 1, ipiv, 1) 
!
          if (j + jb > n) cycle  
!
!              APPLY INTERCHANGES TO COLUMNS J+JB:N.
!
          call dlaswp (n - j - jb + 1, a(1,j+jb), lda, j, j + jb - 1, ipiv, 1) 
!
!              COMPUTE BLOCK ROW OF U.
!
          call dtrsm ('LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', jb, n - j - jb&
             + 1, one, a(j,j), lda, a(j,j+jb), lda) 
          if (j + jb > m) cycle  
!
!                 UPDATE TRAILING SUBMATRIX.
!
          call dgemm ('NO TRANSPOSE', 'NO TRANSPOSE', m - j - jb + 1, n - j - &
            jb + 1, jb, (-one), a(j+jb,j), lda, a(j,j+jb), lda, one, a(j+jb,j+&
            jb), lda) 
        end do 
      endif 
      return  
!
!     END OF DGETRF
!
      end subroutine dgetrf 


      subroutine dgetri(n, a, lda, ipiv, work, lwork) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use ilaenv_I 
      use dgemm_I 
      use dgemv_I 
      use dswap_I 
      use dtrsm_I 
      use dtrtri_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: n 
      integer  :: lda 
      integer , intent(in) :: lwork 
      integer , intent(in) :: ipiv(*) 
      real(double)  :: a(lda,*) 
      real(double)  :: work(lwork) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+0 
      real(double), parameter :: one = 1.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: iws, j, jb, jj, jp, ldwork, nb, nbmin, nn 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max, min 
!-----------------------------------------------
!
!  -- LAPACK ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     JUNE 30, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DGETRI COMPUTES THE INVERSE OF A MATRIX USING THE LU FACTORIZATION
!  COMPUTED BY DGETRF.
!
!  THIS METHOD INVERTS U AND THEN COMPUTES INV(A) BY SOLVING THE SYSTEM
!  INV(A)*L = INV(U) FOR INV(A).
!
!  ARGUMENTS
!  =========
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX A.  N >= 0.
!
!  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
!          ON ENTRY, THE FACTORS L AND U FROM THE FACTORIZATION
!          A = P*L*U AS COMPUTED BY DGETRF.
!          ON EXIT, IF INFO = 0, THE INVERSE OF THE ORIGINAL MATRIX A.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
!
!  IPIV    (INPUT) INTEGER ARRAY, DIMENSION (N)
!          THE PIVOT INDICES FROM DGETRF; FOR 1<=I<=N, ROW I OF THE
!          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
!
!  WORK    (WORKSPACE) DOUBLE PRECISION ARRAY, DIMENSION (LWORK)
!          IF INFO RETURNS 0, THEN WORK(1) RETURNS N*NB, THE MINIMUM
!          VALUE OF LWORK REQUIRED TO USE THE OPTIMAL BLOCKSIZE.
!
!  LWORK   (INPUT) INTEGER
!          THE DIMENSION OF THE ARRAY WORK.  LWORK >= MAX(1,N).
!          FOR OPTIMAL PERFORMANCE LWORK SHOULD BE AT LEAST N*NB,
!          WHERE NB IS THE OPTIMAL BLOCKSIZE RETURNED BY ILAENV.
!
!  =====================================================================
!
!     .. PARAMETERS ..
!     ..
!     .. LOCAL SCALARS ..
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXTERNAL SUBROUTINES ..
!     ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     QUICK RETURN IF POSSIBLE
!
      if (n == 0) return  
!
!     FORM INV(U).  IF INFO > 0 FROM DTRTRI, THEN U IS SINGULAR,
!     AND THE INVERSE IS NOT COMPUTED.
!
      call dtrtri ('UPPER', 'NON-UNIT', n, a, lda)   
!
!     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
!
      nb = ilaenv(1,'DGETRI',n,-1,-1) 
      nbmin = 2 
      ldwork = n 
      if (nb>1 .and. nb<n) then 
        iws = max(ldwork*nb,1) 
        if (lwork < iws) then 
          nb = lwork/ldwork 
          nbmin = max(2,ilaenv(2,'DGETRI',n,-1,-1)) 
        endif 
      else 
        iws = n 
      endif 
!
!     SOLVE THE EQUATION INV(A)*L = INV(U) FOR INV(A).
!
      if (nb<nbmin .or. nb>=n) then 
!
!        USE UNBLOCKED CODE.
!
        do j = n, 1, -1 
!
!           COPY CURRENT COLUMN OF L TO WORK AND REPLACE WITH ZEROS.
!
          work(j+1:n) = a(j+1:n,j) 
          a(j+1:n,j) = zero 
          if (j >= n) cycle  
!
!           COMPUTE CURRENT COLUMN OF INV(A).
!
          call dgemv ('NO TRANSPOSE', n, n - j, (-one), a(1,j+1), lda, work(j+1&
            ), 1, one, a(1,j), 1) 
        end do 
      else 
!
!        USE BLOCKED CODE.
!
        nn = ((n - 1)/nb)*nb + 1 
        do j = nn, 1, -nb 
          jb = min(nb,n - j + 1) 
!
!           COPY CURRENT BLOCK COLUMN OF L TO WORK AND REPLACE WITH
!           ZEROS.
!
          do jj = j, j + jb - 1 
            work(jj+1+(jj-j)*ldwork:n+(jj-j)*ldwork) = a(jj+1:n,jj) 
            a(jj+1:n,jj) = zero 
          end do 
!
!           COMPUTE CURRENT BLOCK COLUMN OF INV(A).
!
          if (j + jb <= n) call dgemm ('NO TRANSPOSE', 'NO TRANSPOSE', n, jb, n&
             - j - jb + 1, (-one), a(1,j+jb), lda, work(j+jb), ldwork, one, a(1&
            ,j), lda) 
          call dtrsm ('RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n, jb, one, &
            work(j), ldwork, a(1,j), lda) 
        end do 
      endif 
!
!     APPLY COLUMN INTERCHANGES.
!
      do j = n - 1, 1, -1 
        jp = ipiv(j) 
        if (jp == j) cycle  
        call dswap (n, a(1,j), 1, a(1,jp), 1) 
      end do 
!
      work(1) = iws 
      return  
!
!     END OF DGETRI
!
      end subroutine dgetri 


      subroutine diegrd
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : coord,  nfirst, nlast, nat
      use molkst_C, only : numat
      use cosmo_C, only : abcmat, nps, iatsp, srad, rds, cosurf, &
      & fepsi, bh, nps2, nden, qden
      use funcon_C, only : a0, ev, fpc_9
      use parameters_C, only : dd, qq
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use cqden_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, ix, i0, i2, i1, k, iak, l, ial, iden, idel, nati 
      real(double), dimension(0:3,10) :: db 
      real(double), dimension(3) :: xk, xl, xx 
      real(double) :: ri, fact,  posi, qsk, dist2, xxx, ff, ddi, qqi2, ff0, &
        rm2, rm4, dx, rm 
      real(double) :: dxyz(3,3)
!-----------------------------------------------
      do i = 1, nps 
        j = iatsp(i) 
        ri = srad(j) - rds 
        cosurf(:,i) = cosurf(:,i)*ri + coord(:,j) 
      end do 
      db(1:3,:) = 0.D0 
      db(0,1) = 1.D0 
      call cqden () 
      fact = -ev*a0*fepsi*fpc_9 
      bh(:nps) = 0.D0 
      i0 = nps2 - nden 
      do i = 1, nps 
        i2 = (i*(i - 1))/2 
        i1 = i0 + i*nden 
        posi = 0.D0 
        posi = sum(qden(:nden)*abcmat(1+i1:nden+i1)) 
        bh(:i) = bh(:i) + posi*abcmat(1+i2:i+i2) 
        do k = i + 1, nps 
          bh(k) = bh(k) + posi*abcmat(i+(k*(k-1))/2) 
        end do 
      end do 
      do k = 1, nps 
        iak = iatsp(k) 
        xk = cosurf(:,k) 
        qsk = bh(k) 
        do l = 1, k - 1 
          ial = iatsp(l) 
          if (ial == iak) cycle  
          dist2 = 0.D0 
          do ix = 1, 3 
            xxx = cosurf(ix,l) - xk(ix) 
            xl(ix) = xxx 
            dist2 = dist2 + xxx*xxx 
          end do 
          ff = qsk*bh(l)*fact*dist2**(-1.5D0) 
          dxyz(:,iak) = dxyz(:,iak) - xl*ff 
          dxyz(:,ial) = dxyz(:,ial) + xl*ff 
        end do 
      end do 
      do k = 1, nps 
        iak = iatsp(k) 
        xk = cosurf(:,k) 
        qsk = bh(k) 
        iden = 0 
        do i = 1, numat 
          idel = nlast(i) - nfirst(i) 
          idel = min(idel,3) 
          if (i /= iak) then 
            nati = nat(i) 
            dist2 = 0.D0 
            do ix = 1, 3 
              xxx = xk(ix) - coord(ix,i) 
              xx(ix) = xxx 
              dist2 = dist2 + xxx*xxx 
            end do 
            ddi = dd(nati)*2*a0 
            qqi2 = (a0*qq(nati))**2 
            ff0 = -qsk*fact*dist2**(-1.5D0) 
            if (idel /= 0) then 
              rm2 = 1.D0/dist2 
              rm4 = rm2**2 
              db(0,2) = ddi*3*xx(1)*rm2 
              db(0,4) = ddi*3*xx(2)*rm2 
              db(0,7) = ddi*3*xx(3)*rm2 
              db(0,3) = 1.D0 + qqi2*(15*xx(1)**2*rm2-3.D0)*rm2 
              db(0,6) = 1.D0 + qqi2*(15*xx(2)**2*rm2-3.D0)*rm2 
              db(0,10) = 1.D0 + qqi2*(15*xx(3)**2*rm2-3.D0)*rm2 
              db(0,5) = qqi2*30*xx(1)*xx(2)*rm4 
              db(0,8) = qqi2*30*xx(1)*xx(3)*rm4 
              db(0,9) = qqi2*30*xx(3)*xx(2)*rm4 
              db(1,2) = ddi 
              db(2,4) = db(1,2) 
              db(3,7) = db(1,2) 
              db(1,3) = 6*qqi2*xx(1)*rm2 
              db(2,6) = 6*qqi2*xx(2)*rm2 
              db(3,10) = 6*qqi2*xx(3)*rm2 
              db(1,5) = db(2,6) 
              db(2,5) = db(1,3) 
              db(1,8) = db(3,10) 
              db(3,8) = db(1,3) 
              db(2,9) = db(3,10) 
              db(3,9) = db(2,6) 
            endif 
            do j = 1, 1 + idel*idel 
              ff = ff0*qden(iden+j) 
              do ix = 1, 3 
                dx = (xx(ix)*db(0,j)-db(ix,j))*ff 
                dxyz(ix,iak) = dxyz(ix,iak) + dx 
                dxyz(ix,i) = dxyz(ix,i) - dx 
              end do 
            end do 
          endif 
          iden = iden + 1 + idel**2 
        end do 
      end do 
      do i = 1, nps 
        j = iatsp(i) 
        rm = srad(j) - rds 
        cosurf(:,i) = (cosurf(:,i)-coord(:,j))/rm 
      end do 
      return  
      end subroutine diegrd 


      subroutine dielen(edie) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use cosmo_C, only : nps2, nden, nps, qden, abcmat
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use cqden_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(out) :: edie 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i0, i 
      real(double) :: qi 
!-----------------------------------------------
      call cqden () 
      edie = 0.D0 
      i0 = nps2 + nden*nps 
      do i = 1, nden 
        qi = qden(i) 
        if (i - 1 > 0) then 
          edie = edie + sum(2*qi*abcmat(i0+1:i-1+i0)*qden(:i-1)) 
          i0 = i - 1 + i0 
        endif 
        i0 = i0 + 1 
        edie = edie + qi*abcmat(i0)*qi 
      end do 
      return  
      end subroutine dielen 


      subroutine dlaswp(n, a, lda, k1, k2, ipiv, incx) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dswap_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: n 
      integer  :: lda 
      integer , intent(in) :: k1 
      integer , intent(in) :: k2 
      integer , intent(in) :: incx 
      integer , intent(in) :: ipiv(*) 
      real(double)  :: a(lda,*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, ip, ix 
!-----------------------------------------------
!
!  -- LAPACK AUXILIARY ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     OCTOBER 31, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DLASWP PERFORMS A SERIES OF ROW INTERCHANGES ON THE MATRIX A.
!  ONE ROW INTERCHANGE IS INITIATED FOR EACH OF ROWS K1 THROUGH K2 OF A.
!
!  ARGUMENTS
!  =========
!
!  N       (INPUT) INTEGER
!          THE NUMBER OF COLUMNS OF THE MATRIX A.
!
!  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
!          ON ENTRY, THE MATRIX OF COLUMN DIMENSION N TO WHICH THE ROW
!          INTERCHANGES WILL BE APPLIED.
!          ON EXIT, THE PERMUTED MATRIX.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.
!
!  K1      (INPUT) INTEGER
!          THE FIRST ELEMENT OF IPIV FOR WHICH A ROW INTERCHANGE WILL
!          BE DONE.
!
!  K2      (INPUT) INTEGER
!          THE LAST ELEMENT OF IPIV FOR WHICH A ROW INTERCHANGE WILL
!          BE DONE.
!
!  IPIV    (INPUT) INTEGER ARRAY, DIMENSION (M*ABS(INCX))
!          THE VECTOR OF PIVOT INDICES.  ONLY THE ELEMENTS IN POSITIONS
!          K1 THROUGH K2 OF IPIV ARE ACCESSED.
!          IPIV(K) = L IMPLIES ROWS K AND L ARE TO BE INTERCHANGED.
!
!  INCX    (INPUT) INTEGER
!          THE INCREMENT BETWEEN SUCCESSIVE VALUES OF IPIV.  IF IPIV
!          IS NEGATIVE, THE PIVOTS ARE APPLIED IN REVERSE ORDER.
!
! =====================================================================
!
!     .. LOCAL SCALARS ..
!     ..
!     .. EXTERNAL SUBROUTINES ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     INTERCHANGE ROW I WITH ROW IPIV(I) FOR EACH OF ROWS K1 THROUGH K2.
!
      if (incx == 0) return  
      if (incx > 0) then 
        ix = k1 
      else 
        ix = 1 + (1 - k2)*incx 
      endif 
      select case (incx)  
      case (1)  
        do i = k1, k2 
          ip = ipiv(i) 
          if (ip == i) cycle  
          call dswap (n, a(i,1), lda, a(ip,1), lda) 
        end do 
      case (2:)  
        do i = k1, k2 
          ip = ipiv(ix) 
          if (ip /= i) call dswap (n, a(i,1), lda, a(ip,1), lda) 
          ix = ix + incx 
        end do 
      case (:(-1))  
        do i = k2, k1, -1 
          ip = ipiv(ix) 
          if (ip /= i) call dswap (n, a(i,1), lda, a(ip,1), lda) 
          ix = ix + incx 
        end do 
      end select 
!
      return  
!
!     END OF DLASWP
!
      end subroutine dlaswp 


      subroutine dmecip(coeffs, deltap, delta, cdiag, cif2, eig, vectci&
        , conf) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use cosmo_C, only : nps2, nden, qden, abcmat, nps
      USE meci_C, only : occa, microa, microb, nelec, nmos, lab, nalmat, nstate
      use permanent_arrays, only : nfirst, nlast
      use molkst_c, only : norbs, numat
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mxm_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(in) :: cif2 
      real(double)  :: coeffs(norbs,norbs) 
      real(double)  :: deltap(nmos,nmos) 
      real(double)  :: delta(norbs,nmos) 
      real(double) , intent(in) :: cdiag(*) 
      real(double) , intent(inout) :: eig(*) 
      real(double) , intent(inout) :: vectci(*) 
      real(double) , intent(in) :: conf(*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ist, ii, j, i, id, jd, ix, iy, ij, k, iden, iat, i0 
      real(double) :: sum, edie, qi, cdiagi 
!-----------------------------------------------
!***********************************************************************
!
!   DMECIP WILL CALCULATE THE COSMO CORRECTION TO ALL C.I.- STATES
!
!              ON INPUT
!
!  COEFFS       : ALL M.O.'S (NORBS M.O.S)
!  NORBS        : NUMBER OF MOLECULAR ORBITALS = NUMBER OF A.O.'S
!  P            : TOTAL DENSITY MATRIX
!  NMOS         : NUMBER OF M.O.'S IN ACTIVE SPACE
!  VECTCI       : STATE VECTOR OF LENGTH LAB
!  MICROA(I,J)  : ALPHA OCCUPANCY OF M.O. 'I' IN MICROSTATE 'J'
!  MICROB(I,J)  : BETA  OCCUPANCY OF M.O. 'I' IN MICROSTATE 'J'
!
!  NOTE: THIS IS A MODIFICATION OF CODE ORIGINALLY WRITTEN BY
!        PROF. DANIEL LIOTARD
!***********************************************************************
      do ist = 1, lab 
        ii = lab*(ist - 1) 
        vectci(:lab) = conf(ii+1:lab+ii) 
!     INITIALIZE WITH THE OPPOSITE OF THE 'SCF' DENSITY.
        do i = 1, nmos 
          deltap(i,i) = -occa(i)*2.D0 
          deltap(i,:i-1) = 0.D0 
        end do 
!
!     ADD THE C.I. CORRECTION
        do id = 1, lab 
          do jd = 1, id 
!     CHECK SPIN AGREEMENT
            if (nalmat(id) /= nalmat(jd)) cycle  
            ix = 0 
            iy = 0 
            do j = 1, nmos 
              ix = ix + abs(microa(j,id)-microa(j,jd)) 
              iy = iy + abs(microb(j,id)-microb(j,jd)) 
            end do 
!     CHECK NUMBER OF DIFFERING M.O.
            if (ix + iy > 2) cycle  
            if (ix == 2) then 
!        DETERMINANTS ID AND JD DIFFER BY M.O I IN ID AND M.O J IN JD:
              do i = 1, nmos 
                if (microa(i,id) == microa(i,jd)) cycle  
                exit  
              end do 
              ij = microb(i,id) 
              do j = i + 1, nmos 
                if (microa(j,id) /= microa(j,jd)) exit  
                ij = ij + microa(j,id) + microb(j,id) 
              end do 
!        IJ GIVES THE SIGN OF THE PERMUTATION
              sum = 0.D0 
              do k = 1, nstate 
                sum = sum + vectci(id+(k-1)*lab)*vectci(jd+(k-1)*lab) 
              end do 
              deltap(j,i) = deltap(j,i) + sum*dble(1 - 2*mod(ij,2))/nstate 
            else if (iy == 2) then 
!        DETERMINANTS ID AND JD DIFFER BY M.O J IN ID AND M.O I IN JD:
              do i = 1, nmos 
                if (microb(i,id) == microb(i,jd)) cycle  
                exit  
              end do 
              ij = 0 
              do j = i + 1, nmos 
                if (microb(j,id) /= microb(j,jd)) exit  
                ij = ij + microa(j,id) + microb(j,id) 
              end do 
              ij = ij + microa(j,id) 
              sum = 0.D0 
              do k = 1, nstate 
                sum = sum + vectci(id+(k-1)*lab)*vectci(jd+(k-1)*lab) 
              end do 
              deltap(j,i) = deltap(j,i) + sum*dble(1 - 2*mod(ij,2))/nstate 
            else 
!        DETERMINANTS ID AND JD ARE IDENTICAL:
              sum = 0.D0 
              do k = 1, nstate 
                sum = sum + vectci(id+(k-1)*lab)**2 
              end do 
              do i = 1, nmos 
                deltap(i,i) = deltap(i,i) + (microa(i,id)+microb(i,id))*sum/&
                  nstate 
              end do 
            endif 
          end do 
        end do 
!
!     BACK TRANSFORM INTO A.O. BASIS.
!     -------------------------------
!     P(C.I.) = P(SCF) + C * DELTAP * C'
        do i = 1, nmos 
          deltap(:i-1,i) = deltap(i,:i-1) 
        end do 
!     STEP 1: DELTAP = C * DELTAP
!      DO 162 J=1,NMOS
!         DO 161 I=1,NORBS
! 161     DELTA(I,J)=0.D0
!         DO 162 K=1,NMOS
!            DO 162 I=1,NORBS
! 162   DELTA(I,J)=DELTA(I,J)+COEFFS(I,NELEC+K)*DELTAP(K,J)
        call mxm (coeffs(1,nelec+1), norbs, deltap, nmos, delta, nmos) 
!     STEP 2: P = P + DELTAP * C'
        iden = 0 
        do iat = 1, numat 
          do i = nfirst(iat), nlast(iat) 
            do j = nfirst(iat), i 
              sum = 0.D0 
              do k = 1, nmos 
                sum = sum + delta(i,k)*coeffs(j,nelec+k) 
              end do 
              iden = iden + 1 
              qden(iden) = sum 
            end do 
          end do 
        end do 
        edie = 0.D0 
        i0 = nps2 + nden*nps 
        do i = 1, nden 
          qi = qden(i) 
          do j = 1, i - 1 
            i0 = i0 + 1 
            edie = edie + 2*qi*abcmat(i0)*qden(j) 
          end do 
          i0 = i0 + 1 
          edie = edie + qi*abcmat(i0)*qi 
        end do 
        edie = cif2*edie 
        cdiagi = 0.D0 
        do j = 1, lab 
          cdiagi = cdiagi + vectci(j)**2*cdiag(j) 
        end do 
        eig(ist) = eig(ist) + edie - cdiagi 
      end do 
      vectci(:lab) = conf(:lab) 
      return  
      end subroutine dmecip 


      subroutine dtrti2(uplo, diag, n, a, lda, info) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use dscal_I 
      use dtrmv_I 
      use xerbla_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      integer  :: lda 
      integer , intent(out) :: info 
      character  :: uplo 
      character  :: diag 
      real(double)  :: a(lda,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: one = 1.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j 
      real(double) :: ajj 
      logical :: nounit, upper 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC max 
!-----------------------------------------------
!
!  -- LAPACK ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DTRTI2 COMPUTES THE INVERSE OF A REAL UPPER OR LOWER TRIANGULAR
!  MATRIX.
!
!  THIS IS THE LEVEL 2 BLAS VERSION OF THE ALGORITHM.
!
!  ARGUMENTS
!  =========
!
!  UPLO    (INPUT) CHARACTER*1
!          SPECIFIES WHETHER THE MATRIX A IS UPPER OR LOWER TRIANGULAR.
!          = 'U':  UPPER TRIANGULAR
!          = 'L':  LOWER TRIANGULAR
!
!  DIAG    (INPUT) CHARACTER*1
!          SPECIFIES WHETHER OR NOT THE MATRIX A IS UNIT TRIANGULAR.
!          = 'N':  NON-UNIT TRIANGULAR
!          = 'U':  UNIT TRIANGULAR
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX A.  N >= 0.
!
!  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
!          ON ENTRY, THE TRIANGULAR MATRIX A.  IF UPLO = 'U', THE
!          LEADING N BY N UPPER TRIANGULAR PART OF THE ARRAY A CONTAINS
!          THE UPPER TRIANGULAR MATRIX, AND THE STRICTLY LOWER
!          TRIANGULAR PART OF A IS NOT REFERENCED.  IF UPLO = 'L', THE
!          LEADING N BY N LOWER TRIANGULAR PART OF THE ARRAY A CONTAINS
!          THE LOWER TRIANGULAR MATRIX, AND THE STRICTLY UPPER
!          TRIANGULAR PART OF A IS NOT REFERENCED.  IF DIAG = 'U', THE
!          DIAGONAL ELEMENTS OF A ARE ALSO NOT REFERENCED AND ARE
!          ASSUMED TO BE 1.
!
!          ON EXIT, THE (TRIANGULAR) INVERSE OF THE ORIGINAL MATRIX, IN
!          THE SAME STORAGE FORMAT.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
!
!  =====================================================================
!
!     .. PARAMETERS ..
!     ..
!     .. LOCAL SCALARS ..
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXTERNAL SUBROUTINES ..
!     ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      info = 0 
      upper = lsame(uplo,'U') 
      nounit = lsame(diag,'N') 
      if (.not.upper .and. .not.lsame(uplo,'L')) then 
        info = -1 
      else if (.not.nounit .and. .not.lsame(diag,'U')) then 
        info = -2 
      else if (n < 0) then 
        info = -3 
      else if (lda < max(1,n)) then 
        info = -5 
      endif 
      if (info /= 0) then 
        call xerbla ('DTRTI2', (-info)) 
        return  
      endif 
!
      if (upper) then 
!
!        COMPUTE INVERSE OF UPPER TRIANGULAR MATRIX.
!
        do j = 1, n 
          if (nounit) then 
            a(j,j) = one/a(j,j) 
            ajj = -a(j,j) 
          else 
            ajj = -one 
          endif 
!
!           COMPUTE ELEMENTS 1:J-1 OF J-TH COLUMN.
!
          call dtrmv ('UPPER', 'NO TRANSPOSE', diag, j - 1, a, lda, a(1,j), 1) 
          call dscal (j - 1, ajj, a(1,j), 1) 
        end do 
      else 
!
!        COMPUTE INVERSE OF LOWER TRIANGULAR MATRIX.
!
        do j = n, 1, -1 
          if (nounit) then 
            a(j,j) = one/a(j,j) 
            ajj = -a(j,j) 
          else 
            ajj = -one 
          endif 
          if (j >= n) cycle  
!
!              COMPUTE ELEMENTS J+1:N OF J-TH COLUMN.
!
          call dtrmv ('LOWER', 'NO TRANSPOSE', diag, n - j, a(j+1,j+1), lda, a(&
            j+1,j), 1) 
          call dscal (n - j, ajj, a(j+1,j), 1) 
        end do 
      endif 
!
      return  
!
!     END OF DTRTI2
!
      end subroutine dtrti2 


      subroutine dtrtri(uplo, diag, n, a, lda) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use lsame_I 
      use ilaenv_I 
      use dtrmm_I 
      use dtrsm_I 
      use dtrti2_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: n 
      integer  :: lda 
      character  :: uplo 
      character  :: diag 
      real(double)  :: a(lda,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: one = 1.0D+0 
      real(double), parameter :: zero = 0.0D+0 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j, jb, nb, nn, info
      logical :: nounit, upper 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC min 
!-----------------------------------------------
!
!  -- LAPACK ROUTINE (VERSION 1.0B) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 29, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!     .. ARRAY ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  DTRTRI COMPUTES THE INVERSE OF A REAL UPPER OR LOWER TRIANGULAR
!  MATRIX A.
!
!  THIS IS THE LEVEL 3 BLAS VERSION OF THE ALGORITHM.
!
!  ARGUMENTS
!  =========
!
!  UPLO    (INPUT) CHARACTER*1
!          SPECIFIES WHETHER THE MATRIX A IS UPPER OR LOWER TRIANGULAR.
!          = 'U':  UPPER TRIANGULAR
!          = 'L':  LOWER TRIANGULAR
!
!  DIAG    (INPUT) CHARACTER*1
!          SPECIFIES WHETHER OR NOT THE MATRIX A IS UNIT TRIANGULAR.
!          = 'N':  NON-UNIT TRIANGULAR
!          = 'U':  UNIT TRIANGULAR
!
!  N       (INPUT) INTEGER
!          THE ORDER OF THE MATRIX A.  N >= 0.
!
!  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
!
!          ON ENTRY, THE TRIANGULAR MATRIX A.  IF UPLO = 'U', THE
!          LEADING N BY N UPPER TRIANGULAR PART OF THE ARRAY A CONTAINS
!          THE UPPER TRIANGULAR MATRIX, AND THE STRICTLY LOWER
!          TRIANGULAR PART OF A IS NOT REFERENCED.  IF UPLO = 'L', THE
!          LEADING N BY N LOWER TRIANGULAR PART OF THE ARRAY A CONTAINS
!          THE LOWER TRIANGULAR MATRIX, AND THE STRICTLY UPPER
!          TRIANGULAR PART OF A IS NOT REFERENCED.  IF DIAG = 'U', THE
!          DIAGONAL ELEMENTS OF A ARE ALSO NOT REFERENCED AND ARE
!          ASSUMED TO BE 1.
!
!          ON EXIT, THE (TRIANGULAR) INVERSE OF THE ORIGINAL MATRIX, IN
!          THE SAME STORAGE FORMAT.
!
!  LDA     (INPUT) INTEGER
!          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
!
!  INFO    (OUTPUT) INTEGER
!          = 0: SUCCESSFUL EXIT
!          > 0: IF INFO = K, A(K,K) IS EXACTLY ZERO.  THE TRIANGULAR
!               MATRIX IS SINGULAR AND ITS INVERSE CAN NOT BE COMPUTED.
!          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
!
!  =====================================================================
!
!     .. PARAMETERS ..
!     ..
!     .. LOCAL SCALARS ..
!     ..
!     .. EXTERNAL FUNCTIONS ..
!     ..
!     .. EXTERNAL SUBROUTINES ..
!     ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
!     TEST THE INPUT PARAMETERS.
!
      upper = lsame(uplo,'U') 
      nounit = lsame(diag,'N') 
!
!     QUICK RETURN IF POSSIBLE
!
      if (n == 0) return  
!
!     CHECK FOR SINGULARITY IF NON-UNIT.
!
      if (nounit) then 
        do info = 1, n 
          if (a(info,info) /= zero) cycle  
          return  
        end do 
        info = 0 
      endif 
!
!     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
!
      nb = ilaenv(1,'DTRTRI',n,-1,-1) 
      if (nb<=1 .or. nb>=n) then 
!
!        USE UNBLOCKED CODE
!
        call dtrti2 (uplo, diag, n, a, lda, info) 
      else 
!
!        USE BLOCKED CODE
!
        if (upper) then 
!
!           COMPUTE INVERSE OF UPPER TRIANGULAR MATRIX
!
          do j = 1, n, nb 
            jb = min(nb,n - j + 1) 
!
!              COMPUTE ROWS 1:J-1 OF CURRENT BLOCK COLUMN
!
            call dtrmm ('LEFT', 'UPPER', 'NO TRANSPOSE', diag, j - 1, jb, one, &
              a, lda, a(1,j), lda) 
            call dtrsm ('RIGHT', 'UPPER', 'NO TRANSPOSE', diag, j - 1, jb, (-&
              one), a(j,j), lda, a(1,j), lda) 
!
!              COMPUTE INVERSE OF CURRENT DIAGONAL BLOCK
!
            call dtrti2 ('UPPER', diag, jb, a(j,j), lda, info) 
          end do 
        else 
!
!           COMPUTE INVERSE OF LOWER TRIANGULAR MATRIX
!
          nn = ((n - 1)/nb)*nb + 1 
          do j = nn, 1, -nb 
            jb = min(nb,n - j + 1) 
            if (j + jb <= n) then 
!
!                 COMPUTE ROWS J+JB:N OF CURRENT BLOCK COLUMN
!
              call dtrmm ('LEFT', 'LOWER', 'NO TRANSPOSE', diag, n - j - jb + 1&
                , jb, one, a(j+jb,j+jb), lda, a(j+jb,j), lda) 
              call dtrsm ('RIGHT', 'LOWER', 'NO TRANSPOSE', diag, n - j - jb + &
                1, jb, (-one), a(j,j), lda, a(j+jb,j), lda) 
            endif 
!
!              COMPUTE INVERSE OF CURRENT DIAGONAL BLOCK
!
            call dtrti2 ('LOWER', diag, jb, a(j,j), lda, info) 
          end do 
        endif 
      endif 
!
      return  
!
!     END OF DTRTRI
!
      end subroutine dtrtri 


      subroutine dvfill(nppa, dirvec) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use chanel_C, only : iw
!     FUELLEN DES FELDES DIRVEC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nppa 
      real(double) , intent(inout) :: dirvec(3,*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(3,20) :: fset 
      integer , dimension(2,30) :: kset 
      integer :: nd, i, j, m, k, l, kh, na, nb, nc, j1, j2 
      real(double) :: r, h, beta, t, dist 
!-----------------------------------------------
      data kset/ 1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 12, 11, 12, 10, 12, 9, 12, 8, 12&
        , 7, 2, 3, 3, 4, 4, 5, 5, 6, 6, 2, 7, 8, 8, 9, 9, 10, 10, 11, 11, 7, 2&
        , 7, 7, 3, 3, 8, 8, 4, 4, 9, 9, 5, 5, 10, 10, 6, 6, 11, 11, 2/  
      data fset/ 1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 12, 11, 10, 12, &
        10, 9, 12, 9, 8, 12, 8, 7, 12, 7, 11, 2, 3, 7, 3, 4, 8, 4, 5, 9, 5, 6, &
        10, 6, 2, 11, 7, 8, 3, 8, 9, 4, 9, 10, 5, 10, 11, 6, 11, 7, 2/  
      dirvec(1,1) = -1.D0 
      dirvec(2,1) = 0.D0 
      dirvec(3,1) = 0.D0 
      nd = 1 
      r = sqrt(0.8D0) 
      h = sqrt(0.2D0) 
      do i = -1, 1, 2 
        do j = 1, 5 
          nd = nd + 1 
          beta = 1.D0 + j*1.25663706D0 + (i + 1)*0.3141593D0 
          dirvec(2,nd) = r*cos(beta) 
          dirvec(3,nd) = r*sin(beta) 
          dirvec(1,nd) = i*h 
        end do 
      end do 
      dirvec(2,12) = 0.D0 
      dirvec(3,12) = 0.D0 
      dirvec(1,12) = 1.D0 
      nd = 12 
!  NPPA=10*3**K*4**L+2
      m = (nppa - 2)/10 
      do k = 0, 10 
        if ((m/3)*3 /= m) exit  
        m = m/3 
      end do 
      do l = 0, 10 
        if ((m/4)*4 /= m) exit  
        m = m/4 
      end do 
      if (10*3**k*4**l + 2 /= nppa) then 
        write (iw, *) 'VALUE OF NPPA NOT ALLOWED: IT MUST BE 10*3**K*4**L+2' 
        call mopend ('VALUE OF NPPA NOT ALLOWED: IT MUST BE 10*3**K*4**L+2') 
        return  
      endif 
      kh = k/2 
      m = 2**l*3**kh 
! CREATE ON EACH EDGE 2**L*3**KH-1 NEW POINTS
      do i = 1, 30 
        na = kset(1,i) 
        nb = kset(2,i) 
        do j = 1, m - 1 
          nd = nd + 1 
          dirvec(:,nd) = dirvec(:,na)*(m - j) + dirvec(:,nb)*j 
        end do 
      end do 
! CREATE POINTS WITHIN EACH TRIANGLE
      do i = 1, 20 
        na = fset(1,i) 
        nb = fset(2,i) 
        nc = fset(3,i) 
        do j1 = 1, m - 1 
          do j2 = 1, m - j1 - 1 
            nd = nd + 1 
            dirvec(:,nd) = dirvec(:,na)*(m - j1 - j2) + dirvec(:,nb)*j1 + &
              dirvec(:,nc)*j2 
          end do 
        end do 
      end do 
      if (k /= 2*kh) then 
! CREATE TO ADDITIONAL SUBGRIDS
        t = 1.D0/3 
        do i = 1, 20 
          na = fset(1,i) 
          nb = fset(2,i) 
          nc = fset(3,i) 
          do j1 = 0, m - 1 
            do j2 = 0, m - j1 - 1 
              nd = nd + 1 
              dirvec(:,nd) = dirvec(:,na)*(m - j1 - j2 - 2*t) + dirvec(:,nb)*(&
                j1 + t) + dirvec(:,nc)*(j2 + t) 
            end do 
          end do 
        end do 
        t = 2.D0/3 
        do i = 1, 20 
          na = fset(1,i) 
          nb = fset(2,i) 
          nc = fset(3,i) 
          do j1 = 0, m - 2 
            do j2 = 0, m - j1 - 2 
              nd = nd + 1 
              dirvec(:,nd) = dirvec(:,na)*(m - j1 - j2 - 2*t) + dirvec(:,nb)*(&
                j1 + t) + dirvec(:,nc)*(j2 + t) 
            end do 
          end do 
        end do 
      endif 
! NORMALIZE ALL VECTORS
      do i = 1, nppa 
        dist = sum(dirvec(:,i)**2) 
        dist = 1.D0/sqrt(dist) 
        dirvec(:,i) = dirvec(:,i)*dist 
      end do 
      return  
      end subroutine dvfill 


      integer function ilaenv (ispec, name, n1, n2, n4) 
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ispec 
      integer , intent(in) :: n1 
      integer , intent(in) :: n2 
      integer , intent(in) :: n4 
      character , intent(in) :: name*(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, ic, iz, nb, nbmin, nx 
      logical :: cname, sname 
      character :: c1, c2*2, c4*2, c3*3, subnam*6 
!-----------------------------------------------
!   I n t r i n s i c  F u n c t i o n s
!-----------------------------------------------
      INTRINSIC char, ichar, int, min, real 
!-----------------------------------------------
!
!  -- LAPACK AUXILIARY ROUTINE (PRELIMINARY VERSION) --
!     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
!     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
!     FEBRUARY 20, 1992
!
!     .. SCALAR ARGUMENTS ..
!     ..
!
!  PURPOSE
!  =======
!
!  ILAENV IS CALLED FROM THE LAPACK ROUTINES TO CHOOSE PROBLEM-DEPENDENT
!  PARAMETERS FOR THE LOCAL ENVIRONMENT.  SEE ISPEC FOR A DESCRIPTION OF
!  THE PARAMETERS.
!
!  THIS VERSION PROVIDES A SET OF PARAMETERS WHICH SHOULD GIVE GOOD,
!  BUT NOT OPTIMAL, PERFORMANCE ON MANY OF THE CURRENTLY AVAILABLE
!  COMPUTERS.  USERS ARE ENCOURAGED TO MODIFY THIS SUBROUTINE TO SET
!  THE TUNING PARAMETERS FOR THEIR PARTICULAR MACHINE USING THE OPTION
!  AND PROBLEM SIZE INFORMATION IN THE ARGUMENTS.
!
!  THIS ROUTINE WILL NOT FUNCTION CORRECTLY IF IT IS CONVERTED TO ALL
!  LOWER CASE.  CONVERTING IT TO ALL UPPER CASE IS ALLOWED.
!
!  ARGUMENTS
!  =========
!
!  ISPEC   (INPUT) INTEGER
!          SPECIFIES THE PARAMETER TO BE RETURNED AS THE VALUE OF
!          ILAENV.
!          = 1: THE OPTIMAL BLOCKSIZE; IF THIS VALUE IS 1, AN UNBLOCKED
!               ALGORITHM WILL GIVE THE BEST PERFORMANCE.
!          = 2: THE MINIMUM BLOCK SIZE FOR WHICH THE BLOCK ROUTINE
!               SHOULD BE USED; IF THE USABLE BLOCK SIZE IS LESS THAN
!               THIS VALUE, AN UNBLOCKED ROUTINE SHOULD BE USED.
!          = 3: THE CROSSOVER POINT (IN A BLOCK ROUTINE, FOR N LESS
!               THAN THIS VALUE, AN UNBLOCKED ROUTINE SHOULD BE USED)
!          = 4: THE NUMBER OF SHIFTS, USED IN THE NONSYMMETRIC
!               EIGENVALUE ROUTINES
!          = 5: THE MINIMUM COLUMN DIMENSION FOR BLOCKING TO BE USED;
!               RECTANGULAR BLOCKS MUST HAVE DIMENSION AT LEAST K BY M,
!               WHERE K IS GIVEN BY ILAENV(2,...) AND M BY ILAENV(5,...)
!          = 6: THE CROSSOVER POINT FOR THE SVD (WHEN REDUCING AN M BY N
!               MATRIX TO BIDIAGONAL FORM, IF MAX(M,N)/MIN(M,N) EXCEEDS
!               THIS VALUE, A QR FACTORIZATION IS USED FIRST TO REDUCE
!               THE MATRIX TO A TRIANGULAR FORM.)
!          = 7: THE NUMBER OF PROCESSORS
!          = 8: THE CROSSOVER POINT FOR THE MULTISHIFT QR AND QZ METHODS
!               FOR NONSYMMETRIC EIGENVALUE PROBLEMS.
!
!  NAME    (INPUT) CHARACTER*(*)
!          THE NAME OF THE CALLING SUBROUTINE, IN EITHER UPPER CASE OR
!          LOWER CASE.
!
!          THE CHARACTER OPTIONS TO THE SUBROUTINE NAME, CONCATENATED
!          INTO A SINGLE CHARACTER STRING.  FOR EXAMPLE, UPLO = 'U',
!          TRANS = 'T', AND DIAG = 'N' FOR A TRIANGULAR ROUTINE WOULD
!          BE SPECIFIED AS OPTS = 'UTN'.
!
!  N1      (INPUT) INTEGER
!  N2      (INPUT) INTEGER
!  N4      (INPUT) INTEGER
!          PROBLEM DIMENSIONS FOR THE SUBROUTINE NAME; THESE MAY NOT ALL
!          BE REQUIRED.
!
! (ILAENV) (OUTPUT) INTEGER
!          >= 0: THE VALUE OF THE PARAMETER SPECIFIED BY ISPEC
!          < 0:  IF ILAENV = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE.
!
!  FURTHER DETAILS
!  ===============
!
!  THE FOLLOWING CONVENTIONS HAVE BEEN USED WHEN CALLING ILAENV FROM THE
!  LAPACK ROUTINES:
!  1)  OPTS IS A CONCATENATION OF ALL OF THE CHARACTER OPTIONS TO
!      SUBROUTINE NAME, IN THE SAME ORDER THAT THEY APPEAR IN THE
!      ARGUMENT LIST FOR NAME, EVEN IF THEY ARE NOT USED IN DETERMINING
!      THE VALUE OF THE PARAMETER SPECIFIED BY ISPEC.
!  2)  THE PROBLEM DIMENSIONS N1, N2, N4 ARE SPECIFIED IN THE ORDER
!      THAT THEY APPEAR IN THE ARGUMENT LIST FOR NAME.  N1 IS USED
!      FIRST, N2 SECOND, AND SO ON, AND UNUSED PROBLEM DIMENSIONS ARE
!      PASSED A VALUE OF -1.
!  3)  THE PARAMETER VALUE RETURNED BY ILAENV IS CHECKED FOR VALIDITY IN
!      THE CALLING SUBROUTINE.  FOR EXAMPLE, ILAENV IS USED TO RETRIEVE
!      THE OPTIMAL BLOCKSIZE FOR STRTRI AS FOLLOWS:
!
!      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!      IF( NB.LE.1 ) NB = MAX( 1, N )
!
!  =====================================================================
!
!     .. LOCAL SCALARS ..
!     ..
!     .. INTRINSIC FUNCTIONS ..
!     ..
!     .. EXECUTABLE STATEMENTS ..
!
      select case (ispec)  
!
!     INVALID VALUE FOR ISPEC
!
      case default 
        ilaenv = -1 
        return  
!
      case (1:3)  
        ilaenv = 1 
        subnam = name 
        ic = ichar(subnam(1:1)) 
        iz = ichar('Z') 
        if (iz==90 .or. iz==122) then 
!
!        ASCII CHARACTER SET
!
          if (ic>=97 .and. ic<=122) then 
            subnam(1:1) = char(ic - 32) 
            do i = 2, 6 
              ic = ichar(subnam(i:i)) 
              if (ic<97 .or. ic>122) cycle  
              subnam(i:i) = char(ic - 32) 
            end do 
          endif 
!
        else if (iz==233 .or. iz==169) then 
!
!        EBCDIC CHARACTER SET
!
          if (ic>=129 .and. ic<=137 .or. ic>=145 .and. ic<=153 .or. ic>=162&
             .and. ic<=169) then 
            subnam(1:1) = char(ic + 64) 
            do i = 2, 6 
              ic = ichar(subnam(i:i)) 
              if (.not.(ic>=129 .and. ic<=137 .or. ic>=145 .and. ic<=153 .or. &
                ic>=162 .and. ic<=169)) cycle  
              subnam(i:i) = char(ic + 64) 
            end do 
          endif 
!
        else if (iz==218 .or. iz==250) then 
!
!        PRIME MACHINES:  ASCII+128
!
          if (ic>=225 .and. ic<=250) then 
            subnam(1:1) = char(ic - 32) 
            do i = 2, 6 
              ic = ichar(subnam(i:i)) 
              if (ic<225 .or. ic>250) cycle  
              subnam(i:i) = char(ic - 32) 
            end do 
          endif 
        endif 
!
        c1 = subnam(1:1) 
        sname = c1=='S' .or. c1=='D' 
        cname = c1=='C' .or. c1=='Z' 
        if (.not.(cname .or. sname)) return  
        c2 = subnam(2:3) 
        c3 = subnam(4:6) 
        c4 = c3(2:3) 
!
        select case (ispec)  
!
        case default 
          nb = 1 
!
          if (c2 == 'GE') then 
            if (c3 == 'TRF') then 
              if (sname) then 
                nb = 64 
              else 
                nb = 64 
              endif 
            else if (c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF') &
                then 
              if (sname) then 
                nb = 32 
              else 
                nb = 32 
              endif 
            else if (c3 == 'HRD') then 
              if (sname) then 
                nb = 32 
              else 
                nb = 32 
              endif 
            else if (c3 == 'BRD') then 
              if (sname) then 
                nb = 32 
              else 
                nb = 32 
              endif 
            else if (c3 == 'TRI') then 
              if (sname) then 
                nb = 64 
              else 
                nb = 64 
              endif 
            endif 
          else if (c2 == 'PO') then 
            if (c3 == 'TRF') then 
              if (sname) then 
                nb = 64 
              else 
                nb = 64 
              endif 
            endif 
          else if (c2 == 'SY') then 
            if (c3 == 'TRF') then 
              if (sname) then 
                nb = 64 
              else 
                nb = 64 
              endif 
            else if (sname .and. c3=='TRD') then 
              nb = 1 
            else if (sname .and. c3=='GST') then 
              nb = 64 
            endif 
          else if (cname .and. c2=='HE') then 
            select case (c3)  
            case ('TRF')  
              nb = 64 
            case ('TRD')  
              nb = 1 
            case ('GST')  
              nb = 64 
            end select 
          else if (sname .and. c2=='OR') then 
            if (c3(1:1) == 'G') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nb = 32 
            else if (c3(1:1) == 'M') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nb = 32 
            endif 
          else if (cname .and. c2=='UN') then 
            if (c3(1:1) == 'G') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nb = 32 
            else if (c3(1:1) == 'M') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nb = 32 
            endif 
          else if (c2 == 'GB') then 
            if (c3 == 'TRF') then 
              if (sname) then 
                if (n4 <= 64) then 
                  nb = 1 
                else 
                  nb = 32 
                endif 
              else 
                if (n4 <= 64) then 
                  nb = 1 
                else 
                  nb = 32 
                endif 
              endif 
            endif 
          else if (c2 == 'PB') then 
            if (c3 == 'TRF') then 
              if (sname) then 
                if (n2 <= 64) then 
                  nb = 1 
                else 
                  nb = 32 
                endif 
              else 
                if (n2 <= 64) then 
                  nb = 1 
                else 
                  nb = 32 
                endif 
              endif 
            endif 
          else if (c2 == 'TR') then 
            if (c3 == 'TRI') then 
              if (sname) then 
                nb = 64 
              else 
                nb = 64 
              endif 
            endif 
          else if (c2 == 'LA') then 
            if (c3 == 'UUM') then 
              if (sname) then 
                nb = 64 
              else 
                nb = 64 
              endif 
            endif 
          else if (sname .and. c2=='ST') then 
            if (c3 == 'EBZ') nb = 1 
          endif 
          ilaenv = nb 
          return  
!
        case (2)  
          nbmin = 2 
          if (c2 == 'GE') then 
            if (c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF') then 
              if (sname) then 
                nbmin = 2 
              else 
                nbmin = 2 
              endif 
            else if (c3 == 'HRD') then 
              if (sname) then 
                nbmin = 2 
              else 
                nbmin = 2 
              endif 
            else if (c3 == 'BRD') then 
              if (sname) then 
                nbmin = 2 
              else 
                nbmin = 2 
              endif 
            else if (c3 == 'TRI') then 
              if (sname) then 
                nbmin = 2 
              else 
                nbmin = 2 
              endif 
            endif 
          else if (c2 == 'SY') then 
            if (c3 == 'TRF') then 
              if (sname) then 
                nbmin = 2 
              else 
                nbmin = 2 
              endif 
            else if (sname .and. c3=='TRD') then 
              nbmin = 2 
            endif 
          else if (cname .and. c2=='HE') then 
            if (c3 == 'TRD') nbmin = 2 
          else if (sname .and. c2=='OR') then 
            if (c3(1:1) == 'G') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nbmin = 2 
            else if (c3(1:1) == 'M') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nbmin = 2 
            endif 
          else if (cname .and. c2=='UN') then 
            if (c3(1:1) == 'G') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nbmin = 2 
            else if (c3(1:1) == 'M') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nbmin = 2 
            endif 
          endif 
          ilaenv = nbmin 
          return  
!
        case (3)  
          nx = 0 
          if (c2 == 'GE') then 
            if (c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF') then 
              if (sname) then 
                nx = 128 
              else 
                nx = 128 
              endif 
            else if (c3 == 'HRD') then 
              if (sname) then 
                nx = 128 
              else 
                nx = 128 
              endif 
            else if (c3 == 'BRD') then 
              if (sname) then 
                nx = 128 
              else 
                nx = 128 
              endif 
            endif 
          else if (c2 == 'SY') then 
            if (sname .and. c3=='TRD') nx = 1 
          else if (cname .and. c2=='HE') then 
            if (c3 == 'TRD') nx = 1 
          else if (sname .and. c2=='OR') then 
            if (c3(1:1) == 'G') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nx = 128 
            endif 
          else if (cname .and. c2=='UN') then 
            if (c3(1:1) == 'G') then 
              if (c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4==&
                'HR' .or. c4=='TR' .or. c4=='BR') nx = 128 
            endif 
          endif 
          ilaenv = nx 
          return  
!
        case (4)  
          ilaenv = 6 
          return  
!
        case (5)  
          ilaenv = 2 
          return  
!
        case (6)  
          ilaenv = int(real(min(n1,n2))*1.6E0) 
          return  
!
        case (7)  
          ilaenv = 1 
          return  
!
        case (8)  
          ilaenv = 50 
          return  
        end select 
      end select 
!
!     END OF ILAENV
!
      end function ilaenv 


      subroutine initsn(indeps, dirsm, dirsmh) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      use cosmo_C, only : disex2, nspa, nps2, srad, rds, lenabc, &
      & nden, fepsi, nppa, cif2, cif1, nps, dirvec, lenab2  
      USE molkst_C, only : keywrd, moperr, numat
      use permanent_arrays, only : nat, nfirst, nlast
      use chanel_C, only : iw 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use wrdkey_I 
      use reada_I 
      use mopend_I 
      use dvfill_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: indeps 
      real(double)  :: dirsm(3,nppa) 
      real(double)  :: dirsmh(3,nppa/3) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, incif, nsp, maxnps, inrsol, indels, indise, &
        iat, i4, n 
      real(double), dimension(53) :: rvdw, usevdw 
      real(double) :: epsi, ri1, ri2, rsolv, delsc, disex, avdw, x0, z3, z4, x 
      character, dimension(53) :: elemnt*2 
!-----------------------------------------------
!      DIMENSION RVDW(53), USEVDW(53), DIRSM(3,NPPA), DIRSMH(3,NPPA/3)
      data rvdw/ 1.08D0, 1.D0, 1.80D0, 999.D0, 999.D0, 1.53D0, 1.48D0, 1.36D0, &
        1.30D0, 999.D0, 2.30D0, 999.D0, 2.05D0, 2.10D0, 1.75D0, 1.70D0, 1.65D0&
        , 999.D0, 2.80D0, 2.75D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, &
        999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0&
        , 1.80D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, &
        999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0, 999.D0&
        , 999.D0, 2.05D0/  
      data elemnt/ 'H ', 'HE', 'LI', 'BE', 'B ', 'C ', 'N ', 'O ', 'F ', 'NE', &
        'NA', 'MG', 'AL', 'SI', 'P ', 'S ', 'CL', 'AR', 'K ', 'CA', 'SC', 'TI'&
        , 'V ', 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', &
        'SE', 'BR', 'KR', 'RB', 'SR', 'Y ', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH'&
        , 'PD', 'AG', 'CD', 'IN', 'SN', 'SB', 'TE', 'I '/  
      do i = 1, 53 
        j = 2 
        if (elemnt(i)(2:2) == ' ') j = 1 
        usevdw(i) = wrdkey(keywrd,'VDW(',4,':'//elemnt(i)(:j),j+1,rvdw(i)) 
      end do 
      epsi = reada(keywrd,indeps) 
      fepsi = (epsi - 1.D0)/(epsi + 0.5D0) 
      ri1 = 2.D0 
      incif = index(keywrd,'N**2') 
      if (incif /= 0) ri1 = reada(keywrd,incif + 4) 
      ri2 = sqrt(ri1**2) 
      cif2 = (ri2 - 1)/(ri2 + .5D0)/fepsi 
      cif1 = cif2 
      if (ri1 < 0) cif1 = 0.D0 
      nps = 0 
      nsp = 0 
      do i = 1, numat 
        nsp = nsp + min(nlast(i)-nfirst(i)+1,4) 
      end do 
      nden = 3*nsp - 2*numat 
      maxnps = int(sqrt(2*lenab2 + 0.251) - nden - 0.5) 
      maxnps = min(maxnps,lenabc) 
      write (iw, *) 'MAXIMUM NUMBER OF SEGMENTS ALLOWED:', maxnps 
      if ((nden*(nden + 1))/2 > lenabc) then 
        write (iw, *) 'PARAMETER LENABC IS TOO SMALL FOR THIS SYSTEM' 
        call mopend ('PARAMETER LENABC IS TOO SMALL FOR THIS SYSTEM') 
        return  
      endif 
      if (maxnps < 3*numat) then 
        write (iw, *) ' PARAMETER LENABC MUST BE INCREASED FOR THIS SYSTEM' 
        call mopend ('PARAMETER LENABC MUST BE INCREASED FOR THIS SYSTEM') 
        return  
      endif 
      rsolv = 1.D0 
      inrsol = index(keywrd,'RSOLV=') 
      if (inrsol /= 0) rsolv = reada(keywrd,inrsol) 
      if (rsolv < 0.) then 
        write (iw, *) 'RSOLV MUST NOT BE NEGATIVE' 
        call mopend ('RSOLV MUST NOT BE NEGATIVE') 
        return  
      endif 
      delsc = rsolv 
      indels = index(keywrd,'DELSC=') 
      if (indels /= 0) delsc = reada(keywrd,indels) 
      if (delsc < 0.1D0) write (iw, *) ' DELSC TOO SMALL: SET TO 0.1' 
      if (delsc > rsolv + 0.5D0) then 
        write (iw, *) ' DELSC UNREASONABLY LARGE' 
        call mopend (' DELSC UNREASONABLY LARGE') 
        return  
      endif 
      rds = max(delsc,0.1D0) 
      disex = 2.D0 
      indise = index(keywrd,'DISEX=') 
      if (indise /= 0) disex = reada(keywrd,indise) 
      do i = 1, numat 
        iat = nat(i) 
        if (iat > 53) then 
          write (iw, *) 'MISSING VAN DER WAALS RADIUS FOR '//elemnt(iat) 
          call mopend ('MISSING VAN DER WAALS RADIUS //ELEMNT(IAT)') 
          return  
        else 
          avdw = usevdw(iat) 
          if (avdw > 10.D0) then 
            write (iw, *) 'MISSING VAN DER WAALS RADIUS '//elemnt(iat) 
            call mopend ('MISSING VAN DER WAALS RADIUS //ELEMNT(IAT)') 
            return  
          endif 
        endif 
        srad(i) = avdw + rsolv 
      end do 
      nspa = 60 
      if (index(keywrd,'NSPA=') /= 0) nspa = nint(reada(keywrd,index(keywrd,&
        'NSPA'))) 
      x0 = log(nspa*0.1D0 - 0.199999D0) 
      z3 = log(3.D0) 
      z4 = log(4.D0) 
      i4 = int(x0/z4) 
      nps2 = 0 
      do i = 0, i4 
        x = x0 - i*z4 
        n = 3**int(x/z3)*4**i 
        nps2 = max0(n,nps2) 
      end do 
      nps = nps2/3 
      if (mod(nps2,3) /= 0) nps = nps2/4 
      nps2 = 10*nps2 + 2 
      nps = max(12,nps*10 + 2) 
      call dvfill (nps2, dirsm) 
      if (moperr) return  
      call dvfill (nps, dirsmh) 
      if (moperr) return  
      nps = -nps 
      disex2 = (4*(1.5D0 + rsolv - rds)*disex)**2/nspa 
      call dvfill (nppa, dirvec) 
      if (moperr) return  
      return  
      end subroutine initsn 


      subroutine initsv(indeps) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      use cosmo_C, only : nppa, abcmat
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  14:15:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use initsn_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: indeps  
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
      call initsn (indeps, abcmat, abcmat(3*nppa + 1)) 
      return  
      end subroutine initsv 
