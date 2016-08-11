      subroutine enpart(wd) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : h, p, pa, pb, nfirst, nlast, nat, &
      & coord, w, nw
      use molkst_C, only : keywrd, norbs, numat, lm6, uhf, &
       method_mndod
      use elemts_C, only : elemnt 
      use chanel_C, only : iw
      USE parameters_C, only : alp, tore, guess1, guess2, guess3,  &
      gpp, gp2, hsp, gss, gsp, uss, upp, udd 
!----------------------------------------------------------*
!
!     SUBROUTINE ENPART,  MODIFIED BY TSUNEO HIRANO 1986/6/3/
!
!----------------------------------------------------------*
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:12  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rotatd_I 
      use elenuc_I 
      implicit none
      real(double), dimension(lm6,lm6) :: wd
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: j1, j4, linear, i, k, ii, ia, ib, ni, j, nwii, iv, jw, ij&
        , ijw, kw, l, lw, ip, jp, ijp, im, jm, klw, ikw, jlw, iss, ia1, ia2, &
        ixx, iyy, izz, ixy, ixz, iyz, isx, isy, isz, kl, n, iminus, jj, ja, jb&
        , ka, ik, kk, nab, nj, jss, nt, ig, lmw, ja2, jb2, ib2, i2, j2, ij2, &
        kinc, jap1, kc, iap1, i3, j3, kb, k3, jk, l3, il, jl, numat1 
      real(double), dimension(171) :: h2 
      real(double), dimension(8100) :: w2 
      real(double), dimension((numat*(numat + 1))/2,2) :: ea
      real(double), dimension((numat*(numat + 1))/2,4) :: e
      real(double), dimension((numat*(numat + 1))/2,3) :: ex
      real(double) :: t, sum1, aa, sum, ss1, ss2, ss3, ss4, ss5&
        , tt1, tt2, tt3, tt4, tt5, eau, eae, tone, oneii, onejj, g, r, scale, &
        enuc, bb, pij, eabr, eabx, eabee, eaben, eabnn, eabrx, &
        eabe, ttwo, et 
      logical :: am1, sparkl
!----------------------------------------------- 
!--- DEFINED HERE, AND TO BE USED FOR ENPART-PRINT ONLY ---*
!--- END OF DIMENSION DEFINITION ----------------- BY TH --*
!***********************************************************************
!
! *** ENERGY PARTITIONING WITHIN THE UMNDO SCHEME
!     ROUTINE WRITTEN BY S.OLIVELLA, BARCELONA NOV. 1979.
!     EXTENDED TO AM1 AND PM3 BY JJPS.
!
!   ON INPUT UHF     = .TRUE. IF A U.H.F. CALCULATION.
!            H       = ONE-ELECTRON MATRIX.
!            pa      = Alpha ELECTRON DENSITY.
!            pb      = Beta ELECTRON DENSITY.
!            P       = TOTAL ELECTRON DENSITY.
!            Q       = ATOM ELECTRON DENSITIES.
!
!    NOTHING IS CHANGED ON EXIT.
!
!***********************************************************************
! *** RECALCULATE THE DENSITY MATRICES IN THE UHF SCHEME
!
      linear = norbs*(norbs + 1)/2 
      if (.not.uhf) pb = pa 
!
! *** ONE-CENTER ENERGIES
!
      k = 0 
      do ii = 1, numat 
        ia = nfirst(ii) 
        ib = nlast(ii) 
        ni = nat(ii) 
        ea(ii,1) = 0.0D0 
        go to (60,40,40,40,20,20,20,20,20) ib - ia + 1 
   20   continue 
        t = udd(ni) 
        do j = ia + 4, ib 
          ea(ii,1) = ea(ii,1) + p((j*(j+1))/2)*t 
        end do 
   40   continue 
        t = upp(ni) 
        do j = ia + 1, ia + 3 
          ea(ii,1) = ea(ii,1) + p((j*(j+1))/2)*t 
        end do 
   60   continue 
        ea(ii,1) = ea(ii,1) + p((ia*(ia+1))/2)*uss(ni) 
        if (method_mndod) then 
          sum1 = 0.D0 
          nwii = nw(ii) - 1 
!
!   One-center coulomb and exchange terms for atom II.
!
!  F(i,j)=F(i,j)+sum(k,l)((PA(k,l)+PB(k,l))*<i,j|k,l>
!                        -(PA(k,l)        )*<i,k|j,l>), k,l on atom II.
!
          do i = ia, ib 
            iv = i - ia + 1 
            aa = 2.D0 
            do j = ia, i 
              if (i == j) aa = 1.D0 
              jw = j - ia + 1 
!
!    Address in `F'
!
              ij = (i*(i - 1))/2 + j 
!
!    `J' Address IJ in W
!
              ijw = nwii + (iv*(iv - 1))/2 + jw 
              sum = 0.D0 
              do k = ia, ib 
                kw = k - ia + 1 
                do l = ia, ib 
                  lw = l - ia + 1 
                  ip = max(k,l) 
                  jp = min(k,l) 
!
!    Address in `P'
!
                  ijp = (ip*(ip - 1))/2 + jp 
!
!    `J' Address KL in W
!
                  im = max(kw,lw) 
                  jm = min(kw,lw) 
                  klw = nwii + (im*(im - 1))/2 + jm 
!
!    `K' Address IK in W
!
                  im = max(kw,jw) 
                  jm = min(kw,jw) 
                  ikw = nwii + (im*(im - 1))/2 + jm 
!
!    `K' Address JL in W
!
                  im = max(lw,iv) 
                  jm = min(lw,iv) 
                  jlw = nwii + (im*(im - 1))/2 + jm 
!
!   The term itself
!
                  sum = sum + aa*(pa(ij)*(p(ijp)*wd(ijw,klw)-pa(ijp)*wd(&
                    ikw,jlw))+pb(ij)*(p(ijp)*wd(ijw,klw)-pb(ijp)*wd(ikw,jlw&
                    ))) 
                end do 
              end do 
              sum1 = sum1 + sum 
            end do 
          end do 
          ea(ii,2) = sum1*0.5D0 
        else 
          iss = (ia*(ia + 1))/2 
          ea(ii,2) = 0.5D0*gss(ni)*p(iss)*p(iss) - 0.5D0*gss(ni)*(pa(iss)*&
            pa(iss)+pb(iss)*pb(iss)) 
          if (ia == ib) cycle  
          ia1 = ia + 1 
          ia2 = ia + 2 
          ixx = ia1*ia2/2 
          iyy = ia2*ib/2 
          izz = (ib*(ib + 1))/2 
          ixy = ia1 + ia2*ia1/2 
          ixz = ia1 + ib*ia2/2 
          iyz = ia2 + ib*ia2/2 
          isx = ia + ia1*ia/2 
          isy = ia + ia2*ia1/2 
          isz = ia + ib*ia2/2 
          ss1 = p(ixx)*p(ixx) + p(iyy)*p(iyy) + p(izz)*p(izz) 
          ss2 = p(iss)*(p(ixx)+p(iyy)+p(izz)) 
          ss3 = p(ixx)*p(iyy) + p(ixx)*p(izz) + p(iyy)*p(izz) 
          ss4 = p(isx)*p(isx) + p(isy)*p(isy) + p(isz)*p(isz) 
          ss5 = p(ixy)*p(ixy) + p(ixz)*p(ixz) + p(iyz)*p(iyz) 
          tt1 = pa(ixx)*pa(ixx) + pa(iyy)*pa(iyy) + pa(izz)*&
            pa(izz) + pb(ixx)*pb(ixx) + pb(iyy)*pb(iyy) + pb(izz)*&
            pb(izz) 
          tt2 = pa(iss)*(pa(ixx)+pa(iyy)+pa(izz)) + pb(iss)*(pb&
            (ixx)+pb(iyy)+pb(izz)) 
          tt3 = pa(ixx)*pa(iyy) + pa(ixx)*pa(izz) + pa(iyy)*&
            pa(izz) + pb(ixx)*pb(iyy) + pb(ixx)*pb(izz) + pb(iyy)*&
            pb(izz) 
          tt4 = pa(isx)*pa(isx) + pa(isy)*pa(isy) + pa(isz)*&
            pa(isz) + pb(isx)*pb(isx) + pb(isy)*pb(isy) + pb(isz)*&
            pb(isz) 
          tt5 = pa(ixy)*pa(ixy) + pa(ixz)*pa(ixz) + pa(iyz)*&
            pa(iyz) + pb(ixy)*pb(ixy) + pb(ixz)*pb(ixz) + pb(iyz)*&
            pb(iyz) 
          ea(ii,2) = ea(ii,2) + 0.5D0*gpp(ni)*ss1 + gsp(ni)*ss2 + gp2(ni)*ss3&
             + hsp(ni)*ss4*2.0D0 + 0.5D0*(gpp(ni)-gp2(ni))*ss5*2.0D0 - 0.5D0*&
            gpp(ni)*tt1 - gsp(ni)*tt4 - gp2(ni)*tt5 - hsp(ni)*(tt2 + tt4) - &
            0.5D0*(gpp(ni)-gp2(ni))*(tt3 + tt5) 
        endif 
      end do 
      am1 = index(keywrd,'AM1') + index(keywrd,'PM3') /= 0 
      if (index(keywrd,'PM3') /= 0) then 
        write (iw, '(3/,10X,''TOTAL ENERGY PARTITIONING IN PM3'')') 
      else if (index(keywrd,'AM1') /= 0) then 
        write (iw, '(3/,10X,''TOTAL ENERGY PARTITIONING IN AM1'')') 
      else 
        write (iw, '(3/,10X,''TOTAL ENERGY PARTITIONING IN MNDO'')') 
      endif 
      write (iw, '(/10X,''ALL ENERGIES ARE IN ELECTRON VOLTS'')') 
      kl = 0 
      k = kl + 1 
      kl = kl + 10 
      kl = min0(kl,numat) 
      do while(numat > kl) 
        k = kl + 1 
        kl = kl + 10 
        kl = min0(kl,numat) 
      end do 
      eau = 0.0D0 
      eae = 0.0D0 
      do i = 1, numat 
        eau = eau + ea(i,1) 
        eae = eae + ea(i,2) 
      end do 
      tone = eau + eae 
! *** TWO-CENTER ENERGIES
!     RESONANCE (E(N,1)) TERMS
      n = 1 
      do ii = 2, numat 
        e(n,1) = 0.0D0 
        ia = nfirst(ii) 
        ib = nlast(ii) 
        iminus = ii - 1 
        oneii = 1.D0 
        if (nat(ii) == 102) oneii = 0.D0 
        do jj = 1, iminus 
          n = n + 1 
          ja = nfirst(jj) 
          jb = nlast(jj) 
          onejj = 1.D0 
          if (nat(jj) == 102) onejj = 0.D0 
          e(n,1) = 0.0D0 
          do i = ia, ib 
            ka = (i*(i - 1))/2 
            do k = ja, jb 
              ik = ka + k 
              e(n,1) = e(n,1) + 2.0D0*p(ik)*h(ik)*oneii*onejj 
            end do 
          end do 
        end do 
        n = n + 1 
      end do 
!
!     THE CODE THAT FOLLOWS APPLIES ONLY TO MNDO
!
!     CORE-CORE REPULSION (E(N,2)) AND CORE-ELEC. ATTRACTION (E(N,3)).
        n = 1 
        sparkl = .FALSE. 
        kk = 0 
        do ii = 2, numat 
          e(n,2) = 0.0D0 
          e(n,3) = 0.0D0 
          ia = nfirst(ii) 
          ib = nlast(ii) 
          nab = ib - ia + 1 
          ni = nat(ii) 
          iss = (ia*(ia + 1))/2 
          iminus = ii - 1 
          do jj = 1, iminus 
            n = n + 1 
            e(n,2) = 0.0D0 
            e(n,3) = 0.0D0 
            ja = nfirst(jj) 
            jb = nlast(jj) 
            nj = nat(jj) 
            jss = (ja*(ja + 1))/2 
            if (ib>=ia .and. jb>=ja) then 
!
!   Pick up the <ss|ss> term
!
              if (method_mndod) then 
                g = wd(nw(ii),nw(jj)) 
              else 
                kk = kk + 1 
                g = w(kk) 
                e(n,3) = -(p(iss)*tore(nj)+p(jss)*tore(ni))*g 
              endif 
            else 
              sparkl = .TRUE. 
              g = 0.D0 
            endif 
            r = sqrt((coord(1,ii)-coord(1,jj))**2+(coord(2,ii)-coord(2,jj))**2+&
              (coord(3,ii)-coord(3,jj))**2) 
            scale = 1.0D0 + exp((-alp(ni)*r)) + exp((-alp(nj)*r)) 
            nt = ni + nj 
            if (nt>=8 .and. nt<=9) then 
              if (ni==7 .or. ni==8) scale = scale + (r - 1.0D0)*exp((-alp(ni)*r&
                )) 
              if (nj==7 .or. nj==8) scale = scale + (r - 1.0D0)*exp((-alp(nj)*r&
                )) 
            endif 
            e(n,2) = tore(ni)*tore(nj)*g*scale 
            if (am1) then 
              scale = 0.0D0 
              do ig = 1, 4 
                if (abs(guess1(ni,ig)) > 0.D0) scale = scale + tore(ni)*tore(nj)/r&
                  *guess1(ni,ig)*exp((-guess2(ni,ig)*(r-guess3(ni,ig))**2)) 
                if (abs(guess1(nj,ig)) <= 0.D0) cycle  
                scale = scale + tore(ni)*tore(nj)/r*guess1(nj,ig)*exp((-guess2(nj,ig)&
                  *(r-guess3(nj,ig))**2)) 
              end do 
              e(n,2) = e(n,2) + scale 
            endif 
            if (jb<ja .or. ib<ia) cycle  
            if (method_mndod) then 
              lmw = 90 
              ja2 = 1 
              jb2 = jb - ja + 1 
              ia2 = jb2 + 1 
              ib2 = ib - ia + ia2 
              jp = 1 
              ip = 1 + (nab*(nab + 1))/2 
              call rotatd (ip, jp, ia2, ib2, ja2, jb2, ni, nj, coord(1,ii), &
                coord(1,jj), w2, lmw, enuc) 
!
!   Electron-nuclear attraction terms
!
              linear = (ib2*(ib2 + 1))/2 
              h2(:linear) = 0.D0 
              call elenuc (ia2, ib2, ja2, jb2, h2) 
              sum = 0.D0 
              i2 = jb2 
              do i = ia, ib 
                i2 = i2 + 1 
                j2 = jb2 
                do j = ia, i 
                  j2 = j2 + 1 
                  ij = (i*(i - 1))/2 + j 
                  ij2 = (i2*(i2 - 1))/2 + j2 
                  bb = 2.D0 
                  if (i == j) bb = 1.D0 
                  sum = sum + bb*p(ij)*h2(ij2) 
                end do 
              end do 
              e(n,3) = e(n,3) + sum 
              sum = 0.D0 
              i2 = 0 
              do i = ja, jb 
                i2 = i2 + 1 
                j2 = 0 
                do j = ja, i 
                  j2 = j2 + 1 
                  ij = (i*(i - 1))/2 + j 
                  ij2 = (i2*(i2 - 1))/2 + j2 
                  bb = 2.D0 
                  if (i == j) bb = 1.D0 
                  sum = sum + bb*p(ij)*h2(ij2) 
                end do 
              end do 
              e(n,3) = e(n,3) + sum 
            else 
              if (ja >= jb) go to 220 
              kinc = 9 
              jap1 = ja + 1 
              do k = jap1, jb 
                kc = (k*(k - 1))/2 
                do l = ja, k 
                  kl = kc + l 
                  kk = kk + 1 
                  bb = 2.0D0 
                  if (k == l) bb = 1.0D0 
                  e(n,3) = e(n,3) - p(kl)*tore(ni)*bb*w(kk) 
                end do 
              end do 
            endif 
            go to 230 
  220       continue 
            kinc = 0 
  230       continue 
            if (method_mndod) cycle  
            if (ia >= ib) cycle  
            iap1 = ia + 1 
            do i = iap1, ib 
              ka = (i*(i - 1))/2 
              do j = ia, i 
                ij = ka + j 
                aa = 2.0D0 
                if (i == j) aa = 1.0D0 
                kk = kk + 1 
                e(n,3) = e(n,3) - p(ij)*tore(nj)*aa*w(kk) 
                kk = kk + kinc 
              end do 
            end do 
          end do 
          n = n + 1 
        end do 
!     COULOMB (E(N,4)) AND EXCHANGE (EX(N)) TERMS
        n = 1 
        kk = 0 
        do ii = 2, numat 
          e(n,4) = 0.0D0 
          ex(n,1) = 0.0D0 
          ia = nfirst(ii) 
          ib = nlast(ii) 
          iminus = ii - 1 
          do jj = 1, iminus 
            ja = nfirst(jj) 
            jb = nlast(jj) 
            n = n + 1 
            e(n,4) = 0.0D0 
            ex(n,1) = 0.0D0 
            i3 = 0 
            do i = ia, ib 
              i3 = i3 + 1 
              ka = (i*(i - 1))/2 
              j3 = 0 
              do j = ia, i 
                j3 = j3 + 1 
                kb = (j*(j - 1))/2 
                ij = ka + j 
                aa = 2.0D0 
                if (i == j) aa = 1.0D0 
                pij = p(ij) 
                k3 = 0 
                if (method_mndod) then 
                  do k = ja, jb 
                    k3 = k3 + 1 
                    kc = (k*(k - 1))/2 
                    ik = ka + k 
                    jk = kb + k 
                    l3 = 0 
                    do l = ja, k 
                      l3 = l3 + 1 
                      il = ka + l 
                      jl = kb + l 
                      kl = kc + l 
                      bb = 2.0D0 
                      if (k == l) bb = 1.0D0 
                      g = wd(nw(ii)+(i3*(i3-1))/2+j3-1,nw(jj)+(k3*(k3-1))/2+l3-&
                        1) 
                      e(n,4) = e(n,4) + aa*bb*g*pij*p(kl) 
                      ex(n,1) = ex(n,1) - 0.5D0*aa*bb*g*(pa(ik)*pa(jl)+&
                        pa(il)*pa(jk)+pb(ik)*pb(jl)+pb(il)*pb(jk)&
                        ) 
                    end do 
                  end do 
                else 
                  do k = ja, jb 
                    k3 = k3 + 1 
                    kc = (k*(k - 1))/2 
                    ik = ka + k 
                    jk = kb + k 
                    l3 = 0 
                    do l = ja, k 
                      l3 = l3 + 1 
                      il = ka + l 
                      jl = kb + l 
                      kl = kc + l 
                      bb = 2.0D0 
                      if (k == l) bb = 1.0D0 
                      kk = kk + 1 
                      g = w(kk) 
                      e(n,4) = e(n,4) + aa*bb*g*pij*p(kl) 
                      ex(n,1) = ex(n,1) - 0.5D0*aa*bb*g*(pa(ik)*pa(jl)+&
                        pa(il)*pa(jk)+pb(ik)*pb(jl)+pb(il)*pb(jk)&
                        ) 
                    end do 
                  end do 
                endif 
              end do 
            end do 
          end do 
          n = n + 1 
        end do 
      numat1 = (numat*(numat + 1))/2 
      e(numat1,:) = 0.0D0 
      ex(numat1,:) = 0.0D0 
!@ --------------------------*
!-----PRINT OUT ONE AND TWO CENTER ENERGIES
!
!     E(I,1):     RESONANCE ENERGY
!     E(I,2):     NUCLEAR-NUCLEAR REPULSION ENERGY
!     E(I,3):     ELECTRON-NUCLEAR ATTRACTION ENERGY
!     E(I,4):     ELECTRON-ELECTRON REPULSION ENERGY
!     EX(I,1):    EXCHANGE  ENERGY
!     EX(I,2):    EXCHANGE + RESONANCE ENERGY
!#      WRITE(IW,'(//,''       ONE AND TWO CENTER ENERGIES (EV) '')')
!
!#      WRITE(IW,'(/,''  [RESONANCE TERM] (EV)'')')
!#      CALL VECPRT(E,NUMAT)
!
!#      WRITE(IW,'(/,''  [EXCHANGE TERM] (EV)'')')
!#      CALL VECPRT(EX,NUMAT)
!
!#      WRITE(IW,'(/,''  [RESONANCE + EXCHANGE] (EV)'')')
      ex(:numat1,2) = e(:numat1,1) + ex(:numat1,1) 
!
!   ADD IN MONOCENTRIC EXCHANGE AND COULOMBIC TERM
!
      do i = 1, numat 
        ex((i*(i+1))/2,2) = ea(i,2) 
      end do 
!
!
      do i = 1, numat 
        e((i*(i+1))/2,3) = ea(i,1) 
      end do 
!
!
      ex(:numat1,3) = e(:numat1,4) + e(:numat1,3) + e(:numat1,2) 
!     PRINT OUT OF TOTAL COULOMB TERM
!     PRINT OUT OF TWO-CENTER SUM(OFF-DIAGONAL) +
!                  ONE-CENTER SUM(DIAGONAL).
      write (iw, '(/,8(10X,A,/))') '  ONE-CENTER TERMS', ' ', &
        'E-E:  ELECTRON-ELECTRON REPULSION', &
        'E-N:  ELECTRON-NUCLEAR ATTRACTION' 
      write (iw, '(/,''   ATOM     E-E       E-N    (E-E + E-N)'')') 
      do i = 1, numat 
        j = (i*(i + 1))/2 
        write (iw, '(2X,A2,I3,1X,2F10.4,F10.4)') elemnt(nat(i)), i, ex(j,2), e(&
          j,3), ex(j,2) + e(j,3) 
      end do 
      write (iw, '(/,8(10X,A,/))') '    TWO-CENTER TERMS', ' ', &
        'J:   RESONANCE ENERGY          E-E: ELECTRON-ELECTRON REPULSION', &
        'K:   EXCHANGE ENERGY           E-N: ELECTRON-NUCLEAR ATTRACTION', &
        '                               N-N: NUCLEAR-NUCLEAR REPULSION', &
        'C:   COULOMBIC INTERACTION = E-E + E-N + N-N', &
        'EE:  TOTAL OF ELECTRONIC AND NUCLEAR ENERGIES' 
      if (sparkl) write (iw, &
      '(/,'' **** WARNING - SPARKLES '',                       ''ARE NOT TREATE&
      &D CORRECTLY ****'',2/)') 
      write (iw, &
      '(/,''   ATOM          J        K       E-'',            ''E       E-N   &
      &   N-N      C        EE'')') 
      write (iw, '(''   PAIR'')') 
      ij = 0 
      do i = 1, numat 
        if (i<6 .or. i==numat) then 
          do j = 1, i 
            ij = ij + 1 
            if (i /= j) then 
              write (iw, &
      '(1X,A2,I3,1X,A2,I3,1X,2F9.4,                   F9.4,F10.4,F9.4,F8.4     &
      &                                         ,F9.4)') elemnt(nat(i)), i, &
                elemnt(nat(j)), j, e(ij,1), ex(ij,1), e(ij,4), e(ij,3), e(ij,2)&
                , ex(ij,3), ex(ij,2) + ex(ij,3) 
            else 
              write (iw, *) 
            endif 
          end do 
        else 
          do j = 1, i 
            ij = ij + 1 
            if (i /= j) then 
              write (iw, &
      '(1X,A2,I3,1X,A2,I3,1X,2F9.4,                   F9.4,F10.4,F9.4,F8.4     &
      &                                         ,F9.4)') elemnt(nat(i)), i, &
                elemnt(nat(j)), j, e(ij,1), ex(ij,1), e(ij,4), e(ij,3), e(ij,2)&
                , ex(ij,3), ex(ij,2) + ex(ij,3) 
            else 
              write (iw, &
      '(/,''   ATOM          J        K       E-'',''E       E-N      N-N      &
      &C        EE'')') 
              write (iw, '(''   PAIR'')') 
            endif 
          end do 
        endif 
      end do 
!
!     ++++   TOTALS   ++++
!
      eabr = 0.0D0 
      eabx = 0.0D0 
      eabee = 0.0D0 
      eaben = 0.0D0 
      eabnn = 0.0D0 
      e((/(j1,j1=1,numat)/)*(/(j4,j4=2,numat+1)/)/2,3) = 0.D0 
      do i = 1, numat1 
        eabr = eabr + e(i,1) 
        eabx = eabx + ex(i,1) 
        eabee = eabee + e(i,4) 
        eaben = eaben + e(i,3) 
        eabnn = eabnn + e(i,2) 
      end do 
      eabrx = eabr + eabx 
      eabe = eabee + eaben + eabnn 
      ttwo = eabrx + eabe 
      et = tone + ttwo 
!@ ***************************************************************
      write (iw, 450) 
  450 format(/,/,/,'***  SUMMARY OF ENERGY PARTITION  ***') 
      write (iw, 460) 
  460 format(' ','---------------------------------------') 
      write (iw, '(''     ONE-CENTER TERMS'')') 
      write (iw, 470) eau 
  470 format(/,' ELECTRON-NUCLEAR  (ONE-ELECTRON) ',f17.4,' EV') 
      write (iw, 480) eae 
  480 format(' ELECTRON-ELECTRON (TWO-ELECTRON) ',f17.4,' EV') 
      write (iw, 490) tone 
  490 format(/,' TOTAL OF ONE-CENTER TERMS        ',18x,f15.4,' EV') 
      write (iw, 460) 
      write (iw, '(''     TWO-CENTER TERMS'')') 
      write (iw, 500) eabr 
  500 format(/,' RESONANCE ENERGY',8x,f15.4,' EV') 
      write (iw, 510) eabx 
  510 format(' EXCHANGE ENERGY ',8x,f15.4,' EV') 
      write (iw, 520) eabrx 
  520 format(/,' EXCHANGE + RESONANCE ENERGY:       ',f15.4,' EV') 
      write (iw, 530) eabee 
  530 format(/,' ELECTRON-ELECTRON REPULSION',f12.4,' EV') 
      write (iw, 540) eaben 
  540 format(' ELECTRON-NUCLEAR ATTRACTION',f12.4,' EV') 
      write (iw, 550) eabnn 
  550 format(' NUCLEAR-NUCLEAR REPULSION  ',f12.4,' EV') 
      write (iw, 560) eabe 
  560 format(/,' TOTAL ELECTROSTATIC INTERACTION    ',f15.4,' EV',/) 
      write (iw, 570) ttwo 
  570 format(' GRAND TOTAL OF TWO-CENTER TERMS   ',17x,f15.4,' EV') 
      write (iw, 460) 
      write (iw, 580) et 
  580 format(' ETOT (EONE + ETWO)   ',30x,f15.4,' EV'/,/) 
      return  
      end subroutine enpart 
