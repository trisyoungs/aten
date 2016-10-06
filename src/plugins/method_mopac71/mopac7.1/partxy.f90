      subroutine partxy(c34, pq34, w, wd) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numcal, numat, lm6, method_dorbs
      use permanent_arrays, only : nat, nfirst, nlast, nw
      use parameters_C, only : gss, gsp, gpp, gp2, hsp

!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:32  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use formxd_I 
      use formxy_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: c34(*) 
      real(double)  :: pq34(*) 
      real(double)  :: w(*) 
      real(double)  :: wd(lm6,lm6) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(-1:8) :: nb 
      integer :: icalcn, kk, iband, kr, ls, ii, ia, ib, izn, nwii, lsp, i, iw, &
        j, jw, ijw, lsw, k, kw, l, lw, km, lm, klw, lx, ly, lz, jband, js, jj, &
        nwjj, jb, ja 
      real(double) :: aa, sum, bb, hpp, pp, gspss 
      save nb, icalcn 
!-----------------------------------------------
!------------------------------------------------------------------
!
!    PARTXY WORKS OUT  IN MNDO FORMALISM THE FIRST 2-INDICES TRANSFO.
!          REQUIRED IN THE COMPUTATION OF 2-ELECTRONS REPULSION OVER M.O
!  INPUT
!     C34   : VECTOR OF THE CURRENT CHARGE DISTRIBUTION BETWEEN TWO M.O.
!  OUTPUT
!     PQ34(PQ) : <P(1),Q(1)|C3(2),C4(2)> WHERE P ,Q  ARE A.O.
!                                          AND C3,C4 ARE M.O.
!                P AND Q RUN IN CANONICAL ORDER OVER THE A.O BELONGING
!                TO AN ATOM 'A' ONLY (BASIC ASSUMPTION OF MNDO SCHEME)
!                AND 'A' RUNS OVER THE ATOMS OF THE SYSTEM.
!     D.L. (DEWAR GROUP) 1986
!----------------------------------------------------------------------
      data nb/ 0, 1, 0, 0, 10, 0, 0, 0, 0, 45/  
      data icalcn/ 0/  
      if (numcal /= icalcn) then 
        icalcn = numcal 
      endif 
!     KK    : POINTER OF SUPPORTING ATOM, SPARKLES SKIPPED OUT.
      kk = 0 
!
!     LOOP OVER OUTER ATOM A, SPARKLES EXCLUDED.
!     ------------------------------------------
      iband = 1 
      kr = 1 
      ls = 0 
      do ii = 1, numat 
        ia = nfirst(ii) 
        ib = nlast(ii) 
        if (ib < ia) cycle  
        kk = kk + 1 
        ls = ls + iband 
        iband = nb(ib-ia) 
!
!     PQ34(IJ) = <IJ|KL> * C34(KL)  , 1-CENTRE CONTRIBUTIONS.
        izn = nat(ii) 
        if (method_dorbs) then 
          nwii = nw(ii) - 1 
          lsp = ls - 1 
          do i = ia, ib 
            iw = i - ia + 1 
            aa = 1.D0 
            do j = ia, i 
              if (i == j) aa = 0.5D0 
              lsp = lsp + 1 
              jw = j - ia + 1 
!
!    `J' Address IJ in W
!
              ijw = nwii + (iw*(iw - 1))/2 + jw 
              sum = 0.D0 
              lsw = ls - 1 
              do k = ia, ib 
                kw = k - ia + 1 
                bb = 1.D0 
                do l = ia, k 
                  if (l == k) bb = 0.5D0 
                  lsw = lsw + 1 
                  lw = l - ia + 1 
!
!    `J' Address KL in W
!
                  km = max(kw,lw) 
                  lm = min(kw,lw) 
                  klw = nwii + (km*(km - 1))/2 + lm 
!
!   The term itself
!
                  sum = sum + c34(lsw)*wd(ijw,klw)*bb 
                end do 
              end do 
              pq34(lsp) = sum*aa 
            end do 
          end do 
        else 
!     BLOCK SS
          pq34(ls) = c34(ls)*gss(izn)*0.25D0 
          if (ib > ia) then 
!        BLOCK SP AND PP
            hpp = 0.5D0*(gpp(izn)-gp2(izn)) 
            lx = ls + 2 
            ly = ls + 5 
            lz = ls + 9 
            pp = c34(lx) + c34(ly) + c34(lz) 
            pq34(ls+1) = hsp(izn)*c34(ls+1) 
            pq34(lx) = gpp(izn)*c34(lx)*0.25D0 
            pq34(ls+3) = hsp(izn)*c34(ls+3) 
            pq34(ls+4) = hpp*c34(ls+4) 
            pq34(ly) = gpp(izn)*c34(ly)*0.25D0 
            pq34(ls+6) = hsp(izn)*c34(ls+6) 
            pq34(ls+7) = hpp*c34(ls+7) 
            pq34(ls+8) = hpp*c34(ls+8) 
            pq34(lz) = gpp(izn)*c34(lz)*0.25D0 
            gspss = gsp(izn)*c34(ls)*0.25D0 
            pq34(ls) = pq34(ls) + gsp(izn)*pp*0.25D0 
            pq34(lx) = pq34(lx) + gp2(izn)*(c34(ly)+c34(lz))*0.25D0 + gspss 
            pq34(ly) = pq34(ly) + gp2(izn)*(c34(lz)+c34(lx))*0.25D0 + gspss 
            pq34(lz) = pq34(lz) + gp2(izn)*(c34(lx)+c34(ly))*0.25D0 + gspss 
          endif 
        endif 
        if (kk <= 1) cycle  
!
!        LOOP OVER CHARGE DISTRIBUTION OF INNER ATOMS  B < A .
!        -----------------------------------------------------
!        PQ34(IJ)=<IJ|KL>*C34(KL) 2-CENTRES CONTRIBUTIONS.
!
        jband = 1 
        js = 0 
        do jj = 1, ii - 1 
          js = js + jband 
          jband = nb(nlast(jj)-nfirst(jj)) 
          if (jband == 0) cycle  
!
!   IBAND AND JBAND ARE EITHER 0, 1, 10, OR 45
!
          if (method_dorbs) then 
            nwjj = nw(jj) - 1 
            jb = nlast(jj) 
            ja = nfirst(jj) 
            call formxd (wd, nwii, nwjj, pq34(ls), pq34(js), c34(ls), ib - ia&
               + 1, c34(js), jb - ja + 1) 
          else 
            call formxy (w(kr), kr, pq34(ls), pq34(js), c34(ls), iband, c34(js)&
              , jband) 
          endif 
        end do 
      end do 
      return  
      end subroutine partxy 
