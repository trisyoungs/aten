      subroutine solrot(ni, nj, xi, xj, wj, wk, kr, e1b, e2a, enuc, cutoff, ib&
        ) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use euler_C, only : tvec, l1l,l1u, l2l, l2u, l3l,l3u
      use molkst_C, only : numcal
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:02  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rotate_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: ni 
      integer  :: nj 
      integer , intent(inout) :: kr 
      integer , intent(in) :: ib 
      real(double) , intent(out) :: enuc 
      real(double)  :: cutoff 
      real(double) , intent(in) :: xi(3) 
      real(double) , intent(in) :: xj(3) 
      real(double) , intent(out) :: wj(100) 
      real(double) , intent(out) :: wk(100) 
      real(double) , intent(inout) :: e1b(10) 
      real(double) , intent(inout) :: e2a(10) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(107) :: imap 
      integer , dimension(5,5) :: kbfar 
      integer :: icalcn, ntyp, i, j, k, jj, ii, kb
      real(double), dimension(100) :: wsum, wbits 
      real(double), dimension(3) :: xjuc 
      real(double), dimension(10) :: e1bits, e2bits 
      real(double), dimension(100) :: wmax 
      real(double), dimension(3) :: xdumy 
      real(double), dimension(100,5,5) :: far 
      real(double), dimension(10,5,5) :: fare1b 
      real(double), dimension(5,5) :: farnuc 
      real(double) :: one, cutof2, r, enubit 

      save xdumy, imap, far, icalcn, ntyp 
!-----------------------------------------------
!***********************************************************************
!
!   SOLROT FORMS THE TWO-ELECTRON TWO-ATOM J AND K INTEGRAL STRINGS.
!          ON EXIT WJ = "J"-TYPE INTEGRALS
!                  WK = "K"-TYPE INTEGRALS
!
!      FOR MOLECULES, WJ = WK.
!***********************************************************************
      data icalcn/ 0/  
      data xdumy/ 3*0.D0/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
        ntyp = 0 
        imap = 0 
      endif 
      if (ib == 1) then 
        far = 0.D0 
      endif 
      one = 1.D0 
      if (Abs(xi(1) - xj(1)) <1.d-20 .and. Abs(xi(2) - xj(2)) <1.d-20 .and. &
      Abs(xi(3) - xj(3)) <1.d-20) one = 0.5D0 
      wmax = 0.D0 
      wsum = 0.D0 
      wbits = 0.D0 
      e1b = 0.D0 
      e2a = 0.D0 
      enuc = 0.D0 
      cutof2 = cutoff**2 
      do i = l1l, l1u 
        do j = l2l, l2u 
          do k = l3l, l3u 
            xjuc = xj + tvec(:,1)*i + tvec(:,2)*j + tvec(:,3)*k - xi 
            r = xjuc(1)**2 + xjuc(2)**2 + xjuc(3)**2 
            if (r > cutof2) then 
!
!  INTERACTION DISTANCE IS GREATER THAN CUTOFF
!
              jj = imap(nj) 
              if (jj == 0) then 
                ntyp = ntyp + 1 
                imap(nj) = ntyp 
                jj = ntyp 
              endif 
              ii = imap(ni) 
              if (ii == 0) then 
                ntyp = ntyp + 1 
                imap(ni) = ntyp 
                ii = ntyp 
              endif 
              if (far(1,ii,jj) == 0.D0) then 
                kb = 1 
                call rotate (ni, nj, xdumy, xjuc, far(1,ii,jj), kb, fare1b(1,ii&
                  ,jj), fare1b(1,jj,ii), farnuc(ii,jj), cutoff) 
                kb = kb - 1 
                kbfar(ii,jj) = kb 
              endif 
              kb = kbfar(ii,jj) 
              wsum(:kb) = wsum(:kb) + far(:kb,ii,jj) 
              e1b = e1b + fare1b(:,ii,jj) 
              e2a = e2a + fare1b(:,jj,ii) 
              enuc = enuc + farnuc(ii,jj)*one 
            else 
!
!  INTERACTION DISTANCE IS LESS THAN CUTOFF
!
              kb = 1 
              call rotate (ni, nj, xdumy, xjuc, wbits, kb, e1bits, e2bits, &
                enubit, cutoff) 
              kb = kb - 1 
              wsum(:kb) = wsum(:kb) + wbits(:kb) 
              if (wmax(1) < wbits(1)) then 
                wmax(:kb) = wbits(:kb) 
              endif 
              e1b = e1b + e1bits 
              e2a = e2a + e2bits 
              enuc = enuc + enubit*one 
            endif 
          end do 
        end do 
      end do 
      if (one < 0.9D0) then 
        wmax(:kb) = 0.D0 
      endif 
      wk(:kb) = wmax(:kb) 
      wj(:kb) = wsum(:kb) 
      kr = kb + kr 
      return  
      end subroutine solrot 
