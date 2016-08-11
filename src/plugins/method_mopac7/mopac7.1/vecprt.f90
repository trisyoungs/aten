      subroutine vecprt(a, numm) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat, norbs
      use permanent_arrays, only : nfirst, nlast, nat
      use chanel_C, only : iw
      use elemts_C, only : elemnt
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:04  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: numm 
      real(double) , intent(inout) :: a(*) 
 
 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(norbs+3*numat) :: natom 
      integer :: i, jlo, jhi, l, k, numb, limit, kk, na, ll, m, ma, n 
      real(double) :: sumax, fact 
      character , dimension(21) :: line*6 
      character , dimension(9) :: atorbs*2 
      character, dimension(norbs+3*numat) :: itext*2, jtext*2 

      save atorbs 
!-----------------------------------------------
!**********************************************************************
!
!  VECPRT PRINTS A LOWER-HALF TRIANGLE OF A SQUARE MATRIX, THE
!         LOWER-HALF TRIANGLE BEING STORED IN PACKED FORM IN THE
!         ARRAY "A"
!
! ON INPUT:
!      A      = ARRAY TO BE PRINTED
!      NUMM   = SIZE OF ARRAY TO BE PRINTED
!(REF) NUMAT  = NUMBER OF ATOMS IN THE MOLECULE (THIS IS NEEDED TO
!               DECIDE IF AN ATOMIC ARRAY OR ATOMIC ORBITAL ARRAY IS
!               TO BE PRINTED
!(REF) NAT    = LIST OF ATOMIC NUMBERS
!(REF) NFIRST = LIST OF ORBITAL COUNTERS
!(REF) NLAST  = LIST OF ORBITAL COUNTERS
!
!  NONE OF THE ARGUMENTS ARE ALTERED BY THE CALL OF VECPRT
!
!*********************************************************************
      data atorbs/ ' S', 'PX', 'PY', 'PZ', 'X2', 'XZ', 'Z2', 'YZ', 'XY'/  
      sumax = 1.D0 
      do i = 1, numm 
        sumax = max(abs(a((i*(i+1))/2)),sumax) 
      end do 
      i = int(log10(sumax)) 
      if (i==1 .or. i==2) i = 0 
      fact = 10.D0**(-i) 
      if (abs(fact - 1.D0) > 0.001D0) then 
        write (iw, '(A,F12.6)') 'Diagonal Terms should be Multiplied by', 1.D0/&
          fact 
        do i = 1, numm 
          a((i*(i+1))/2) = a((i*(i+1))/2)*fact 
        end do 
      endif 
      if (numat/=0 .and. numat==numm) then 
!
!    PRINT OVER ATOM COUNT
!
        do i = 1, numat 
          itext(i) = '  ' 
          jtext(i) = elemnt(nat(i)) 
          natom(i) = i 
        end do 
      else 
        if (numat/=0 .and. nlast(numat)==numm) then 
          do i = 1, numat 
            jlo = nfirst(i) 
            jhi = nlast(i) 
            l = nat(i) 
            k = 0 
            itext(jlo:jhi) = atorbs(:jhi-jlo+1) 
            jtext(jlo:jhi) = elemnt(l) 
            natom(jlo:jhi) = i 
          end do 
        else 
          numb = abs(numm) 
          do i = 1, numb 
            itext(i) = '  ' 
            jtext(i) = '  ' 
            natom(i) = i 
          end do 
        endif 
      endif 
      numb = abs(numm) 
      line = '------' 
      limit = (numb*(numb + 1))/2 
      kk = 8 
      na = 1 
   80 continue 
      ll = 0 
      m = min0(numb + 1 - na,6) 
      ma = 2*m + 1 
      m = na + m - 1 
      write (iw, 130) (itext(i),jtext(i),natom(i),i=na,m) 
      write (iw, 140) (line(k),k=1,ma) 
      do i = na, numb 
        ll = ll + 1 
        k = (i*(i - 1))/2 
        l = min0(k + m,k + i) 
        k = k + na 
        if (kk + ll > 50) then 
          write (iw, 150) 
          write (iw, 130) (itext(n),jtext(n),natom(n),n=na,m) 
          write (iw, 140) (line(n),n=1,ma) 
          kk = 4 
          ll = 0 
        endif 
        write (iw, 160) itext(i), jtext(i), natom(i), (a(n),n=k,l) 
      end do 
      if (l >= limit) go to 110 
      kk = kk + ll + 4 
      na = m + 1 
      if (kk + numb + 1 - na <= 50) go to 80 
      kk = 4 
      write (iw, 150) 
      go to 80 
  110 continue 
      if (abs(fact - 1.D0) > 0.001D0) then 
        do i = 1, numm 
          a((i*(i+1))/2) = a((i*(i+1))/2)/fact 
        end do 
      endif 
      return  
!
  130 format(/,/,13x,10(1x,a2,1x,a2,i3,2x)) 
  140 format(' ',21a6) 
  150 format('1') 
  160 format(' ',a2,1x,a2,i5,10f11.6)  
!
      end subroutine vecprt 
