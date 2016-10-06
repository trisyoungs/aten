      subroutine mpcpop(c, icok) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : norbs, numat
      use permanent_arrays, only : nat, nfirst, nlast
      use parameters_C, only : tore
      use chanel_C, only : iw
      use elemts_C, only : elemnt
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:29  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: icok 
      real(double) , intent(in) :: c(norbs*norbs) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, if, il, j, k 
      real(double), dimension(numat) :: pop, chrg 
      real(double) :: sum 
!-----------------------------------------------
!
! This subroutine calculates the total Mulliken populations on the
!   atoms by summing the diagonal elements from the  Mulliken
!   population analysis.
!
!
! ICOK = 1 ==> PRINT POPULATIONS
! ICOK = 0 ==> KEYWORD mulliken = .f.
!         NO POPULATION ANALYSIS PERFORMED
!
      if (icok /= 0) then 
        do i = 1, numat 
          if = nfirst(i) 
          il = nlast(i) 
          sum = 0.D0 
          pop(i) = 0.D0 
          chrg(i) = 0.D0 
          do j = if, il 
!
!    Diagonal element of mulliken matrix
!
            sum = sum + c((j*(j+1))/2) 
          end do 
          k = nat(i) 
!
!    Mulliken population for i'th atom
!
          pop(i) = sum 
          chrg(i) = tore(k) - pop(i) 
        end do 
        write (iw, '(3/8X,''MULLIKEN POPULATIONS AND CHARGES'',/)') 
        write (iw, &
      '(6X,''NO.'',2X,''ATOM'',3X,''POPULATION'',6X,                     ''CHAR&
      &GE'')') 
        do j = 1, numat 
          write (iw, '(5X,I4,3X,A2,F13.6,F14.6)') j, elemnt(nat(j)), pop(j), &
            chrg(j) 
        end do 
      endif 
      return     
      end subroutine mpcpop 
