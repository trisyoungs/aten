      subroutine refer 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE refs_C , only : allref
      use chanel_C, only : iw
      use permanent_arrays, only : nat
      use molkst_C, only : numat, keywrd
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:00  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, mode
      logical :: allok 
      logical , dimension(107) :: elemns 
      logical :: mixok, mix 

      save mix 
!-----------------------------------------------
      data mix/ .FALSE./  
      mixok = index(keywrd,'PARASOK') /= 0 
      elemns(:102) = .FALSE. 
      if (index(keywrd,'MNDOD') /= 0) then 
        mode = 5 
      else if (index(keywrd,'PM3') /= 0) then 
        mode = 4 
      else if (index(keywrd,'AM1') /= 0) then 
        mode = 3 
      else 
        mode = 1 
      endif 
      allref(99,mode) = &
        ' DUMMY ATOMS ARE USED; THESE DO NOT AFFECT THE CALCULATION' 
      allref(100,mode) = ' ' 
      elemns(nat(:numat)) = .TRUE. 
      allok = .TRUE. 
      do i = 1, 102 
        if (.not.elemns(i)) cycle  
        if (i<99 .and. .not.mix .and. mode==3) mix = index(allref(i,3),'MNDO')&
           /= 0 
        if (allref(i,5)(1:1) /= ' ') allref(i,5) = allref(i,1) 
        if (allref(i,mode)(1:1) /= ' ') then 
          write (iw, '(A,I3)') ' DATA ARE NOT AVAILABLE FOR ELEMENT NO.', i 
          allok = .FALSE. 
        else 
          write (iw, '(A)') allref(i,mode) 
        endif 
      end do 
      if (mix .and. .not.mixok) then 
        write (iw, 40) 'SOME ELEMENTS HAVE BEEN SPECIFIED FOR WHICH ONLY MNDO'&
          , 'PARAMETERS ARE AVAILABLE.  SUCH MIXTURES OF METHODS ARE', &
          'VERY RISKY AND HAVE NOT BEEN FULLY TESTED.  IF YOU FEEL', &
          'THE RISK IS WORTH WHILE - CHECK THE MANUAL FIRST - THEN', &
          'SPECIFY "PARASOK" IN THE KEYWORDS' 
          call mopend ('MIXED PARAMETER SETS.  USE "PARASOK" TO CONTINUE') 
        return  
      endif 
      if (allok) return  
      write (iw, 40) 'SOME ELEMENTS HAVE BEEN SPECIFIED FOR WHICH', &
        'NO PARAMETERS ARE AVAILABLE.  CALCULATION STOPPED.' 
      return  
   40 format(/,/,/,/,/,10x,a,4(/,10x,a))  
      end subroutine refer 
