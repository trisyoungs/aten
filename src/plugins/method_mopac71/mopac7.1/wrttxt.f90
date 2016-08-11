      subroutine wrttxt(iprt) 
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      use molkst_C, only : keywrd, koment, title 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iprt 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i 
!-----------------------------------------------
      do i = 81, 2, -1 
        if (keywrd(i:i) == ' ') cycle  
        exit  
      end do 
      write (iprt, '(A)') keywrd(:i) 
      if (index(keywrd(1:81),' +') + index(keywrd(1:81),'&') + index(keywrd,&
        'SETUP') /= 0) then 
        do i = 161, 82, -1 
          if (keywrd(i:i) == ' ') cycle  
          exit  
        end do 
        if (keywrd(81:81) /= ' ') then 
          write (iprt, '(1X,A)') keywrd(81:i) 
        else 
          write (iprt, '(A)') keywrd(81:i) 
        endif 
      endif 
      if (index(keywrd(81:241),' +') + index(keywrd(81:241),'&') + index(keywrd&
        ,'SETUP') /= 0) then 
        do i = 241, 162, -1 
          if (keywrd(i:i) == ' ') cycle  
          exit  
        end do 
        if (keywrd(161:161) /= ' ') then 
          write (iprt, '(1X,A)') keywrd(161:i) 
        else 
          write (iprt, '(A)') keywrd(161:i) 
        endif 
      endif 
      do i = 81, 2, -1 
        if (koment(i:i) == ' ') cycle  
        exit  
      end do 
      if (index(koment,' NULL ') == 0) write (iprt, '(A)') koment(:i) 
      do i = 81, 2, -1 
        if (title(i:i) == ' ') cycle  
        exit  
      end do 
      if (index(title,' NULL ') == 0) write (iprt, '(A)') title(:i) 
      return  
      end subroutine wrttxt 
