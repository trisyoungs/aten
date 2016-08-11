      subroutine gettxt 
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:18  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use upcase_I 
      use mopend_I 
      use chanel_C, only: ir, iw, isetup
      use molkst_C, only: keywrd, koment, title 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      
      integer , dimension(3) :: is 
      integer :: i, j 
      character :: ch, ch2, filen*50, oldkey*80 
!-----------------------------------------------
 
      is(1) = 161 
      is(2) = 81 
      is(3) = 1 
!
!  Next four lines necessary for some compilers
!
      do i = 1, 81 
        koment(i:i) = ' ' 
        title(i:i) = ' ' 
      end do 
      keywrd = ' ' 
      koment = '    NULL  ' 
      title = '    NULL  ' 
      read (ir, '(A)', end=110, err=100) keywrd(:80) 
      oldkey = keywrd(:80) 
      call upcase (keywrd(1:80), 80) 
      if (index(keywrd,'SETUP') /= 0) then 
        i = index(keywrd,'SETUP=') 
        if (i /= 0) then 
          j = index(keywrd(i:),' ') 
          filen = oldkey(i+6:i+j-1) 
        else 
          filen = 'SETUP' 
        endif 
        open(unit=isetup, file=filen, status='UNKNOWN', form='FORMATTED', &
          position='asis') 
        rewind isetup 
        read (isetup, '(A)', end=50, err=50) keywrd(81:160) 
        if (keywrd(81:160) == ' ') go to 50 
        call upcase (keywrd(81:160), 80) 
        read (isetup, '(A)', end=20, err=20) keywrd(161:240) 
        call upcase (keywrd(161:240), 80) 
   20   continue 
        read (ir, '(A)', end=110, err=100) koment, title 
      else if (index(keywrd(1:80),' +') /= 0) then 
!
!  READ SECOND KEYWORD LINE
!
        read (ir, '(A)', end=110, err=100) keywrd(81:160) 
        oldkey = keywrd(81:160) 
        call upcase (keywrd(81:160), 80) 
        if (index(keywrd(81:160),'SETUP') /= 0) then 
          i = index(keywrd,'SETUP=') 
          if (i /= 0) then 
            j = index(keywrd(i:),' ') 
            filen = oldkey(i-74:i+j-80) 
          else 
            filen = 'SETUP' 
          endif 
          open(unit=isetup, file=filen, status='UNKNOWN', form='FORMATTED', &
            position='asis') 
          rewind isetup 
          read (isetup, '(A)', end=30, err=30) keywrd(161:240) 
          call upcase (keywrd(161:240), 80) 
   30     continue 
        else if (index(keywrd(81:160),' +') /= 0) then 
!
!  READ THIRD KEYWORD LINE
!
          read (ir, '(A)', end=110, err=100) keywrd(161:240) 
          call upcase (keywrd(161:240), 80) 
        endif 
!
!  READ TITLE LINE
!
        read (ir, '(A)', end=110, err=100) koment, title 
      else if (index(keywrd(:80),'&') /= 0) then 
        read (ir, '(A)', end=110, err=100) keywrd(81:160) 
        oldkey = keywrd(81:160) 
        call upcase (keywrd(81:160), 80) 
        if (index(keywrd(81:160),'SETUP') /= 0) then 
          i = index(keywrd,'SETUP=') 
          if (i /= 0) then 
            j = index(keywrd(i:),' ') 
            filen = oldkey(i-74:i+j-80) 
!               write(*,*)' <'//FILEN//'>'
!               CALL MOPEND
!      RETURN
          else 
            filen = 'SETUP' 
          endif 
          open(unit=isetup, file=filen, status='UNKNOWN', form='FORMATTED', &
            position='asis') 
          rewind isetup 
          read (isetup, '(A)', end=40, err=40) keywrd(161:240) 
          call upcase (keywrd(161:240), 80) 
          read (ir, '(A)', end=110, err=100) title 
   40     continue 
        else if (index(keywrd(81:160),'&') /= 0) then 
          read (ir, '(A)', end=110, err=100) keywrd(161:240) 
          call upcase (keywrd(161:240), 80) 
        else 
          read (ir, '(A)', end=110, err=100) title 
        endif 
      else 
        read (ir, '(A)', end=110, err=100) koment, title 
      endif 
      go to 60 
   50 continue 
      write (iw, '(A)') ' SETUP FILE MISSING, EMPTY OR CORRUPT' 
      call mopend ('SETUP FILE MISSING, EMPTY OR CORRUPT') 
      return  
   60 continue 
      do j = 1, 3 
        if (keywrd(is(j):is(j)) == ' ') cycle  
        ch = keywrd(is(j):is(j)) 
        keywrd(is(j):is(j)) = ' ' 
        do i = is(j) + 1, 239 
          ch2 = keywrd(i:i) 
          keywrd(i:i) = ch 
          ch = ch2(1:1) 
          if (keywrd(i+1:i+2) /= '  ') cycle  
          keywrd(i+1:i+1) = ch 
          go to 80 
        end do 
        write (iw, '(A,I2,A)') ' LINE', j, ' OF KEYWORDS DOES NOT HAVE ENOUGH' 
        write (iw, '(A)') ' SPACES FOR PARSING.  PLEASE CORRECT LINE.' 
        call mopend (&
      'LINE OF KEYWORDS DOES NOT HAVE ENOUGH SPACES FOR PARSING.  PLEASE CORREC&
      &T LINE.') 
        return  
   80   continue 
      end do 
      call upcase (keywrd, 241) 
      return  
  100 continue 
      write (iw, '(A)') ' ERROR IN READ OF FIRST THREE LINES' 
  110 continue 
      call mopend ('ERROR IN READ OF FIRST THREE LINES') 
      return  
      end subroutine gettxt 
