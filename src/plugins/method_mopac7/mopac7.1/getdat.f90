      subroutine getdat(input, output)
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      use molkst_C, only : natoms, jobnam
!...Translated by Pacific-Sierra Research 77to90  4.4G  22:53:42  03/15/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!----------------------------------------------- 
      use mopend_I 
      implicit none
!-----------------------------------------------
      integer, intent (in) :: input, output
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer, parameter :: from_data_set = 7
      integer :: i 
      logical :: exists 
      character :: line*80 
      external getarg
      integer, external :: iargc
      save i 
!-----------------------------------------------
!
!***********************************************************************
!
!   GETDAT READS IN ALL THE DATA USING CHANEL "from_data_set", AND WRITES IT
!   TO SCRATCH CHANNEL "input".  THIS WAY THE ORIGINAL DATA-SET IS
!   FREED UP AS SOON AS THE JOB STARTS.
!
!
!          Channel numbers are assigned as follows:
!
!    File            Channel  Name     Location in IFILES
!
!    Surface in MEP     3   <Filename>.sur        3   FORMATTED
!    Setup              4   SETUP                 4   FORMATTED
!    from_data_set      7   <Filename>.dat        2   FORMATTED
!    Input (During run)25                         5   SCRATCH
!    Output            26   <Filename>.out        6   FORMATTED
!                      27   <Filename>.           7   UNFORMATTED
!    Restart            9   <Filename>.res        9   UNFORMATTED
!    Density           10   <Filename>.den       10   UNFORMATTED
!    Log               11   <Filename>.log       11   FORMATTED
!    Archive           12   <Filename>.arc       12   FORMATTED
!    Graphics          13   <Filename>.gpt       13   UNFORMATTED
!    EXTERNAL params   14   Defined by EXTERNAL  14   FORMATTED
!    ESP restart       15   <Filename>.esr       15   UNFORMATTED
!    MEP map           15   <Filename>.mep       15   FORMATTED
!    SOL map in MEP    17   <Filename>.sol       17   FORMATTED
!    Brillouin Zone    18   <Filename>.brz       18   UNFORMATTED
!    B-table in MEP    18   <Filename>.tab       18   FORMATTED
!    Grid map          20   <Filename>.ump       20   FORMATTED
!    Electrostatic map 21   <Filename>.esp       21   FORMATTED
!    Scratch           22                        22   FORMATTED
!
!  The UNIT number is contained in IFILES.  If you want to change the
!  UNIT number, modify IFILES in this subroutine AFTER the line "C====="
!
!********************************************************************  
      if (iargc() == 0) then 
        jobnam = 'test' 
      else 
        call getarg (1, jobnam) 
      endif
!
! Check for the data set in the order: <file>.mop, <file>.dat, <file>
      line = jobnam(:len_trim(jobnam))//'.mop'
      inquire(file=line, exist=exists)
      if ( .not. exists) then
        line = jobnam(:len_trim(jobnam))//'.dat'
        inquire(file=line, exist=exists)
      end if
      if ( .not. exists) then
        line = jobnam(:len_trim(jobnam))
        inquire(file=line, exist=exists)
      end if
      if (exists) then 
        open(unit=from_data_set, file=line, status='OLD', position=&
          'asis') 
      else 
        write (output, *) ' The input data file'//jobnam(:i)//'.dat does not exist' 
        call mopend ('The input data file does not exist') 
        return  
      endif 
!
!  CLOSE UNIT IFILES(5) IN CASE IT WAS ALREADY PRE-ASSIGNED.
!
      close(input) 
      open(unit=input, file=jobnam(:len_trim(jobnam))//'.temp', status='UNKNOWN', &
        position='asis') 
      rewind input 
      rewind from_data_set 
      i = 0 
   20 continue 
      read (from_data_set, '(A80)', end=30, err=30) line 
      i = i + 1 
      if (line(1:1) /= '*') write (input, '(A80)') line 
      go to 20 
   30 continue 
      line = ' ' 
      write (input, '(A80)') line 
      rewind input 
      if (i < 3) then 
        write (output, '(A)') ' INPUT FILE MISSING OR EMPTY' 
        call mopend ('INPUT FILE MISSING OR EMPTY') 
        return  
      endif 
      natoms = i
      close(from_data_set) 
      return  
      end subroutine getdat 
 
