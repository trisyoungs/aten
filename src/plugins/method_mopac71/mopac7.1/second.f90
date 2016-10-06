      real(kind(0.0d0)) function second (mode) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : jobnam 
      USE chanel_C, only : iw, iend
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:48:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use geout_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mode 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i
      real(double) :: shut 
      logical :: setok, first 
      character :: x 
      real, save :: t0 = 0.0
      real :: t1

      save setok, first, shut 
!-----------------------------------------------
!******************************************************
!
!   SECOND, ON EXIT, CONTAINS THE NUMBER OF CPU SECONDS
!   SINCE THE START OF THE CALCULATION.
!
!******************************************************
      data setok/ .TRUE./  
      data shut/ 0.D0/  
      data first/ .TRUE./  
      if (first) then 
        first = .FALSE. 
!
!   CHECK TO SEE IF AN OLD `.end' FILE EXISTS.  IF IT DOES, DELETE IT.
!
        rewind iend 
        i = index(jobnam,' ') - 1 
        open(unit=iend, file=jobnam(:i)//'.end', status='UNKNOWN', position=&
          'asis') 
        read (iend, '(A)', end=20, err=20) x 
!
!   FILE EXISTS.  DELETE IT.
!
        if (ichar(x) /= (-1)) close(iend, status='DELETE') 
      endif 
    if ( mode == 0 ) call cpu_time(t0)

!**********************************************************************
!
!   NOW TO SEE IF A FILE CALLED <FILENAME>.end EXISTS, IF IT DOES
!   THEN INCREMENT CPU TIME BY 1,000,000 SECONDS.
!
!***********************************************************************
      if (mode == 22) then 
        shut = 0.D0 
        rewind iend 
        i = index(jobnam,' ') - 1 
        open(unit=iend, file=jobnam(:i)//'.end', status='UNKNOWN', position=&
          'asis') 
        read (iend, '(A)', end=10, err=10) x 
!
!          FILE EXISTS, THEREFORE INCREMENT TIME
!
        shut = 1.D7 
        if (setok) then 
          write (iw, '(3/10X,''****   JOB STOPPED BY OPERATOR   ****'')') 
          call geout (1) 
          setok = .FALSE. 
        endif 
   10   continue 
        close(iend) 
      endif 
   20 continue 
      call cpu_time(t1)
      second = dble(t1 - t0) + shut 
      return  
      end function second 
