      subroutine timout(nout, tim) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!
!     CONVERT THE TIME FROM SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
!
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:04  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nout 
      real(double) , intent(in) :: tim 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: idays, ihours, imins 
      real(double) :: mins, days, hours, secs 
!-----------------------------------------------
!
!
!
!
      days = tim/86400.0D0 
      idays = idint(days) 
      hours = (days - dble(idays))*24.0D0 
      ihours = idint(hours) 
      mins = (hours - dble(ihours))*60.0D0 
      imins = idint(mins) 
      secs = (mins - dble(imins))*60.0D0 
!
      if (idays > 1) then 
        write (nout, 10) idays, ihours, imins, secs 
      else if (idays == 1) then 
        write (nout, 20) idays, ihours, imins, secs 
      else if (ihours > 0) then 
        write (nout, 30) ihours, imins, secs 
      else if (imins > 0) then 
        write (nout, 40) imins, secs 
      else 
        write (nout, 50) secs 
      endif 
!
   10 format(10x,'COMPUTATION TIME = ',i2,1x,'DAYS',2x,i2,1x,'HOURS',1x,i2,1x,&
        'MINUTES AND',1x,f7.3,1x,'SECONDS') 
   20 format(10x,'COMPUTATION TIME = ',i2,1x,'DAY',2x,i2,1x,'HOURS',1x,i2,1x,&
        'MINUTES AND',1x,f7.3,1x,'SECONDS') 
   30 format(10x,'COMPUTATION TIME = ',i2,1x,'HOURS',1x,i2,1x,'MINUTES AND',1x,&
        f7.3,1x,'SECONDS') 
   40 format(10x,'COMPUTATION TIME = ',i2,1x,'MINUTES AND',1x,f7.3,1x,'SECONDS'&
        ) 
   50 format(10x,'COMPUTATION TIME = ',f7.3,1x,'SECONDS') 
      return  
      end subroutine timout 
