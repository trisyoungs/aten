      subroutine paths() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : iflepo, numat, keywrd, nvar, tleft, &
      & time0, jobnam
      use maps_C, only : lparam, react, latom
      use permanent_arrays, only : geo, xparam
      use ef_C, only : alparm, x0, x1, x2, iloop  
      use chanel_C, only : iw, ires
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:32  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use dfpsav_I 
      use second_I 
      use ef_I 
      use flepo_I 
      use writmo_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(20) :: mdfp 
      integer :: maxcyc, i, j, lpr 
      real(double), dimension(3*numat) :: gd, xlast 
      real(double), dimension(20) :: xdfp 
      real(double) :: totime, funct1, funct, rnord, x3, c3, cc1, cc2, cb1, cb2&
        , delf0, delf1, aconst, bconst, cconst 
      logical :: lef 
      character, dimension(3) :: type*10 

      save mdfp, xdfp, type 
!-----------------------------------------------
     
!***********************************************************************
!
!   PATH FOLLOWS A REACTION COORDINATE.   THE REACTION COORDINATE IS ON
!        ATOM LATOM, AND IS A DISTANCE IF LPARAM=1,
!                           AN ANGLE   IF LPARAM=2,
!                           AN DIHEDRALIF LPARAM=3.
!
!***********************************************************************
      
      data type/ 'ANGSTROMS ', 'DEGREES   ', 'DEGREES   '/  
      iloop = 1 
      maxcyc = 100000 
      if (index(keywrd,' BIGCYCLES') /= 0) maxcyc = nint(reada(keywrd,index(&
        keywrd,' BIGCYCLES'))) 
      lef = index(keywrd,' TS') + index(keywrd,' EF') /= 0 
      if (lef) then 
        write (iw, '(''  ABOUT TO ENTER EF FROM PATH'')') 
        if (index(keywrd,'RESTAR') /= 0) then 
          i = index(jobnam,' ') - 1 
          open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
          rewind ires 
          read (ires, end=120, err=120) ((alparm(j,i),j=1,3),i=1,nvar) 
          read (ires, end=120, err=120) iloop, x0, x1, x2 
        endif 
      else 
        write (iw, '(''  ABOUT TO ENTER FLEPO FROM PATH'')') 
        if (index(keywrd,'RESTAR') /= 0) then 
          mdfp(9) = 0 
          gd = 0.d0
          xlast = 0.d0
          totime = 0.d0
          funct1 = 0.d0
          xdfp = 0.d0
          call dfpsav (totime, xparam, gd, xlast, funct1, mdfp, xdfp) 
          write (iw, '(2/10X,'' RESTARTING AT POINT'',I3)') iloop 
        endif 
      endif 
      if (iloop <= 1) then 
        time0 = second(1) 
        if (maxcyc == 0) tleft = -100.D0 
        if (lef) then 
          call ef (xparam, nvar, funct) 
        else 
          call flepo (xparam, nvar, funct) 
        endif 
        if (iflepo == (-1)) return  
        write (iw, '(''  OPTIMIZED VALUES OF PARAMETERS, INITIAL POINT'')') 
        call writmo
        time0 = second(1) 
      endif 
      if (iloop <= 2) then 
        geo(lparam,latom) = react(2) 
        if (iloop == 1) then 
          x0 = react(1) 
          x1 = x0 
          x2 = react(2) 
          if (x2 < (-100.D0)) call mopend ('Error in PATHS') 
          if (x2 < (-100.D0)) return  
          alparm(2,:nvar) = xparam(:nvar) 
          alparm(1,:nvar) = xparam(:nvar) 
          iloop = 2 
        endif 
        if (maxcyc == 1) tleft = -100.D0 
        if (lef) then 
!        call ef (xparam, nvar, funct) 
        else 
          call flepo (xparam, nvar, funct) 
        endif 
        if (iflepo == (-1)) return  
        rnord = react(2) 
        if (lparam > 1) rnord = rnord*57.29577951308232D0 
        write (iw, &
      '(1X,16(''*****''),2/17X,''REACTION COORDINATE = ''       ,F12.4,2X,A10,1&
      &9X,2/1X,16(''*****''))') rnord, type(lparam) 
        call writmo 
        time0 = second(1) 
        alparm(3,:nvar) = xparam(:nvar) 
!
!   NOW FOR THE MAIN INTERPOLATION ROUTE
!
        if (iloop == 2) iloop = 3 
      endif 
      lpr = iloop 
      do iloop = lpr, 100 
        if (iloop - lpr > maxcyc - 3) tleft = -100.D0 
!
        if (react(iloop) < (-100.D0)) return  
!
        rnord = react(iloop) 
        if (lparam > 1) rnord = rnord*57.29577951308232D0 
        write (iw, &
      '(1X,16(''*****''),2/19X,''REACTION COORDINATE = ''    ,F12.4,2X,A10,19X,&
      &2/1X,16(''*****''))') rnord, type(lparam) 
!
        x3 = react(iloop) 
        c3 = (x0**2 - x1**2)*(x1 - x2) - (x1**2 - x2**2)*(x0 - x1) 
!      WRITE(IW,'(''   C3:'',F13.7)')C3
        if (abs(c3) < 1.D-8) then 
!
!    WE USE A LINEAR INTERPOLATION
!
          cc1 = 0.D0 
          cc2 = 0.D0 
        else 
!    WE DO A QUADRATIC INTERPOLATION
!
          cc1 = (x1 - x2)/c3 
          cc2 = (x0 - x1)/c3 
        endif 
        cb1 = 1.D0/(x1 - x2) 
        cb2 = (x1**2 - x2**2)*cb1 
!
!    NOW TO CALCULATE THE INTERPOLATED COORDINATES
!
        do i = 1, nvar 
          delf0 = alparm(1,i) - alparm(2,i) 
          delf1 = alparm(2,i) - alparm(3,i) 
          aconst = cc1*delf0 - cc2*delf1 
          bconst = cb1*delf1 - aconst*cb2 
          cconst = alparm(3,i) - bconst*x2 - aconst*x2**2 
          xparam(i) = cconst + bconst*x3 + aconst*x3**2 
          alparm(1,i) = alparm(2,i) 
          alparm(2,i) = alparm(3,i) 
        end do 
!
!   NOW TO CHECK THAT THE GUESSED GEOMETRY IS NOT TOO ABSURD
!
        do i = 1, nvar 
          if (abs(xparam(i)-alparm(3,i)) > 0.2D0) go to 70 
        end do 
!
!   NOW TO CHECK THAT THE GUESSED GEOMETRY IS NOT TOO ABSURD
!
        go to 90 
   70   continue 
        write (iw, &
      '('' GEOMETRY TOO UNSTABLE FOR EXTRAPOLATION '',      ''TO BE USED'',/,  &
      &                                               '' - THE LAST GEOMETRY IS &
      &BEING USED TO START THE NEXT''          ,'' CALCULATION'')') 
        xparam(:nvar) = alparm(3,:nvar) 
   90   continue 
        x0 = x1 
        x1 = x2 
        x2 = x3 
        geo(lparam,latom) = react(iloop) 
        if (lef) then 
   !       call ef (xparam, nvar, funct) 
        else 
          call flepo (xparam, nvar, funct) 
        endif 
        if (iflepo == (-1)) return  
        call writmo 
        time0 = second(1) 
        alparm(3,:nvar) = xparam(:nvar) 
      end do 
      return  
  120 continue 
      write (iw, '(2/10X,''Restart file is corrupt!'')') 
      call mopend ('Restart file is corrupt!') 
      return  
      end subroutine paths 
