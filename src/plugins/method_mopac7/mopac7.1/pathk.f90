      subroutine pathk() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : iflepo, numat, keywrd, nvar, tleft, escf, &
      jobnam
      use maps_C, only : currt, lparam, react, latom, kloop
      use permanent_arrays, only : geo, xparam, profil
      use chanel_C, only : iw, ires, iarc
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
      use geout_I 
      use wrttxt_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(20) :: mdfp 
      integer :: npts, maxcyc, i, lloop, iloop, l, m, k 
      real(double), dimension(3*numat) :: gd, xlast 
      real(double), dimension(20) :: xdfp 
      real(double) :: step, degree, c1, cputot, cpu1, cpu2, cpu3, stepc1 
      logical :: useef, opend 
!-----------------------------------------------
      useef = index(keywrd,' TS') + index(keywrd,' EF') /= 0 
      step = reada(keywrd,index(keywrd,'STEP') + 5) 
      npts = nint(reada(keywrd,index(keywrd,'POINT') + 6)) 
!
!  THE SMALLEST VALUE IN THE PATH IS
!      REACT(1) DEGREE OR GEO(LPARAM,LATOM) RADIANS
!
      degree = 57.29577951308232D0 
      if (lparam /= 1) then 
        step = step/degree 
        c1 = degree 
      else 
        c1 = 1.D0 
      endif 
!
      kloop = 1 
      maxcyc = 100000 
      if (index(keywrd,' BIGCYCLES') /= 0) maxcyc = nint(reada(keywrd,index(&
        keywrd,' BIGCYCLES'))) 
      cputot = 0.0D0 
      currt = geo(lparam,latom) 
      profil(1) = 0.D0 
      if (useef) then 
        write (iw, '(''  ABOUT TO ENTER EF FROM PATHK'')') 
        if (index(keywrd,'RESTAR') /= 0) then 
          i = index(jobnam,' ') - 1 
          open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
          rewind ires 
          read (ires, err=60) kloop 
          read (ires, err=60) currt 
          read (ires, err=60) (profil(i),i=1,kloop) 
          write (iw, '(2/10X,'' RESTARTING AT POINT'',I3)') kloop 
        endif 
      else 
        write (iw, '(''  ABOUT TO ENTER FLEPO FROM PATHK'')') 
        if (index(keywrd,'RESTAR') /= 0) then 
          mdfp(9) = 0 
          gd = 0.d0
          xlast = 0.d0
          xdfp = 0.d0
          call dfpsav (cputot, xparam, gd, xlast, escf, mdfp, xdfp) 
          write (iw, '(2/10X,'' RESTARTING AT POINT'',I3)') kloop 
        endif 
      endif 
!
      geo(lparam,latom) = currt 
      lloop = kloop 
      do iloop = kloop, npts 
        if (iloop - lloop >= maxcyc) tleft = -100.D0 
        cpu1 = second(2) 
        currt = geo(lparam,latom) 
        if (useef) then 
          call ef (xparam, nvar, escf) 
        else 
          call flepo (xparam, nvar, escf) 
        endif 
        if (iflepo == (-1)) return  
        kloop = kloop + 1 
        cpu2 = second(2) 
        cpu3 = cpu2 - cpu1 
        cputot = cputot + cpu3 
        profil(iloop) = escf 
        write (iw, '(/''          VARIABLE        FUNCTION'')') 
        write (iw, '('' :'',F16.5,F16.6)') geo(lparam,latom)*c1, escf 
        call geout (iw) 
        geo(lparam,latom) = geo(lparam,latom) + step 
      end do 
      react(1) = react(1)*c1 
      stepc1 = step*c1 
      do i = 2, npts 
        react(i) = react(i-1) + stepc1 
      end do 
      write (iw, &
      '(/16X,''POINTS ON REACTION PATH '',                               /16X,'&
      &'AND CORRESPONDING HEATS'',2/)') 
      inquire(unit=iarc, opened=opend) 
      if (opend) close(unit=iarc, status='KEEP') 
      i = index(jobnam,' ') - 1 
      open(unit=iarc, file=jobnam(:i)//'.arc', status='UNKNOWN', position=&
        'asis') 
      write (iarc, 30) 
      call wrttxt (iarc) 
   30 format(' ARCHIVE FILE FOR PATH CALCULATION'/,&
        'A PROFIL OF COORDINATES - HEATS'/) 
      write (iarc, '(/'' TOTAL CPU TIME : '',F10.3/)') cputot 
!
      l = npts/8 
      m = npts - l*8 
      if (l >= 1) then 
        do k = 0, l - 1 
          write (iw, '(8F7.2)') (react(i),i=k*8 + 1,k*8 + 8) 
          write (iw, '(8F7.2,/)') (profil(i),i=k*8 + 1,k*8 + 8) 
          write (iarc, '(8F7.2)') (react(i),i=k*8 + 1,k*8 + 8) 
          write (iarc, '(8F7.2,/)') (profil(i),i=k*8 + 1,k*8 + 8) 
        end do 
      endif 
      if (m > 0) then 
        write (iw, '(8F7.2)') (react(i),i=l*8 + 1,l*8 + m) 
        write (iw, '(8F7.2,/)') (profil(i),i=l*8 + 1,l*8 + m) 
        write (iarc, '(8F7.2)') (react(i),i=l*8 + 1,l*8 + m) 
        write (iarc, '(8F7.2,/)') (profil(i),i=l*8 + 1,l*8 + m) 
      endif 
      return  
   60 continue 
      write (iw, '(A,I3,A)') ' ERROR DETECTED DURING READ FROM CHANNEL', ires, &
        ' IN SUBROUTINE PATHK' 
      call mopend ('ERROR DETECTED DURING READ IN SUBROUTINE PATHK') 
      return  
      end subroutine pathk 
