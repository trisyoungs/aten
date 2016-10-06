      subroutine grid 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use chanel_C, only : iw, iarc, ires, iump
      use permanent_arrays, only : geo, xparam 
      use molkst_C, only : nvar, iflepo, keywrd, tleft, jobnam
      use maps_C, only : currt1, currt2, ione, ijlp, ilp, jlp, jlp1, surf, &
      lpara1, latom1, lpara2, latom2
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:20  03/09/06  
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
      implicit none 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(20) :: mdfp 
      integer :: npts1, npts2, maxcyc, i, loops, iloop, jloop, jloop1, ij, n, j 
      real(double), dimension(nvar) :: gd, xlast 
      real(double), dimension(20) :: xdfp 
      real(double), dimension(23,23) :: surfac 
      real(double), dimension(23) :: umpx, umpy 
      real(double), dimension(529) :: umpz 
      real(double) :: step1, step2, degree, start1, start2, c1, c2, cputot, &
        escf, cpu1, cpu2, cpu3 
      logical :: restrt, useef, opend 
!-----------------------------------------------
      useef = index(keywrd,' EF') /= 0 
!
      step1 = reada(keywrd,index(keywrd,'STEP1') + 6) 
      step2 = reada(keywrd,index(keywrd,'STEP2') + 6) 
      npts1 = 11 
      npts2 = 11 
      if (index(keywrd,' MAX') /= 0) then 
        npts1 = 23 
        npts2 = 23 
        go to 10 
      endif 
      maxcyc = 100000 
      if (index(keywrd,' BIGCYCLES') /= 0) maxcyc = nint(reada(keywrd,index(&
        keywrd,' BIGCYCLES'))) 
      if (index(keywrd,'POINT1') /= 0) npts1 = nint(abs(reada(keywrd,index(&
        keywrd,'POINT1') + 7))) 
      if (index(keywrd,'POINT2') /= 0) npts2 = nint(abs(reada(keywrd,index(&
        keywrd,'POINT2') + 7))) 
   10 continue 
      restrt = index(keywrd,'RESTART') /= 0 
!
!  THE TOP-LEFT VALUE OF THE FIRST AND SECOND DIMENSIONS ARE
!      GEO(LPARA1,LATOM1) AND GEO(LPARA2,LATOM2)
!
      umpy(1) = geo(lpara1,latom1) 
      umpx(1) = geo(lpara2,latom2) 
      degree = 57.29577951308232D0 
      if (lpara1 /= 1) step1 = step1/degree 
      if (lpara2 /= 1) step2 = step2/degree 
!
!  NOW SET THE STARTING POINT TO THE DESIRED CORNER
!
      if (step1>0.0D0 .and. step2>0.0D0) then 
        start1 = geo(lpara1,latom1) 
        start2 = geo(lpara2,latom2) 
      endif 
! BOTTOM-LEFT
      if (step1<0.0D0 .and. step2>0.0D0) then 
        start1 = geo(lpara1,latom1) + (npts1 - 1)*abs(step1) 
        start2 = geo(lpara2,latom2) 
      endif 
! TOP-RIGHT
      if (step1>0.0D0 .and. step2<0.0D0) then 
        start1 = geo(lpara1,latom1) 
        start2 = geo(lpara2,latom2) + abs((npts2 - 1)*step2) 
      endif 
! BOTTOM-RIGHT
      if (step1<0.0D0 .and. step2<0.0D0) then 
        start1 = geo(lpara1,latom1) + abs((npts1 - 1)*step1) 
        start2 = geo(lpara2,latom2) + abs((npts2 - 1)*step2) 
      endif 
!
!  NOW TO SWEEP THROUGH THE GRID OF POINTS LEFT TO RIGHT THEN RIGHT
!  TO LEFT OR VISA VERSA. THIS SHOULD AVOID THE GEOMETRY OR SCF GETTING
!  MESSED UP.
!
      if (lpara1 /= 1) then 
        c1 = degree 
      else 
        c1 = 1.D0 
      endif 
      if (lpara2 /= 1) then 
        c2 = degree 
      else 
        c2 = 1.D0 
      endif 
!   THESE PARAMETERS NEED TO BE DUMPED IN '.RES'
      currt1 = start1 
      currt2 = start2 
      ione = -1 
      cputot = 0.0D0 
      ijlp = 0 
      ilp = 1 
      jlp = 1 
      jlp1 = 1 
      surf(1) = 0.D0 
!
      if (restrt) then 
        if (useef) then 
          i = index(jobnam,' ') - 1 
          open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
          rewind ires 
          read (ires) ijlp, ilp, jlp, jlp1, ione 
          read (ires) currt1, currt2 
          read (ires) (surf(i),i=1,ijlp) 
        else 
          mdfp(9) = 0 
          gd = 0.d0
          xlast = 0.d0
          escf = 0.d0
          xdfp = 0.d0
          call dfpsav (cputot, xparam, gd, xlast, escf, mdfp, xdfp) 
        endif 
      endif 
!
      geo(lpara1,latom1) = currt1 
      geo(lpara2,latom2) = currt2 
      loops = 0 
      do iloop = ilp, npts1 
        ione = -ione 
        do jloop = jlp, npts2 
          loops = loops + 1 
          if (loops >= maxcyc) tleft = -100.D0 
          jloop1 = 0 
          if (ione < 0) jloop1 = npts2 + 1 
          if (restrt) then 
            jloop1 = jlp1 
            ione = -ione 
            restrt = .FALSE. 
          else 
            jloop1 = jloop1 + ione 
            jlp1 = jloop1 
          endif 
          cpu1 = second(2) 
          currt1 = geo(lpara1,latom1) 
          currt2 = geo(lpara2,latom2) 
          if (useef) then 
            call ef (xparam, nvar, escf) 
          else 
            call flepo (xparam, nvar, escf) 
          endif 
          if (iflepo == (-1)) return  
          cpu2 = second(2) 
          cpu3 = cpu2 - cpu1 
          cputot = cputot + cpu3 
          jlp = jlp + 1 
          ijlp = ijlp + 1 
          surf(ijlp) = escf 
          write (iw, &
            '(/''       FIRST VARIABLE   SECOND VARIABLE        FUNCTION'')') 
          write (iw, '('' :'',F16.5,F16.5,F16.6)') geo(lpara1,latom1)*c1, geo(&
            lpara2,latom2)*c2, escf 
          call geout (iw)
          geo(lpara2,latom2) = geo(lpara2,latom2) + step2*ione 
        end do 
        geo(lpara1,latom1) = geo(lpara1,latom1) + step1 
        geo(lpara2,latom2) = geo(lpara2,latom2) - step2*ione 
        ilp = ilp + 1 
        jlp = 1 
      end do 
      write (iw, &
      '(/10X,''HORIZONTAL: VARYING SECOND PARAMETER,'',                  /10X,'&
      &'VERTICAL:   VARYING FIRST PARAMETER'')') 
      write (iw, '(/10X,''WHOLE OF GRID, SUITABLE FOR PLOTTING'',2/)') 
!
!  ARCHIVE
      i = index(jobnam,' ') - 1 
      inquire(unit=iarc, opened=opend) 
      if (opend) close(unit=iarc, status='KEEP') 
      open(unit=iarc, file=jobnam(:i)//'.arc', status='UNKNOWN', position=&
        'asis') 
      open(unit=iump, file=jobnam(:i)//'.ump', status='UNKNOWN', position=&
        'asis') 
      write (iarc, 40) 
      call wrttxt (iarc) 
   40 format(' ARCHIVE FILE FOR GRID CALCULATION'/,'GRID OF HEATS'/) 
      write (iarc, '(/'' TOTAL CPU TIME IN FLEPO : '',F10.3/)') cputot 
!
!  WRITE OUT THE GRIDS
      ione = 1 
      iloop = 1 
      jloop1 = 1 
      do ij = 1, npts1*npts2 
        surfac(jloop1,iloop) = surf(ij) 
        n = ij - (ij/npts2)*npts2 
        if (n == 0) then 
          iloop = iloop + 1 
          jloop1 = jloop1 + ione 
          ione = -ione 
        endif 
        jloop1 = jloop1 + ione 
      end do 
!
      do i = 2, npts1 
        umpy(i) = umpy(1) + (i - 1)*abs(step1) 
      end do 
      do i = 2, npts2 
        umpx(i) = umpx(1) + (i - 1)*abs(step2) 
      end do 
      n = 0 
      if (step1>0.0D0 .and. step2>0.0D0) then 
        do i = 1, npts1 
          if (npts2 > 0) then 
            umpz(n+1:npts2+n) = surfac(:npts2,i) 
            n = npts2 + n 
          endif 
          write (iw, '(11F7.2)') (surfac(j,i),j=1,npts2) 
          write (iarc, '(11F7.2)') (surfac(j,i),j=1,npts2) 
        end do 
      endif 
      if (step1<0.0D0 .and. step2>0.0D0) then 
        do i = npts1, 1, -1 
          if (npts2 > 0) then 
            umpz(n+1:npts2+n) = surfac(:npts2,i) 
            n = npts2 + n 
          endif 
          write (iw, '(11F7.2)') (surfac(j,i),j=1,npts2) 
          write (iarc, '(11F7.2)') (surfac(j,i),j=1,npts2) 
        end do 
      endif 
      if (step1>0.0D0 .and. step2<0.0D0) then 
        do i = 1, npts1 
          if (npts2 > 0) then 
            umpz(n+1:npts2+n) = surfac(npts2:1:(-1),i) 
            n = npts2 + n 
          endif 
          write (iw, '(11F7.2)') (surfac(j,i),j=npts2,1,(-1)) 
          write (iarc, '(11F7.2)') (surfac(j,i),j=npts2,1,(-1)) 
        end do 
      endif 
      if (step1<0.0D0 .and. step2<0.0D0) then 
        do i = npts1, 1, -1 
          if (npts2 > 0) then 
            umpz(n+1:npts2+n) = surfac(npts2:1:(-1),i) 
            n = npts2 + n 
          endif 
          write (iw, '(11F7.2)') (surfac(j,i),j=npts2,1,(-1)) 
          write (iarc, '(11F7.2)') (surfac(j,i),j=npts2,1,(-1)) 
        end do 
      endif 
      do i = 0, npts1 - 1 
        do j = 1, npts2 
          n = i*npts2 + j 
          write (iump, '(3(1X,F8.3))') umpx(j), umpy(i+1), umpz(n) 
        end do 
      end do 
      close(iump) 
      return  
      end subroutine grid 
