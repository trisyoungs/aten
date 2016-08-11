      subroutine wrtkey(keywrd) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : tleft, tdump, numcal, uhf, is_PARAM
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use myword_I  
      use reada_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      character  :: keywrd*241 
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, maxgeo, l, ii, ilevel, ielec, method, ip1, ip2, ip 
      real(double) :: time 
      logical ::  trip, birad, exci, ci 
      character :: allkey*241, ch, chrono*7 
!-----------------------------------------------
!**********************************************************************
!
!  WRTKEY CHECKS ALL KEY-WORDS AND PRINTS THOSE IT RECOGNIZES.  IF IT
!  FINDS A WORD IT DOES NOT RECOGNIZE THE PROGRAM WILL BE STOPPED.
!
!********************************************************************** 
!     LOGICAL MNDO
      allkey = keywrd 
!    DUMMY IF STATEMENT TO REMOVE AMPERSAND AND PLUS SIGNS, IF PRESENT
      if (myword(allkey(160:),' SETUP')) i = 1 
      if (myword(allkey,'&')) i = 2 
      if (myword(allkey,' +')) i = 3 
      if (myword(allkey,'AUTHOR')) then 
        write (iw, &
      '(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',/        10X,''   &
      &ORIGINAL VERSION WRITTEN IN 1983'')') 
        write (iw, &
      '(10X,''     BY JAMES J. P. STEWART AT THE'',/                 ,10X,''   &
      &  UNIVERSITY OF TEXAS AT AUSTIN'',/                     ,10X,''          &
      &AUSTIN, TEXAS, 78712'')') 
      endif 
      if (myword(allkey,'VECT')) write (iw, 270) 
      if (myword(allkey,' EXTE')) then 
        i = index(keywrd,' EXTE') 
        j = index(keywrd(i:),'=') + i 
        i = index(keywrd(j:),' ') + j - 1 
        write (iw, 280) keywrd(j:i) 
      endif 
      maxgeo = 0 
!
!  The following code checks for the relevant keywords, but does
!  nothing else.  This reduces the number of unrecognized
!  keywords.
!  The following code modified by Mike Peterson, U/Toronto Chemistry,
!  since:
!  1) it is intended to remove DEBUG keywords but then they aren't
!     listed in the output file if DEBUG is in fact used, and
!  2) it accepts invalid keywords if DEBUG is used, with no
!     checking that they are valid DEBUG keywords, and
!  3) the code either removes all the following keywords, or
!     just until the first one is found, depending on the
!     compiler and optimization level since FORTRAN does not
!     guarantee that all components of a compound expression
!     are evaluated.
!
!  The code has been changed to remove DEBUG keywords only if
!  DEBUG has not been specified, and to do so under all circumstances.
!  Note that the final IF on L is required to prevent some compilers
!  from deleting the large IF block, since otherwise L is not used.
!
      l = 0 
      if (index(keywrd,' DEBUG') == 0) then 
        if (myword(allkey,'CARTAB')) l = l + 1 
        if (myword(allkey,'CHARST')) l = l + 1 
        if (myword(allkey,'DCART')) l = l + 1 
        if (myword(allkey,'DERI1')) l = l + 1 
        if (myword(allkey,' DERI2')) l = l + 1 
        if (myword(allkey,'DERITR')) l = l + 1 
        if (myword(allkey,'DERIV')) l = l + 1 
        if (myword(allkey,'DERNVO')) l = l + 1 
        if (myword(allkey,' DIIS')) l = l + 1 
        if (myword(allkey,'FLEPO')) l = l + 1 
        if (myword(allkey,'FMAT')) l = l + 1 
        if (myword(allkey,'DFORCE')) l = l + 1 
        if (myword(allkey,'HCORE')) l = l + 1 
        if (myword(allkey,'MOLDAT')) l = l + 1 
        if (myword(allkey,' FREQCY')) l = l + 1 
        if (myword(allkey,'ITER')) l = l + 1 
        if (myword(allkey,'LINMIN')) l = l + 1 
        if (myword(allkey,'ALLVEC')) l = l + 1 
        if (myword(allkey,' MOLSYM')) l = l + 1 
        if (myword(allkey,'SYMOIR')) l = l + 1 
        if (myword(allkey,' CUTOF')) l = l + 1 
        if (myword(allkey,' GROUP')) l = l + 1 
        if (myword(allkey,'SYMTRZ')) l = l + 1 
        if (myword(allkey,' SIZE ')) l = l + 1 
        if (myword(allkey,'DEN=')) l = l + 1 
        if (myword(allkey,' ROT')) l = l + 1 
      endif 
      if (l < 0) write (iw, '('' *** LOGIC ERROR IN WRTKEY ***'')') 
!  End of modified code.
      if (myword(allkey,' DENS')) write (iw, 290) 
      if (myword(allkey,' SPIN')) write (iw, 300) 
      if (myword(allkey,' DEP ')) write (iw, 310) 
      if (myword(allkey,' VELO')) write (iw, 320) 
      if (myword(allkey,' TIMES')) write (iw, 330) 
      if (myword(allkey,' PARASOK')) write (iw, 340) 
      if (myword(allkey,' NODIIS')) write (iw, 350) 
      if (myword(allkey,' NOTHIEL')) write (iw, 360) 
      if (myword(allkey,' BONDS')) write (iw, 370) 
      if (myword(allkey,' GEO-OK')) write (iw, 380) 
      if (myword(allkey,' VDW')) write (iw, 390) 
      if (myword(allkey,' FOCK')) write (iw, 400) 
      if (myword(allkey,' LARGE')) write (iw, 410) 
      if (myword(allkey,' NOLOG')) write (iw, 420) 
      if (myword(allkey,' GREENF')) write (iw, 430) 
      if (myword(allkey,' AIGIN')) write (iw, 440) 
      if (myword(allkey,' AIGOUT')) write (iw, 450) 
      if (myword(allkey,' AIDER')) write (iw, 460) 
      if (myword(allkey,' S1978')) write (iw, 470) 
      if (myword(allkey,' SI1978')) write (iw, 480) 
      if (myword(allkey,' GRAP')) write (iw, 490) 
      if (myword(allkey,' NOANCI')) write (iw, 510) 
      if (myword(allkey,' 1ELEC')) write (iw, 500) 
      if (myword(allkey(:162),' SETUP')) write (iw, 540) 
      if (myword(allkey,' NOMM')) write (iw, 530) 
      if (myword(allkey,' MMOK')) write (iw, 550) 
      if (myword(allkey,' INTERP')) write (iw, 560) 
      if (myword(allkey,' SUPER')) write (iw, 570) 
      if (myword(allkey,' ESR')) write (iw, 520) 
      if (myword(allkey,' DFP')) write (iw, 580) 
      if (myword(allkey,' ANALYT')) write (iw, 590) 
      if (myword(allkey,' MECI')) write (iw, 600) 
      if (myword(allkey,' LOCAL')) write (iw, 640) 
      if (myword(allkey,' MULLIK')) write (iw, 650) 
      if (myword(allkey,' XYZ')) write (iw, 660) 
      if (myword(allkey,' PI')) write (iw, 670) 
      if (myword(allkey,' ECHO')) write (iw, 680) 
      if (myword(allkey,' FIELD')) write (iw, 690) 
      if (myword(allkey,' PECI')) write (iw, 1050) 
      if (myword(allkey,' CIS ')) write (iw, 1060) 
      if (myword(allkey,' CISD ')) write (iw, 1070) 
      if (myword(allkey,' CISDT ')) write (iw, 1080) 
      if (myword(allkey,' SING')) write (iw, 1090) 
      if (myword(allkey,' DOUB')) write (iw, 1100) 
      if (myword(allkey,' QUAR')) write (iw, 1120) 
      if (myword(allkey,' QUIN')) write (iw, 1130) 
      if (myword(allkey,' SEXT')) write (iw, 1140) 
      if (myword(allkey,' SEPT')) write (iw, 1142) 
      if (myword(allkey,' H-PRIO')) write (iw, 700) 
      if (myword(allkey,' X-PRIO')) write (iw, 710) 
      if (myword(allkey,' T-PRIO')) write (iw, 720) 
      if (myword(allkey,' COMPFG')) write (iw, 740) 
      if (myword(allkey,' POLAR')) write (iw, 730) 
      if (myword(allkey,' DEBUG ')) write (iw, 750) 
      if (myword(allkey,' RESTART')) write (iw, 760) 
!
!     KEYWORDS ADDED FOR ESP MOPAC
!
      if (myword(allkey,' ESP ')) write (iw, 770) 
      if (myword(allkey,' NSURF')) write (iw, 780) 
      if (myword(allkey,' SCALE')) write (iw, 790) 
      if (myword(allkey,' SCINCR')) write (iw, 800) 
      if (myword(allkey,' SLOPE')) write (iw, 810) 
      if (myword(allkey,' DIPOLE')) write (iw, 820) 
      if (myword(allkey,' DIPX')) write (iw, 830) 
      if (myword(allkey,' DIPY')) write (iw, 840) 
      if (myword(allkey,' DIPZ')) write (iw, 850) 
      if (myword(allkey,' CONNOLLY')) write (iw, 860) 
      if (myword(allkey,' ESPRST')) write (iw, 870) 
      if (myword(allkey,' POTWRT')) write (iw, 880) 
!
!                       KEYWORDS ADDED FOR PMEP BY BINGZE WANG AUG 1993
!
      if (myword(allkey,' PMEP ')) write (iw, 890) 
      if (myword(allkey,' PMEPR ')) write (iw, 900) 
      if (myword(allkey,' PRTMEP')) write (iw, 920) 
      if (myword(allkey,' MINMEP')) write (iw, 930) 
      if (myword(allkey,' QPMEP ')) then 
        write (iw, 910) 
        if (myword(allkey,' WILLIAMS')) write (iw, 940) 
      endif 
!                                       END OF PMEP ADDITION BY B WANG
!
      if (myword(allkey,' WILLIAMS')) write (iw, 890) 
      if (myword(allkey,' SYMAVG')) write (iw, 950) 
      if (myword(allkey,' STO3G')) write (iw, 960) 
      if (myword(allkey,' IUPD')) then 
        ii = nint(reada(keywrd,index(keywrd,' IUPD='))) 
        if (ii == 0) write (iw, 90) 
        if (ii == 1) write (iw, 100) 
        if (ii == 2) write (iw, 110) 
      endif 
      if (myword(allkey,' HESS')) then 
        ii = nint(reada(keywrd,index(keywrd,' HESS='))) 
        if (ii == 0) write (iw, 120) 
        if (ii == 1) write (iw, 130) 
        if (ii == 2) write (iw, 140) 
        if (ii == 3) write (iw, 150) 
      endif 
      if (myword(allkey,' RSCAL')) write (iw, 170) 
      if (myword(allkey,' MODE')) write (iw, 160) nint(reada(keywrd,index(&
        keywrd,'MODE='))) 
      if (myword(allkey,' RECALC')) write (iw, 180) nint(reada(keywrd,index(&
        keywrd,'RECALC'))) 
      if (myword(allkey,' RMAX')) write (iw, 210) reada(keywrd,index(keywrd,&
        ' RMAX=')) 
      if (myword(allkey,' RMIN')) write (iw, 230) reada(keywrd,index(keywrd,&
        ' RMIN=')) 
      if (myword(allkey,' DDMAX')) write (iw, 220) reada(keywrd,index(keywrd,&
        ' DDMAX=')) 
      if (myword(allkey,' DDMIN')) write (iw, 240) reada(keywrd,index(keywrd,&
        ' DDMIN=')) 
      if (myword(allkey,' DMAX')) write (iw, 190) reada(keywrd,index(keywrd,&
        'DMAX=')) 
      if (myword(allkey,' OMIN')) write (iw, 200) reada(keywrd,index(keywrd,&
        'OMIN=')) 
      if (myword(allkey,' MS=')) write (iw, 250) nint(reada(keywrd,index(keywrd&
        ,' MS='))) 
      if (myword(allkey,' PRNT')) write (iw, 260) 
      if (myword(allkey,' IRC=')) then 
        maxgeo = 1 
        write (iw, 970) nint(reada(keywrd,index(keywrd,' IRC='))) 
      else if (myword(allkey,' IRC')) then 
        maxgeo = 1 
        write (iw, 980) 
      endif 
      if (myword(allkey,' CHARGE')) write (iw, 990) nint(reada(keywrd,index(&
        keywrd,' CHARGE'))) 
      if (myword(allkey,' GRAD')) write (iw, 1000) 
      uhf = myword(allkey,' UHF') 
      if (uhf) write (iw, 1010) 
      birad = myword(allkey,' BIRAD') 
      if (birad) write (iw, 1030) 
      exci = myword(allkey,' EXCITED') 
      if (exci) write (iw, 1040) 
      trip = myword(allkey,' TRIP') 
      if (trip) write (iw, 1110) 
      if (myword(allkey,' NOREOR')) write (iw, 1150) 
      if (myword(allkey,' SYMM')) write (iw, 1160) 
      if (myword(allkey,' NOSYM')) write (iw, 1170) 
      if (myword(allkey,' AUTOSYM')) write (iw, 1180) 
      if (myword(allkey,' OPEN(')) then 
        i = index(keywrd,' OPEN(') 
        j = index(keywrd(i:i+10),',') + i - 1 
        ilevel = nint(reada(keywrd,j)) 
        ielec = nint(reada(keywrd,index(keywrd,'OPEN(') + 5)) 
        write (iw, 1200) ielec, ilevel 
      endif 
      if (myword(allkey,' MICROS')) write (iw, 1190) int(reada(keywrd,index(&
        keywrd,' MICROS'))) 
      if (myword(allkey,' DRC=')) then 
        maxgeo = 1 
        write (iw, 620) reada(keywrd,index(keywrd,' DRC=')) 
      else if (myword(allkey,' DRC')) then 
        maxgeo = 1 
        write (iw, 610) 
      endif 
      if (myword(allkey,' KINE')) write (iw, 630) reada(keywrd,index(keywrd,&
        ' KINE')) 
      chrono = 'SECONDS' 
      time = 1 
      if (myword(allkey,' T=')) then 
        i = index(keywrd,' T=') 
        tleft = reada(keywrd,i) 
        do j = i + 3, 241 
          if (j/=241 .and. keywrd(j+1:j+1)/=' ') cycle  
          ch = keywrd(j:j) 
          if (ch == 'M') then 
            chrono = 'MINUTES' 
            time = 60 
          endif 
          if (ch == 'H') then 
            chrono = 'HOURS' 
            time = 3600 
          endif 
          if (ch == 'D') then 
            chrono = 'DAYS' 
            time = 86400 
          endif 
          exit  
        end do 
        if (tleft < 99999.9D0) then 
          write (iw, 1210) tleft, chrono 
        else 
          write (iw, 1220) tleft, chrono 
        endif 
!
!  Limit time to 9,999,999 seconds =115.74 days.
!
        tleft = min(1.D7 - 1.D0,tleft*time) 
      else if (numcal == 1) then 
        tleft = 3600 
        write (iw, 1210) tleft, 'SECONDS' 
      else 
        write (iw, 1210) tleft, 'SECONDS' 
      endif 
      time = 1 
      chrono = 'SECONDS' 
      if (myword(allkey,' DUMP')) then 
        i = index(keywrd,' DUMP') 
        tdump = reada(keywrd,i) 
        do j = i + 6, 241 
          if (j/=241 .and. keywrd(j+1:j+1)/=' ') cycle  
          ch = keywrd(j:j) 
          if (ch == 'M') then 
            chrono = 'MINUTES' 
            time = 60.D0 
          endif 
          if (ch == 'H') then 
            chrono = 'HOURS' 
            time = 3600.D0 
          endif 
          if (ch == 'D') then 
            chrono = 'DAYS' 
            time = 86400.D0 
          endif 
          exit  
        end do 
        if (tdump < 99999.9D0) then 
          write (iw, 1230) tdump, chrono 
        else 
          write (iw, 1240) tdump, chrono 
        endif 
        tdump = tdump*time 
      else if (numcal == 1) then 
        tdump = 3600
        write (iw, 1230) tdump, 'SECONDS' 
      else 
        write (iw, 1230) tdump, 'SECONDS' 
      endif 
      if (myword(allkey,' 1SCF')) write (iw, 1250) 
      ci = myword(allkey,' C.I.') 
      if (ci) then 
        j = index(keywrd,' C.I.=(') 
        if (j /= 0) then 
          j = index(keywrd(j:j+10),',') + j - 1 
          write (iw, 1270) int(reada(keywrd,j)), int(reada(keywrd,index(keywrd,&
            'C.I.=(') + 5)) 
        else 
          write (iw, 1260) int(reada(keywrd,index(keywrd,'C.I.') + 5)) 
        endif 
      endif 
      if (myword(allkey,' FORCE')) then 
        write (iw, 1280) 
        maxgeo = maxgeo + 1 
      endif 
      if (myword(allkey,' EF')) then 
        write (iw, 70) 
        maxgeo = maxgeo + 1 
      endif 
      if (myword(allkey,' TS')) then 
        write (iw, 80) 
        maxgeo = maxgeo + 1 
      endif 
      method = 0  
      if (myword(allkey,' AM1')) then 
        write (iw, 1300) 
        method = method + 1 
      endif 
      if (myword(allkey,' PM3')) then 
        write (iw, 1310) 
        method = method + 1 
      endif 
      if (myword(allkey,' MNDO')) method = method + 1 
      if (myword(allkey,' OLDGEO')) write (iw, 1330) 
      if (myword(allkey,' OLDFPC')) write (iw, 1340) 
      if (myword(allkey,' PREC')) write (iw, 1320) 
      if (myword(allkey,' NOINT')) write (iw, 1350) 
      if (myword(allkey,' ISOTOPE')) write (iw, 1360) 
      if (myword(allkey,' DENOUT')) write (iw, 1370) 
      if (myword(allkey,' SHIFT')) write (iw, 1380) reada(keywrd,index(keywrd,&
        ' SHIFT')) 
      if (myword(allkey,' OLDENS')) write (iw, 1390) 
      if (myword(allkey,' SCFCRT')) write (iw, 1400) reada(keywrd,index(keywrd,&
        ' SCFCRT')) 
      if (myword(allkey,' ENPART')) write (iw, 1410) 
      if (myword(allkey,' NOXYZ')) write (iw, 1420) 
      if (myword(allkey,' SIGMA')) then 
        write (iw, 1430) 
        maxgeo = maxgeo + 1 
      endif 
      if (myword(allkey,' NLLSQ')) then 
        write (iw, 1440) 
        maxgeo = maxgeo + 1 
      endif 
      if (myword(allkey,' ROOT')) write (iw, 1450) nint(reada(keywrd,index(&
        keywrd,' ROOT'))) 
      if (myword(allkey,' TRANS=')) then 
        write (iw, 1470) nint(reada(keywrd,index(keywrd,' TRANS='))) 
      else if (myword(allkey,' TRANS')) then 
        write (iw, 1460) 
      endif 
      if (myword(allkey,' SADDLE')) write (iw, 1480) 
      if (myword(allkey,' LET')) write (iw, 1490) 
      if (myword(allkey,' COMPFG')) write (iw, 1530) 
      if (myword(allkey,' GNORM')) write (iw, 1540) reada(keywrd,index(keywrd,&
        ' GNORM')) 
      if (myword(allkey,' PULAY')) write (iw, 1550) 
      if (myword(allkey,' STEP1')) write (iw, 1560) reada(keywrd,index(keywrd,&
        'STEP1') + 6) 
      if (myword(allkey,' STEP2')) write (iw, 1570) reada(keywrd,index(keywrd,&
        'STEP2') + 6) 
      if (myword(allkey,' STEP')) write (iw, 1800) reada(keywrd,index(keywrd,&
        'STEP') + 5) 
      if (myword(allkey,' MERS')) write (iw, 1810) nint(reada(keywrd,index(&
        keywrd,' MERS'))) 
      if (myword(allkey,' POINT1')) then 
        ip1 = nint(reada(keywrd,index(keywrd,'POINT1') + 7)) 
        write (iw, 1580) ip1 
      endif 
      if (myword(allkey,' POINT2')) then 
        ip2 = nint(reada(keywrd,index(keywrd,'POINT2') + 7)) 
        write (iw, 1590) ip2 
      endif 
      if (myword(allkey,' MAX')) write (iw, 1600) 
      if (myword(allkey,' POINT')) then 
        ip = nint(reada(keywrd,index(keywrd,'POINT') + 6)) 
        write (iw, 1820) ip 
      endif 
      if (myword(allkey,' BAR')) write (iw, 1610) reada(keywrd,index(keywrd,&
        ' BAR')) 
      if (myword(allkey,' CAMP')) write (iw, 1620) 
      if (myword(allkey,' KING')) write (iw, 1620) 
      if (myword(allkey,' BCC')) write (iw, 1630) 
      if (myword(allkey,' EIGS')) write (iw, 1640) 
      if (myword(allkey,' EIGINV')) write (iw, 1650) 
      if (myword(allkey,' NONR')) write (iw, 1660) 
      if (myword(allkey,' ORIDE')) write (iw, 1670) 
      if (myword(allkey,' HYPERF')) write (iw, 1680) 
      if (myword(allkey,' PL')) write (iw, 1690) 
      if (myword(allkey,' FILL')) write (iw, 1700) nint(reada(keywrd,index(&
        keywrd,' FILL'))) 
      if (myword(allkey,' ITRY')) write (iw, 1720) nint(reada(keywrd,index(&
        keywrd,' ITRY'))) 
      if (myword(allkey,' CYCLES')) write (iw, 1730) nint(reada(keywrd,index(&
        keywrd,' CYCLES'))) 
      if (myword(allkey,' BIGCYCLES')) write (iw, 1740) nint(reada(keywrd,&
        index(keywrd,' BIGCYCLES'))) 
      if (myword(allkey,' 0SCF')) write (iw, 1760) 
      if (myword(allkey,' K=')) then 
        i = index(keywrd,' K=') 
        write (iw, 1770) 
        write (iw, 1780) reada(keywrd,i) 
        i = index(keywrd(i:),',') + i 
        write (iw, 1790) nint(reada(keywrd,i)) 
      endif 
      if (uhf) then 
        if (birad .or. exci .or. ci) then 
          write (iw, &
      '(2/10X, '' UHF USED WITH EITHER BIRAD, EXCITED OR C.I. '')') 
          write (iw, 1750) 
          go to 60 
        endif 
      else 
        if (exci .and. trip) then 
          write (iw, '(2/10X,'' EXCITED USED WITH TRIPLET'')') 
          write (iw, 1750) 
          go to 60 
        endif 
      endif 
      if (index(keywrd,' T-PRIO')/=0 .and. index(keywrd,' DRC')==0) then 
        write (iw, '(2/10X,''T-PRIO AND NO DRC'')') 
        write (iw, 1750) 
        go to 60 
      endif 
      if (method > 1) then 
        write (iw, &
      '(2/10X,'' ONLY ONE OF MNDO, AM1 AND PM3 ALLOWED'')') 
        write (iw, 1750) 
        go to 60 
      end if
      if (myword(allkey,'MEP=')) then 
        i = nint(reada(keywrd,index(keywrd,'MEP='))) 
        if (i == 1) then 
          write (iw, 1910) 
        else 
          write (iw, 1920) 
        endif 
      endif 
      if (myword(allkey,' THERMO')) write (iw, 1710) 
      if (maxgeo > 1) then 
        write (iw, &
      '(2/10X,''MORE THAN ONE GEOMETRY OPTION HAS BEEN '',  ''SPECIFIED'',/10X,&
      & ''CONFLICT MUST BE RESOLVED BEFORE JOB WILL RUN'')') 
        call mopend (&
      'MORE THAN ONE GEOMETRY OPTION HAS BEEN SPECIFIED. CONFLICT MUST BE RESOL&
      &VED BEFORE JOB WILL RUN.') 
        return  
      endif 
      if (index(keywrd,' MULLIK')/=0 .and. uhf) then 
        write (iw, '(A)') ' MULLIKEN POPULATION NOT AVAILABLE WITH UHF' 
        call mopend ('MULLIKEN POPULATION NOT AVAILABLE WITH UHF') 
        return  
      endif 
! COSMO change
! KEYWORDS FOR solvation
      if (myword(allkey,' EPS=')) write (iw, 1830) reada(keywrd,index(keywrd,&
        ' EPS')) 
      if (myword(allkey,' DIPL')) write (iw, 1850) reada(keywrd,index(keywrd,&
        ' DIPL')) 
      if (myword(allkey,' RSOLV')) write (iw, 1860) reada(keywrd,index(keywrd,&
        ' RSOLV')) 
      if (myword(allkey,' DELSC')) write (iw, 1900) reada(keywrd,index(keywrd,&
        ' DELSC')) 
      if (myword(allkey,' DISEX')) write (iw, 1890) reada(keywrd,index(keywrd,&
        ' DISEX')) 
      if (myword(allkey,'N**2')) write (iw, 1840) reada(keywrd,4 + index(keywrd&
        ,'N**2')) 
      if (myword(allkey,' NSPA')) write (iw, 1870) nint(reada(keywrd,index(&
        keywrd,' NSPA'))) 
      if (myword(allkey,' ROTX')) write (iw, 1880) nint(reada(keywrd,index(&
        keywrd,' ROTX'))) 
! end of COSMO change
      if (allkey /= ' ' .and. .not. is_PARAM) then 
        j = 0 
        do i = 1, 240 
          if (allkey(i:i)==' ' .and. allkey(i:i+1)=='  ') cycle  
          j = j + 1 
          ch = allkey(i:i) 
          allkey(j:j) = ch 
        end do 
        if (allkey(241:241) /= ' ') then 
          j = j + 1 
          ch = allkey(241:241) 
          allkey(j:j) = ch 
        endif 
        j = max(1,j) 
        l = index(keywrd,' DEBUG') 
        if (l /= 0) then 
          write (iw, '('' *  DEBUG KEYWORDS USED:  '',A)') allkey(:j) 
        else 
          write (iw, '(3/10X,''UNRECOGNIZED KEY-WORDS: ('',A,'')'')') allkey(:j&
            ) 
          write (iw, &
      '(3/10X,''CALCULATION STOPPED TO AVOID'',         '' WASTING TIME.'')') 
          write (iw, &
      '(3/10X,''IF THESE ARE DEBUG KEYWORDS, '',        ''ADD THE KEYWORD "DEBU&
      &G"'')') 
          call mopend (&
      'UNRECOGNIZED KEY-WORDS. CALCULATION STOPPED TO AVOID WASTING TIME. IF TH&
      &ESE ARE DEBUG KEYWORDS, ADD THE KEYWORD "DEBUG".') 
          return  
        endif 
      endif 
      if (index(keywrd,' SAFE')/=0 .and. index(keywrd,' UNSAFE')/=0) then 
        write (iw, '(/,A,/)') &
          '              USE EITHER `SAFE'' OR `UNSAFE'', BUT NOT BOTH' 
        call mopend ('USE EITHER "SAFE" OR "UNSAFE", BUT NOT BOTH') 
      endif 
      return  
   60 continue 
      write (iw, '(2/10X,'' CALCULATION ABANDONED, SORRY!'')') 
      call mopend ('CALCULATION ABANDONED, SORRY!') 
      return  
! ***********************************************************
! ***********************************************************
   70 format(' *  EF       - USE EF ROUTINE FOR MINIMUM SEARCH') 
   80 format(' *  TS       - USE EF ROUTINE FOR TS SEARCH') 
   90 format(' *  IUPD=0   - HESSIAN WILL NOT BE UPDATED') 
  100 format(' *  IUPD=1   - HESSIAN WILL BE UPDATED USING POWELL') 
  110 format(' *  IUPD=2   - HESSIAN WILL BE UPDATED USING BFGS') 
  120 format(' *  HESS=0   - DIAGONAL HESSIAN USED AS INITIAL GUESS') 
  130 format(' *  HESS=1   - INITIAL HESSIAN WILL BE CALCULATED') 
  140 format(' *  HESS=2   - INITIAL HESSIAN READ FROM DISK') 
  150 format(' *  HESS=3   - INITIAL HESSIAN READ FROM INPUT') 
  160 format(' *  MODE=    - FOLLOW HESSIAN MODE',i3,' TOWARD TS') 
  170 format(' *  RSCAL    - SCALE P-RFO STEP IN EF TO TRUST RADIUS') 
  180 format(' *  RECALC=  - DO',i4,' CYCLES BETWEEN HESSIAN RECALC') 
  190 format(' *  DMAX=    - STARTING TRUST RADIUS',f7.3,' ANG/RAD') 
  200 format(' *  OMIN=    - MINIMUM EIGENVECTOR OVERLAP IN TS',f7.3) 
  210 format(' *  RMAX=    - MAX. CALC./PRED. ENERGY STEP IN TS',f7.3) 
  220 format(' *  DDMAX=   - MAXIMUM TRUST RADIUS',f7.3,' ANG/RAD') 
  230 format(' *  RMIN=    - MIN. CALC./PRED. ENERGY STEP IN TS',f7.3) 
  240 format(' *  DDMIN=   - MINIMUM TRUST RADIUS',f7.3,' ANG/RAD') 
  250 format(' *  MS=      - IN MECI, MAGNETIC COMPONENT OF SPIN =',i3) 
  260 format(' *  PRNT     - EXTRA PRINTING IN EF ROUTINE') 
! ***********************************************************
  270 format(' *  VECTORS  - FINAL EIGENVECTORS TO BE PRINTED') 
  280 format(' *  EXTERNAL - USE ATOMIC PARAMETERS FROM THE FOLLOWING ','FILE',&
        /,15x,a) 
  290 format(' *  DENSITY  - FINAL DENSITY MATRIX TO BE PRINTED') 
  300 format(' *  SPIN     - FINAL UHF SPIN MATRIX TO BE PRINTED') 
  310 format(' *  DEP      - OUTPUT FORTRAN CODE FOR BLOCK-DATA') 
  320 format(' *  VELOCITY - INPUT STARTING VELOCITIES FOR DRC') 
  330 format(' *  TIMES    - TIMES OF VARIOUS STAGES TO BE PRINTED') 
  340 format(' *  PARASOK  - USE SOME MNDO PARAMETERS IN AN AM1 CALCULA','TION'&
        ) 
  350 format(' *  NODIIS   - DO NOT USE GDIIS GEOMETRY OPTIMIZER') 
  360 format(' *  NOTHIEL  - DO NOT USE THIEL''S FSTMIN TECHNIQUE') 
  370 format(' *  BONDS    - FINAL BOND-ORDER MATRIX TO BE PRINTED') 
  380 format(' *  GEO-OK   - OVERRIDE INTERATOMIC DISTANCE CHECK') 
  390 format(' *  VDW(...) - VAN DER WAALS RADII IN COSMO RESET ') 
  400 format(' *  FOCK     - LAST FOCK MATRIX TO BE PRINTED') 
  410 format(' *  LARGE    - EXPANDED OUTPUT TO BE PRINTED') 
  420 format(' *  NOLOG    - SUPPRESS LOG FILE TRAIL, WHERE POSSIBLE') 
  430 format(' *  GREEN    - RUN DANOVICH''S GREEN''S FUNCTION CALCN.') 
  440 format(' *  AIGIN    - GEOMETRY MUST BE IN GAUSSIAN FORMAT') 
  450 format(' *  AIGOUT   - IN ARC FILE, INCLUDE AB-INITIO GEOMETRY') 
  460 format(' *  AIDER    - READ IN AB INITIO DERIVATIVES') 
  470 format(' *  S1978    - 1978 SULFUR PARAMETERS TO BE USED') 
  480 format(' *  SI1978   - 1978 SILICON PARAMETERS TO BE USED') 
  490 format(' *  GRAPH    - GENERATE FILE FOR GRAPHICS') 
  500 format(' *  1ELECTRON- FINAL ONE-ELECTRON MATRIX TO BE PRINTED') 
  510 format(' *  NOANCI   - DO NOT USE ANALYTICAL C.I. DERIVATIVES') 
  520 format(' *  ESR      - RHF SPIN DENSITY CALCULATION REQUESTED') 
  530 format(' *  NOMM     - DO NOT MAKE MM CORRECTION TO CONH BARRIER') 
  540 format(' *  SETUP    - EXTRA KEYWORDS TO BE READ FROM FILE SETUP') 
  550 format(' *  MMOK     - APPLY MM CORRECTION TO CONH BARRIER') 
  560 format(' *  INTERP   - PRINT DETAILS OF CAMP-KING CONVERGER') 
  570 format(' *  SUPER    - PRINT SUPERDELOCALIZABILITIES') 
  580 format(' *  DFP      - USE DAVIDON FLETCHER POWELL OPTIMIZER') 
  590 format(' *  ANALYT   - USE ANALYTIC DERIVATIVES ') 
  600 format(' *  MECI     - M.E.C.I. WORKING TO BE PRINTED') 
  610 format(' *  DRC      - DYNAMIC REACTION COORDINATE CALCULATION') 
  620 format(' *  DRC=     - HALF-LIFE FOR KINETIC ENERGY LOSS =',f9.2,&
        ' * 10**(-15) SECONDS') 
  630 format(' *  KINETIC= - ',f9.3,' KCAL KINETIC ENERGY ADDED TO DRC') 
  640 format(' *  LOCALIZE - LOCALIZED ORBITALS TO BE PRINTED') 
  650 format(' *  MULLIK   - THE MULLIKEN ANALYSIS TO BE PERFORMED') 
  660 format(' *   XYZ     - CARTESIAN COORDINATE SYSTEM TO BE USED') 
  670 format(' *   PI      - BONDS MATRIX, SPLIT INTO SIGMA-PI-DELL',&
        ' COMPONENTS, TO BE PRINTED') 
  680 format(' *  ECHO     - ALL INPUT DATA TO BE ECHOED BEFORE RUN') 
  690 format(' *  FIELD    - AN EXTERNAL ELECTRIC FIELD IS TO BE USED') 
  700 format(' *  H-PRIOR  - HEAT OF FORMATION TAKES PRIORITY IN DRC') 
  710 format(' *  X-PRIOR  - GEOMETRY CHANGES TAKE PRIORITY IN DRC') 
  720 format(' *  T-PRIOR  - TIME TAKES PRIORITY IN DRC') 
  730 format(' *  POLAR    - CALCULATE FIRST, SECOND AND THIRD-ORDER',&
        ' POLARIZABILITIES') 
  740 format(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG') 
  750 format(' *  DEBUG    - DEBUG OPTION TURNED ON') 
  760 format(' *  RESTART  - CALCULATION RESTARTED') 
!
!     KEYWORDS ADDED FOR ESP MOPAC
!
  770 format(' *  ESP      - ELECTROSTATIC POTENTIAL CALCULATION') 
  780 format(' *  NSURF    - NUMBER OF LAYERS') 
  790 format(' *  SCALE    - SCALING FACTOR FOR VAN DER WAALS DISTANCE') 
  800 format(' *  SCINCR   - INCREMENT BETWEEN LAYERS') 
  810 format(' *  SLOPE    - SLOPE - USED TO SCALE MNDO ESP CHARGES') 
  820 format(' *  DIPOLE   - FIT THE ESP TO THE CALCULATED DIPOLE') 
  830 format(' *  DIPX     - X COMPONENT OF DIPOLE TO BE FIT') 
  840 format(' *  DIPY     - Y COMPONENT OF DIPOLE TO BE FIT') 
  850 format(' *  DIPZ     - Z COMPONENT OF DIPOLE TO BE FIT') 
  860 format(' *  CONNOLLY - USE CONNOLLY SURFACE') 
  870 format(' *  ESPRST   - RESTART OF ELECTRIC POTENTIAL CALCULATION') 
  880 format(' *  POTWRT   - WRITE OUT ELECTRIC POT. DATA TO FILE 21') 
  890 format(' *  PMEP     - COMPLETE SEMIEMPIRICAL MEP CALCULATION') 
  900 format(' *  PMEPR    - COMPLETE SEMIEMPIRICAL MEP IN A PLANE TO',&
        ' BE DEFINED') 
  910 format(' *  QPMEP    - CHARGES DERIVED FROM WANG-FORD TYPE AM1',' MEP') 
  920 format(' *  PRTMEP   - MEP CONTOUR DATA OUTPUT TO <FILENAME>.mep') 
  930 format(' *  MINMEP   - MINIMIZE MEP MINIMA IN THE PLANE DEFINED') 
  940 format(' *  WILLIAMS - USE WILLIAMS SURFACE') 
  950 format(' *  SYMAVG   - AVERAGE SYMMETRY EQUIVALENT ESP CHARGES') 
  960 format(' *  STO3G    - DEORTHOGONALIZE ORBITALS IN STO-3G BASIS') 
  970 format(' *  IRC=N    - INTRINSIC REACTION COORDINATE',i3,' DEFINED') 
  980 format(' *  IRC      - INTRINSIC REACTION COORDINATE CALCULATION') 
  990 format(3(' *',/),' *',15x,'  CHARGE ON SYSTEM =',i6,3(/,' *')) 
 1000 format(' *  GRADIENTS- ALL GRADIENTS TO BE PRINTED') 
 1010 format(' *  UHF      - UNRESTRICTED HARTREE-FOCK CALCULATION') 
! 1020 format(' *  SINGLET  - STATE REQUIRED MUST BE A SINGLET') 
 1030 format(' *  BIRADICAL- SYSTEM HAS TWO UNPAIRED ELECTRONS') 
 1040 format(' *  EXCITED  - FIRST EXCITED STATE IS TO BE OPTIMIZED') 
 1050 format(' *  PECI     - SINGLE AND PAIRED ELECTRON',' EXCITATIONS ONLY') 
 1060 format(' *  CIS      - C.I. USES 1 ELECTRON EXCITATIONS ONLY') 
 1070 format(' *  CISD     - C.I. USES 1 AND 2 ELECTRON EXCITATIONS') 
 1080 format(' *  CISDT    - C.I. USES 1, 2 AND 3 ELECTRON EXCITATIONS') 
 1090 format(' *  SINGLET  - SPIN STATE DEFINED AS A SINGLET') 
 1100 format(' *  DOUBLET  - SPIN STATE DEFINED AS A DOUBLET') 
 1110 format(' *  TRIPLET  - SPIN STATE DEFINED AS A TRIPLET') 
 1120 format(' *  QUARTET  - SPIN STATE DEFINED AS A QUARTET') 
 1130 format(' *  QUINTET  - SPIN STATE DEFINED AS A QUINTET') 
 1140 format(' *  SEXTET   - SPIN STATE DEFINED AS A SEXTET') 
 1142 format(' *  SEPTET   - SPIN STATE DEFINED AS A SEPTET') 
 1150 format(&
      ' *  NOREOR   - DO NOT REORIENTATE SYSTEM WHEN WORKING OUT'' THE SYMMETRY&
      &') 
 1160 format(' *  SYMMETRY - SYMMETRY CONDITIONS TO BE IMPOSED') 
 1170 format(' *  NOSYM    - POINT-GROUP SYMMETRY SET TO C1') 
 1180 format(' *  AUTOSYM  - SYMMETRY TO BE IMPOSED AUTOMATICALLY') 
 1190 format(' *  MICROS=N -',i4,' MICROSTATES TO BE SUPPLIED FOR C.I.') 
 1200 format(' *  OPEN(N,N)- THERE ARE',i2,' ELECTRONS IN',i2,' LEVELS') 
 1210 format(' *   T=      - A TIME OF',f10.3,' ',a7,' REQUESTED') 
 1220 format(' *   T=      - A TIME OF',g11.3,' ',a7,' REQUESTED') 
 1230 format(' *  DUMP=N   - RESTART FILE WRITTEN EVERY',f10.3,' ',a7) 
 1240 format(' *  DUMP=N   - RESTART FILE WRITTEN EVERY',g11.3,' ',a7) 
 1250 format(' *  1SCF     - DO 1 SCF AND THEN STOP ') 
 1260 format(' *  C.I.=N   -',i2,' M.O.S TO BE USED IN C.I.') 
 1270 format(' * C.I.=(N,M)-',i3,' DOUBLY FILLED LEVELS USED IN A ',/,&
        ' *             C.I. INVOLVING ',i2,' M.O.''S') 
 1280 format(' *  FORCE    - FORCE CALCULATION SPECIFIED') 
 1300 format(' *  AM1      - THE AM1 HAMILTONIAN TO BE USED') 
 1310 format(' *  PM3      - THE PM3 HAMILTONIAN TO BE USED') 
 1320 format(' *  PRECISE  - CRITERIA TO BE INCREASED BY 100 TIMES') 
 1330 format(' *  OLDGEO   - PREVIOUS GEOMETRY TO BE USED') 
 1340 format(' *  OLDFPC   - OLD FUNDAMENTAL PHYSICAL CONSTANTS',' TO BE USED') 
 1350 format(' *  NOINTER  - INTERATOMIC DISTANCES NOT TO BE PRINTED') 
 1360 format(' *  ISOTOPE  - FORCE MATRIX WRITTEN TO DISK (CHAN. 9 )') 
 1370 format(' *  DENOUT   - DENSITY MATRIX OUTPUT ON CHANNEL 10') 
 1380 format(' *  SHIFT    - A DAMPING FACTOR OF',f8.2,' DEFINED') 
 1390 format(' *  OLDENS   - INITIAL DENSITY MATRIX READ OF DISK') 
 1400 format(' *  SCFCRT   - DEFAULT SCF CRITERION REPLACED BY',g12.3) 
 1410 format(' *  ENPART   - ENERGY TO BE PARTITIONED INTO COMPONENTS') 
 1420 format(' *  NOXYZ    - CARTESIAN COORDINATES NOT TO BE PRINTED') 
 1430 format(' *  SIGMA    - GEOMETRY TO BE OPTIMIZED USING SIGMA.') 
 1440 format(' *  NLLSQ    - GRADIENTS TO BE MINIMIZED USING NLLSQ.') 
 1450 format(' *  ROOT     - IN A C.I. CALCULATION, ROOT',i2,&
        ' TO BE OPTIMIZED.') 
 1460 format(' *  TRANS    - THE REACTION VIBRATION TO BE DELETED FROM',&
        ' THE THERMO CALCULATION') 
 1470 format(' *  TRANS=   - ',i4,' VIBRATIONS ARE TO BE DELETED FROM',&
        ' THE THERMO CALCULATION') 
 1480 format(' *  SADDLE   - TRANSITION STATE TO BE OPTIMIZED') 
 1490 format(' *   LET     - OVERRIDE SOME SAFETY CHECKS') 
! 1500 format(' *  SAFE     - ALLOW MEMORY INTENSIVE SCF CONVERGERS') 
 !1510 format(' *             (NOTE: THIS IS THE DEFAULT)') 
! 1520 format(' *  UNSAFE   - USE REDUCED MEMORY - MAY ',&
 !       'AFFECT SCF CONVERGANCE') 
 1530 format(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG') 
 1540 format(' *  GNORM=   - EXIT WHEN GRADIENT NORM DROPS BELOW ',g8.3) 
 1550 format(' *  PULAY    - PULAY''S METHOD TO BE USED IN SCF') 
 1560 format(' *  STEP1    - FIRST  STEP-SIZE IN GRID =',f7.2) 
 1570 format(' *  STEP2    - SECOND STEP-SIZE IN GRID =',f7.2) 
 1580 format(' *  POINT1   - NUMBER OF ROWS IN GRID =',i3) 
 1590 format(' *  POINT2   - NUMBER OF COLUMNS IN GRID =',i3) 
 1600 format(' *  MAX      - GRID SIZE 23*23 ') 
 1610 format(' *  BAR=     - REDUCE BAR LENGTH BY A MAX. OF',f7.2) 
 1620 format(' *  CAMP,KING- THE CAMP-KING CONVERGER TO BE USED') 
 1630 format(' *  BCC      - THE SYSTEM IS BODY-CENTERED CUBIC') 
 1640 format(' *  EIGS     - PRINT ALL EIGENVALUES IN ITER') 
 1650 format(' *  EIGINV   - USE HESSIAN EIGENVALUE REVERSION IN EF') 
 1660 format(' *  NONR     - DO NOT USE NEWTON-RAPHSON STEP IN EF') 
 1670 format(' *  ORIDE    - UNCONDITIONALLY, USE CALCULATED LAMDAS IN'/,/,&
        ' EF') 
 1680 format(' *  HYPERFINE- HYPERFINE COUPLING CONSTANTS TO BE',' PRINTED') 
 1690 format(' *   PL      - MONITOR CONVERGANCE IN DENSITY MATRIX') 
 1700 format(' *  FILL=    - IN RHF CLOSED SHELL, FORCE M.O.',i3,&
        ' TO BE FILLED') 
 1710 format(' *  THERMO   - THERMODYNAMIC QUANTITIES TO BE CALCULATED') 
 1720 format(' *  ITRY=    - DO A MAXIMUM OF',i6,' ITERATIONS FOR SCF') 
 1730 format(' *  CYCLES=  - DO A MAXIMUM OF',i6,' STEPS') 
 1740 format(' *  BIGCYCLES= DO A MAXIMUM OF',i6,' BIG STEPS') 
 1750 format(/,/,10x,' IMPOSSIBLE OPTION REQUESTED,') 
 1760 format(' *  0SCF     - AFTER READING AND PRINTING DATA, STOP') 
 1770 format(' *   K=      - BRILLOUIN ZONE STRUCTURE TO BE CALCULATED') 
 1780 format(' *             STEP SIZE IN SAMPLING ZONE:',f8.4) 
 1790 format(' *             NO. OF ATOMS IN FUNDAMENTAL UNIT CELL:',i6) 
 1800 format(' *  STEP     - STEP-SIZE IN PATH=',f7.3) 
 1810 format(' *  MERS=N   - NUMBER OF FUNDAMENTAL UNIT CELLS USED:',i3) 
 1820 format(' *  POINT    - NUMBER OF POINTS IN PATH=',i3) 
! KEYWORDS FOR SOLVATION
 1830 format(' *  EPS=',f7.3) 
 1840 format(' *  n**2 =',f7.3,' for COSMO-CI') 
 1850 format(' *  DIPL=',f7.3) 
 1860 format(' *  RSOLV=',f7.3) 
 1870 format(' *  NSPA=',i5) 
 1880 format(' *  ROTX=',i5) 
 1890 format(' *  DISEX=',f7.3) 
 1900 format(' *  DELSC=',f7.3) 
 1910 format(' *  MEP=1    - MEP IN A CUBIC GRID') 
 1920 format(' *  MEP=2    - MEP IN CONNOLLY SURFACE') 
       end subroutine wrtkey 
