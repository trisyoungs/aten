      subroutine thermo(a, b, c, linear, sym, wt, vibs, nvibs, escf) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use funcon_C, only : fpc_5, fpc_6, fpc_7, fpc_8, fpc_10
      use molkst_C, only : keywrd, title, koment
      use chanel_C, only : iw
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:04  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nvibs 
      real(double) , intent(in) :: a 
      real(double) , intent(in) :: b 
      real(double) , intent(in) :: c 
      real(double) , intent(in) :: sym 
      real(double) , intent(in) :: wt 
      real(double) , intent(in) :: escf 
      logical , intent(in) :: linear 
      real(double) , intent(inout) :: vibs(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: it1, it2, istep, i, ilim, itemp, ir 
      real(double), dimension(300) :: trange 
      real(double) :: r, h, ak, ac, qtr2, t, c1, qv, hv, cpv, sv1, wi, ewj, &
        ewjr, e0, sv, qr, cpr, hr, sr, qint, hint, cpint, sint, qtr, cptr, htr&
        , str, cptot, stot, htot, h298 
      character :: tmpkey*241 
!-----------------------------------------------
      r = fpc_5 
      h = fpc_6
      ak = fpc_7 
      ac = fpc_8 
      it1 = 200 
      it2 = 400 
      istep = 10 
      tmpkey = keywrd 
      i = index(tmpkey,'THERMO(') 
      if (i /= 0) then 
!
!   ERASE ALL TEXT FROM TMPKEY EXCEPT THERMO DATA
!
        tmpkey(:i) = ' ' 
        tmpkey(index(tmpkey,')'):) = ' ' 
        it1 = nint(reada(tmpkey,i)) 
        if (it1 < 100) then 
          write (iw, &
      '(2/10X,''TEMPERATURE RANGE STARTS TOO LOW,'',     '' LOWER BOUND IS RESE&
      &T TO 30K'')') 
          it1 = 100 
        endif 
        i = index(tmpkey,',') 
        if (i /= 0) then 
          tmpkey(i:i) = ' ' 
          it2 = nint(reada(tmpkey,i)) 
          if (it2 < it1) then 
            it2 = it1 + 200 
            istep = 10 
            go to 10 
          endif 
          i = index(tmpkey,',') 
          if (i /= 0) then 
            tmpkey(i:i) = ' ' 
            istep = nint(reada(tmpkey,i)) 
            istep = max0(1,istep) 
          else 
            istep = (it2 - it1)/20 
            if (istep == 0) istep = 1 
            if (istep>=2 .and. istep<5) istep = 2 
            if (istep>=5 .and. istep<10) istep = 5 
            if (istep>=10 .and. istep<20) istep = 10 
            if (istep>=20 .and. istep<50) istep = 20 
            if (istep>=50 .and. istep<100) istep = 50 
            istep = min0(100,istep) 
          endif 
        else 
          it2 = it1 + 200 
        endif 
      endif 
   10 continue 
      write (iw, '(2/,A)') title 
      write (iw, '(A)') koment 
      if (linear) then 
        write (iw, '(2/10X,''MOLECULE IS LINEAR'')') 
      else 
        write (iw, '(2/10X,''MOLECULE IS NOT LINEAR'')') 
      endif 
      write (iw, &
        '(/10X,''THERE ARE'',I3,'' GENUINE VIBRATIONS IN THIS '',''SYSTEM'')') &
        nvibs 
      write (iw, 20) 
   20 format(10x,'THIS THERMODYNAMICS CALCULATION IS LIMITED TO',/,10x,&
        'MOLECULES WHICH HAVE NO INTERNAL ROTATIONS'/,/) 
      write (iw, '(2/20X,''CALCULATED THERMODYNAMIC PROPERTIES'')') 
      write (iw, '(42X,''*'')') 
      write (iw, &
      '(''   TEMP. (K)   PARTITION FUNCTION   H.O.F.'',        ''    ENTHALPY  &
      & HEAT CAPACITY  ENTROPY'')') 
      write (iw, &
      '(  ''                                    KCAL/MOL'',    ''   CAL/MOLE   &
      & CAL/K/MOL   CAL/K/MOL'',/)') 
      do i = 1, nvibs 
        vibs(i) = abs(vibs(i)) 
      end do 
      ilim = 1 
      do itemp = it1, it2, istep 
        ilim = ilim + 1 
        trange(ilim) = itemp 
      end do 
      trange(1) = 298.D0 
      qtr2 = 2.D0*3.14159265358979D0*ac*wt/fpc_10 
      do ir = 1, ilim 
        itemp = int(trange(ir)) 
        t = itemp 
!   ***   INITIALISE SOME VARIABLES   ***
        c1 = h*ac/ak/t 
        qv = 1.0D0 
        hv = 0.0D0 
        cpv = 0.0D0 
        sv1 = 0.0D0 
!   ***   CONSTRUCT THE FREQUENCY DEPENDENT PARTS OF PARTITION FUNCTION
        do i = 1, nvibs 
          wi = vibs(i) 
!     INSERTED BY PROF. HIRANO
!        To exclude imaginary Wi (here expressed in negative value) and
!        small Wi arising from the imperfect geometry optimization,
!        Wi less than 100 cm-1 (an arbitrary threshold) are neglected
!        in the calculations of vibrational contributions.
          if (wi < 100.D0) cycle  
!
          ewj = dexp((-wi*c1)) 
          ewjr = 1.D0 - ewj 
          qv = qv/ewjr 
          e0 = wi*ewj/ewjr 
          hv = hv + e0 
          cpv = cpv + e0*e0/ewj 
          sv1 = sv1 + dlog(ewjr) 
        end do 
!   ***   FINISH CALCULATION OF VIBRATIONAL PARTS   ***
        e0 = r*c1 
        sv = hv*e0 - r*sv1 
        hv = hv*e0*t 
        cpv = cpv*e0*c1 
!   ***   NOW CALCULATE THE ROTATIONAL PARTS  (FIRST LINEAR MOLECULES
        if (.not.linear) then 
          e0 = 3.14159265358979D0/(a*b*c) 
          qr = dsqrt(e0/c1)/c1/sym 
          cpr = 1.5D0*r 
          hr = cpr*t 
          sr = r*(1.5D0*dlog(1.D0/c1) - dlog(sym) + dlog(e0)/2.D0 + 1.5D0) 
        else 
          qr = 1.D0/(c1*a*sym) 
          cpr = r 
          hr = cpr*t 
          sr = r*dlog(qr) + r 
        endif 
!   ***   CALCULATE INTERNAL CONTRIBUTIONS   ***
        qint = qv*qr 
        hint = hv + hr 
        cpint = cpv + cpr 
        sint = sv + sr 
!   ***   CONSTRUCT TRANSLATION CONTRIBUTIONS   ***
        qtr = dsqrt(qtr2/c1/h)**3 
        cptr = 2.5D0*r 
        htr = cptr*t 
!     UPDATED BY PROF.HIRANO AND MODIFIED BY YI
        str = 4.96804D0*(log(t) + 0.6D0*log(wt)) - 2.31482D0 
!
!  Which is best?
!
!   STR=2.2868D0*(5.D0*LOG10(T)+3.D0*LOG10(WT))-2.3135D0
!   STR=0.993608D0*(5.D0*LOG(T)+3.D0*LOG(WT))-2.31482D0
!   STR=4.96804D0*(LOG(T)+0.6D0*LOG(WT))-2.31482D0
!
!   ***   CONSTRUCT TOTALS   ***
        cptot = cptr + cpint 
        stot = str + sint 
        htot = htr + hint 
!   ***   OUTPUT SECTION   ***
!     UPDATED BY PROF. HIRANO
        if (ir == 1) h298 = htot 
        write (iw, '(/,F7.2,''  VIB.'',D15.4,16X,3F11.4)') t, qv, hv, cpv, sv 
        write (iw, '(9X,''ROT.'',D15.4,16X,3F11.4)') qr, hr, cpr, sr 
        write (iw, '(9X,''INT.'',D15.4,16X,3F11.4)') qint, hint, cpint, sint 
        write (iw, '(9X,''TRA.'',D15.4,16X,3F11.4)') qtr, htr, cptr, str 
        write (iw, '(9X,''TOT.'',13X,F17.3,F12.4,2F11.4)') escf + (htot - h298)&
          /1000.D0, htot, cptot, stot 
      end do 
      write (iw, &
      '(/3X,''*: NOTE: HEATS OF FORMATION ARE RELATIVE TO THE'',/12X,'' ELEMENT&
      &S IN THEIR STANDARD STATE AT 298K'')') 
!     INSERTED BY PROF.HIRANO
      write (iw, &
      '(12X,''  (=  Standard Enthalpy of Formation).'',        /12X,''Heat of F&
      &ormation at T is calculated as '',                ''[Escf + (HT - H298.15&
      &)-.'')') 
      write (iw, &
      '(/12X,''Hvib:  Zero-point energy is not included.'',    /12X,''       Wi&
      & < 100 cm-1 are not included.'',                  /12X,''Hrot = (3/2)RT''&
      &,                                          /12X,''Htra = (3/2)RT + pV = (&
      &5/2)RT'')') 
      write (iw, '(/12X,''Heat capacity is Cp, not Cv'')') 
      write (iw, &
      '(/12X,''     See Section 8.20 in Manual for further '', ''information.  &
      & (by TH).'')') 
!  END OF INSERT
      return  
      end subroutine thermo 
