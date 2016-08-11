      subroutine force() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : natoms, ndep,  nvar, gnorm, iflepo, keywrd, &
      & limscf, last, numat, escf
      use permanent_arrays, only : xparam, na, nb, nc, geo, geoa, &
      & labels, coord, loc, simbol, grad, errfn
      use symmetry_C, only : name, igroup
      USE elemts_C, only : elemnt  
      USE funcon_C, only : fpc_10, fpc_6, fpc_8 
      USE euler_C, only : id  
      USE chanel_C, only : iw 
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:03:08  03/16/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use xyzint_I 
      use gmetry_I 
      use second_I 
      use compfg_I 
      use dot_I 
      use nllsq_I 
      use flepo_I 
      use writmo_I 
      use axis_I 
      use symtrz_I 
      use fmat_I 
      use vecprt_I 
      use frame_I 
      use rsp_I 
      use matou1_I 
      use matout_I 
      use freqcy_I 
      use drc_I 
      use anavib_I 
      use reada_I 
      use thermo_I 
      use intfc_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      integer , dimension(60) :: irot 
      integer , dimension(2,3*natoms) :: locold 
      integer , dimension(natoms) :: nar, nbr, ncr 
      integer :: j, i, l, nvaold, ndeold, ilim, iu, il, nvib, ij, &
        im1, ju, jl, ii, jj, ni, k, nto6, nrem6, iinc1, iinc2 
      real(double), dimension(3,3*numat) :: deldip, trdip  
      real(double), dimension(3*numat) :: dipt, travel, freq, redmas
      real(double), dimension(3,3) :: rot  
      real(double) :: time2, tscf, tder, time1, time3, a, b, c, &
        wtmol, sum, const, summ, sum1, sym 
      real(double), dimension(:), allocatable :: cnorml, fmatrx, store, ff, oldf
      logical :: restrt, linear, debug, bartel, prnt, large 
!-----------------------------------------------
!**********************************************************************
!
!   FORCE CALCULATES THE FORCE CONSTANTS FOR THE MOLECULE, AND THE
!         VIBRATIONAL FREQUENCIES.  ISOTOPIC SUBSTITUTION IS ALLOWED.
!
!**********************************************************************
!  Point-Groups are stored in order of increasing symmetry.  Groups
!   supported are, in order
!
! C1, Cs, Ci, C2, D2, C2v, C2h, D2h, C3, C4, S4, D3, C3v, C3h, C5, C6,
! S6, C7, C8, S8, C4v, D4, D2d, C5v, C6v, D6, C4h, D3h, D3d, C7v, C7h,
! C8v, D8, D4d, D4h, C5h, D5, D5h, D5d, C6h, D6h, D6d, D7, D7h, D7d,
! C8h, D8h, T, Td, O, Th, Oh, I, Ih, Cv, Dh, R3
!
      data irot/ 1, 1, 1, 2, 4, 2, 2, 4, 3, 4, 2, 6, 3, 3, 5, 6, 3, 7, 8, 4, 4&
        , 8, 4, 5, 6, 12, 4, 6, 6, 7, 7, 8, 16, 8, 8, 5, 10, 10, 10, 6, 12, 12&
        , 14, 14, 14, 8, 16, 12, 12, 24, 12, 24, 24, 24, 1, 2, 4*1/  
!
! TEST GEOMETRY TO SEE IF IT IS OPTIMIZED
      time2 = -1.D9 
! Save the connectivity and geometry for restarts
      nar(:natoms) = na(:natoms) 
      nbr(:natoms) = nb(:natoms) 
      ncr(:natoms) = nc(:natoms) 
      if (allocated(geoa)) deallocate(geoa)
      allocate(geoa(3,natoms))
      geoa = geo 
     
      if (id==0 .and. index(keywrd,' NOREOR')==0) then 
!
!   NEED TO ENSURE THAT XYZINT WILL WORK CORRECTLY BEFORE CALL
!   TO DRC.
!
        l = 0 
        do i = 1, natoms 
          if (labels(i) == 99) cycle  
          l = l + 1 
          labels(l) = labels(i) 
        end do 
        numat = l 
        natoms = numat 
        call xyzint (coord, numat, na, nb, nc, 1.D0, geo) 
        call gmetry (geo, coord) 
        nvaold = nvar 
        locold(1,:nvar) = loc(1,:nvar) 
        locold(2,:nvar) = loc(2,:nvar) 
      endif 
      nvar = 0 
      ndeold = ndep 
      ndep = 0 
      numat = 0 
      if (labels(1) /= 99) numat = 1 
      do i = 2, natoms 
        if (labels(i) == 99) cycle  
        if (i == 2) ilim = 1 
        if (i == 3) ilim = 2 
        if (i > 3) ilim = 3 
!
!  IS IT A POLYMER?
!
        if (labels(i) == 107) then 
          ilim = 1 
        else 
          numat = numat + 1 
        endif 
!$DOIT ASIS
        do j = 1, ilim 
          nvar = nvar + 1 
          loc(1,nvar) = i 
          loc(2,nvar) = j 
          xparam(nvar) = geo(j,i) 
        end do 
      end do 
       i = 3*numat
       if (allocated(grad))    deallocate(grad)
       if (allocated(errfn))   deallocate(errfn)
       allocate(cnorml(i**2), fmatrx((i*(i+1))/2),ff(i**2), &
       & store((i*(i+1))/2), grad(i), oldf((i*(i + 1))/2), errfn(i))
       errfn = 0.d0
!
!   IF A RESTART, THEN TSCF AND TDER WILL BE FAULTY, THEREFORE SET TO -1
!
      tscf = -1.D0 
      tder = -1.D0 
      prnt = index(keywrd,'RC=') == 0 
      debug = index(keywrd,'DFORCE') /= 0 
      large = index(keywrd,'LARGE') /= 0 
      bartel = index(keywrd,'NLLSQ') /= 0 
      restrt = index(keywrd,'RESTART') /= 0 
      time1 = second(1) 
      if (restrt) then 
!
!   CHECK TO SEE IF CALCULATION IS IN NLLSQ OR FORCE.
!
        if (bartel) go to 70 
!
!   CALCULATION IS IN FORCE
!
        go to 110 
      endif 
      call compfg (xparam, .TRUE., escf, .TRUE., grad, .FALSE.) 
      if (prnt) write (iw, &
        '(2/10X,''HEAT OF FORMATION ='',F12.6,           '' KCALS/MOLE'')') &
        escf 
      time2 = second(1) 
      tscf = time2 - time1 
      call compfg (xparam, .TRUE., escf, .FALSE., grad, .TRUE.) 
      time3 = second(1) 
      tder = time3 - time2 
      if (prnt) write (iw, &
      '(2/10X,''INTERNAL COORDINATE DERIVATIVES'',2/3X,''NUMBER  ATOM'',7X,''BO&
      &ND'',9X,''ANGLE'',6X,''DIHEDRAL'',/)') 
      l = 0 
      iu = 0 
      if (.not.prnt) then 
        do i = 1, natoms 
          if (labels(i) == 99) cycle  
          l = l + 1 
          il = iu + 1 
          if (i == 1) iu = il - 1 
          if (i == 2) iu = il 
          if (i == 3) iu = il + 1 
          if (i > 3) iu = il + 2 
          if (labels(i) == 107) iu = il 
          cycle  
        end do 
      else 
        do i = 1, natoms 
          if (labels(i) == 99) cycle  
          l = l + 1 
          il = iu + 1 
          if (i == 1) iu = il - 1 
          if (i == 2) iu = il 
          if (i == 3) iu = il + 1 
          if (i > 3) iu = il + 2 
          if (labels(i) == 107) iu = il 
          write (iw, '(I6,6X,A2,F14.6,2F13.6)') l, elemnt(labels(i)), (grad(j),&
            j=il,iu) 
        end do 
      endif 
!   TEST SUM OF GRADIENTS
      gnorm = sqrt(dot(grad,grad,nvar)) 
      if (prnt) write (iw, '(2/10X,''GRADIENT NORM ='',F10.5)') gnorm 
      if (gnorm < 10.D0) go to 90 
      if (index(keywrd,' LET ') /= 0) then 
        write (iw, &
      '(3/1X,''** GRADIENT IS VERY LARGE, BUT SINCE'',     '' "LET" IS USED, CA&
      &LCULATION WILL CONTINUE'')') 
        go to 90 
      endif 
      write (iw, &
      '(3/1X,''** GRADIENT IS TOO LARGE TO ALLOW '',              ''FORCE MATRI&
      &X TO BE CALCULATED, (LIMIT=10) **'',2/)') 
   70 continue 
      simbol(:nvar) = '---' 
      write (iw, '(2/10X,'' GEOMETRY WILL BE OPTIMIZED FIRST'')') 
      if (bartel) then 
        write (iw, '(15X,''USING NLLSQ'')') 
        call nllsq () 
      else 
        write (iw, '(15X,''USING FLEPO'')') 
        call flepo (xparam, nvar, escf) 
!
!  DID FLEPO USE ALL THE TIME ALLOWED?
!
      endif 
      if (iflepo == (-1)) then 
        limscf = .FALSE. 
        call compfg (xparam, .TRUE., escf, .TRUE., grad, .FALSE.) 
        call writmo 
        return  
      endif 
      limscf = .FALSE. 
      call compfg (xparam, .TRUE., escf, .TRUE., grad, .TRUE.) 
      call writmo 
      write (iw, '(2/10X,''GRADIENT NORM ='',F10.7)') gnorm 
      call gmetry (geo, coord) 
   90 continue 
      nar(:natoms) = na(:natoms) 
      nbr(:natoms) = nb(:natoms) 
      ncr(:natoms) = nc(:natoms) 
      geoa(:,:natoms) = geo(:,:natoms) 
!
! NOW TO CALCULATE THE FORCE MATRIX
!
! CHECK OUT SYMMETRY
  110 continue 
      if (index(keywrd,'THERMO')/=0 .and. gnorm>1.D0) then 
        write (iw, &
      '(2/30X,''**** WARNING ****'',2/                      10X,'' GRADIENT IS &
      &VERY LARGE FOR A THERMO CALCULATION'',/        10X,'' RESULTS ARE LIKELY &
      &TO BE INACCURATE IF THERE ARE'')') 
        write (iw, &
          '(10X,'' ANY LOW-LYING VIBRATIONS (LESS THAN ABOUT '' ,''400CM-1)'')'&
          ) 
        write (iw, &
      '(10X,'' GRADIENT NORM SHOULD BE LESS THAN ABOUT '',  ''0.2 FOR THERMO'',&
      &/10X,'' TO GIVE ACCURATE RESULTS'')') 
      endif 
      if (tscf > 0.D0) then 
        write (iw, '(2/10X,''TIME FOR SCF CALCULATION ='',F8.2)') tscf 
        write (iw, '( /10X,''TIME FOR DERIVATIVES     ='',F8.2)') tder 
      endif 
      if (ndeold > 0) write (iw, &
      '(2/10X,''SYMMETRY WAS SPECIFIED, BUT '',             ''CANNOT BE USED HE&
      &RE'')') 
      if (prnt) call axis (coord, numat, a, b, c, wtmol, 2, rot) 
      if (rot(1,1) > 2.d0) return ! dummy use of rot
      nvib = 3*numat - 6 
      if (abs(c) < 1.D-20) nvib = nvib + 1 
      if (id /= 0) nvib = 3*numat - 3 
      if (prnt) then 
        write (iw, &
      '(/10X,''ORIENTATION OF MOLECULE '',                  ''IN FORCE CALCULAT&
      &ION'')') 
        write (iw, &
      '(/,4X,''NO.'',7X,''ATOM'',9X,''X'',                  9X,''Y'',9X,''Z'',/&
      &)') 
      endif 
      l = 0 
      if (.not.prnt) then 
        l = l + count(labels(:natoms)/=99) 
      else 
        do i = 1, natoms 
          if (labels(i) == 99) cycle  
          l = l + 1 
          write (iw, '(I6,9X,A2,3X,3F10.4)') l, elemnt(labels(i)), (coord(j,l),&
            j=1,3) 
        end do 
      endif 
      call symtrz (cnorml, cnorml, 2, .FALSE.) 
      call fmat (fmatrx, nvib, tscf, tder, deldip, escf, ff, coord) 
      na(1) = 0 
      na(:natoms) = nar(:natoms) 
      nb(:natoms) = nbr(:natoms) 
      nc(:natoms) = ncr(:natoms) 
      geo(:,:natoms) = geoa(:,:natoms) 
      if (nvib < 0) then 
        ndep = ndeold 
        nvar = 0 
        iflepo = -1 
        return  
      endif 
!
!   THE FORCE MATRIX IS PRINTED AS AN ATOM-ATOM MATRIX RATHER THAN
!   AS A 3N*3N MATRIX, AS THE 3N MATRIX IS VERY CONFUSING!
!
      ij = 0 
      iu = 0 
      do i = 1, numat 
        il = iu + 1 
        iu = il + 2 
        im1 = i - 1 
        ju = 0 
        do j = 1, im1 
          jl = ju + 1 
          ju = jl + 2 
          sum = 0.D0 
!$DOIT ASIS
          do ii = il, iu 
!$DOIT ASIS
            do jj = jl, ju 
              sum = sum + fmatrx((ii*(ii-1))/2+jj)**2 
            end do 
          end do 
          ij = ij + 1 
          store(ij) = sqrt(sum) 
        end do 
        ij = ij + 1 
        store(ij) = sqrt(fmatrx(((il+0)*(il+1))/2)**2+fmatrx(((il+1)*(il+2))/2)&
          **2+fmatrx(((il+2)*(il+3))/2)**2+2.D0*(fmatrx(((il+1)*(il+2))/2-1)**2&
          +fmatrx(((il+2)*(il+3))/2-2)**2+fmatrx(((il+2)*(il+3))/2-1)**2)) 
      end do 
      if (debug) then 
        write (iw, '(2/10X,'' FULL FORCE MATRIX, INVOKED BY "DFORCE"'')') 
        i = -nvar 
        call vecprt (fmatrx, i) 
      endif 
      if (prnt) then 
        write (iw, '(2/10X,'' FORCE MATRIX IN MILLIDYNES/ANGSTROM'')') 
        call vecprt (store, numat) 
      endif 
      l = (nvar*(nvar + 1))/2 
      store(:l) = fmatrx(:l) 
      if (prnt) call axis (coord, numat, a, b, c, sum, 0, rot) 
      if (prnt) write (iw, &
        '(2/10X,''HEAT OF FORMATION ='',F12.6,           '' KCALS/MOLE'')') &
        escf 
      if (large) then 
        call frame (store, numat, 0) 
        call rsp (store, nvar, nvar, freq, cnorml) 
        do i = nvib + 1, nvar 
          j = int((freq(i)+50.D0)*0.01D0) 
          freq(i) = freq(i) - j*100 
        end do 
        if (prnt) then 
          write (iw, '(2/10X,''TRIVIAL VIBRATIONS, SHOULD BE ZERO'')') 
          write (iw, &
      '(/, F9.4,''=TX'',F9.4,''=TY'',F9.4,''=TZ'',                    F9.4,''=R&
      &X'',F9.4,''=RY'',F9.4,''=RZ'')') (freq(i),i=nvib + 1,nvar) 
          call symtrz (cnorml, freq, 2, .TRUE.) 
          write (iw, '(2/''      MOLECULAR POINT GROUP   :   '',A4)') name 
          write (iw, '(2/10X,'' EIGENVECTORS  '')') 
          i = 3*numat 
          call matou1 (cnorml, freq, nvib, i, nvib, 5) 
          write (iw, &
      '(2/10X,''FORCE CONSTANTS IN MILLIDYNES/ANGSTROM'' ,'' (= 10**5 DYNES/CM)&
      &'',/)') 
          write (iw, '(8F10.5)') (freq(i),i=1,nvib) 
! CONVERT TO WEIGHTED FMAT
          write (iw, '(2/10X,'' ASSOCIATED EIGENVECTORS'')') 
          i = -nvar 
          call matout (cnorml, freq, nvib, i, nvar) 
        endif 
      endif 
      call freqcy (fmatrx, freq, cnorml, redmas, travel, .TRUE., deldip, ff, oldf) 
      if (index(keywrd,' K=') /= 0) return  
!
!  CALCULATE ZERO POINT ENERGY
!
!
!  THESE CONSTANTS TAKEN FROM CODATA BULLETIN,NO.63(1986).
!   N AVOGADRO'S NUMBER
!   H PLANCK'S CONSTANT
!   C SPEED OF LIGHT
!   CONST=0.5*N*H*C/(1.D10*4.184)
      const = 0.5D0*fpc_10*fpc_6*fpc_8/(1.D10*4.184D0) 
      sum = 0.D0 
      ni = 0 
      do i = 1, nvib 
        if (freq(i) > 0) then 
          sum = sum + freq(i) 
        else 
          ni = ni + 1 
        endif 
      end do 
      sum = sum*const 
      if (prnt) then 
        write (iw, '(/9X,'' ZERO POINT ENERGY'', F12.3,'' KCAL/MOL'')') sum 
        if (ni /= 0) then 
          write (iw, '(2(/9X,A))') &
            'NOTE: SYSTEM IS NOT A GROUND STATE, THEREFORE ZERO POINT', &
            'ENERGY IS NOT MEANINGFULL. ZERO POINT ENERGY PRINTED' 
          write (iw, '(9X,A,I3,A)') 'DOES NOT INCLUDE THE', ni, &
            ' IMAGINARY FREQUENCIES' 
        endif 
      endif 
      summ = 0.D0 
      do i = 1, nvar 
        sum1 = 1.D-20 
        do j = 1, nvar 
          sum1 = sum1 + cnorml(j+(i-1)*nvar)**2 
        end do 
        sum1 = 1.D0/sqrt(sum1) 
        grad = 0.D0 
        do k = 1, 3 
          sum = 0.D0 
          do j = 1, nvar 
            sum = sum + cnorml(j+(i-1)*nvar)*deldip(k,j) 
          end do 
          summ = summ + abs(sum) 
          trdip(k,i) = sum*sum1 
        end do 
        dipt(i) = sqrt(trdip(1,i)**2+trdip(2,i)**2+trdip(3,i)**2) 
      end do 
      if (prnt) then 
        write (iw, &
      '(2/3X,'' THE LAST'',I2,'' VIBRATIONS ARE THE'',      '' TRANSLATION AND &
      &ROTATION MODES'')') nvar - nvib 
        write (iw, &
      '(3X,'' THE FIRST THREE OF THESE BEING TRANSLATIONS'','' IN X, Y, AND Z, &
      &RESPECTIVELY'')') 
      endif 
      if (prnt .and. large) then 
        write (iw, &
      '(2/10X,'' FREQUENCIES, REDUCED MASSES AND '',        ''VIBRATIONAL DIPOL&
      &ES''/)') 
        nto6 = nvar/6 
        nrem6 = nvar - nto6*6 
        iinc1 = -5 
        if (nto6 >= 1) then 
          do i = 1, nto6 
            write (iw, '(/)') 
            iinc1 = iinc1 + 6 
            iinc2 = iinc1 + 5 
            write (iw, '(3X,''I'',10I10)') (j,j=iinc1,iinc2) 
            write (iw, '('' FREQ(I)'',6F10.4,/)') (freq(j),j=iinc1,iinc2) 
            write (iw, '('' MASS(I)'',6F10.5,/)') (redmas(j),j=iinc1,iinc2) 
            write (iw, '('' DIPX(I)'',6F10.5)') (trdip(1,j),j=iinc1,iinc2) 
            write (iw, '('' DIPY(I)'',6F10.5)') (trdip(2,j),j=iinc1,iinc2) 
            write (iw, '('' DIPZ(I)'',6F10.5,/)') (trdip(3,j),j=iinc1,iinc2) 
            write (iw, '('' DIPT(I)'',6F10.5)') (dipt(j),j=iinc1,iinc2) 
          end do 
        endif 
        if (nrem6 >= 1) then 
          write (iw, '(/)') 
          iinc1 = iinc1 + 6 
          iinc2 = iinc1 + (nrem6 - 1) 
          write (iw, '(3X,''I'',10I10)') (j,j=iinc1,iinc2) 
          write (iw, '('' FREQ(I)'',6F10.4)') (freq(j),j=iinc1,iinc2) 
          write (iw, '(/,'' MASS(I)'',6F10.5)') (redmas(j),j=iinc1,iinc2) 
          write (iw, '(/,'' DIPX(I)'',6F10.5)') (trdip(1,j),j=iinc1,iinc2) 
          write (iw, '('' DIPY(I)'',6F10.5)') (trdip(2,j),j=iinc1,iinc2) 
          write (iw, '('' DIPZ(I)'',6F10.5)') (trdip(3,j),j=iinc1,iinc2) 
          write (iw, '(/,'' DIPT(I)'',6F10.5)') (dipt(j),j=iinc1,iinc2) 
        endif 
      endif 
      if (prnt) then 
        write (iw, '(2/10X,'' NORMAL COORDINATE ANALYSIS'')') 
        j = 3*numat 
        i = -nvar 
        call matou1 (cnorml, freq, nvib, j, nvib, 5) 
      endif 
!
!   CARRY OUT IRC IF REQUESTED.
!
      if (index(keywrd,'IRC') + index(keywrd,'DRC') /= 0) then 
        loc(1,:nvar) = 0 
        loc(2,:nvar) = 0 
        nvar = nvaold 
        loc(1,:nvar) = locold(1,:nvar) 
        loc(2,:nvar) = locold(2,:nvar) 
        call xyzint (coord, numat, na, nb, nc, 1.D0, geo) 
        last = 1 
        call drc (cnorml, freq) 
        na(1) = 0 
        ndep = ndeold 
        nvar = 0 
        geo(:,:natoms) = geoa(:,:natoms) 
        return  
      endif 
!fck -315 !  deldip is used twice
      call freqcy (fmatrx, freq, cnorml, deldip, deldip, .FALSE., deldip, ff, &
        oldf) 
!fck +315
      write (iw, '(2/10X,'' MASS-WEIGHTED COORDINATE ANALYSIS'')') 
      i = -nvar 
      j = 3*numat 
      call matou1 (cnorml, freq, nvib, j, nvib, 5) 
     call anavib (coord, freq, dipt, nvar, cnorml, store, nvib, fmatrx, travel&
        , redmas, ff) 
      if (index(keywrd,'THERMO') /= 0) then 
        call gmetry (geo, coord) 
        sym = irot(igroup) 
        write (iw, '(A,A,A,I3)') ' SYMMETRY NUMBER FOR POINT-GROUP ', name, '='&
          , irot(igroup) 
        linear = abs(a*b*c) < 1.D-10 
        i = index(keywrd,' TRANS') 
!
!   "I" IS GOING TO MARK THE BEGINNING OF THE GENUINE VIBRATIONS.
!
        if (i /= 0) then 
          i = index(keywrd,' TRANS=') 
          if (i /= 0) then 
            i = nint(1 + reada(keywrd,i)) 
            j = nvib - i + 1 
            write (iw, &
      '(2/1X,''THE LOWEST'',I3,'' VIBRATIONS ARE '',  ''NOT'',/,'' TO BE USED I&
      &N THE THERMO CALCULATION'')') i - 1 
          else 
            write (iw, '(2/10X,''SYSTEM IS A TRANSITION STATE'')') 
            i = 2 
            j = nvib - 1 
          endif 
        else 
          write (iw, '(2/10X,''SYSTEM IS A GROUND STATE'')') 
          i = 1 
          j = nvib 
        endif 
        call thermo (a, b, c, linear, sym, wtmol, freq(i), j, escf) 
      endif 
      call intfc (oldf, xparam, geoa, nar, nbr, ncr) 
      na(1) = 0 
      nvar = 0 
      ndep = ndeold 
      geo(:,:natoms) = geoa(:,:natoms) 
      return  
      end subroutine force 
