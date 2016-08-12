      real(kind(0.0d0)) function meci () 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use meci_C, only : rjkaa, rjkab, microa, microb, nmeci, nelec, nstate, &
      & lab, labsiz, cif2, cdiagi, cdiag, occa, maxci, nalmat, conf, nmos, &
      & vectci, xy, dijkl, ispin, eig, ispqr,  k 
      use symmetry_C, only : jndex, namo, state
      USE permanent_arrays, only : nfirst, nlast, eigs, c
      USE molkst_C, only : nopen, nclose, fract, nelecs, norbs, numcal, &
      & numat, keywrd, last, lm61, lm6, rjkab1
      USE chanel_C, only : ir, iw
      use cosmo_C, only : iseps
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  16:24:08  03/14/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use mopend_I 
      use upcase_I 
      use perm_I 
      use diagi_I 
      use mecih_I 
      use vecprt_I 
      use rsp_I 
      use symtrz_I 
      use matou1_I 
      use matout_I 
      use ciosci_I 
      use dmecip_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(:), allocatable :: nfa
      integer :: icalcn, ndoubl, msdel, j, l, mdim, lroot, ne, lima, &
        limb, i1, j1, ntot, izero, limci, nupp, &
        ndown, loc, i2, j2, kroot, iroot, i, ii, ji, m, maxvec, &
        iuj, iofset, il, iu 
      real(double)  :: cij(lm61 + lm6) 
      real(double)  :: ckl(lm61 + lm6) 
      real(double)  :: wcij(lm61 + lm6) 
      integer, dimension(:,:), allocatable :: nperma, npermb 
      real(double), dimension(:), allocatable :: eiga, cimat, spin
      real(double), dimension(:), allocatable :: diag 
      real(double), dimension(nmeci, nmeci) :: deltap
      real(double), dimension(nmeci*norbs) :: delta
      real(double), dimension(:,:), allocatable :: oscil
      real(double), dimension(3) :: work 
      real(double) :: smult, xx, x, gse, y, sum, summ 
      logical :: debug, large, prnt, lspin, lspin1, peci, first1, bigprt, sing&
        , doub, trip, quar, quin, sext, prnt2, geook, getmic, sept 
      character , dimension(7) :: tspin*8 
      character :: line*80 

!--TGAY 08/2016 - peci was not saved
      save spin, debug, large, lspin1, first1, sing, doub, trip, quar, quin, &
        sext, prnt2, geook, getmic, sept, tspin, icalcn, ndoubl, msdel, j, l, &
        mdim, lroot, smult, ne, xx, lima, limb, eiga, &
        oscil, nperma, npermb, nfa, peci
!-----------------------------------------------
!**********************************************************************
!
!                 PROGRAM MECI
!
!   A MULTI-ELECTRON CONFIGURATION INTERACTION CALCULATION
!
!   WRITTEN BY JAMES J. P. STEWART, AT THE
!              FRANK J. SEILER RESEARCH LABORATORY
!              USAFA, COLORADO SPRINGS, CO 80840
!
!              1985
!
!**********************************************************************
!
!
!   MATRICES FOR PERMUTATION WORK
!
!
!   MATRICES FOR ONE AND TWO ELECTRON INTEGRALS
!
!
!   SPIN MATRICES
!
!
!   MATRICES FOR SEC.DET., VECTORS, AND EIGENVALUES.
!
      data icalcn/ 0/  
      data tspin/ 'SINGLET ', 'DOUBLET ', 'TRIPLET ', 'QUARTET ', 'QUINTET ', &
        'SEXTET  ', 'SEPTET  '/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
        first1 = .TRUE. 
        mdim = maxci
        geook = index(keywrd,'GEO-OK') /= 0 
        lspin1 = index(keywrd,'ESR') /= 0 
        debug = index(keywrd,'DEBUG') /= 0 
        prnt2 = index(keywrd,'MECI') /= 0 
        debug = debug .and. prnt2 
        large = index(keywrd,'LARGE') /= 0 
        peci = index(keywrd,' PECI') /= 0 
        ndoubl = 99 
        i = index(keywrd,'C.I.=(') 
        if (i /= 0) then 
          j = index(keywrd(i:i+10),',') + i - 1 
          ndoubl = nint(reada(keywrd,j)) 
          nmos = nint(reada(keywrd,index(keywrd,'C.I.=(') + 5)) 
          ndoubl = min(ndoubl,nclose) 
        else if (index(keywrd,'C.I.=') /= 0) then 
          nmos = nint(reada(keywrd,index(keywrd,'C.I.=') + 5)) 
        else 
          nmos = nopen - nclose 
        endif 
        nmos = min(nmos,norbs) 
        if (allocated(occa)) deallocate(occa)
        if (allocated(nfa))  deallocate(nfa)
        allocate(occa(nmos))
        allocate(nfa(nmos + 3))
        lroot = 1 
        if (index(keywrd,'EXCI') /= 0) lroot = 2 
        i = index(keywrd,'ROOT') 
        if (i /= 0) lroot = nint(reada(keywrd,i)) 
        if (ndoubl == 99) then 
          j = max(min((nclose + nopen + 1)/2 - (nmos - 1)/2,norbs - nmos + 1),1&
            ) 
        else 
          j = nclose - ndoubl + 1 
          if (fract > 1.99D0) j = j + 1 
        endif 
        l = 0 
        if (nclose - j + 1 > 0) then 
          occa(:nclose-j+1) = 1 
          l = nclose - j + 1 
        endif 
        j = max(j,nclose + 1) 
        if (nopen - j + 1 > 0) then 
          occa(l+1:nopen-j+1+l) = fract*0.5D0 
          l = nopen - j + 1 + l 
        endif 
        do i = 1, nmeci 
          l = l + 1 
          if (l > nmos) exit  
          occa(l) = 0.D0 
        end do 
        sing = index(keywrd,'SING') + index(keywrd,'EXCI') + index(keywrd,&
          'BIRAD') /= 0 
        doub = index(keywrd,'DOUB') /= 0 
        trip = index(keywrd,'TRIP') /= 0 
        quar = index(keywrd,'QUAR') /= 0 
        quin = index(keywrd,'QUIN') /= 0 
        sext = index(keywrd,'SEXT') /= 0 
        sept = index(keywrd,'SEPT') /= 0 
!
!  DEFINE MAGNETIC COMPONENT OF SPIN
!
        msdel = index(keywrd,' MS') 
        if (msdel /= 0) then 
          msdel = nint(reada(keywrd,index(keywrd,' MS'))) 
        else 
          if (trip .or. quar) msdel = 1 
          if (quin .or. sext) msdel = 2 
          if (sept) msdel = 3 
        endif 
        smult = -.5D0 
        if (sing) smult = 0.00D0 
        if (doub) smult = 0.75D0 
        if (trip) smult = 2.00D0 
        if (quar) smult = 3.75D0 
        if (quin) smult = 6.00D0 
        if (sext) smult = 8.75D0 
        if (sept) smult = 12.0D0 
        x = 0.D0 
        do j = 1, nmos 
          x = x + occa(j) 
        end do 
        xx = x + x 
        ne = nint(xx) 
        i = (nelecs - ne + 1)/2 
        nelec = (nelecs - ne + 1)/2 
      endif 
      prnt = debug .or. last==3 .and. prnt2 
      bigprt = prnt .and. large 
!--TGAY 08/2016 - Added initialisation of meci 
      meci = 0.D0 
!
!    TEST TO SEE IF THE SET OF ENERGY LEVELS USED IN MECI IS COMPLETE,
!    I.E., ALL COMPONENTS OF DEGENERATE IRREDUCIBLE REPRESENTATIONS
!    ARE USED.  IF NOT, THEN RESULTS WILL BE NONSENSE.  GIVE USERS A
!    CHANCE TO REALLY FOUL THINGS UP BY ALLOWING JOB TO CONTINUE IF
!    'GEO-OK' IS SPECIFIED.
!
      if (nelec < 0) then 
        write (iw, *) &
          ' NUMBER OF ELECTRONS IN M.O.S BELOW ACTIVE SPACE IS LESS THAN ZERO!' 
        meci = 0.D0 
        call mopend (&
           ' NUMBER OF ELECTRONS IN M.O.S BELOW ACTIVE SPACE IS LESS THAN ZERO') 
        return  
      endif 
      if (nelec + nmos > norbs) then 
        write (iw, *) &
      ' UPPER BOUND OF ACTIVE SPACE IS GREATER THAN THE NUMBER OF ORBITALS!' 
        meci = 0.D0 
        call mopend (&
       ' UPPER BOUND OF ACTIVE SPACE IS GREATER THAN THE NUMBER OF ORBITALS!') 
        return  
      endif 




      if (allocated(eiga)) then
        deallocate(eiga, rjkaa, xy, rjkab, dijkl)
      end if
      allocate(eiga(nmos), rjkaa(nmos,nmos), xy(nmos, nmos, nmos, nmos), &
      & rjkab(nmos, nmos), dijkl(norbs, nmos,(nmos*(nmos + 1))/2))





      eiga(:nmos) = eigs(1+nelec:nmos+nelec) 
      lspin = lspin1 .and. last==3 
      if (bigprt) then 
        write (iw, '(''  INITIAL EIGENVALUES'')') 
        write (iw, '(5F12.6)') (eiga(i),i=1,nmos) 
        write (iw, '(2/10X,''NUMBER OF ELECTRONS IN C.I. ='',F5.1)') xx 
      endif 
      i = nmos + nelec + 1 
      if (.not.geook .and. nelec>0) then 
        if (abs(eigs(nelec+1)-eigs(nelec))<1.D-1 .or. abs(eigs(nelec+1+nmos)-&
          eigs(nelec+nmos))<1.D-1) then 
          write (iw, '(3/10X,A)') 'DEGENERATE ENERGY LEVELS DETECTED IN MECI' 
          write (iw, '(10X,A)') &
            'SOME OF THESE LEVELS WOULD BE TREATED BY MECI,' 
          write (iw, '(10X,A)') 'WHILE OTHERS WOULD NOT.  THIS WOULD RESULT IN' 
          write (iw, '(10X,A)') 'NON-REPRODUCIBLE ELECTRONIC ENERGIES.' 
          write (iw, '(/2X,A)') 'EIGENVALUES OF ACTIVE SPACE' 
          write (iw, '(8F10.4)') (eigs(i),i=nelec + 1,nelec + nmos) 
          write (iw, '(/2X,A)') 'EIGENVALUES BELOW ACTIVE SPACE' 
          write (iw, '(8F10.4)') (eigs(i),i=max(1,nelec - 5),nelec) 
          write (iw, '(/2X,A)') 'EIGENVALUES ABOVE ACTIVE SPACE' 
          write (iw, '(8F10.4)') (eigs(i),i=nelec + nmos + 1,min(norbs,nelec + &
            nmos + 5)) 
          write (iw, *) 
          write (iw, '(10X,A)') &
            '  JOB STOPPED.   TO CONTINUE, SPECIFY "GEO-OK"' 
          meci = 0.D0 
          call mopend ('JOB STOPPED. TO CONTINUE, SPECIFY "GEO-OK".') 
          return  
        endif 
      endif 
      if (bigprt) then 
        write (iw, '(2/10X,''EIGENVECTORS'',/)') 
        do i = 1, norbs 
          write (iw, '(6F12.6)') (c(i,j+nelec),j=1,nmos) 
        end do 
      endif 
      nfa(2) = 1 
      nfa(1) = 1 
      do i = 3, nmos + 1 
        nfa(i) = nfa(i-1)*(i - 1) 
      end do 
      call ijkl (c(1,nelec+1), c, nelec, nmos, dijkl, cij, ckl, wcij, xy) 
      do i = 1, nmos 
        do j = 1, nmos 
          rjkaa(i,j) = xy(i,i,j,j) - xy(i,j,i,j) 
          rjkab(i,j) = xy(i,i,j,j) 
        end do 
      end do 
      rjkab1 = rjkab(1,1)
      do i = 1, nmos 
        x = 0.D0 
        do j = 1, nmos 
          x = x + (rjkaa(i,j)+rjkab(i,j))*occa(j) 
        end do 
        eiga(i) = eiga(i) - x 
      end do 
      if (bigprt) then 
        write (iw, 120) 
  120   format(/,5x,'EIGENVALUES AFTER REMOVAL OF INTER-ELECTRONIC',&
          ' INTERACTIONS',/) 
        write (iw, '(8F10.5)') (eiga(i),i=1,nmos) 
        write (iw, '(3/10X,''TWO-ELECTRON J-INTEGRALS'',/)') 
        do i1 = 1, nmos 
          write (iw, '(10F8.4)') (rjkab(i1,j1),j1=1,min(10,nmos)) 
        end do 
        write (iw, '(3/10X,''TWO-ELECTRON K-INTEGRALS'',/)') 
        do i1 = 1, nmos 
          write (iw, '(10F8.4)') (rjkab(i1,j1) - rjkaa(i1,j1),j1=1,min(nmos,10)&
            ) 
        end do 
      endif 
      i = 1 
      if (nmos > 0) then 
        j = 1 
        rjkaa(:nmos,:nmos) = rjkaa(:nmos,:nmos)*0.5D0 
        j = nmos + 1 
        i = nmos + 1 
      endif 
      if (first1) then 
        getmic = index(keywrd,'MICROS') /= 0 
        if (getmic) then 
          k = nint(reada(keywrd,index(keywrd,'MICROS'))) 
          lab = k 
          if (prnt) write (iw, '(''    MICROSTATES READ IN'')') 
          ntot = nint(xx) 
          rewind ir 
          do i = 1, 10000 
            read (ir, '(A)') line 
            call upcase (line, 80) 
            if (index(line,'MICRO') == 0) cycle  
            exit  
          end do 
          do i = 1, 1000 
            read (ir, '(A)', end=190, err=190) line 
            call upcase (line, 80) 
            if (index(line,'MICRO') /= 0) go to 200 
          end do 
  190     continue 
          write (iw, &
      '('' MICROSTATES SPECIFIED BY KEYWORDS'',          '' BUT MISSING FROM DA&
      &TA'')') 
          meci = 0.D0 
          call mopend (&
             'MICROSTATES SPECIFIED BY KEYWORDS BUT MISSING FROM DATA') 
          return  
  200     continue 
          if (allocated(microa)) deallocate(microa)
          if (allocated(microb)) deallocate(microb)
          allocate(microa(nmos, lab), microb(nmos,lab))
          do i = 1, lab 
            read (ir, '(A)') line 
            izero = max(0,min(index(line,'0'),index(line,'1')) - 1) 
            do j = 1, nmos 
              if (line(j+izero:j+izero) /= '1') line(j+izero:j+izero) = '0' 
              if (line(j+nmos+izero:j+nmos+izero) /= '1') line(j+nmos+izero:j+&
                nmos+izero) = '0' 
              microa(j,i) = ichar(line(j+izero:j+izero)) - ichar('0') 
              microb(j,i) = ichar(line(j+nmos+izero:j+nmos+izero)) - ichar('0') 
            end do 
            if (prnt) write (iw, '(20I3)') (microa(j,i),j=1,nmos), (microb(j,i)&
              ,j=1,nmos) 
            k = 0 
            do j = 1, nmos 
              k = k + microa(j,i) + microb(j,i) 
            end do 
            if (k == ntot) cycle  
            ntot = k 
            xx = k 
            write (iw, &
      '(/10X,''NUMBER OF ELECTRONS IN C.I. '',     ''REDEFINED TO:'',I4,/)') k 
          end do 
          if (getmic) go to 240 
          limci = 0 
        endif 
        nupp = (ne + 1)/2 + msdel 
        ndown = ne - nupp 
        if (nupp*ndown < 0) then 
          write (iw, '(/10X,''IMPOSSIBLE VALUE OF DELTA S'')') 
          meci = 0.D0 
          call mopend ('IMPOSSIBLE VALUE OF DELTA S') 
          return  
        endif 
        if (limci == 0) then 
          lima = nfa(nmos+1)/(nfa(nupp+1)*nfa(nmos-nupp+1)) 
          limb = nfa(nmos+1)/(nfa(ndown+1)*nfa(nmos-ndown+1)) 



          if (allocated(nperma)) deallocate(nperma)
          if (allocated(npermb)) deallocate(npermb)
          allocate(nperma(nmos, lima), npermb(nmos, limb))
         




        endif 
        lab = lima*limb 
         
        if (peci) limci = 4 
        if (index(keywrd,' CIS') /= 0) then 
          limci = 2 
          if (index(keywrd,' CISD') /= 0) limci = 4 
          if (index(keywrd,' CISDT') /= 0) limci = 6 
        else 
          if (lima > maxci .or. limb > maxci) then 
            write (iw, '(/10X,A)') ' TOO MANY CONFIGURATIONS' 
            meci = 0.D0 
            return  
          endif 
        endif 
        call perm (nperma, nupp, nmos, lima, limci) 
        call perm (npermb, ndown, nmos, limb, limci) 
        if (allocated(microa)) deallocate(microa)
        if (allocated(microb)) deallocate(microb)
        allocate(microa(nmos, lab), microb(nmos,lab))
      else 
        if (.not.getmic) lab = lima*limb 
      endif 
  240 continue 

      
      if (allocated(spin))   deallocate(spin)
      if (allocated(nalmat)) deallocate(nalmat)
      if (allocated(eig))    deallocate(eig)
      if (allocated(conf))   deallocate(conf)
      if (allocated(vectci)) deallocate(vectci)
      if (allocated(ispin))  deallocate(ispin)
      if (allocated(oscil))  deallocate(oscil)
      if (allocated(ispqr))  deallocate(ispqr)

      allocate(spin(lab), nalmat(lab), eig(lab + 1), conf(lab**2), vectci(7*lab), &
      & ispin(lab), oscil(3,lab + 4), ispqr(lab,nmos + 1))
      ispqr = -99999



      if (prnt) write (iw, 250) (nupp - ndown)*0.5D0 
  250 format(10x,'COMPONENT OF SPIN  = ',f4.1) 
      gse = 0.0D0 
      do i = 1, nmos 
        gse = gse + eiga(i)*occa(i)*2.D0 
        gse = gse + xy(i,i,i,i)*occa(i)*occa(i) 
        do j = i + 1, nmos 
          gse = gse + 2.D0*(2.D0*xy(i,i,j,j)-xy(i,j,i,j))*occa(i)*occa(j) 
        end do 
      end do 
      if (limci /= 0) then 
!
! REMOVE ALL UNWANTED EXCITATIONS
!
        loc = 0 
        if (limci == 2) loc = -1 
        do i = 1, lima 
          do j = 1, limb 
!
!        DETERMINE THE TYPE OF EXCITATION
!
            k = 0 
            do l = 1, nmos 
              k = k + abs(nperma(l,i)-nperma(l,1)) + abs(npermb(l,j)-npermb(l,1&
                )) 
            end do 
            if (peci) then 
              l = 1 
              if (nmos > 0) then 
                do l = 1, nmos 
                  if (.not.k<=2 .and. .not.nperma(l,i)==npermb(l,j)) k = 1000 
                end do 
                l = nmos + 1 
              endif 
            endif 
!
!        COPY DET AND INCREMENT LOC
!
            if (k > limci) cycle  
            loc = loc + 1 
            if (loc <= 0) cycle  
            l = 1 
            if (nmos > 0) then 
              microa(:nmos,loc) = nperma(:nmos,i) 
              microb(:nmos,loc) = npermb(:nmos,j) 
              l = nmos + 1 
            endif 
          end do 
        end do 
        lab = loc 
      endif 
      if (prnt) write (iw, 310) lab 
  310 format(/,/,10x,' NO OF CONFIGURATIONS CONSIDERED = ',i4) 
      if (allocated(cdiag))  deallocate(cdiag)
      allocate(cimat((lab*(lab + 1))/2 + 9), diag(lab), cdiag(lab))
      if (getmic .or. limci/=0) then 
        do i = 1, lab 
          diag(i) = diagi(microa(1,i),microb(1,i),eiga,xy,nmos) - gse 
          cdiag(i) = cdiagi 
        end do 
        go to 400 
      endif       
      j = 0 
      i = 0 
      do i1 = 1, lima 
        do i2 = 1, limb 
          i = i + 1 
          cimat(i) = diagi(nperma(1,i1),npermb(1,i2),eiga,xy,nmos) - gse 
        end do 
      end do 
      if (lab <= mdim) then 
        k = 0 
        do i = 1, lima 
          do j = 1, limb 
            k = k + 1 
            diag(k) = cimat(k) 
            l = 1 
            if (nmos > 0) then 
              microa(:nmos,k) = nperma(:nmos,i) 
              microb(:nmos,k) = npermb(:nmos,j) 
              l = nmos + 1 
            endif 
          end do 
        end do 
      else 
!
!   SELECT THE MDIM LOWEST MICROSTATES
!
        do lab = 1, mdim 
          x = 1000.D0 
          i = 0 
          do i1 = 1, lima 
            do i2 = 1, limb 
              i = i + 1 
              if (cimat(i) >= x) cycle  
              x = cimat(i) 
              j = i 
              j1 = i1 
              j2 = i2 
            end do 
          end do 
!
!   MICROSTATE J IS THE LOWEST IN THE REMAINING SET
!
          k = 1 
          if (nmos > 0) then 
            microa(:nmos,lab) = nperma(:nmos,j1) 
            microb(:nmos,lab) = npermb(:nmos,j2) 
            k = nmos + 1 
          endif 
          diag(lab) = cimat(j) 
          cimat(j) = 1.D8 
        end do 
        lab = mdim 
      endif 
  400 continue 
      do i = 1, lab 
        k = 0 
        x = 0.D0 
        do j = 1, nmos 
          x = x + microa(j,i)*microb(j,i) 
          k = k + microa(j,i) 
        end do 
        nalmat(i) = k 
        spin(i) = 4.D0*x - (xx - 2*nalmat(i))**2 
      end do 
!
!   BEFORE STARTING, CHECK THAT THE ROOT WANTED CAN EXIST
!
      if (lab < lroot) then 
        write (iw, &
      '(2/10X,''C.I. IS OF SIZE LESS THAN '',               ''ROOT SPECIFIED'')&
      ') 
        write (iw, '(10X,''MODIFY SIZE OF C.I. OR ROOT NUMBER'')') 
        meci = 0.D0 
        call mopend (&
       'C.I. IS OF SIZE LESS THAN ROOT SPECIFIED. MODIFY SIZE OF C.I. OR ROOT NU&
       &MBER.') 
        return  
      endif 
      if (prnt) then 
        write (iw, &
      '(/,'' CONFIGURATIONS CONSIDERED IN C.I.      '',/              '' M.O. N&
      &UMBER :      '',19I4)') (i,i=nelec + 1,nelec + nmos) 
        write (iw, '(''          ENERGY'')') 
        do i = 1, lab 
          write (iw, '(/10X,I4,6X,19I4)') i, (microa(k,i),k=1,nmos) 
          write (iw, '(6X,F10.4,4X,19I4)') diag(i), (microb(k,i),k=1,nmos) 
        end do 
      endif 
      call mecih (diag, cimat, nmos, lab, xy) 
      if (bigprt) then 
        write (iw, '(2/,'' C.I. MATRIX'')')  
        call vecprt (cimat, (-lab)) 
      else 
        if (prnt) then 
          write (iw, '(2/,'' DIAGONAL OF C.I. MATRIX'')') 
          write (iw, '(5F13.6)') (cimat((i*(i+1))/2),i=1,lab) 
        endif 
      endif 
      labsiz = min(lab,lroot + 10) 
      if (last==3 .and. prnt2) labsiz = lab 
      call rsp (cimat, lab, labsiz, eig, conf) 
!
!   DECIDE WHICH ROOT TO EXTRACT
!
      kroot = 0 
      if (smult < (-0.1D0)) then 
        meci = eig(lroot) 
        i = 0 
        l = min(lroot + 6,lab) 
        do k = lroot, l 
          if (abs(eig(k)-eig(lroot)) > 0.1D0) go to 450 
          j = 1 
          if (lab > 0) then 
            vectci(i+1:lab+i) = conf(1+lab*(k-1):lab*k) 
            j = lab + 1 
            i = lab + i 
          endif 
        end do 
        k = l + 1 
  450   continue 
        nstate = k - lroot 
        kroot = lroot 
      endif 
      if (last > 0) call symtrz (c(1,nelec+1), eig, 3, .TRUE.) 
      if (bigprt) then 
        write (iw, '(2/20X,''STATE VECTORS'',2/)') 
        if (last == 3) then 
          call matou1 (conf, eig, lab, lab, lab, 5) 
        else 
          call matout (conf, eig, lab, lab, lab) 
        endif 
      endif 
      if (prnt) then 
        write (iw, &
      '(/,''  STATE       ENERGY (EV)'',                    ''        Q.N.  SPI&
      &N   SYMMETRY     POLARIZATION'')') 
        write (iw, &
      '(''         ABSOLUTE    RELATIVE'',                  28X,''X       Y    &
      &   Z'',/)') 
      endif 
      iroot = 0 
      cimat = 0.1D0 
      do i = 1, labsiz 
        if (last == 0) then 
          namo(i) = ' ' 
          jndex(i) = i 
        endif 
        x = 0.5D0*xx 
        ii = (i - 1)*lab 
        do j = 1, lab 
          ji = j + ii 
          x = x - conf(ji)*conf(ji)*spin(j)*0.25D0 
          k = ispqr(j,1) 
          if (k == 1) cycle  
          l = 2 
          if (k - 1 > 0) then 
            do l = 1, k - 1 
              x = x + conf(ji)*conf(ispqr(j,l+1)+ii)*2.D0 
            end do 
            l = k + 1 
          endif 
        end do 
        y = (-1.D0 + sqrt(1.D0 + 4.D0*x))*0.5D0 
        if (abs(smult - x) < 0.01D0) then 
          iroot = iroot + 1 
          if (iroot == lroot) then 
            kroot = i 
            meci = eig(i) 
            m = 0 
            do k = i, min(lab,i + 4) 
              if (abs(eig(k)-eig(i)) > 1.D-1) go to 510 
              j = 1 
              if (lab > 0) then 
                vectci(m+1:lab+m) = conf(1+lab*(k-1):lab*k) 
                j = lab + 1 
                m = lab + m 
              endif 
            end do 
            k = min(lab,i + 4) + 1 
  510       continue 
            nstate = k - i 
          endif 
        endif 
        ispin(i) = nint(y*2.D0 + 1) 
        cimat(j) = cimat(j) + 1 
      end do 
      if (kroot < 1) then 
        write (iw, *) ' ROOT REQUESTED DOES NOT EXIST IN C.I.'  
        meci = 0.D0 
        call mopend ('ROOT REQUESTED DOES NOT EXIST IN C.I.') 
        return  
      endif 
      state(3) = namo(kroot) 
      state(1) = tspin(ispin(kroot))(:4) 
      state(2) = tspin(ispin(kroot))(5:) 
      if (prnt) then 
        call ciosci (c(1,nelec+1), oscil, conf) 
        deltap = 0.d0
        delta = 0.d0
        if (iseps) call dmecip (c, deltap, delta, cdiag, cif2, eig, vectci, conf) 
        do i = 1, labsiz 
          do j = i + 1, i + 4 
            if (.not.(eig(j)-eig(j-1)>1.D-4 .or. jndex(j)/=jndex(j-1) .or. namo&
              (j)/=namo(j-1))) cycle  
            exit  
          end do 
          work = 0.D0 
          do k = i, j - 1 
            work = work + oscil(:,k)**2 
            l = 4 
          end do 
          if (work(1) + work(2) + work(3) > 1.D-9) then 
            k = 3 
          else 
            k = 0 
          endif 
          if (i > 1) then 
            if (eig(i) - eig(i-1)>1.D-4 .or. jndex(i)/=jndex(i-1) .or. namo(i)&
              /=namo(i-1)) write (iw, 580) i, eig(i), eig(i) - eig(1), jndex(i)&
              , tspin(ispin(i)), namo(i), (sqrt(work(j)),j=1,k) 
          else 
            write (iw, 580) i, eig(i), eig(i) - eig(1), jndex(i), tspin(ispin(i&
              )), namo(i), (sqrt(work(j)),j=1,k) 
          endif 
        end do 
      endif 
      if (prnt .and. msdel/=0) write (iw, '(A)') &
        ' ''RELATIVE ENERGY'' IS RELATIVE TO LOWEST STATE CALCULATED.', &
        ' THIS MAY OR MAY NOT BE THE GROUND STATE' 
  580 format(i5,2f12.6,i6,3x,a8,4x,a4,3f8.4) 
      if (kroot == 0) then 
        write (iw, '(2/10X,''THE STATE REQUIRED IS NOT PRESENT IN THE'')') 
        write (iw, '(10X,  ''    SET OF CONFIGURATIONS AVAILABLE'')') 
        write (iw, &
      '(/ 4X,''NUMBER OF STATES ACCESSIBLE '',              ''USING CURRENT KEY&
      &-WORDS'',/)') 
        do i = 1, 7 
          if (cimat(i) <= 0.5D0) cycle  
          write (iw, '((24X,A8,I4))') tspin(i), nint(cimat(i)) 
        end do 
        meci = 0.D0 
        call mopend (&
       'THE STATE REQUIRED IS NOT PRESENT IN THE SET OF CONFIGURATIONS AVAILABLE') 
        return  
      endif 
      maxvec = 0 
      if (lspin) maxvec = kroot + min(3,lab - kroot) 
      if (lspin .and. (ne/2)*2==ne) write (iw, &
        '(''   ESR SPECIFIED FOR AN EVEN-ELECTRON SYSTEM'')') 
      do iuj = kroot, maxvec 
        iofset = (iuj - 1)*lab 
        write (iw, &
      '(2/,''      MICROSTATE CONTRIBUTIONS TO '',          ''STATE EIGENFUNCTI&
      &ON'',I3)') iuj 
        write (iw, '(5F13.6)') (conf(i+iofset),i=1,lab) 
        do i = 1, lab 
          conf(i) = conf(i+iofset)**2 
        end do 
!                                             SECOND VECTOR!
        do i = 1, nmos 
          sum = 0.D0 
          do j = 1, lab 
            sum = sum + (microa(i,j)-microb(i,j))*conf(j) 
          end do 
          eiga(i) = sum 
        end do 
        write (iw, &
          '(/,''    SPIN DENSITIES FROM EACH M.O., ENERGY:''    ,F7.3)') eig(&
          iuj) 
        write (iw, '(5F12.6)') (eiga(i),i=1,nmos) 
        write (iw, *) 
        write (iw, *) '     SPIN DENSITIES FROM EACH ATOMIC ORBITAL' 
        write (iw, '(30X,A)') 'S        PX        PY        PZ        TOTAL' 
        do i = 1, numat
          il = nfirst(i) 
          iu = nlast(i) 
          l = 0 
          summ = 0.D0 
          do k = il, iu 
            l = l + 1 
            sum = 0.D0 
            do j = 1, nmos 
              sum = sum + c(k,j+nelec)**2*eiga(j) 
            end do 
            summ = summ + sum 
            eigs(l) = sum 
          end do 
          if (l == 4) then 
            write (iw, '(''  ATOM'',I4,''    SPIN DENSITY  '',5F10.7)') i, (&
              eigs(k),k=1,l), summ 
          else 
            write (iw, &
              '(''  ATOM'',I4,''    SPIN DENSITY  '',         F10.7,30X,F10.7)'&
              ) i, eigs(1), summ 
          endif 
        end do 
      end do 
      first1 = .FALSE. 
      return  
      end function meci 
