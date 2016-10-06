      subroutine moldat(mode) 
!-----------------------------------------------
!
!   If mode == 1 run silently
!   If mode == 2 generate normal MOPAC output
!
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : nfirst, nlast, nat, uspd, &
      & pdiag, labels, coord, geo
      USE molmec_C, only : nnhco, nhco, htype
      USE molkst_C, only : natoms, norbs, nalpha, nbeta, nclose, nopen, &
      & nelecs, fract, numat, mpack, keywrd, n2elec, lm61, moperr, lm6, &
      & uhf, method_mndod, method_am1, method_pm3, method_mndo
      USE parameters_C, only : natorb,ios, iop, iod, uss, upp, udd, tore
      USE symmetry_C, only : name
      use meci_C, only: nmos
      USE euler_C, only : id
      USE chanel_C, only : iw
!***********************************************************************
!
!       MOLDAT works out essential molecular data, such as orbital
!              counters (NFIRST, NLAST), number of electrons,
!              starting populations, etc.
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.4G  18:38:35  03/15/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use mopend_I 
      use refer_I 
      use gmetry_I 
      use symtrz_I 
      use vecprt_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mode 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: kharge, i, ndorbs, ia, ib, nheavy, nnull, ii, &
        k, k1, j, ielec, ilevel, ndoubl, msdel, ne, nupp, &
        ndown, l, iminr, jminr, ij, ji, jk, kj, kl, lk, m, mk, km, jj, icount, &
        ireal, jreal, ni, n1, n4, n9
      real(double) :: elecs,  c(1), yy, w, sum, rmin   ! c(1) is for a dummy call
      logical :: debug, exci, trip, birad, halfe, slow
      real(double), dimension ((numat*(numat + 1))/2) :: rxyz
!-----------------------------------------------
      debug = index(keywrd,'MOLDAT') /= 0 
!
!   SPECIAL MODIFIERS FOR LIMITATIONS IN AVAILABLE PARAMETERS
!
      kharge = 0 
      i = index(keywrd,'CHARGE') 
      if (i /= 0) kharge = nint(reada(keywrd,i)) 
      elecs = -kharge 
      ndorbs = 0 
      if (index(keywrd,' EXTERNAL')/=0 .and. mode/=1) return  
      if (uss(1) > (-1.D0)) then 
        write (iw, &
      '(''  THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN'' ,'' THIS PROGRAM'')&
      ') 
      call mopend (&
           'THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN THIS PROGRAM') 
        return  
      endif 
      numat = 0 
      ia = 1 
      ib = 0 
      nheavy = 0 
      nnull = 0 
      do ii = 1, natoms 
        if (labels(ii)/=99 .and. labels(ii)/=107) then 
          numat = numat + 1 
          nat(numat) = labels(ii) 
          nfirst(numat) = ia 
          ni = nat(numat) 
          elecs = elecs + tore(ni) 
          ib = ia + natorb(ni) - 1 
          if (natorb(ni) == 9) ndorbs = ndorbs + 5 
          nlast(numat) = ib 
          uspd(ia) = uss(ni) 
          if (ia /= ib) then 
            k = ia + 1 
            k1 = ia + 3 
            do j = k, k1 
              uspd(j) = upp(ni) 
            end do 
            if (ib > ia) then 
              nheavy = nheavy + 1 
            else 
              nnull = nnull + 1 
            endif 
            if (k1 /= ib) then 
              k = k1 + 1 
              uspd(k:ib) = udd(ni) 
            endif 
          endif 
        endif 
        ia = ib + 1 
      end do 
      if (numat == 1) then 
        if (index(keywrd,'FORCE') /= 0) then 
          write (iw, '(3/,A)') '      A SINGLE ATOM HAS NO VIBRATIONAL MODES' 
          call mopend ('A SINGLE ATOM HAS NO VIBRATIONAL MODES') 
          return  
        endif 
      endif 
      if (mode /= 1) call refer 
      call gmetry (geo, coord) 
      call symtrz (c, pdiag, 1, .FALSE.) 
      if (moperr) return  
      write (iw, '(2/''      MOLECULAR POINT GROUP   :   '',A4)') name 
      n9 = 0
      n4 = 0
      n1 = 0
      do i = 1, natoms
      j = labels(i)
      if (j > 0 .and. j /= 99 .and. j /= 107) then
        k = natorb(j)
        if (k == 1) then
          n1 = n1 + 1
        else if (k == 4) then
          n4 = n4 + 1
        else if (k == 9) then
          n9 = n9 + 1
        end if
      end if
      end do
      if (method_mndod) then
        n2elec = 45*n9 + 10*n4 + n1
        lm6 = n2elec
        lm61 = 0
      else
         n2elec = 2025*n9+100*n4+n1+2025*(n9*(n9-1))/2+450*n9*n4+45*n9*n1+&
     & 100*(n4*(n4-1))/2+10*n4*n1+ (n1*(n1-1))/2+10
         lm61 = 45*n9 + 10*n4 + n1
         lm6 = 0
       end if

      norbs = nlast(numat) 
      mpack = (norbs*(norbs+1))/2
!
!   NOW TO CALCULATE THE NUMBER OF LEVELS OCCUPIED
!
      trip = index(keywrd,'TRIP') /= 0 
      exci = index(keywrd,'EXCI') /= 0 
      birad = exci .or. index(keywrd,'BIRAD')/=0 
      if (index(keywrd,'C.I.')/=0 .and. uhf) then 
        write (iw, '(2/10X,''C.I. NOT ALLOWED WITH UHF '')')
        call mopend ('C.I. NOT ALLOWED WITH UHF')  
        return  
      endif 
!
! NOW TO WORK OUT HOW MANY ELECTRONS ARE IN EACH TYPE OF SHELL
!
      nalpha = 0 
      nbeta = 0 
!
!      PROTECT DUMB USERS FROM DUMB ERRORS!
!
      nelecs = nint(max(elecs,0.D0)) 
      nelecs = min(2*norbs,nelecs) 
      nclose = 0 
      nopen = 0 
      fract = 0.D0 
      if (uhf) then 
        fract = 1.D0 
        nbeta = nelecs/2 
        if (trip) then 
          if (nbeta*2 /= nelecs) then 
            write (iw, &
      '(2/10X,''TRIPLET SPECIFIED WITH ODD NUMBER'',              '' OF ELECTRO&
      &NS, CORRECT FAULT '')')
           call mopend (&
               'TRIPLET SPECIFIED WITH ODD NUMBER OF ELECTRONS, CORRECT FAULT')  
            return  
          else 
            if (mode /= 1) write (iw, '(2/'' TRIPLET STATE CALCULATION'')') 
            nbeta = nbeta - 1 
          endif 
        endif 
        if (index(keywrd,'QUAR') /= 0) then 
          if (nbeta*2 == nelecs) then 
            write (iw, &
      '(2/10X,''QUARTET SPECIFIED WITH EVEN NUMBER'',             '' OF ELECTRO&
      &NS, CORRECT FAULT '')') 
            call mopend (&
               'QUARTET SPECIFIED WITH EVEN NUMBER OF ELECTRONS, CORRECT FAULT')
            return  
          else 
            if (mode /= 1) write (iw, '(2/'' QUARTET STATE CALCULATION'')') 
            nbeta = nbeta - 1 
          endif 
        endif 
        if (index(keywrd,'QUIN') /= 0) then 
          if (nbeta*2 /= nelecs) then 
            write (iw, &
      '(2/10X,''QUINTET SPECIFIED WITH ODD NUMBER'',              '' OF ELECTRO&
      &NS, CORRECT FAULT '')') 
            call mopend (&
               'QUINTET SPECIFIED WITH ODD NUMBER OF ELECTRONS, CORRECT FAULT')  
            return  
          else 
            if (mode /= 1) write (iw, '(2/'' QUINTET STATE CALCULATION'')') 
            nbeta = nbeta - 2 
          endif 
        endif 
        if (index(keywrd,'SEXT') /= 0) then 
          if (nbeta*2 == nelecs) then 
            write (iw, &
      '(2/10X,''SEXTET SPECIFIED WITH EVEN NUMBER'',              '' OF ELECTRO&
      &NS, CORRECT FAULT '')')
            call mopend (&
               'SEXTET SPECIFIED WITH EVEN NUMBER OF ELECTRONS, CORRECT FAULT')  
            return  
          else 
            if (mode /= 1) write (iw, '(2/'' SEXTET STATE CALCULATION'')') 
            nbeta = nbeta - 2 
          endif 
        endif 
        if (index(keywrd,'SEPT') /= 0) then 
          if (nbeta*2 /= nelecs) then 
            write (iw, &
      '(2/10X,''SEPTET SPECIFIED WITH ODD NUMBER'',               '' OF ELECTRO&
      &NS, CORRECT FAULT '')') 
            call mopend (&
               'SEPTET SPECIFIED WITH ODD NUMBER OF ELECTRONS, CORRECT FAULT') 
            return  
          else 
            if (mode /= 1) write (iw, '(2/'' SEPTET STATE CALCULATION'')') 
            nbeta = nbeta - 3 
          endif 
        endif 
        nalpha = nelecs - nbeta 
        if (mode /= 1) write (iw, &
      '(2/10X,''UHF CALCULATION, NO. OF ALPHA ELECTRONS ='',I  3,/27X,''NO. OF &
      &BETA  ELECTRONS ='',I3)') nalpha, nbeta 
      else 
!
!   NOW TO DETERMINE OPEN AND CLOSED SHELLS
!
        ielec = 0 
        ilevel = 0 
        if (mode==1 .and. trip) write (iw, '(2/'' TRIPLET STATE CALCULATION'')'&
          ) 
        if (exci .or. birad) then 
          if ((nelecs/2)*2 /= nelecs) then 
            write (iw, &
      '(2/10X,''SYSTEM SPECIFIED WITH ODD NUMBER'',               '' OF ELECTRO&
      &NS, CORRECT FAULT '')') 
            call mopend (&
               'SYSTEM SPECIFIED WITH ODD NUMBER OF ELECTRONS, CORRECT FAULT') 
            return  
          endif 
          if (mode /= 1) then 
            if (birad) write (iw, '(2/'' SYSTEM IS A BIRADICAL'')') 
            if (exci) write (iw, '(2/'' EXCITED STATE CALCULATION'')') 
          endif 
          ielec = 2 
          ilevel = 2 
        else if ((nelecs/2)*2 /= nelecs) then 
          ielec = 1 
          ilevel = 1 
        endif 
        if (index(keywrd,'QUAR') /= 0) then 
          if (mode /= 1) write (iw, '(2/'' QUARTET STATE CALCULATION'')') 
!#            IELEC=3
!#            ILEVEL=3
        endif 
        if (index(keywrd,'QUIN') /= 0) then 
          if (mode /= 1) write (iw, '(2/'' QUINTET STATE CALCULATION'')') 
!#            IELEC=4
!#            ILEVEL=4
        endif 
        if (index(keywrd,'SEXT') /= 0) then 
          if (mode /= 1) write (iw, '(2/'' SEXTET STATE CALCULATION'')') 
!#            IELEC=5
!#            ILEVEL=5
        endif 
        i = index(keywrd,'OPEN(') 
        if (i /= 0) then 
          j = index(keywrd(i:i+10),',') + i - 1 
          ilevel = nint(reada(keywrd,j)) 
          ielec = nint(reada(keywrd,index(keywrd,'OPEN(') + 5)) 
        endif 
        nclose = nelecs/2 
        nopen = nelecs - nclose*2 
        if (ielec /= 0) then 
          if ((nelecs/2)*2==nelecs .neqv. (ielec/2)*2==ielec) then 
            write (iw, &
              '('' IMPOSSIBLE NUMBER OF OPEN SHELL '',        ''ELECTRONS'')') 
            write (iw, '(A,I5)') ' NUMBER OF ELECTRONS IN SYSTEM:    ', nelecs 
            write (iw, '(A,I5)') ' NUMBER OF ELECTRONS IN OPEN SHELL:', ielec 
            call mopend ('IMPOSSIBLE NUMBER OF OPEN SHELL ELECTRONS') 
            return  
          endif 
          nclose = nclose - ielec/2 
          nopen = ilevel 
          if (nclose + nopen > norbs) then 
            write (iw, '(A)') &
              ' NUMBER OF DOUBLY FILLED PLUS PARTLY FILLED LEVELS' 
            write (iw, '(A)') ' GREATER THAN TOTAL NUMBER OF ORBITALS.' 
            call mopend (&
       'NUMBER OF DOUBLY FILLED PLUS PARTLY FILLED LEVELS GREATER THAN TOTAL NUM&
       &BER OF ORBITALS') 
            return  
          endif 
          if (ilevel == 0) then 
            write (iw, *) ' NUMBER OF M.O.s IN OPEN(M,N) IS ZERO!'
            call mopend (' NUMBER OF ELECTRONS IN OPEN(M,N) IS ZERO!')   
            return  
          endif 
          fract = ielec*1.D0/ilevel 
          if (nclose < 0) then 
            write (iw, '(A)') ' IMPOSSIBLE NUMBER OF CLOSED SHELLS' 
            call mopend ('IMPOSSIBLE NUMBER OF CLOSED SHELLS') 
            return  
          endif 
          if (mode /= 1) write (iw, &
            '(/6X,''THERE ARE'',I5,'' DOUBLY FILLED LEVELS'')') nclose 
        endif 
        if (mode /= 1) write (iw, &
      '(2/6X,''RHF CALCULATION, NO. OF '',     ''DOUBLY OCCUPIED LEVELS ='',I3)&
      ') nclose 
        if (mode/=1 .and. nopen/=0 .and. abs(fract-1.D0)<1.D-4) write (iw, &
          '(/23X,''NO. OF SINGLY OCCUPIED LEVELS ='',I3)') nopen 
        if (mode/=1 .and. nopen/=0 .and. abs(fract-1.D0)>1.D-4) write (iw, &
          '(/23X,''NO. OF LEVELS WITH OCCUPANCY'',F6.3,''  =''     ,I3)') fract&
          , nopen 
        i = index(keywrd,'C.I.=(') 
        if (i /= 0) then 
          j = index(keywrd(i:i+10),',') + i - 1 
          ndoubl = nint(reada(keywrd,j)) 
          if (ndoubl > nclose) then 
            write (iw, '(A,I3)') &
              '       NUMBER OF DOUBLY FILLED LEVELS IN C.I. RESET TO', nclose 
            ndoubl = nclose 
          endif 
          i = nint(reada(keywrd,index(keywrd,'C.I.=(') + 5)) 
          i = i - ndoubl 
          if (nopen > i) then 
            write (iw, &
      '(2/,'' NUMBER OF OPEN-SHELLS ALLOWED IN ''     ,''C.I. IS LESS '',/,    &
      &                                         ''    THAN THAT SPECIFIED BY OTH&
      &ER KEYWORDS'')') 
            call mopend (&
       'NUMBER OF OPEN-SHELLS ALLOWED IN C.I. IS LESS THAN THAT SPECIFIED BY OTH&
       &ER KEYWORDS') 
            return  
          endif 
          if (i + nclose > norbs) then 
            write (iw, '(A,/,A,I3)') &
      ' NUMBER OF M.O.s REQUESTED IN C.I. IS GREATER THAN THE NUMBER OF ORBITAL&
      &S' 
            call mopend (&
       'NUMBER OF M.O.s REQUESTED IN C.I. IS GREATER THAN THE NUMBER OF ORBITALS&
       ') 
            return  
          endif 
        endif 
        if (index(keywrd,'C.I.')/=0 .and. nopen==0) then 
          nopen = 1 
          nclose = nclose - 1 
          fract = 2.D0 
        endif 
        nopen = nopen + nclose 
      endif 
!
!  WORK OUT IF DEFINED SPIN-STATE ALLOWED
!
      msdel = index(keywrd,' MS') 
      if (msdel /= 0) then 
        msdel = nint(reada(keywrd,index(keywrd,' MS'))) 
      else 
        if (index(keywrd,'TRIP') + index(keywrd,'QUAR') > 0) msdel = 1 
        if (index(keywrd,'QUIN') + index(keywrd,'SEXT') > 0) msdel = 2 
        if (index(keywrd,'SEPT') > 0) msdel = 3 
      endif 
      if (msdel/=0 .and. .not.uhf) then 
!
!   MSDEL = NUMBER OF ALPHA ELECTRONS - NUMBER OF BETA ELECTRONS
!
        ndoubl = 99 
        i = index(keywrd,'C.I.=(') 
        if (i /= 0) then 
          j = index(keywrd(i:i+10),',') + i - 1 
          ndoubl = nint(reada(keywrd,j)) 
          nmos = nint(reada(keywrd,index(keywrd,'C.I.=(') + 5)) 
        else if (index(keywrd,'C.I.=') /= 0) then 
          nmos = nint(reada(keywrd,index(keywrd,'C.I.=') + 5)) 
        else 
          nmos = nopen - nclose 
          nmos = min0(norbs,nmos) 
        endif 
        if (ndoubl == 99) then 
          j = max(min((nclose + nopen + 1)/2 - (nmos - 1)/2,norbs - nmos + 1),1&
            ) 
        else 
          j = nclose - ndoubl + 1 
        endif 
        ne = int(max(0.D0,nclose - j + 1.D0)*2.D0 + max(0.D0,(nopen - nclose)*&
          fract) + 0.5D0) 
        nupp = (ne + 1)/2 + msdel 
        ndown = ne - nupp 
!
!  NUPP  = NUMBER OF ALPHA ELECTRONS IN ACTIVE SPACE
!  NDOWN = NUMBER OF BETA  ELECTRONS IN ACTIVE SPACE
!
        if (nupp*ndown<0 .or. nupp>nmos .or. ndown>nmos) then 
          write (iw, '(A)') &
            ' SPECIFIED SPIN COMPONENT NOT SPANNED BY ACTIVE SPACE' 
          call mopend ('SPECIFIED SPIN COMPONENT NOT SPANNED BY ACTIVE SPACE') 
          return  
        endif 
      endif 
!#      WRITE(IW,'(''  NOPEN,NCLOSE,NALPHA,NBETA,FRACT'',4I4,F12.5)')
!#     1 NOPEN, NCLOSE, NLAPHA, NBETA, FRACT
!
!   MAKE SURE ANALYT IS NOT USED WITH ANALYTICAL C.I. DERIVATIVES
!
      halfe = nopen>nclose .and. Abs(fract -2.D0) > 1.d-20 .and. Abs(fract) > 1.d-20 &
      & .or. index(keywrd,'C.I.')/=0 
      slow = index(keywrd,'EXCI')/=0 .or. index(keywrd,'ROOT')/=0 .and. index(&
        keywrd,'ROOT=1')==0 
      if (halfe) halfe = .not.slow 
      if (index(keywrd,'NOANCI')==0 .and. index(keywrd,'ANALYT')/=0 .and. halfe&
        ) then 
        write (iw, *) 
        write (iw, '(A)') ' KEYWORD ''ANALYT'' CANNOT BE USED HERE: ', &
          ' ANALYICAL C.I. DERIVATIVES MUST USE FINITE DIFFERENCES', &
          ' TO CORRECT, REMOVE KEYWORD ''ANALYT'' OR ADD ''NOANCI''' 
        call mopend ('KEYWORD "ANALYT" CANNOT BE USED HERE') 
        return  
      endif 
      if (halfe .and. id/=0 .and. index(keywrd,' NOANCI')+index(keywrd,' 0SCF')&
        ==0) then 
        write (iw, *) 
        write (iw, *) ' `NOANCI'' MUST BE USED FOR RHF OPEN-SHELL SYSTEMS' 
        write (iw, *) ' THAT INVOLVE TRANSLATION VECTORS'  
        call mopend (&
       '"NOANCI" MUST BE USED FOR RHF OPEN-SHELL SYSTEMS THAT INVOLVE TRANSLATIO&
       &N VECTORS') 
        return  
      endif 
      yy = dble(kharge)/(norbs + 1.D-10) 
      pdiag(:norbs) = 0.D0 
      do i = 1, numat 
        ni = nat(i) 
        j = nfirst(i) - 1 
        l = nlast(i) - j 
        if (l == 0) cycle  
        w = ios(ni) - yy 
        j = j + 1 
        pdiag(j) = w 
        if (l == 1) cycle  
        w = iop(ni)/3.D0 - yy 
        pdiag(j+1:3+j) = w 
        j = 3 + j 
        if (l == 4) cycle  
        w = iod(ni)/5.D0 - yy 
        pdiag(j+1:5+j) = w 
      end do 
      pdiag(:norbs) = 0.D0 
      do i = 1, numat 
        ni = nat(i) 
        if (nlast(i) - nfirst(i) == (-1)) cycle  
        l = nfirst(i) - 1 
        if (nlast(i) - nfirst(i) == 0) then 
!
!    Hydrogen
!
          l = l + 1 
          pdiag(l) = tore(ni) - yy 
        else if (nlast(i) - nfirst(i) == 3) then 
!
!    Normal heavy atom
!
          w = tore(ni)*0.25D0 - yy 
          pdiag(l+1:4+l) = w 
        else 
!
!   This atom has a 'd' shell
!
          if (ni<21 .or. ni>30 .and. ni<39 .or. ni>48 .and. ni<57) then 
!
!   Main Group Element:  The "d" shell is formally empty.
!
            w = tore(ni)*0.25D0 - yy 
            pdiag(l+1:4+l) = w 
            l = 4 + l 
            pdiag(l+1:5+l) = -yy 
          else if (ni < 99) then 
!
!   Transition metal
!
            sum = tore(ni) - 9*yy 
!   First, put 2 electrons in the 's' shell
            l = l + 1 
            pdiag(l) = max(0.D0,min(sum,2.D0)) 
            sum = sum - 2.D0 
            if (sum > 0.D0) then 
!
!   Now put as many electrons as possible into the 'd' shell
!
              l = l + 3 
              do j = 1, 5 
                l = l + 1 
                pdiag(l) = max(0.D0,min(sum*0.2D0,2.D0)) 
              end do 
              sum = sum - 10.D0 
              if (sum > 0) then 
!
!   Put the remaining electrons in the 'p' shell
!
                l = l - 8 
                pdiag(l+1:3+l) = sum/3.D0 
              endif 
            endif 
          endif 
        endif 
      end do 
!
!   WRITE OUT THE INTERATOMIC DISTANCES
!
      rmin = 100.D0 
      l = 0 
      do i = 1, numat 
        do j = 1, i 
          l = l + 1 
          rxyz(l) = sqrt((coord(1,i)-coord(1,j))**2+(coord(2,i)-coord(2,j))**2+&
            (coord(3,i)-coord(3,j))**2) 
          if (.not.(rmin>rxyz(l) .and. i/=j .and. (nat(i)<103 .or. nat(j)<103))&
            ) cycle  
          iminr = i 
          jminr = j 
          rmin = rxyz(l) 
        end do 
      end do 
      nnhco = 0 
!
!   SET UP MOLECULAR-MECHANICS CORRECTION TO -(C=O)-(NH)- LINKAGE
!   THIS WILL BE USED IF MMOK HAS BEEN SPECIFIED.
!
        if (method_mndo) htype = 6.1737D0 
        if (method_am1)  htype = 3.3191D0 
        if (method_pm3)  htype = 7.1853D0 
      ii = 0 
      if (index(keywrd,'NOMM') /= 0) ii = 1 
!
!   IDENTIFY O=C-N-H SYSTEMS VIA THE INTERATOMIC DISTANCES MATRIX
      l230: do j = 1, numat 
        if (nat(j) /= 6) cycle  l230 
        do i = 1, numat 
          if (nat(i) /= 8) cycle  
          ij = max(i,j) 
          ji = i + j - ij 
          if (rxyz((ij*(ij-1))/2+ji) > 1.3D0) cycle  
          do k = 1, numat 
            if (nat(k) /= 7) cycle  
            jk = max(j,k) 
            kj = j + k - jk 
            if (rxyz((jk*(jk-1))/2+kj) > 1.6D0) cycle  
            do l = 1, numat 
              if (nat(l) /= 1) cycle  
              kl = max(k,l) 
              lk = k + l - kl 
              if (rxyz((kl*(kl-1))/2+lk) > 1.3D0) cycle  
!
!   WE HAVE A H-N-C=O SYSTEM.  THE ATOM NUMBERS ARE L-K-J-I
!   NOW SEARCH OUT ATOM ATTACHED TO NITROGEN, THIS SPECIFIES
!   THE SYSTEM X-N-C=O
!
              l190: do m = 1, numat 
                if (m==k .or. m==l .or. m==j) cycle  l190 
                mk = max(m,k) 
                km = m + k - mk 
                if (rxyz((mk*(mk-1))/2+km) > 1.7D0) cycle  l190 
                do jj = 1, nnhco, 2 
                  if (nhco(3,jj) /= k) cycle  
                  cycle  l190 
                end do 
                nnhco = nnhco + 1 
                nhco(1,nnhco) = i 
                nhco(2,nnhco) = j 
                nhco(3,nnhco) = k 
                nhco(4,nnhco) = m 
                nnhco = nnhco + 1 
                nhco(1,nnhco) = i 
                nhco(2,nnhco) = j 
                nhco(3,nnhco) = k 
                nhco(4,nnhco) = l 
                if (ii /= 0) then 
                  ii = ii + 2 
                  nnhco = nnhco - 2 
                endif 
                cycle  l230 
              end do l190 
            end do 
          end do 
        end do 
      end do l230 
      if (mode/=1 .and. ii>1) then 
        write (iw, '(A,I2,2A)') ' THERE ARE ', ii/2, &
          ' PEPTIDE LINKAGES IDENTIFIED IN THIS SYSTEM' 
        write (iw, '(A)') &
      ' IF YOU WANT MM CORRECTION TO THE CONH BARRIER, ADD THE KEY-WORD "MMOK"' 
      endif 
      if (mode/=1 .and. nnhco/=0) then 
        if (index(keywrd,'MMOK') /= 0) then 
          write (iw, '(A)') &
            ' MOLECULAR MECHANICS CORRECTION APPLIED TO PEPTIDE LINKAGE' 
        else if (index(keywrd,'NOMM') /= 0) then 
          nnhco = 0 
        else 
          write (iw, '(A)') ' THIS SYSTEM CONTAINS -HNCO- GROUPS.' 
          write (iw, '(A)') &
      ' YOU MUST SPECIFY "NOMM" OR "MMOK" REGARDING MOLECULAR MECHANICS CORRECT&
      &ION' 
          write (iw, '(A)') '   LINK    C    N' 
          write (iw, '(I6,I6,I5)') (i/2,(nhco(j,i),j=2,3),i=2,nnhco,2)
          if (index(keywrd,' 0SCF') == 0) call mopend (&
       'THIS SYSTEM CONTAINS -HNCO- GROUPS. YOU MUST SPECIFY "NOMM" OR "MMOK"')  
          if (index(keywrd,' 0SCF') == 0) return  
        endif 
      endif 
      if (mode/=1 .and. index(keywrd,'NOINT')==0) then 
        write (iw, '(2/10X,''  INTERATOMIC DISTANCES'')') 
        call vecprt (rxyz, numat) 
      endif 
      if (rmin<0.8D0 .and. index(keywrd,'GEO-OK')==0) then 
        icount = 0 
        ireal = iminr 
        jreal = jminr 
        do i = 1, natoms 
          if (labels(i)==99 .or. labels(i)==107) cycle  
          icount = icount + 1 
          if (icount == iminr) ireal = i 
          if (icount /= jminr) cycle  
          jreal = i 
        end do 
        write (iw, 250) ireal, jreal, rmin 
  250   format(/,/,'   ATOMS',i3,' AND',i3,' ARE SEPARATED BY',f8.4,&
          ' ANGSTROMS.',/,'   TO CONTINUE CALCULATION SPECIFY "GEO-OK"') 
        write (iw, 260) 
  260   format(/,'   NOTE THAT THE ATOM NUMBERS CORRESPOND TO THE',/,&
          '   ''CARTESIAN COORDINATES'' ATOM LIST.') 
        if (index(keywrd,' 0SCF') == 0) then 
        call mopend (&
             'Error in MOLDAT.  TO CONTINUE CALCULATION SPECIFY "GEO-OK".') 
          return  
        endif 
      endif 
      if (.not.debug) return  
      write (iw, 290) numat, norbs, ndorbs, natoms 
  290 format('   NUMBER OF REAL ATOMS:',i4,/,'   NUMBER OF ORBITALS:  ',i4,/,&
        '   NUMBER OF D ORBITALS:',i4,/,'   TOTAL NO. OF ATOMS:  ',i4) 
      write (iw, 300) (uspd(i),i=1,norbs) 
  300 format('   ONE-ELECTRON DIAGONAL TERMS',/,10(/,10f8.3)) 
      write (iw, 310) (pdiag(i),i=1,norbs) 
  310 format('   INITIAL P FOR ALL ATOMIC ORBITALS',/,10(/,10f8.3)) 
      return  
      end subroutine moldat 
