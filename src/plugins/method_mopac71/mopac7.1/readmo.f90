      subroutine readmo 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!--TGAY 08/2016 - Added ilog to use list
      use chanel_C, only : iw, ir, iarc, ilog
      USE maps_C, ONLY: latom, lparam, lpara1, latom1, lpara2, latom2 
      USE symmetry_C, ONLY: idepfn, locdep, depmul, locpar 
      use molkst_C, only : ndep, numat, numcal, natoms, nvar, keywrd, &
      & verson, method_mndo, method_am1, method_pm3, is_PARAM, &
      & method_mndod, method_dorbs, moperr, maxatoms, koment, title, &
      jobnam, isok
      use elemts_C, only : elemnt
      use permanent_arrays, only : xparam, loc, labels, na, nb, nc, &
      & geo, coord
      USE parameters_C, only : natorb, natsp, natspd
  
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:00  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use gettxt_I 
      use mopend_I 
      use getgeg_I 
      use getgeo_I 
      use xyzint_I 
      use geout_I 
      use wrtkey_I 
      use getsym_I 
      use symtry_I 
      use nuchar_I 
      use wrttxt_I 
      use gmetry_I 
      use maksym_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
       double precision, dimension (400) :: react
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(:,:), allocatable :: lopt 
      integer :: il = 0, ireact 
      integer , dimension(18,2) :: idepco 
      integer :: naigin, i, j, k, iflag, nreact, ij, iend, l 
      real(double), dimension(40) :: value 
      real(double), dimension(400) :: xyzt 
      real(double) :: degree, convrt, dum1, dum2 
      logical :: int = .true., aigeo, xyz 
      character :: line*80, banner*80 
      character :: space, space2*2, ch, ch2*2, idate*24 
      save  space, space2, int, ireact, lopt
!-----------------------------------------------
!
! MODULE TO READ IN GEOMETRY FILE, OUTPUT IT TO THE USER,
! AND CHECK THE DATA TO SEE IF IT IS REASONABLE.
! EXIT IF NECESSARY.
!
!
!
!  ON EXIT NATOMS    = NUMBER OF ATOMS PLUS DUMMY ATOMS (IF ANY).
!          KEYWRD    = KEYWORDS TO CONTROL CALCULATION
!          KOMENT    = COMMENT CARD
!          TITLE     = TITLE CARD
!          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS.
!          GEO       = ARRAY OF INTERNAL COORDINATES.
!          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
!          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
!          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
!          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
!          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
!          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
!          REACT     = REACTION COORDINATE PARAMETERS
!          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMIZED.
!          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
!          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMIZED.
!          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMIZED.
!
!***********************************************************************
! *** IR THE TRIAL GEOMETRY  (IE.  KGEOM=0)
!   LABEL(I) = THE ATOMIC NUMBER OF ATOM(I).
!            = 99, THEN THE I-TH ATOM IS A DUMMY ATOM USED ONLY TO
!              SIMPLIFY THE DEFINITION OF THE MOLECULAR GEOMETRY.
!   GEO(1,I) = THE INTERNUCLEAR SEPARATION (IN ANGSTROMS) BETWEEN ATOMS
!              NA(I) AND (I).
!   GEO(2,I) = THE ANGLE NB(I):NA(I):(I) IR IN DEGREES; STORED IN
!              RADIANS.
!   GEO(3,I) = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND NA(I):(I)
!              IR IN DEGREES - STORED IN RADIANS.
!  LOPT(J,I) = -1 IF GEO(J,I) IS THE REACTION COORDINATE.
!            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMIZED
!            =  0 OTHERWISE.
! *** NOTE:    MUCH OF THIS DATA IS NOT INCLUDED FOR THE FIRST 3 ATOMS.
!     ATOM1  IR LABELS(1) ONLY.
!     ATOM2  IR LABELS(2) AND GEO(1,2) SEPARATION BETWEEN ATOMS 1+2
!     ATOM3  IR LABELS(3), GEO(1,3)    SEPARATION BETWEEN ATOMS 2+3
!              AND GEO(2,3)              ANGLE ATOM1 : ATOM2 : ATOM3
!
!***********************************************************************
!
      data space, space2/ ' ', '  '/  
      data naigin/ 0/  
      data idepco/ 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 2, &
        3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3/  
      write(0,*) "READMO1 = "
      aigeo = .FALSE. 
      nvar = 0 
      ndep = 0 
      if (.not. allocated(lopt)) allocate(lopt(3,maxatoms))
      write(0,*) "READMO2 = "
   10 continue 
      call gettxt 
      write(0,*) "READMO3 = "
      if (moperr) then
        natoms = 0
        return  
      end if
      if (index(keywrd,' ECHO') /= 0) then 
        rewind ir 
        if (.not.isok) then 
          write (iw, '(A)') ' ECHO is not allowed at this point'  
          call mopend ('ECHO is not allowed at this point') 
          return  
        endif 
        isok = .FALSE. 
        do i = 1, 1000 
          read (ir, '(A)', end=60) keywrd 
          do j = 80, 2, -1 
            if (keywrd(j:j) /= ' ') go to 30 
          end do 
          j = 1 
   30     continue 
          do k = 1, j 
            if (ichar(keywrd(k:k)) >= 32) cycle  
            keywrd(k:k) = '*' 
          end do 
          write (iw, '(1X,A)') keywrd(1:j) 
        end do 
   60   continue 
        rewind ir 
        call gettxt 
        if (moperr) return  
      endif 
      if (index(keywrd,' ECHO') /= 0) write (iw, '(''1'')') 
      if (keywrd(1:1) /= space) then 
        ch = keywrd(1:1) 
        keywrd(1:1) = space 
        do i = 2, 239 
          ch2 = keywrd(i:i) 
          keywrd(i:i) = ch 
          ch = ch2(1:1) 
          if (keywrd(i+1:i+2) /= space2) cycle  
          keywrd(i+1:i+1) = ch 
          go to 80 
        end do 
        ch2 = keywrd(240:240) 
        keywrd(240:240) = ch 
        keywrd(241:241) = ch2(1:1) 
   80   continue 
      endif 
      if (koment(1:1) /= space) then 
        ch = koment(1:1) 
        koment(1:1) = space 
        do i = 2, 79 
          ch2 = koment(i:i) 
          koment(i:i) = ch 
          ch = ch2(1:1) 
          if (koment(i+1:i+2) /= space2) cycle  
          koment(i+1:i+1) = ch 
          go to 100 
        end do 
        ch2 = koment(80:80) 
        koment(80:80) = ch 
        koment(81:81) = ch2(1:1) 
  100   continue 
      endif 
      if (title(1:1) /= space) then 
        ch = title(1:1) 
        title(1:1) = space 
        do i = 2, 79 
          ch2 = title(i:i) 
          title(i:i) = ch 
          ch = ch2(1:1) 
          if (title(i+1:i+2) /= space2) cycle  
          title(i+1:i+1) = ch 
          go to 120 
        end do 
        ch2 = title(80:80) 
        title(80:80) = ch 
        title(81:81) = ch2(1:1) 
  120   continue 
      endif 
      latom = 0 
      lparam = 0 
      xyz = index(keywrd,' XYZ') + index(keywrd,' IRC') + index(keywrd,' DRC')&
         /= 0 
!
!   Top level
!
      if (index(keywrd,' OLDGEO') == 0) then 
!
!  Read in a new geometry
!
      write(0,*) "READMO4 = ", nvar

        nvar = 0 
        ndep = 0 
        if (aigeo .or. index(keywrd,' AIGIN')/=0) then 
          call getgeg (ir, labels, geo, lopt, na, nb, nc) 
          if (moperr) return  
          if (xyz) then 
            write (iw, '(A)') &
              ' CARTESIAN CALCULATION NOT ALLOWED WITH GAUSSIAN INPUT'
              call mopend (&
               'CARTESIAN CALCULATION NOT ALLOWED WITH GAUSSIAN INPUT')  
            return  
          endif 
          if (nvar == 0) then 
            lopt(:,:natoms) = 0 
          endif 
        else 
          call getgeo (ir, labels, geo, coord, lopt, na, nb, nc, react, int) 
          if (moperr) return  
          if (natoms < 0) then 
            rewind ir 
            if (.not.isok) then 
              write (iw, '(A)') &
                ' Use AIGIN to allow more geometries to be used' 
                call mopend ('Use AIGIN to allow more geometries to be used') 
!
!   This is a deadly error - to prevent an infinite loop, kill the job.
!
              stop  
            endif 
            isok = .FALSE. 
            if (numcal /= 1) then 
              naigin = naigin + 1 
              write (iw, '(2/,2A)') '   GAUSSIAN INPUT REQUIRES', &
                ' STAND-ALONE JOB' 
              write (iw, '(/,A)') '   OR KEYWORD "AIGIN"'
              call mopend (&
                 'GAUSSIAN INPUT REQUIRES STAND-ALONE JOB OR KEYWORD "AIGIN"')               
              return  
            endif 
            aigeo = .TRUE. 
            go to 10 
          endif 
        endif 
        if (natoms == 0 .and. numcal > 1) then 
          write (iw, '(A)') 'NO ATOMS IN SYSTEM'
          call mopend ('NO ATOMS IN SYSTEM')  
          return  
        endif 
      else 
!
!   Use the old geometry
!
        if (.not.xyz) then 
          degree = 1.D0 
          if (na(1) == 99) then 
            lopt(:,:natoms) = 1 
            coord(:,:natoms) = geo(:,:natoms) 
            lopt(1,1) = 0 
            lopt(2,1) = 0 
            lopt(3,1) = 0 
            lopt(2,2) = 0 
            lopt(3,2) = 0 
            lopt(3,3) = 0 
            call xyzint (coord, numat, na, nb, nc, degree, geo) 
          else 
            geo(2:3,:natoms) = geo(2:3,:natoms)*degree 
          endif 
        endif 
      endif 
      if (index(keywrd,' FORCE')/=0 .and. labels(natoms)==107) then 
        do i = 1, na(natoms) 
          if (labels(i) /= 99) cycle  
          write (iw, '(A)') ' NO DUMMY ATOMS ALLOWED BEFORE TRANSLATION' 
          write (iw, '(A)') ' ATOM IN A FORCE CALCULATION'
          call mopend (&
       'NO DUMMY ATOMS ALLOWED BEFORE TRANSLATION ATOM IN A FORCE CALCULATION')  
          return  
        end do 
      endif 
      write(0,*) "READMOEND = ", nvar

!
!
! OUTPUT FILE TO UNIT 6
!
!    WRITE HEADER
      idate = ' ' 
      call fdate (idate) 
      write (iw, '(1X,15(''*****''),''****'')') 
!
!     CHANGE THE FOLLOWING LINE TO SUIT LOCAL ENVIRONMENT, IF DESIRED
!
      banner = &
      ' **  MOPAC: Public Domain Version              Written by James J. P. Stewart **' 
      if (1 == 1) then 
        write (iw, '(A)') banner 
!
!    THE BANNER DOES NOT APPEAR ANYWHERE ELSE.
!
        write (iw, '(1X,79(''*''))') 
      endif
      if (.not. is_PARAM) then
        method_am1   = (index(keywrd,' AM1') /= 0)
        method_pm3   = (index(keywrd,' PM3') /= 0)
        method_mndod = (index(keywrd,' MNDOD') /= 0)
        method_mndo = (.not.(method_am1 .or. method_pm3 .or. method_mndod))
        method_dorbs = method_mndod
      endif
      if (method_mndod) then 
        natorb = natspd
      else 
        natorb = natsp 
      endif
      line = '   MNDO' 
      if (method_am1)   line = '    AM1' 
      if (method_pm3)   line = '    PM3' 
      if (method_mndod) line = 'MNDO-d ' 
        write (iw, &
      '(/29X,A,'' CALCULATION RESULTS'',28X,3/1X,             15(''*****''),''*&
      &***'' )') line(:7) 
        write (iw, &
      '('' *'',10X,''         MOPAC  '',F5.2,                  15X,''CALC''''D.&
      & '',A)') verson, idate 
      if (na(1) /= 99) then 
        do i = 1, natoms 
          if (labels(i) <= 0) then 
            write (iw, '('' ATOMIC NUMBER OF '',I3,'' ?'')') labels(i) 
            if (i == 1) then 
              write (iw, '(A)') ' THIS WAS THE FIRST ATOM' 
            else 
              write (iw, '(A)') &
                '    GEOMETRY UP TO, BUT NOT INCLUDING, THE FAULTY ATOM' 
              natoms = i - 1 
              xparam(1) = -1.D0 
              call geout (iw) 
            endif  
            call mopend ('Error in READMO') 
            return  
          endif 
          if (.not.(na(i)>=i .or. nb(i)>=i .or. nc(i)>=i .or. na(i)==nb(i)&
             .and. i>1 .or. (na(i)==nc(i) .or. nb(i)==nc(i)) .and. i>2 .or. na(&
            i)*nb(i)*nc(i)==0 .and. i>3)) cycle  
          write (iw, '('' ATOM NUMBER '',I3,'' IS ILL-DEFINED'')') i 
          if (i == 1) then 
            return  
          endif 
          write (iw, '(/,''  GEOMETRY READ IN'',/)') 
          xparam(1) = -1.D0 
          call geout (iw) 
          call mopend ('Error in READMO')  
          return  
        end do 
      endif 
!
! WRITE KEYWORDS BACK TO USER AS FEEDBACK
      call wrtkey (keywrd) 
      if (moperr) return  
      if (1 == 1) write (iw, &
        '(1X,14(''*****''),''*********'')')
!
! FILL IN GEO MATRIX IF NEEDED
      if (index(keywrd,' SYMM')/=0 .and. ndep==0) then 
        call getsym(locpar, idepfn, locdep, depmul) 
        if (xyz .eqv. int) then 
          write (iw, *) ' SYMMETRY CANNOT BE USED WHEN COORDINATE SYSTEMS ', &
            'ARE CHANGED' 
          ndep = 0 
        endif 
      endif 
!
!   FORCE OPTIMIZATION FLAGS OFF FOR DEPENDENT COORDINATES
!
      j = 1 
      if (xyz) j = 2 
      do i = 1, ndep 
        lopt(idepco(idepfn(i),j),locdep(i)) = 0 
      end do 
      if (ndep /= 0) call symtry 
!
! INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      write(0,*) "READMO6 = ", nvar

      iflag = 0 
      latom = 0 
      numat = 0 
      if (nvar /= 0) then 
        numat = natoms 
      else 
        do i = 1, natoms 
          if (labels(i)/=99 .and. labels(i)/=107) numat = numat + 1 
          do j = 1, 3 
            if (lopt(j,i) > 0) go to 200 
            if (lopt(j,i) == 0) cycle  
            convrt = 1.D0 
            if (iflag /= 0) then 
              if (index(keywrd,' STEP1') /= 0) then 
                lpara1 = lparam 
                latom1 = latom 
                lpara2 = j 
                latom2 = i 
                latom = 0 
                iflag = 0 
                cycle  
              else 
                write (iw,'('' ONLY ONE REACTION COORDINATE PERMITTED'')') 
               call mopend ('ONLY ONE REACTION COORDINATE PERMITTED') 
                return  
              endif 
            endif 
            latom = i 
            lparam = j 
            if (j > 1) convrt = 0.01745329252D00 
            react(1) = geo(j,i) 
            ireact = 1 
            iflag = 1 
            cycle  
!    FLAG FOR OPTIMIZE
  200       continue 
            nvar = nvar + 1 
            loc(1,nvar) = i 
            loc(2,nvar) = j 
            xparam(nvar) = geo(j,i) 
          end do 
        end do 
      endif 
      write(0,*) "READMO7 = ", nvar

! READ IN PATH VALUES
      if (iflag /= 0) then 
        if (index(keywrd,' NLLSQ') /= 0) then 
          write (iw, '(A)') &
            ' NLLSQ USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED' 
          call mopend (&
             'NLLSQ USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED') 
          return  
        endif 
        if (index(keywrd,' SIGMA') /= 0) then 
          write (iw, '(A)') &
            ' SIGMA USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED'
          call mopend (&
             'SIGMA USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED')  
          return  
        endif 
        if (index(keywrd,' STEP=') + index(keywrd,' POINT=') /= 0) then 
          go to 250 
        endif 
  220   continue 
        read (ir, '(A)', end=240) line 
        call nuchar (line, value, nreact) 
        if (nreact == 0) go to 240 
        do i = 1, nreact 
          ij = ireact + i 
          react(ij) = value(i)*convrt 
          if (abs(react(ij)-react(ij-1)) >= 1.D-5) cycle  
          dum1 = react(ij)/convrt 
          dum2 = react(ij-1)/convrt 
          write (iw, &
      '(3/,'' TWO ADJACENT POINTS ARE IDENTICAL:  '',    F7.3,2X,F7.3,/,'' THIS&
      & IS NOT ALLOWED IN A PATH CALCULATION'')') dum1, dum2 
          call mopend (&
       'TWO ADJACENT POINTS ARE IDENTICAL: THIS IS NOT ALLOWED IN A PATH CALCULA&
       &TION') 
          return  
        end do 
        ireact = ireact + nreact 
        go to 220 
  240   continue 
        degree = 1.D0 
        if (lparam > 1) degree = 57.29577951308232D0 
        if (ireact <= 1) then 
          write (iw, &
      '(2/10X,'' NO POINTS SUPPLIED FOR REACTION PATH'')') 
          write (iw, '(2/10X,'' GEOMETRY AS READ IN IS AS FOLLOWS'')') 
          xparam(1) = -1.D0 
          call geout (1)
          call mopend ('NO POINTS SUPPLIED FOR REACTION PATH')  
          return  
        else 
          write (iw, '(2/10X,'' POINTS ON REACTION COORDINATE'')') 
          write (iw, '(10X,8F8.2)') (react(i)*degree,i=1,ireact) 
        endif 
        iend = ireact + 1 
        react(iend) = -1.D12 
      endif 
  250 continue 
      write(0,*) "READMO8 = ", nvar

      call wrttxt (iw) 
      write(0,*) "READMO9 = ", nvar

!
! CHECK DATA
!
      if (index(keywrd,' 0SCF') == 0) then 

        if (xyz) then 
          if (index(keywrd,' IRC') + index(keywrd,' DRC') + index(keywrd,&
            ' 1SCF') == 0) then 
            if (nvar/=0 .and. int .and. nvar<3*numat-6) then 
              write (iw, &
      '(2/10X,'' INTERNAL COORDINATES READ IN, AND'','' CALCULATION '',/10X,''T&
      &O BE RUN IN CARTESIAN COORDINATES, '',/10X,''BUT NOT ALL COORDINATES MARK&
      &ED FOR OPTIMISATION'')') 
              write (iw, &
      '(2/10X,'' THIS INVOLVES A LOGICALLY '',     ''ABSURD CHOICE'',/10X,'' SO&
      & THE CALCULATION IS '',               ''TERMINATED AT THIS POINT'')') 
              call mopend ('INCONSISTENT USE OF OPTIMIZATION FLAGS') 
              return  
            endif 
          endif 
        else 
          if (index(keywrd,' 1SCF')==0 .or. index(keywrd,' GRAD')/=0) then 
            if (.not.int .and. nvar/=0 .and. nvar<3*numat-6) then 
              write (iw, &
      '(2/10X,'' CARTESIAN COORDINATES READ IN, AND'','' CALCULATION '',/10X,''&
      &TO BE RUN IN INTERNAL COORDINATES, '',/10X,''BUT NOT ALL COORDINATES MARK&
      &ED FOR OPTIMISATION'')') 
              write (iw, &
      '(2/10X,''MOPAC, BY DEFAULT, USES '',        ''INTERNAL COORDINATES'',/10&
      &X,''TO SPECIFY CARTESIAN '',          ''COORDINATES USE KEY-WORD :XYZ:'')&
      &                               ') 
              write (iw, &
      '(10X,''YOUR CURRENT CHOICE OF '',           ''KEY-WORDS INVOLVES''      &
      &                                      ,'' A LOGICALLLY'',/10X,''ABSURD CH&
      &OICE SO THE CALCULATION IS'',  '' TERMINATED AT THIS POINT'')') 
              call mopend ('INCONSISTENT USE OF OPTIMIZATION FLAGS') 
              return  
            endif 
          endif 
        endif 
      endif 

      if (index(keywrd,' NOLOG') == 0) then 

        i = index(jobnam,' ') - 1 
!--TGAY 08/2016 - Unit number used was 'il', which doesn't exist (in chanel_C)
!---------------- CHANGED:   il => ilog
        open(unit=ilog, form='FORMATTED', status='UNKNOWN', file=jobnam(:i)//&
          '.log', position='asis') 
        call wrttxt (il) 
      endif  

      call geout(1)

      call gmetry (geo, coord) 
      if (moperr) return  
      if (index(keywrd,' AUTOSYM') /= 0) call maksym(loc, xparam, xyzt) 
      if (index(keywrd,' 0SCF') /= 0) then 
        i = index(jobnam,' ') - 1 
        open(unit=iarc, file=jobnam(:i)//'.arc', status='UNKNOWN', position=&
          'asis') 
        rewind iarc 
      endif 
      if (index(keywrd,' NOXYZ') == 0) then 
        if (index(keywrd,' 0SCF') /= 0) then 
!
!  WRITE OUT CARTESIAN COORDINATES FOR USE AS A DATA-SET
!
          write (iw, '(A)') '   GEOMETRY IN CARTESIAN COORDINATE FORMAT' 
          call wrttxt (iw) 
          write (iarc, '(A)') '   GEOMETRY IN CARTESIAN COORDINATE FORMAT' 
          call wrttxt (iarc) 
          j = 0 
          do i = 1, natoms 
            if (labels(i) == 99) cycle  
            j = j + 1 
            write (iw, '(2X,A,3(F19.13,I3))') elemnt(labels(i)), (coord(k,j),1,&
              k=1,3) 
            write (iarc, '(2X,A,3(F19.13,I3))') elemnt(labels(i)), (coord(k,j),&
              1,k=1,3) 
          end do 
          write (iw, *) 
          write (iarc, *) 
        else 
           write (iw, '(2/10X,''CARTESIAN COORDINATES '',/)') 
          write (iw, &
      &'(4X,''NO.'',7X,''ATOM'',9X,''X'',                   9X,''Y'',9X,''Z'',/)&
      &') 
          l = 0 
          do i = 1, natoms 
            if (labels(i)==99 .or. labels(i)==107) cycle  
            l = l + 1 
            write (iw, '(I6,8X,A2,4X,3F10.4)') l, elemnt(labels(i)), (coord(j,l&
              ),j=1,3) 
          end do 
        endif 
      endif 
      return  
      end subroutine readmo 
