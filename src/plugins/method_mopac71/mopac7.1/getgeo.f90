      subroutine getgeo(iread, labels, geo, xyz, lopt, na, nb, nc, react, int) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use parameters_C, only : ams
      use molkst_C, only : natoms, keywrd, ltxt, numat
      use chanel_C, only : iw
      use maps_C, only :
      use permanent_arrays, only :atmass, simbol, txtatm
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:17  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use upcase_I    
      use reada_I 
      use mopend_I 
      use geout_I 
      use nuchar_I 
      use gmetry_I 
      use xyzint_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iread 
      integer , intent(out) :: labels(*) 
      integer , intent(out) :: lopt(3,*)
      real(double), intent(out) :: react(*) 
      integer, intent(out)  :: na(*) 
      integer, intent(out)  :: nb(*) 
      integer, intent(out)  :: nc(*) 
      real(double), intent(out)  :: geo(3,*), xyz(3,*)
      logical :: int

!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(40) :: istart 
      integer :: i, icapa, icapz, maxtxt, iserr, k, icomma, khar, nvalue&
        , label, j, ndmy, jj 
      real(double) :: weight, real, sum, const, degree 
      logical :: lxyz, velo, leadsp, ircdrc
      character , dimension(107) :: elemnt*2 
      character :: line*80, space, nine, zero, comma, string*80, ele*2, turn 

      save elemnt, space, nine, zero, comma 
!-----------------------------------------------
!***********************************************************************
!
!   GETGEO READS IN THE GEOMETRY. THE ELEMENT IS SPECIFIED BY IT'S
!          CHEMICAL SYMBOL, OR, OPTIONALLY, BY IT'S ATOMIC NUMBER.
!
!  ON INPUT   IREAD  = CHANNEL NUMBER FOR READ, NORMALLY 5
!             AMS    = DEFAULT ATOMIC MASSES.
!
! ON OUTPUT LABELS = ATOMIC NUMBERS OF ALL ATOMS, INCLUDING DUMMIES.
!           GEO    = INTERNAL COORDINATES, IN ANGSTROMS, AND DEGREES.
!                    OR CARTESIAN COORDINATES, DEPENDING ON WHETHER
!                    KEYWORD ' XYZ' IS PRESENT
!           LOPT   = INTEGER ARRAY, A '1' MEANS OPTIMIZE THIS PARAMETER,
!                    '0' MEANS DO NOT OPTIMIZE, AND A '-1' LABELS THE
!                    REACTION COORDINATE.
!           NA     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
!           NB     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
!           NC     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
!           ATMASS = ATOMIC MASSES OF ATOMS.
!***********************************************************************
      data (elemnt(i),i=1,107)/ 'H', 'HE', 'LI', 'BE', 'B', 'C', 'N', 'O', 'F'&
        , 'NE', 'NA', 'MG', 'AL', 'SI', 'P', 'S', 'CL', 'AR', 'K', 'CA', 'SC', &
        'TI', 'V', 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', &
        'SE', 'BR', 'KR', 'RB', 'SR', 'Y', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH', &
        'PD', 'AG', 'CD', 'IN', 'SN', 'SB', 'TE', 'I', 'XE', 'CS', 'BA', 'LA', &
        'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO', 'ER', 'TM'&
        , 'YB', 'LU', 'HF', 'TA', 'W', 'RE', 'OS', 'IR', 'PT', 'AU', 'HG', 'TL'&
        , 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 'RA', 'AC', 'TH', 'PA', 'U', 'NP'&
        , 'PU', 'AM', 'CM', 'BK', 'CF', 'XX', 'FM', 'MD', 'CB', '++', '+', '--'&
        , '-', 'TV'/  
      data comma, space, nine, zero/ ',', ' ', '9', '0'/  
      ircdrc = index(keywrd,'IRC') + index(keywrd,'DRC') /= 0 
      icapa = ichar('A') 
      icapz = ichar('Z') 
      maxtxt = 0 
      simbol(:natoms) = '---' 
      natoms = 0 
      numat = 0 
      iserr = 0      
   20 continue 
      read (iread, '(A)', end=120, err=200) line 
      if (line /= ' ') then 
        natoms = natoms + 1 
!
!   SEE IF TEXT IS ASSOCIATED WITH THIS ELEMENT
!
        i = index(line,'(') 
        if (i /= 0) then 
!
!  YES, ELEMENT IS LABELLED.
!
          k = index(line,')') 
          txtatm(natoms) = line(i:k) 
          maxtxt = max(maxtxt,k - i + 1) 
          string = line(1:i-1)//line(k+1:) 
          line = string 
        else 
          txtatm(natoms) = ' ' 
        endif 
!   CLEAN THE INPUT DATA
        call upcase (line, 80) 
        icomma = ichar(comma) 
        do i = 1, 80 
          khar = ichar(line(i:i)) 
          if (khar /= icomma) cycle  
          line(i:i) = space 
        end do 
!
!   INITIALIZE ISTART TO INTERPRET BLANKS AS ZERO'S
        istart(:10) = 80 
!
! FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
!     BY A CHARACTER AND STORE IN ISTART
        leadsp = .TRUE. 
        nvalue = 0 
        do i = 1, 80 
          if (leadsp .and. line(i:i)/=space) then 
            nvalue = nvalue + 1 
            istart(nvalue) = i 
          endif 
          leadsp = line(i:i) == space 
        end do 
!
! ESTABLISH THE ELEMENT'S NAME AND ISOTOPE, CHECK FOR ERRORS OR E.O.DATA
!
        weight = 0.D0 
        string = line(istart(1):istart(2)-1) 
        if (string(1:1)>=zero .and. string(1:1)<=nine) then 
!  ATOMIC NUMBER USED: NO ISOTOPE ALLOWED
          label = nint(reada(string,1)) 
          if (label == 0) go to 110 
          if (label<0 .or. label>107) then 
            write (iw, '(''  ILLEGAL ATOMIC NUMBER'')') 
            go to 210 
          endif 
          go to 70 
        endif 
!  ATOMIC SYMBOL USED
        real = abs(reada(string,1)) 
        if (real < 1.D-15) then 
!   NO ISOTOPE
          ele = string(1:2) 
        else 
          weight = real 
          if (string(2:2)>=zero .and. string(2:2)<=nine) then 
            ele = string(1:1) 
          else 
            ele = string(1:2) 
          endif 
        endif 
!   CHECK FOR ERROR IN ATOMIC SYMBOL
        if (ele(1:1)=='-' .and. ele(2:2)/='-') ele(2:2) = ' ' 
        do i = 1, 107 
          if (ele /= elemnt(i)) cycle  
          label = i 
          go to 70 
        end do 
        if (ele(1:1) == 'X') then 
          label = 99 
          go to 70 
        endif 
        write (iw, '(''  UNRECOGNIZED ELEMENT NAME: ('',A,'')'')') ele 
        go to 210 
!
! ALL O.K.
!
   70   continue 
        if (label /= 99) numat = numat + 1 
        if (weight /= 0.D0) then 
          write (iw, &
            '('' FOR ATOM'',I4,''  ISOTOPIC MASS:''                   ,F15.5)')&
             natoms, weight 
          atmass(numat) = weight 
        else 
          if (label /= 99) atmass(numat) = ams(label) 
        endif 
        labels(natoms) = label 
        geo(1,natoms) = reada(line,istart(2)) 
        geo(2,natoms) = reada(line,istart(4)) 
        geo(3,natoms) = reada(line,istart(6)) 
        if (ircdrc) then 
          turn = line(istart(3):istart(3)) 
          if (turn == 'T') then 
            lopt(1,natoms) = 1 
            if (natoms == 1) write (iw, '(A)') &
              ' IN DRC MONITOR POTENTIAL    GY TURNING POINTS' 
          else 
            lopt(1,natoms) = 0 
          endif 
          turn = line(istart(5):istart(5)) 
          if (turn == 'T') then 
            lopt(2,natoms) = 1 
          else 
            lopt(2,natoms) = 0 
          endif 
          turn = line(istart(7):istart(7)) 
          if (turn == 'T') then 
            lopt(3,natoms) = 1 
          else 
            lopt(3,natoms) = 0 
          endif 
        else 
          lopt(1,natoms) = nint(reada(line,istart(3))) 
          lopt(2,natoms) = nint(reada(line,istart(5))) 
          lopt(3,natoms) = nint(reada(line,istart(7))) 
          do i = 3, 7, 2 
            if (.not.(ichar(line(istart(i):istart(i)))>=icapa .and. ichar(line(&
              istart(i):istart(i)))<=icapz .and. natoms>1)) cycle  
            iserr = 1 
          end do 
        endif 
        sum = reada(line,istart(8)) 
        if (abs(sum - nint(sum)) > 1.D-5) sum = 0.D0 
        na(natoms) = nint(sum) 
        na(natoms) = nint(reada(line,istart(8))) 
        nb(natoms) = nint(reada(line,istart(9))) 
        nc(natoms) = nint(reada(line,istart(10))) 
!
!  SPECIAL CASE OF USERS FORGETTING TO ADD DIHEDRAL DATA FOR ATOM 3
!
        if (natoms == 3) then 
          if (lopt(3,3) == 2) then 
            na(3) = 1 
            nb(3) = 2 
            geo(3,3) = 0.D0 
            lopt(3,3) = 0 
          else if (lopt(3,3)==1 .and. abs(geo(3,3)-2.D0)<1.D-4) then 
            na(3) = 2 
            nb(3) = 1 
            geo(3,3) = 0.D0 
            lopt(3,3) = 0 
          endif 
        endif 
        if ((lopt(1,natoms)>1 .or. lopt(2,natoms)>1 .or. lopt(3,natoms)>1)&
           .and. natoms>1) iserr = 1 
        if (iserr == 1) then 
!
!  MUST BE GAUSSIAN GEOMETRY INPUT
!
          do i = 2, natoms 
            do k = 1, 3 
              j = nint(geo(k,i)) 
              if (abs(geo(k,i)-j) <= 1.D-5) cycle  
!
!   GEOMETRY CANNOT BE GAUSSIAN
!
              write (iw, '(A)') ' GEOMETRY IS FAULTY.  GEOMETRY READ IN IS' 
              const = 1.7453292519943D-02 
              geo(2,:natoms) = geo(2,:natoms)*const 
              geo(3,:natoms) = geo(3,:natoms)*const 
              atmass(1) = -1.D0 
              call geout (iw) 
              call mopend ('GEOMETRY IS FAULTY') 
              return  
            end do 
          end do 
          natoms = -1 
          return  
        endif 
        go to 20 
!***********************************************************************
! ALL DATA READ IN, CLEAN UP AND RETURN
!***********************************************************************
  110   continue 
        natoms = natoms - 1 
      endif 
  120 continue 
      na(2) = 1 
      ltxt = char(maxtxt) 
      if (natoms > 3) then 
        int = na(4) /= 0 
      else 
        if (geo(2,3)<10 .and. natoms==3) write (iw, &
      '(2/10X,''WARNING: INTERNAL COORDINATES ARE ASSUMED -'', /10X,'' FOR THRE&
      &E-ATOM SYSTEMS '',2/)') 
        int = .TRUE. 
      endif 
      if (int) geo(2,2) = 0 
      velo = index(keywrd,'VELO') > 0 
!
!     READ IN VELOCITY VECTOR, IF PRESENT
!
      if (velo) then 
        if (int .and. index(keywrd,' LET')==0) then 
          write (iw, '(A)') &
            ' COORDINATES MUST BE CARTESIAN WHEN VELOCITY VECTOR IS USED.' 
          call mopend (&
             'COORDINATES MUST BE CARTESIAN WHEN VELOCITY VECTOR IS USED.') 
          return  
        endif 
        do i = 1, natoms 
          read (iread, '(A)') line 
          call nuchar (line, react((i-1)*3+1), ndmy) 
          if (ndmy == 3) cycle  
          write (iw, '(/10X,A)') &
            '  THERE MUST BE EXACTLY THREE VELOCITY DATA PER LINE' 
          call mopend ('THERE MUST BE EXACTLY THREE VELOCITY DATA PER LINE.') 

          return  
        end do 
      else 
        react(:3*natoms) = 0.D0 
      endif 
      if (int .and. .not.ircdrc) then 
!
!  Check flags on first three atoms, correct if necessary
!
        lopt(1,1) = 0 
        na(1) = 0 
        geo(1,1) = 0.D0 
        lopt(2,1) = 0 
        nb(1) = 0 
        geo(2,1) = 0.D0 
        lopt(3,1) = 0 
        nc(1) = 0 
        geo(3,1) = 0.D0 
        lopt(2,2) = 0 
        nb(2) = 0 
        geo(2,2) = 0.D0 
        lopt(3,2) = 0 
        nc(2) = 0 
        geo(3,2) = 0.D0 
        lopt(3,3) = 0 
        nc(3) = 0 
        geo(3,3) = 0.D0 
      endif 
      lxyz = index(keywrd,' XYZ')/=0 .or. velo 
!
!  Switch for converting between coordinate systems.
!
!  If coordinates are internal, and should be Cartesian, switch.
!  If coordinates are Cartesian, and should be Cartesian, do nothing.
!  If coordinates are internal, and should be internal, do nothing.
!  If coordinates are Cartesian, and should be internal, switch.
!  INT = true if coordinates were supplied as internal.
!  LXYZ= true if coordinates should be Cartesian.
!
!
      if (lxyz) then 
!
!    COORDINATES SHOULD BE CARTESIAN
!
        if (int) then 
          if (index(keywrd,' 0SCF')==0 .and. index(keywrd,' SYMM')/=0) then 
            write (iw, *) ' KEYWORD ''SYMMETRY'' USED.  GEOMETRY SUPPLIED', &
              ' IN INTERNAL COORDINATES BUT '' XYZ''  SPECIFIED', &
              ' ''SYMMETRY'' HAS NO MEANING IN THIS CONTEXT' 
            call mopend ('KEYWORD "SYMMETRY" CANNOT BE USED HERE') 
            return  
          endif 
!
!  Coordinates are internal,  therefore switch.
!
!
!  COORDINATES ARE INTERNAL, BUT IN DEGREES.  CONVERT TO RADIANS FIRST
!
          degree = 1.7453292519943D-02 
          geo(2:3,:natoms) = geo(2:3,:natoms)*degree 
          if (natoms == 0) then 
            call mopend ('Error in GETGEO') 
            return  
          endif 
          call gmetry (geo, xyz) 
!
!  Get rid of dummy atoms
!
          numat = 0 
          do i = 1, natoms 
            if (labels(i) /= 99) then 
              numat = numat + 1 
              labels(numat) = labels(i) 
            endif 
            geo(:,i) = xyz(:,i) 
          end do 
          natoms = numat 
!
!    System supplied in Cartesian coordinates.
!
        endif 
        na(1) = 99 
      else 
!
!    COORDINATES SHOULD BE INTERNAL
!
        if (na(3) == 0) then 
          nb(3) = 1 
          na(3) = 2 
        endif 
        if (int) then 
!
!   Coordinates supplied in internal.
!
          na(1) = 0 
        else 
          if (index(keywrd,' 0SCF')==0 .and. index(keywrd,' SYMM')/=0) then 
            write (iw, *) ' KEYWORD ''SYMMETRY'' USED.  GEOMETRY SUPPLIED', &
              ' IN CARTESIAN  COORDINATES BUT ''XYZ'' NOT SPECIFIED', &
              ' ''SYMMETRY'' HAS NO MEANING IN THIS CONTEXT' 
            call mopend ('KEYWORD "SYMMETRY" CANNOT BE USED HERE') 
            return  
          endif 
!
!  Coordinates are Cartesian, and should be internal, therefore switch.
!
          xyz(:,:natoms) = geo(:,:natoms) 
          degree = 57.29577951308232D0 
          call xyzint (xyz, numat, na, nb, nc, degree, geo) 
!
!  UNCONDITIONALLY SET FLAGS FOR INTERNAL COORDINATES
!
          do i = 1, 3 
            lopt(i:3,i) = 0 
          end do 
          if (abs(geo(2,3)-180.D0)<1.D-4 .or. abs(geo(2,3))<1.D-4) then 
            write (iw, '(A)') &
      ' DUE TO PROGRAM BUG, THE FIRST THREE ATOMS MUST NOT LIE IN A STRAIGHT LI&
      &NE.' 
            call mopend ('Error in GETGEO') 
            return  
          endif 
        endif 
!
!  COORDINATES ARE INTERNAL, BUT IN DEGREES.  CONVERT TO RADIANS
!
        degree = 1.7453292519943D-02 
!$DOIT VBEST
        geo(2:3,:natoms) = geo(2:3,:natoms)*degree 
      endif 
!
!   End of the big switch
!
      return  
! ERROR CONDITIONS
  200 continue 
      if (iread == 5) then 
        write (iw, '( '' ERROR DURING READ AT ATOM NUMBER '', I3 )') natoms 
      else 
        natoms = 0 
        return  
      endif 
  210 continue 
      j = natoms - 1 
      write (iw, '('' DATA CURRENTLY READ IN ARE '')') 
      do k = 1, j 
        write (iw, 230) labels(k), (geo(jj,k),lopt(jj,k),jj=1,3), na(k), nb(k)&
          , nc(k) 
      end do 
  230 format(i4,2x,3(f10.5,2x,i2,2x),3(i2,1x)) 
      return  
      end subroutine getgeo 
