      subroutine geoutg(iprt) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      use permanent_arrays, only : na, coord, geo, loc, labels, &
      & simbol, nb, nc, txtatm
      use molkst_C, only : natoms, nvar, ndep, ltxt
      USE symmetry_C, ONLY: locpar, idepfn, locdep
      use chanel_C, only : iscr
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:16  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use xyzint_I  
      use xxx_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iprt 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(3,natoms) :: igeo 
      integer :: i, j, maxtxt, nopt, nbi, nci, l 
      real(double) :: degree, w, x 
      character , dimension(3,natoms) :: line*14 
      character , dimension(3) :: type 
      character , dimension(3*natoms) :: optdat*14 
      character :: blank*80 
      character, dimension(107) :: elemnt*2 

      save type, elemnt 
!-----------------------------------------------
!***********************************************************************
!
!   GEOUTG WRITES OUT THE GEOMETRY IN GAUSSIAN-8X STYLE
!
!*********************************************************************** 
      data elemnt/ ' H', 'He', 'Li', 'Be', ' B', ' C', ' N', ' O', ' F', 'Ne', &
        'Na', 'Mg', 'Al', 'Si', ' P', ' S', 'Cl', 'Ar', ' K', 'Ca', 'Sc', 'Ti'&
        , ' V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge', 'As', &
        'Se', 'Br', 'Kr', 'Rb', 'Sr', ' Y', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh'&
        , 'Pd', 'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te', ' I', 'Xe', 'Cs', 'Ba', &
        'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er'&
        , 'Tm', 'Yb', 'Lu', 'Hf', 'Ta', ' W', 'Re', 'Os', 'Ir', 'Pt', 'Au', &
        'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', 'Pa'&
        , ' U', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'XX', 'Fm', 'Md', 'Cb', &
        '++', ' +', '--', ' -', 'Tv'/  
      data type/ 'r', 'a', 'd'/  
      degree = 57.29577951308232D0 
      if (na(1) == 99) then 
        coord(:,:natoms) = geo(:,:natoms) 
        call xyzint (coord, natoms, na, nb, nc, 1.D0, geo) 
        nvar = 0 
        do i = 1, natoms 
          do j = 1, min(3,i - 1) 
            nvar = nvar + 1 
            loc(1,nvar) = i 
            loc(2,nvar) = j 
          end do 
        end do 
      else 
!
!   CONSTRAIN ANGLES TO THE RANGE 0 to 180 DEGREES
!
        do i = 4, natoms 
          w = geo(2,i)*degree 
          x = geo(3,i)*degree 
          w = w - aint(w/360.D0)*360.D0 
          if (w < 0) w = w + 360.D0 
          if (w > 180.D0) then 
            x = x + 180.D0 
            w = 360.D0 - w 
          endif 
!
!  CONSTRAIN DIHEDRAL TO DOMAIN -180 - 180 DEGREES
!
          x = x - aint(x/360.D0 + sign(0.5D0 - 1.D-9,x) - 1.D-9)*360.D0 
          geo(2,i) = w/degree 
          geo(3,i) = x/degree 
        end do 
      endif 
      igeo(:,:natoms) = -1 
      do i = 1, nvar 
        igeo(loc(2,i),loc(1,i)) = -2 
      end do 
      do i = 1, ndep 
        if (idepfn(i) == 14) then 
          igeo(3,locdep(i)) = -locpar(i) 
        else 
          if (idepfn(i) > 3) cycle  
          igeo(idepfn(i),locdep(i)) = locpar(i) 
        endif 
      end do 
      open(unit=iscr, status='SCRATCH', position='asis') 
      maxtxt = ichar(ltxt) 
      nopt = 0 
      do i = 1, natoms 
        do j = 1, 3 
          line(j,i) = ' ' 
          if (igeo(j,i) == (-1)) then 
            rewind iscr 
            if (j /= 1) then 
              write (iscr, '(F12.6)') geo(j,i)*degree 
            else 
              write (iscr, '(F12.6)') geo(j,i) 
            endif 
            rewind iscr 
            read (iscr, '(A)') line(j,i) 
          else if (igeo(j,i) == (-2)) then 
            nopt = nopt + 1 
            if (simbol(nopt) /= '---') then 
              if (simbol(nopt)(1:1) == '-') then 
                line(j,i)(4:) = simbol(nopt)(2:) 
              else 
                line(j,i)(4:) = simbol(nopt) 
              endif 
            else 
              nbi = nb(i) 
              nci = nc(i) 
              if (j /= 3) nci = 0 
              if (j == 1) nbi = 0 
              call xxx (type(j), i, na(i), nbi, nci, line(j,i)(4:)) 
            endif 
            optdat(nopt) = line(j,i) 
          else if (igeo(j,i) < 0) then 
            line(3,i) = line(3,(-igeo(j,i))) 
            line(3,i)(3:3) = '-' 
          else 
            line(j,i) = line(j,igeo(j,i)) 
          endif 
        end do 
        blank = elemnt(labels(i))//txtatm(i)//'  ' 
        if (labels(i) == 99) blank(1:1) = ' ' 
        j = max(4,maxtxt + 2) 
        select case (i)  
        case (1)  
          write (iprt, '(1X,A,I4,A,I4,A,I4,A,I4)') blank(:j) 
        case (2)  
          write (iprt, '(1X,A,I4,A,I4,A,I4,A,I4)') blank(:j), na(i), line(1,i) 
        case (3)  
          write (iprt, '(1X,A,I4,A,I4,A,I4,A,I4)') blank(:j), na(i), line(1,i)&
            , nb(i), line(2,i) 
        case default 
          l = 0 
          write (iprt, '(1X,A,I4,A,I4,A,I4,A,I4)') blank(:j), na(i), line(1,i)&
            , nb(i), line(2,i), nc(i), line(3,i), l 
        end select 
      end do 
      write (iprt, *) 
      do l = 1, 3 
        do i = 1, nopt 
          if (loc(2,i) /= l) cycle  
          if (loc(2,i) /= 1) then 
            write (iprt, '(A,F12.6)') optdat(i), geo(loc(2,i),loc(1,i))*degree 
          else 
            write (iprt, '(A,F12.6)') optdat(i), geo(loc(2,i),loc(1,i)) 
          endif 
        end do 
      end do 
      return  
      end subroutine geoutg 
