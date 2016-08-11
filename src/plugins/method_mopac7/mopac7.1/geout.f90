      subroutine geout(mode1) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : labels, na, nb, nc, geo, nat, loc, txtatm, &
      & p
      USE maps_C, ONLY: latom, lparam 
      USE parameters_C, only : tore
      USE molkst_C, ONLY: numat, natoms, ndep, nvar, keywrd, ltxt
      USE symmetry_C, ONLY: depmul, locpar, idepfn, locdep 
      USE elemts_C, only : elemnt
      USE chanel_C, only : iw 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:50:09  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use xyzint_I 
      use wrttxt_I 
      use chrge_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mode1 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(2,3*natoms + 1) :: loctmp 
      integer :: mode, iprt, nvartm, i, j, maxtxt, itemp1, itemp2, itemp3, n&
        , ia, ii, k 
      real(double), dimension(natoms) :: q2 
      real(double), dimension(3,natoms) :: xyz 
      real(double) :: degree, w, x 
      logical :: cart, lxyz 
      character , dimension(3) :: q*2 
      character :: flag1*2, flag0*2, flagn*2, blank*80 
!-----------------------------------------------
!*********************************************************************
!
!   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME,
!         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING.
!
!*********************************************************************
      mode = mode1 
      lxyz = index(keywrd,' XYZ') + index(keywrd,'VELO') /= 0 
      if (index(keywrd,' 0SCF')/=0 .and. lxyz) then 
!
!  If 0SCF and XYZ and a polymer, then get rid of TV.
!
        natoms = numat 
        lxyz = .FALSE. 
      endif 
      if (mode == 1) then 
        flag1 = ' *' 
        flag0 = '  ' 
        flagn = ' +' 
        iprt = iw 
      else 
        flag1 = ' 1' 
        flag0 = ' 0' 
        flagn = '-1' 
        iprt = abs(mode) 
      endif 
!
! *** OUTPUT THE PARAMETER DATA.
!
      cart = .FALSE. 
!
!  Convert to internal coordinates, if in Cartesian and XYZ is not used
!
      if (na(1)/=0 .and. .not.lxyz) then 
        cart = .TRUE. 
        call xyzint (geo, natoms, na, nb, nc, 1.D0, xyz) 
        loctmp(1,1) = 2 
        loctmp(2,1) = 1 
        loctmp(1,2) = 3 
        loctmp(2,2) = 1 
        loctmp(1,3) = 3 
        loctmp(2,3) = 2 
        nvartm = 0 
        do i = 4, natoms 
          nvartm = nvartm + 3 
          do j = 1, 3 
            loctmp(1,nvartm+j) = i 
            loctmp(2,nvartm+j) = j 
          end do 
        end do 
        nvartm = nvartm + 3 
      else 
        loctmp(1,:nvar) = loc(1,:nvar) 
        loctmp(2,:nvar) = loc(2,:nvar) 
        nvartm = nvar 
!$DOUT VBEST
        xyz(:,:natoms) = geo(:,:natoms) 
      endif 
      degree = 57.29577951308232D0 
      if (lxyz) degree = 1.D0 
      maxtxt = ichar(ltxt) 
      blank = ' ' 
      if (lxyz) then 
        if (mode == 1) then 
          itemp1 = max(2,maxtxt - 4) 
          itemp2 = max(4,maxtxt - 2) 
          itemp3 = max(18,maxtxt + 12) 
          write (iprt, 40) blank(:itemp1), blank(:itemp2), blank(:itemp3) 
   40     format(/,4x,'ATOM',3x,'CHEMICAL',a,6x,'X',16x,'Y',13x,'Z',/,3x,&
            'NUMBER',2x,'SYMBOL',a,' (ANGSTROMS)',6x,'(ANGSTROMS)',3x,&
            '(ANGSTROMS)',/,7x,a,21x,/) 
        else 
          if (mode > 0) call wrttxt (iprt) 
        endif 
      else 
        if (mode == 1) then 
          itemp1 = max(2,maxtxt - 4) 
          itemp2 = max(4,maxtxt - 2) 
          itemp3 = max(18,maxtxt + 12) 
          write (iprt, 50) blank(:itemp1), blank(:itemp2), blank(:itemp3) 
   50     format(/,4x,'ATOM',3x,'CHEMICAL',a,'BOND LENGTH',4x,'BOND ANGLE',4x,&
            ' TWIST ANGLE',/,3x,'NUMBER',2x,'SYMBOL',a,'(ANGSTROMS)',5x,&
            '(DEGREES)',5x,' (DEGREES)',/,4x,'(I)',a,'NA:I',10x,'NB:NA:I',5x,&
            ' NC:NB:NA:I',5x,'NA',3x,'NB',3x,'NC',/) 
        else 
          if (mode > 0) call wrttxt (iprt) 
        endif 
      endif 
      if (mode/=1 .and. allocated(p)) then 
        call chrge (p, q2) 
        q2(:numat) = tore(nat(:numat)) - q2(:numat) 
      else 
        q2(:numat) = 0.D0 
      endif 
      n = 1 
      ia = loctmp(1,1) 
      ii = 0 
      do i = 1, natoms 
        do j = 1, 3 
          q(j) = flag0 
          if (ia /= i) cycle  
          if (j/=loctmp(2,n) .or. n>nvartm) cycle  
          q(j) = flag1 
          n = n + 1 
          ia = loctmp(1,n) 
        end do 
        w = xyz(2,i)*degree 
        x = xyz(3,i)*degree 
        if (.not.lxyz) then 
!
!  CONSTRAIN ANGLE TO DOMAIN 0 - 180 DEGREES
!
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
        endif 
        if (latom == i) then 
          j = lparam 
          q(j) = flagn 
        endif 
        blank = elemnt(labels(i))//txtatm(i)//'  ' 
        if (mode /= 1) then 
          j = max(4,maxtxt + 2) 
          k = max(0,8 - j) 
        else 
          j = max(9,maxtxt + 3) 
        endif 
        if (labels(i) == 0) cycle  
        if (lxyz) then 
          if (mode /= 1) then 
            if (labels(i)/=99 .and. labels(i)/=107) then 
              ii = ii + 1 
              write (iprt, '(1X,A,F11.7,1X,A2,F14.7,1X,A2,F14.7,1X,A2,A,F7.4)')&
                 blank(:j), xyz(1,i), q(1), w, q(2), x, q(3), blank(20:20+k), &
                q2(ii) 
            else 
              write (iprt, '(1X,A,F11.7,1X,A2,F14.7,1X,A2,F14.7,1X,A2)') blank(&
                :j), xyz(1,i), q(1), w, q(2), x, q(3) 
            endif 
          else 
            write (iprt, '(3X,I4 ,5X,A,F11.7,1X,A2,F14.7,1X,A2,F11.7,1X,A2)') i&
              , blank(:j), xyz(1,i), q(1), w, q(2), x, q(3) 
          endif 
        else 
          if (mode /= 1) then 
            if (labels(i)/=99 .and. labels(i)/=107) then 
              ii = ii + 1 
              write (iprt, &
                '(1X,A,F12.8,1X,A2,F13.7,1X,A2,F13.7,1X,A2,3I5,A,F7.4)') blank(&
                :j), xyz(1,i), q(1), w, q(2), x, q(3), na(i), nb(i), nc(i), &
                blank(20:20+k), q2(ii) 
            else 
              write (iprt, '(1X,A,F12.8,1X,A2,F13.7,1X,A2,F13.7,1X,A2,3I5)') &
                blank(:j), xyz(1,i), q(1), w, q(2), x, q(3), na(i), nb(i), nc(i&
                ) 
            endif 
          else if (i > 3) then 
            write (iprt, &
              '(3X,I4 ,5X,A,F9.5,1X,A2,F14.5,1X,A2,F11.5,1X,A2,I4,2I5)') i, &
              blank(:j), xyz(1,i), q(1), w, q(2), x, q(3), na(i), nb(i), nc(i) 
          else if (i == 3) then 
            write (iprt, '(''      3'',5X,A,F9.5,1X,A2,F14.5,1X,A2,13X,2I5)') &
              blank(:j), xyz(1,3), q(1), w, q(2), na(3), nb(3) 
          else if (i == 2) then 
            write (iprt, '(''      2'',5X,A,F9.5,1X,A2,30X,I5)') blank(:j), xyz&
              (1,2), q(1), na(2) 
          else 
            write (iprt, '(''      1'',5X,A)') blank(:j) 
          endif 
        endif 
      end do 
      if (cart) na(1) = 99 
      if (mode == 1) return  
      write (iprt, *) 
      if (ndep == 0) return  
!
!   OUTPUT SYMMETRY DATA.
!
      n = 0 
      i = 1 
  110 continue 
      j = i 
  120 continue 
      if (j == ndep) go to 130 
      if (locpar(j)==locpar(j+1) .and. idepfn(j)==idepfn(j+1) .and. j-i<15) &
        then 
        j = j + 1 
        go to 120 
      else 
        if (idepfn(i) == 18) then 
          n = n + 1 
          write (iprt, '(I4,I3,F13.9,I5,15I4)') locpar(i), idepfn(i), depmul(n)&
            , (locdep(k),k=i,j) 
        else 
          write (iprt, '(I4,I3,I5,15I4)') locpar(i), idepfn(i), (locdep(k),k=i,&
            j) 
        endif 
      endif 
      i = j + 1 
      go to 110 
  130 continue 
      if (idepfn(i) == 18) then 
        n = n + 1 
        write (iprt, '(I4,I3,F13.9,I5,15I4)') locpar(i), idepfn(i), depmul(n), &
          (locdep(k),k=i,j) 
      else 
        write (iprt, '(I4,I3,I5,15I4)') locpar(i), idepfn(i), (locdep(k),k=i,j) 
      endif 
      write (iprt, *) 
      return  
      end subroutine geout 
