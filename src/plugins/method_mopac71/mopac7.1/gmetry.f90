      subroutine gmetry(geo, coord) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : natoms, numcal, keywrd, step
      use permanent_arrays, only : labels, geoa, na, nb, nc
      use euler_C, only : id, tvec
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:18  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      use geout_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) :: geo(3,natoms) 
      real(double) , intent(out) :: coord(3,natoms) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, j, i, mb, mc, ma, k, l 
      real(double) :: sum, error, ccos, cosa, xb, yb, zb, rbc, xa, ya, za, xyb&
        , xpa, xpb, costh, sinth, ypa, sinph, cosph, zqa, yza, coskh, sinkh, &
        sina, sind, cosd, xd, yd, zd, ypd, zpd, xpd, zqd, xqd, yqd, xrd
      logical :: geook=.true.
      character, dimension(4) :: ndimen*15 
      real(double), dimension(3, natoms) :: geovec

      save ndimen, geook, icalcn 
!-----------------------------------------------
      data icalcn/ 0/  
      data ndimen/ ' MOLECULE     ', ' POLYMER       ', 'LAYER STRUCTURE', &
        ' SOLID         '/  
!***********************************************************************
!
!    GMETRY  COMPUTES COORDINATES FROM BOND-ANGLES AND LENGTHS.
! *** IT IS ADAPTED FROM THE PROGRAM WRITTEN BY M.J.S. DEWAR.
!
!     THREE SEPARATE OPTIONS EXIST WITHIN GMETRY. THESE ARE:
!    (A) IF NA(1) IS EQUAL TO 99 (IMPOSSIBLE UNDER NORMAL CIRCUMSTANCES)
!        THEN GEO IS ASSUMED TO BE IN CARTESIAN RATHER THAN INTERNAL
!        COORDINATES, AND COORD IS THEN SET EQUAL TO GEO.
!    (B) IF STEP IS NON-ZERO (THIS IS THE CASE WHEN "SADDLE" IS USED)
!        THEN GEO IS FIRST MODIFIED BY SHIFTING THE INTERNAL COORDINATES
!        ALONG A RADIUS FROM GEOA TO PLACE GEO AT ADISTANCESTEPFROMGEOA.
!    (C) NORMAL CONVERSION FROM INTERNAL TO CARTESIAN COORDINATESISDONE.
!
!  ON INPUT:
!         GEO    = ARRAY OF INTERNAL COORDINATES.
!         NATOMS = NUMBER OF ATOMS, INCLUDING DUMMIES.
!         NA     = ARRAY OF ATOM LABELS FOR BOND LENGTHS.
!
!  ON OUTPUT:
!         COORD  = ARRAY OF CARTESIAN COORDINATES
!
!***********************************************************************
!                                     OPTION (B)
      if (abs(step) > 1.D-4) then 
        sum = 0.D0 
        do j = 1, 3 
          do i = 1, natoms 
            geovec(j,i) = geo(j,i) - geoa(j,i) 
            sum = sum + geovec(j,i)**2 
          end do 
        end do 
        sum = sqrt(sum) 
        error = (sum - step)/sum 
      else 
        error = 0.D0 
        geovec = 0.d0
      endif 
      geo = geo - error*geovec(:,:natoms) 
!                                     OPTION (A)
      if (na(1) == 99) then 
!$DOIT VBEST
        coord = geo 
        go to 100 
      endif 
!                                     OPTION (C)
      coord(1,1) = 0.0D00 
      coord(2,1) = 0.0D00 
      coord(3,1) = 0.0D00 
      if (natoms == 1) return  
      coord(1,2) = geo(1,2) 
      coord(2,2) = 0.0D00 
      coord(3,2) = 0.0D00 
      if (natoms /= 2) then 
        ccos = cos(geo(2,3)) 
        if (na(3) == 1) then 
          coord(1,3) = coord(1,1) + geo(1,3)*ccos 
        else 
          coord(1,3) = coord(1,2) - geo(1,3)*ccos 
        endif 
        coord(2,3) = geo(1,3)*sin(geo(2,3)) 
        coord(3,3) = 0.0D00 
        do i = 4, natoms 
          cosa = cos(geo(2,i)) 
          mb = nb(i) 
          mc = na(i) 
          xb = coord(1,mb) - coord(1,mc) 
          yb = coord(2,mb) - coord(2,mc) 
          zb = coord(3,mb) - coord(3,mc) 
          rbc = xb*xb + yb*yb + zb*zb 
          if (rbc < 1.D-16) then 
!
!     TWO ATOMS ARE COINCIDENT.  A FATAL ERROR.
!
            write (iw, '(A,I4,A,I4,A)') ' ATOMS', mb, ' AND', mc, &
              ' ARE COINCIDENT' 
            write (iw, '(A)') ' THIS IS A FATAL ERROR, RUN STOPPED IN GMETRY' 
            call mopend ('TWO ATOMS ARE COINCIDENT.  A FATAL ERROR.') 
            return  
          else 
            rbc = 1.0D00/sqrt(rbc) 
          endif 
          ma = nc(i) 
          xa = coord(1,ma) - coord(1,mc) 
          ya = coord(2,ma) - coord(2,mc) 
          za = coord(3,ma) - coord(3,mc) 
!
!     ROTATE ABOUT THE Z-AXIS TO MAKE YB=0, AND XB POSITIVE.  IF XYB IS
!     TOO SMALL, FIRST ROTATE THE Y-AXIS BY 90 DEGREES.
!
          xyb = sqrt(xb*xb + yb*yb) 
          k = -1 
          if (xyb <= 0.1D00) then 
            xpa = za 
            za = -xa 
            xa = xpa 
            xpb = zb 
            zb = -xb 
            xb = xpb 
            xyb = sqrt(xb*xb + yb*yb) 
            k = 1 
          endif 
!
!     ROTATE ABOUT THE Y-AXIS TO MAKE ZB VANISH
!
          costh = xb/xyb 
          sinth = yb/xyb 
          xpa = xa*costh + ya*sinth 
          ypa = ya*costh - xa*sinth 
          sinph = zb*rbc 
          cosph = sqrt(abs(1.D00 - sinph*sinph)) 
          zqa = za*cosph - xpa*sinph 
!
!     ROTATE ABOUT THE X-AXIS TO MAKE ZA=0, AND YA POSITIVE.
!
          yza = sqrt(ypa**2 + zqa**2) 
          if (yza >= 1.D-4) then 
            if (abs(cosa) < 0.9998D0) then 
              if (yza<2.D-2 .and. .not.geook) then 
                write (iw, '(2/20X,'' CALCULATION ABANDONED AT THIS POINT'')') 
                write (iw, &
      '(2/10X,'' THREE ATOMS BEING USED TO DEFINE ''  ,''THE''                 &
      &                                         ,/10X,'' COORDINATES OF A FOURTH&
      & ATOM, WHOSE BOND-ANGLE IS'')') 
                write (iw, &
      '(10X,'' NOT ZERO OR 180 DEGREES, ARE '',       ''IN AN ALMOST STRAIGHT''&
      &)') 
                write (iw, &
      '(10X,                                          '' LINE.  THERE IS A HIGH&
      & PROBABILITY THAT THE'',/10X,            '' COORDINATES OF THE ATOM WILL &
      &BE INCORRECT.'')') 
                write (iw, '(2/20X,''THE FAULTY ATOM IS ATOM NUMBER'',I4)') i 
                geoa(1,1) = -1.D0 
                call geout (1) 
                write (iw, &
      '(2/20X,                                        ''CARTESIAN COORDINATES U&
      &P TO FAULTY ATOM'')') 
                write (iw, '(2/5X,''I'',12X,''X'',12X,''Y'',12X,''Z'')') 
                do j = 1, i 
                  write (iw, '(I6,F16.5,2F13.5)') j, (coord(k,j),k=1,3) 
                end do 
                write (iw, &
      '(2/6X,'' ATOMS'',I3,'','',I3,'', AND'',I3,     '' ARE WITHIN'',F7.4,'' A&
      &NGSTROMS OF A STRAIGHT LINE'')') mc, mb, ma, yza 
                call mopend ('FAULT DETECTED IN INTERNAL COORDINATES') 
                return  
              endif 
            endif 
            coskh = ypa/yza 
            sinkh = zqa/yza 
          else 
!
!   ANGLE TOO SMALL TO BE IMPORTANT
!
            coskh = 1.D0 
            sinkh = 0.D0 
          endif 
!
!     COORDINATES :-   A=(???,YZA,0),   B=(RBC,0,0),  C=(0,0,0)
!     NONE ARE NEGATIVE.
!     THE COORDINATES OF I ARE EVALUATED IN THE NEW FRAME.
!
          sina = sin(geo(2,i)) 
          sind = -sin(geo(3,i)) 
          cosd = cos(geo(3,i)) 
          xd = geo(1,i)*cosa 
          yd = geo(1,i)*sina*cosd 
          zd = geo(1,i)*sina*sind 
!
!     TRANSFORM THE COORDINATES BACK TO THE ORIGINAL SYSTEM.
!
          ypd = yd*coskh - zd*sinkh 
          zpd = zd*coskh + yd*sinkh 
          xpd = xd*cosph - zpd*sinph 
          zqd = zpd*cosph + xd*sinph 
          xqd = xpd*costh - ypd*sinth 
          yqd = ypd*costh + xpd*sinth 
          if (k >= 1) then 
            xrd = -zqd 
            zqd = xqd 
            xqd = xrd 
          endif 
          coord(1,i) = xqd + coord(1,mc) 
          coord(2,i) = yqd + coord(2,mc) 
          coord(3,i) = zqd + coord(3,mc) 
        end do 
!
! *** NOW REMOVE THE TRANSLATION VECTORS, IF ANY, FROM THE ARRAY COOR
!
      endif 
  100 continue 
      k = natoms 
      do while(labels(k) == 107) 
        k = k - 1 
      end do 
      k = k + 1 
      if (icalcn /= numcal) id = 0 
      if (k <= natoms) then 
!
!   SYSTEM IS A SOLID, OF DIMENSION NATOMS+1-K
!
        l = 0 
        do i = k, natoms 
          l = l + 1 
          mc = na(i) 
          if (mc == 0) then 
            tvec(1,l) = coord(1,i) 
            tvec(2,l) = coord(2,i) 
            tvec(3,l) = coord(3,i) 
          else 
            tvec(1,l) = coord(1,i) - coord(1,mc) 
            tvec(2,l) = coord(2,i) - coord(2,mc) 
            tvec(3,l) = coord(3,i) - coord(3,mc) 
          endif 
        end do 
        id = l 
        if (icalcn /= numcal) then 
          icalcn = numcal 
          write (iw, 140) ndimen(id+1) 
  140     format(/,10x,'    THE SYSTEM IS A ',a15,/) 
          if (id == 0) go to 170 
          write (iw, 150) 
          write (iw, 160) (i,(tvec(j,i),j=1,3),i=1,id) 
  150     format(/,'                UNIT CELL TRANSLATION VECTORS',/,/,&
            '              X              Y              Z') 
  160     format('    T',i1,' = ',f11.7,'    ',f11.7,'    ',f11.7) 
          if (index(keywrd,' GREEN') /= 0) then 
            write (iw, *) 
            write (iw, *) '               GREENS FUNCTION IS NOT ALLOWED ', &
              'WITH EXTENDED SYSTEMS' 
            write (iw, *) 
            call mopend ('GREENS FUNCTION IS NOT ALLOWED WITH EXTENDED SYSTEMS'&
              ) 
            return  
          endif 
        endif 
      endif 
  170 continue 
      j = 0 
      do i = 1, natoms 
        if (labels(i)==99 .or. labels(i)==107) cycle  
        j = j + 1 
!$DOIT ASIS
        coord(:,j) = coord(:,i) 
      end do 
      return  
      end subroutine gmetry 
