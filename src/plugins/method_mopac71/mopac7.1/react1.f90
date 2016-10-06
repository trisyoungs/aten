      subroutine react1
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : keywrd, natoms, moperr, numat, ndep, nvar, &
      tleft,  gnorm, escf, cosine, mpack, step, time0
      use chanel_C, only : iw, ir
      use permanent_arrays, only : p, pa, pb, geo, geoa, coord, &
      & labels, loc, na, nb, nc, xparam, grad
!...Translated by Pacific-Sierra Research 77to90  4.4G  15:14:06  03/25/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use getgeo_I 
      use mopend_I 
      use second_I 
      use symtry_I 
      use geout_I 
      use gmetry_I 
    !  use ef_I 
    !  use flepo_I 
      use compfg_I 
      use dot_I 
      use writmo_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: idum1(natoms), idum2(natoms), idum3(natoms)
      integer , dimension(2,3) :: irot 
      integer :: linear, iflag, i, maxstp, l, maxcyc, numat2, j, jr&
        , k, iloop, lopt(3,natoms)
      real(double), dimension(3*numat) :: xold, grold, xstore 
      real(double) :: stepmx,  x, sumx, sumy, sumz, sum, ca, sa, summ, &
        step0, one, dell, eold, time1, swap, funct1, time2, gold, c1, c2, &
        react(1), pastor(mpack), pbstor(mpack)
      logical :: gradnt, finish, xyz, intl 
      logical , dimension(2) :: gok 
      logical :: lef 

      save gradnt, finish, xyz, intl, gok, irot 
!-----------------------------------------------
!***********************************************************************
!
!  REACT1 DETERMINES THE TRANSITION STATE OF A CHEMICAL REACTION.
!
!   REACT WORKS BY USING TWO SYSTEMS SIMULTANEOUSLY, THE HEATS OF
!   FORMATION OF BOTH ARE CALCULATED, THEN THE MORE STABLE ONE
!   IS MOVED IN THE DIRECTION OF THE OTHER. AFTER A STEP THE
!   ENERGIES ARE COMPARED, AND THE NOW LOWER-ENERGY FORM IS MOVED
!   IN THE DIRECTION OF THE HIGHER-ENERGY FORM. THIS IS REPEATED
!   UNTIL THE SADDLE POINT IS REACHED.
!
!   IF ONE FORM IS MOVED 3 TIMES IN SUCCESSION, THEN THE HIGHER ENERGY
!   FORM IS RE-OPTIMIZED WITHOUT SHORTENING THE DISTANCE BETWEEN THE TWO
!   FORMS. THIS REDUCES THE CHANCE OF BEING CAUGHT ON THE SIDE OF A
!   TRANSITION STATE.
!
!***********************************************************************
      data irot/ 1, 2, 1, 3, 2, 3/  
      linear = 0 
      iflag = 1 
      gok(1) = .FALSE. 
      gok(2) = .FALSE. 
      xyz = index(keywrd,' XYZ') /= 0 
      lef = index(keywrd,' EF') /= 0 
      gradnt = index(keywrd,'GRAD') /= 0 
      i = index(keywrd,' BAR') 
      stepmx = 0.15D0 
      if (i /= 0) stepmx = reada(keywrd,i) 
      maxstp = 1000 
!
!    READ IN THE SECOND GEOMETRY.
!
      allocate(geoa(3,natoms))
      if (xyz) then 
        call getgeo (ir, labels, geoa, coord, loc, na, nb, nc, react, intl) 
      else 
         call getgeo (ir, labels, geoa, coord, lopt, idum1, idum2, idum3, react, intl) 
!  IF INTERNAL COORDINATES ARE TO BE USED, CHECK THE CONNECTIVITY
!
        l = 0 
        do i = 1, natoms 
          if (idum1(i) /= na(i)) then 
            l = l + 1 
            if (l == 1) write (iw, &
              '(10X,''ERRORS DETECTED IN '',2/       ''CONNECTIVITY'')') 
            write (iw, '(A,I3,A,I3,A,I3,A)') ' FOR ATOM', i, &
              ' THE BOND LABELS ARE DIFFERENT:      ', idum1(i), ' AND', na(i) 
          endif 
          if (idum2(i) /= nb(i)) then 
            l = l + 1 
            if (l == 1) write (iw, &
      '(10X,                                ''ERRORS DETECTED IN CONNECTIVITY''&
      &)') 
            write (iw, '(A,I3,A,I3,A,I3,A)') ' FOR ATOM', i, &
              ' THE BOND ANGLE LABELS ARE DIFFERENT:', idum2(i), ' AND', nb(i) 
          endif 
          if (idum3(i) == nc(i)) cycle  
          l = l + 1 
          if (l == 1) write (iw, &
      '(10X,                                ''ERRORS DETECTED IN CONNECTIVITY''&
      &)') 
          write (iw, '(A,I3,A,I3,A,I3,A)') ' FOR ATOM', i, &
            ' THE DIHEDRAL LABELS ARE DIFFERENT:  ', idum3(i), ' AND', nc(i) 
        end do 
        if (moperr) return  
        if (l /= 0) then 
          write (iw, '(10X,A)') ' CORRECT BEFORE RESUBMISSION' 
          call mopend ('ERRORS DETECTED IN CONNECTIVITY') 
        endif 
        if (l /= 0) return  
      endif 
      if (intl .or. lopt(1,1) == -6 .or. react(1) < -1.d5) l = -l ! Dummy use of intl, etc.
      time0 = second(2) 
      maxcyc = 100000 
      if (index(keywrd,' BIGCYCLES') /= 0) maxcyc = nint(reada(keywrd,index(&
        keywrd,' BIGCYCLES'))) 
!
!  SWAP FIRST AND SECOND GEOMETRIES AROUND
!  SO THAT GEOUT CAN OUTPUT DATA ON SECOND GEOMETRY.
!
      numat2 = 0 
      do i = 1, natoms 
        if (labels(i) /= 99) numat2 = numat2 + 1 
        x = geoa(1,i) 
        geoa(1,i) = geo(1,i) 
        geo(1,i) = x 
        x = geoa(2,i) 
!         X=GEOA(2,I)*0.0174532925199432957D0
        geoa(2,i) = geo(2,i) 
        geo(2,i) = x 
        x = geoa(3,i) 
!         X=GEOA(3,I)*0.0174532925199432957D0
        geoa(3,i) = geo(3,i) 
        geo(3,i) = x 
      end do 
      if (numat2 /= numat) then 
        write (iw, &
      '(2/10X,'' NUMBER OF ATOMS IN SECOND SYSTEM IS '',    ''INCORRECT'',/)') 
        write (iw, '('' NUMBER OF ATOMS IN FIRST  SYSTEM ='',I4)') numat 
        write (iw, '('' NUMBER OF ATOMS IN SECOND SYSTEM ='',I4)') numat2 
        go to 280 
      endif 
      write (iw, '(2/10X,'' GEOMETRY OF SECOND SYSTEM'',/)') 
      if (ndep /= 0) call symtry 
      call geout (1) 
!
!     CONVERT TO CARTESIAN, IF NECESSARY
!
      if (xyz) then 
        call gmetry (geo, coord) 
        if (moperr) return  
        sumx = 0.D0 
        sumy = 0.D0 
        sumz = 0.D0 
        do j = 1, numat 
          sumx = sumx + coord(1,j) 
          sumy = sumy + coord(2,j) 
          sumz = sumz + coord(3,j) 
        end do 
        sumx = sumx/numat 
        sumy = sumy/numat 
        sumz = sumz/numat 
        geo(1,:numat) = coord(1,:numat) - sumx 
        geo(2,:numat) = coord(2,:numat) - sumy 
        geo(3,:numat) = coord(3,:numat) - sumz 
        write (iw, '(2/,''  CARTESIAN GEOMETRY OF FIRST SYSTEM'',2/)') 
        write (iw, '(3F14.5)') ((geo(j,i),j=1,3),i=1,numat) 
        sumx = 0.D0 
        sumy = 0.D0 
        sumz = 0.D0 
        do j = 1, numat 
          sumx = sumx + geoa(1,j) 
          sumy = sumy + geoa(2,j) 
          sumz = sumz + geoa(3,j) 
        end do 
        sum = 0.D0 
        sumx = sumx/numat 
        sumy = sumy/numat 
        sumz = sumz/numat 
        do j = 1, numat 
          geoa(1,j) = geoa(1,j) - sumx 
          geoa(2,j) = geoa(2,j) - sumy 
          geoa(3,j) = geoa(3,j) - sumz 
          sum = sum + (geo(1,j)-geoa(1,j))**2 + (geo(2,j)-geoa(2,j))**2 + (geo(&
            3,j)-geoa(3,j))**2 
        end do 
        do l = 3, 1, -1 
!
!     DOCKING IS DONE IN STEPS OF 16, 4, AND 1 DEGREES AT A TIME.
!
          ca = cos(4.D0**(l - 1)*0.0174532925199432957D0) 
          sa = sqrt(abs(1.D0 - ca**2)) 
          l100: do j = 1, 3 
            ir = irot(1,j) 
            jr = irot(2,j) 
            do i = 1, 10 
              summ = 0.D0 
              do k = 1, numat 
                x = ca*geoa(ir,k) + sa*geoa(jr,k) 
                geoa(jr,k) = (-sa*geoa(ir,k)) + ca*geoa(jr,k) 
                geoa(ir,k) = x 
                summ = summ + (geo(1,k)-geoa(1,k))**2 + (geo(2,k)-geoa(2,k))**2&
                   + (geo(3,k)-geoa(3,k))**2 
              end do 
              if (summ > sum) then 
                if (i > 1) then 
                  sa = -sa 
                  do k = 1, numat 
                    x = ca*geoa(ir,k) + sa*geoa(jr,k) 
                    geoa(jr,k) = (-sa*geoa(ir,k)) + ca*geoa(jr,k) 
                    geoa(ir,k) = x 
                  end do 
                  cycle  l100 
                endif 
                sa = -sa 
              endif 
              sum = summ 
            end do 
          end do l100 
        end do 
        write (iw, '(2/,''  CARTESIAN GEOMETRY OF SECOND SYSTEM'',2/)') 
        write (iw, '(3F14.5)') ((geoa(j,i),j=1,3),i=1,numat) 
        write (iw, '(2/,''   "DISTANCE":'',F13.6)') sqrt(sum) 
        write (iw, '(2/,''  REACTION COORDINATE VECTOR'',2/)') 
        write (iw, '(3F14.5)') ((geoa(j,i) - geo(j,i),j=1,3),i=1,numat) 
        na(1) = 99 
        j = 0 
        nvar = 0 
        do i = 1, natoms 
          if (labels(i) == 99) cycle  
          j = j + 1 
          do k = 1, 3 
            nvar = nvar + 1 
            loc(2,nvar) = k 
            loc(1,nvar) = j 
          end do 
          labels(j) = labels(i) 
        end do 
        natoms = numat 
      endif 
!
!   XPARAM HOLDS THE VARIABLE PARAMETERS FOR GEOMETRY IN GEO
!   XOLD   HOLDS THE VARIABLE PARAMETERS FOR GEOMETRY IN GEOA
!
      if (nvar == 0) then 
        write (iw, &
      '(3/10X,''THERE ARE NO VARIABLES IN THE SADDLE'',    '' CALCULATION!'')') 
        call mopend ('THERE ARE NO VARIABLES IN THE SADDLE CALCULATION!') 
        return  
      endif 
      sum = 0.D0 
      do i = 1, nvar 
        grold(i) = 1.D0 
        xparam(i) = geo(loc(2,i),loc(1,i)) 
        xold(i) = geoa(loc(2,i),loc(1,i)) 
        sum = sum + (xparam(i)-xold(i))**2 
      end do 
      step0 = sqrt(sum) 
      if (step0 < 1.D-5) then 
        write (iw, '(2/,3(5X,A,/))') ' BOTH GEOMETRIES ARE IDENTICAL', &
          ' A SADDLE CALCULATION INVOLVES A REACTANT AND A PRODUCT', &
          ' THESE MUST BE DIFFERENT GEOMETRIES' 
        call mopend ('BOTH GEOMETRIES IN SADDLE ARE IDENTICAL.') 
        return  
      endif 
      one = 1.D0 
      dell = 0.1D0 
      eold = -2000.D0 
      time1 = second(2) 
      swap = 0.D0 
      do iloop = 1, maxstp 
        if (iloop >= maxcyc) tleft = -100.D0 
        write (iw, '('' '',40(''*+''))') 
!
!   THIS METHOD OF CALCULATING 'STEP' IS QUITE ARBITARY, AND NEEDS
!   TO BE IMPROVED BY INTELLIGENT GUESSWORK!
!
        gnorm = dmax1(1.D-3,gnorm) 
        step = min(min(min(min(swap,0.5D0),6.D0/gnorm),dell),stepmx*step0) 
        step = min(0.2D0,step/step0)*step0 
        swap = swap + 1.D0 
        dell = dell + 0.1D0 
        write (iw, '(''  BAR SHORTENED BY'',F12.7,'' PERCENT'')') step/step0*&
          100.D0 
        step0 = step0 - step 
        if (step0 < 0.01D0) exit  
        step = step0 
        xstore(:nvar) = xparam(:nvar) 
        if (lef) then 
          call ef (xparam, nvar, escf) 
          if (moperr) return  
        else 
          call flepo (xparam, nvar, escf) 
          if (moperr) return  
        endif 
        if (linear == 0) then 
          pastor = pa 
          pbstor = pb
        endif 
        do i = 1, nvar 
          xparam(i) = geo(loc(2,i),loc(1,i)) 
        end do 
        if (iflag == 1) then 
          write (iw, '(2/10X,''FOR POINT'',I3,'' SECOND STRUCTURE'')') iloop 
        else 
          write (iw, '(2/10X,''FOR POINT'',I3,'' FIRST  STRUCTURE'')') iloop 
        endif 
        write (iw, '('' DISTANCE A - B  '',F12.6)') step 
!
!   NOW TO CALCULATE THE "CORRECT" GRADIENTS, SWITCH OFF 'STEP'.
!
        step = 0.D0 
        grad(:nvar) = grold(:nvar) 
        call compfg (xparam, .TRUE., funct1, .TRUE., grad, .TRUE.) 
        if (moperr) return  
        grold(:nvar) = grad(:nvar) 
        if (gradnt) then 
          write (iw, '(''  ACTUAL GRADIENTS OF THIS POINT'')') 
          write (iw, '(8F10.4)') (grad(i),i=1,nvar) 
        endif 
        write (iw, '('' HEAT            '',F12.6)') funct1 
        gnorm = sqrt(dot(grad,grad,nvar)) 
        write (iw, '('' GRADIENT NORM   '',F12.6)') gnorm 
        cosine = cosine*one 
        write (iw, '('' DIRECTION COSINE'',F12.6)') cosine 
        call geout (iw) 
        if (swap>2.9D0 .or. iloop>3 .and. cosine<0.D0 .or. escf>eold) then 
          if (swap > 2.9D0) then 
            swap = 0.D0 
          else 
            swap = 0.5D0 
          endif 
!
!   SWAP REACTANT AND PRODUCT AROUND
!
          finish = gok(1) .and. gok(2) .and. cosine<0.D0 
          if (finish) then 
            write (iw, &
      '(2/10X,'' BOTH SYSTEMS ARE ON THE SAME '',2/    ''SIDE OF THE TRANSITION&
      & STATE -'',/10X,                          '' GEOMETRIES OF THE SYSTEMS'',&
      &                                   '' ON EACH SIDE OF THE T.S. ARE AS FOL&
      &LOWS'')') 
            xparam(:nvar) = xstore(:nvar) 
            call compfg (xparam, .TRUE., funct1, .TRUE., grad, .TRUE.) 
            if (moperr) return  
            write (iw, &
      '(2/10X,'' GEOMETRY ON ONE SIDE OF '',2/         ''THE TRANSITION'','' ST&
      &ATE'')') 
            call writmo () 
            if (moperr) return  
          endif 
          time2 = second(2) 
          write (iw, '('' TIME='',F9.2)') time2 - time1 
          time1 = time2 
          write (iw, '(''  REACTANTS AND PRODUCTS SWAPPED AROUND'')') 
          iflag = 1 - iflag 
          one = -1.D0 
          eold = escf 
          i = int(1.7D0 + one*0.5D0) 
          if (gnorm > 10.D0) gok(i) = .TRUE. 
          gnorm = sum 
          do i = 1, natoms 
            do j = 1, 3 
              x = geo(j,i) 
              geo(j,i) = geoa(j,i) 
              geoa(j,i) = x 
            end do 
          end do 
          do i = 1, nvar 
            x = xold(i) 
            xold(i) = xparam(i) 
            xparam(i) = x 
          end do 
!
!
!    SWAP AROUND THE DENSITY MATRICES.
!
          do i = 1, linear 
            x = pastor(i) 
            pastor(i) = pa(i) 
            pa(i) = x 
            x = pbstor(i) 
            pbstor(i) = pb(i) 
            pb(i) = x 
            p(i) = pa(i) + pb(i) 
          end do 
          if (finish) exit  
        else 
          one = 1.D0 
        endif 
      end do 
      write (iw, '('' AT END OF REACTION'')') 
      gold = sqrt(dot(grad,grad,nvar)) 
      call compfg (xparam, .TRUE., funct1, .TRUE., grad, .TRUE.) 
      if (moperr) return  
      gnorm = sqrt(dot(grad,grad,nvar)) 
      grold(:nvar) = xparam(:nvar) 
      call writmo () 
      if (moperr) return  
!
! THE GEOMETRIES HAVE (A) BEEN OPTIMIZED CORRECTLY, OR
!                     (B) BOTH ENDED UP ON THE SAME SIDE OF THE T.S.
!
!  TRANSITION STATE LIES BETWEEN THE TWO GEOMETRIES
!
      c1 = gold/(gold + gnorm) 
      c2 = 1.D0 - c1 
      write (iw, '('' BEST ESTIMATE GEOMETRY OF THE TRANSITION STATE'')') 
      write (iw, '(2/10X,'' C1='',F8.3,5X,''C2='',F8.3)') c1, c2 
      xparam(:nvar) = c1*grold(:nvar) + c2*xold(:nvar) 
      step = 0.D0 
      call compfg (xparam, .TRUE., funct1, .TRUE., grad, .TRUE.) 
      if (moperr) return  
      call writmo () 
      if (moperr) return  
  280 continue 
      return  
      end subroutine react1
