     subroutine drc(startv, startk) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : nvar, nalpha, tleft, escf, gnorm, ndep, numat, keywrd, &
      jobnam
      USE chanel_C, only : iw, iden, ires, iscr
      use permanent_arrays, only : geo, coord, loc, grad, xparam, na, atmass, &
      & pa, pb, errfn
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:08:47  03/16/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use second_I 
      use reada_I 
      use dot_I 
      use gmetry_I 
      use compfg_I 
      use prtdrc_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: startv(3*numat) 
      real(double) , intent(in) :: startk(3*numat) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, l, j, iskin,   iloop, k, kl, ncoprt, &
        ii, i1, maxcyc, iupper, ilp 
        integer, dimension(2,3*numat) :: mcoprt
      real(double), dimension(3*numat) :: velo0, velo1, velo2, velo3, gerror, grold2 
      real(double), dimension(10) :: past10 
      real(double), dimension(3*numat) :: grold 
      real(double), dimension(3,numat) :: georef 
      real(double) :: ekin, elost1, etold, dlold2, tnow, oldtim, delold, gtot, &
        accu, gnlim, half, addonk, deltat, quadr, etot, const, one, summ&
        , summas, ams, error, velvec, delta1, elost, scfold, sum, dummy, tcycle 
      logical :: addk, letot, let, velred, opend, parmax
!-----------------------------------------------
!***********************************************************************
!                                                                      *
!    DRC IS DESIGNED TO FOLLOW A REACTION PATH FROM THE TRANSITION     *
!    STATE.  TWO MODES ARE SUPPORTED, FIRST: GAS PHASE:- AS THE SYSTEM *
!    MOVES FROM THE T/S THE MOMENTUM OF THE ATOMS IS STORED AND THE    *
!    POSITION OF THE ATOMS IS RELATED TO THE OLD POSITION BY (A) THE   *
!    CURRENT VELOCITY OF THE ATOM, AND (B) THE FORCES ACTING ON THAT   *
!    ATOM.  THE SECOND MODE IS CONDENSED PHASE, IN WHICH THE ATOMS MOVE*
!    IN RESPONSE TO THE FORCES ACTING ON THEM. I.E. INFINITELY DAMPED  *
!                                                                      *
!***********************************************************************
      addk = .TRUE. 
      ekin = 0.D0 
      elost1 = 0.D0 
      etold = 0.D0 
      dlold2 = 0.D0 
      past10 = 0.D0 
      tnow = second(1) 
      oldtim = second(1) 
      delold = 10.D0 
      gtot = 0.D0 
      if (allocated(grad))  deallocate(grad)
      if (allocated(errfn)) deallocate(errfn)
      allocate(grad(3*numat), errfn(3*numat))
      errfn = 0.d0
      inquire(unit=iscr, opened=opend) 
      if (opend) close(unit=iscr) 
      open(unit=iscr, status='SCRATCH', position='asis') 
      if (index(keywrd,' PREC') /= 0) then 
        accu = 0.25D0 
      else 
        accu = 1.D0 
      endif 
      gnlim = 1.D0 
      past10(5) = 100.D0 
      i = index(keywrd,'GNORM') 
      if (i /= 0) gnlim = reada(keywrd,i) 
      velred = index(keywrd,'VELO') /= 0 
      if (dot(startv,startv,3*numat) > 0.001D0) then 
!
!     PRINT OUT INITIAL VELOCITIES
!
        write (iw, '(A)') ' INITIAL VELOCITY IN DRC' 
        write (iw, '(3F16.5)') (startv(i),i=1,numat*3) 
      endif 
!#      CALL ARREST(STARTV,GEO,ATMASS,NUMAT)
!#      IF(DOT(STARTV,STARTV,3*NUMAT).GT.0.001D0)THEN
!#C
!#C     PRINT OUT INITIAL VELOCITIES, AFTER ARRESTING ANY TRANSLATIONS
!#C
!# C#        WRITE(IW,'(A)')' INITIAL VELOCITY IN DRC AFTER'//
!#     1' REMOVING NET TRANSLATIONS AND ROTATIONS'
!#         WRITE(IW,'(3F16.5)')(STARTV(I),I=1,NUMAT*3)
!#      ENDIF
!#      LET=(INDEX(KEYWRD,' GEO-OK').NE.0.OR.VELRED)
      let = velred 
      if (index(keywrd,' SYMM') /= 0) then 
        write (iw, *) '  SYMMETRY SPECIFIED, BUT CANNOT BE USED IN DRC' 
        ndep = 0 
      endif 
!
!      CONVERT TO CARTESIAN COORDINATES, IF NOT ALREADY DONE.
!
      if (index(keywrd,' XYZ') + index(keywrd,'VELO') == 0) then 
        na(1) = 0 
        call gmetry (geo, coord) 
        l = 0 
!
        geo(:,:numat) = coord(:,:numat) 
        coord(:,:numat) = 0.0D0 
!
        na(1) = 99 
      endif 
      do i = 1, numat 
        if (atmass(i) >= 1.D-1) cycle  
        write (iw, '(A,I3,A)') ' ATOMIC MASS OF ATOM', i, ' TOO SMALL' 
        return  
      end do 
!
!  TRANSFER COORDINATES TO XPARAM AND LOC
!
      if (index(keywrd,' DRC') /= 0) then 
        parmax = .true. ! warning loc(1,1) == 1 
        if (parmax) then 
          j = 1 
        else 
          j = 0 
        endif 
        nvar = nvar - j 
        mcoprt(1,:nvar) = loc(1,1+j:nvar+j) 
        mcoprt(2,:nvar) = loc(2,1+j:nvar+j) 
        if (loc(1,1) == 0) nvar = 0 
        ncoprt = nvar 
      else 
        ncoprt = 0 
      endif 
      l = 0 
      do i = 1, numat 
        loc(1,l+1) = i 
        loc(2,l+1) = 1 
        georef(1,i) = geo(1,i) 
        xparam(l+1) = geo(1,i) 
!
        loc(1,l+2) = i 
        loc(2,l+2) = 2 
        georef(2,i) = geo(2,i) 
        xparam(l+2) = geo(2,i) 
!
        loc(1,l+3) = i 
        loc(2,l+3) = 3 
        georef(3,i) = geo(3,i) 
        xparam(l+3) = geo(3,i) 
!
        l = l + 3 
      end do 
      nvar = numat*3 
!
! DETERMINE DAMPING FACTOR
!
      if (index(keywrd,'DRC=') /= 0) then 
        half = reada(keywrd,index(keywrd,'DRC=')) 
        write (iw, &
      '(2/10X,                                              '' DAMPING FACTOR F&
      &OR KINETIC ENERGY ='',F12.6)') half 
      else if (index(keywrd,'DRC') == 0) then 
        half = 0.D0 
      else 
        half = 1.D6 
      endif 
!
!  LETOT IS TRUE IF CORRECTIONS ARE NOT TO BE MADE PART WAY INTO
!        THE CALCULATION
!
!  USAGE OF LETOT:
! (1) WHILE LETOT IS FALSE, NO DAMPING WILL BE DONE
! (2) WHEN LETOT IS TURNED TRUE,
!     IF AN IRC, THEN ETOT IS RESET SO THE ERROR IS ZERO.
!     IF A  DRC, EXCESS KINETIC ENERGY USED TO START THE RUN IS REMOVED.
!
      letot = index(keywrd,'IRC=')==0 .and. .not.let 
      half = sign(max(0.000001D0,abs(half)),half) 
!
! DETERMINE EXCESS KINETIC ENERGY
!
      iskin = 0 
      if (index(keywrd,'KINE') /= 0) then 
        iskin = 1 
        addonk = reada(keywrd,index(keywrd,'KINE')) 
        write (iw, &
      '(2/10X,                                              '' EXCESS KINETIC E&
      &NERGY ENTERED INTO SYSTEM ='',F12.6)') addonk 
      else 
        addonk = 0.D0 
      endif 
!
!   LOOP OVER TIME-INTERVALS OF DELTAT SECOND
!
      deltat = 1.D-16 
      quadr = 1.D0 
      etot = 0.D0 
      escf = 0.D0 
      const = 1.D0 
      if (index(keywrd,'RESTART')/=0 .and. index(keywrd,'IRC=')==0) then 
!
!  RESTART FROM A PREVIOUS RUN
!
        i = index(jobnam,' ') - 1 
        inquire(unit=ires, opened=opend) 
        if (opend) close(unit=ires, status='KEEP') 
          open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
        rewind ires 
        inquire(unit=iden, opened=opend) 
        if (opend) close(unit=iden, status='KEEP') 
        open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
          'UNFORMATTED', position='asis') 
        rewind iden 
          read (ires) (xparam(i),i=1,nvar) 
          read (ires) (velo0(i),i=1,nvar) 
          read (ires) (grad(i),i=1,nvar) 
          read (ires) (grold(i),i=1,nvar) 
          read (ires) (grold2(i),i=1,nvar) 
          read (ires) etot, escf, ekin, delold, deltat, dlold2, iloop, gnorm, &
            letot, elost1, gtot 
        write (iw, &
      '(2/10X,''CALCULATION RESTARTED, CURRENT'',           '' KINETIC ENERGY='&
      &',F10.5,2/)') ekin 
        go to 100 
      else 
!                         NOT A RESTART
        iloop = 1 
        velo0(:nvar) = 0.D0 
        grold2(:nvar) = 0.D0 
        grold(:nvar) = 0.D0 
        grad(:nvar) = 0.D0 
        if (index(keywrd,'IRC=')/=0 .or. velred) then 
!
!  GET HOLD OF VELOCITY VECTOR
!
          if (index(keywrd,'IRC=') /= 0) then 
            k = nint(reada(keywrd,index(keywrd,'IRC='))) 
          else 
            k = 1 
          endif 
          if (k < 0) then 
            k = -k 
            one = -1.D0 
          else 
            one = 1.D0 
          endif 
          kl = (k - 1)*nvar 
          summ = 0.D0 
          velo1(1) = 0.D0 
          velo1(2) = 0.D0 
          velo1(3) = 0.D0 
          summas = 0.D0 
          i = 0 
          do ii = 1, numat 
            ams = atmass(ii) 
            summas = summas + ams 
            velo0(i+1) = startv(kl+i+1)*one 
            velo1(1) = velo1(1) + velo0(i+1)*ams 
!
            velo0(i+2) = startv(kl+i+2)*one 
            velo1(2) = velo1(2) + velo0(i+2)*ams 
!
            velo0(i+3) = startv(kl+i+3)*one 
            velo1(3) = velo1(3) + velo0(i+3)*ams 
!
            i = i + 3 
          end do 
!$DOIT ASIS
          velo1(:3) = -velo1(:3)/summas 
          i = 0 
!$DOIT ASIS
          if (addonk>1.D-5 .or. .not.velred) then 
            do ii = 1, numat 
              ams = atmass(ii) 
!$DOIT ASIS
              do i1 = 1, 3 
                i = i + 1 
                velo0(i) = velo0(i) + velo1(i1) 
                summ = summ + velo0(i)**2*ams 
              end do 
            end do 
          else 
            do ii = 1, numat 
              ams = atmass(ii) 
!$DOIT ASIS
              do i1 = 1, 3 
                i = i + 1 
                summ = summ + velo0(i)**2*ams 
              end do 
            end do 
          endif 
          if (addonk<1.D-5 .and. velred) addonk = 0.5D0*summ/4.184D10 
          if (addonk<1.D-5 .and. .not.velred) then 
            if (abs(half)>1.D-3 .and. startk(k)>105.D0) then 
              write (iw, '(A,F10.3,A,/,A)') &
                ' BY DEFAULT, ONE QUANTUM OF ENERGY, EQUIVALENT TO', startk(k)&
                , ' CM(-1)', ' WILL BE USED TO START THE DRC' 
!
!    2.8585086D-3 CONVERTS CM(-1) INTO KCAL/MOLE
!
              addonk = startk(k)*2.8585086D-3 
              write (iw, '(A,F7.2,A)') ' THIS REPRESENTS AN ENERGY OF', addonk&
                , ' KCALS/MOLE' 
            else if (abs(half) > 1.D-3) then 
              write (iw, '(A,F9.2,A)') ' THE VIBRATIONAL FREQUENCY (', startk(k&
                ), 'CM(-1)) IS TOO SMALL', ' FOR ONE QUANTUM TO BE USED' 
              write (iw, '(A)') &
                ' INSTEAD 0.3KCAL/MOLE WILL BE USED TO START THE IRC' 
              addonk = 0.3D0 
            else 
              addonk = 0.3D0 
            endif 
          endif 
!
!   AT THIS POINT ADDONK IS IN KCAL/MOLE
!   NORMALIZE SO THAT TOTAL K.E. = ONE QUANTUM (DEFAULT) (DRC ONLY)
!                              OR 0.3KCAL/MOLE (IRC ONLY)
!                              OR ADDONK IF KINETIC=NN SUPPLIED
!
          if (summ < 1.D-4) then 
            write (iw, '(A)') ' SYSTEM IS APPARENTLY NOT MOVING!' 
            return  
          endif 
!
!  ADDONK IS EXCESS KINETIC ENERGY.  IF THE CALCULATION IS AN IRC,
!  THIS ENERGY MUST BE REMOVED AFTER A SHORT 'TIME'.
!
!  MAKE AN AD-HOC CORRECTION: IF ADDONK IS NON-ZERO AND HALF IS LARGER
!  THAN 0.1, MODIFY ADDONK TO REFLECT ERRORS DUE TO START-UP.
!
          if (half>0.1D0 .and. half<10000.D0) addonk = addonk*(1.D0 + 0.06972D0&
            /half) 
!
!  MAKE AN AD-HOC CORRECTION: IF ADDONK IS NON-ZERO AND HALF IS LESS
!  THAN -0.1, MODIFY ADDONK TO REFLECT ERRORS DUE TO START-UP.
!
          if (half<(-0.1D0) .and. half>(-10000.D0)) addonk = addonk*(1.D0 + &
            0.06886D0/half) 
          summ = sqrt(addonk/(0.5D0*summ/4.184D10)) 
          addk = .FALSE. 
          if (summ > 1.D-10) then 
            velo0(:nvar) = velo0(:nvar)*summ 
!
!  IF IT IS A DRC, DESTROY ADDONK.  THE KINETIC ENERGY USED WILL COME
!  FROM THE VELOCITY ONLY.
!
            if (half > 1.D-3) addonk = 0.D0 
          endif 
        endif 
      endif 
  100 continue 
      maxcyc = 4999 
      if (index(keywrd,' CYCLES') /= 0) maxcyc = nint(reada(keywrd,index(keywrd&
        ,' CYCLES'))) 
      iupper = iloop + maxcyc 
      ilp = iloop 
      one = 0.D0 
      if (index(keywrd,'RESTART')/=0 .and. index(keywrd,'IRC=')==0) one = 1.D0 
      gerror(:nvar) = 0.D0 
      do iloop = ilp, iupper 
!
!  MOVEMENT OF ATOMS WILL BE PROPORTIONAL TO THE AVERAGE VELOCITIES
!  OF THE ATOMS BEFORE AND AFTER TIME INTERVAL
!
!
!  RAPID CHANGE IN GRADIENT IMPLIES SMALL STEP SIZE FOR DELTAT
!
!   KINETIC ENERGY = 1/2 * M * V * V
!                  = 0.5 / (4.184D10) * M * V * V
!   NEW VELOCITY = OLD VELOCITY + GRADIENT * TIME / MASS
!                = KCAL/ANGSTROM*SECOND/(ATOMIC WEIGHT)
!                =4.184*10**10(ERGS)*10**8(PER CM)*DELTAT(SECONDS)
!   NEW POSITION = OLD POSITION - AVERAGE VELOCITY * TIME INTERVAL
!
!
!   ESTABLISH REFERENCE TOTAL ENERGY
!
        error = etot - (ekin + escf) 
        if (iloop > 2) then 
          quadr = 1.D0 + error/(ekin*const + 0.001D0)*0.5D0 
          quadr = min(1.3D0,max(0.8D0,quadr)) 
        else 
          quadr = 1.D0 
        endif 
        if ((let .or. ekin>0.2D0) .and. addk) then 
!
!   DUMP IN EXCESS KINETIC ENERGY
!
          etot = etot + addonk 
          addk = .FALSE. 
          addonk = 0.D0 
        endif 
!
!  CALCULATE THE DURATION OF THE NEXT STEP.
!  STEP SIZE IS THAT REQUIRED TO PRODUCE A CONSTANT CHANGE IN GEOMETRY
!
!
!  IF DAMPING IS USED, CALCULATE THE NEW TOTAL ENERGY AND
!  THE RATIO FOR REDUCING THE KINETIC ENERGY
!
        const = max(1.D-36,0.5D0**(deltat*1.D15/half)) 
        const = sqrt(const) 
        velvec = 0.D0 
        ekin = 0.D0 
        delta1 = delold + dlold2 
        elost = 0.D0 
        if (iloop > 3) then 
          do i = 1, nvar 
!
!   CALCULATE COMPONENTS OF VELOCITY AS
!   V = V(0) + V'*T + V"*T*T
!   WE NEED ALL THREE TERMS, V(0), V' AND V"
!
            velo1(i) = 1.D0/atmass(loc(1,i))*grad(i) 
            velo3(i) = 2.D0/atmass(loc(1,i))*(delta1*(grold(i)-grad(i))-delold*&
              (grold2(i)-grad(i)))/(delta1*(delold**2*1.D30) - delold*(delta1**&
              2*1.D30)) 
            velo2(i) = 1.D0/atmass(loc(1,i))*(grad(i)-grold(i)-0.5D0*velo3(i)*(&
              1.D30*delold**2))/(delold*1.D15) 
!
!  MOVE ATOMS THROUGH DISTANCE EQUAL TO VELOCITY * DELTA-TIME, NOTE
!  VELOCITY CHANGES FROM START TO FINISH, THEREFORE AVERAGE.
!
            xparam(i) = xparam(i) - 1.D8*(deltat*velo0(i)*one+0.5D0*deltat**2*&
              velo1(i)+0.16666D0*(deltat**2*1.D15)*deltat*velo2(i)+0.0416666D0*&
              deltat**2*(1.D30*deltat**2)*velo3(i)) 
!
!   CORRECT ERRORS DUE TO CUBIC COMPONENTS IN ENERGY GRADIENT,
!   ALSO TO ADD ON EXCESS ENERGY, IF NECESSARY.
!
            velvec = velvec + velo0(i)**2 
!
!   MODIFY VELOCITY IN LIGHT OF CURRENT ENERGY GRADIENTS.
!
!   VELOCITY = OLD VELOCITY + (DELTA-T / ATOMIC MASS) * CURRENT GRADIENT
!                           + 1/2 *(DELTA-T * DELTA-T /ATOMIC MASS) *
!                             (SLOPE OF GRADIENT)
!              SLOPE OF GRADIENT = (GRAD(I)-GROLD(I))/DELOLD
!
!
!   THIS EXPRESSION IS ACCURATE TO SECOND ORDER IN TIME.
!
            velo0(i) = velo0(i) + deltat*velo1(i) + 0.5D0*deltat**2*velo2(i)*&
              1.D15 + 0.166666D0*deltat*(1.D30*deltat**2)*velo3(i) 
            if (let .or. gnorm>3.D0) then 
              let = .TRUE. 
              elost = elost + velo0(i)**2*atmass(loc(1,i))*(1 - const**2) 
              velo0(i) = velo0(i)*const*quadr 
            endif 
!
!  CALCULATE KINETIC ENERGY (IN 2*ERGS AT THIS POINT)
!
            ekin = ekin + velo0(i)**2*atmass(loc(1,i)) 
          end do 
        else 
          do i = 1, nvar 
!
!   CALCULATE COMPONENTS OF VELOCITY AS
!   V = V(0) + V'*T + V"*T*T
!   WE NEED ALL THREE TERMS, V(0), V' AND V"
!
            velo1(i) = 1.D0/atmass(loc(1,i))*grad(i) 
            velo2(i) = 1.D0/atmass(loc(1,i))*(grad(i)-grold(i))/(1.D15*delold) 
            velo3(i) = 0.D0 
!
!  MOVE ATOMS THROUGH DISTANCE EQUAL TO VELOCITY * DELTA-TIME, NOTE
!  VELOCITY CHANGES FROM START TO FINISH, THEREFORE AVERAGE.
!
            xparam(i) = xparam(i) - 1.D8*(deltat*velo0(i)*one+0.5D0*deltat**2*&
              velo1(i)+0.16666D0*(deltat**2*1.D15)*deltat*velo2(i)+0.0416666D0*&
              deltat**2*(1.D30*deltat**2)*velo3(i)) 
!
!   CORRECT ERRORS DUE TO CUBIC COMPONENTS IN ENERGY GRADIENT,
!   ALSO TO ADD ON EXCESS ENERGY, IF NECESSARY.
!
            velvec = velvec + velo0(i)**2 
!
!   MODIFY VELOCITY IN LIGHT OF CURRENT ENERGY GRADIENTS.
!
!   VELOCITY = OLD VELOCITY + (DELTA-T / ATOMIC MASS) * CURRENT GRADIENT
!                           + 1/2 *(DELTA-T * DELTA-T /ATOMIC MASS) *
!                             (SLOPE OF GRADIENT)
!              SLOPE OF GRADIENT = (GRAD(I)-GROLD(I))/DELOLD
!
!
!   THIS EXPRESSION IS ACCURATE TO SECOND ORDER IN TIME.
!
            velo0(i) = velo0(i) + deltat*velo1(i) + 0.5D0*deltat**2*velo2(i)*&
              1.D15 + 0.166666D0*deltat*(1.D30*deltat**2)*velo3(i) 
            if (let .or. gnorm>3.D0) then 
              let = .TRUE. 
              elost = elost + velo0(i)**2*atmass(loc(1,i))*(1 - const**2) 
              velo0(i) = velo0(i)*const*quadr 
            endif 
!
!  CALCULATE KINETIC ENERGY (IN 2*ERGS AT THIS POINT)
!
            ekin = ekin + velo0(i)**2*atmass(loc(1,i)) 
          end do 
        endif 
        one = 1.D0 
        if (let .or. gnorm>3.D0) then 
          if (.not.letot) then 
            if (abs(half) < 1.D-3) then 
!
!  IT IS AN IRC, SO RESET THE TOTAL ENERGY
!
              etot = escf + elost1 
              addonk = 0.D0 
              elost1 = 0.D0 
              elost = 0.D0 
            else if (iskin == 0) then 
!
!  IT IS A DRC AND KINETIC NOT USED, SO REMOVE EXTRA KINETIC ENERGY
!
              etot = etot - addonk 
            endif 
          endif 
          letot = .TRUE. 
        endif 
!
!  CONVERT ENERGY INTO KCAL/MOLE
!
        ekin = 0.5D0*ekin/4.184D10 
!
!  IF IT IS A DAMPED DRC, MODIFY ETOT TO REFLECT LOSS OF KINETIC ENERGY
!
        if (letot .and. abs(half)>0.00001D0) etot = etot - ekin/const**2 + ekin 
        elost1 = elost1 + 0.5D0*elost/4.184D10 
!
! STORE OLD GRADIENTS FOR DELTA - VELOCITY CALCULATION
!
        grold2(:nvar) = grold(:nvar) 
        grold(:nvar) = grad(:nvar) 
        grad(:nvar) = 0.D0 
!
!   CALCULATE ENERGY AND GRADIENTS
!
        scfold = escf 
        call compfg (xparam, .TRUE., escf, .TRUE., grad, .TRUE.) 
        if (iloop > 2) then 
          gnorm = 0.D0 
          do i = 1, nvar, 3 
            sum = sqrt(dot(grad(i),grad(i),3)/(dot(velo0(i),velo0(i),3)+1.D-20)&
              ) 
            gerror(i:2+i) = gerror(i:2+i) + grad(i:2+i) + velo0(i:2+i)*sum 
          end do 
          gnorm = sqrt(dot(gerror,gerror,nvar)) 
          gtot = gnorm 
        endif 
        gnorm = sqrt(dot(grad,grad,nvar)) 
!
!   CONVERT GRADIENTS INTO ERGS/CM
!
        grad(:nvar) = grad(:nvar)*4.184D18 
!
!   SPECIAL TREATMENT FOR FIRST POINT - SET "OLD" GRADIENTS EQUAL TO
!   CURRENT GRADIENTS.
!
        if (iloop == 1) then 
          grold(:nvar) = grad(:nvar) 
        endif 
        dlold2 = delold 
        delold = deltat 
        sum = 0.D0 
        do i = 1, nvar 
          sum = sum + ((grad(i)-grold(i))/4.184D18)**2 
        end do 
        if (abs(half) < 0.001D0) then 
          deltat = deltat*min(2.D0,5.D-5*accu/(abs(escf + elost1 - etold) + &
            1.D-20))**0.25D0 
          etold = escf + elost1 
          if (iloop>5 .and. scfold-escf<(-1.D-3) .or. iloop>30 .and. scfold-&
            escf<0.D0) then 
            write (iw, '(2/,'' IRC CALCULATION COMPLETE '')') 
            return  
          endif 
        else 
          deltat = deltat*min(1.05D0,10.D0*accu/(sum + 1.D-4)) 
          deltat = min(deltat,3.D-15*accu) 
          past10(10) = gnorm 
          sum = 0.D0 
          do i = 1, 9 
            sum = sum + abs(past10(i)-past10(i+1)) 
            past10(i) = past10(i+1) 
          end do 
          if (sum < gnlim) then 
            write (iw, '(2/,A)') &
              ' GRADIENT CONSTANT AND SMALL -- ASSUME ALL MOTION STOPPED' 
            write (iw, '(A)') ' TO CONTINUE, USE KEYWORD ''GNORM=0''' 
            return  
          endif 
          deltat = min(deltat,2.D-15) 
!***********************************************************************
!
!         TESTING CODE - REMOVE BEFORE FINAL VERSION ASSEMBLED
!#          (ILOOP/400)*400.EQ.ILOOP)DELTAT=-DELTAT
!
!***********************************************************************
        endif 
        deltat = max(1.D-16,deltat) 
        if (abs(half) < 0.00001D0) then 
!
!   FOR THE IRC:
!
! ESCF   = POTENTIAL ENERGY
! ELOST1 = ENERGY LOST (IN DRC, THIS WOULD HAVE BEEN THE KINETIC ENERGY)
! ETOT   = COMPUTED TOTAL ENERGY = STARTING POTENTIAL ENERGY
!
!   IN DRCOUT  'TOTAL' = ESCF + ELOST1
!              'ERROR' = ESCF + ELOST1 - ETOT
!
          call prtdrc (deltat, xparam, georef, elost1, gtot, etot, velo0&
            , mcoprt, ncoprt, parmax) 
        else 
!
!   FOR THE DRC:
!
! ESCF   = POTENTIAL ENERGY
! EKIN   = CURRENT KINETIC ENERGY
! ETOT   = COMPUTED TOTAL ENERGY = STARTING POTENTIAL ENERGY -
!          KINETIC ENERGY LOST THROUGH DAMPING, IF PRESENT.
!
!   IN DRCOUT  'TOTAL' = ESCF + EKIN
!              'ERROR' = ESCF + EKIN - ETOT
!
          call prtdrc (deltat, xparam, georef, ekin, dummy, etot, velo0, &
            mcoprt, ncoprt, parmax) 
        endif 
        tnow = second(2) 
        tcycle = tnow - oldtim 
        oldtim = tnow 
        tleft = tleft - tcycle 
        if (iloop/=iupper .and. tleft>=3*tcycle) cycle  
        i = index(jobnam,' ') - 1 
        inquire(unit=ires, opened=opend) 
        if (opend) close(unit=ires, status='DELETE') 
          open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
        rewind ires 
        open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
          'UNFORMATTED', position='asis') 
        rewind iden 
          write (ires) (xparam(i),i=1,nvar) 
          write (ires) (velo0(i),i=1,nvar) 
          write (ires) (grad(i),i=1,nvar) 
          write (ires) (grold(i),i=1,nvar) 
          write (ires) (grold2(i),i=1,nvar) 
          i = iloop + 1 
          write (ires) etot, escf, ekin, delold, deltat, dlold2, i, gnorm, &
            letot, elost1, gtot 
        escf = -1.D9 
        call prtdrc (deltat, xparam, georef, ekin, elost, etot, velo0, &
          mcoprt, ncoprt, parmax) 
        write (iden) pa
        if (nalpha /= 0) write (iden) pb
        write (iw, '(2/10X,'' RUNNING OUT OF TIME, RESTART FILE WRITTEN'')') 
        write (iw, '(A)') ' GEOMETRY AND VELOCITY ARE IN RESTART FILE IN ASCII' 
        return  
      end do 
      return   
      end subroutine drc 
