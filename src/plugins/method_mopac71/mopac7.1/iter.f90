      subroutine iter(ee, fulscf, rand) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : eigs, p, pa, pb, cb, h, nw, &
       &  c, nat, nfirst, nlast, eigb, pdiag, f, w, wj, wk, fb
      use iter_C, only : pold, pold2, pbold, pbold2, &
      & pold3, pbold3     
      USE parameters_C, only :  
      USE funcon_C, only : fpc_9
      USE maps_C, ONLY: latom 
      USE chanel_C, only : iw, iden
      USE euler_C, only : id 
      USE molkst_C, ONLY: numat, norbs, nalpha, nbeta, uhf, &
    &    nclose, nopen, fract, numcal, mpack, iflepo, iscf, &
    &    enuclr, keywrd, gnorm, moperr, last, nscf, emin, lm6, &
         method_dorbs, limscf, atheat, jobnam 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:49:43  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use meci_I 
      use reada_I 
      use mopend_I 
      use vecprt_I 
      use second_I 
      use timer_I 
      use fockd2_I 
      use fock2_I 
      use fockd1_I 
      use fock1_I 
      use writmo_I 
      use helect_I 
      use capcor_I 
      use interp_I 
      use pulay_I 
      use rsp_I 
      use diag_I 
      use matout_I 
      use swap_I 
      use densit_I 
      use cnvg_I 
      use chrge_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(out) :: ee 
      logical , intent(in) :: fulscf 
      logical , intent(in) :: rand 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      double precision :: selcon
      integer :: l, icalcn, itrmax, na2el, na1el, nb2el, nb1el, ifill, &
        irrr, jalp, ialp, jbet, ibet, ihomo, ihomob, i, j, iemin, &
        iemax, iredy, niter, modea, modeb
      real(double), dimension(numat) :: q 
      real(double), dimension(10) :: escf0 
      real(double) :: plb, scfcrt, pl, bshift, pltest, trans, w1, w2, random, &
        shift, shiftb, shfmax, ten, tenold, plchek, scorr, shfto, &
        shftbo, titer0, t1, eold, diff, tf1, enrgy, titer, t0, escf, &
        sellim, sum
      logical :: debug, prtfok, prteig, prtden, prt1el, minprt, newdg, prtpl, &
        prtvec, camkin, ci, okpuly, scf1, oknewd, times, force, allcon, &
        halfe, gs, capps, incitr, timitr, frst, bfrst, ready, glow,  &
        makea, makeb, getout 
      character, dimension(3) :: abprt*5 

      save icalcn, debug, prtfok, prteig, prtden, prt1el, abprt, plb, &
        minprt, newdg, scfcrt, prtpl, prtvec, pl, bshift, pltest, itrmax, na2el&
        , na1el, nb2el, nb1el, ifill, camkin, ci, okpuly, scf1, oknewd, &
        times, force, allcon, trans, halfe, w1, w2, random, gs, shift, shiftb, &
        shfmax, capps, ten, tenold, incitr, irrr, plchek, timitr, &
        scorr, shfto, shftbo, titer0, t1, jalp, ialp, jbet, ibet 
!-----------------------------------------------
!***********************************************************************
!
!     ITER GENERATES A SCF FIELD AND RETURNS THE ENERGY IN "ENERGY"
!
! THE MAIN ARRAYS USED IN ITER ARE:
!            P      ONLY EVER CONTAINS THE TOTAL DENSITY MATRIX
!            PA     ONLY EVER CONTAINS THE ALPHA DENSITY MATRIX
!            PB     ONLY EVER CONTAINS THE BETA DENSITY MATRIX
!            C      ONLY EVER CONTAINS THE EIGENVECTORS
!            H      ONLY EVER CONTAINS THE ONE-ELECTRON MATRIX
!            F      STARTS OFF CONTAINING THE ONE-ELECTRON MATRIX,
!                   AND IS USED TO HOLD THE FOCK MATRIX
!            W      ONLY EVER CONTAINS THE TWO-ELECTRON MATRIX
!
! THE MAIN INTEGERS CONSTANTS IN ITER ARE:
!
!            LINEAR SIZE OF PACKED TRIANGLE = NORBS*(NORBS+1)/2
!
! THE MAIN INTEGER VARIABLES ARE
!            NITER  NUMBER OF ITERATIONS EXECUTED
!
!  PRINCIPAL REFERENCES:
!
!   ON MNDO: "GROUND STATES OF MOLECULES. 38. THE MNDO METHOD.
!             APPROXIMATIONS AND PARAMETERS."
!             DEWAR, M.J.S., THIEL,W., J. AM. CHEM. SOC.,99,4899,(1977).
!   ON SHIFT: "THE DYNAMIC 'LEVEL SHIFT' METHOD FOR IMPROVING THE
!             CONVERGENCE OF THE SCF PROCEDURE", A. V. MITIN, J. COMP.
!             CHEM. 9, 107-110 (1988)
!   ON HALF-ELECTRON: "MINDO/3 COMPARISON OF THE GENERALIZED S.C.F.
!             COUPLING OPERATOR AND "HALF-ELECTRON" METHODS FOR
!             CALCULATING THE ENERGIES AND GEOMETRIES OF OPEN SHELL
!             SYSTEMS"
!             DEWAR, M.J.S., OLIVELLA, S., J. CHEM. SOC. FARA. II,
!             75,829,(1979).
!   ON PULAY'S CONVERGER: "CONVERGANCE ACCELERATION OF ITERATIVE
!             SEQUENCES. THE CASE OF SCF ITERATION", PULAY, P.,
!             CHEM. PHYS. LETT, 73, 393, (1980).
!   ON CNVG:  IT ENCORPORATES THE IMPROVED ITERATION SCHEME (IIS) BY
!             PIOTR BADZIAG & FRITZ SOLMS. ACCEPTED FOR PUBLISHING
!             IN COMPUTERS & CHEMISTRY
!   ON PSEUDODIAGONALISATION: "FAST SEMIEMPIRICAL CALCULATIONS",
!             STEWART. J.J.P., CSASZAR, P., PULAY, P., J. COMP. CHEM.,
!             3, 227, (1982)
!
!***********************************************************************
!***********************************************************************
!                                                                      *
!   PACK ALL THE ARRAYS USED BY PULAY INTO A COMMON BLOCK SO THAT THEY *
!   CAN BE USED BY THE C.I. DERIVATIVE, IF NEEDED                      *
!                                                                      *
!***********************************************************************
      data icalcn/ 0/  
      data debug/ .FALSE./  
      data prtfok/ .FALSE./  
      data t1/ 0.D0/  
      data titer0/ 0.D0/  
      data prteig/ .FALSE./  
      data prtden/ .FALSE./  
      data prt1el/ .FALSE./   
      data ten/ 10.D0/  
      data tenold/ 10.D0/  
      data plb/ 0.D0/  
      data scorr/ 0.D0/  
      data abprt/ '     ', 'ALPHA', ' BETA'/  
!
!  INITIALIZE
!
      ifill = 0 
      ihomo = max(1,nclose + nalpha) 
      ihomob = max(1,nclose + nbeta) 
      eold = 1.D2 
      ready = .FALSE. 
      diff = 0.D0 
      tf1 = 0.D0 
      escf0 = 0.D0 
      if (icalcn /= numcal) then 
        enrgy = fpc_9
        glow = .FALSE. 
        irrr = 5 
        shift = 0.D0 
        icalcn = numcal 
        shfmax = 20.D0 
!
!    DEBUG KEY-WORDS WORKED OUT
!
        debug = index(keywrd,' DEBUG') /= 0 
        minprt = index(keywrd,' SADDLE') + latom==0 .or. debug 
        prteig = index(keywrd,' eigs') /= 0 
        prtpl = index(keywrd,' PL ') /= 0 
        prt1el = index(keywrd,' 1ELE')/=0 .and. debug 
        prtden = index(keywrd,' DENS')/=0 .and. debug 
        prtfok = index(keywrd,' FOCK')/=0 .and. debug 
        prtvec = index(keywrd,' VECT')/=0 .and. debug 
        debug = index(keywrd,' ITER') /= 0 
!
! INITIALIZE SOME LOGICALS AND CONSTANTS
!
        newdg = .FALSE. 
        plchek = 0.005D0 
        pl = 1.D0 
        bshift = 0.D0 
        shift = 1.D0 
        shfto = 0.D0 
        shftbo = 0.D0 
        itrmax = 200 
        na2el = nclose 
        na1el = nalpha + nopen 
        nb2el = 0 
        nb1el = nbeta + nopen 
!
!  USE KEY-WORDS TO ASSIGN VARIOUS CONSTANTS
!
        if (index(keywrd,' FILL') /= 0) ifill = -nint(reada(keywrd,index(keywrd&
          ,' FILL'))) 
        if (index(keywrd,' SHIFT') /= 0) bshift = -reada(keywrd,index(keywrd,&
          ' SHIFT')) 
        if (Abs(bshift) > 1.d-20) ten = bshift 
        if (index(keywrd,' ITRY') /= 0) itrmax = nint(reada(keywrd,index(keywrd&
          ,' ITRY'))) 
        camkin = index(keywrd,'KING') + index(keywrd,'CAMP') /= 0 
        if (camkin .and. uhf) then
          write(iw,*)" In this program, the Camp-King converger does not work with UHF"
          call mopend ('Camp-King and UHF are not allowed') 
          return  
        end if
        ci = index(keywrd,' MICROS') + index(keywrd,' C.I.') /= 0 
        okpuly = .FALSE. 
        okpuly = index(keywrd,' PULAY') /= 0 
        scf1 = index(keywrd,' 1SCF') /= 0 
        oknewd = abs(bshift) < 0.001D0 
        if (camkin .and. abs(bshift)>1.D-5) bshift = 4.44D0 
        times = index(keywrd,' TIMES') /= 0 
        timitr = times .and. debug 
        force = index(keywrd,' FORCE') /= 0 
        gs = index(keywrd,' TS') + index(keywrd,' NLLSQ') + index(keywrd,&
          ' SIGMA')==0 .and. .not.force 
        allcon = okpuly .or. camkin 
!
!   DO WE NEED A CAPPED ATOM CORRECTION?
!
        j = 0 
        j = j + count(nat(:numat)==102) 
        capps = j > 0 
        iscf = 1 
        trans = 0.200D0 
        if (index(keywrd,' OLDENS') /= 0) then 
            i = index(jobnam,' ') - 1 
            open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
              'UNFORMATTED', position='asis')  
          rewind iden 
          read (iden, err=30, end=30) pa
          go to 40 
   30     continue 
          write (iw, '(A)') ' Density Restart File missing or corrupt'  
          call mopend ('Density Restart File missing or corrupt') 
          return  
   40     continue 
          if (uhf) then 
            read (iden, end=30, err=30) pb
            pold = pa 
            pbold = pb 
            p = pa + pb
          else 
            pold = pa 
            p = pa*2.D0 
          endif 
        else 
          nscf = 0 
          p = 0.D0 
          pa = 0.D0 
          pb = 0.D0 
          w1 = na1el/(na1el + 1.D-6 + nb1el) 
          w2 = 1.D0 - w1 
          if (w1 < 1.D-6) w1 = 0.5D0 
          if (w2 < 1.D-6) w2 = 0.5D0 
!
!  SLIGHTLY PERTURB THE DENSITY MATRIX IN CASE THE SYSTEM IS
!  TRAPPED IN A S**2 = 0 STATE.
!
          random = 1.0D0 
          glow = glow .or. gnorm<2.D0 .and. gnorm>1.D-9 
          if (.not.glow .and. uhf .and. na1el==nb1el) random = 1.1D0 
          do i = 1, norbs 
            j = (i*(i + 1))/2 
            p(j) = pdiag(i) 
            pa(j) = p(j)*w1*random 
            random = 1.D0/random 
            pb(j) = p(j)*w2*random 
          end do 
          if (uhf) then 
            do i = 1, norbs 
              random = 1.D0/random 
              pb((i*(i+1))/2) = p((i*(i+1))/2)*w2*random 
            end do 
          endif 
          pold(1:mpack) = pa(1:mpack) 
          if (uhf) then 
            pbold(1:mpack) = pb(1:mpack)
          endif 
          do i = 1, norbs 
            pold2(i) = pold((i*(i+1))/2) 
          end do 
        endif 
        halfe = nopen/=nclose .and. Abs(fract - 2.D0) > 1.d-20 .and. Abs(fract) > 1.d-20 
        if (gs) gs = .not.halfe .and. .not.ci 
!
!   DETERMINE THE SELF-CONSISTENCY CRITERION
!
!
! SCFCRT IS MACHINE-PRECISION DEPENDENT
!
        scfcrt = 1.D-4 
!
!  INCREASE PRECISION FOR EVERYTHING EXCEPT NORMAL GROUND-STATE
!  CALCULATIONS
!
        if (index(keywrd,' NLLSQ') + index(keywrd,' SIGMA') + index(keywrd,&
          ' TS')/=0 .or. force) then 
          scfcrt = scfcrt*0.001D0 
        else if (index(keywrd,' PRECISE')/=0 .or. nopen/=nclose) then 
          scfcrt = scfcrt*0.01D0 
        endif 
        if (index(keywrd,' EF') /= 0) scfcrt = scfcrt*0.01D0 
        if (index(keywrd,' POLAR') /= 0) scfcrt = 1.D-11 
        scfcrt = max(scfcrt,1.D-12) 
!
!  THE USER CAN STATE THE SCF CRITERION, IF DESIRED.
!
        i = index(keywrd,' SCFCRT') 
        if (i /= 0) then 
          scfcrt = reada(keywrd,i) 
          write (iw, '(''  SCF CRITERION ='',G14.4)') scfcrt 
          if (scfcrt < 1.D-12) write (iw, &
      '(2/2X,'' THERE IS A RISK OF INFINITE LOOPING WITH'',   '' THE SCFCRT LES&
      &S THAN 1.D-12'')') 
        else 
          if (debug) write (iw, '(''  SCF CRITERION ='',G14.4)') scfcrt 
        endif 
        if (.not.scf1) last = 0 
!
!   END OF INITIALIZATION SECTION.
!
      else if (nscf>0 .and. .not.uhf) then 
!
!   RESET THE DENSITY MATRIX IF MECI HAS FORMED AN EXCITED STATE.  THIS
!   PREVENTS THE SCF GETTING TRAPPED ON AN EXCITED STATE, PARTICULARLY
!   IF THE PULAY CONVERGER IS USED.
!
        pb = pa
        p = 2.D0*pa 
      endif 
!
!   INITIALIZATION OPERATIONS DONE EVERY TIME ITER IS CALLED
!
      makea = .TRUE. 
      makeb = .TRUE. 
      iemin = 0 
      iemax = 0 
      if (irrr /= 5) then 
        if (uhf) then 
          pold(1:mpack) = pa(1:mpack)
          pbold(1:mpack) = pb(1:mpack) 
          do i = 1, norbs 
            pold2(i) = pa((i*(i+1))/2) 
            pbold2(i) = pb((i*(i+1))/2) 
          end do 
        else 
          pold(1:mpack) = p(1:mpack) 
          do i = 1, norbs 
            pold2(i) = p((i*(i+1))/2) 
          end do 
        endif 
      endif 
!
!  TURN OFF SHIFT IF NOT A FULL SCF.
!
      if (.not.fulscf) shift = 0.D0 
      if (newdg) newdg = abs(bshift) < 0.001D0 
      if (last == 1) newdg = .FALSE. 
!
!   SELF-CONSISTENCY CRITERIA: SELCON IS IN KCAL/MOL, PLTEST IS
!   A LESS IMPORTANT TEST TO MAKE SURE THAT THE SELCON TEST IS NOT
!   PASSED 'BY ACCIDENT'
!                              IF GNORM IS LARGE, MAKE SELCON BIGGER
!
      selcon = scfcrt 
!
!  LET SELCON BE DETERMINED BY SCFCRT AND GNORM, BUT IN NO CASE
!  CAN IT BE MORE THAN 100*SELCON OR 0.1
!
      if (gs) selcon = min(min(scfcrt*100.D0,0.1D0),max(scfcrt*gnorm**3*10**(-&
        id*3),scfcrt)) 
      pltest = 0.05D0*sqrt(abs(selcon)) 
!
!  SOMETIMES HEAT GOES SCF BUT DENSITY IS STILL FLUCTUATING IN UHF
!  IN WHICH CASE PAY LESS ATTENTION TO DENSITY MATRIX
!
      if (nalpha/=nbeta .and. uhf) pltest = 0.001D0 
      if (debug) write (iw, '(''  SELCON, PLTEST'',3G16.7)') selcon, pltest 
      if (prt1el) then 
        write (iw, '(2/10X,''ONE-ELECTRON MATRIX AT ENTRANCE TO ITER'')') 
        call vecprt (h, norbs) 
      endif 
      iredy = 1 
  180 continue 
      niter = 0 
      frst = .TRUE. 
      if (camkin) then 
        modea = 1 
        modeb = 1 
      else 
        modea = 0 
        modeb = 0 
      endif 
      bfrst = .TRUE. 
!*********************************************************************
!                                                                    *
!                                                                    *
!                START THE SCF LOOP HERE                             *
!                                                                    *
!                                                                    *
!*********************************************************************
      incitr = .TRUE. 
  250 continue 
      incitr = modea/=3 .and. modeb/=3 
      if (incitr) niter = niter + 1 
      if (timitr) then 
        titer = second(1) 
        write (iw, *) 
        write (iw, '(A,F7.2)') '     TIME FOR ITERATION:', titer - titer0 
        titer0 = titer 
      endif 
      if (niter>itrmax - 10 .and. .not.allcon) then 
!***********************************************************************
!                                                                      *
!                   SWITCH ON ALL CONVERGERS                           *
!                                                                      *
!***********************************************************************
        okpuly = .true.
        camkin = .not.halfe 
          write (iw, &
      '(2/,'' ALL CONVERGERS ARE NOW FORCED ON'',/,                '' SHIFT=10,&
      & PULAY ON, CAMP-KING ON'',/,                          '' AND ITERATION CO&
      &UNTER RESET'',2/)') 
        allcon = .TRUE. 
        bshift = 4.44D0 
        iredy = -4 
        eold = 100.D0 
        newdg = .FALSE. 
        go to 180 
      endif 
!***********************************************************************
!                                                                      *
!                        MAKE THE ALPHA FOCK MATRIX                    *
!                                                                      *
!***********************************************************************
      if (abs(shift)>1.D-10 .and. bshift/=0.D0) then 
        l = 0 
        if (niter > 1) then 
          if (newdg .and. .not.(halfe .or. camkin)) then 
!
!  SHIFT WILL APPLY TO THE VIRTUAL ENERGY LEVELS USED IN THE
!  PSEUDODIAGONALIIZATION. IF DIFF IS -VE, GOOD, THEN LOWER THE
!  HOMO-LUMO GAP BY 0.1EV, OTHERWISE INCREASE IT.
            if (diff > 0.D0) then 
              shift = 1.D0 
!
! IF THE PSEUDODIAGONALIZATION APPROXIMATION -- THAT THE WAVEFUNCTION
! IS ALMOST STABLE -- IS INVALID, TURN OFF NEWDG
              if (diff > 1) newdg = .FALSE. 
            else 
              shift = -0.1D0 
            endif 
          else 
            shift = ten + eigs(ihomo+1) - eigs(ihomo) + shift 
          endif 
          if (diff > 0.D0) then 
            if (shift > 4.D0) shfmax = 4.5D0 
            if (shift > shfmax) shfmax = max(shfmax - 0.5D0,0.D0) 
          endif 
!
!   IF SYSTEM GOES UNSTABLE, LIMIT SHIFT TO THE RANGE -INFINITY - SHFMAX
!   BUT IF SYSTEM IS STABLE, LIMIT SHIFT TO THE RANGE -INFINITY - +20
!
          shift = max(-20.D0,min(shfmax,shift)) 
          if (abs(shift - shfmax) < 1.D-5) shfmax = shfmax + 0.01D0 
!
!  THE CAMP-KING AND PULAY CONVERGES NEED A CONSTANT SHIFT.
!  IF THE SHIFT IS ALLOWED TO VARY, THESE CONVERGERS WILL NOT
!  WORK PROPERLY.
!
          if (okpuly .or. abs(bshift-4.44D0)<1.D-5) then 
            shift = -8.D0 
            if (newdg) shift = 0.D0 
          endif 
          if (uhf) then 
            if (newdg .and. .not.(halfe .or. camkin)) then 
              shiftb = ten - tenold 
            else 
              shiftb = ten + eigs(ihomob+1) - eigs(ihomob) + shiftb 
            endif 
            if (diff > 0.D0) shiftb = min(4.D0,shiftb) 
            shiftb = max(-20.D0,min(shfmax,shiftb)) 
            if (okpuly .or. abs(bshift-4.44D0)<1.D-5) then 
              shiftb = -8.D0 
              if (newdg) shiftb = 0.D0 
            endif 
            eigb(ihomob+1:norbs) = eigb(ihomob+1:norbs) + shiftb 
          endif 
        endif 
        tenold = ten 
        if (pl > plchek) then 
          shftbo = shiftb 
          shfto = shift 
        else 
          shiftb = shftbo 
          shift = shfto 
        endif 
        if (id /= 0) shift = 0.D0 
        eigs(ihomo+1:norbs) = eigs(ihomo+1:norbs) + shift 
        if (id /= 0) shift = -80.D0 
        do i = 1, norbs 
          do j = 1, i 
            l = l + 1 
            f(l) = h(l) + shift*pa(l) + 1.D-16*l 
          end do 
          f(l) = f(l) - shift 
        end do 
      else if (last==0 .and. niter<2 .and. fulscf) then 
!
!  SLIGHTLY PERTURB THE FOCK MATRIX IN CASE THE SYSTEM IS
!  TRAPPED IN A METASTABLE EXCITED ELECTRONIC STATE
!
        random = 0.001D0 
        glow = glow .or. gnorm<2.D0 .and. gnorm>1.D-9 
        if (glow) random = 0.D0 
        do i = 1, mpack
          random = -random 
          f(i) = h(i) + random 
        end do 
      else 
        f = h 
      endif 
  320 continue 
      if (timitr) then 
        t0 = second(1) 
        write (iw, '(A,F7.2)') ' LOAD FOCK MAT. INTEGRAL', t0 - titer0 
      endif 
      if (timitr) call timer ('BEFORE FOCKS') 
      if (method_dorbs) then 
        call fockd2 (f, p, pa, w, lm6, wj, wk, numat, nfirst, nlast, nw) 
      else 
        call fock2 (f, p, pa, w, wj, wk, numat, nfirst, nlast) 
      endif 
      if (uhf) then 
        if (method_dorbs) then 
          call fockd1 (f, p, pa, w) 
        else 
          call fock1 (f, p, pa, pb) 
        endif 
      else 
        if (method_dorbs) then 
          call fockd1 (f, p, pa, w) 
        else 
          call fock1 (f, p, pa, pa) 
        endif 
      endif 
      if (timitr) call timer ('AFTER FOCKS') 
      if (timitr) then 
        t0 = second(1) 
        tf1 = tf1 + t0 - t1 
        write (iw, '(2(A,F7.2))') '  FOCK1:', t0 - t1, 'INTEGRAL:', t0 - titer0 
      endif 
!***********************************************************************
!                                                                      *
!                        MAKE THE BETA FOCK MATRIX                     *
!                                                                      *
!***********************************************************************
      if (uhf) then 
        if (shiftb /= 0.D0) then 
          l = 0 
          do i = 1, norbs 
            if (i > 0) then 
              fb(l+1:i+l) = h(l+1:i+l) + shiftb*pb(l+1:i+l) 
              l = i + l 
            endif 
            fb(l) = fb(l) - shiftb 
          end do 
        else if (rand .and. last==0 .and. niter<2 .and. fulscf) then 
          random = 0.001D0 
          if (glow) random = 0.D0 
          do i = 1, mpack 
            random = -random 
            fb(i) = h(i) + random 
          end do 
        else 
          fb = h
        endif 
        if (method_dorbs) then 
          call fockd2 (fb, p, pb, w, lm6, wj, wk, numat, nfirst, nlast, nw) 
          call fockd1 (fb, p, pb, w) 
        else 
          call fock2 (fb, p, pb, w, wj, wk, numat, nfirst, nlast) 
          call fock1 (fb, p, pb, pa) 
        endif 
      endif 
      if (.not.fulscf) go to 600 
      if (prtfok) then 
        write (iw, 370) niter 
  370   format('   FOCK MATRIX ON ITERATION',i3) 
        call vecprt (f, norbs) 
      endif 
!
!   CODE THE FOLLOWING LINE IN PROPERLY SOMETIME
!   THIS OPERATION IS BELIEVED TO GIVE RISE TO A BETTER FOCK MATRIX
!   THAN THE CONVENTIONAL GUESS.
!
      if (irrr == 0) then 
        do i = 1, norbs 
          f((i*(i+1))/2) = f((i*(i+1))/2)*0.5D0 
        end do 
      endif 
      irrr = 2 
!***********************************************************************
!                                                                      *
!                        CALCULATE THE ENERGY IN KCAL/MOLE             *
!                                                                      *
!***********************************************************************
      if (niter >= itrmax) then 
        if (diff<1.D-3 .and. pl<1.D-4 .and. .not.force) then 
          if (abs(shift) < 1.D-10) write (iw, &
      '('' """""""""""""""UNABLE TO ACHIEVE'',           '' SELF-CONSISTENCE, J&
      &OB CONTINUING'')') 
          incitr = .TRUE. 
          getout = .TRUE. 
          go to 410 
        endif 
        if (minprt) write (iw, 390) 
  390   format(/,/,10x,'"""""""""""""UNABLE TO ','ACHIEVE SELF-CONSISTENCE',/) 
        write (iw, 400) diff, pl 
  400   format(/,/,10x,'DELTAE= ',e12.4,5x,'DELTAP= ',e12.4,/,/,/) 
        iflepo = 9 
        iscf = 2 
        call writmo
        call mopend ('UNABLE TO ACHIEVE SELF-CONSISTENCE') 
        return  
      endif 
      ee = helect(norbs,pa,h,f) 
      if (uhf) then 
        ee = ee + helect(norbs,pb,h,fb) 
      else 
        ee = ee*2.D0 
      endif 
      if (capps) ee = ee + capcor(nat,nfirst,nlast,p,h) 
      if (bshift /= 0.D0) scorr = shift*(nopen - nclose)*enrgy*0.25D0*(fract*(&
        2.D0 - fract)) 
      escf = (ee + enuclr)*enrgy + atheat + scorr 
      getout = .FALSE. 
  410 continue 
      if (incitr) then 
        if (getout) go to 470 
        diff = escf - eold 
        if (diff > 0) then 
          ten = ten - 1.D0 
        else 
          ten = ten*0.975D0 + 0.05D0 
        endif 
        sellim = max(selcon,1.d-15*max(abs(ee),1.D0)) 
!
! SCF TEST:  CHANGE IN HEAT OF FORMATION IN KCAL/MOL SHOULD BE
!            LESS THAN SELLIM.  THE OTHER TESTS ARE SAFETY MEASURES
!
        if (.not.(niter>4 .and. (pl==0.D0 .or. pl<pltest .and. abs(diff)<sellim&
          ) .and. ready)) go to 490 
!***********************************************************************
!                                                                      *
!          SELF-CONSISTENCY TEST, EXIT MODE FROM ITERATIONS            *
!                                                                      *
!***********************************************************************
  470   continue 
        if (abs(shift) < 1.D-10) go to 600 
        shift = 0.D0 
        shiftb = 0.D0 
        f = h 
        makea = .TRUE. 
        makeb = .TRUE. 
        go to 320 
  490   continue 
        if (limscf .and. emin/=0.D0 .and. .not.(ci .or. halfe)) then 
!
!  THE FOLLOWING TESTS ARE INTENDED TO ALLOW A FAST EXIT FROM ITER
!  IF THE RESULT IS 'GOOD ENOUGH' FOR THE CURRENT STEP IN THE GEOMETRY
!  OPTIMIZATION
!
          if (escf < emin) then 
!
!  THE ENERGY IS LOWER THAN THE PREVIOUS MINIMUM.  NOW CHECK THAT
!  IT IS CONSISTENTLY LOWER.
!
            iemax = 0 
            iemin = min(5,iemin + 1) 
            escf0(:iemin-1) = escf0(2:iemin) 
            escf0(iemin) = escf 
!
!  IS THE DIFFERENCE IN ENERGY BETWEEN TWO ITERATIONS LESS THAN 5%
!  OF THE ENERGY GAIN FOR THIS GEOMETRY RELATIVE TO THE PREVIOUS
!  MINIMUM.
!
            if (iemin > 3) then 
              do i = 2, iemin 
                if (abs(escf0(i)-escf0(i-1)) > 0.05D0*(emin - escf)) go to 540 
              end do 
!
! IS GOOD ENOUGH -- RAPID EXIT
!
              if (debug) write (iw, *) &
                ' RAPID EXIT BECAUSE ENERGY IS CONSISTENTLY LOWER' 
              incitr = .TRUE. 
              getout = .TRUE. 
              go to 410 
            endif 
          else 
!
!  THE ENERGY HAS RISEN ABOVE THAT OF THE PREVIOUS MINIMUM.
!  WE NEED TO CHECK WHETHER THIS IS A FLUKE OR IS THIS REALLY
!  A BAD GEOMETRY.
!
            iemin = 0 
            iemax = min(5,iemax + 1) 
            escf0(:iemax-1) = escf0(2:iemax) 
            escf0(iemax) = escf 
!
!  IS THE DIFFERENCE IN ENERGY BETWEEN TWO ITERATIONS LESS THAN 5%
!  OF THE ENERGY LOST FOR THIS GEOMETRY RELATIVE TO THE PREVIOUS
!  MINIMUM.
!
            if (iemax > 3) then 
              do i = 2, iemax 
                if (abs(escf0(i)-escf0(i-1)) > 0.05D0*(escf - emin)) go to 540 
              end do 
!
! IS GOOD ENOUGH -- RAPID EXIT
!
              if (debug) write (iw, *) &
                ' RAPID EXIT BECAUSE ENERGY IS CONSISTENTLY HIGHER' 
              incitr = .TRUE. 
              getout = .TRUE. 
              go to 410 
            endif 
          endif 
        endif 
  540   continue 
        ready = iredy>0 .and. (abs(diff)<sellim*10.D0 .or. pl==0.D0) 
        iredy = iredy + 1 
      endif 
      if (prtpl .or. debug .and. niter>itrmax-20) then 
        if (abs(escf) > 99999.D0) escf = sign(99999.D0,escf) 
        if (abs(diff) > 9999.D0) diff = 0.D0 
        if (incitr) then 
          write (iw, &
      '('' ITERATION'',I3,'' PLS='',2E10.3,'' ENERGY  '',F14.7,'' DELTAE'',F13.&
      &7)') niter, pl, plb, escf, diff 
          endfile (iw) 
          backspace (iw) 
        endif 
      endif 
      if (incitr) eold = escf 
!***********************************************************************
!                                                                      *
!                        INVOKE THE CAMP-KING CONVERGER                *
!                                                                      *
!***********************************************************************
      if (niter>2 .and. camkin .and. makea) &
      call interp (na1el, norbs - na1el, modea, escf/enrgy, f, c) 
      makeb = .FALSE. 
      if (modea /= 3) then 
        makeb = .TRUE. 
        if (timitr) then 
          t0 = second(1) 
          write (iw, '(2(A,F7.2))') ' ADJUST DAMPER  INTEGRAL', t0 - titer0 
        endif 
        if (debug) then 
          write (iw, *) ' Diagonal of FOCK Matrix' 
          write (iw, '(8F10.6)') (f((i*(i+1))/2),i=1,norbs) 
        endif 
        if (newdg) then 
!***********************************************************************
!                                                                      *
!                        INVOKE PULAY'S CONVERGER                      *
!                                                                      *
!***********************************************************************
          if (okpuly .and. makea .and. iredy>1) call pulay (f, pa, norbs, pold&
            , pold2, pold3, jalp, ialp, 6*mpack, frst, pl) 
!***********************************************************************
!                                                                      *
!           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
! WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
!                                                                      *
!***********************************************************************
          if (halfe .or. camkin) then 
            call rsp (f, norbs, norbs, eigs, c) 
          else 
            if (timitr) call timer ('BEFORE DIAG') 
            call diag (f, c, na1el, eigs, norbs, norbs) 
            if (timitr) call timer ('AFTER DIAG') 
          endif 
        else 
          if (timitr) call timer ('BEFORE RSP') 
          call rsp (f, norbs, norbs, eigs, c) 
          if (timitr) call timer ('AFTER RSP') 
          if (timitr) then 
            t1 = second(1) 
            write (iw, '(2(A,F7.2))') '  RSP:', t1 - t0, ' INTEGRAL', t1 - &
              titer0 
          endif 
        endif 
        j = 1 
        if (prtvec) then 
          j = 1 
          if (uhf) j = 2 
          write (iw, &
      '(2/10X,A,                                            '' EIGENVECTORS AND&
      & EIGENVALUES ON ITERATION'',I3)') abprt(j), niter 
          call matout (c, eigs, norbs, norbs, norbs) 
        else 
          if (prteig) write (iw, 550) abprt(j), niter, (eigs(i),i=1,norbs) 
        endif 
  550   format(10x,a,'  EIGENVALUES ON ITERATION',i3,/,10(6g13.6,/)) 
      endif 
      if (ifill /= 0) call swap (c, norbs, norbs, na2el, ifill) 
!***********************************************************************
!                                                                      *
!            CALCULATE THE ALPHA OR RHF DENSITY MATRIX                 *
!                                                                      *
!***********************************************************************
      if (uhf) then 
        call densit (c, norbs, norbs, na2el, na1el, fract, pa, 1) 
        if (modea/=3 .and. .not.(newdg .and. okpuly)) call cnvg (pa, pold, &
          pold2,  niter, pl) 
      else 
        if (timitr) call timer ('BEFORE DENSIT') 
        call densit (c, norbs, norbs, na2el, na1el, fract, p, 1) 
        if (timitr) call timer ('AFTER DENSIT') 
        if (modea/=3 .and. .not.(newdg .and. okpuly)) then 
          if (timitr) call timer ('BEFORE CNVG') 
          call cnvg (p, pold, pold2,  niter, pl) 
          if (timitr) call timer ('AFTER CNVG') 
        endif 
      endif 
!***********************************************************************
!                                                                      *
!                       UHF-SPECIFIC CODE                              *
!                                                                      *
!***********************************************************************
      if (uhf) then 
!***********************************************************************
!                                                                      *
!                        INVOKE THE CAMP-KING CONVERGER                *
!                                                                      *
!***********************************************************************
        if (niter>2 .and. camkin .and. makeb) &
        call interp (nb1el, norbs - nb1el, modeb, escf/enrgy, fb, cb) 
        makea = .FALSE. 
        if (modeb /= 3) then 
          makea = .TRUE. 
          if (newdg) then 
!***********************************************************************
!                                                                      *
!                        INVOKE PULAY'S CONVERGER                      *
!                                                                      *
!***********************************************************************
            if (okpuly .and. makeb .and. iredy>1) call pulay (fb, pb, norbs, &
              pbold, pbold2, pbold3, jbet, ibet, 6*mpack, bfrst, plb) 
!***********************************************************************
!                                                                      *
!           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
! WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
!                                                                      *
!***********************************************************************
            if (halfe .or. camkin) then 
              call rsp (fb, norbs, norbs, eigb, cb) 
            else 
              call diag (fb, cb, nb1el, eigb, norbs, norbs) 
            endif 
          else 
            call rsp (fb, norbs, norbs, eigb, cb) 
          endif 
          if (prtvec) then 
            write (iw, &
      '(2/10X,A,'' EIGENVECTORS AND EIGENVALUES ON '',   ''ITERATION'',I3)') &
              abprt(3), niter 
            call matout (cb, eigb, norbs, norbs, norbs) 
          else 
            if (prteig) write (iw, 550) abprt(3), niter, (eigb(i),i=1,norbs) 
          endif 
        endif 
!***********************************************************************
!                                                                      *
!                CALCULATE THE BETA DENSITY MATRIX                     *
!                                                                      *
!***********************************************************************
        call densit (cb, norbs, norbs, nb2el, nb1el, fract, pb, 1) 
        if (.not.(newdg .and. okpuly)) call cnvg (pb, pbold, pbold2,  &
          niter, plb) 
      endif 
!***********************************************************************
!                                                                      *
!                   CALCULATE THE TOTAL DENSITY MATRIX                 *
!                                                                      *
!***********************************************************************
      if (uhf) then 
        p = pa + pb 
      else 
        pa = p*0.5D0 
        pb = pa 
      endif 
      if (debug) then 
        call chrge (p, q) 
        write (iw, *) ' CHARGES' 
        write (iw, '(8F10.7)') (q(i),i=1,numat) 
      endif 
      if (prtden) then 
        write (iw, '('' DENSITY MATRIX ON ITERATION'',I4)') niter 
        call vecprt (p, norbs) 
      endif 
      oknewd = pl<sellim .or. oknewd 
      newdg = pl<trans .and. oknewd .or. newdg 
      if (pl < trans*0.3333D0) oknewd = .TRUE. 
      go to 250 
!*********************************************************************
!                                                                    *
!                                                                    *
!                      END THE SCF LOOP HERE                         *
!                NOW CALCULATE THE ELECTRONIC ENERGY                 *
!                                                                    *
!                                                                    *
!*********************************************************************
!          SELF-CONSISTENCE ACHEIVED.
!
  600 continue 
      ee = helect(norbs,pa,h,f) 
      if (uhf) then 
        ee = ee + helect(norbs,pb,h,fb) 
      else 
        ee = ee*2.D0 + shift*(nopen - nclose)*enrgy*0.25D0*(fract*(2.D0 - fract&
          )) 
      endif 
      if (capps) ee = ee + capcor(nat,nfirst,nlast,p,h)  
      if (nscf==0 .or. last==1 .or. ci .or. halfe) then 
!
!  PUT F AND FB INTO POLD IN ORDER TO NOT DESTROY F AND FB
!  AND DO EXACT DIAGONALISATIONS
        pold(1:mpack) = f(1:mpack) 
        call rsp (pold, norbs, norbs, eigs, c) 
        if (uhf) then 
          pold(1:mpack) = fb(1:mpack) 
          call rsp (pold, norbs, norbs, eigb, cb) 
          pold(1:mpack) = pa(1:mpack) 
        else 
          pold(1:mpack) = p(1:mpack) 
        endif 
        if (ci .or. halfe) then 
          sum = meci() 
          if (moperr) return  
          ee = ee + sum 
          if (prtpl) then 
            escf = (ee + enuclr)*enrgy + atheat 
            write (iw, '(27X,''AFTER MECI, ENERGY  '',F14.7)') escf 
          endif 
        endif 
      endif 
      nscf = nscf + 1 
      if (debug) write (iw, '('' NO. OF ITERATIONS ='',I6)') niter 
!            IF(FORCE)  SCFCRT=1.D-5
      if (allcon .and. abs(bshift-4.44D0)<1.D-7) then 
        camkin = .FALSE. 
        allcon = .FALSE. 
        newdg = .FALSE. 
        bshift = -10.D0 
        okpuly = .FALSE. 
      endif 
      shift = 1.D0 
      if (emin == 0.D0) then 
        emin = escf 
      else 
        emin = min(emin,escf) 
      endif 
      return  
      end subroutine iter
