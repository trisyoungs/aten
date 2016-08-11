      subroutine ef(xparam, nvar, funct) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, ONLY: numat,  ndep, numcal, tdump, iflepo, &
      & tleft, last, time0, keywrd, moperr
      use permanent_arrays, only : loc, geo, grad
      USE ef_C, ONLY: oldf, d, ddd, rmin, rmax, xlamd, xlamd0, skal, mode, &
        nstep, negreq, iprnt, oldu, pmat, uc, hessc, &
        & hess, bmat, u, oldhss
      USE chanel_C, only : iw, ilog
!-----------------------------------------------
      use efstr_I 
      use second_I 
      use compfg_I 
      use dot_I 
      use gethes_I 
      use updhes_I 
      use geout_I 
      use prttim_I 
      use prjfc_I 
      use rsp_I 
      use prthes_I 
      use formd_I 
      use symtry_I 
      use efsav_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: nvar 
      real(double)  :: xparam(*) 
      real(double)  :: funct  
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(9) :: ipow 
      integer :: ihess, icalcn, ldump, ntime, iloop, mxstep, igthes, iupd, &
        ireclc, i, nflush, itime, itry1, ij, neg, k, l, imode, ittest, &
        instep 
      real(double), dimension(nvar) :: eigval, tvec, svec, fx, oldfx, oldeig&
        , ooldf 
      real(double) :: dmax, tol2, time1, time2, zero, one, two, three, four, &
        tmone, tmtwo, tmsix, pt5, pt75, demin, gmin, odd, odmax, oolde, ddmax, &
        dmin, t0, absmin, osmin, rmx, tstep, tprt, olde, oldgn, depre, xtmp, ss&
        , frodo, gntest, deact, ratio, tt0, gnfina
      logical :: scf1, log, let, lupd, lts, lrjk, lorjk, rrscal, donr, gnmin 
      character :: txt 

      save scf1, log, let, lupd, ihess, icalcn, ldump, ntime, iloop, mxstep, &
        igthes, iupd, ireclc, dmax, tol2, time1, time2, zero, one, two, three, &
        four, tmone, tmtwo, tmsix, pt5, pt75, demin, gmin 
!-----------------------------------------------
      data icalcn, zero, one, two/ 0, 0.D0, 1.D0, 2.D0/  
       tmone = 1.0D-1  
       tmtwo = 1.0D-2  
       tmsix = 1.0D-06  
       three = 3.0D0  
       four = 4.0D0  
       pt5 =0.50D0  
       pt75 = 0.75D0  
       demin = 2.0D-2  
       gmin = 5.0D0 
       igthes = 0 
       iupd = 0 
       ihess = 0 
       nstep = 0 
       if (icalcn /= numcal) then 
        eigval(1) = 0.d0
        fx(1) = 0.d0
        mode = 0
        odmax = 0.d0
        odd = 0.d0
        oolde = 0.d0
        rmin = 0.d0
        rmax = 0.d0
        call setup_ef(1)
        call efstr (xparam, funct, ihess, ntime, iloop, igthes, mxstep, ireclc&
          , iupd, dmax, ddmax, dmin, tol2, time1, time2, nvar, scf1, lupd, &
          ldump, log, rrscal, donr, gnmin, hess, pmat, bmat) 
        if (moperr) return  
        let = index(keywrd,' LET') /= 0 
      endif 
      t0 = second(1) 
      grad(:nvar) = 0.D0 
      call compfg (xparam, .TRUE., funct, .TRUE., grad, .TRUE.) 
      if (moperr) return  
      nflush = 1 
      absmin = 1.D9 
      lts = .FALSE. 
      if (negreq == 1) lts = .TRUE. 
      lorjk = .FALSE. 
!     OSMIN IS SMALLEST STEP FOR WHICH A TS-MODE OVERLAP LESS THAN OMIN
!     WILL BE REJECTED. FOR UPDATED HESSIANS THERE IS LITTLE HOPE OF
!     BETTER OVERLAP BY REDUCING THE STEP BELOW 0.005. FOR EXACT HESSIAN
!     THE OVERLAP SHOULD GO TOWARD ONE AS THE STEP BECOME SMALLER, BUT
!     DON'T ALLOW VERY SMALL STEPS
      osmin = 0.005D0 
      if (ireclc == 1) osmin = 0.001D0 
      if (scf1) then 
        gnfina = sqrt(dot(grad,grad,nvar)) 
        iflepo = 1 
        return  
      endif 
!     CHECK THAT GEOMETRY IS NOT ALREADY OPTIMIZED
      rmx = sqrt(dot(grad,grad,nvar)) 
      if (rmx < tol2) then 
        iflepo = 2 
        last = 1 
        icalcn = numcal 
        return  
      endif 
!     GET INITIAL HESSIAN. IF ILOOP IS .LE.0 THIS IS AN OPTIMIZATION
!     RESTART AND HESSIAN SHOULD ALREADY BE AVAILABLE
      if ((icalcn/=numcal .or. abs(eigval(1))<1.D-5) .and. iloop>0) then 
        call gethes (xparam, igthes, nvar, iloop, hess, pmat, bmat) 
        if (moperr) return  
      endif 
      icalcn = numcal 
!     START OF MAIN LOOP
!     WE NOW HAVE GRADIENTS AND A HESSIAN. IF THIS IS THE FIRST
!     TIME THROUGH DON'T UPDATE THE HESSIAN. FOR LATER LOOPS ALSO
!     CHECK IF WE NEED TO RECALCULATE THE HESSIAN
      iflepo = 0 
      itime = 0 
   20 continue 
      oldfx(:nvar) = fx(:nvar) 
      ooldf(:nvar) = oldf(:nvar) 
      oldeig(:nvar) = eigval(:nvar) 
      oldhss(:nvar,:nvar) = hess(:nvar,:nvar) 
      oldu(:nvar,:nvar) = u(:nvar,:nvar) 
      if (ihess>=ireclc .and. iflepo/=15) then 
        iloop = 1 
        ihess = 0 
        if (igthes /= 3) igthes = 1 
        call gethes (xparam, igthes, nvar, iloop, hess, pmat, bmat) 
        if (moperr) return  
      endif 
      if (ihess > 0) call updhes (svec, tvec, nvar, iupd, hess) 
      if (iprnt >= 2) call geout (iw) 
      if (iprnt >= 2) then 
        write (iw, '('' XPARAM '')') 
        write (iw, '(5(2I3,F10.4))') (loc(1,i),loc(2,i),xparam(i),i=1,nvar) 
        write (iw, '('' GRADIENTS'')') 
        write (iw, '(3X,8F9.3)') (grad(i),i=1,nvar) 
      endif 
!
!        PRINT RESULTS IN CYCLE
      gnfina = sqrt(dot(grad,grad,nvar)) 
      time2 = second(2) 
      if (itime == 0) time1 = t0 
      tstep = time2 - time1 
      tstep = dmax1(zero,tstep) 
      tleft = tleft - tstep 
      if (tleft < 0.0D0) tleft = -0.1D0 
      time1 = time2 
      itime = itime + 1 
      if (tleft >= tstep*two) then 
        call prttim (tleft, tprt, txt) 
        if (ldump == 0) then 
          write (iw, 50) nstep + 1, min(tstep,9999.99D0), tprt, txt, min(gnfina&
            ,999999.999D0), funct 
          if (log) write (ilog, 50) nstep + 1, min(tstep,9999.99D0), tprt, txt&
            , min(gnfina,999999.999D0), funct 
   50     format(' CYCLE:',i4,' TIME:',f8.3,' TIME LEFT:',f6.2,a1,'  GRAD.:',&
            f10.3,' HEAT:',g13.7) 
          if (nflush /= 0) then 
            if (mod(nstep + 1,nflush) == 0) then 
              endfile (iw) 
              backspace (iw) 
              if (log) then 
                endfile (ilog) 
                backspace (ilog) 
              endif 
            endif 
          endif 
        else 
          write (iw, 60) tprt, txt, min(gnfina,999999.999D0), funct 
          if (log) write (ilog, 60) tprt, txt, min(gnfina,999999.999D0), funct 
   60     format(' RESTART FILE WRITTEN,    TIME LEFT:',f6.2,a1,'  GRAD.:',&
            f10.3,' HEAT:',g13.7) 
          if (nflush /= 0) then 
            if (mod(nstep + 1,nflush) == 0) then 
              endfile (iw) 
              backspace (iw) 
              if (log) then 
                endfile (ilog) 
                backspace (ilog) 
              endif 
            endif 
          endif 
        endif 
        ihess = ihess + 1 
        nstep = nstep + 1 
!
!        TEST FOR CONVERGENCE
!
        rmx = sqrt(dot(grad,grad,nvar)) 
        if (rmx < tol2) go to 310 
        if (mode==0 .and. nstep>5) then 
          if (absmin - funct < 1.D-7) then 
            if (funct - absmin > 1.D0) itry1 = 0 
            itry1 = itry1 + 1 
            if (itry1 > 10) then 
              write (iw, &
      '(2/,'' HEAT OF FORMATION IS '',                ''ESSENTIALLY STATIONARY'&
      &')') 
              go to 330 
            endif 
          else 
            itry1 = 0 
            absmin = funct 
          endif 
        endif 
        olde = funct 
        oldgn = rmx 
        oldf(:nvar) = grad(:nvar) 
!
!     IF THE OPTIMIZATION IS IN CARTESIAN COORDINATES, WE SHOULD REMOVE
!     TRANSLATION AND ROTATION MODES. POSSIBLE PROBLEM IF RUN IS IN
!     INTERNAL BUT WITH EXACTLY 3*NATOMS VARIABLE (I.E. DUMMY ATOMS
!     ARE ALSO OPTIMIZED).
        if (nvar == 3*numat) then 
          if (nstep == 1) write (iw, 80) 
   80     format(1x,'WARNING! EXACTLY 3N VARIABLES. ',&
            'EF ASSUMES THIS IS A CARTESIAN OPTIMIZATION.',/,1x,&
            'IF THE OPTIMIZATION IS IN INTERNAL COORDINATES, ',&
            'EF WILL NOT WORK') 
          call prjfc (hess, xparam, nvar, u) 
          if (moperr) return  
        endif 
        ij = 0 
        do i = 1, nvar 
          if (i > 0) then 
            hessc(ij+1:i+ij) = hess(:i,i) 
            ij = i + ij 
          endif 
        end do 
        call rsp (hessc, nvar, nvar, eigval, uc) 
        do i = 1, nvar 
          if (abs(eigval(i)) >= tmsix) cycle  
          eigval(i) = zero 
        end do 
        ij = nvar**2 
        do i = nvar, 1, -1 
          u(nvar:1:(-1),i) = uc(ij:ij+1-nvar:(-1)) 
          ij = ij - nvar 
        end do 
        if (iprnt >= 3) call prthes (eigval, nvar, hess, u) 
        if (mxstep == 0) then 
          nstep = 0 
          go to 340 
        endif 
        neg = 0 
        neg = neg + count(eigval(:nvar)<zero) 
        if (iprnt >= 1) write (iw, 130) neg, (eigval(i),i=1,neg) 
  130   format(/,10x,'HESSIAN HAS',i3,' NEGATIVE EIGENVALUE(S)',6f7.1,/) 
!     IF AN EIGENVALUE HAS BEEN ZERO OUT IT IS PROBABLY ONE OF THE T,R
!     MODES IN A CARTESIAN OPTIMIZATION. ZERO CORRESPONDING FX TO
!     ALLOW FORMATION OF STEP WITHOUT THESE CONTRIBUTIONS. A SAFER
!     CRITERION FOR DECIDING WHETHER THIS ACTUALLY IS A CARTESIAN
!     OPTIMIZATION SHOULD BE PUT IN SOME DAY...
        do i = 1, nvar 
          fx(i) = dot(u(1,i),grad,nvar) 
          if (abs(eigval(i)) > 1.d-20) cycle  
          fx(i) = zero 
        end do 
!     FORM GEOMETRY STEP D
  150   continue 
        call formd (eigval, fx, nvar, dmax, osmin, lts, lorjk, rrscal, donr, u) 
        if (moperr) return  
!     IF LORJK IS TRUE, THEN TS MODE OVERLAP IS LESS THAN OMIN, REJECT
!     PREVIOUS STEP
        if (lorjk) then 
          if (iprnt >= 1) write (iw, *) '      NOW UNDOING PREVIOUS STEP' 
          dmax = odmax 
          ddd = odd 
          olde = oolde 
          fx(:nvar) = oldfx(:nvar) 
          oldf(:nvar) = ooldf(:nvar) 
          eigval(:nvar) = oldeig(:nvar) 
          hess(:nvar,:nvar) = oldhss(:nvar,:nvar) 
          u(:nvar,:nvar) = oldu(:nvar,:nvar) 
          do i = 1, nvar 
            xparam(i) = xparam(i) - d(i) 
            k = loc(1,i) 
            l = loc(2,i) 
            geo(l,k) = xparam(i) 
          end do 
          if (ndep /= 0) call symtry 
          dmax = min(dmax,ddd)/two 
          odmax = dmax 
          odd = ddd 
          nstep = nstep - 1 
          if (dmax < dmin) go to 290 
          if (iprnt >= 1) write (iw, *) &
            '      FINISH UNDOING, NOW GOING FOR NEW STEP' 
          go to 150 
        endif 
!
!  FORM NEW TRIAL XPARAM AND GEO
!
        do i = 1, nvar 
          xparam(i) = xparam(i) + d(i) 
          k = loc(1,i) 
          l = loc(2,i) 
          geo(l,k) = xparam(i) 
        end do 
        if (ndep /= 0) call symtry 
!
!     COMPARE PREDICTED E-CHANGE WITH ACTUAL
!
        depre = zero 
        imode = 1 
        if (mode /= 0) imode = mode 
        do i = 1, nvar 
          xtmp = xlamd 
          if (lts .and. i==imode) xtmp = xlamd0 
          if (abs(xtmp - eigval(i)) < tmtwo) then 
            ss = zero 
          else 
            ss = skal*fx(i)/(xtmp - eigval(i)) 
          endif 
          frodo = ss*fx(i) + pt5*ss*ss*eigval(i) 
!        WRITE(IW,88)I,FX(I),SS,XTMP,EIGVAL(I),FRODO
          depre = depre + frodo 
        end do 
!
!     GET GRADIENT FOR NEW GEOMETRY
!
        grad(:nvar) = 0.D0 
!
!     GET GRADIENT FOR NEW GEOMETRY
!
        call compfg (xparam, .TRUE., funct, .TRUE., grad, .TRUE.) 
        if (moperr) return  
        if (gnmin) gntest = sqrt(dot(grad,grad,nvar)) 
        deact = funct - olde 
        ratio = deact/depre 
        if (iprnt >= 1) write (iw, 220) deact, depre, ratio 
  220   format(5x,'ACTUAL, PREDICTED ENERGY CHANGE, RATIO',2f10.3,f10.5) 
        lrjk = .FALSE. 
!     WITH THE DEFAULT RMIN = 0, THIS IS EQUIVALENT TO NOT ALLOWING
!     THE ENERGY TO RAISE, BUT THE USER HAS CONTROL OVER RMIN
        if (.not.lts .and. ratio<rmin) then 
          if (iprnt >= 1) write (iw, 230) min(dmax,ddd)/two 
  230     format(1x,'RATIO BELOW RMIN,',' REJECTING STEP, REDUCING DMAX TO',f&
            7.4) 
          lrjk = .TRUE. 
        endif 
        if (gnmin .and. gntest>oldgn) then 
          if (iprnt >= 1) write (iw, 240) gntest, min(dmax,ddd)/two 
  240     format(1x,'GRADIENT NORM RAISES ',f10.4,' REJECTING STEP, ',&
            'REDUCING DMAX TO',f7.4) 
          lrjk = .TRUE. 
        endif 
        if (lts .and. (ratio<rmin .or. ratio>rmax) .and. (abs(depre)>demin&
           .or. abs(deact)>demin)) then 
          if (iprnt >= 1) write (iw, 250) min(dmax,ddd)/two 
  250     format(1x,'UNACCEPTABLE RATIO,',' REJECTING STEP, REDUCING DMAX TO',f&
            7.4) 
          lrjk = .TRUE. 
        endif 
!
!   BYPASS ALL TESTS
!
        if (let) lrjk = .FALSE. 
!
!   REMOVE BEFORE RELEASE
!
        if (lrjk) then 
          do i = 1, nvar 
            xparam(i) = xparam(i) - d(i) 
            k = loc(1,i) 
            l = loc(2,i) 
            geo(l,k) = xparam(i) 
          end do 
          if (ndep /= 0) call symtry 
          dmax = min(dmax,ddd)/two 
          if (dmax < dmin) go to 290 
          go to 150 
        endif 
        if (iprnt >= 1) write (iw, 270) ddd 
  270   format(5x,'STEPSIZE USED IS',f9.5) 
        if (iprnt >= 2) then 
          write (iw, '('' CALCULATED STEP'')') 
          write (iw, '(3X,8F9.5)') (d(i),i=1,nvar) 
        endif 
!
!     POSSIBLE USE DYNAMICAL TRUST RADIUS
        odmax = dmax 
        odd = ddd 
        oolde = olde 
        if (lupd .and. (rmx>gmin .or. abs(depre)>demin .or. abs(deact)>demin)) &
          then 
!   FLETCHER RECOMMEND DMAX=DMAX/4 AND DMAX=DMAX*2
!   THESE ARE A LITTLE MORE CONSERVATIVE SINCE HESSIAN IS BEING UPDATED
!   DON'T REDUCE TRUST RADIUS DUE TO RATIO FOR MIN SEARCHES
          if (lts .and. ratio<=tmone .or. ratio>=three) dmax = min(dmax,ddd)/two 
          if (lts .and. ratio>=pt75 .and. ratio<=four/three .and. ddd>dmax-tmsix&
            ) dmax = dmax*sqrt(two) 
!     ALLOW WIDER LIMITS FOR INCREASING TRUST RADIUS FOR MIN SEARCHES
          if (.not.lts .and. ratio>=pt5 .and. ddd>dmax-tmsix) dmax = dmax*sqrt(&
            two) 
!     BE BRAVE IF  0.90 < RATIO < 1.10 ...
          if (abs(ratio - one) < tmone) dmax = dmax*sqrt(two) 
          dmax = max(dmax,dmin - tmsix) 
          dmax = min(dmax,ddmax) 
        endif 
!     ALLOW STEPSIZE UP TO 0.1 IN THE END-GAME WHERE CHANGES ARE LESS
!     THAN DEMIN AND GRADIENT IS LESS THAN GMIN
        if (lupd .and. rmx<gmin .and. abs(depre)<demin .and. abs(deact)<demin) &
          dmax = max(dmax,tmone) 
        if (iprnt >= 1) write (iw, 280) dmax 
  280   format(5x,'CURRENT TRUST RADIUS = ',f7.5) 
  290   continue 
        if (dmax < dmin) then 
          write (iw, 300) dmin 
  300     format(/,5x,'TRUST RADIUS NOW LESS THAN ',f7.5,&
            ' OPTIMIZATION TERMINATING',/,5x,&
            ' GEOMETRY MAY NOT BE COMPLETELY OPTIMIZED',/,5x,&
            ' (TO CONTINUE, ADD ''LET DDMIN=0.0'' TO THE KEYWORD LINE)') 
          go to 330 
        endif 
!     CHECK STEPS AND ENOUGH TIME FOR ANOTHER PASS
        if (nstep >= mxstep) go to 340 
!     IN USER UNFRIENDLY ENVIROMENT, SAVE RESULTS EVERY 1 CPU HRS
        ittest = int((time2 - time0)/tdump) 
        if (ittest > ntime) then 
          ldump = 1 
          ntime = max(ittest,ntime + 1) 
          ipow(9) = 2 
          tt0 = second(1) - time0 
          instep = -nstep 
          call efsav (tt0, hess, funct, grad, xparam, pmat, instep, nstep, bmat&
            , ipow) 
          if (moperr) return  
        else 
          ldump = 0 
        endif 
!     RETURN FOR ANOTHER CYCLE
        go to 20 
!
!     ****** OPTIMIZATION TERMINATION ******
!
  310   continue 
        write (iw, 320) rmx, tol2 
  320   format(/,5x,'RMS GRADIENT =',f9.5,'  IS LESS THAN CUTOFF =',f9.5,/,/) 
  330   continue 
        iflepo = 15 
        last = 1 
!     SAVE HESSIAN ON FILE 9
        ipow(9) = 2 
        tt0 = second(1) - time0 
        instep = -nstep 
        call efsav (tt0, hess, funct, grad, xparam, pmat, instep, nstep, bmat,ipow) 
        if (moperr) return  
!     CALL COMPFG TO CALCULATE ENERGY FOR FIXING MO-VECTOR BUG
        call compfg (xparam, .TRUE., funct, .TRUE., grad, .FALSE.) 
        return  
      endif 
  340 continue 
      if (tleft < tstep*two) then 
        write (iw, 350) 
  350   format(/,5x,'NOT ENOUGH TIME FOR ANOTHER CYCLE') 
      endif 
      if (nstep >= mxstep) then 
        write (iw, 360) 
  360   format(/,5x,'EXCESS NUMBER OF OPTIMIZATION CYCLES') 
      endif 
      ipow(9) = 1 
      tt0 = second(1) - time0 
      instep = -nstep 
      call efsav (tt0, hess, funct, grad, xparam, pmat, instep, nstep, bmat,ipow) 
      if (moperr) return  
      iflepo = -1 
      return  
      end subroutine ef


      subroutine formd(eigval, fx, nvar, dmax, osmin, ts, lorjk, rrscal, donr, &
        u) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE ef_C, ONLY: d, ddd, xlamd, xlamd0, skal, mode, iprnt 
      USE chanel_C, only : iw
!     THIS VERSION FORMS GEOMETRY STEP BY EITHER PURE NR, P-RFO OR QA
!     ALGORITHM, UNDER THE CONDITION THAT THE STEPLENGTH IS LESS THAN
!     DMAX
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use overlp_I 
      use mopend_I 
      use dot_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: nvar 
      real(double)  :: dmax 
      real(double)  :: osmin 
      logical , intent(in) :: ts 
      logical  :: lorjk 
      logical , intent(in) :: rrscal 
      logical , intent(in) :: donr 
      real(double) , intent(in) :: eigval(nvar) 
      real(double) , intent(in) :: fx(nvar) 
      real(double)  :: u(nvar,nvar) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: it, jt, newmod, i, ncnt
      real(double) :: lamda, lamda0, zero, half, toll, step, big, ten, one, &
        four, tmtwo, tmsix, sfix, eps, eigit, eone, ssmin, ssmax, sstoll, d2max&
        , sstep, bl, bu, fl, fu, xlamda, fm, temp 
      logical :: rscal, frodo1, frodo2 

      save zero, half, toll, step, big, ten, one, four, tmtwo, tmsix, sfix, eps 
!-----------------------------------------------
      data zero/ 0.0D0/  
      data half/ 0.5D0/  
      data toll/ 1.0D-8/  
      data step/ 5.0D-02/  
      data ten/ 1.0D+1/  
      data one/ 1.0D+0/  
      data big/ 1.0D+3/  
      data four/ 4.0D+00/  
      data tmtwo/ 1.0D-2/  
      data tmsix/ 1.0D-06/  
      data sfix/ 1.0D+01/  
      data eps/ 1.0D-12/  
!
      skal = one 
      rscal = rrscal 
      it = 0 
      jt = 1 
      if (ts) then 
        if (mode /= 0) then 
          call overlp (dmax, osmin, newmod, nvar, lorjk, u) 
          if (lorjk) return  
!
!  ON RETURN FROM OVERLP, NEWMOD IS THE TS MODE
!
          if (newmod/=mode .and. iprnt>=1) write (iw, 10) mode, newmod 
   10     format(5x,'WARNING! MODE SWITCHING. WAS FOLLOWING MODE ',i3,&
            ' NOW FOLLOWING MODE ',i3) 
          mode = newmod 
          it = mode 
        else 
          it = 1 
        endif 
        eigit = eigval(it) 
        if (abs(eigval(it)) < 1.d-20) then 
          write (iw, *) ' TRYING TO FOLLOW MODE WITH EIGENVALUE', &
            ' EXACT = 0. THIS WILL NOT WORK.....' 
        call mopend (&
       'TRYING TO FOLLOW MODE WITH EIGENVALUE EXACT = 0. THIS WILL NOT WORK.....') 
          return  
        endif 
        if (iprnt >= 1) then 
          write (iw, 20) it, eigit 
          write (iw, 30) (u(i,it),i=1,nvar) 
   20     format(/,5x,'TS MODE IS NUMBER',i3,' WITH EIGENVALUE',f9.1,/,5x,&
            'AND COMPONENTS',/) 
   30     format(5x,8f9.4) 
        endif 
      endif 
!     JT SHOULD BE LOWEST MODE WHICH IS NOT THE TS-MODE AND NOT T, R MOD
!     IN CARTESIAN COORDINATES. JT SHOULD BE ONE OF THE FIRST 8 MODES,
!     SEARCH ALL TO ALLOW FOR ACCIDENTAL EIGENVALUES = 0.
      do i = 1, nvar 
        if (i==it .or. abs(eigval(i)) < 1.d-20) cycle  
        jt = i 
        exit  
      end do 
      eone = eigval(jt) 
      ssmin = max(abs(eone)*eps,ten*eps) 
      ssmax = max(big,abs(eone)) 
      ssmax = ssmax*big 
      sstoll = toll 
      d2max = dmax*dmax 
!  SOLVE ITERATIVELY FOR LAMDA
!  INITIAL GUESS FOR LAMDA IS ZERO EXCEPT NOTE THAT
!  LAMDA SHOULD BE LESS THAN EIGVAL(1)
!  START BY BRACKETING ROOT, THEN HUNT IT DOWN WITH BRUTE FORCE BISECT.
!
      frodo1 = .FALSE. 
      frodo2 = .FALSE. 
      lamda = zero 
      lamda0 = zero 
      if (ts .and. eigit<zero .and. eone>=zero .and. donr) then 
        if (iprnt >= 1) write (iw, *) &
          ' TS SEARCH, CORRECT HESSIAN, TRYING PURE NR STEP' 
        go to 120 
      endif 
      if (.not.ts .and. eone>=zero .and. donr) then 
        if (iprnt >= 1) write (iw, *) &
          ' MIN SEARCH, CORRECT HESSIAN, TRYING PURE NR STEP' 
        go to 120 
      endif 
   60 continue 
      if (ts) then 
        lamda0 = eigval(it) + sqrt(eigval(it)**2+four*fx(it)**2) 
        lamda0 = lamda0*half 
        if (iprnt >= 1) write (iw, 270) lamda0 
      endif 
      sstep = step 
      if (eone <= zero) then 
        lamda = eone - sstep 
      else 
        sstep = eone 
      endif 
      bl = lamda - sstep 
      bu = lamda + sstep*half 
   70 continue 
      fl = zero 
      fu = zero 
      do i = 1, nvar 
        if (i == it) cycle  
        fl = fl + (fx(i)*fx(i))/(bl - eigval(i)) 
        fu = fu + (fx(i)*fx(i))/(bu - eigval(i)) 
      end do 
      fl = fl - bl 
      fu = fu - bu 
!        WRITE(IW,*)'BL,BU,FL,FU FROM BRACK'
!        WRITE(IW,668)BL,BU,FL,FU
!668     FORMAT(6F20.15)
      if (fl*fu < zero) go to 90 
      bl = bl - (eone - bl) 
      bu = bu + half*(eone - bu) 
      if (bl <= (-ssmax)) then 
        bl = -ssmax 
        frodo1 = .TRUE. 
      endif 
      if (abs(eone - bu) <= ssmin) then 
        bu = eone - ssmin 
        frodo2 = .TRUE. 
      endif 
      if (frodo1 .and. frodo2) then 
        write (iw, *) 'NUMERICAL PROBLEMS IN BRACKETING LAMDA', eone, bl, bu, &
          fl, fu 
        write (iw, *) ' GOING FOR FIXED STEP SIZE....' 
        go to 200 
      endif 
      go to 70 
   90 continue 
      ncnt = 0 
      xlamda = zero 
  100 continue 
      fl = zero 
      fu = zero 
      fm = zero 
      lamda = half*(bl + bu) 
      do i = 1, nvar 
        if (i == it) cycle  
        fl = fl + (fx(i)*fx(i))/(bl - eigval(i)) 
        fu = fu + (fx(i)*fx(i))/(bu - eigval(i)) 
        fm = fm + (fx(i)*fx(i))/(lamda - eigval(i)) 
      end do 
      fl = fl - bl 
      fu = fu - bu 
      fm = fm - lamda 
!        WRITE(IW,*)'BL,BU,LAMDA,FL,FU,FM FROM SEARCH'
!        WRITE(IW,668)BL,BU,LAMDA,FL,FU,FM
      if (abs(xlamda - lamda) < sstoll) go to 120 
      ncnt = ncnt + 1 
      if (ncnt > 1000) then 
        write (iw, *) 'TOO MANY ITERATIONS IN LAMDA BISECT', bl, bu, lamda, fl&
          , fu 
         call mopend ('TOO MANY ITERATIONS IN LAMDA BISECT IN EF') 
        return  
      endif 
      xlamda = lamda 
      if (fm*fu < zero) bl = lamda 
      if (fm*fl < zero) bu = lamda 
      go to 100 
!
  120 continue 
      if (iprnt >= 1) write (iw, 280) lamda 
!
!  CALCULATE THE STEP
!
      d(:nvar) = zero 
      do i = 1, nvar 
        if (abs(lamda) < 1.d-20 .and. abs(eigval(i))<tmtwo) then 
          temp = zero 
        else 
          temp = fx(i)/(lamda - eigval(i)) 
        endif 
        if (i == it) then 
          if (abs(lamda0 - eigval(it)) < 1.D-9) then 
            write (iw, *) ' TS FAILED TO LOCATE TRANSITION STATE' 
            call mopend ('TS FAILED TO LOCATE TRANSITION STATE') 
            return  
          endif 
          temp = fx(it)/(lamda0 - eigval(it)) 
        endif 
        if (iprnt >= 5) write (iw, *) 'FORMD, DELTA STEP', i, temp 
        d(:nvar) = d(:nvar) + temp*u(:nvar,i) 
      end do 
      ddd = sqrt(dot(d,d,nvar)) 
      if (abs(lamda) + abs(lamda0) < 1.d-20 .and. iprnt>=1) write (iw, 160) ddd 
  160 format(1x,'PURE NR-STEP HAS LENGTH',f10.5) 
      if (Abs(lamda) > 1.d-20 .and. abs(lamda0 + lamda) > 1.d-20.and. iprnt>=1) write (iw, 170) ddd 
  170 format(1x,'P-RFO-STEP   HAS LENGTH',f10.5) 
      if (ddd < dmax + tmsix) then 
        xlamd = lamda 
        xlamd0 = lamda0 
        return  
      endif 
      if (abs(lamda) + abs(lamda0) < 1.d-20) go to 60 
      if (rscal) then 
        skal = dmax/ddd 
        d(:nvar) = d(:nvar)*skal 
        ddd = sqrt(dot(d,d,nvar)) 
        if (iprnt >= 1) write (iw, 190) skal 
  190   format(5x,'CALCULATED STEP SIZE TOO LARGE, SCALED WITH',f9.5) 
        xlamd = lamda 
        xlamd0 = lamda0 
        return  
      endif 
  200 continue 
      lamda = zero 
      frodo1 = .FALSE. 
      frodo2 = .FALSE. 
      sstep = step 
      if (eone <= zero) lamda = eone - sstep 
      if (ts .and. (-eigit)<eone) lamda = (-eigit) - sstep 
      if (eone > zero) sstep = eone 
      bl = lamda - sstep 
      bu = lamda + sstep*half 
  210 continue 
      fl = zero 
      fu = zero 
      do i = 1, nvar 
        if (i == it) cycle  
        fl = fl + (fx(i)/(bl-eigval(i)))**2 
        fu = fu + (fx(i)/(bu-eigval(i)))**2 
      end do 
      if (ts) then 
        fl = fl + (fx(it)/(bl+eigval(it)))**2 
        fu = fu + (fx(it)/(bu+eigval(it)))**2 
      endif 
      fl = fl - d2max 
      fu = fu - d2max 
!        WRITE(IW,*)'BL,BU,FL,FU FROM BRACK2'
!        WRITE(IW,668)BL,BU,FL,FU
      if (fl*fu < zero) go to 230 
      bl = bl - (eone - bl) 
      bu = bu + half*(eone - bu) 
      if (bl <= (-ssmax)) then 
        bl = -ssmax 
        frodo1 = .TRUE. 
      endif 
      if (abs(eone - bu) <= ssmin) then 
        bu = eone - ssmin 
        frodo2 = .TRUE. 
      endif 
      if (frodo1 .and. frodo2) then 
        write (iw, *) 'NUMERICAL PROBLEMS IN BRACKETING LAMDA', eone, bl, bu, &
          fl, fu 
        write (iw, *) ' GOING FOR FIXED LEVEL SHIFTED NR STEP...' 
!           BOTH LAMDA SEARCHES FAILED, GO FOR FIXED LEVEL SHIFTED NR
!           THIS IS UNLIKELY TO PRODUCE ANYTHING USEFUL, BUT MAYBE WE'RE
        lamda = eone - sfix 
        lamda0 = eigit + sfix 
        rscal = .TRUE. 
        go to 120 
      endif 
      go to 210 
  230 continue 
      ncnt = 0 
      xlamda = zero 
  240 continue 
      fl = zero 
      fu = zero 
      fm = zero 
      lamda = half*(bl + bu) 
      do i = 1, nvar 
        if (i == it) cycle  
        fl = fl + (fx(i)/(bl-eigval(i)))**2 
        fu = fu + (fx(i)/(bu-eigval(i)))**2 
        fm = fm + (fx(i)/(lamda-eigval(i)))**2 
      end do 
      if (ts) then 
        fl = fl + (fx(it)/(bl+eigval(it)))**2 
        fu = fu + (fx(it)/(bu+eigval(it)))**2 
        fm = fm + (fx(it)/(lamda+eigval(it)))**2 
      endif 
      fl = fl - d2max 
      fu = fu - d2max 
      fm = fm - d2max 
!        WRITE(IW,*)'BL,BU,LAMDA,FL,FU,FM FROM SEARCH2'
!        WRITE(IW,668)BL,BU,LAMDA,FL,FU,FM
      if (abs(xlamda - lamda) < sstoll) go to 260 
      ncnt = ncnt + 1 
      if (ncnt > 1000) then 
        write (iw, *) 'TOO MANY ITERATIONS IN LAMDA BISECT', bl, bu, lamda, fl&
          , fu 
        call mopend ('TOO MANY ITERATIONS IN LAMDA BISECT IN EF') 
        return  
      endif 
      xlamda = lamda 
      if (fm*fu < zero) bl = lamda 
      if (fm*fl < zero) bu = lamda 
      go to 240 
!
  260 continue 
      lamda0 = -lamda 
      rscal = .TRUE. 
      go to 120 
!
  270 format(1x,'LAMDA THAT MAXIMIZES ALONG TS MODES =   ',f15.5) 
  280 format(1x,'LAMDA THAT MINIMIZES ALONG ALL MODES =  ',f15.5)  
      end subroutine formd 


      subroutine gethes(xparam, igthes, nvar, iloop, hess, pmat, bmat) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      use molkst_C, only : natoms, nalpha, tleft, time0, ndep, keywrd, moperr    
      use permanent_arrays, only : loc, geo, grad, gnext1, gmin1
      USE ef_C, ONLY: mode, nstep,  iprnt 
      USE chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use efsav_I 
      use second_I 
      use compfg_I 
      use symtry_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: igthes 
      integer , intent(in) :: nvar 
      integer , intent(in) :: iloop 
      real(double)  :: xparam(*) 
      real(double)  :: hess(nvar,nvar) 
      real(double)  :: pmat(nvar*nvar) 
      real(double)  :: bmat(nvar,nvar) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      integer , dimension(9) :: ipow 
      integer :: i, j, ij, nxxx, mtmp, iidum = 0, if, k, l 
      real(double) :: zzero, two, dghss, dghsa, dghsd, xinc, sum, dg1, dg2, dg3&
        , tdm, fdmy = 0.d0, time1, tstore, dummy, time2, tstep, tt0, funct = 0.d0

      save zzero, two, dghss, dghsa, dghsd, xinc 
!-----------------------------------------------
!     GET THE HESSIAN. DEPENDING ON IGTHES WE GET IT FROM :
!
!      0 : DIAGONAL MATRIX, DGHSX*I (DEFAULT FOR MIN-SEARCH)
!      1 : CALCULATE IT NUMERICALLY (DEFAULT FOR TS-SEARCH)
!      2 : READ IN FROM FTN009
!      3 : CALCULATE IT BY DOUBLE NUMERICAL DIFFERENTIATION
!      4 : READ IN FROM FTN009 (DURING RESTART, PARTLY OR WHOLE,
!          ALREADY DONE AT THIS POINT)
      data zzero, two/ 0.D0, 2.D0/  
!
!      DATA DGHSS,DGHSA,DGHSD /2500.D0,900.D0,1000.D0/
      data dghss, dghsa, dghsd/ 1000.D0, 500.D0, 200.D0/  
      data xinc/ 1.D-3/  
!     DGHSX IS HESSIAN DIAGONAL FOR IGTHES=0 (STRETCHING, ANGLE,
!     DIHEDRAL).  THE VALUES SHOULD BE 'OPTIMUM' FOR CYCLOHEXANONE
!     XINC IS STEPSIZE FOR HESSIAN CALCULATION. TESTS SHOWS THAT IT SHOU
!     BE IN THE RANGE 10(-2) TO 10(-4). 10(-3) APPEARS TO BE
!     A REASONABLE COMPROMISE BETWEEN ACCURACY AND NUMERICAL PROBLEMS
      if (igthes == 0) then 
        write (iw, 10) 
   10   format(/,10x,'DIAGONAL MATRIX USED AS START HESSIAN',/) 
        hess = zzero 
        sum = 200.D0 
        if (index(keywrd,' XYZ') /= 0) then 
          dg1 = sum 
          dg2 = sum 
          dg3 = sum 
        else 
          dg1 = dghss 
          dg2 = dghsa 
          dg3 = dghsd 
        endif 
        ij = 1 
        l30: do j = 1, natoms 
          do i = 1, 3 
            if (loc(2,ij)/=i .or. loc(1,ij)/=j) cycle  
            if (i == 1) hess(ij,ij) = dg1 
            if (i == 2) hess(ij,ij) = dg2 
            if (i == 3) hess(ij,ij) = dg3 
            ij = ij + 1 
            if (ij <= nvar) cycle  
            exit  l30 
          end do 
        end do l30 
        ij = ij - 1 
        if (ij /= nvar) write (iw, *) 'ERROR IN IGTHES=0,IJ,NVAR', ij, nvar 
      endif 
!
      if (igthes == 2) then 
        write (iw, 50) 
   50   format(/,10x,'HESSIAN READ FROM DISK',/) 
        ipow(9) = 0 
!        USE DUMMY ARRAY FOR CALL EXCEPT FOR HESSIAN
!        TEMPORARY SET NALPHA = 0, THEN WE CAN READ HESSIAN FROM RHF
!        RUN FOR USE IN SAY UHF RUNS
!        ALSO SAVE MODE, TO ALLOW FOLLOWING A DIFFERENT MODE THAN THE ON
!        CURRENTLY ON RESTART FILE
        nxxx = nalpha 
        nalpha = 0 
        mtmp = mode 
        tdm = 0.d0
        call efsav (tdm, hess, fdmy, gnext1, gmin1, pmat, iidum, j, bmat, ipow) 
        if (moperr) return  
        nalpha = nxxx 
        mode = mtmp 
        nstep = 0 
      endif 
      if (igthes==1 .or. igthes==3 .or. igthes==4) then 
!       IF IGTHES IS .EQ. 4, THEN THIS IS A HESSIAN RESTART.
!       USE GNEXT1 AND DUMMY FOR CALLS TO COMPFG DURING HESSIAN
!       CALCULATION
        if (igthes == 1) write (iw, 60) 
   60   format(/,10x,'HESSIAN CALCULATED NUMERICALLY',/) 
        if (igthes == 3) write (iw, 70) 
   70   format(/,10x,'HESSIAN CALCULATED DOUBLE NUMERICALLY',/) 
        if (iprnt >= 5) write (iw, '(I3,12(8F9.4,/3X))') 0, (grad(if),if=1,nvar&
          ) 
        time1 = second(1) 
        tstore = time1 
        do i = iloop, nvar 
          xparam(i) = xparam(i) + xinc 
          call compfg (xparam, .TRUE., dummy, .TRUE., gnext1, .TRUE.) 
          if (iprnt >= 5) write (iw, '(I3,12(8F9.4,/3X))') i, (gnext1(if),if=1,&
            nvar) 
          xparam(i) = xparam(i) - xinc 
          if (igthes == 3) then 
            xparam(i) = xparam(i) - xinc 
            call compfg (xparam, .TRUE., dummy, .TRUE., gmin1, .TRUE.) 
            if (Abs(dummy) < -1.d10) return ! dummy use of dummy
            if (iprnt >= 5) write (iw, '(I3,12(8F9.4,/3X))') (-i), (gmin1(if),&
              if=1,nvar) 
            xparam(i) = xparam(i) + xinc 
            hess(i,:) = (gnext1(:nvar)-gmin1(:nvar))/(xinc + xinc) 
          else 
            hess(i,:) = (gnext1(:nvar)-grad(:nvar))/xinc 
          endif 
          time2 = second(1) 
          tstep = time2 - time1 
          tleft = tleft - tstep 
          time1 = time2 
          if (tleft >= tstep*two) cycle  
!
!  STORE PARTIAL HESSIAN PATRIX
!  STORE GRADIENTS FOR GEOMETRY AND ILOOP AS POSITIVE
          write (iw, '(A)') ' NOT ENOUGH TIME TO COMPLETE HESSIAN' 
          write (iw, '(A,I4)') ' STOPPING IN HESSIAN AT COORDINATE:', i 
          ipow(9) = 1 
          tt0 = second(1) - time0 
          call efsav (tt0, hess, funct, grad, xparam, pmat, i, nstep, bmat,ipow) 
          if (moperr) return  
          return  
        end do 
!     FIX LAST ENTRY IN GEO ARRAY, THIS IS CURRENTLY AT VALUE-XINC
        k = loc(1,nvar) 
        l = loc(2,nvar) 
        geo(l,k) = xparam(nvar) 
        if (ndep /= 0) call symtry 
!        ADD ALL TIME USED BACK TO TLEFT, THIS WILL THEN BE SUBTRACTED
!        AGAIN IN MAIN EF ROUTINE
        time2 = second(1) 
        tstep = time2 - tstore 
        tleft = tleft + tstep 
      endif 
!
!     SYMMETRIZE HESSIAN
!
      do i = 1, nvar 
        hess(i,:i-1) = (hess(i,:i-1)+hess(:i-1,i))/two 
        hess(:i-1,i) = hess(i,:i-1) 
      end do 
      return  
      end subroutine gethes 


      subroutine overlp(dmax, osmin, newmod, nvar, lorjk, u) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numcal
      USE ef_C, ONLY: mode, iprnt, vmode, omin
      USE chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      use dot_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: newmod 
      integer  :: nvar 
      real(double) , intent(in) :: dmax 
      real(double) , intent(in) :: osmin 
      logical , intent(out) :: lorjk 
      real(double)  :: u(nvar,nvar) 

!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, it, i 
      real(double) :: tovlp, ovlp 

      save icalcn, it 
!-----------------------------------------------
      data icalcn/ 0/  
!
!  ON THE FIRST STEP SIMPLY DETERMINE WHICH MODE TO FOLLOW
!
!     IF(NSTEP.EQ.1) THEN
      if (icalcn /= numcal) then 
        icalcn = numcal 
        if (mode > nvar) then 
          write (iw, *) 'ERROR!! MODE IS LARGER THAN NVAR', mode 
          call mopend ('ERROR!! MODE IS LARGER THAN NVAR') 
          return  
        endif 
        it = mode 
        if (iprnt >= 1) write (iw, 10) mode 
   10   format(5x,'HESSIAN MODE FOLLOWING SWITCHED ON'/,'     FOLLOWING MODE ',&
          i3) 
!
      else 
!
!  ON SUBSEQUENT STEPS DETERMINE WHICH HESSIAN EIGENVECTOR HAS
!  THE GREATEST OVERLAP WITH THE MODE WE ARE FOLLOWING
!
        it = 1 
        lorjk = .FALSE. 
        tovlp = dot(u(1,1),vmode,nvar) 
        tovlp = abs(tovlp) 
        do i = 2, nvar 
          ovlp = dot(u(1,i),vmode,nvar) 
          ovlp = abs(ovlp) 
          if (ovlp <= tovlp) cycle  
          tovlp = ovlp 
          it = i 
        end do 
!
        if (iprnt >= 1) write (iw, 30) it, tovlp 
        if (tovlp < omin) then 
          if (dmax > osmin) then 
            lorjk = .TRUE. 
            if (iprnt >= 1) write (iw, 40) omin 
            return  
          else 
            if (iprnt >= 1) write (iw, 50) omin, dmax, osmin 
          endif 
        endif 
      endif 
   30 format(5x,'OVERLAP OF CURRENT MODE',i3,' WITH PREVIOUS MODE IS ',f6.3) 
   40 format(5x,'OVERLAP LESS THAN OMIN',f6.3,' REJECTING PREVIOUS STEP') 
   50 format(5x,'OVERLAP LESS THAN OMIN',f6.3,' BUT TRUST RADIUS',f6.3,&
        ' IS LESS THAN',f6.3,/,5x,' ACCEPTING STEP') 
!
!  SAVE THE EIGENVECTOR IN VMODE
!
      vmode(:nvar) = u(:nvar,it) 
!
      newmod = it 
      return  
!
      end subroutine overlp 


      subroutine prjfc(f, xparam, nvar, cof) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE permanent_arrays, only : atmass
      use molkst_C, only : numat
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dgefa_I 
      use dgedi_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nvar 
      real(double) , intent(inout) :: f(nvar,nvar) 
      real(double) , intent(in) :: xparam(nvar) 
      real(double) , intent(inout) :: cof(nvar,nvar) 
   !   real(double) , intent(inout) :: p(nvar,nvar) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      real(double), parameter :: zero = 0.0D+00 
      real(double), parameter :: one = 1.0D+00 
      real(double), parameter :: cut8 = 1.0D-08 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(6) :: iscr 
      integer :: natm, nc1, ij, i, l, j, info, ip, indx, jp, jndx, ic, jend, jc&
        , ia, ib, ja, jb, ii, jj, k 
      real(double), dimension(nvar) :: x, rm 
      real(double), dimension(3,3,3) :: tens 
      real(double), dimension(3,3) :: rot
      real(double), dimension(3) :: cmass 
      real(double), dimension(3,numat) :: coord 
      real(double), dimension(nvar) :: dx 
      real(double) :: tmp, totm, chk, det, trp, sum, p(1,1)
      save tens 
!-----------------------------------------------
!
!  CALCULATES PROJECTED FORCE CONSTANT MATRIX (F).
!  THIS ROUTINE CAME ORIGINALLY FROM POLYRATE. IT IS USED BY PERMISSION
!  OF D. TRUHLAR. THE CURRENT VERSION IS LIFTED FROM GAMESS AND
!  ADAPTED BY F.JENSEN, ODENSE, DK
!  IF WE ARE AT A STATIONARY POINT (STPT=.T.), I.E. GNORM .LT. 10,
!  THEN THE ROTATIONAL AND TRANSLATIONAL MODES ARE PROJECTED OUT
!  AND THEIR FREQUENCIES BECOME IDENTICAL ZERO. IF NOT AT A STATIONARY
!  POINT THEN THE MASS-WEIGHTED GRADIENT IS ALSO PROJECTED OUT AND
!  THE CORRESPONDING FREQUENCY BECOME ZERO.
! ************************************************
!   X : MASS-WEIGHTED COORDINATE
!   DX: NORMALIZED MASS-WEIGHTED GRADIENT VECTOR
!   F : MASS-WEIGHTED FORCE CONSTANT MATRIX
!   RM: INVERSION OF SQUARE ROOT OF MASS
!   P, COF: BUFFER
!
!
! TOTALLY ASYMMETRIC CARTESIAN TENSOR.
!
      data tens/ 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, -1.0D+00, 0.0D+00&
        , 1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, &
        0.0D+00, -1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, -1.0D+00, 0.0D+00, &
        1.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00/  
!
      natm = nvar/3 
      nc1 = nvar 
      ij = 1 
      coord(1,:natm) = xparam(ij:(natm-1)*3+ij:3) 
      coord(2,:natm) = xparam(ij+1:natm*3-2+ij:3) 
      coord(3,:natm) = xparam(ij+2:natm*3-1+ij:3) 
!     CALCULATE 1/SQRT(MASS)
      l = 0 
      do i = 1, natm 
        tmp = one/sqrt(atmass(i)) 
        rm(l+1:3+l) = tmp 
        l = 3 + l 
      end do 
!     PREPARE GRADIENT
      dx(:nc1) = zero 
!     FIND CMS AND CALCULATED MASS WEIGHTED COORDINATES
      totm = zero 
      cmass(1) = zero 
      cmass(2) = zero 
      cmass(3) = zero 
      do i = 1, natm 
        totm = totm + atmass(i) 
        cmass = cmass + atmass(i)*coord(:,i) 
      end do 
      cmass = cmass/totm 
      l = 0 
      do i = 1, natm 
        do j = 1, 3 
          tmp = sqrt(atmass(i)) 
          l = l + 1 
          x(l) = tmp*(coord(j,i)-cmass(j)) 
        end do 
      end do 
      rot = zero 
      do i = 1, natm 
        l = 3*(i - 1) + 1 
        rot(1,1) = rot(1,1) + x(l+1)**2 + x(l+2)**2 
        rot(1,2) = rot(1,2) - x(l)*x(l+1) 
        rot(1,3) = rot(1,3) - x(l)*x(l+2) 
        rot(2,2) = rot(2,2) + x(l)**2 + x(l+2)**2 
        rot(2,3) = rot(2,3) - x(l+1)*x(l+2) 
        rot(3,3) = rot(3,3) + x(l)**2 + x(l+1)**2 
      end do 
      rot(2,1) = rot(1,2) 
      rot(3,1) = rot(1,3) 
      rot(3,2) = rot(2,3) 
      chk = rot(1,1)*rot(2,2)*rot(3,3) 
      if (abs(chk) <= cut8) then 
        if (abs(rot(1,1)) <= cut8) then 
          if (abs(rot(2,2)) <= cut8) then 
            if (abs(rot(3,3)) <= cut8) then 
              write (iw, 90) rot(1,1), rot(2,2), rot(3,3) 
   90         format(1x,'EVERY DIAGONAL ELEMENTS ARE ZERO ?',3f20.10) 
              return  
            endif 
!
!* 1. X,Y=0 BUT Z.NE.0
            rot(3,3) = one/rot(3,3) 
            go to 170 
          endif 
! Y.NE.0
          if (abs(rot(3,3)) > cut8) go to 150 
!* 2. X,Z=0 BUT Y.NE.0
          rot(2,2) = one/rot(2,2) 
          go to 170 
        endif 
! X.NE.0
        if (abs(rot(2,2)) <= cut8) then 
          if (abs(rot(3,3)) > cut8) go to 140 
!* 3. Y,Z=0 BUT X.NE.0
          rot(1,1) = one/rot(1,1) 
          go to 170 
        endif 
!* 4. X,Y.NE.0 BUT Z=0
        det = rot(1,1)*rot(2,2) - rot(1,2)*rot(2,1) 
        trp = rot(1,1) 
        rot(1,1) = rot(2,2)/det 
        rot(2,2) = trp/det 
        rot(1,2) = -rot(1,2)/det 
        rot(2,1) = -rot(2,1)/det 
        go to 170 
!* 5. X,Z.NE.0 BUT Y=0
  140   continue 
        det = rot(1,1)*rot(3,3) - rot(1,3)*rot(3,1) 
        trp = rot(1,1) 
        rot(1,1) = rot(3,3)/det 
        rot(3,3) = trp/det 
        rot(1,3) = -rot(1,3)/det 
        rot(3,1) = -rot(3,1)/det 
        go to 170 
!* 6. Y,Z.NE.0 BUT X=0
  150   continue 
        det = rot(3,3)*rot(2,2) - rot(3,2)*rot(2,3) 
        trp = rot(3,3) 
        rot(3,3) = rot(2,2)/det 
        rot(2,2) = trp/det 
        rot(3,2) = -rot(3,2)/det 
        rot(2,3) = -rot(2,3)/det 
        go to 170 
      endif 
      info = 0 
      call dgefa (rot, 3, 3, iscr, info) 
      if (info /= 0) return  
      det = zero 
      call dgedi (rot, 3, 3, iscr, 1) 
!
  170 continue 
      do ip = 1, natm 
        indx = 3*(ip - 1) 
        do jp = 1, ip 
          jndx = 3*(jp - 1) 
          do ic = 1, 3 
            jend = 3 
            if (jp == ip) jend = ic 
            do jc = 1, jend 
              sum = zero 
              do ia = 1, 3 
                do ib = 1, 3 
                  if (tens(ia,ib,ic) == 0.d0) cycle  
                  do ja = 1, 3 
                    do jb = 1, 3 
                      if (tens(ja,jb,jc) == 0.d0) cycle  
                      sum = sum + tens(ia,ib,ic)*tens(ja,jb,jc)*rot(ia,ja)*x(&
                        indx+ib)*x(jndx+jb) 
                    end do 
                  end do 
                end do 
              end do 
              ii = indx + ic 
              jj = jndx + jc 
              p(ii,jj) = sum + dx(ii)*dx(jj) 
              if (ic /= jc) cycle  
              p(ii,jj) = p(ii,jj) + one/(rm(ii)*rm(jj)*totm) 
            end do 
          end do 
        end do 
      end do 
!
! 7. COMPUTE DELTA(I,J)-P(I,J)
      do i = 1, nc1 
        do j = 1, i 
          p(i,j) = -p(i,j) 
          if (i /= j) cycle  
          p(i,j) = one + p(i,j) 
        end do 
      end do 
!
! 8. NEGLECT SMALLER VALUES THAN 10**-8.
      do i = 1, nc1 
        do j = 1, i 
          if (abs(p(i,j)) < cut8) p(i,j) = zero 
          p(j,i) = p(i,j) 
        end do 
      end do 
      do i = 1, nc1 
        do j = 1, nc1 
          sum = zero 
          do k = 1, nc1 
            sum = sum + f(i,k)*p(k,j) 
          end do 
          cof(i,j) = sum 
        end do 
      end do 
!
! 11. COMPUTE P*F*P.
      do i = 1, nc1 
        do j = 1, nc1 
          sum = zero 
          do k = 1, nc1 
            sum = sum + p(i,k)*cof(k,j) 
          end do 
          f(i,j) = sum 
        end do 
      end do 
      return  
      end subroutine prjfc 


      subroutine prthes(eigval, nvar, hess, u) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE ef_C, ONLY: iprnt 
      USE chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nvar 
      real(double) , intent(in) :: eigval(nvar) 
      real(double) , intent(in) :: hess(nvar,nvar) 
      real(double) , intent(in) :: u(nvar,nvar) 

!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  low, nup, i, j 
!-----------------------------------------------

      if (iprnt >= 4) then 
        write (iw, *) ' ' 
        write (iw, *) '              HESSIAN MATRIX' 
        low = 1 
        nup = 8 
   10   continue 
        nup = min(nup,nvar) 
        write (iw, 50) (i,i=low,nup) 
        do i = 1, nvar 
          write (iw, 60) i, (hess(i,j),j=low,nup) 
        end do 
        nup = nup + 8 
        low = low + 8 
        if (low <= nvar) go to 10 
      endif 
      write (iw, *) ' ' 
      write (iw, *) '              HESSIAN EIGENVALUES AND -VECTORS' 
      low = 1 
      nup = 8 
   30 continue 
      nup = min(nup,nvar) 
      write (iw, 50) (i,i=low,nup) 
      write (iw, 70) (eigval(i),i=low,nup) 
      do i = 1, nvar 
        write (iw, 80) i, (u(i,j),j=low,nup) 
      end do 
      nup = nup + 8 
      low = low + 8 
      if (low <= nvar) go to 30 
   50 format(/,3x,8i9) 
   60 format(1x,i3,8f9.1) 
   70 format(/,4x,8f9.1,/) 
   80 format(1x,i3,8f9.4) 
      return  
      end subroutine prthes 


      subroutine updhes(svec, tvec, nvar, iupd, hess) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numcal
      use permanent_arrays, only : grad
      USE ef_C, ONLY: oldf, d, ddd, iprnt 
      USE chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dot_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: nvar 
      integer , intent(in) :: iupd 
      real(double)  :: svec(*) 
      real(double)  :: tvec(*) 
      real(double) , intent(inout) :: hess(nvar,nvar) 

!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn,  i, j 
      real(double) :: zero, dds, ddtd, temp 

      save zero, icalcn 
!-----------------------------------------------
 
      data icalcn/ 0/  
      data zero/ 0.0D0/  
!
!  UPDATING OF THE HESSIAN
!  DEPENDS ON CURRENT GRADIENTS, OLD GRADIENTS AND THE
!  CORRECTION VECTOR USED ON THE LAST CYCLE
!  SVEC & TVEC ARE FOR TEMPORARY STORAGE
!
!  2 UPDATING PROCEDURES ARE POSSIBLE
!  (I)   THE POWELL UPDATE
!        THIS PRESERVES THE SYMMETRIC CHARACTER OF THE HESSIAN
!        WHILST ALLOWING ITS EIGENVALUE STRUCTURE TO CHANGE.
!        IT IS THE DEFAULT UPDATE FOR A TRANSITION STATE SEARCH
!  (II)  THE BFGS UPDATE
!        THIS UPDATE HAS THE IMPORTANT CHARACTERISTIC OF RETAINING
!        POSITIVE DEFINITENESS (NOTE: THIS IS NOT RIGOROUSLY
!        GUARANTEED, BUT CAN BE CHECKED FOR BY THE PROGRAM).
!        IT IS THE DEFAULT UPDATE FOR A MINIMUM SEARCH
!
!     SWITCH : IUPD
!       IUPD = 0  :  SKIP UPDATE
!       IUPD = 1  :  POWELL
!       IUPD = 2  :  BFGS
!
      if (icalcn /= numcal) then 
        icalcn = numcal 
        if (iprnt >= 2) then 
          if (iupd == 0) write (iw, 120) 
          if (iupd == 1) write (iw, 110) 
          if (iupd == 2) write (iw, 130) 
        endif 
      endif 
      if (iupd == 0) return  
      tvec(:nvar) = zero 
      do j = 1, nvar 
        tvec(:nvar) = tvec(:nvar) + hess(:nvar,j)*d(j) 
      end do 
!
      if (iupd == 1) then 
!
!   (I) POWELL UPDATE
!
        tvec(:nvar) = grad(:nvar) - oldf(:nvar) - tvec(:nvar) 
        svec(:nvar) = grad(:nvar) - oldf(:nvar) 
        dds = ddd*ddd 
        ddtd = dot(tvec,d,nvar) 
        ddtd = ddtd/dds 
!
        do i = 2, nvar 
          do j = 1, i - 1 
            temp = tvec(i)*d(j) + d(i)*tvec(j) - d(i)*ddtd*d(j) 
            hess(i,j) = hess(i,j) + temp/dds 
            hess(j,i) = hess(i,j) 
          end do 
        end do 
        do i = 1, nvar 
          temp = d(i)*(2.0D0*tvec(i)-d(i)*ddtd) 
          hess(i,i) = hess(i,i) + temp/dds 
        end do 
!
      endif 
      if (iupd == 2) then 
!
!  (II) BFGS UPDATE
!
        svec(:nvar) = grad(:nvar) - oldf(:nvar) 
        dds = dot(svec,d,nvar) 
!
!  IF DDS IS NEGATIVE, RETENTION OF POSITIVE DEFINITENESS IS NOT
!  GUARANTEED. PRINT A WARNING AND SKIP UPDATE THIS CYCLE.
!
!
        ddtd = dot(d,tvec,nvar) 
!
        do i = 2, nvar 
          do j = 1, i - 1 
            temp = (svec(i)*svec(j))/dds - (tvec(i)*tvec(j))/ddtd 
            hess(i,j) = hess(i,j) + temp 
            hess(j,i) = hess(i,j) 
          end do 
        end do 
        do i = 1, nvar 
          temp = (svec(i)*svec(i))/dds - (tvec(i)*tvec(i))/ddtd 
          hess(i,i) = hess(i,i) + temp 
        end do 
      endif 
!
      return  
!
  110 format(/,5x,'HESSIAN IS BEING UPDATED USING THE POWELL UPDATE',/) 
  120 format(/,5x,'HESSIAN IS NOT BEING UPDATED',/) 
  130 format(/,5x,'HESSIAN IS BEING UPDATED USING THE BFGS UPDATE',/)  
      end subroutine updhes 


! **********************************************************************
!     BELOW ARE MATH ROUTINES NEEDED FOR PRJFC. THEY ARE BASICALY JUST
!     MATRIX DIAGONALIZATION ROUTINES AND SHOULD AT SOME POINT BE
!     REPLACED WITH THE DIAGONALIZATION ROUTINE USED IN THE REST OF THE
!     PROGRAM. THE ROUTINES BELOW HAVE BEEN LIFTED FROM GAMESS
! **********************************************************************
      subroutine dgedi(a, lda, n, ipvt, job) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dscal_I 
      use daxpy_I 
      use dswap_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: lda 
      integer  :: n 
      integer , intent(in) :: job 
      integer , intent(in) :: ipvt(n) 
      real(double)  :: a(lda,n)      
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, k, kp1, j, nm1, kb, l 
      real(double) :: ten, t 
      real(double) :: det(2) 
      real(double) :: work(n) 
!-----------------------------------------------
!
!     DGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
!     USING THE FACTORS COMPUTED BY DGECO OR DGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE PRECISION(LDA, N)
!                THE OUTPUT FROM DGECO OR DGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM DGECO OR DGEFA.
!
!        WORK    DOUBLE PRECISION(N)
!                WORK VECTOR.  CONTENTS DESTROYED.
!
!        JOB     INTEGER
!                = 11   BOTH DETERMINANT AND INVERSE.
!                = 01   INVERSE ONLY.
!                = 10   DETERMINANT ONLY.
!
!     ON RETURN
!
!        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE UNCHANGED.
!
!        DET     DOUBLE PRECISION(2)
!                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE NOT REFERENCED.
!                DETERMINANT = DET(1) * 10.0**DET(2)
!                WITH  1.0 .LE. ABS(DET(1)) .LT. 10.0
!                OR  DET(1) .EQ. 0.0 .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
!        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
!        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
!        AND IF DGECO HAS SET RCOND .GT. 0.0 OR DGEFA HAS SET
!        INFO .EQ. 0 .
!
!     LINPACK. THIS VERSION DATED 08/14/78 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS DAXPY,DSCAL,DSWAP
!     FORTRAN ABS,MOD
!
!     COMPUTE DETERMINANT
!
      if (job/10 /= 0) then 
        det(1) = 1.0D+00 
        det(2) = 0.0D+00 
        ten = 10.0D+00 
        do i = 1, n 
          if (ipvt(i) /= i) det(1) = -det(1) 
          det(1) = a(i,i)*det(1) 
!        ...EXIT
          if (det(1) == 0.0D+00) exit  
   10     continue 
          if (abs(det(1)) >= 1.0D+00) go to 20 
          det(1) = ten*det(1) 
          det(2) = det(2) - 1.0D+00 
          go to 10 
   20     continue 
   30     continue 
          if (abs(det(1)) < ten) cycle  
          det(1) = det(1)/ten 
          det(2) = det(2) + 1.0D+00 
          go to 30 
        end do 
      endif 
      if (mod(job,10) /= 0) then 
        do k = 1, n 
          a(k,k) = 1.0D+00/a(k,k) 
          t = -a(k,k) 
          call dscal (k - 1, t, a(1,k), 1) 
          kp1 = k + 1 
          if (n < kp1) cycle  
          do j = kp1, n 
            t = a(k,j) 
            a(k,j) = 0.0D+00 
            call daxpy (k, t, a(1,k), 1, a(1,j), 1) 
          end do 
        end do 
!
!        FORM INVERSE(U)*INVERSE(L)
!
        nm1 = n - 1 
        if (nm1 >= 1) then 
          do kb = 1, nm1 
            k = n - kb 
            kp1 = k + 1 
            work(kp1:n) = a(kp1:n,k) 
            a(kp1:n,k) = 0.0D+00 
            do j = kp1, n 
              t = work(j) 
              call daxpy (n, t, a(1,j), 1, a(1,k), 1) 
            end do 
            l = ipvt(k) 
            if (l == k) cycle  
            call dswap (n, a(1,k), 1, a(1,l), 1) 
          end do 
        endif 
      endif 
      return  
      end subroutine dgedi 


      subroutine dgefa(a, lda, n, ipvt, info) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use idamax_I 
      use dscal_I 
      use daxpy_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: lda 
      integer , intent(in) :: n 
      integer , intent(out) :: info 
      integer , intent(out) :: ipvt(n) 
      real(double)  :: a(lda,n) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: nm1, k, kp1, l, j 
      real(double) :: t 
!-----------------------------------------------
!
!     DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION.
!
!     DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .
!
!     ON ENTRY
!
!        A       DOUBLE PRECISION(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!                     INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO
!                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE
!                     INDICATION OF SINGULARITY.
!
!     LINPACK. THIS VERSION DATED 08/14/78 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS DAXPY,DSCAL,IDAMAX
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
      info = 0 
      nm1 = n - 1 
      if (nm1 >= 1) then 
        do k = 1, nm1 
          kp1 = k + 1 
!
!        FIND L = PIVOT INDEX
!
          l = idamax(n - k + 1,a(k,k),1) + k - 1 
          ipvt(k) = l 
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
          if (a(l,k) /= 0.0D+00) then 
!
!           INTERCHANGE IF NECESSARY
!
            if (l /= k) then 
              t = a(l,k) 
              a(l,k) = a(k,k) 
              a(k,k) = t 
            endif 
!
!           COMPUTE MULTIPLIERS
!
            t = -1.0D+00/a(k,k) 
            call dscal (n - k, t, a(k+1,k), 1) 
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            do j = kp1, n 
              t = a(l,j) 
              if (l /= k) then 
                a(l,j) = a(k,j) 
                a(k,j) = t 
              endif 
              call daxpy (n - k, t, a(k+1,k), 1, a(k+1,j), 1) 
            end do 
          else 
            info = k 
          endif 
        end do 
      endif 
      ipvt(n) = n 
      if (a(n,n) == 0.0D+00) info = n 
      return  
      end subroutine dgefa 


      subroutine efsav(tt0, hess, funct, grad, xparam, pmat, il, jl, bmat, ipow) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE maps_C, only : currt1, currt2, ijlp, ione, jlp, jlp1, ilp, surf, &
      currt, latom, kloop
      use molkst_C, only : nalpha, nvar, keywrd, nscf, jobnam
      use permanent_arrays, only : pa, pb, profil
      USE ef_C, ONLY: oldf, d, ddd, mode, &
        nstep, negreq, vmode, alparm, x0, x1, x2, iloop
      USE chanel_C, only : iw, ires, iden
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dot_I 
      use geout_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: il 
      integer  :: jl 
      real(double)  :: tt0 
      real(double)  :: funct 
      integer  :: ipow(9) 
      real(double)  :: hess(nvar,nvar) 
      real(double)  :: grad(nvar) 
      real(double)  :: xparam(nvar) 
      real(double)  :: pmat(*) 
      real(double)  :: bmat(nvar,nvar) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, j, linear 
      real(double) :: funct1 
      logical :: opend 
!-----------------------------------------------
!*********************************************************************
!
! EFSAV STORES AND RETRIEVE DATA USED IN THE EF GEOMETRY
!        OPTIMISATION. VERY SIMILAR TO POWSAV.
!
!  ON INPUT HESS   = HESSIAN MATRIX, PARTIAL OR WHOLE.
!           GRAD   = GRADIENTS.
!           XPARAM = CURRENT STATE OF PARAMETERS.
!           IL     = INDEX OF HESSIAN,
!           JL     = CYCLE NUMBER REACHED SO-FAR.
!           BMAT   = "B" MATRIX!
!           IPOW   = INDICES AND FLAGS.
!           IPOW(9)= 0 FOR RESTORE, 1 FOR DUMP, 2 FOR SILENT DUMP
!
!*********************************************************************
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
      if (ipow(9)==1 .or. ipow(9)==2) then 
        funct1 = sqrt(dot(grad,grad,nvar)) 
        if (ipow(9) == 1) then 
          write (iw, &
            '(2/10X,''CURRENT VALUE OF GRADIENT NORM =''         ,F12.6)') &
            funct1 
          write (iw, '(/10X,''CURRENT VALUE OF GEOMETRY'',/)') 
          call geout (iw) 
        endif 
!
!  IPOW(1) AND IPOW(9) ARE USED ALREADY, THE REST ARE FREE FOR USE
!
        ipow(8) = nscf 
        if (latom /= 0) then 
          if (index(keywrd,'STEP') /= 0) then 
            write (ires) kloop 
            write (ires) currt 
            write (ires) (profil(i),i=1,kloop) 
          else 
            write (ires) ((alparm(j,i),j=1,3),i=1,nvar) 
            write (ires) iloop, x0, x1, x2 
          endif 
        endif 
        if (index(keywrd,'STEP1') /= 0) then 
          write (ires) ijlp, ilp, jlp, jlp1, ione 
          write (ires) currt1, currt2 
          write (ires) (surf(i),i=1,ijlp) 
        endif 
        write (ires) ipow, il, jl, funct, tt0 
        write (ires) (xparam(i),i=1,nvar) 
        write (ires) (grad(i),i=1,nvar) 
        write (ires) ((hess(j,i),j=1,nvar),i=1,nvar) 
        write (ires) ((bmat(j,i),j=1,nvar),i=1,nvar) 
        write (ires) (oldf(i),i=1,nvar), (d(i),i=1,nvar), (vmode(i),i=1,nvar) 
        write (ires) ddd, mode, nstep, negreq 
        linear = (nvar*(nvar + 1))/2 
        write (ires) (pmat(i),i=1,linear) 
        write (iden) pa
        if (nalpha /= 0) write (iden) pb
        close(iden) 
        close(ires) 
        return  
      else 
        if (latom /= 0) then 
          if (index(keywrd,'STEP') /= 0) then 
            read (ires) kloop 
            read (ires) currt 
            read (ires) (profil(i),i=1,kloop) 
          else 
            read (ires) ((alparm(j,i),j=1,3),i=1,nvar) 
            read (ires) iloop, x0, x1, x2 
          endif 
        endif 
        if (index(keywrd,'STEP1') /= 0) then 
          read (ires) ijlp, ilp, jlp, jlp1, ione 
          read (ires) currt1, currt2 
          read (ires) (surf(i),i=1,ijlp) 
        endif 
        read (ires, end=10, err=10) ipow, il, jl, funct, tt0 
        nscf = ipow(8) 
        i = int(tt0/1000000) 
        tt0 = tt0 - i*1000000 
        write (iw, &
      '(2/10X,''TOTAL TIME USED SO FAR:'',                      F13.2,'' SECOND&
      &S'')') tt0 
        write (iw, '(  10X,''              FUNCTION:'',F17.6)') funct 
        read (ires, end=20, err=20) (xparam(i),i=1,nvar) 
        read (ires, end=20, err=20) (grad(i),i=1,nvar) 
        read (ires, end=20, err=20) ((hess(j,i),j=1,nvar),i=1,nvar) 
        read (ires, end=20, err=20) ((bmat(j,i),j=1,nvar),i=1,nvar) 
        read (ires, end=20, err=20) (oldf(i),i=1,nvar), (d(i),i=1,nvar), (vmode&
          (i),i=1,nvar) 
        read (ires, end=20, err=20) ddd, mode, nstep, negreq 
        linear = (nvar*(nvar + 1))/2 
        read (ires, end=20, err=20) (pmat(i),i=1,linear) 
        close(ires) 
        return  
   10   continue 
        write (iw, '(2/10X,''NO RESTART FILE EXISTS!'')') 
        call mopend ('NO RESTART FILE EXISTS!') 
        return  
   20   continue 
        write (iw, '(2/10X,''Restart file is corrupt!'')')
        call mopend ('Restart file is corrupt!')  
        return  
      endif 
      return  
      end subroutine efsav 


      subroutine efstr(xparam, funct, ihess, ntime, iloop, igthes, mxstep, &
        ireclc, iupd, dmax, ddmax, dmin, tol2, time1, time2, nvar, scf1, lupd, &
        ldump, log, rrscal, donr, gnmin, hess, bmat, pmat) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat, time0, last, limscf, keywrd, moperr
      USE ef_C, ONLY: rmin, rmax, mode, &
        nstep, negreq, iprnt, omin
      use permanent_arrays, only : grad
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:12:16  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use mopend_I 
      use efsav_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: ihess 
      integer , intent(out) :: ntime 
      integer , intent(out) :: iloop 
      integer , intent(inout) :: igthes 
      integer , intent(out) :: mxstep 
      integer , intent(out) :: ireclc 
      integer , intent(inout) :: iupd 
      integer , intent(inout) :: nvar 
      integer , intent(out) :: ldump 
      real(double)  :: funct 
      real(double) , intent(out) :: dmax 
      real(double) , intent(out) :: ddmax 
      real(double) , intent(out) :: dmin 
      real(double) , intent(out) :: tol2 
      real(double) , intent(out) :: time1 
      real(double) , intent(out) :: time2 
      logical , intent(out) :: scf1 
      logical , intent(out) :: lupd 
      logical , intent(out) :: log 
      logical , intent(out) :: rrscal 
      logical , intent(out) :: donr 
      logical , intent(out) :: gnmin 
      real(double)  :: xparam(*) 
      real(double)  :: hess(nvar,nvar) 
      real(double)  :: bmat(nvar,nvar) 
      real(double)  :: pmat(nvar,nvar) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!----------------------------------------------- 
      integer , dimension(9) :: ipow 
      integer :: imin, its, i, ip, mtmp, j, k 
      real(double) :: tt0 = 0.d0
      logical :: restrt 
!-----------------------------------------------
!
!     GET ALL INITIALIZATION DATA
      nvar = abs(nvar) 
      ldump = 0 
      lupd = index(keywrd,' NOUPD') == 0 
      restrt = index(keywrd,' RESTART') /= 0 
      log = index(keywrd,' NOLOG') == 0 
      scf1 = index(keywrd,' 1SCF') /= 0 
      nstep = 0 
      ihess = 0 
      last = 0 
      ntime = 0 
      iloop = 1 
      imin = index(keywrd,' EF') 
      if (imin /= 0) then 
        mode = 0 
        igthes = 0 
        iupd = 2 
        negreq = 0 
        rmin = 0.0D0 
        ddmax = 0.5D0 
      endif 
      limscf = .FALSE. 
      its = index(keywrd,' TS') 
      if (its /= 0) then 
        mode = 1 
        igthes = 1 
        iupd = 1 
        negreq = 1 
        rmin = 0.0D0 
        rmax = 4.0D0 
        omin = 0.8D0 
        ddmax = 0.3D0 
      endif 
      rrscal = .FALSE. 
!
!   THE FOLLOWING CODE CAN BE USED IF 'EF(CYCLES=45...)' IS IMPLEMENTED
!
!#      MXSTEP=WRDKEY(KEYWRD,'EF(',3,'CYCLES',6,600.D0)*1.00001D0
!#      IRECLC=WRDKEY(KEYWRD,'EF(',3,'RECALC',6,999999.D0)*1.00001D0
!#      IUPD  =WRDKEY(KEYWRD,'EF(',3,'UPD',3,IUPD*1.01D0)*1.00001D0
!#      MODE  =WRDKEY(KEYWRD,'EF(',3,'MODE',4,MODE*1.01D0)*1.00001D0
!#      DMIN  =WRDKEY(KEYWRD,'EF(',3,'DDMIN',5,1.D-3)*1.00001D0
!#      DMAX  =WRDKEY(KEYWRD,'EF(',3,'DMAX',4,2.D-1)*1.00001D0
!#      DDMAX =WRDKEY(KEYWRD,'EF(',3,'GMAX',4,DDMAX*1.01D0)*1.00001D0
!#      IGTHES=WRDKEY(KEYWRD,'EF(',3,'HESS',4,IGTHES*1.01D0)*1.00001D0
!#      RMIN  =WRDKEY(KEYWRD,'EF(',3,'RMIN',4,RMIN)*1.00001D0
!#      RMAX  =WRDKEY(KEYWRD,'EF(',3,'RMAX',4,RMAX)*1.00001D0
!#      OMIN  =WRDKEY(KEYWRD,'EF(',3,'OMIN',4,OMIN)*1.00001D0
!
      i = index(keywrd,' RSCAL') 
      if (i /= 0) rrscal = .TRUE. 
      donr = .TRUE. 
      i = index(keywrd,' NONR') 
      if (i /= 0) donr = .FALSE. 
      gnmin = .FALSE. 
      i = index(keywrd,' GNMIN') 
      if (i /= 0) gnmin = .TRUE. 
      iprnt = 0 
      ip = index(keywrd,' PRNT=') 
      if (ip /= 0) iprnt = nint(reada(keywrd,ip)) 
      iprnt = min0(5,iprnt) 
      iprnt = max0(0,iprnt) 
      mxstep = 500 
      i = index(keywrd,' CYCLES=') 
      if (i /= 0) mxstep = nint(reada(keywrd,i)) 
      if (i/=0 .and. mxstep==0 .and. ip==0) iprnt = 3 
      ireclc = 999999 
      i = index(keywrd,' RECALC=') 
      if (i /= 0) ireclc = nint(reada(keywrd,i)) 
      i = index(keywrd,' IUPD=') 
      if (i /= 0) iupd = nint(reada(keywrd,i)) 
      i = index(keywrd,' MODE=') 
      if (i /= 0) mode = nint(reada(keywrd,i)) 
      dmin = 1.0D-3 
      i = index(keywrd,' DDMIN=') 
      if (i /= 0) dmin = reada(keywrd,i) 
      dmax = 0.05D0 
      i = index(keywrd,' DMAX=') 
      if (i /= 0) dmax = reada(keywrd,i) 
      i = index(keywrd,' DDMAX=') 
      if (i /= 0) ddmax = reada(keywrd,i) 
      tol2 = 1.D+0 
      if (index(keywrd,' PREC') /= 0) tol2 = 5.D-2 
      i = index(keywrd,' GNORM=') 
      if (i /= 0) tol2 = reada(keywrd,i) 
      if (index(keywrd,' LET')==0 .and. tol2<0.01D0) then 
        write (iw, '(/,A)') '  GNORM HAS BEEN SET TOO LOW, RESET TO 0.01', &
          '  SPECIFY LET AS KEYWORD TO ALLOW GNORM LESS THAN 0.01' 
        tol2 = 0.01D0 
      endif 
      i = index(keywrd,' HESS=') 
      if (i /= 0) igthes = nint(reada(keywrd,i)) 
      i = index(keywrd,' RMIN=') 
      if (i /= 0) rmin = reada(keywrd,i) 
      i = index(keywrd,' RMAX=') 
      if (i /= 0) rmax = reada(keywrd,i) 
      i = index(keywrd,' OMIN=') 
      if (i /= 0) omin = reada(keywrd,i) 
      time1 = time0 
      time2 = time1 
!   DONE WITH ALL INITIALIZING STUFF.
!   CHECK THAT OPTIONS REQUESTED ARE RESONABLE
      if (nvar>3*numat - 6 .and. numat>=3) write (iw, 10) 
   10 format(/,'*** WARNING! MORE VARIABLES THAN DEGREES OF FREEDOM',/) 
      if (its/=0 .and. iupd==2) then 
        write (iw, *) ' TS SEARCH AND BFGS UPDATE WILL NOT WORK' 
        call mopend ('TS SEARCH AND BFGS UPDATE WILL NOT WORK')  
        return  
      endif 
      if (its/=0 .and. igthes==0) then 
        write (iw, *) ' TS SEARCH REQUIRE BETTER THAN DIAGONAL HESSIAN' 
        call mopend ('TS SEARCH REQUIRE BETTER THAN DIAGONAL HESSIAN') 
        return  
      endif 
      if (igthes<0 .or. igthes>3) then 
        write (iw, *) ' UNRECOGNIZED HESS OPTION', igthes 
        call mopend ('UNRECOGNIZED HESS OPTION') 
        return  
      endif 
      if (omin<0.D0 .or. omin>1.D0) then 
        write (iw, *) ' OMIN MUST BE BETWEEN 0 AND 1', omin 
        write (iw, *) ' OMIN MUST BE BETWEEN 0 AND 1', omin   
        return  
      endif 
      nstep = 0 
      if (restrt) then 
!
!   RESTORE DATA. I INDICATES (HESSIAN RESTART OR OPTIMIZATION
!   RESTART). IF I .GT. 0 THEN HESSIAN RESTART AND I IS LAST
!   STEP CALCULATED IN THE HESSIAN. IF I .LE. 0 THEN J (NSTEP)
!   IN AN OPTIMIZATION HAS BEEN DONE.
!
        ipow(9) = 0 
        mtmp = mode 
        j=0
        call efsav (tt0, hess, funct, grad, xparam, pmat, i, j, bmat, ipow) 
        if (moperr) return  
        mode = mtmp 
        k = int(tt0/1000000.D0) 
        time0 = time0 - tt0 + k*1000000.D0 
        iloop = i 
        if (i > 0) then 
          igthes = 4 
          nstep = j 
          write (iw, '(10X,''RESTARTING HESSIAN AT POINT'',I4)') iloop 
          if (nstep /= 0) write (iw, '(10X,''IN OPTIMIZATION STEP'',I4)') nstep 
        else 
          nstep = j 
          write (iw, '(2/10X,''RESTARTING OPTIMIZATION AT STEP'',I4)') nstep 
        endif 
      endif 
      return  
      end subroutine efstr 
      subroutine setup_ef(n)
      USE ef_C, ONLY: hess, bmat, u, oldhss, oldu, pmat, uc, hessc, oldf, &
      & d, vmode
      use molkst_C, only : nvar
      implicit none
      integer :: n
      if(n > 0) then
        if (allocated(hess)) deallocate(hess)
        if (allocated(bmat)) deallocate(bmat)
        if (allocated(u)) deallocate(u)
        if (allocated(oldhss)) deallocate(oldhss)
        if (allocated(oldu)) deallocate(oldu)
        if (allocated(pmat)) deallocate(pmat)
        if (allocated(uc)) deallocate(uc)
        if (allocated(hessc)) deallocate(hessc)
        if (allocated(oldf)) deallocate(oldf)
        if (allocated(d)) deallocate(d)
        if (allocated(vmode)) deallocate(vmode)
        allocate (hess(nvar,nvar), bmat(nvar,nvar), u(nvar,nvar), oldhss(nvar,nvar), &
       & oldu(nvar,nvar), pmat(nvar**2), uc(nvar**2), hessc(nvar**2), oldf(nvar), &
       & d(nvar), vmode(nvar))
       else
       end if
      end subroutine setup_ef
