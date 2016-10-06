 
      subroutine powsq() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : tleft, time0, numcal, keywrd, last, &
      &  nscf, tdump, nvar, iflepo, escf
      use funcon_C, only : a0
      use permanent_arrays, only : loc, grad, xparam, gnext1, gmin1
      use chanel_C, only : iw, ilog
!...Translated by Pacific-Sierra Research 77to90  4.4G  13:47:42  03/25/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use second_I 
      use reada_I 
   !   use powsav_I 
      use compfg_I 
      use dot_I 
      use vecprt_I 
      use rsp_I 
      use search_I 
      use prttim_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: hess(nvar,nvar) 
      real(double)  :: bmat(nvar,nvar) 
      real(double)  :: pmat(nvar*nvar) 
      real(double)  :: pvec(nvar*nvar) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(9) :: ipow 
      integer :: icyc, iloop, icalcn, maxcyc, i, ilpr, if, j, jcyc, ij, k, l, &
        il, ik, ip1, id 
      real(double), dimension(nvar) :: sig, e1, e2, p, work, eig, q 
      real(double) :: time1, time2, tlast, xinc, rho2, tol2, gmin, tstep, &
        sum, alpha, rmx, rt, rmu, sk, pmax, tprt, amin, anext 
      logical :: debug, restrt, times, scf1, resfil, log 
      character :: txt 

      save debug, restrt, times, scf1, resfil, log, time1, time2, icyc, tlast, &
        iloop, xinc, rho2, tol2, icalcn 
!-----------------------------------------------
!*********************************************************************
!
!   POWSQ OPTIMIZES THE GEOMETRY BY MINIMISING THE GRADIENT NORM.
!         THUS BOTH GROUND AND TRANSITION STATE GEOMETRIES CAN BE
!         CALCULATED. IT IS ROUGHLY EQUIVALENT TO FLEPO, FLEPO MINIMIZES
!         THE ENERGY, POWSQ MINIMIZES THE GRADIENT NORM.
!
!  ON ENTRY XPARAM = VALUES OF PARAMETERS TO BE OPTIMIZED.
!           NVAR   = NUMBER OF PARAMETERS TO BE OPTIMIZED.
!
!  ON EXIT  XPARAM = OPTIMIZED PARAMETERS.
!           escf  = HEAT OF FORMATION IN KCALS.
!
!*********************************************************************
!        *****  ROUTINE PERFORMS  A LEAST SQUARES MINIMIZATION  *****
!        *****  OF A FUNCTION WHICH IS A SUM OF SQUARES.        *****
!        *****  INITIALLY WRITTEN BY J.W. MCIVER JR. AT SUNY/   *****
!        *****  BUFFALO, SUMMER 1971.  REWRITTEN AND MODIFIED   *****
!        *****  BY A.K. AT SUNY BUFFALO AND THE UNIVERSITY OF   *****
!        *****  TEXAS.  DECEMBER 1973                           *****
!
      data icalcn/ 0/  
      tleft = tleft - second(1) + time0 
      if (icalcn /= numcal) then 
        icalcn = numcal 
        if (allocated(gnext1)) deallocate(gnext1)
        if (allocated(gmin1)) deallocate(gmin1)
        allocate (gnext1(nvar), gmin1(nvar))
        restrt = index(keywrd,' RESTART') /= 0 
        maxcyc = 100000 
        if (index(keywrd,' CYCLES') /= 0) maxcyc = nint(reada(keywrd,index(&
          keywrd,' CYCLES'))) 
        log = index(keywrd,' NOLOG') == 0 
        scf1 = index(keywrd,' 1SCF') /= 0 
        time1 = second(2) 
        time2 = time1 
        icyc = 0 
        times = index(keywrd,' TIME') /= 0 
        tlast = tleft 
        resfil = .FALSE. 
        last = 0 
        iloop = 1 
        xinc = a0*0.01D0 
        rho2 = 1.D-4 
        tol2 = 4.D-1 
        if (index(keywrd,' PREC') /= 0) tol2 = 1.D-2 
        if (index(keywrd,' GNORM') /= 0) then 
          tol2 = reada(keywrd,index(keywrd,' GNORM')) 
          if (tol2<0.01D0 .and. index(keywrd,' LET')==0) then 
            write (iw, '(/,A)') '  GNORM HAS BEEN SET TOO LOW, RESET TO 0.01' 
            tol2 = 0.01D0 
          endif 
        endif 
        debug = index(keywrd,' POWSQ') /= 0 
        if (restrt) then 
!
!   RESTORE STORED DATA
!
          ipow(9) = 0 
          hess = 0.d0
          pmat = 0.d0
          bmat = 0.d0
          call powsav (hess, gmin1, xparam, pmat, iloop, bmat, ipow) 
          icyc = ipow(3) 
          if (scf1) go to 390 
          nscf = ipow(8) 
          grad(:nvar) = gmin1(:nvar) 
          gnext1(:nvar) = gmin1(:nvar) 
          write (iw, '('' XPARAM'',6F10.6)') (xparam(i),i=1,nvar) 
          if (iloop > 0) then 
!#               ILOOP=ILOOP+1
            write (iw, '(2/10X,'' RESTARTING AT POINT'',I3)') iloop 
          else 
            write (iw, &
      '(2/10X,''RESTARTING IN OPTIMISATION'',                  '' ROUTINES'')') 
          endif 
        endif 
!
!   DEFINITIONS:   NVAR   = NUMBER OF GEOMETRIC VARIABLES = 3*NUMAT-6
!
      endif 
      nvar = abs(nvar) 
      if (debug) then 
        write (iw, '('' XPARAM'')') 
        write (iw, '(5(2I3,F10.4))') (loc(1,i),loc(2,i),xparam(i),i=1,nvar) 
      endif 
      if (.not.restrt) then 
        grad(:nvar) = 0.D0 
        call compfg (xparam, .TRUE., escf, .TRUE., grad, .TRUE.) 
      endif 
      if (debug) then 
        write (iw, '('' STARTING GRADIENTS'')') 
        write (iw, '(3X,8F9.4)') (grad(i),i=1,nvar) 
      endif 
      gmin = sqrt(dot(grad,grad,nvar)) 
      gnext1 = grad 
      gmin1 = gnext1 
!
!    NOW TO CALCULATE THE HESSIAN MATRIX.
!
      if (iloop < 0) go to 140 
!
!   CHECK THAT HESSIAN HAS NOT ALREADY BEEN CALCULATED.
!
      ilpr = iloop 
      do iloop = ilpr, nvar 
        time1 = second(1) 
        xparam(iloop) = xparam(iloop) + xinc 
        call compfg (xparam, .TRUE., escf, .TRUE., grad, .TRUE.) 
        if (scf1) go to 390 
        if (debug) write (iw, '(I3,12(8F9.4,/3X))') iloop, (grad(if),if=1,nvar) 
        grad(iloop) = grad(iloop) + 1.D-5 
        xparam(iloop) = xparam(iloop) - xinc 
        hess(iloop,:nvar) = -(grad(:nvar)-gnext1(:nvar))/xinc 
        time2 = second(2) 
        tstep = time2 - time1 
        if (times) write (iw, '('' TIME FOR STEP:'',F8.3,'' LEFT'',F8.3)') &
          tstep, tleft 
        if (tlast - tleft > tdump) then 
          tlast = tleft 
          resfil = .TRUE. 
          ipow(9) = 2 
          i = iloop 
          ipow(3) = icyc 
          ipow(8) = nscf 
          call powsav (hess, gmin1, xparam, pmat, i, bmat, ipow) 
        endif 
        if (tleft>=tstep*2.D0 .and. iloop-ilpr<=maxcyc) cycle  
!
!  STORE RESULTS TO DATE.
!
        ipow(9) = 1 
        i = iloop 
        ipow(8) = nscf 
        ipow(3) = icyc 
        call powsav (hess, gmin1, xparam, pmat, i, bmat, ipow) 
        iflepo = -1 
        return  
      end do 
!        *****  SCALE -HESSIAN- MATRIX                           *****
      if (debug) then 
        write (iw, '(2/10X,''UN-NORMALIZED HESSIAN MATRIX'')') 
        do i = 1, nvar 
          write (iw, '(8F10.4)') (hess(j,i),j=1,nvar) 
        end do 
      endif 
      do i = 1, nvar 
        sum = 0.0D0 
        do j = 1, nvar 
          sum = sum + hess(i,j)**2 
        end do 
        work(i) = 1.0D0/sqrt(sum) 
      end do 
      do i = 1, nvar 
        hess(i,:nvar) = hess(i,:nvar)*work(i) 
      end do 
      if (debug) then 
        write (iw, '(2/10X,''HESSIAN MATRIX'')') 
        do i = 1, nvar 
          write (iw, '(8F10.4)') (hess(j,i),j=1,nvar) 
        end do 
      endif 
!        *****  INITIALIZE B MATIRX                        *****
      do i = 1, nvar 
        bmat(i,:nvar) = 0.0D0 
        bmat(i,i) = work(i)*2.D0 
      end do 
!***********************************************************************
!
!  THIS IS THE START OF THE BIG LOOP TO OPTIMIZE THE GEOMETRY
!
!***********************************************************************
      iloop = -99 
      tstep = tstep*4.D0 
      jcyc = icyc 
  130 continue 
      if (tlast - tleft > tdump) then 
        tlast = tleft 
        resfil = .TRUE. 
        ipow(9) = 2 
        i = iloop 
        ipow(8) = nscf 
        ipow(3) = icyc 
        call powsav (hess, gmin1, xparam, pmat, i, bmat, ipow) 
      endif 
      if (tleft<tstep*2.D0 .or. icyc-jcyc>maxcyc) then 
!
!  STORE RESULTS TO DATE.
!
        ipow(9) = 1 
        i = iloop 
        ipow(8) = nscf 
        ipow(3) = icyc 
        call powsav (hess, gmin1, xparam, pmat, i, bmat, ipow) 
        iflepo = -1 
        return  
      endif 
  140 continue 
      ij = 0 
      do j = 1, nvar 
        do i = 1, j 
          ij = ij + 1 
          sum = 0.0D0 
          do k = 1, nvar 
            sum = sum + hess(i,k)*hess(j,k) 
          end do 
          pmat(ij) = sum 
        end do 
      end do 
      do i = 1, nvar 
        sum = 0.0D0 
        do k = 1, nvar 
          sum = sum - hess(i,k)*gmin1(k) 
        end do 
        p(i) = -sum 
      end do 
      l = 0 
      if (debug) then 
        write (iw, '(/10X,''P MATRIX IN POWSQ'')') 
        call vecprt (pmat, nvar) 
      endif 
      call rsp (pmat, nvar, nvar, eig, pvec) 
!        *****  CHECK FOR ZERO EIGENVALUE                  *****
!#      WRITE(IW,'(''  EIGS IN POWSQ:'')')
!#      WRITE(IW,'(6F13.8)')(EIG(I),I=1,NVAR)
      if (eig(1) >= rho2) then 
!        *****  IF MATRIX IS NOT SINGULAR FORM INVERSE     *****
!        *****  BY BACK TRANSFORMING THE EIGENVECTORS      *****
        ij = 0 
        do i = 1, nvar 
          do j = 1, i 
            ij = ij + 1 
            sum = 0.0D0 
            do k = 1, nvar 
              sum = sum + pvec((k-1)*nvar+j)*pvec((k-1)*nvar+i)/eig(k) 
            end do 
            pmat(ij) = sum 
          end do 
        end do 
!        *****  FIND -Q- VECTOR                            *****
        l = 0 
        il = l + 1 
        l = il + i - 1 
        do i = 1, nvar 
          sum = 0.0D0 
          do k = 1, i 
            ik = (i*(i - 1))/2 + k 
            sum = sum + pmat(ik)*p(k) 
          end do 
          ip1 = i + 1 
          do k = ip1, nvar 
            ik = (k*(k - 1))/2 + i 
            sum = sum + pmat(ik)*p(k) 
          end do 
          q(i) = sum 
        end do 
      else 
        q(:nvar) = pvec(:nvar) 
      endif 
      do i = 1, nvar 
        sig(i) = 0.0D0 
        do j = 1, nvar 
          sig(i) = sig(i) + q(j)*bmat(i,j) 
        end do 
      end do 
!        *****  DO A ONE DIMENSIONAL SEARCH                *****
      if (debug) then 
        write (iw, '('' SEARCH VECTOR'')') 
        write (iw, '(8F10.5)') (sig(i),i=1,nvar) 
      endif 
      call search (xparam, alpha, sig, nvar, gmin, escf, amin, anext) 
      if (nvar == 1) go to 390 
!
!  FIRST WE ATTEMPT TO OPTIMIZE GEOMETRY USING SEARCH.
!  IF THIS DOES NOT WORK, THEN SWITCH TO LINMIN, WHICH ALWAYS WORKS,
!  BUT IS TWICE AS SLOW AS SEARCH.
!
      rmx = 0.0D0 
      do k = 1, nvar 
        rt = abs(gmin1(k)) 
        rmx = dmax1(rt,rmx) 
      end do 
      if (rmx < tol2) go to 390 
!        *****  TWO STEP ESTIMATION OF DERIVATIVES         *****
      e1(:nvar) = (gmin1(:nvar)-gnext1(:nvar))/(amin - anext) 
      rmu = dot(e1,gmin1,nvar)/dot(gmin1,gmin1,nvar) 
      e2(:nvar) = e1(:nvar) - rmu*gmin1(:nvar) 
!        *****  SCALE -E2- AND -SIG-                       *****
      sk = 1.0D0/sqrt(dot(e2,e2,nvar)) 
      sig(:nvar) = sk*sig(:nvar) 
      e2(:nvar) = sk*e2(:nvar) 
!        *****  FIND INDEX OF REPLACEMENT DIRECTION        *****
      pmax = -1.0D+20 
      do i = 1, nvar 
        if (abs(p(i)*q(i)) <= pmax) cycle  
        pmax = abs(p(i)*q(i)) 
        id = i 
      end do 
!        *****  REPLACE APPROPRIATE DIRECTION AND DERIVATIVE ***
      hess(id,:nvar) = -e2(:nvar) 
!        *****  REPLACE STARTING POINT                     *****
      bmat(:nvar,id) = sig(:nvar)/a0 
      gnext1(:nvar) = gmin1(:nvar) 
      time1 = time2 
      time2 = second(2) 
      tstep = time2 - time1 
      tleft = tleft - tstep 
      if (tleft < 0.0D0) tleft = -0.1D0 
      icyc = icyc + 1 
      call prttim (tleft, tprt, txt) 
      if (resfil) then 
        write (iw, 370) tprt, txt, min(gmin,999999.999D0), escf 
        if (log) write (ilog, 370) tprt, txt, min(gmin,999999.999D0), escf 
  370   format('  RESTART FILE WRITTEN,   TIME LEFT:',f6.2,a1,'  GRAD.:',f10.3,&
          ' HEAT:',g14.7) 
        resfil = .FALSE. 
      else 
        write (iw, 380) icyc, min(tstep,9999.99D0), tprt, txt, min(gmin,&
          999999.999D0), escf 
        if (log) write (ilog, 380) icyc, min(tstep,9999.99D0), tprt, txt, min(&
          gmin,999999.999D0), escf 
  380   format(' CYCLE:',i4,' TIME:',f8.3,' TIME LEFT:',f6.2,a1,'  GRAD.:',&
          f10.3,' HEAT:',g14.7) 
      endif 
      endfile (iw) 
      backspace (iw) 
      if (log) then 
        endfile (ilog) 
        backspace (ilog) 
      endif 
      if (times) write (iw, '('' TIME FOR STEP:'',F8.3,'' LEFT'',F8.3)') tstep&
        , tleft 
      go to 130 
  390 continue 
      grad(:nvar) = 0.D0 
      last = 1 
      call compfg (xparam, .TRUE., escf, .TRUE., grad, .TRUE.) 
      grad(:nvar) = gmin1(:nvar) 
      iflepo = 11 
      if (scf1) iflepo = 13 
      return  
      end subroutine powsq 
      subroutine search(xparam, alpha, sig, nvar, gmin, funct, amin, anext ) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE permanent_arrays, only : gmin1, gnext1
      use chanel_C, only : iw
      use molkst_C, only : numcal, keywrd
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:02  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dot_I 
      use compfg_I   
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: nvar 
      real(double) , intent(inout) :: alpha 
      real(double) , intent(inout) :: gmin 
      real(double)  :: funct, amin, anext
      real(double)  :: xparam(*) 
      real(double) , intent(in) :: sig(*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: looks, icalcn, i, itrys 
      real(double), dimension(nvar) :: grad, xref, gref, xmin1 
      real(double) :: g, tiny, tolerg, gb, gstore, gminn, ta, tb, ga, sum, gtot 
      logical :: debug 

      save debug, g, tiny, looks, tolerg, icalcn 
!-----------------------------------------------
!***********************************************************************
!
! SEARCH PERFORMS A LINE SEARCH FOR POWSQ. IT MINIMIZES THE NORM OF
!        THE GRADIENT VECTOR IN THE DIRECTION SIG.
!
! ON INPUT  XPARAM = CURRENT POINT IN NVAR DIMENSIONAL SPACE.
!           ALPHA  = STEP SIZE (IN FACT ALPHA IS CALCULATED IN SEARCH).
!           SIG    = SEARCH DIRECTION VECTOR.
!           NVAR   = NUMBER OF PARAMETERS IN SIG (& XPARAM)
!
! ON OUTPUT XPARAM = PARAMETERS OF MINIMUM.
!           ALPHA  = DISTANCE TO MINIMUM.
!           GMIN   = GRADIENT NORM AT MINIMUM.
!***********************************************************************
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
!
!    TOLG   = CRITERION FOR EXIT BY RELATIVE CHANGE IN GRADIENT.
!
        debug = index(keywrd,'LINMIN') /= 0 
        looks = 0  
        tiny = 0.1D0 
        tolerg = 0.02D0 
        g = 100.D0 
        alpha = 0.1D0 
      endif 
      gref(:nvar) = gmin1(:nvar) 
      gnext1(:nvar) = gmin1(:nvar) 
      xmin1(:nvar) = xparam(:nvar) 
      xref(:nvar) = xparam(:nvar) 
      if (abs(alpha) > 0.2D0) alpha = sign(0.2D0,alpha) 
      if (debug) then 
        write (iw, '('' SEARCH DIRECTION VECTOR'')') 
        write (iw, '(6F12.6)') (sig(i),i=1,nvar) 
        write (iw, '('' INITIAL GRADIENT VECTOR'')') 
        write (iw, '(6F12.6)') (gmin1(i),i=1,nvar) 
      endif 
      gb = dot(gmin1,gref,nvar) 
      if (debug) write (iw, '('' GRADIENT AT START OF SEARCH:'',F16.6)') sqrt(&
        gb) 
      gstore = gb 
      amin = 0.D0 
      gminn = 1.D9 
      ta = 0.D0 
      tb = 0.D0 
      ga = gb 
      gb = 1.D9 
      itrys = 0 
      go to 30 
   20 continue 
      sum = ga/(ga - gb) 
      itrys = itrys + 1 
      if (abs(sum) > 3.D0) sum = sign(3.D0,sum) 
      alpha = (tb - ta)*sum + ta 
!
!         XPARAM IS THE GEOMETRY OF THE PREDICTED MINIMUM ALONG THE LINE
!
   30 continue 
      xparam(:nvar) = xref(:nvar) + alpha*sig(:nvar) 
!
!         CALCULATE GRADIENT NORM AND GRADIENTS AT THE PREDICTED MINIMUM
!
      if (itrys == 1) then 
        grad(:nvar) = 0.D0 
      endif 
      call compfg (xparam, .TRUE., funct, .TRUE., grad, .TRUE.) 
      looks = looks + 1 
!
!          G IS THE PROJECTION OF THE GRADIENT ALONG SIG.
!
      g = dot(gref,grad,nvar) 
      gtot = sqrt(dot(grad,grad,nvar)) 
      if (debug) write (iw, &
      & '('' LOOKS'',I3,'' ALPHA ='',F12.6,'' GRADIENT'',F12.3,  '' G  ='',F16.6)&
      & ') looks, alpha, sqrt(dot(grad,grad,nvar)), g 
      if (gtot < gminn) then 
        gminn = gtot 
        if (abs(amin - alpha) > 1.D-2) then 
!
! WE CAN MOVE ANEXT TO A POINT NEAR, BUT NOT TOO NEAR, AMIN, SO THAT THE
! SECOND DERIVATIVESWILLBEREALISTIC(D2E/DX2=(GNEXT1-GMIN1)/(ANEXT-AMIN))
!
          anext = amin 
          gnext1(:nvar) = gmin1(:nvar) 
        endif 
        amin = alpha 
        if (gminn < gmin) then 
          xmin1(:nvar) = xparam(:nvar) 
          gmin1(:nvar) = grad(:nvar) 
        else 
          gmin1(:nvar) = grad(:nvar) 
        endif 
        gmin = min(gminn,gmin) 
      endif 
      if (itrys > 8) go to 80 
      if (abs(g/gstore)<tiny .or. abs(g)<tolerg) go to 80 
      if (abs(g)<max(abs(ga),abs(gb)) .or. ga*gb>0.D0 .and. g*ga<0.D0) then 
!
!   G IS AN IMPROVEMENT ON GA OR GB.
!
        if (abs(gb) < abs(ga)) then 
          ta = alpha 
          ga = g 
          go to 20 
        else 
          tb = alpha 
          gb = g 
          go to 20 
        endif 
      else 
!#         WRITE(IW,'(//10X,'' FAILED IN SEARCH, SEARCH CONTINUING'')')
        go to 80 
      endif 
   80 continue 
      gminn = sqrt(dot(gmin1,gmin1,nvar)) 
      xparam(:nvar) = xmin1(:nvar) 
      if (debug) then 
        write (iw, '('' AT EXIT FROM SEARCH'')') 
        write (iw, '('' XPARAM'',6F12.6)') (xparam(i),i=1,nvar) 
        write (iw, '('' GNEXT1'',6F12.6)') (gnext1(i),i=1,nvar) 
        write (iw, '('' GMIN1 '',6F12.6)') (gmin1(i),i=1,nvar) 
        write (iw, '('' AMIN, ANEXT,GMIN'',4F12.6)') amin, anext, gmin 
      endif 
      if (gminn > gmin) then 
        xparam(:nvar) = xref(:nvar) 
      endif 
      return  
!
      end subroutine search 

 
