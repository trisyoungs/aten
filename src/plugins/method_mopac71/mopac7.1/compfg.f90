      subroutine compfg(xparam, int, escf, fulscf, grad, lgrad) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE parameters_C, only : 
      USE funcon_C, only : fpc_9  
      USE chanel_C, only : iw, irot 
      USE molmec_C, only : nnhco, nhco, htype 
      use permanent_arrays, only : nat, loc, geo, &
      & coord, xparef, aicorr
      USE molkst_C, ONLY: numat, norbs, nelecs, nclose, nopen, fract, natoms, numcal, &
      & ndep, nvar, mpack, elect, enuclr, keywrd, moperr, emin, method_dorbs, &
      atheat 
      use analyt_C, only : ltype, mtype, nztype
      use cosmo_C, only : iseps, useps
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:18:48  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      use setupg_I 
      use symtry_I 
      use gmetry_I 
      use timer_I 
      use prtpar_I 
      use hcored_I 
      use prthco_I 
      use hcore_I
      use iter_I 
      use dihed_I 
      use consts_I 
      use btoc_I 
      use deriv_I 
      use mecip_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(out) :: escf 
      logical , intent(in) :: int 
      logical, intent(in)  :: fulscf 
      logical , intent(in) :: lgrad 
      real(double) , intent(in) :: xparam(nvar) 
      real(double)  :: grad(nvar) 
 !-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, j, k, l
      real(double), dimension(3) :: degree 
      real(double) :: angle
      logical :: debug, print, analyt, large, usedci, force, times, aider

      save debug, print, analyt, large, usedci, force, times, aider, degree, &
        icalcn 
!***********************************************************************
!
!   COMPFG CALCULATES (A) THE HEAT OF FORMATION OF THE SYSTEM, AND
!                     (B) THE GRADIENTS, IF LGRAD IS .TRUE.
!
!   ON INPUT  XPARAM = ARRAY OF PARAMETERS TO BE USED IN INTERNAL COORDS
!             LGRAD  = .TRUE. IF GRADIENTS ARE NEEDED, .FALSE. OTHERWISE
!             INT    = .TRUE. IF HEAT OF FORMATION IS TO BE CALCULATED
!             FULSCF = .TRUE. IF FULL SCF TO BE DONE, .FALSE. OTHERWISE.
!
!   ON OUTPUT ESCF  = HEAT OF FORMATION.
!             GRAD   = ARRAY OF GRADIENTS, IF LGRAD = .TRUE.
!
!***********************************************************************
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
        ltype = 0 
        mpack = (norbs*(norbs+1))/2
        do i = 1, numat 
          if (nat(i) >= 99) cycle  
          do j = 1, ltype 
            if (nat(i) == mtype(j)) go to 20 
          end do 
          ltype = ltype + 1 
          if (ltype > 30) then 
            write (iw, *) ' LTYPE is too large in COMPFG. ', ltype 
            call mopend ('LTYPE is too large in COMPFG') 
            return  
          endif 
          mtype(ltype) = nat(i) 
          nztype(nat(i)) = ltype 
!
!       LTYPE = NUMBER OF TYPES OF REAL ATOM PRESENT
!       MTYPE = TYPES OF REAL ATOMS PRESENT
          j = ltype 
   20     continue 
        end do 
        aider = index(keywrd,'AIDER') /= 0 
        times = index(keywrd,'TIMES') /= 0 
        analyt = index(keywrd,'ANALYT') /= 0 
        if (int .and. analyt) call setupg 
        degree(1) = 1.D0 
        if (index(keywrd,' XYZ') + index(keywrd,'VELO') /= 0) then 
          degree(2) = 1.D0 
        else 
          degree(2) = 57.29577951308232D0 
        endif 
        degree(3) = degree(2) 
        usedci = nclose/=nopen .and. Abs(fract - 2.d0) > 1.d-20 .and. fract > 1.d-20 .or. index(&
          keywrd,'C.I.')/=0 
        force = index(keywrd,'FORCE') /= 0 
        large = index(keywrd,'LARGE') /= 0 
        print = index(keywrd,'COMPFG') /= 0 
        debug = index(keywrd,'DEBUG')/=0 .and. print 
        emin = 0.D0 
        xparef(:nvar) = xparam(:nvar) 
      endif 
!
! SET UP COORDINATES FOR CURRENT CALCULATION
!
!       PLACE THE NEW VALUES OF THE VARIABLES IN THE ARRAY GEO.
!       MAKE CHANGES IN THE GEOMETRY.
      write(0,*) "COMPFG 01"
      do i = 1, nvar 
        k = loc(1,i) 
        l = loc(2,i) 
        geo(l,k) = xparam(i) 
      end do 
!      IMPOSE THE SYMMETRY CONDITIONS + COMPUTE THE DEPENDENT-PARAMETERS
      if (ndep /= 0) call symtry 
!      NOW COMPUTE THE ATOMIC COORDINATES.
      if (debug) then 
        if (large) then 
          k = natoms 
        else 
          k = min(5,natoms) 
        endif 
        write (iw, fmt='('' INTERNAL COORDS'',/100(/,3F12.6))') ((geo(j,i)*&
          degree(j),j=1,3),i=1,k) 
      endif 
      write(0,*) "COMPFG 02"
      call gmetry (geo, coord) 
      if (debug) then 
        if (large) then 
          k = numat 
        else 
          k = min(5,numat) 
        endif 
        write (iw, fmt='('' CARTESIAN COORDS'',/100(/,3F16.9))') ((coord(j,i),j&
          =1,3),i=1,k) 
      endif 
      if (int .and. analyt) rewind irot
      if (.not.useps) then 
        if (index(keywrd,' HCORE') /= 0) call prtpar 
        if (times) call timer ('BEFORE HCORE') 
        if (method_dorbs) then        
          if (int) call hcored () 
          if (index(keywrd,' HCORE') /= 0) call prthco () 
        else 
          if (int) call hcore () 
        endif 
        if (times) call timer ('AFTER HCORE') 
!
! COMPUTE THE HEAT OF FORMATION.
!
        if (norbs>0 .and. nelecs>0) then 
          if (times) call timer ('BEFORE ITER') 
          write(0,*) "Calling ITER..."
          if (int) call iter (elect, fulscf, .TRUE.) 
          if (moperr) return  
          if (times) call timer ('AFTER ITER') 
        else 
          elect = 0.D0 
        endif
        write(0,*) "ESCF0 = ", elect, enuclr, fpc_9, atheat

        escf = (elect + enuclr)*fpc_9 + atheat 
        write(0,*) "ESCF1 = ", escf
        if (escf<emin .or. emin==0.D0) emin = escf 
        write(0,*) "ESCF2 = ", escf
        do i = 1, nnhco 
          call dihed (coord, nhco(1,i), nhco(2,i), nhco(3,i), nhco(4,i), angle) 
          escf = escf + htype*sin(angle)**2 

        end do 
      endif 
      if (iseps) then 
! The following routine constructs the dielectric screening surface
        call consts () 
        if (moperr) return  
! The following routine constructs dielectric response matrix CCMAT
        call btoc () 
        useps = .TRUE. 
        if (times) call timer ('BEFORE HCORE') 
        if (int) then 
          if (method_dorbs) then 
            if (index(keywrd,' HCORE') /= 0) call prtpar 
            if (int) call hcored () 
            if (index(keywrd,' HCORE') /= 0) call prthco () 
          else 
            call hcore () 
          endif 
        endif 
        if (times) call timer ('AFTER HCORE') 
!
! COMPUTE THE HEAT OF FORMATION.
!
        if (norbs>0 .and. nelecs>0) then 
          if (times) call timer ('BEFORE ITER') 
          if (int) call iter (elect, fulscf, .TRUE.) 
          if (moperr) return  
          if (times) call timer ('AFTER ITER') 
        else 
          elect = 0.D0 
        endif 
        escf = (elect + enuclr)*fpc_9 + atheat 
        if (escf<emin .or. emin==0.D0) emin = escf 
        do i = 1, nnhco 
          call dihed (coord, nhco(1,i), nhco(2,i), nhco(3,i), nhco(4,i), angle) 
          escf = escf + htype*sin(angle)**2 
        end do 
      endif 
!
! FIND DERIVATIVES IF DESIRED
!
      if (lgrad) then 
        if (times) call timer ('BEFORE DERIV') 
      write(0,*) "Before DERIV"
        if (nelecs > 0) call deriv (geo, grad) 
      write(0,*) "After DERIV"
        if (moperr) return  
        if (times) call timer ('AFTER DERIV') 
      endif 
      if (aider) then 
!
!  ADD IN AB INITIO CORRECTION
!
        escf = escf + dot_product(xparam(:nvar)-xparef(:nvar),aicorr(:nvar)) 
      endif 
      if (int .and. print) write (iw, '(/10X,'' HEAT OF FORMATION'',G30.17)') &
        escf 
      if (print .and. lgrad) write (iw, fmt=&
        '('' GRADIENT       '',8F8.2,(/10F8.2))') (grad(i),i=1,nvar) 
!
! REFORM DENSITY MATRIX, IF A C.I. DONE AND EITHER THE LAST SCF OR A
! FORCE CALCULATION
!
      if (usedci .and. force) call mecip () 
      return  
      end subroutine compfg 
