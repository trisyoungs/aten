      subroutine deritr() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE molkst_C, ONLY: norbs, nelecs, numcal, ndep, nvar, &
      & enuclr, keywrd, natoms
      use permanent_arrays, only : loc, geo, p, pa, coord, errfn
      USE funcon_C, only : fpc_9  
      USE chanel_C, only : iw 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  17:52:28  03/12/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use symtry_I 
      use gmetry_I 
      use hcore_I 
      use iter_I 
      implicit none
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, k, l, j
      real(double), dimension(3) :: change 
      real(double), dimension(3) :: xderiv 
      real(double), dimension(3*natoms) :: xparam
      real(double) :: delta, const, aa, xstore, ee 
      logical :: debug 

      save icalcn, debug, change, xderiv, delta 
!-----------------------------------------------
!***********************************************************************
!
!    DERITR CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
!          INTERNAL COORDINATES. THIS IS DONE BY FINITE DIFFERENCES
!          USING FULL SCF CALCULATIONS.
!
!          THIS IS VERY TIME-CONSUMING, AND SHOULD ONLY BE USED WHEN
!          NO OTHER DERIVATIVE CALCULATION WILL DO.
!
!    THE MAIN ARRAYS IN DERIV ARE:
!        LOC    INTEGER ARRAY, LOC(1,I) CONTAINS THE ADDRESS OF THE ATOM
!               INTERNAL COORDINATE LOC(2,I) IS TO BE USED IN THE
!               DERIVATIVE CALCULATION.
!        GEO    ARRAY \GEO\ HOLDS THE INTERNAL COORDINATES.
!
!***********************************************************************
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        debug = index(keywrd,'DERITR') /= 0 
!
!   DELTA IS A MACHINE-PRECISION DEPENDANT CONSTANT
!
        i = index(keywrd,' DELTA') 
        if (i /= 0) then 
          delta = reada(keywrd,i) 
        else 
          delta = 0.00001D0 
        endif 
        icalcn = numcal 
!
!   CONST = 23.06... eV to KCAL
!
        const = fpc_9 
        change(1) = delta 
        change(2) = delta 
        change(3) = delta 
!
!    CHANGE(I) IS THE STEP SIZE USED IN CALCULATING THE DERIVATIVES.
!    BECAUSE FULL SCF CALCULATIONS ARE BEING DONE QUITE LARGE STEPS
!    ARE NEEDED.  ON THE OTHER HAND, THE STEP CANNOT BE VERY LARGE,
!    AS THE SECOND DERIVATIVE IN FLEPO IS CALCULATED FROM THE
!    DIFFERENCES OF TWO FIRST DERIVATIVES. CHANGE(1) IS FOR CHANGE IN
!    BOND LENGTH, (2) FOR ANGLE, AND (3) FOR DIHEDRAL.
!
        xderiv(1) = 0.5D0/change(1) 
        xderiv(2) = 0.5D0/change(2) 
        xderiv(3) = 0.5D0/change(3) 
      endif 
      do i = 1, nvar 
        xparam(i) = geo(loc(2,i),loc(1,i)) 
      end do 
      if (ndep /= 0) call symtry 
      call gmetry (geo, coord) 
!
!  ESTABLISH THE ENERGY AT THE CURRENT POINT
!
      call hcore () 
      if (norbs*nelecs > 0) then 
        call iter (aa, .TRUE., .FALSE.) 
      else 
        aa = 0.D0 
      endif 
!
!  RESTORE THE DENSITY MATRIX (WHY?)
!
      p = pa*2.D0 
      aa = aa + enuclr 
!      IJ=0
!      DO 50 II=1,NUMAT
!         DO 40 IL=L1L,L1U
!            DO 40 JL=L2L,L2U
!               DO 40 KL=L3L,L3U
!                  DO 30 LL=1,3
!   30             XJUC(LL)=COORD(LL,II)+TVEC(LL,1)*IL+TVEC(LL,2)*JL+
!     1TVEC(LL,3)*KL
!                  IJ=IJ+1
!   40    CONTINUE
!   50 CONTINUE
      do i = 1, nvar 
        k = loc(1,i) 
        l = loc(2,i) 
        xstore = xparam(i) 
        do j = 1, nvar 
          geo(loc(2,j),loc(1,j)) = xparam(j) 
        end do 
        geo(l,k) = xstore - change(l) 
        if (ndep /= 0) call symtry 
        call gmetry (geo, coord) 
!
!   IF NEEDED, CALCULATE "EXACT" DERIVATIVES.
!
        call hcore () 
        if (norbs*nelecs > 0) then 
          call iter (ee, .TRUE., .FALSE.) 
        else 
          ee = 0.D0 
        endif 
        p = pa*2.D0 
        ee = ee + enuclr 
        errfn(i) = (aa - ee)*const*xderiv(l)*2.D0 
      end do 
      if (debug) then 
        write (iw, '('' ERROR FUNCTION'')') 
        write (iw, '(10F8.3)') (errfn(i),i=1,nvar) 
      endif 
      return  
      end subroutine deritr 
