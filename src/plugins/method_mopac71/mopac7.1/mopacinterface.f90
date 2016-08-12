!-----------------------------------------------
!   Interface to MOPAC7.1
!   Based on original mopac.f90
!   Arguments:
!       UNITNO     : Integer unit number for output file
!       OUTFILE    : Name of output file to write
!-----------------------------------------------

      logical function runmopac71(JOBBASENAME)
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use permanent_arrays, only : nfirst, nlast, nat, xparam, na, grad, nw
      USE molkst_C, only : gnorm, natoms, numat, nvar, numcal, &
      & escf, iflepo, iscf, keywrd, norbs, lm61, last, moperr, maxatoms, &
      & method_dorbs, time0, atheat, errtxt, jobnam, isok
      USE parameters_C, only : tore, ios, iop, iod, eisol, eheat
      use cosmo_C, only : lenabc, abcmat, nppa, lenab2, srad, iseps, useps, &
      & iatsp, nn, cosurf, nar, xsp, nset, bh, qden 
      USE funcon_C, only : fpc_1, fpc_2, a0, ev, fpc_5, fpc_6, fpc_7, fpc_8,&
      & fpc_9, fpc_10
      use conref_C, only : fpcref 
      USE maps_C, only : latom, react
      USE chanel_C, only : ir, iw, iarc
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use second_I  
      use initsv_I 
      use geout_I 
      use wrttxt_I 
      use geoutg_I 
      use datin_I 
      use fbx_I 
      use fordd_I 
      use calpar_I 
      use inid_I  
      use compfg_I 
      use react1_I 
      use grid_I 
      use paths_I 
      use pathk_I 
      use force_I 
      use drc_I 
      use nllsq_I 
      use powsq_I 
      use ef_I 
      use flepo_I 
      use writmo_I 
      use polar_I 
      use pmep_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, indeps,  l
      real(double) :: time00, eat,  tim 
!---TGAY ADDED 08/2016-------------------------- 
      character(*) :: JOBBASENAME
      runmopac71 = .false.
!----------------------------------------------- 

!---TGAY ADDED 08/2016-------------------------- 
      write(0,*) JOBBASENAME
      jobnam="water"
      tore = ios + iop + iod
      tore(57:71) = 3.d0
      call getdat(ir,iw)
!
!   CLOSE UNIT IW IN CASE IT WAS ALREADY PRE-ASSIGNED
!
     close(iw) 
     i = index(jobnam,' ') - 1 
     open(unit=iw, file=jobnam(:i)//'.out', status='UNKNOWN', position='asis') 
     rewind iw 
      numcal = 0 
      isok = .TRUE. 
      errtxt = 'Job stopped by operator' 
      time00 = second(1)
!
! Set up essential arrays: arrays needed for reading in the data
!
      if (natoms == 0) stop

      call setup_mopac_arrays(natoms, 1)
      maxatoms = natoms
   10 continue 
      numcal = numcal + 1 
      moperr = .FALSE. 
      gnorm = 0.D0 
      na(1) = 0 
      time0 = second(1) 
!
!    Read in all the data for the current job
!
      write(0,*) "NVARX2.5 = ", nvar

      call readmo 
      write(0,*) "NVARX3 = ", nvar

      if (natoms == 0) stop
      if (moperr) go to 10 
      if (index(keywrd,' OLDFPC') + index(keywrd, ' MNDOD') > 0) then
!
! Use old fundamental physical constants
!
!--TGAY 08/2016- Copied to target vars, and removed fpc() array.
!--------------- Subroutines that used a0, for instance, and didn't use fpc()
!--------------- would find a0 zeroed (since it was an equivalent to fpc(3)).
!--------------- Probably a compiled issue, but easily fixed.
        fpc_1  = fpcref(2,1)
        fpc_2  = fpcref(2,2)
        a0     = fpcref(2,3)
        ev     = fpcref(2,4)
        fpc_5  = fpcref(2,5)
        fpc_6  = fpcref(2,6)
        fpc_7  = fpcref(2,7)
        fpc_8  = fpcref(2,8)
        fpc_9  = fpcref(2,9)
        fpc_10 = fpcref(2,10)
      else
!
! Use CODATA fundamental physical constants
!
        fpc_1  = fpcref(1,1)
        fpc_2  = fpcref(1,2)
        a0     = fpcref(1,3)
        ev     = fpcref(1,4)
        fpc_5  = fpcref(1,5)
        fpc_6  = fpcref(1,6)
        fpc_7  = fpcref(1,7)
        fpc_8  = fpcref(1,8)
        fpc_9  = fpcref(1,9)
        fpc_10 = fpcref(1,10)
      endif  
      write(0,*) "CONSTANTS = ", a0, ev
!
! Load in parameters for the method to be used
!
      call switch (0) 
!
! Set up all the data for the molecule
!
      write(0,*) "NVARX4 = ", nvar

      call moldat (0)  
      write(0,*) "NVARX5 = ", nvar
    
      if (moperr) goto 100     
      indeps = index(keywrd,' EPS=') 
      if (index(keywrd,' 0SCF')==0 .and. indeps/=0) then 
        iseps = .TRUE. 
        lenab2 = max(min((1000 + 3*norbs - 2*natoms)**2/2,(3*norbs + 10*&
      natoms)**2/2 + 3*lenabc),123*nppa - 2) 
        allocate(abcmat(lenab2), srad(numat), iatsp(lenabc + 1), &
        & nn(3, numat), cosurf(3,lenabc), nar(lenabc), xsp(3,lenabc), &
        & nset(nppa*numat), bh(lenabc), qden(lm61))
        call initsv (indeps) 
        if (moperr) go to 100 
      else
        iseps = .FALSE. 
        useps = .FALSE. 
      endif 
      if (index(keywrd,' 0SCF') /= 0) then 
        write (iw, '(A)') ' GEOMETRY IN MOPAC Z-MATRIX FORMAT' 
        write (iarc, '(A)') ' GEOMETRY IN MOPAC Z-MATRIX FORMAT' 
        xparam(1) = -1.D0 
        call geout (iw) 
        call geout (iarc) 
        if (index(keywrd,' AIGOUT') /= 0) then 
          write (iw, '(2/,A)') '  GEOMETRY IN GAUSSIAN Z-MATRIX FORMAT' 
          call wrttxt (iw) 
          call geoutg (iw) 
          write (iarc, '(2/,A)') '  GEOMETRY IN GAUSSIAN Z-MATRIX FORMAT' 
          call wrttxt (iarc) 
          call geoutg (iarc) 
        endif 
        go to 100 
      endif 
      if (index(keywrd,' EXTERNAL') /= 0) then 
        call datin () 
        if (moperr) go to 100 
      endif 
!
! Everything is ready - now set up the arrays used by the SCF, etc.
!
      write(0,*) "NVAR1 = ", nvar

       call setup_mopac_arrays(1,2)
      write(0,*) "NVAR2 = ", nvar
      if (method_dorbs) then 
        call fbx 
        call fordd 
        call calpar 
        call inid 
        if (allocated(nw)) deallocate(nw)
        allocate(nw(numat))
        l = 1 
        do i = 1, numat 
          nw(i) = l 
          l = l + ((nlast(i)-nfirst(i)+1)*(nlast(i)-nfirst(i)+2))/2 
        end do 
      endif 
!
!  CALCULATE THE ATOMIC ENERGY
!
      write(0,*) "NVAR3 = ", nvar, a0, ev
      eat = 0.D0 
      atheat = sum(eheat(nat(:numat))) 
      eat = sum(eisol(nat(:numat))) 
      atheat = atheat - eat*fpc_9 
      if (index(keywrd,' RESTART') == 0) then 
        if (index(keywrd,' 1SCF') /= 0) then 
          if (latom /= 0) then 
            write (iw, '(2/,10X,A)') &
              '1SCF SPECIFIED WITH PATH.  THIS PAIR OF' 
            write (iw, '(   10X,A)') 'OPTIONS IS NOT ALLOWED' 
            go to 100 
          endif 
          iflepo = 1 
          iscf = 1 
          last = 1 
          i = index(keywrd,' GRAD') 
          grad(:nvar) = 0.D0 
          call compfg (xparam, .TRUE., escf, .TRUE., grad, i/=0) 
          go to 60 
        endif 
      endif 
!
! CALCULATE DYNAMIC REACTION COORDINATE.
!
!
      write(0,*) "NVAR4 = ", nvar
      if (index(keywrd,' SADDLE') /= 0) then 
        call react1 () 
        if (moperr) go to 100 
        go to 100 
      endif 
      if (index(keywrd,' STEP1') /= 0) then 
        call grid () 
        if (moperr) go to 100 
        go to 100 
      endif 

      if (latom /= 0) then 
!
!       DO PATH
!
        if (index(keywrd,' STEP')==0 .or. index(keywrd,' POINT')==0) then 
          call paths () 
          if (moperr) go to 100 
          go to 100 
        endif 
        call pathk () 
        if (moperr) go to 100 
        go to 100 
      endif 

!
! FORCE CALCULATION IF DESIRED
!
      if (index(keywrd,' FORCE')/=0 .or. index(keywrd,' IRC=')/=0 .or. &
        index(keywrd,' THERM')/=0) then 
        call force () 
        if (moperr) go to 100 
        go to 100 
      endif 

!
!   IN THIS CONTEXT, "REACT" HOLDS INITIAL VELOCITY VECTOR COMPONENTS.
!
      if (index(keywrd,' DRC') + index(keywrd,' IRC') /= 0) then 
        if (allocated(react)) deallocate(react)  !  warning - shoul this be inside drc?
        allocate(react(3*numat))
        react = 0.d0
        call drc (react, react) 
        if (moperr) go to 100 
        go to 100 
      endif 
!
      if (index(keywrd,' NLLSQ') /= 0) then 
        call nllsq () 
        if (moperr) go to 100 
        if (iflepo > 0) call compfg (xparam, .TRUE., escf, .TRUE., grad, &
          .TRUE.) 
        if (moperr) go to 100 
        go to 60 
      endif 
!
      if (index(keywrd,' SIGMA') /= 0) then 
        call powsq () 
        if (moperr) go to 100 
        go to 60 
      endif 
!
!  EF OPTIMISATION
!
      if (index(keywrd,' EF')/=0 .or. index(keywrd,' TS')/=0) then 
        if (index(keywrd,' XYZ') + index(keywrd,' GEO-OK')==0 .and. nvar>3*&
          natoms-6) then 
          write (iw, '(A)') &
            ' EIGENVECTOR FOLLOWING IS NOT RECOMMENDED WHEN' 
          write (iw, '(A)') &
            ' MORE THAN 3N-6 COORDINATES ARE TO BE OPTIMIZED' 
          write (iw, '(A)') ' TO CONTINUE, SPECIFY ''GEO-OK'''  
          go to 100 
        endif 
        call ef (xparam, nvar, escf) 
        if (moperr) go to 100 
        go to 60 
      endif 
!
! ORDINARY GEOMETRY OPTIMISATION
!
      write(0,*) "Before FLEPO"
      call flepo (xparam, nvar, escf) 
      write(0,*) "After FLEPO"
      if (moperr) go to 100 
 60     continue 
      last = 1 
      if (iflepo >= 0) then 
        call writmo
        if (moperr) go to 100 
        if (index(keywrd,' POLAR') /= 0) then 
          call polar () 
          if (moperr) go to 100 
        endif 
! PMEP: calculate the MEP and MEP charges, by Bingze Wang, Aug 1993
!
!   WARNING!  Do NOT add a space between the ' and the letters PMEP
!
        if (index(keywrd,'PMEP') /= 0) call pmep () 
        if (moperr) go to 100 
! PMEP by Bingze Wang
        if (index(keywrd,' ESP') /= 0) then 
          call esp () 
          if (moperr) go to 100 
        endif 
      endif 

  100 continue 
      tim = second(2) - time00 
      write (iw, '(3/,'' TOTAL CPU TIME: '',F16.2,'' SECONDS'')') tim 
      write (iw, '(/,'' == MOPAC DONE =='')') 
      call setup_mopac_arrays(0,0) 
      go to 10

      end function runmopac71
