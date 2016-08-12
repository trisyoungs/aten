      subroutine writmo
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
    
!...Translated by Pacific-Sierra Research 77to90  4.4G  18:02:55  03/16/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use cosmo_C, only : iseps, area
      use molkst_C, only : numat, nclose, nopen, fract, nalpha, nelecs, nbeta, &
      & norbs, nvar, gnorm, iflepo, enuclr,elect, ndep, nscf, numcal, escf, &
      & keywrd, na1, verson, time0, moperr, last, iscf, jobnam
      use permanent_arrays, only: loc, geo, pa, pb, na, eigs, eigb, coord, &
      & dxyz, p, grad, labels, xparam, nat, nfirst, nlast, cb, fb, &
      & h, c, f, atmass, w
      use elemts_C, only : elemnt
      use maps_C, only : latom, lparam
      use meci_C, only : rjkab
      use parameters_C, only : tore
      use symmetry_C, only : name, state
      use euler_C, only : tvec, id
!--TGAY 08/2016 - fpc_9 was unused
      use funcon_C, only : fpc_10   ! fpc_9
      use chanel_C, only : iw, iarc, iden, ibrz
!-----------------------------------------------
      use meci_I 
   !   use fdate_I 
      use dot_I 
      use wrttxt_I 
      use geout_I 
      use mopend_I 
      use symtrz_I 
      use dielen_I 
      use deriv_I 
      use gmetry_I 
      use dimens_I 
      use volume_I 
      use second_I 
      use timout_I 
      use symtry_I 
      use vecprt_I 
      use matou1_I 
      use superd_I 
      use mecip_I 
      use chrge_I 
      use dipole_I 
      use reada_I 
      use brlzon_I 
      use denrot_I 
      use molval_I 
      use bonds_I 
      use local_I 
      use enpart_I 
      use mullik_I 
      use mpcpop_I 
   !   use greenf_I 
      use geoutg_I 
      implicit none
!-------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      real(double) ::  rxyz((numat*(numat + 1))/2),  gcoord(3,numat)




      integer , dimension(107) :: iel1, nelemt, iel2 
      integer , dimension(3) :: mers 
      integer :: icalcn, i, inam, jnam, jend, iend, iuhf, &
        loc11, loc21, nopn, j, k, l, kchrge, &
        mono3, igo, ichfor&
        , nzs, kfrst, ksec, iwrite, mvar
      real(double), dimension(99) :: q, q2 
      real(double) :: degree, edie, &
        xreact, eionis, sumw, vol, tim, xi, sum, dip, sz, ss2, &
        dumy(3), step
      logical :: uhf, ci, prtgra, still, bcc, exists, opend 
      character , dimension(3) :: type*11 
      character :: idate*24 
      character , dimension(2) :: calcn*5 
      character :: gtype*13, grtype*14 
      character , dimension(17) :: flepo*58 
      character , dimension(2) :: iter*58 
      character , dimension(11) :: numbrs 
      character , dimension(20) :: ielemt*2 
      character :: caltyp*7, namfil*80 
      character, dimension(3) :: staten*4 

      save type, calcn, flepo, iter, numbrs, namfil, icalcn, i, inam, jnam, &
        jend, iend 
!-----------------------------------------------
!***********************************************************************
!
!   WRITMO PRINTS OUT MOST OF THE RESULTS.
!         IT SHOULD NOT ALTER ANY PARAMETERS, SO THAT IT CAN BE CALLED
!         AT ANY CONVENIENT TIME.
!
!***********************************************************************
      data icalcn/ 0/   
      data calcn/ '     ', 'ALPHA'/  
      data numbrs/ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ' '/  
      data flepo(1), flepo(2), flepo(3)/ &
        ' 1SCF WAS SPECIFIED, SO BFGS WAS NOT USED                 ', &
        ' GRADIENTS WERE INITIALLY ACCEPTABLY SMALL                ', &
        ' HERBERTS TEST WAS SATISFIED IN BFGS                      '/  
      data flepo(4), flepo(5), flepo(6)/ &
        ' THE LINE MINIMIZATION FAILED TWICE IN A ROW.   TAKE CARE!', &
        ' BFGS FAILED DUE TO COUNTS EXCEEDED. TAKE CARE!           ', &
        ' PETERS TEST WAS SATISFIED IN BFGS OPTIMIZATION           '/  
      data flepo(7), flepo(8), flepo(9)/ &
        ' THIS MESSAGE SHOULD NEVER APPEAR, CONSULT A PROGRAMMER!! ', &
        ' GRADIENT TEST NOT PASSED, BUT FURTHER WORK NOT JUSTIFIED ', &
        ' A FAILURE HAS OCCURRED, TREAT RESULTS WITH CAUTION!!     '/  
      data flepo(10), flepo(11), flepo(12)/ &
        ' GEOMETRY OPTIMIZED USING NLLSQ. GRADIENT NORM MINIMIZED  ', &
        ' GEOMETRY OPTIMIZED USING POWSQ. GRADIENT NORM MINIMIZED  ', &
        ' CYCLES EXCEEDED, GRADIENT NOT FULLY MINIMIZED IN NLLSQ   '/  
      data flepo(13), flepo(14), flepo(15)/ &
        ' 1SCF RUN AFTER RESTART.  GEOMETRY MIGHT NOT BE OPTIMIZED ', &
        ' HEAT OF FORMATION MINIMIZED IN ONE LINE SEARCH           ', &
        ' GEOMETRY OPTIMISED USING EIGENVECTOR FOLLOWING (EF).     '/  
      data flepo(16), flepo(17)/ &
        ' EF-OPTIMIZED GEOMETRY.  NUMBER OF -VE ROOTS INCORRECT    ', &
        ' GEOMETRY OPTIMISED USING EIGENVECTOR FOLLOWING (TS).     '/  
      data iter/ ' SCF FIELD WAS ACHIEVED                                   ', &
        '  ++++----**** FAILED TO ACHIEVE SCF. ****----++++        '/   
!
! SUMMARY OF RESULTS (NOTE: THIS IS IN A SUBROUTINE SO IT
!          CAN BE USED BY THE PATH OPTION)
      if (icalcn == 0) namfil = '**NULL**' 
      idate = ' ' 
      if (iflepo==15 .and. index(keywrd,' TS')/=0) iflepo = 17 
      if (iflepo == 0) iflepo = 7 
      iuhf = min(index(keywrd,' UHF'),1) + 1 
      prtgra = index(keywrd,' GRAD')/=0 .and. nvar>0 
      ci = index(keywrd,' C.I.') /= 0 
      if (index(keywrd,' AM1') /= 0) then 
        caltyp = '  AM1  ' 
      else if (index(keywrd,' PM3') /= 0) then 
        caltyp = '  PM3  ' 
      else if (index(keywrd,' MNDOD') /= 0) then 
        caltyp = ' MNDO-D' 
      else 
        caltyp = ' MNDO  ' 
      endif 
      uhf = iuhf == 2 
      call fdate (idate) 
      degree = 57.29577951308232D0 
      if (na1 == 99) then 
        degree = 1.D0 
        type(1) = 'CARTESIAN X' 
        type(2) = 'CARTESIAN Y' 
        type(3) = 'CARTESIAN Z' 
      else 
        type(1) = 'BOND       ' 
        type(2) = 'ANGLE      ' 
        type(3) = 'DIHEDRAL   ' 
      endif 
      gnorm = 0.D0 
      if (nvar /= 0) gnorm = sqrt(dot(grad,grad,nvar)) 
      write (iw, '(/,'' ----'',15(''-----''))') 
      call wrttxt (iw) 
      write (iw, '(2/4X,A58)') flepo(iflepo) 
      iscf = max(1,iscf) 
      write (iw, '(4X,A58)') iter(iscf) 
      write (iw, '(2/30X,A7,''  CALCULATION'')') caltyp 
      write (iw, '(55X,''MOPAC '',F5.2)') verson 
      write (iw, '(55X,A24)') idate 
      if (iscf == 2) then 
!
!   RESULTS ARE MEANINGLESS. DON'T PRINT ANYTHING!
!
        write (iw, '(A)') ' ' 
        write (iw, '(A)') ' ' 
        write (iw, '(A)') ' FOR SOME REASON THE SCF CALCULATION FAILED.' 
        write (iw, '(A)') ' ' 
        write (iw, '(A)') &
          ' THE RESULTS WOULD BE MEANINGLESS, SO WILL NOT BE PRINTED.' 
        write (iw, '(A)') &
          ' TRY TO FIND THE REASON FOR THE FAILURE BY USING ''PL''.' 
        write (iw, '(A)') ' ' 
        write (iw, '(A)') &
          ' CHECK YOUR GEOMETRY AND ALSO TRY USING SHIFT OR PULAY. ' 
        call geout (1) 
        call mopend ('THE SCF CALCULATION FAILED.') 
        return  
      endif 
      write (iw, &
      &'(4/10X,''FINAL HEAT OF FORMATION ='',F17.5,'' KCAL''  ,'' = '',F12.5,'' &
      &KJ'')') escf, escf*4.184D0 
      if (index(keywrd,' EPS') /= 0) write (iw, '(10X,A,F14.2,A)') &
        'VAN DER WAALS AREA      =', area, ' SQUARE ANGSTROMS' 
      if (latom == 0) write (iw, '(/)') 
      if (state(1) /= '    ') then 
         write (iw, &
      &'(    10X,''TOTAL ENERGY            ='',F17.5,'' EV'' ,''  STATE:  '',3A4&
      &)') elect + enuclr, (state(i),i=1,3) 
      else 
        staten(1) = ' ' 
        write (iw, '(    10X,''TOTAL ENERGY            ='',F17.5,'' EV'' )') &
          elect + enuclr 
      endif 
      if (norbs > 0) call symtrz (c, eigs, 1, .TRUE.) 
      if (moperr) return  
      if (id == 0) then 
        write (iw, &
      '(    10X,''ELECTRONIC ENERGY       ='',F17.5,'' EV'' ,''  POINT GROUP:  &
      &   '',A4)') elect, name 
      else 
        write (iw, '(    10X,''ELECTRONIC ENERGY       ='',F17.5,'' EV'' )') &
          elect 
      endif 
      write (iw, '(    10X,''CORE-CORE REPULSION     ='',F17.5,'' EV''    )') &
        enuclr 
! COSMO change
      if (iseps) then 
        call dielen (edie) 
        write (iw, '(    10X,''DIELECTRIC ENERGY       ='',F17.5,'' EV''   )') &
          edie 
      endif 
! end of COSMO change
      if (latom == 0) write (iw, '(1X)') 
      prtgra = prtgra .or. gnorm>2.D0 
      if (prtgra) write (iw, '(    10X,''GRADIENT NORM           ='',F17.5)') &
        gnorm 
      if (gnorm>2.D0 .and. fract>0.05D0 .and. fract<1.95D0 .and. index(keywrd,&
        ' 1SCF')==0 .and. index(keywrd,' UHF')==0 .and. index(keywrd,' NOANCI')&
        ==0) write (iw, *) &
      ' TO REDUCE GNORM FURTHER, TRY ADDING KEYWORD ''NOANCI'' AND RE-RUN THE J&
      &OB' 
      still = .TRUE. 
      if (latom == 0) then 
        if (index(keywrd,' AIDER') == 0) then 
          if (index(keywrd,' 1SCF')==0 .or. index(keywrd,' GRAD')/=0) then 
!
!   CHECK THAT THE CARTESIAN COORDINATE GRADIENT IS ALSO SMALL
!
       sum = dot(dxyz,dxyz,3*numat)
            if (dot(dxyz,dxyz,3*numat)>max(16.D0,4*gnorm**2) .and. gnorm<2.D0&
               .and. nclose==nopen .and. id==0) then 
              write (iw, '(A)') &
                ' WARNING -- GEOMETRY IS NOT AT A STATIONARY POINT' 
              still = .FALSE. 
            endif 
          endif 
        endif 
      else 
!
!   WE NEED TO CALCULATE THE REACTION COORDINATE GRADIENT.
!
        mvar = nvar 
        loc11 = loc(1,1) 
        loc21 = loc(2,1) 
        nvar = 1 
        loc(1,1) = latom 
        loc(2,1) = lparam 
        xreact = geo(lparam,latom)
        gcoord = 0.d0 
        call deriv (geo, gcoord) 
        nvar = mvar 
        loc(1,1) = loc11 
        loc(2,1) = loc21 
        grtype = ' KCAL/ANGSTROM' 
        if (lparam == 1) then 
          write (iw, &
      '(    10X,''FOR REACTION COORDINATE ='',F17.5              ,'' ANGSTROMS'&
      &')') xreact 
        else 
          if (na(1) /= 99) grtype = ' KCAL/RADIAN  ' 
          write (iw, &
      &'(    10X,''FOR REACTION COORDINATE ='',F17.5              ,'' DEGREES'')&
      &') xreact*degree 
        endif 
        write (iw, '(    10X,''REACTION GRADIENT       ='',F17.5,A14         )'&
      &   ) gcoord, grtype 
      endif 
      eionis = 0.D0 
      if (nalpha > 0) then 
        eionis = -max(eigs(nalpha),eigb(nbeta)) 
      else if (nelecs == 1) then 
        eionis = -eigs(1) 
      else if (nelecs > 1) then 
        if (nclose > 0) eionis = -eigs(nclose) 
        if (nopen > 0) eionis = min(eionis,(-eigs(nopen))) 
      endif 
      i = nclose 
      if (fract > 1.99D0) i = nopen 
      nopn = nopen - i 
!   CORRECTION TO I.P. OF DOUBLETS
      if (nopn == 1) eionis = eionis + 0.5D0*rjkab(1,1) 
      if (abs(eionis) > 1.D-5) write (iw, &
        '(       10X,''IONIZATION POTENTIAL    ='',F17.5)') eionis 
      if (uhf) then 
        write (iw, '(      10X,''NO. OF ALPHA ELECTRONS  ='',I11)') nalpha 
        write (iw, '(      10X,''NO. OF BETA  ELECTRONS  ='',I11)') nbeta 
      else 
        write (iw, '(      10X,''NO. OF FILLED LEVELS    ='',I11)') nopen - &
          nopn 
        if (nopn /= 0) write (iw, '(   10X,''AND NO. OF OPEN LEVELS  ='',I11)')&
           nopn 
      endif 
      sumw = 0.D0 
      do i = 1, numat 
        sumw = sumw + atmass(i) 
      end do 
      if (sumw > 0.1D0) write (iw, &
        '(    10X,''MOLECULAR WEIGHT        ='',F11.3)') sumw 
      call gmetry (geo, coord) 
      call dimens (coord, iw) 
      if (id == 3) then 
        vol = volume(tvec,3) 
        write (iw, '(/10X,A,F17.3,A,/10X,A,F21.3,A)') 'VOLUME OF UNIT CELL', &
          vol, ' CUBIC ANGSTROMS', 'DENSITY        ', sumw*1.D24/fpc_10/vol, &
          ' GRAMS/CC' 
      endif 
      if (latom == 0) write (iw, '(/)') 
      write (iw, '(10X,''SCF CALCULATIONS  =   '',I14 )') nscf 
      tim = second(1) - time0 
      i = int(tim*0.000001D0) 
      tim = tim - i*1000000 
      call timout (iw, tim) 
      if (ndep /= 0) call symtry 
      do i = 1, nvar 
        xparam(i) = geo(loc(2,i),loc(1,i)) 
      end do 
      call gmetry (geo, coord) 
      if (prtgra) then 
        write (iw, '(3/7X,''FINAL  POINT  AND  DERIVATIVES'',/)') 
        write (iw, &
      '(''   PARAMETER     ATOM    TYPE  ''                     ,''          VA&
      &LUE       GRADIENT'')') 
        do i = 1, nvar 
          j = loc(2,i) 
          k = loc(1,i) 
          l = labels(k) 
          xi = xparam(i) 
          if (j /= 1) xi = xi*degree 
          if (j==1 .or. na(1)==99) then 
            gtype = 'KCAL/ANGSTROM' 
          else 
            gtype = 'KCAL/RADIAN  ' 
          endif 
          write (iw, '(I7,I11,1X,A2,4X,A11,F13.6,F13.6,2X,A13)') i, k, elemnt(l&
            ), type(j), xi, grad(i), gtype 
        end do 
      endif 
!
!     WRITE OUT THE GEOMETRY
!
      write (iw, '(3/)') 
      call geout (1) 
      if (index(keywrd,' NOINT') == 0) then 
!
!   WRITE OUT THE INTERATOMIC DISTANCES
!
        l = 0 
        do i = 1, numat 
          do j = 1, i 
            l = l + 1 
            rxyz(l) = sqrt((coord(1,i)-coord(1,j))**2+(coord(2,i)-coord(2,j))**&
              2+(coord(3,i)-coord(3,j))**2) 
          end do 
        end do 
        write (iw, '(2/10X,''  INTERATOMIC DISTANCES'')') 
        call vecprt (rxyz, numat) 
      endif 
      where (.not.eigs(:norbs)>=(-999.D0) .or. .not.eigs(:norbs)<=1000.D0)  
        eigs(:norbs) = 1.D-12 
      end where 
      where (eigb(:norbs)<(-999.D0) .or. eigb(:norbs)>1000.D0)  
        eigb(:norbs) = 1.D-12 
      end where 
      if (norbs*nelecs > 0) then 
        if (id == 0) write (iw, '(2/''      MOLECULAR POINT GROUP   :   '',A4)'&
          ) name 
        if (index(keywrd,' VECT')*nelecs /= 0) then 
          write (iw, '(2/10X,A5,'' EIGENVECTORS  '')') calcn(iuhf) 
          call matou1 (c, eigs, norbs, norbs, norbs, 2) 
          if (uhf) then 
            write (iw, '(2/10X,'' BETA EIGENVECTORS  '')') 
            call symtrz (cb, eigb, 1, .TRUE.) 
            call matou1 (cb, eigb, norbs, norbs, norbs, 2) 
          endif 
        else 
          write (iw, '(2/10X,A5,''   EIGENVALUES'',/)') calcn(iuhf) 
          write (iw, '(8F10.5)') (eigs(i),i=1,norbs) 
          if (uhf) then 
            write (iw, '(2/10X,'' BETA EIGENVALUES '')') 
            write (iw, '(8F10.5)') (eigb(i),i=1,norbs) 
          endif 
        endif 
        if (index(keywrd,' SUPER') /= 0) then 
          write (iw, '(/,10X,A,/)') ' SUPERDELOCALIZABILITIES' 
          call superd (c, eigs, norbs, nelecs, numat, nat) 
        endif 
      endif 
      if (nelecs /= 0) then 
!
!   Correct density matrix, if necessary
!
        if (nclose/=nopen .and. abs(fract - 2.d0) > 1.d-20 .and. &
         fract > 1.d-20 .or. index(keywrd,' C.I.')/=0) call mecip () 
        write (iw, &
      '(2/13X,'' NET ATOMIC CHARGES AND DIPOLE '',          ''CONTRIBUTIONS'',/&
      &)') 
        write (iw, &
      '(8X,'' ATOM NO.   TYPE          CHARGE        ATOM'' ,''  ELECTRON DENSI&
      &TY'')') 
        call chrge (p, q) 
        sum = 0.D0 
        do i = 1, numat 
          l = nat(i) 
          q2(i) = tore(l) - q(i) 
          sum = sum + q2(i) 
          write (iw, '(I12,9X,A2,4X,F15.6,F14.4)') i, elemnt(l), q2(i), q(i) 
        end do 
        kchrge = nint(sum) 
        dip = dipole(p,q2,coord,dumy,1) 
        sum = dumy(1) ! Dummy operation - to use dumy
      endif 
      if (index(keywrd,' NOXYZ') == 0) then 
        write (iw, '(2/10X,''CARTESIAN COORDINATES '',/)') 
        write (iw, &
      '(4X,''NO.'',7X,''ATOM'',15X,''X'',                     9X,''Y'',9X,''Z''&
      &,/)') 
        write (iw, '(I6,8X,A2,14X,3F10.4)') (i,elemnt(nat(i)),(coord(j,i),j=1,3&
          ),i=1,numat) 
      else 
        q2(:numat) = tore(nat(:numat)) 
      endif 
      if (norbs > 0) then 
        if (index(keywrd,' K=') /= 0) then 
!
!  GO INTO BRILLOUIN ZONE MODE
!
          i = index(keywrd,' K=') 
          step = reada(keywrd,i) 
          mono3 = nlast(nint(reada(keywrd(i:),index(keywrd(i:),',')))) 
          if (uhf) write (iw, '(A)') '  ALPHA BANDS' 
          call brlzon (f, norbs, mono3, step, 2) 
          if (uhf) then 
            write (iw, '(A)') '  BETA BANDS' 
            call brlzon (fb, norbs, mono3, step, 2) 
          endif 
        endif 
        if (index(keywrd,' FOCK') /= 0) then 
          write (iw, '('' FOCK MATRIX '')') 
          call vecprt (f, norbs) 
        endif 
        j = index(keywrd,' MERS') 
        if (j /= 0) then 
          mers(2) = 1 
          mers(3) = 1 
          i = index(keywrd(j:),' ') + j 
          k = 0 
          do l = 1, 3 
            j = j + k 
            if (l>1 .and. k==0) exit  
            mers(l) = nint(reada(keywrd(j:),1)) 
            k = index(keywrd(j:i),',') 
          end do 
          bcc = index(keywrd,' BCC') /= 0 
          i = index(jobnam,' ') - 1 
          open(unit=ibrz, file=jobnam(:i)//'.brz', status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
          write (ibrz) norbs, mers, bcc 
          write (ibrz) (f(i),i=1,(norbs*(norbs + 1))/2) 
          write (ibrz) tvec, id, numat, ((coord(j,i),j=1,3),i=1,numat) 
          write (ibrz) (nfirst(i),nlast(i),i=1,numat) 
        endif 
        if (nelecs /= 0) then 
          if (index(keywrd,' DENS') /= 0) then 
            write (iw, '(2/,20X,'' DENSITY MATRIX IS '')') 
            call vecprt (p, norbs) 
          else 
            write (iw, '(2/10X,''ATOMIC ORBITAL ELECTRON POPULATIONS'' ,/)') 
            write (iw, '(8F10.5)') (p((i*(i+1))/2),i=1,norbs) 
          endif 
          if (index(keywrd,' PI') /= 0) then 
            write (iw, '(2/10X,''SIGMA-PI BOND-ORDER MATRIX'')') 
            call denrot () 
          endif 
        endif 
        if (uhf) then 
          sz = abs(nalpha - nbeta)*0.5D0 
          ss2 = sz*sz 
          l = 0 
          do i = 1, norbs 
            do j = 1, i 
              l = l + 1 
              pa(l) = pa(l) - pb(l) 
              ss2 = ss2 + pa(l)**2 
            end do 
            ss2 = ss2 - 0.5D0*pa(l)**2 
          end do 
          write (iw, '(2/20X,''(SZ)    ='',F10.6)') sz 
          write (iw, '(  20X,''(S**2)  ='',F10.6)') ss2 
          if (index(keywrd,' SPIN') /= 0) then 
            write (iw, '(2/10X,''SPIN DENSITY MATRIX'')') 
            call vecprt (pa, norbs) 
          else 
            write (iw, '(2/10X,''ATOMIC ORBITAL SPIN POPULATIONS'',/)') 
            write (iw, '(8F10.5)') (pa((i*(i+1))/2),i=1,norbs) 
          endif 
          if (index(keywrd,' HYPERFINE') /= 0) then 
!
!  WORK OUT THE HYPERFINE COUPLING CONSTANTS.
!
            write (iw, '(2/10X,''    HYPERFINE COUPLING COEFFICIENTS'' ,/)') 
            j = (nalpha - 1)*norbs 
      !      q(:numat) = pa(nfirst(:numat)*(nfirst(:numat)+1)/2)*0.3333333D0 + c&
        !      (nfirst(:numat)+j)**2*0.66666666D0 
            write (iw, '(5(2X,A2,I2,F9.5,1X))') (elemnt(nat(i)),i,q(i),i=1,&
              numat) 
          endif 
          pa = p - pb
        endif 
        if (index(keywrd,' BONDS') /= 0) then 
          if (nbeta == 0) then 
            write (iw, '(/10X,''BONDING CONTRIBUTION OF EACH M.O.'',/)') 
            call molval (c, p, 2.D0) 
          else 
            write (iw, '(/10X,''BONDING CONTRIBUTION OF EACH ALPHA M.O.'',/)') 
            call molval (c, p, 1.D0) 
            write (iw, '(/10X,''BONDING CONTRIBUTION OF EACH BETA  M.O.'',/)') 
            call molval (cb, pb, 1.D0) 
          endif 
!
!  Scratch: NEA:NATOMS**2/2, NE: NATOMS**2, EX:NATOMS**2/2
!  Try NPO3 for NEA,
          call bonds () 
        endif 
        i = nclose + nalpha 
        if (index(keywrd,' LOCAL') /= 0) then 
          j = 2 
          if (nbeta /= 0) j = 1 
          call local (c, i, eigs) 
          if (nbeta /= 0) then 
            write (iw, '(2/10X,'' LOCALIZED BETA MOLECULAR ORBITALS'')') 
            call local (cb, nbeta, eigb) 
          endif 
        endif 
        if (index(keywrd,' 1ELE') /= 0) then 
          write (iw, '('' FINAL ONE-ELECTRON MATRIX '')') 
          call vecprt (h, norbs) 
        endif 
        if (index(keywrd,' ENPART') /= 0) call enpart (w) 
      endif 
!
!   WORK OUT HOW MANY ATOMS OF EACH ELEMENT ARE IN THE SYSTEM
!
      nelemt = 0 
      do i = 1, numat 
        igo = nat(i) 
        if (igo > 107) cycle  
        nelemt(igo) = nelemt(igo) + 1 
      end do 
      ichfor = 0 
      if (nelemt(6) /= 0) then 
        ichfor = 1 
        ielemt(1) = elemnt(6) 
        nzs = nelemt(6) 
        if (nzs < 10) then 
          if (nzs == 1) then 
            iel1(1) = 11 
          else 
            iel1(1) = nzs + 1 
          endif 
          iel2(1) = 11 
        else 
          kfrst = nzs/10 
          ksec = nzs - 10*kfrst 
          iel1(1) = kfrst + 1 
          iel2(1) = ksec + 1 
        endif 
      endif 
      nelemt(6) = 0 
      do i = 1, 107 
        if (nelemt(i) == 0) cycle  
        ichfor = ichfor + 1 
        ielemt(ichfor) = elemnt(i) 
        nzs = nelemt(i) 
        if (nzs < 10) then 
          if (nzs == 1) then 
            iel1(ichfor) = 11 
          else 
            iel1(ichfor) = nzs + 1 
          endif 
          iel2(ichfor) = 11 
        else 
          kfrst = nzs/10 
          ksec = nzs - 10*kfrst 
          iel1(ichfor) = kfrst + 1 
          iel2(ichfor) = ksec + 1 
        endif 
      end do 
      if (index(keywrd,' DENOUT') /= 0) then 
        i = index(jobnam,' ') - 1 
        open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
          'UNFORMATTED', position='asis') 
        rewind iden 
        write (iden) pa
        if (uhf) write (iden) pb
        close(iden) 
      endif 
      if ((ci .or. nopen/=nclose .and. Abs(fract-2.d0) > 1.d-20 .and. fract > 1.d-20 .or. &
        index(keywrd,' SIZE')/=0) .and. index(keywrd,' MECI')+index(keywrd,&
        ' ESR')/=0) then 
        write (iw, &
      '(2/10X,                                              ''MULTI-ELECTRON CO&
      &NFIGURATION INTERACTION CALCULATION'',2/)') 
        last = 3 
        sum = meci() 
        if (moperr) return  
      endif 
      if (index(keywrd,' MULLIK') + index(keywrd,' GRAPH') /= 0) then 
        if (index(keywrd,' MULLIK') /= 0) write (iw, &
          '(/10X,'' MULLIKEN POPULATION ANALYSIS'')') 
        call mullik () 
        if (index(keywrd,' GRAPH') /= 0) write (iw, &
          '(/10X,'' DATA FOR GRAPH WRITTEN TO DISK'')') 
      endif 
!
!  NOTE THAT THE DENSITY, H AND F MATRICES ARE CORRUPTED BY A  CALL TO
!  MULLIK.  ON EXIT FROM MULLIK, F HOLDS THE MULLIKEN ANALYSIS.
!
      if (index(keywrd,'MULLIK') /= 0) then 
        call mpcpop (pb, 1) 
      endif  
      if (icalcn /= numcal) then 
        if (namfil == '**NULL**') then 
          i = index(jobnam,' ') - 1 
          namfil = jobnam(:i)//'.arc' 
          inam = ichar('a') 
          jnam = inam 
          jend = index(namfil,' ') 
          iend = jend + 1 
        endif 
  240   continue 
        inquire(file=namfil, exist=exists) 
        if (exists) then 
          namfil(iend:iend) = char(inam) 
          namfil(jend:jend) = char(jnam) 
          if (inam == ichar('z')) then 
            inam = inam - 26 
            jnam = jnam + 1 
          endif 
          inam = inam + 1 
          go to 240 
        endif 
        inquire(unit=iarc, opened=opend) 
        if (opend) close(unit=iarc, status='KEEP') 
        open(unit=iarc, file=namfil, status='NEW', position='asis') 
        rewind iarc 
        icalcn = numcal 
      endif 
      if (moperr) return  
      iwrite = iarc 
      write (iwrite, &
      '(2/20X,'' SUMMARY OF '',A7,                         '' CALCULATION'',/)'&
        ) caltyp 
      write (iwrite, '(60X,''MOPAC  '',F5.2)') verson 
      write (iwrite, 260) (ielemt(i),numbrs(iel1(i)),numbrs(iel2(i)),i=1,ichfor&
        ) 
  260 format(/,/,1x,17(a2,a1,a1)) 
      write (iwrite, '(55X,A24)') idate 
      call wrttxt (iwrite) 
      write (iwrite, '(2/4X,A58)') flepo(iflepo) 
      write (iwrite, '(4X,A58)') iter(iscf) 
      write (iwrite, &
      '(2/10X,''HEAT OF FORMATION       =''                ,F17.6,'' KCAL = '',&
      &F12.5,'' KJ'')') escf, escf*4.184D0 
      if (staten(1) /= '    ') then 
        write (iwrite, &
      '(  10X,''ELECTRONIC ENERGY       ='',F17.6,'' EV'',''      STATE: '',3A4&
      &)') elect, (staten(i),i=1,3) 
      else 
        write (iwrite, '(  10X,''ELECTRONIC ENERGY       ='',F17.6,'' EV'')') &
          elect 
      endif 
      write (iwrite, &
        '(  10X,''tore-tore REPULSION     =''                ,F17.6,'' EV'')') &
        enuclr 
      if (prtgra) write (iwrite, &
        '(  10X,''GRADIENT NORM           =''                ,F17.6)') gnorm 
      if (latom == 0) then 
        if (.not.still) write (iwrite, '(A)') &
          ' WARNING -- GEOMETRY IS NOT AT A STATIONARY POINT' 
      else 
        grtype = ' KCAL/ANGSTROM' 
        if (lparam == 1) then 
          write (iwrite, &
      '(    10X,''FOR REACTION COORDINATE ='',F17.4          ,'' ANGSTROMS'')')&
             xreact 
        else 
          if (na(1) /= 99) grtype = ' KCAL/RADIAN  ' 
          write (iwrite, &
      '(    10X,''FOR REACTION COORDINATE ='',F17.4          ,'' DEGREES'')') &
            xreact*degree 
        endif 
        write (iwrite, '(    10X,''REACTION GRADIENT       ='',F17.6,A14     )'&
          ) gcoord, grtype 
      endif 
      write (iwrite, &
      '(  10X,''DIPOLE                  =''                ,F16.5, '' DEBYE    &
      &SYMMETRY:       '',A)') dip, name 
      if (uhf) then 
        write (iwrite, '(  10X,''(SZ)                    ='',F17.6)') sz 
        write (iwrite, '(  10X,''(S**2)                  ='',F17.6)') ss2 
        write (iwrite, '(  10X,''NO. OF ALPHA ELECTRONS  ='',I10)') nalpha 
        write (iwrite, '(  10X,''NO. OF BETA  ELECTRONS  ='',I10)') nbeta 
      else 
        write (iwrite, '(      10X,''NO. OF FILLED LEVELS    ='',I11)') nopen&
           - nopn 
        if (nopn /= 0) write (iwrite, &
          '(  10X,''AND NO. OF OPEN LEVELS  ='',I10)') nopn 
      endif 
      if (ci) write (iwrite, '(  10X,''CONFIGURATION INTERACTION WAS USED'')') 
      if (kchrge /= 0) write (iwrite, &
        '(  10X,''CHARGE ON SYSTEM        ='',I10)') kchrge 
      write (iwrite, &
        '(  10X,''IONIZATION POTENTIAL    =''                ,F17.6,'' EV'')') &
        eionis 
      if (uhf) then 
        if (nalpha >= 1) write (iwrite, &
          '(  10X,''ALPHA SOMO LUMO (EV)    ='',F14.3,F7.3)') eigs(nalpha), (&
          eigs(i),i=nalpha + 1,norbs,10000) 
        if (nbeta >= 1) write (iwrite, &
          '(  10X,''BETA  SOMO LUMO (EV)    ='',F14.3,F7.3)') eigb(nbeta), (&
          eigb(i),i=nbeta + 1,norbs,10000) 
      else if (nopen == nclose) then 
        if (nopen >= 1) write (iwrite, &
          '(  10X,''HOMO LUMO ENERGIES (EV) ='',F14.3,F7.3)') eigs(nopen), (&
          eigs(i),i=nopen + 1,norbs,10000) 
      else if (nopn == 1) then 
        if (nclose >= 1) then 
          write (iwrite, &
      '(  10X,''HOMO (SOMO) LUMO (EV)   ='',F14.3,   '' ('',F7.3,'')'',F7.3)') &
            eigs(nclose), (eigs(i),i=nclose + 1,norbs,10000), (eigs(i),i=nclose&
             + 2,norbs,10000) 
        else 
          write (iwrite, &
      '(  10X,''     (SOMO) LUMO (EV)   ='',         ''      ('',F7.3,'')'',F7.&
      &3)') eigs(nclose+1), (eigs(i),i=nclose + 2,norbs,10000) 
        endif 
      endif 
      write (iwrite, '(  10X,''MOLECULAR WEIGHT        ='',F14.3)') sumw 
      write (iwrite, &
        '(  10X,''SCF CALCULATIONS        =''                ,I10)') nscf 
      tim = second(1) - time0 
      call timout (iwrite, tim) 
      write (iwrite, '(2/10X,''FINAL GEOMETRY OBTAINED'',36X,''CHARGE'')') 
      call geout (iwrite) 
      if (index(keywrd,' AIGOUT') /= 0) then 
        write (iwrite, '(2/,A)') '  GEOMETRY IN GAUSSIAN Z-MATRIX STYLE' 
        call wrttxt (iwrite) 
        call geoutg (iwrite) 
      endif 
  !    if (iwrite/=ifiles(11) .and. index(keywrd,' NOLOG')==0) then 
    !    iwrite = ifiles(11) 
    !    go to 250 
   !   endif 
      nscf = 0 
      return  
      end subroutine writmo
