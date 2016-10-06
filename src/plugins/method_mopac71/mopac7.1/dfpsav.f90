      subroutine dfpsav(totime, xparam, gd, xlast, funct1, mdfp, xdfp) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE ef_C, ONLY: alparm, x0, x1, x2, iloop 
      USE maps_C, only : currt1, currt2, ijlp, ilp, jlp, ione, jlp1, surf, &
      currt, latom, kloop
      use permanent_arrays, only : na, geo, geoa, grad, pa, pb, hesinv, &
      errfn, profil 
      USE molkst_C, ONLY: nalpha, natoms, numcal, nvar, keywrd, &
      & koment, title, jobnam 
      USE chanel_C, only : iw, ires, iden 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:50:30  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use geout_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: totime 
      real(double)  :: funct1 
      integer  :: mdfp(9) 
      real(double)  :: xparam(nvar) 
      real(double)  :: gd(nvar) 
      real(double)  :: xlast(nvar) 
      real(double)  :: xdfp(9)  
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, j, linear 
      logical :: first, opend 

      save first, icalcn 
!-----------------------------------------------
!*********************************************************************
!
! DFPSAV STORES AND RESTORES DATA USED IN THE D-F-P GEOMETRY
!        OPTIMISATION.
!
!  ON INPUT TOTIME = TOTAL CPU TIME ELAPSED DURING THE CALCULATION.
!           XPARAM = CURRENT VALUE OF PARAMETERS.
!           GD     = OLD GRADIENT.
!           XLAST  = OLD VALUE OF PARAMETERS.
!           FUNCT1 = CURRENT VALUE OF HEAT OF FORMATION.
!           MDFP   = INTEGER CONSTANTS USED IN D-F-P.
!           XDFP   = REAL CONSTANTS USED IN D-F-P.
!           MDFP(9)= 1 FOR DUMP, 0 FOR RESTORE.
!*********************************************************************
      data icalcn/ 0/  
      first = icalcn /= numcal 
      if (first) icalcn = numcal 
      inquire(unit=ires, opened=opend) 
      if (opend) close(unit=ires, status='KEEP') 
      inquire(unit=iden, opened=opend) 
      if (opend) close(unit=iden, status='KEEP') 
      i = index(jobnam,' ') - 1 
      open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
        'UNFORMATTED', position='asis') 
      rewind ires 
      open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
        'UNFORMATTED', position='asis') 
      rewind iden 
      if (mdfp(9) /= 0) then 
        if (mdfp(9) == 1) then 
          write (iw, &
      '(2/10X,                                           ''- - - - - - - TIME U&
      &P - - - - - - -'',2/)') 
          if (index(keywrd,'SADDLE') /= 0) then 
            write (iw, &
      '(2/10X,'' NO RESTART EXISTS FOR SADDLE'',2/      10X,'' HERE IS A DATA-F&
      &ILE FILES THAT MIGHT BE SUITABLE'',/       10X,'' FOR RESTARTING THE CALC&
      &ULATION'',3/)') 
            write (iw, '(A)') keywrd, koment, title 
            do iloop = 1, 2 
              call geout ((-iw)) 
              geo(:,:natoms) = geoa(:,:natoms) 
              na(1) = 99 
            end do 
            write (iw, '(3/10X,''CALCULATION TERMINATED HERE'')') 
            call mopend ('NO RESTART EXISTS FOR SADDLE. ') 
            return  
          endif 
          write (iw, &
      '(2/10X,                                           '' - THE CALCULATION I&
      &S BEING DUMPED TO DISK'',                    /10X,''   RESTART IT USING T&
      &HE KEYWORD "RESTART"'')') 
          write (iw, &
            '(2/10X,''CURRENT VALUE OF HEAT OF FORMATION =''     ,F12.6)') &
            funct1 
        endif 
        if (mdfp(9) == 1) call geout (iw) 
        write (ires) mdfp, xdfp, totime, funct1 
        write (ires) (xparam(i),i=1,nvar), (gd(i),i=1,nvar) 
        write (ires) (xlast(i),i=1,nvar), (grad(i),i=1,nvar) 
        linear = (nvar*(nvar + 1))/2 
        write (ires) (hesinv(i),i=1,linear) 
        write (iden) pa
        if (nalpha /= 0) write (iden) pb
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
        write (ires) (errfn(i),i=1,nvar) 
        close(ires) 
        close(iden) 
      else 
        if (first) write (iw, '(2/10X,'' RESTORING DATA FROM DISK''/)') 
        read (ires, end=40, err=40) mdfp, xdfp, totime, funct1 
        if (first) write (iw, '(10X,''FUNCTION ='',F13.6,2/)') funct1 
        read (ires, end=50, err=50) (xparam(i),i=1,nvar), (gd(i),i=1,nvar) 
        read (ires, end=50, err=50) (xlast(i),i=1,nvar), (grad(i),i=1,nvar) 
        linear = (nvar*(nvar + 1))/2 
        read (ires, end=50, err=50) (hesinv(i),i=1,linear)  
        read (iden, end=60, err=60) pa
        if (nalpha /= 0) read (iden, end=60, err=60) pb
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
        read (ires) (errfn(i),i=1,nvar) 
        first = .FALSE. 
        return  
   40   continue 
        write (iw, '(2/10X,''NO RESTART FILE EXISTS!'')') 
        call mopend ('NO RESTART FILE EXISTS!') 
        return  
   50   continue 
        write (iw, '(2/10X,''RESTART FILE EXISTS, BUT IS CORRUPT'')') 
        call mopend ('RESTART FILE EXISTS, BUT IS CORRUPT') 
        return  
   60   continue 
        write (iw, '(2/10X,''DENSITY FILE MISSING OR CORRUPT'')') 
        call mopend ('DENSITY FILE MISSING OR CORRUPT') 
        return  
      endif 
      return  
      end subroutine dfpsav 
