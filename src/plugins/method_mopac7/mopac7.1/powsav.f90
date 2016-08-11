      subroutine powsav(hess, grad, xparam, pmat, iloop, bmat, ipow) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : nvar, keywrd, nalpha, jobnam
      use maps_C, only : latom
      use chanel_C, only : iw, ires, iden
      use permanent_arrays, only : loc, geo, pa, pb, aicorr
      use ef_C, only : alparm, x0, x1, x2
      use meci_C, only : jloop
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:34  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dot_I 
      use geout_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: iloop 
      integer  :: ipow(9) 
      real(double)  :: hess(nvar,*) 
      real(double)  :: grad(*) 
      real(double)  :: xparam(*) 
      real(double)  :: pmat(*) 
      real(double)  :: bmat(nvar,*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, k, l, j, linear 
      real(double) :: funct1 
!-----------------------------------------------
!*********************************************************************
!
! POWSAV STORES AND RESTORES DATA USED IN THE SIGMA GEOMETRY
!        OPTIMISATION.
!
!  ON INPUT HESS   = HESSIAN MATRIX, PARTIAL OR WHOLE.
!           GRAD   = GRADIENTS.
!           XPARAM = CURRENT STATE OF PARAMETERS.
!           ILOOP  = INDEX OF HESSIAN, OR FLAG OF POINT REACHED SO-FAR.
!           BMAT   = "B" MATRIX!
!           IPOW   = INDICES AND FLAGS.
!           IPOW(9)= 0 FOR RESTORE, 1 FOR DUMP
!
!*********************************************************************
      i = index(jobnam,' ') - 1 
      open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
        'UNFORMATTED', position='asis') 
      rewind ires 
      open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
        'UNFORMATTED', position='asis') 
      rewind iden 
      if (ipow(9) /= 0) then 
        if (ipow(9) == 1) then 
          write (iw, &
      '(2/10X,                                           ''- - - - - - - TIME U&
      &P - - - - - - -'',2/)') 
          write (iw, '(10X,A)') ' - THE CALCULATION IS BEING DUMPED TO DISK', &
            '   RESTART IT USING THE KEY-WORD "RESTART"' 
          funct1 = sqrt(dot(grad,grad,nvar)) 
          write (iw, &
            '(2/10X,''CURRENT VALUE OF GRADIENT NORM =''         ,F12.6)') &
            funct1 
          do i = 1, nvar 
            k = loc(1,i) 
            l = loc(2,i) 
            geo(l,k) = xparam(i) 
          end do 
          write (iw, '(/10X,''CURRENT VALUE OF GEOMETRY'',/)') 
          call geout (iw) 
        endif 
        write (ires) ipow, iloop 
        write (ires) (xparam(i),i=1,nvar) 
        write (ires) (grad(i),i=1,nvar) 
        write (ires) ((hess(j,i),j=1,nvar),i=1,nvar) 
        write (ires) ((bmat(j,i),j=1,nvar),i=1,nvar) 
        linear = (nvar*(nvar + 1))/2 
        write (ires) (pmat(i),i=1,linear) 
        if (index(keywrd,'AIDER') /= 0) write (ires) (aicorr(i),i=1,nvar) 
        write (iden) pa
        if (nalpha /= 0) write (iden) pb
        if (latom /= 0) then 
          write (ires) ((alparm(j,i),j=1,3),i=1,nvar) 
          write (ires) jloop, x0, x1, x2 
        endif 
        close(ires) 
        close(iden) 
        return  
      else 
        write (iw, '(2/10X,'' RESTORING DATA FROM DISK''/)') 
        read (ires, end=20, err=20) ipow, iloop 
        read (ires, end=40, err=40) (xparam(i),i=1,nvar) 
        read (ires, end=40, err=40) (grad(i),i=1,nvar) 
        read (ires, end=40, err=40) ((hess(j,i),j=1,nvar),i=1,nvar) 
        read (ires, end=40, err=40) ((bmat(j,i),j=1,nvar),i=1,nvar) 
        funct1 = sqrt(dot(grad,grad,nvar)) 
        write (iw, '(10X,''FUNCTION ='',F13.6,2/)') funct1 
        linear = (nvar*(nvar + 1))/2 
        read (ires, end=40, err=40) (pmat(i),i=1,linear) 
        if (index(keywrd,'AIDER') /= 0) read (ires, end=40, err=40) (aicorr(i),&
          i=1,nvar) 
        read (iden, end=30, err=30) pa
        if (nalpha /= 0) read (iden, end=50, err=50) pb 
        if (latom /= 0) then 
          read (ires, end=40, err=40) ((alparm(j,i),j=1,3),i=1,nvar) 
          read (ires, end=40, err=40) jloop, x0, x1, x2 
          iloop = iloop + 1 
        endif 
        iloop = iloop + 1 
        return  
      endif 
   20 continue 
      write (iw, '(2/10X,''NO RESTART FILE EXISTS!'')') 
      return  
   30 continue 
      write (iw, '(2/10X,''NO DENSITY FILE EXISTS!'')') 
      return  
   40 continue 
      write (iw, '(2/10X,''PROBLEMS READING RESTART FILE'')') 
      return  
   50 continue 
      write (iw, '(2/10X,''PROBLEMS READING DENSITY FILE'')') 
      return  
      end subroutine powsav 
