      subroutine parsav(mode, n, m, q, r, efslst, xlast, iiium) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double    
      USE molkst_C, ONLY:  nalpha, keywrd, jobnam 
      USE chanel_C, only : ires, iden, iw
      use permanent_arrays, only : pa, pb, aicorr, errfn
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:17:05  03/10/06  
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
      integer , intent(in) :: mode 
      integer  :: n 
      integer  :: m 
      integer, dimension(6) :: iiium
      real(double)  :: q(n,n) 
      real(double), dimension(n) :: efslst, xlast
      real(double)  :: r(n + 2,n + 2) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, j 
      logical :: opend 
!-----------------------------------------------
!*********************************************************************
!
!   PARSAV SAVES AND RESTORES DATA USED IN NLLSQ GRADIENT MINIMIZATION.
!
!    IF MODE IS 0 DATA ARE RESTORED, IF 1 THEN SAVED.
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
      if (mode == 0) then 
!
!  MODE=0: RETRIEVE DATA FROM DISK.
!
        read (ires, end=20, err=20) iiium, efslst, n, (xlast(i),i=1,n), &
          m 
        read (ires) ((q(j,i),j=1,m),i=1,m) 
        read (ires) ((r(j,i),j=1,n),i=1,n) 
        if (index(keywrd,'AIDER') /= 0) then
          read (ires) (aicorr(i),i=1,n)
          read (ires) (errfn(i),i=1,n)
        end if
        return  
      endif 
      if (mode == 1) then 
        write (iw, &
      '(2/10X,                                              ''- - - - - - - TIM&
      &E UP - - - - - - -'',2/)') 
        write (iw, '(10X,A)') ' - THE CALCULATION IS BEING DUMPED TO DISK', &
          '   RESTART IT USING THE KEY-WORD "RESTART"' 
        write (iw, '(/10X,''CURRENT VALUE OF GEOMETRY'',/)') 
        call geout (iw) 
      endif 
      write (ires) iiium, efslst, n, (xlast(i),i=1,n), m 
      write (ires) ((q(j,i),j=1,m),i=1,m) 
      write (ires) ((r(j,i),j=1,n),i=1,n) 
      if (index(keywrd,'AIDER') /= 0) write (ires) (aicorr(i),i=1,n) 
      if (index(keywrd,'AIDER') /= 0) write (ires) (errfn(i),i=1,n) 
!*****
!     The density matrix is required by ITER upon restart .
!
    
      write (iden) pa
      if (nalpha /= 0) write (iden) pb
!*****
      close(ires) 
      close(iden) 
      return  
   20 continue 
      write (iw, '(2/10X,''NO RESTART FILE EXISTS!'')') 
      call mopend ('NO RESTART FILE EXISTS!') 
      return  
      end subroutine parsav 
