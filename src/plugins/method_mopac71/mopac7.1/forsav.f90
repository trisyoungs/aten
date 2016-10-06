      subroutine forsav(time, deldip, ipt, fmatrx, coord, nvar, refh, evecs, &
        jstart, fconst, pa, pb) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : nalpha, mpack, jobnam
      USE chanel_C, only : iw, ires, iden
!...Translated by Pacific-Sierra Research 77to90  4.4G  07:59:25  03/16/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: ipt 
      integer , intent(in) :: nvar 
      integer  :: jstart 
      real(double)  :: time 
      real(double)  :: refh 
      real(double)  :: deldip(3,*) 
      real(double)  :: fmatrx((nvar*(nvar+1))/2) 
      real(double)  :: coord(nvar) 
      real(double)  :: evecs(nvar**2) 
      real(double)  :: fconst(nvar) 
      real(double) , intent(in) :: pa(mpack) 
      real(double) , intent(in) :: pb(mpack) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, i99, j, linear, n33 
      logical :: opend 
!-----------------------------------------------
!***********************************************************************
!
!  FORSAV SAVES AND RESTORES DATA USED IN THE FORCE CALCULATION.
!
! ON INPUT TIME = TOTAL TIME ELAPSED SINCE THE START OF THE CALCULATION.
!          IPT  = LINE OF FORCE MATRIX REACHED, IF IN WRITE MODE,
!               = 0 IF IN READ MODE.
!        FMATRX = FORCE MATRIX
!***********************************************************************
      i = index(jobnam,' ') - 1 
      i99 = 0 
   10 continue 
      j = i99 
      inquire(unit=ires, opened=opend) 
      if (opend) close(unit=ires, status='KEEP') 
      open(unit=ires, file=jobnam(:i)//'.res', status='UNKNOWN', form=&
        'UNFORMATTED', iostat=i99, position='asis') 
      if (i99 /= 0) then 
        if (j /= 0) then 
          write (iw, *) ' Fatal error in trying to open RESTART file' 
          call mopend ('Fatal error in trying to open RESTART file') 
          return  
        endif 
        open(unit=ires, file=jobnam(:i)//'.res', status='OLD', iostat=i99, &
          position='asis') 
        close(ires, status='DELETE') 
        go to 10 
      endif 
      rewind ires 
      inquire(unit=iden, opened=opend) 
      if (opend) close(unit=iden, status='KEEP') 
      open(unit=iden, file=jobnam(:i)//'.den', status='UNKNOWN', form=&
        'UNFORMATTED', position='asis') 
      rewind iden 
      if (ipt == 0) then 
!
!   READ IN FORCE DATA
!
        read (ires, end=30, err=30) time, ipt, refh 
        linear = (nvar*(nvar + 1))/2 
        read (ires) (coord(i),i=1,nvar) 
        read (ires, end=20, err=20) (fmatrx(i),i=1,linear) 
        read (ires) ((deldip(j,i),j=1,3),i=1,ipt) 
        n33 = nvar*nvar 
        read (ires) (evecs(i),i=1,n33) 
        read (ires) jstart, (fconst(i),i=1,nvar) 
        return  
      else 
!
!    WRITE FORCE DATA
!
        rewind ires 
        if (time > 1.D7) time = time - 1.D7 
        write (ires) time, ipt, refh 
        linear = (nvar*(nvar + 1))/2 
        write (ires) (coord(i),i=1,nvar) 
        write (ires) (fmatrx(i),i=1,linear) 
        write (ires) ((deldip(j,i),j=1,3),i=1,ipt) 
        n33 = nvar*nvar 
        write (ires) (evecs(i),i=1,n33) 
        write (ires) jstart, (fconst(i),i=1,nvar)  
        write (iden) pa 
        if (nalpha /= 0) write (iden) pb
        close(ires) 
        close(iden) 
      endif 
      return  
   20 continue 
      write (iw, &
      '(10X,''INSUFFICIENT DATA ON DISK FILES FOR A FORCE '',  ''CALCULATION'',&
      &/10X,''RESTART. PERHAPS THIS STARTED OF AS A '',  ''FORCE CALCULATION '')&
      &') 
      write (iw, &
      &'(10X,''BUT THE GEOMETRY HAD TO BE OPTIMIZED FIRST, '',  ''IN WHICH CASE &
      &'',/10X,''REMOVE THE KEY-WORD "FORCE".'')') 
      call mopend (&
         'INSUFFICIENT DATA ON DISK FILES FOR A FORCE CALCULATION RESTART') 
      return  
   30 continue 
      write (iw, '(2/10X,''NO RESTART FILE EXISTS!'')') 
      call mopend ('NO RESTART FILE EXISTS!') 
      return  
      end subroutine forsav 
