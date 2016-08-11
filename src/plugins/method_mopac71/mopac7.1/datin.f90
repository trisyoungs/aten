      subroutine datin() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE parameters_C, only : dd, qq, eheat, eisol, ddp, po, pocord
      USE parameters_for_mndod_C, only : 
      use molkst_C, only : keywrd, moperr, numat, atheat  
      use funcon_C, only : fpc_9
      use permanent_arrays, only : nat
      use chanel_C, only : iw, iext
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use upcase_I 
      use mopend_I 
      use reada_I 
      use update_I 
      use calpar_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(5,1000) :: ijpars 
      integer :: lpars, i, j, l, nparas, it, iparam, kfn, k, ielmnt 
      real(double), dimension(1000) :: parsij 
      real(double) :: param, eth 
      logical :: once 
      character , dimension(0:9) :: numbrs 
      character , dimension(29) :: partyp*5 
      character :: files*64, dummy*50, text*50, txtnew*50 
      character, dimension(107) :: elemnt*2 

      save numbrs, partyp, elemnt, lpars 
!-----------------------------------------------
      data numbrs/ ' ', '1', '2', '3', '4', '5', '6', '7', '8', '9'/  
      data partyp/ 'USS  ', 'UPP  ', 'UDD  ', 'ZS   ', 'ZP   ', 'ZD   ', &
        'BETAS', 'BETAP', 'BETAD', 'GSS  ', 'GSP  ', 'GPP  ', 'GP2  ', 'HSP  '&
        , 'AM1  ', 'EXPC ', 'GAUSS', 'ALP  ', 'GSD  ', 'GPD  ', 'GDD  ', &
        'ORBS ', 'ZSN  ', 'ZPN  ', 'ZDN  ', 'POCOR', 'FN1  ', 'FN2  ', 'FN3  '&
        /  
      data (elemnt(i),i=1,107)/ 'H ', 'HE', 'LI', 'BE', 'B ', 'C ', 'N ', 'O '&
        , 'F ', 'NE', 'NA', 'MG', 'AL', 'SI', 'P ', 'S ', 'CL', 'AR', 'K ', &
        'CA', 'SC', 'TI', 'V ', 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN', 'GA'&
        , 'GE', 'AS', 'SE', 'BR', 'KR', 'RB', 'SR', 'Y ', 'ZR', 'NB', 'MO', &
        'TC', 'RU', 'RH', 'PD', 'AG', 'CD', 'IN', 'SN', 'SB', 'TE', 'I ', 'XE'&
        , 'CS', 'BA', 'LA', 'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', &
        'DY', 'HO', 'ER', 'TM', 'YB', 'LU', 'HF', 'TA', 'W ', 'RE', 'OS', 'IR'&
        , 'PT', 'AU', 'HG', 'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 'RA', &
        'AC', 'TH', 'PA', 'U ', 'NP', 'PU', 'AM', 'CM', 'BK', 'CF', 'XX', 'FM'&
        , 'MD', 'CB', '++', '+', '--', '-', 'TV'/  
      lpars = 0 
      parsij = 0.0D0 
      ijpars = 0 
      l = 1 
   25 continue 
      i = index(keywrd(l:),'EXTERNAL=') + 9 
      if (i == 9) go to 105 
      l = l + i 
      i = l - 1 
      j = index(keywrd(i:),' ') + i - 1 
      files = keywrd(i:j) 
      open(iext, status='UNKNOWN', file=files, position='asis') 
      once = .TRUE. 
      i = 0 
      nparas = 0 
   30 continue 
      read (iext, '(A40)', err=100, end=100) text 
      if (once) then 
        write (iw, &
      '(2/5X,                                                        '' PARAMET&
      &ER TYPE      ELEMENT    PARAMETER'')') 
        once = .FALSE. 
      endif 
      nparas = nparas + 1 
      if (text == ' ') go to 100 
      if (index(text,'END') /= 0) go to 100 
      call upcase (text, 50) 
      if (index(text,'END') /= 0) go to 100 
      do j = 1, 29 
        if (j > 25) then 
          it = index(text,'FN') 
          txtnew = text(1:it+2) 
          if (index(txtnew,partyp(j)) /= 0) go to 50 
        endif 
        if (index(text,partyp(j)) /= 0) go to 50 
      end do 
      write (iw, '(''  FAULTY LINE:'',A)') txtnew 
      write (iw, '(''  FAULTY LINE:'',A)') text 
      write (iw, '(''   NAME NOT FOUND'')') 
      call mopend ('NAME NOT FOUND') 
      return  
   50 continue 
      iparam = j 
      if (iparam > 26) then 
        i = index(text,'FN') 
        kfn = nint(reada(text,i + 3)) 
      else 
        kfn = 0 
        i = index(text,partyp(j)) 
      endif 
      k = index(text(i:),' ') + 1 
      dummy = text(k:) 
      text = dummy 
      do j = 1, 107 
        if (index(text,' '//elemnt(j)) /= 0) go to 70 
      end do 
      write (iw, '('' ELEMENT NOT FOUND '')') 
      write (iw, *) ' FAULTY LINE: "'//text//'"' 
      call mopend ('ELEMENT NOT FOUND') 
      return  
   70 continue 
      ielmnt = j 
      param = reada(text,index(text,elemnt(j))) 
      do i = 1, lpars 
        if (ijpars(1,i)==kfn .and. ijpars(2,i)==ielmnt .and. ijpars(3,i)==&
          iparam) go to 90 
      end do 
      lpars = lpars + 1 
      i = lpars 
   90 continue 
      ijpars(1,i) = kfn 
      ijpars(2,i) = ielmnt 
      ijpars(3,i) = iparam 
      parsij(i) = param 
      go to 30 
  100 continue 
      if (nparas == 0) then 
        write (iw, '(2/10X,A)') ' EXTERNAL PARAMETERS FILE  MISSING OR EMPTY' 
        call mopend ('EXTERNAL PARAMETERS FILE MISSING OR EMPTY') 
        return  
      endif 
      close(iext) 
      go to 25 
  105 continue 
      do j = 1, 107 
        do k = 1, 29 
          do i = 1, lpars 
            iparam = ijpars(3,i) 
            kfn = ijpars(1,i) 
            ielmnt = ijpars(2,i) 
            if (iparam /= k) cycle  
            if (ielmnt /= j) cycle  
            param = parsij(i) 
            if (kfn /= 0) then 
              write (iw, '(10X,A6,11X,A2,F17.6)') partyp(iparam)(:3)//numbrs(&
                kfn)//'  ', elemnt(ielmnt), param 
            else 
              write (iw, '(10X,A6,11X,A2,F17.6)') partyp(iparam)//numbrs(kfn), &
                elemnt(ielmnt), param 
            endif 
            call update (iparam, ielmnt, param, kfn) 
            if (.not.moperr) cycle  
            return  
          end do 
        end do 
      end do 
      call moldat (1) 
      if (moperr) return  
      call calpar 
      do i = 1, 107 
        po(7,i) = po(1,i) 
        po(9,i) = po(1,i) 
        if (pocord(i) > 1.D-5) po(9,i) = pocord(i) 
        ddp(2,i) = dd(i) 
        ddp(3,i) = qq(i)*sqrt(2.D0) 
      end do 
      eth = 0.D0 
      atheat = sum(eheat(nat(:numat))) 
      eth = sum(eisol(nat(:numat))) 
      atheat = atheat - eth*fpc_9
      return  
      end subroutine datin 
