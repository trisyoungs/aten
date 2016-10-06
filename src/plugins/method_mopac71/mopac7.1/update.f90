      subroutine update(iparam, ielmnt, param, kfn) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE parameters_C, only : alp, zs, zp, zd, zsn, zpn, zdn, uss, upp, udd, &
      guess1, guess2, guess3, gpp, gp2, hsp, gss, gsp, betas, betap, betad, &
      natorb, po
      use chanel_C, only : iw
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:04  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iparam 
      integer , intent(in) :: ielmnt 
      integer , intent(in) :: kfn 
      real(double) , intent(in) :: param 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i 
!-----------------------------------------------
!***********************************************************************
!
!  UPDATE UPDATES THE MODULES WHICH HOLD ALL THE PARAMETERS FOR
!         RUNNING MNDO.
!         IPARAM REFERS TO THE TYPE OF PARAMETER,
!         IELMNT REFERS TO THE ELEMENT,
!         PARAM IS THE VALUE OF THE PARAMETER, AND
!
!*********************************************************************** 
      select case (iparam)  
      case default 
        uss(ielmnt) = param 
        return  
      case (2)  
        upp(ielmnt) = param 
        return  
      case (3)  
        udd(ielmnt) = param 
        return  
      case (4)  
        zs(ielmnt) = param 
        return  
      case (5)  
        zp(ielmnt) = param 
        return  
      case (6)  
        zd(ielmnt) = param 
        return  
      case (7)  
        betas(ielmnt) = param 
        return  
      case (8)  
        betap(ielmnt) = param 
        return  
      case (9)  
        betad(ielmnt) = param 
        return  
      case (10)  
        gss(ielmnt) = param 
        return  
      case (11)  
        gsp(ielmnt) = param 
        return  
      case (12)  
        gpp(ielmnt) = param 
        return  
      case (13)  
        gp2(ielmnt) = param 
        return  
      case (14)  
        hsp(ielmnt) = param 
        return  
      case (15)  
        return  
      case (16)  
        return  
      case (17)  
        write (iw, *) ' ERROR IN UPDATE' 
        call mopend ('Error in UPDATE') 
        return  
      case (18)  
        alp(ielmnt) = param 
        return  
      case (19)  
        return  
      case (20)  
        return  
      case (21)  
        return  
      case (22)  
        natorb(ielmnt) = nint(param) 
        i = int(param + 0.5D0) 
        if (i/=9 .and. i/=4 .and. i/=1) then 
          write (iw, &
            '(3/10X,'' UNACCEPTABLE VALUE FOR NO. OF ORBITALS'', '' ON ATOM'')'&
            ) 
          call mopend ('UNACCEPTABLE VALUE FOR NO. OF ORBITALS ON ATOM') 
          return  
        endif 
        return  
      case (23)  
        zsn(ielmnt) = param 
        return  
      case (24)  
        zpn(ielmnt) = param 
        return  
      case (25)  
        zdn(ielmnt) = param 
        return  
      case (26)  
        po(9,ielmnt) = param 
        return  
      case (27)  
        guess1(ielmnt,kfn) = param 
        return  
      case (28)  
        guess2(ielmnt,kfn) = param 
        return  
      case (29)  
        guess3(ielmnt,kfn) = param 
        return  
      end select 
      end subroutine update 
