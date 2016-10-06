      subroutine switch(mode) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE parameters_C, only : alp, guess1, guess2, guess3, &
      betas, betap, betad, uss, upp, udd, zs, zp, zd, zsn, zpn, zdn, &
      gss, gsp, gpp, gp2, hsp, hpp, am, ad, aq, dd, qq, &
      dorbs, polvol, ddp, po, pocord
!
!    
      USE molkst_C, only : keywrd, method_mndo, method_pm3, &
      & method_mndod  
      USE parameters_for_mndod_C, only : zsnd, zpnd, zdnd, ussd, uppd, uddd, &
      & zsd, zpd, zdd, betasd, betapd, betadd, alpd, gssd, gspd, &
      & gp2d, hspd, gppd, poc_mndod
      USE parameters_for_mndo_C, only :  ussm, uppm, uddm, zsm, zpm, zdm, betasm, &
        betapm, alpm, gssm, gspm, gppm, gp2m, hspm, polvom 
      USE parameters_for_pm3_C, only : usspm3, upppm3, uddpm3, zspm3, zppm3, &
        zdpm3, betasp, betapp, alppm3, gsspm3, gsppm3, gpppm3, &
        gp2pm3, hsppm3, guesp1, guesp2, guesp3  
      USE parameters_for_am1_C, only : zsam1, zpam1, zdam1, ussam1, uppam1, alpam1, &
      & gssam1, gppam1, gspam1, gp2am1, betasa, betapa, hspam1, guesa1, &
      & guesa2, guesa3
      USE chanel_C, only : iw 
!***********************************************************************
!
!   SWITCH copies data from the reference modules into the
!          arrays used by MOPAC.  The choice of reference data used
!          is given by the keywords MNDO, AM1, PM3, or MNDOD
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.4G  21:03:42  03/15/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use calpar_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mode 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i
      if (mode /= 1) then 
        if (method_mndo) then 
!
!    SWITCH IN MNDO PARAMETERS
!
            polvol = polvom 
            zs = zsm 
            zp = zpm 
            zd = zdm 
            uss = ussm 
            upp = uppm 
            udd = uddm 
            betas = betasm 
            betap = betapm 
            alp = alpm 
            gss = gssm 
            gpp = gppm 
            gsp = gspm 
            gp2 = gp2m 
            hsp = hspm 
        else if (method_pm3) then 
!
!    SWITCH IN MNDO-PM3 PARAMETERS
!
          guess1 = guesp1 
          guess2 = guesp2 
          guess3 = guesp3 
          zs = zspm3 
          zp = zppm3 
          zd = zdpm3 
          uss = usspm3 
          upp = upppm3 
          udd = uddpm3 
          betas = betasp 
          betap = betapp 
          alp = alppm3 
          gss = gsspm3 
          gpp = gpppm3 
          gsp = gsppm3 
          gp2 = gp2pm3 
          hsp = hsppm3 
        else if (method_mndod) then 
!
!    SWITCH IN MNDOD PARAMETERS
!
! *** MNDO/d PARAMETERS.
!     FIRST  SECTION IS STANDARD MNDO   PARAMETERS FOR I.LE.10.
!     SECOND SECTION SPECIFIC MNDO/d PARAMETERS FOR I.GT.10.  WARNING - tidy up
          do i = 1, 10 
            if (abs(ussm(i)) < 1.D-5) cycle  
            uss(i) = ussm(i) 
            upp(i) = uppm(i) 
            zs(i) = zsm(i) 
            zp(i) = zpm(i) 
            betas(i) = betasm(i) 
            betap(i) = betapm(i) 
            alp(i) = alpm(i) 
            gss(i) = gssm(i) 
            gsp(i) = gspm(i) 
            gpp(i) = gppm(i) 
            gp2(i) = gp2m(i) 
            hsp(i) = hspm(i) 
            hpp(i) = 0.5D0*(gppm(i)-gp2m(i)) 
            dorbs(i) = .FALSE. 
          end do 
          po(2,1) = 0.D0 
          po(3,1) = 0.D0 
          po(7,1) = 0.D0 
          dorbs = .FALSE. 
          do i = 11, 80 
            if (abs(ussd(i)) < 1.D-5) cycle  
            uss(i) = ussd(i) 
            upp(i) = uppd(i) 
            zs(i) = zsd(i) 
            zp(i) = zpd(i) 
            betas(i) = betasd(i) 
            betap(i) = betapd(i) 
            alp(i) = alpd(i) 
            gss(i) = gssd(i) 
            gsp(i) = gspd(i) 
            gpp(i) = gppd(i) 
            gp2(i) = gp2d(i) 
            hsp(i) = hspd(i) 
            hpp(i) = 0.5D0*(gppd(i)-gp2d(i)) 
            if (abs(uddd(i)) < 1.D-5) cycle
            udd(i) = uddd(i) 
            zd(i) = zdd(i) 
            betad(i) = betadd(i) 
            zsn(i) = zsnd(i) 
            zpn(i) = zpnd(i) 
            zdn(i) = zdnd(i) 
            dorbs(i) = .TRUE. 
          end do 
        else 
!
!    SWITCH IN AM1 PARAMETERS
!
          guess1 = guesa1 
          guess2 = guesa2 
          guess3 = guesa3 
          zs = zsam1 
          zp = zpam1 
          zd = zdam1 
          uss = ussam1 
          upp = uppam1 
          betas = betasa 
          betap = betapa 
          alp = alpam1 
          gss = gssam1 
          gpp = gppam1 
          gsp = gspam1 
          gp2 = gp2am1 
          hsp = hspam1 
          hpp = 0.5D0*(gpp - gp2) 
        endif 
        call calpar 
        do i = 1, 107 
          if (am(i) < 1.D-4) am(i) = 1.D0 
          po(1,i) = 0.5D0/am(i) 
          if (ad(i) > 1.D-5) po(2,i) = 0.5D0/ad(i) 
          if (aq(i) > 1.D-5) po(3,i) = 0.5D0/aq(i) 
          po(7,i) = po(1,i) 
          po(9,i) = po(1,i) 
          if (method_mndod .and. poc_mndod(i)>1.D-5) po(9,i) = poc_mndod(i)
          if (method_mndod .and. poc_mndod(i)>1.D-5) pocord(i) = poc_mndod(i)  
          ddp(2,i) = dd(i) 
          ddp(3,i) = qq(i)*sqrt(2.D0) 
        end do 
        po(2,1) = 0.D0 
        po(3,1) = 0.D0  
      endif 
      if (index(keywrd,' EXTERNAL')/=0 .and. mode/=1) return  
      if (uss(1) > (-1.D0)) then 
        write (iw, &
      '(''  THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN THIS PROGRAM'')') 
        call mopend (&
          'THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN THIS PROGRAM') 
        return  
      endif 
      return  
      end subroutine switch 
