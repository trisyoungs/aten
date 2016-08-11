      subroutine calpar 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE parameters_C, only : alp, ios, iop, iod, qq, am, ad, aq, dd, &
      guess1, guess2, guess3, gpp, gp2, hsp, gss, gsp, betas, betap, betad, &
      zs, zp, zd, uss, upp, udd, eisol
      USE elemts_C, only : elemnt 
      USE funcon_C, only : ev
      USE molkst_C, only : keywrd
      USE chanel_C, only : iw
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:24:24  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
     
      integer , dimension(107) :: nspqn 
      integer :: i, k, l, jmax, j 
      real(double), dimension(107) :: gssc, gspc, hspc, gp2c, gppc 
      real(double) :: p, p4,  hpp, qn, gdd1, d1, d2, df, hsp1, hsp2, d3, &
        gqq, q1, q2, qf, hpp1, hpp2, q3 

      save nspqn 
!----------------------------------------------- 
!#      SAVE GSDC, GDDC
      data nspqn/ 2*1, 8*2, 8*3, 18*4, 18*5, 32*6, 21*0/  
!
! THE CONTINUATION LINES INDICATE THE PRINCIPAL QUANTUM NUMBER.
!
!
!     SET SCALING PARAMETER.
      p = 2.D0 
      p4 = p**4  
      do i = 2, 98 
!
!  GSSC is the number of two-electron terms of type <SS|SS>
!
        gssc(i) = max(ios(i)-1,0) 
        k = iop(i) 
!
!  GSPC is the number of two-electron terms of type <SS|PP>
!
        gspc(i) = ios(i)*k 
        l = min(k,6 - k) 
!
!  GP2C is the number of two-electron terms of type <PP|PP>
!       plus 0.5 of the number of HPP integrals.
!  (HPP is not used; instead it is replaced by 0.5(GPP-GP2))
!
        gp2c(i) = (k*(k - 1))/2 + 0.5D0*(l*(l - 1))/2 
!
!  GPPC is minus 0.5 times the number of HPP integrals.
!
        gppc(i) = -0.5D0*(l*(l - 1))/2 
!
!  HSPC is the number of two-electron terms of type <SP|SP>.
!       (S and P must have the same spin.  In all cases, if
!  P is non-zero, there are two S electrons)
!
        hspc(i) = -k 
!
!
        if (zp(i)<1.D-4 .and. zs(i)<1.D-4) cycle  
!*********************************************************************
!
!   CONSTRAINTS ON THE POSSIBLE VALUES OF PARAMETERS
!
!*********************************************************************
        zp(i) = dmax1(0.3D0,zp(i)) 
!  PUT IN ANY CONSTRAINTS AT THIS POINT
!*********************************************************************
        hpp = 0.5D0*(gpp(i)-gp2(i)) 
        hpp = max(0.1D0,hpp) 
        hsp(i) = max(1.D-7,hsp(i)) 
        eisol(i) = uss(i)*ios(i) + upp(i)*iop(i) + udd(i)*iod(i) + gss(i)*gssc(&
          i) + gpp(i)*gppc(i) + gsp(i)*gspc(i) + gp2(i)*gp2c(i) + hsp(i)*hspc(i&
          ) 
        qn = nspqn(i) 
        dd(i) = (2.D0*qn + 1)*(4.D0*zs(i)*zp(i))**(qn + 0.5D0)/(zs(i)+zp(i))**(&
          2.D0*qn + 2)/sqrt(3.D0) 
        qq(i) = sqrt((4.D0*qn*qn + 6.D0*qn + 2.D0)/20.D0)/zp(i) 
!     CALCULATE ADDITIVE TERMS, IN ATOMIC UNITS.
        jmax = 5 
        gdd1 = (hsp(i)/(ev*dd(i)**2))**(1.D0/3.D0) 
        d1 = gdd1 
        d2 = gdd1 + 0.04D0 
        do j = 1, jmax 
          df = d2 - d1 
          hsp1 = 0.5D0*d1 - 0.5D0/sqrt(4.D0*dd(i)**2+1.D0/d1**2) 
          hsp2 = 0.5D0*d2 - 0.5D0/sqrt(4.D0*dd(i)**2+1.D0/d2**2) 
          if (abs(hsp2 - hsp1) < 1.D-25) exit  
          d3 = d1 + df*(hsp(i)/ev-hsp1)/(hsp2 - hsp1) 
          d1 = d2 
          d2 = d3 
        end do 
        gqq = (p4*hpp/(ev*48.D0*qq(i)**4))**0.2D0 
        q1 = gqq 
        q2 = gqq + 0.04D0 
        do j = 1, jmax 
          qf = q2 - q1 
          hpp1 = 0.25D0*q1 - 0.5D0/sqrt(4.D0*qq(i)**2+1.D0/q1**2) + 0.25D0/&
            sqrt(8.D0*qq(i)**2+1.D0/q1**2) 
          hpp2 = 0.25D0*q2 - 0.5D0/sqrt(4.D0*qq(i)**2+1.D0/q2**2) + 0.25D0/&
            sqrt(8.D0*qq(i)**2+1.D0/q2**2) 
          if (abs(hpp2 - hpp1) < 1.D-25) exit  
          q3 = q1 + qf*(hpp/ev - hpp1)/(hpp2 - hpp1) 
          q1 = q2 
          q2 = q3 
        end do 
        am(i) = gss(i)/ev 
        ad(i) = d2 
        aq(i) = q2 
      end do 
      do i = 1, 107 
        if (am(i) < 1.d-20) then
          if (gss(i) > 1.d-20) then
            am(i) = gss(i)/ev
          else
            am(i) = 1.d0
          end if
        end if
      end do 
      eisol(1) = uss(1) 
      am(1) = gss(1)/ev 
      ad(1) = am(1) 
      aq(1) = am(1) 
!
!   Atomic number 102 is the capped bond.  It should have a very
!   small AM to prevent division by zero in REPP, and to avoid
!   spurious effects due to normal AM values.
!
      am(102) = 1.D-10 
!
!     DEBUG PRINTING.
!     THIS IS FORMATTED FOR DIRECT INSERTION INTO 'PARAM'
!
      if (index(keywrd,' DEP ') == 0) return  
      write (iw, 70) 
      do i = 1, 107 
        if (zs(i) < 1.d-20) cycle  
        write (iw, '(''C'',20X,''DATA FOR ELEMENT'',I3)') i 
        write (iw, '(A,I3,3A)') '      DATA REFPM3 (', i, ')/''  ', elemnt(i), &
          ' (PM3)' 
        write (iw, '(A)') '     1                                ''/' 
        write (iw, '(6X,''DATA USSPM3('',I3,'')/'',F16.7,''D0/'')') i, uss(i) 
        if (upp(i) /= 0.D0) write (iw, &
          '(6X,''DATA UPPPM3('',I3,'')/'',F16.7,''D0/'')') i, upp(i) 
        if (udd(i) /= 0.D0) write (iw, &
          '(6X,''DATA UDDPM3('',I3,'')/'',F16.7,''D0/'')') i, udd(i) 
        if (betas(i) /= 0.D0) write (iw, &
          '(6X,''DATA BETASP('',I3,'')/'',F16.7,''D0/'')') i, betas(i) 
        if (betap(i) /= 0.D0) write (iw, &
          '(6X,''DATA BETAPP('',I3,'')/'',F16.7,''D0/'')') i, betap(i) 
        if (betad(i) /= 0.D0) write (iw, &
          '(6X,''DATA BETADP('',I3,'')/'',F16.7,''D0/'')') i, betad(i) 
        write (iw, '(6X,''DATA ZSPM3 ('',I3,'')/'',F16.7,''D0/'')') i, zs(i) 
        if (zp(i) /= 0.D0) write (iw, &
          '(6X,''DATA ZPPM3 ('',I3,'')/'',F16.7,''D0/'')') i, zp(i) 
        if (zd(i) /= 0.D0) write (iw, &
          '(6X,''DATA ZDPM3 ('',I3,'')/'',F16.7,''D0/'')') i, zd(i) 
        write (iw, '(6X,''DATA ALPPM3('',I3,'')/'',F16.7,''D0/'')') i, alp(i) 
        write (iw, '(6X,''DATA EISOLP('',I3,'')/'',F16.7,''D0/'')') i, eisol(i) 
        if (gss(i) /= 0.D0) write (iw, &
          '(6X,''DATA GSSPM3('',I3,'')/'',F16.7,''D0/'')') i, gss(i) 
        if (gsp(i) /= 0.D0) write (iw, &
          '(6X,''DATA GSPPM3('',I3,'')/'',F16.7,''D0/'')') i, gsp(i) 
        if (gpp(i) /= 0.D0) write (iw, &
          '(6X,''DATA GPPPM3('',I3,'')/'',F16.7,''D0/'')') i, gpp(i) 
        if (gp2(i) /= 0.D0) write (iw, &
          '(6X,''DATA GP2PM3('',I3,'')/'',F16.7,''D0/'')') i, gp2(i) 
        if (hsp(i) /= 0.D0) write (iw, &
          '(6X,''DATA HSPPM3('',I3,'')/'',F16.7,''D0/'')') i, hsp(i) 
        if (dd(i) /= 0.D0) write (iw, &
          '(6X,''DATA DDPM3 ('',I3,'')/'',F16.7,''D0/'')') i, dd(i) 
        if (qq(i) /= 0.D0) write (iw, &
          '(6X,''DATA QQPM3 ('',I3,'')/'',F16.7,''D0/'')') i, qq(i) 
        write (iw, '(6X,''DATA AMPM3 ('',I3,'')/'',F16.7,''D0/'')') i, am(i) 
        if (ad(i) /= 0.D0) write (iw, &
          '(6X,''DATA ADPM3 ('',I3,'')/'',F16.7,''D0/'')') i, ad(i) 
        if (aq(i) /= 0.D0) write (iw, &
          '(6X,''DATA AQPM3 ('',I3,'')/'',F16.7,''D0/'')') i, aq(i) 
        do j = 1, 4 
          if (guess1(i,j) == 0.D0) cycle  
          write (iw, &
            '(6X,''DATA GUESP1('',I3,'','',I1,'')/'',          F16.7,''D0/'')')&
             i, j, guess1(i,j) 
          write (iw, &
            '(6X,''DATA GUESP2('',I3,'','',I1,'')/'',          F16.7,''D0/'')')&
             i, j, guess2(i,j) 
          write (iw, &
            '(6X,''DATA GUESP3('',I3,'','',I1,'')/'',          F16.7,''D0/'')')&
             i, j, guess3(i,j) 
        end do 
   70   format(' ',1x,'OUTPUT INCLUDES DEBUG INFORMATION',/,/) 
      end do 
      return  
      end subroutine calpar 
