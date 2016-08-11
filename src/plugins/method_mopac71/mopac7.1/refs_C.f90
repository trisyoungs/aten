      module refs_C 
      USE vast_kind_param, ONLY:  double 
!...Created by Pacific-Sierra Research 77to90  4.4G  11:05:00  03/09/06   
      character, dimension(107) :: refmn*80, refm3*80, refam*80, refpm3*80, &
        refmd*80  
      character, dimension(107,5) :: allref*80
      equivalence (refmn(1), allref(1,1)), (refm3(1), allref(1,2)), &
      & (refam(1), allref(1,3)), (refpm3(1), allref(1,4)), (refmd(1), allref(1,5))
      data refmd(11)/ &
      ' Na: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(12)/ &
      ' Mg: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(13)/ &
      ' Al: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(14)/ &
      ' Si: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. MOL. STRUCT., 313, 141 (1994&
      &)       '/  
      data refmd(15)/ &
      ' P : (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(16)/ &
      ' S : (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(17)/ &
      ' Cl: (MNDO/d):  W.THIEL AND A.A.VOITYUK, INTERN. J. QUANT. CHEM., 44, 80&
      &7 (1992)'/  
      data refmd(30)/ &
      ' Zn: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(35)/ &
      ' Br: (MNDO/d):  W.THIEL AND A.A.VOITYUK, INTERN. J. QUANT. CHEM., 44, 80&
      &7 (1992)'/  
      data refmd(48)/ &
      ' Cd: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmd(53)/ &
      ' I : (MNDO/d):  W.THIEL AND A.A.VOITYUK, INTERN. J. QUANT. CHEM., 44, 80&
      &7 (1992)'/  
      data refmd(80)/ &
      ' Hg: (MNDO/d):  W.THIEL AND A.A.VOITYUK, J. PHYS. CHEM., 100, 616 (1996)&
      &        '/  
      data refmn(1)/ &
      '  H: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977&
      &)       '/  
      data refmn(3)/ &
      ' Li: (MNDO):  TAKEN FROM MNDOC BY W.THIEL, QCPE NO.438, V. 2, P.63,&
      & (1982).'/  
      data refmn(4)/ &
      ' Be: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. CHEM. SOC., 100, 777, (19&
      &78)     '/  
      data refmn(5)/ &
      '  B: (MNDO):  M.J.S. DEWAR, M.L. MCKEE, J. AM. CHEM. SOC., 99, 5231, (19&
      &77)     '/  
      data refmn(6)/ &
      '  C: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977&
      &)       '/  
      data refmn(7)/ &
      '  N: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977&
      &)       '/  
      data refmn(8)/ &
      '  O: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHEM. SOC., 99, 4899, (1977&
      &)       '/  
      data refmn(9)/ &
      '  F: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. CHEM. SOC., 100, 777, (19&
      &78)     '/  
      data refmn(11)/ &
        ' Na: (MNDO):  ANGEW. CHEM. INT. ED. ENGL. 29,   1042 (1990).'/  
      data refam(11)/ &
      ' Na: (AM1):   SODIUM-LIKE SPARKLE.   USE WITH CARE.                     &
      &        '/  
      data refpm3(11)/ &
      ' Na: (PM3):   SODIUM-LIKE SPARKLE.   USE WITH CARE.                     &
      &        '/  
      data refmn(13)/ &
      ' Al: (MNDO):  L.P. DAVIS, ET.AL.  J. COMP. CHEM., 2, 433, (1981) SEE MAN&
      &UAL.    '/  
      data refmn(14)/ &
      ' Si: (MNDO): M.J.S.DEWAR, ET. AL. ORGANOMETALLICS  5, 375 (1986)        &
      &        '/  
      data refmn(15)/ &
      '  P: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA, J. AM. CHEM. SOC., 100 3&
      &607 1978'/  
      data refmn(16)/ &
      '  S: (MNDO):  M.J.S.DEWAR, C.H. REYNOLDS, J. COMP. CHEM. 7, 140-143 (198&
      &6)      '/  
      data refmn(17)/ &
      ' Cl: (MNDO): M.J.S.DEWAR, H.S.RZEPA, J. COMP. CHEM., 4, 158, (1983)     &
      &        '/  
      data refmn(19)/ ' K: (MNDO): J. C. S. CHEM. COMM. 765, (1992).'/  
      data refam(19)/ &
      ' K: (AM1): POTASSIUM-LIKE SPARKLE.   USE WITH CARE.                     &
      &        '/  
      data refpm3(19)/ &
      ' K: (PM3): POTASSIUM-LIKE SPARKLE.   USE WITH CARE.                     &
      &        '/  
      data refmn(30)/ &
      ' Zn: (MNDO):  M.J.S. DEWAR, K.M. MERZ, ORGANOMETALLICS, 5, 1494-1496 (19&
      &86)     '/  
      data refmn(32)/ &
      ' Ge: (MNDO): M.J.S.DEWAR, G.L.GRADY, E.F.HEALY,ORGANOMETALLICS 6 186-189&
      &, (1987)'/  
      data refmn(35)/ &
      ' Br: (MNDO): M.J.S.DEWAR, E.F. HEALY, J. COMP. CHEM., 4, 542, (1983)    &
      &        '/  
      data refmn(50)/ &
      ' Sn: (MNDO): M.J.S.DEWAR,G.L.GRADY,J.J.P.STEWART, J.AM.CHEM.SOC.,106 677&
      &1 (1984)'/  
      data refmn(53)/ &
      '  I: (MNDO): M.J.S.DEWAR, E.F. HEALY, J.J.P. STEWART, J.COMP.CHEM., 5,35&
      &8,(1984)'/  
      data refmn(80)/ &
      ' Hg: (MNDO): M.J.S.DEWAR,  ET. AL. ORGANOMETALLICS 4, 1964, (1985) SEE M&
      &ANUAL   '/  
      data refmn(82)/ &
      ' Pb: (MNDO): M.J.S.DEWAR, ET.AL ORGANOMETALLICS 4 1973-1980 (1985)      &
      &        '/  
      data refmn(90)/ &
      ' Si: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA, J. AM. CHEM. SOC., 100 3&
      &607 1978'/  
      data refmn(91)/ &
      '  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE, J.AM.CHEM.SOC.100, 3607&
      & (1978).'/  
      data refmn(102)/ &
      ' Cb: (MNDO):  Capped Bond  (Hydrogen-like, takes on a  zero charge.)    &
      &        '/  
      data refam(1)/ &
      '  H: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-3909 (1985)  &
      &        '/  
      data refam(4)/ &
      ' Be: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. CHEM. SOC., 100, 777, (19&
      &78)     '/  
      data refam(5)/ &
      '  B: (AM1):  M.J.S. DEWAR, C. JIE, E. G. ZOEBISCH ORGANOMETALLICS 7, 513&
      & (1988) '/  
      data refam(6)/ &
      '  C: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-3909 (1985)  &
      &        '/  
      data refam(7)/ &
      '  N: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-3909 (1985)  &
      &        '/  
      data refam(8)/ &
      '  O: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC. 107 3902-3909 (1985)  &
      &        '/  
      data refam(9)/ &
      '  F: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THEOCHEM, 180, 1 (1988).   &
      &        '/  
      data refam(13)/ &
      ' Al: (AM1):  M. J. S. Dewar, A. J. Holder, Organometallics, 9, 508-511 (&
      &1990).  '/  
      data refam(14)/ &
      ' Si: (AM1): M.J.S.DEWAR, C. JIE, ORGANOMETALLICS, 6, 1486-1490 (1987).  &
      &        '/  
      data refam(15)/ &
      '  P: (AM1): M.J.S.DEWAR, JIE, C, THEOCHEM, 187, 1 (1989)                &
      &        '/  
      data refam(16)/ &
      '  S: (AM1): M.J.S. DEWAR, Y-C YUAN, INORGANIC CHEMISTRY, 29, 3881:3890, &
      &(1990)  '/  
      data refam(17)/ &
      ' Cl: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THEOCHEM, 180, 1 (1988).   &
      &        '/  
      data refam(30)/ &
      ' Zn: (AM1):  M.J.S. DEWAR, K.M. MERZ, ORGANOMETALLICS, 7, 522-524 (1988)&
      &       '/  
      data refam(32)/ &
      ' Ge: (AM1): M.J.S.Dewar and C.Jie, Organometallics, 8, 1544, (1989)     &
      &        '/  
      data refam(33)/ &
      ' As: (AM1): J. J. P. STEWART                                            &
      &        '/ 
      data refam(34)/ &
      ' Se: (AM1): J. J. P. STEWART                                            &
      &        '/  
      data refam(35)/ &
      ' Br: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THEOCHEM, 180, 1 (1988).   &
      &        '/  
      data refam(51)/ &
      ' Sb: (AM1): J. J. P. STEWART                                            &
      &        '/ 
      data refam(52)/ &
      ' Te: (AM1): J. J. P. STEWART                                            &
      &        '/  
      data refam(53)/ &
      '  I: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THEOCHEM, 180, 1 (1988).   &
      &        '/  
      data  refam( 57) / &
      ' La: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    '/     
      data  refam( 58) / & 
      ' Ce: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    '/     
      data  refam( 59) / & 
      ' Pr: (AM1): R.O. Freire, et.al, J. Organometallic Chemistry 690 (2005) 4&
      &099     '/  
      data  refam( 60) / & 
      ' Nd: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    '/     
      data  refam( 61) / & 
      ' Pm: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    '/     
      data  refam( 62) / &
      " Sm: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/     
      data  refam( 63) / &
      " Eu: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/ 
      data  refam( 64) / &
      " Gd: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/ 
      data  refam( 65) / &
      " Tb: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/ 
      data  refam( 66) / &
      " Dy: (AM1): N.B. da Costa Jr, et.al. Inorg. Chem. Comm. 8 (2005) 831.   &
      &        "/ 
      data  refam( 67) / &
      " Ho: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/ 
      data  refam( 68) / &
      " Er: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/ 
      data  refam( 69) / &
      " Tm: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Chem. Phys. Let., 411 (&
      &2005) 61"/ 
      data  refam( 65) / &
      " Yb: (AM1): R.O. Freire, G.B. Rocha, A.M.  Simas, J. Comp. Chem., 26 (20&
      &05) 1524"/        
      data  refam( 71) / & 
      " Lu: (AM1): R.O. Freire, G.B. Rocha, A.M. Simas, Inorg. Chem.44 (2005) 3&
      &299.    "/     
      data refam(80)/ &
      ' Hg: (AM1): M.J.S.Dewar and C.Jie, Organometallics 8, 1547, (1989)      &
      &        '/  
      data refpm3(1)/ &
      '  H: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(3)/ &
      ' Li: (PM3): E. ANDERS, R. KOCH, P. FREUNSCHT, J. COMP. CHEM 14 1301-1&
      &312 1993'/  
      data refpm3(4)/ &
      ' Be: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(6)/ &
      '  C: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(7)/ &
      '  N: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(8)/ &
      '  O: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(9)/ &
      '  F: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(12)/ &
      ' Mg: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(13)/ &
      ' Al: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(14)/ &
      ' Si: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(15)/ &
      '  P: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(16)/ &
      '  S: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(17)/ &
      ' Cl: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(30)/ &
      ' Zn: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(31)/ &
      ' Ga: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(32)/ &
      ' Ge: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(33)/ &
      ' As: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(34)/ &
      ' Se: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(35)/ &
      ' Br: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(48)/ &
      ' Cd: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(49)/ &
      ' In: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(50)/ &
      ' Sn: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(51)/ &
      ' Sb: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(52)/ &
      ' Te: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(53)/ &
      '  I: (PM3): J. J. P. STEWART, J. COMP. CHEM. 10, 209 (1989).        &
      &        '/  
      data refpm3(80)/ &
      ' Hg: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(81)/ &
      ' Tl: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(82)/ &
      ' Pb: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(83)/ &
      ' Bi: (PM3): J. J. P. STEWART, J. COMP. CHEM. 12, 320-341 (1991).     &
      &        '/  
      data refpm3(102)/ &
      ' Cb: (PM3):  Capped Bond  (Hydrogen-like, takes on a  zero charge.)     &
      &        '/  
      end module refs_C 
