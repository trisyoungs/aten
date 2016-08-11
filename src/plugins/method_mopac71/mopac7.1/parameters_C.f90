      module parameters_C
!
! This module holds all the parameters for the elements.  It is filled
! from the reference parameter sets 
!
      USE vast_kind_param, ONLY:  double 
!...Created by Pacific-Sierra Research 77to90  4.4G  11:04:16  03/09/06  
    logical, dimension(107) :: dorbs
    real(double), dimension(107) :: alp 
    real(double), dimension(107,4) :: guess1, guess2, guess3 
    real(double), dimension(6,107) :: ddp 
    real(double), dimension(9,107) :: po 
    real(double), dimension(107) :: betas, betap, betad 
    real(double), dimension(107) :: uss, upp, udd
    real(double), dimension(107) :: gpp, gp2, hsp, gss, gsp, hpp
    real(double), dimension(107) :: am, ad, aq, dd, qq
    real(double), dimension(107) :: zs, zp, zd, zsn, zpn, zdn
    real(double), dimension(107) :: eisol, eheat
    real(double), dimension(107) :: ams 
    real(double), dimension (107) :: tore, polvol, pocord, f0dd, f2dd, f4dd, &
    f0sd, g2sd, f0pd, f2pd, g1pd, g3pd 
    integer, dimension (107,3) :: npq
    integer, dimension (107) :: iod, iop, ios
    integer, dimension (107) :: natsp, natorb, natspd
    logical, dimension (107) :: main_group
   
    save
!              H           Initial "s" Orbital Occupancies                     He
!              Li Be                                            B  C  N  O  F  Ne
!              Na Mg                                            Al Si P  S  Cl Ar
!              K  Ca Sc            Ti V  Cr Mn Fe Co Ni Cu Zn   Ga Ge As Se Br Kr
!              Rb Sr Y             Zr Nb Mo Tc Ru Rh Pd Ag Cd   In Sn Sb Te I  Xe
!              Cs Ba La Ce-Lu      Hf Ta W  Re Os Ir Pt Au Hg   Tl Pb Bi Po At Rn
!              Fr Ra Ac Th Pa U    Np Pu Am Cm Bk Cf            Cb ++ +  -- -  Tv
!                                      "s" shell
    data ios &
        &/ 1,                                                                2, &!    2
        &  1, 2,                                              2, 2, 2, 2, 2, 0, &!   10
        &  1, 2,                                              2, 2, 2, 2, 2, 0, &!   18
        &  1, 2, 2,              2, 2, 1, 2, 2, 2, 2, 1, 2,   2, 2, 2, 2, 2, 0, &!   36
        &  1, 2, 2,              2, 1, 1, 2, 1, 1, 0, 1, 2,   2, 2, 2, 2, 2, 0, &!   54
        &  1, 2, 2, 5*0,3*2,6*2, 2, 2, 1, 2, 2, 2, 1, 1, 2,   2, 2, 2, 2, 2, 0, &!   86
        &  1, 1, 2, 4, 2, 2,     2, 2, 2, 2, 2, 2, 0, 3,-3,   1, 2, 1,-2,-1, 0 /
!                                  /
!
!              H           Initial "p" Orbital Occupancies                   He
!              Li Be                                          B  C  N  O  F  Ne
!              Na Mg                                          Al Si P  S  Cl Ar
!              K  Ca Sc          Ti V  Cr Mn Fe Co Ni Cu Zn   Ga Ge As Se Br Kr
!              Rb Sr Y           Zr Nb Mo Tc Ru Rh Pd Ag Cd   In Sn Sb Te I  Xe
!              Cs Ba La Ce-Lu    Hf Ta W  Re Os Ir Pt Au Hg   Tl Pb Bi Po At Rn
!              Fr Ra Ac Th Pa U  Np Pu Am Cm Bk Cf (The rest are reserved for MOPAC)
!                                      "p" shell
    data iop / 0 ,                                                           0, &!    2
            &  0, 0,                                          1, 2, 3, 4, 5, 6, &!   10
            &  0, 0,                                          1, 2, 3, 4, 5, 6, &!   18
            &  0, 0, 0,          0, 0, 0, 0, 0, 0, 0, 0, 0,   1, 2, 3, 4, 5, 6, &!   36
            &  0, 0, 0,          0, 0, 0, 0, 0, 0, 0, 0, 0,   1, 2, 3, 4, 5, 6, &!   54
            &  0, 0, 0,  14*0,   0, 0, 0, 0, 0, 0, 0, 0, 0,   1, 2, 3, 4, 5, 6, &!   86
            &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9*0                        /
!
!              H           Initial "d" Orbital Occupancies                   He
!              Li Be                                          B  C  N  O  F  Ne
!              Na Mg                                          Al Si P  S  Cl Ar
!              K  Ca Sc          Ti V  Cr Mn Fe Co Ni Cu Zn   Ga Ge As Se Br Kr
!              Rb Sr Y           Zr Nb Mo Tc Ru Rh Pd Ag Cd   In Sn Sb Te I  Xe
!              Cs Ba La Ce-Lu    Hf Ta W  Re Os Ir Pt Au Hg   Tl Pb Bi Po At Rn
!              Fr Ra Ac Th Pa U  Np Pu Am Cm Bk Cf (The rest are reserved for MOPAC)
!                                      "d" shell
    data iod / 0,                                                           0, &!    2
             & 0, 0,                                         0, 0, 0, 0, 0, 0, &!   10
             & 0, 0,                                         0, 0, 0, 0, 0, 0, &!   18
             & 0, 0, 1,          2, 3, 5, 5, 6, 7, 8, 10, 0, 0, 0, 0, 0, 0, 0, &!   36
             & 0, 0, 1,          2, 4, 5, 5, 7, 8,10, 10, 0, 0, 0, 0, 0, 0, 0, &!   54
             & 0, 0, 1,13*0,  1, 2, 3, 5, 5, 6, 7, 9, 10, 0, 0, 0, 0, 0, 0, 0, &!   86
             & 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9*0                     /
!
!                     Principal Quantum Numbers for all shells.
!
!              H                 "s"  shell                                  He
!              Li Be                                          B  C  N  O  F  Ne
!              Na Mg                                          Al Si P  S  Cl Ar
!              K  Ca Sc          Ti V  Cr Mn Fe Co Ni Cu Zn   Ga Ge As Se Br Kr
!              Rb Sr Y           Zr Nb Mo Tc Ru Rh Pd Ag Cd   In Sn Sb Te I  Xe
!              Cs Ba La Ce-Lu    Hf Ta W  Re Os Ir Pt Au Hg   Tl Pb Bi Po At Rn
!              Fr Ra Ac Th-Lr    ?? ?? ?? ??
!
data npq(1:107,1) / &
             & 1,                                                             1, &!  2
             & 2, 2,                                           2, 2, 2, 2, 2, 3, &! 10
             & 3, 3,                                           3, 3, 3, 3, 3, 4, &! 18
             & 4, 4,             4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, &! 36
             & 5, 5,             5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, &! 54
             & 6, 6, 14 * 6,     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, &! 86
             & 15 * 0, 3, 5 * 0 /
!
!              H                "p"  shell                                   He
!              Li Be                                          B  C  N  O  F  Ne
!              Na Mg                                          Al Si P  S  Cl Ar
!              K  Ca Sc          Ti V  Cr Mn Fe Co Ni Cu Zn   Ga Ge As Se Br Kr
!              Rb Sr Y           Zr Nb Mo Tc Ru Rh Pd Ag Cd   In Sn Sb Te I  Xe
!              Cs Ba La Ce-Lu    Hf Ta W  Re Os Ir Pt Au Hg   Tl Pb Bi Po At Rn
!              Fr Ra Ac Th-Lr    ?? ?? ?? ??
!
data npq(1:107,2) / &
             & 1,                                                             2, &!  2
             & 2, 2,                                           2, 2, 2, 2, 2, 2, &! 10
             & 3, 3,                                           3, 3, 3, 3, 3, 3, &! 18
             & 4, 4,             4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &! 36
             & 5, 5,             5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, &! 54
             & 6, 6, 14 * 6,     6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, &! 86
             & 21 * 0 /
!
!              H                 "d"  shell                                  He
!              Li Be                                          B  C  N  O  F  Ne
!              Na Mg                                          Al Si P  S  Cl Ar
!              K  Ca Sc          Ti V  Cr Mn Fe Co Ni Cu Zn   Ga Ge As Se Br Kr
!              Rb Sr Y           Zr Nb Mo Tc Ru Rh Pd Ag Cd   In Sn Sb Te I  Xe
!              Cs Ba La Ce-Lu    Hf Ta W  Re Os Ir Pt Au Hg   Tl Pb Bi Po At Rn
!              Fr Ra Ac Th-Lr    ?? ?? ?? ??
!
data npq(1:107,3) / &
             & 0,                                                             3, &!  2
             & 0, 0,                                           0, 0, 0, 0, 0, 3, &! 10
             & 3, 3,                                           3, 3, 3, 3, 3, 4, &! 18
             & 3, 3,             3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, &! 36
             & 4, 4,             4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, &! 54
             & 5, 5, 14 * 5,     5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, &! 86
             & 21 * 0 /
!
!   NATSP IS THE NUMBER OF ATOMIC ORBITALS PER ATOM
!
!                               Group
!             1 2  Transition Metals    3 4 5 6 7         8
!
!    H                                                  He
!    Li Be                               B  C  N  O  F  Ne
!    Na Mg                               Al Si P  S  Cl Ar
!    K  Ca Sc Ti V  Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
!    Rb Sr Y  Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I  Xe
!    Cs Ba
!             La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb
!          Lu Hf Ta W  Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn
!    Fr Ra
!          Ac Th Pa U  Np Pu Am Cm Bk Cf
!    XX ?? ?? Cb
!
  data natsp / &
   & 1,                                                 4, & ! H  - He
   & 4, 4,                               4, 4, 4, 4, 4, 4, & ! Li - Ne
   & 0, 4,                               4, 4, 4, 4, 4, 4, & ! Na - Ar
   & 0, 4, 9, 9, 9, 9, 9, 9, 9, 9, 9, 4, 4, 4, 4, 4, 4, 4, & ! K  - Kr
   & 0, 4, 9, 9, 9, 9, 9, 9, 9, 9, 9, 4, 4, 4, 4, 4, 4, 4, & ! Rb - Xe
   & 0, 4,                                                 & ! Cs - Ba
   &          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    & ! The Lanthanides
   &       0, 9, 9, 9, 9, 9, 9, 9, 9, 4, 4, 4, 4, 4, 4, 4, & ! Lu - Rn
   & 0, 4,                                                 & ! Fr - Ra
   &       4, 0, 4, 4, 4, 4, 4, 4, 4, 4,                   & ! The Actinides
   & 0, 0, 0, 1, 0,                                        & ! XX, ??, ??, Cb, ++
   & 0, 0, 0, 0                                            / ! +,  --,  -, Tv 
!    
!     LIST OF ELEMENTS WITH MNDO/d PARAMETERS.
!
!   NATSPD IS THE NUMBER OF ATOMIC ORBITALS PER ATOM FOR MNDO-D.
!
!                               Group
!                 1 2  Transition Metals    3 4 5 6 7         8
       data natspd / &
     &            1,                                          0,        &
     &            4,4,                      4,4,4,4,4,        0,        &
     &            4,4,                      9,9,9,9,9,        0,        &
     &            4,4, 9,9,9,9,9,9,9,9,9,4, 9,9,9,9,9,        0,        &
     &            4,4, 9,9,9,9,9,9,9,9,9,4, 9,9,9,9,9,        0,        &
     &            0,0,                                                  &
     &              0,0,0,0,0,0,0,0,0,0,0,0,0,0,                        &
     &                 9,9,9,9,9,9,9,9,9,4, 0,0,0,0,0,        0,        &
     &            0,0,                                                  &
     &              0,0,0,0,0,0,0,0,0,0,0,0,0,0,                        &
     &                                      0,0,0,0,0/
!
!  Main_group is .true. if the Gss, Gpp, etc., are independent of zsn, zpn, and zdn,
!  .false. otherwise.
!
!  For main-group elements, the Gss value is important, for the transition metals,
!  Gss is less important.
!
    data main_group /&
   &  2*.true.,                              & ! H  - He
   &  8*.true.,                              & ! Li - Ne
   &  8*.true.,                              & ! Na - Ar
   &  2*.true., 9*.false., 7*.true.,         & ! K  - Kr
   &  2*.true., 9*.false., 7*.true.,         & ! Rb - Xe
   &  2*.true.,23*.false., 7*.true.,         & ! Cs - Rn
   & 21*.true.                               / ! Fr - Tv
   !
!     ENTHALPIES OF FORMATION OF GASEOUS ATOMS ARE TAKEN FROM \ANNUAL
!     REPORTS,1974,71B,P 117\  THERE ARE SOME SIGNIFICANT DIFFERENCES
!     BETWEEN THE VALUES REPORTED THERE AND THE VALUES PREVIOUSLY IN
!     THE BLOCK DATA OF THIS PROGRAM.  ONLY THE THIRD  ROW ELEMENTS
!     HAVE BEEN UPDATED.
!
! ALL THE OTHER ELEMENTS ARE TAKEN FROM CRC HANDBOOK 1981-1982
    data eheat(1) / 52.102d0 /
    data eheat(2) / 0.000d0 /
    data eheat(3) / 38.410d0 /
    data eheat(4) / 76.960d0 /
    data eheat(5) / 135.700d0 /
    data eheat(6) / 170.890d0 /
    data eheat(7) / 113.000d0 /
    data eheat(8) / 59.559d0 /
    data eheat(9) / 18.890d0 /
    data eheat(10) / 0.000d0 /
    data eheat(11) / 25.650d0 /
    data eheat(12) / 35.000d0 /
    data eheat(13) / 79.490d0 /
    data eheat(14) / 108.390d0 /
    data eheat(15) / 75.570d0 /
    data eheat(16) / 66.400d0 /
    data eheat(17) / 28.990d0 /
    data eheat(18) / 0.000d0 /
    data eheat(19) / 21.420d0 /
    data eheat(20) / 42.600d0 /
    data eheat(21) / 90.300d0 /
    data eheat(22) / 112.300d0 /
    data eheat(23) / 122.900d0 /
    data eheat(24) / 95.000d0 /
    data eheat(25) / 67.700d0 /
    data eheat(26) / 99.300d0 /
    data eheat(27) / 102.400d0 /
    data eheat(28) / 102.800d0 /
    data eheat(29) / 80.700d0 /
    data eheat(30) / 31.170d0 /
    data eheat(31) / 65.400d0 /
    data eheat(32) / 89.500d0 /
    data eheat(33) / 72.300d0 /
    data eheat(34) / 54.300d0 /
    data eheat(35) / 26.740d0 /
    data eheat(36) / 0.000d0 /
    data eheat(37) / 19.600d0 /
    data eheat(38) / 39.100d0 /
    data eheat(39) / 101.500d0 /
    data eheat(40) / 145.500d0 /
    data eheat(41) / 172.400d0 /
    data eheat(42) / 157.300d0 /
    data eheat(43) / 162.000d0 /
    data eheat(44) / 155.500d0 /
    data eheat(45) / 133.000d0 /
    data eheat(46) / 90.000d0 /
    data eheat(47) / 68.100d0 /
    data eheat(48) / 26.720d0 /
    data eheat(49) / 58.000d0 /
    data eheat(50) / 72.200d0 /
    data eheat(51) / 63.200d0 /
    data eheat(52) / 47.000d0 /
    data eheat(53) / 25.517d0 /
    data eheat(54) / 0.000d0 /
    data eheat(55) / 18.700d0 /
    data eheat(56) / 42.500d0 /
    data eheat(57) / 928.9D0/   !  Represents La(+++)
    data eheat(58) / 944.7D0 /  !  Represents Ce(+++)
    data eheat(59) / 952.9D0 /  !  Represents Pr(+++)
    data eheat(60) / 962.8D0 /  !  Represents Nd(+++)
    data eheat(61) / 976.9D0 /  !  Represents Pm(+++)
    data eheat(62) / 974.4d0 /  !  Represents Sm(+++)
    data eheat(63) / 1006.6d0/  !  Represents Eu(+++)
    data eheat(64) / 991.37d0/  !  Represents Gd(+++)
    data eheat(65) / 999.0d0/   !  Represents Tb(+++)
    data eheat(66) / 1001.3d0 / !  Represents Dy(+++)
    data eheat(67) / 1009.6d0 / !  Represents Ho(+++)
    data eheat(68) / 1016.15d0 /!  Represents Er(+++)
    data eheat(69) / 1022.06d0/ !  Represents Tm(+++)
    data eheat(70) / 1039.03d0/ !  Represents Ho(+++)
    data eheat(71) / 1031.2d0 / !  Represents Lu(+++)
    data eheat(72) / 148.000d0 /
    data eheat(73) / 186.900d0 /
    data eheat(74) / 203.100d0 /
    data eheat(75) / 185.000d0 /
    data eheat(76) / 188.000d0 /
    data eheat(77) / 160.000d0 /
    data eheat(78) / 135.200d0 /
    data eheat(79) / 88.000d0 /
    data eheat(80) / 14.690d0 /
    data eheat(81) / 43.550d0 /
    data eheat(82) / 46.620d0 /
    data eheat(83) / 50.100d0 /
    data eheat(86) / 0.000d0 /
    data eheat(90) / 1674.64d0/
    data eheat(102) / 207.0d0 /
    data eheat(103) / 0.0d0 /
    data eheat(104) / 0.0d0 /
    data eheat(105) / 0.0d0 /
    data eheat(106) / 0.0d0 /
    data eheat(107) / 0.0d0 /
    data eisol / 107 * 0.0d0 /
    data ams / 1.00790d0, 4.00260d0, 6.94000d0, 9.01218d0, 10.81000d0, &
   & 12.01100d0, 14.00670d0, 15.99940d0, 18.99840d0, 20.17900d0, 22.98977d0, &
   & 24.30500d0, 26.98154d0, 28.08550d0, 30.97376d0, 32.06000d0, 35.45300d0, &
   & 39.94800d0, 39.09830d0, 40.08000d0, 44.95590d0, 47.90000d0, 50.94150d0, &
   & 51.99600d0, 54.93800d0, 55.84700d0, 58.93320d0, 58.71000d0, 63.54600d0, &
   & 65.38000d0, 69.73500d0, 72.59000d0, 74.92160d0, 78.96000d0, 79.90400d0, &
   & 83.80000d0, 85.46780d0, 87.62000d0, 88.90590d0, 91.22000d0, 92.90640d0, &
   & 95.94000d0, 98.90620d0, 101.0700d0, 102.9055d0, 106.4000d0, 107.8680d0, &
   & 112.4100d0, 114.8200d0, 118.6900d0, 121.7500d0, 127.6000d0, 126.9045d0, &
   & 131.3000d0, 132.9054d0, 137.3300d0, 138.9060d0, 140.1160d0, 140.9077d0, &
   & 144.2400d0, 145.0000d0, 150.3600d0, 151.9640d0, 157.2500d0, 158.9253d0, &
   & 162.5000d0, 164.9303d0, 167.2600d0, 168.9342d0, 173.0400d0, 174.9670d0, &
   & 178.4900d0, 180.9479d0, &
   & 183.8500d0, 186.2070d0, 190.2000d0, 192.2200d0, 195.0900d0, 196.9665d0, &
   & 200.5900d0, 204.3700d0, 207.2000d0, 208.9804d0, 209.0000d0, 210.0000d0, &
   & 222.0000d0, 223.0000d0, 226.0000d0, 227.0000d0, 232.0381d0, 231.0359d0, &
   & 238.0289d0, 9 * 0.000d0, 1.0079d0, &
   & 5 * 0.000d0 /
      end module parameters_C 
