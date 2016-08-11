      BLOCK DATA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NATORB/ NATORB(107)
***********************************************************************
*
*     COMMON BLOCKS FOR AM1
*
***********************************************************************
      COMMON /ELEMTS/ ELEMNT(107)
     1       /CORE  / CORE(107)
     2       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
     3       /EXPONT/ ZS(107),ZP(107),ZD(107)
     4       /ONELEC/ USS(107),UPP(107),UDD(107)
     5       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     6       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107),
     7                GSD(107),GPD(107),GDD(107)
     8       /ATOMIC/ EISOL(107),EHEAT(107)
     9       /VSIPS / VS(107),VP(107),VD(107)
     1       /ISTOPE/ AMS(107)
     2       /IDEAA / GUESA1(107,10),GUESA2(107,10),GUESA3(107,10)
     3       /IDEAP / GUESP1(107,10),GUESP2(107,10),GUESP3(107,10)
     4       /GAUSS / FN1(107),FN2(107)
***********************************************************************
*
*     COMMON BLOCKS FOR MNDO
*
***********************************************************************
      COMMON /MNDO/  USSM(107), UPPM(107), UDDM(107), ZSM(107),
     1ZPM(107), ZDM(107), BETASM(107), BETAPM(107), BETADM(107),
     2ALPM(107), EISOLM(107), DDM(107), QQM(107), AMM(107), ADM(107),
     3AQM(107) ,GSSM(107), GSPM(107), GPPM(107), GP2M(107), HSPM(107),
     4POLVOM(107)
***********************************************************************
*
*     COMMON BLOCKS FOR PM3
*
***********************************************************************
      COMMON /PM3 /  USSPM3(107), UPPPM3(107), UDDPM3(107), ZSPM3(107),
     1ZPPM3(107), ZDPM3(107), BETASP(107), BETAPP(107), BETADP(107),
     2ALPPM3(107), EISOLP(107), DDPM3(107), QQPM3(107), AMPM3(107),
     3ADPM3(107), AQPM3(107) ,GSSPM3(107), GSPPM3(107), GPPPM3(107),
     4GP2PM3(107), HSPPM3(107),POLVOP(107)
***********************************************************************
*
*     COMMON BLOCKS FOR AM1
*
***********************************************************************
      COMMON /AM1BLO/USSAM1(107), UPPAM1(107), UDDAM1(107), ZSAM1(107),
     1ZPAM1(107), ZDAM1(107), BETASA(107), BETAPA(107), BETADA(107),
     2ALPAM1(107), EISOLA(107), DDAM1(107), QQAM1(107), AMAM1(107),
     3ADAM1(107), AQAM1(107) ,GSSAM1(107), GSPAM1(107), GPPAM1(107),
     4GP2AM1(107), HSPAM1(107),POLVOA(107)
     5       /REFS/ REFMN(107), REFM3(107), REFAM(107), REFPM3(107)
***********************************************************************
*
*  COMMON BLOCKS FOR MINDO/3
*
***********************************************************************
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /TWOEL3 /  F03(107)
     2       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     3       /BETA3  /  BETA3(153)
     4       /ALPHA3 /  ALP3(153)
     5       /EXPON3 /  ZS3(18),ZP3(18)
*
*  END OF MINDO/3 COMMON BLOCKS
*
C
C ELECTRIC FIELD OPTIONS FOR POLARIZABILITY
C
      COMMON /FIELD/ EFIELD(3)
      CHARACTER ELEMNT*2, REFMN*80, REFM3*80, REFAM*80, REFPM3*80
      DATA EFIELD/0.0D00,0.0D00,0.0D00/
      DATA ELEMNT/' H','He',
     1 'Li','Be',' B',' C',' N',' O',' F','Ne',
     2 'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     3 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     6 'Cd','In','Sn','Sb','Te',' I','Xe',
     7 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     8 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     9 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     1 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX',
     2 'Fm','Md','Cb','++',' +','--',' -','Tv'/
C
C   NATORB IS THE NUMBER OF ATOMIC ORBITALS PER ATOM.
C
      DATA NATORB/2*1, 4, 7*4, 0, 7*4, 0, 4, 9*9, 7*4,
     12*4, 9*9, 7*4, 2*2, 14*8, 9*9, 7*4, 15*0,1,5*0/
***********************************************************************
*                      VALENCE SHELLS ARE DEFINED AS                  *
*  PQN   VALENCE SHELLS                                               *
*                 P-GROUP              F-GROUP    TRANSITION METALS   *
*   1       1S                                                        *
*   2       2S 2P                                                     *
*   3       3S 3P  OR  3S 3P 3D                                       *
*   4       4S 4P                                    4S 4P 3D         *
*   5       5S 5P                                    5S 5P 4D         *
*   6       6S 6P                       6S 4F        6S 6P 5D         *
*   7  NOT ASSIGNED YET  ****DO  NOT  USE****                         *
***********************************************************************
      DATA      POLVOM(1) /0.2287D0/
      DATA      POLVOM(6) /0.2647D0/
      DATA      POLVOM(7) /0.3584D0/
      DATA      POLVOM(8) /0.2324D0/
      DATA      POLVOM(9) /0.1982D0/
      DATA      POLVOM(17)/1.3236D0/
      DATA      POLVOM(35)/2.2583D0/
      DATA      POLVOM(53)/4.0930D0/
C
C                STANDARD ATOMIC MASSES
C
      DATA  AMS /  1.00790D0,  4.00260D0,  6.94000D0,  9.01218D0,
     110.81000D0, 12.01100D0, 14.00670D0, 15.99940D0, 18.99840D0,
     220.17900D0, 22.98977D0, 24.30500D0, 26.98154D0, 28.08550D0,
     330.97376D0, 32.06000D0, 35.45300D0, 39.94800D0, 39.09830D0,
     440.08000D0, 44.95590D0, 47.90000D0, 50.94150D0, 51.99600D0,
     554.93800D0, 55.84700D0, 58.93320D0, 58.71000D0, 63.54600D0,
     665.38000D0, 69.73500D0, 72.59000D0, 74.92160D0, 78.96000D0,
     779.90400D0, 83.80000D0, 85.46780D0, 87.62000D0, 88.90590D0,
     891.22000D0, 92.90640D0, 95.94000D0, 98.90620D0, 101.0700D0,
     9102.9055D0, 106.4000D0, 107.8680D0, 112.4100D0, 114.8200D0,
     1118.6900D0, 121.7500D0, 127.6000D0, 126.9045D0, 131.3000D0,
     2132.9054D0, 137.3300D0, 15*0.000D0, 178.4900D0, 180.9479D0,
     3183.8500D0, 186.2070D0, 190.2000D0, 192.2200D0, 195.0900D0,
     4196.9665D0, 200.5900D0, 204.3700D0, 207.2000D0, 208.9804D0,
     518*0.000D0,   1.0079D0,  5*0.000D0/
C
C   CORE IS THE CHARGE ON THE ATOM AS SEEN BY THE ELECTRONS
C
      DATA CORE/1.D0,0.D0,
     1 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     2 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     3 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,11.D0,2.D0,
     4 3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     5 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,11.D0,2.D0,
     6 3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     7 1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,
     8 11.D0,12.D0,13.D0,14.D0,15.D0,16.D0,
     9 3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0,10.D0,11.D0,2.D0,
     1 3.D0,4.D0,5.D0,6.D0,7.D0,0.D0,
     2  15*0.D0,1.D0,2.D0,1.D0,-2.D0,-1.D0,0.D0/
C
C     ENTHALPIES OF FORMATION OF GASEOUS ATOMS ARE TAKEN FROM \ANNUAL
C     REPORTS,1974,71B,P 117\  THERE ARE SOME SIGNIFICANT DIFFERENCES
C     BETWEEN THE VALUES REPORTED THERE AND THE VALUES PREVIOUSLY IN
C     THE BLOCK DATA OF THIS PROGRAM.  ONLY THE THIRD  ROW ELEMENTS
C     HAVE BEEN UPDATED.
C
* ALL THE OTHER ELEMENTS ARE TAKEN FROM CRC HANDBOOK 1981-1982
      DATA EHEAT(1)  / 52.102D0/
      DATA EHEAT(2)  /  0.000D0/
C
      DATA EHEAT(3)  / 38.410D0/
      DATA EHEAT(4)  / 76.960D0/
      DATA EHEAT(5)  /135.700D0/
      DATA EHEAT(6)  /170.890D0/
      DATA EHEAT(7)  /113.000D0/
      DATA EHEAT(8)  / 59.559D0/
      DATA EHEAT(9)  / 18.890D0/
      DATA EHEAT(10) /  0.000D0/
C
      DATA EHEAT(11) / 25.850D0/
      DATA EHEAT(12) / 35.000D0/
      DATA EHEAT(13) / 79.490D0/
      DATA EHEAT(14) /108.390D0/
      DATA EHEAT(15) / 75.570D0/
      DATA EHEAT(16) / 66.400D0/
      DATA EHEAT(17) / 28.990D0/
      DATA EHEAT(18) /  0.000D0/
C
      DATA EHEAT(19) / 21.420D0/
      DATA EHEAT(20) / 42.600D0/
      DATA EHEAT(21) / 90.300D0/
      DATA EHEAT(22) /112.300D0/
      DATA EHEAT(23) /122.900D0/
      DATA EHEAT(24) / 95.000D0/
      DATA EHEAT(25) / 67.700D0/
      DATA EHEAT(26) / 99.300D0/
      DATA EHEAT(27) /102.400D0/
      DATA EHEAT(28) /102.800D0/
      DATA EHEAT(29) / 80.700D0/
      DATA EHEAT(30) / 31.170D0/
      DATA EHEAT(31) / 65.400D0/
      DATA EHEAT(32) / 89.500D0/
      DATA EHEAT(33) / 72.300D0/
      DATA EHEAT(34) / 54.300D0/
      DATA EHEAT(35) / 26.740D0/
      DATA EHEAT(36) /  0.000D0/
C
      DATA EHEAT(37) / 19.600D0/
      DATA EHEAT(38) / 39.100D0/
      DATA EHEAT(39) /101.500D0/
      DATA EHEAT(40) /145.500D0/
      DATA EHEAT(41) /172.400D0/
      DATA EHEAT(42) /157.300D0/
      DATA EHEAT(44) /155.500D0/
      DATA EHEAT(45) /133.000D0/
      DATA EHEAT(46) / 90.000D0/
      DATA EHEAT(47) / 68.100D0/
      DATA EHEAT(48) / 26.720D0/
      DATA EHEAT(49) / 58.000D0/
      DATA EHEAT(50) / 72.200D0/
      DATA EHEAT(51) / 63.200D0/
      DATA EHEAT(52) / 47.000D0/
      DATA EHEAT(53) / 25.517D0/
      DATA EHEAT(54) /  0.000D0/
C
      DATA EHEAT(55) / 18.700D0/
      DATA EHEAT(56) / 42.500D0/
      DATA EHEAT(58) /101.300D0/
      DATA EHEAT(62) / 49.400D0/
      DATA EHEAT(68) / 75.800D0/
      DATA EHEAT(70) / 36.350D0/
      DATA EHEAT(72) /148.000D0/
      DATA EHEAT(73) /186.900D0/
      DATA EHEAT(74) /203.100D0/
      DATA EHEAT(75) /185.000D0/
      DATA EHEAT(76) /188.000D0/
      DATA EHEAT(77) /160.000D0/
      DATA EHEAT(78) /135.200D0/
      DATA EHEAT(79) / 88.000D0/
      DATA EHEAT(80) / 14.690D0/
      DATA EHEAT(81) / 43.550D0/
      DATA EHEAT(82) / 46.620D0/
      DATA EHEAT(83) / 50.100D0/
      DATA EHEAT(86) /  0.000D0/
      DATA EHEAT(102)  / 207.0D0/
C
      DATA VS(1) /  -13.605  /
      DATA VS(5)/-15.16D00/
      DATA VS(6)/-21.34D00/
      DATA VS(7)/-27.51D00/
      DATA VS(8)/-35.30D00/
      DATA VS(9)/-43.70D00/
      DATA VS(14)/-17.82D00/
      DATA VS(15)/-21.10D00/
      DATA VS(16)/-23.84D00/
      DATA VS(17)/-25.26D00/
      DATA VP(1)  /  0.0D00  /
      DATA VP(5)/-8.52D00/
      DATA VP(6)/-11.54D00/
      DATA VP(7)/-14.34D00/
      DATA VP(8)/-17.91D00/
      DATA VP(9)/-20.89D00/
      DATA VP(14)/-8.51D00/
      DATA VP(15)/-10.29D00/
      DATA VP(16)/-12.41D00/
      DATA VP(17)/-15.09D00/
C      DATA NPQ/1,1, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,
C     +4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
C
C *** ONE CENTER REPULSION INTEGRALS
C     GSS ::= (SS,SS)
C     GPP ::= (PP,PP)
C     GSP ::= (SS,PP)
C     GP2 ::= (PP,P*P*)
C     HSP ::= (SP,SP)
************************************************************************
      DATA GSSM(1) / 12.848D00 /
      DATA GSSM(4)/9.00D00/
      DATA GSSM(5)/10.59D00/
      DATA GSSM(6) / 12.23D00 /
      DATA GSSM(7)/13.59D00/
      DATA GSSM(8)/15.42D00/
      DATA GSSM(9)/16.92D00/
      DATA GSSM(13)/8.09D00/
      DATA GSSM(14)/9.82D00/
      DATA GSSM(15)/11.56D00/
      DATA GSSM(16)/12.88D00/
      DATA GSSM(17)/15.03D00/
      DATA GSSM(35)/15.03643948D0/
      DATA GSSM(53)/15.04044855D0/
      DATA GPPM(4)/6.97D00/
      DATA GPPM(5)/8.86D00/
      DATA GPPM(6) / 11.08D00 /
      DATA GPPM(7)/12.98D00/
      DATA GPPM(8)/14.52D00/
      DATA GPPM(9)/16.71D00/
      DATA GPPM(13)/5.98D00/
      DATA GPPM(14)/7.31D00/
      DATA GPPM(15)/8.64D00/
      DATA GPPM(16)/9.90D00/
      DATA GPPM(17)/11.30D00/
      DATA GPPM(35)/11.27632539D0/
      DATA GPPM(53)/11.14778369D0/
      DATA GSPM(4)/7.43D00/
      DATA GSPM(5)/9.56D00/
      DATA GSPM(6) / 11.47D00 /
      DATA GSPM(7)/12.66D00/
      DATA GSPM(8)/14.48D00/
      DATA GSPM(9)/17.25D00/
      DATA GSPM(13)/6.63D00/
      DATA GSPM(14)/8.36D00/
      DATA GSPM(15)/10.08D00/
      DATA GSPM(16)/11.26D00/
      DATA GSPM(17)/13.16D00/
      DATA GSPM(35)/13.03468242D0/
      DATA GSPM(53)/13.05655798D0/
      DATA GP2M(4)/6.22D00/
      DATA GP2M(5)/7.86D00/
      DATA GP2M(6) / 9.84D00 /
      DATA GP2M(7)/11.59D00/
      DATA GP2M(8)/12.98D00/
      DATA GP2M(9)/14.91D00/
      DATA GP2M(13)/5.40D00/
      DATA GP2M(14)/6.54D00/
      DATA GP2M(15)/7.68D00/
      DATA GP2M(16)/8.83D00/
      DATA GP2M(17)/9.97D00/
      DATA GP2M(35)/9.85442552D0/
      DATA GP2M(53)/9.91409071D0/
      DATA HSPM(4)/1.28D00/
      DATA HSPM(5)/1.81D00/
      DATA HSPM(6) / 2.43D00 /
      DATA HSPM(7)/3.14D00/
      DATA HSPM(8)/3.94D00/
      DATA HSPM(9)/4.83D00/
      DATA HSPM(13)/0.70D00/
      DATA HSPM(14)/1.32D00/
      DATA HSPM(15)/1.92D00/
      DATA HSPM(16)/2.26D00/
      DATA HSPM(17)/2.42D00/
      DATA HSPM(35)/2.45586832D0/
      DATA HSPM(53)/2.45638202D0/
C
C     THE MONOCENTRIC INTEGRALS HSP AND GSP FOR ALUMINIUM ARE ONLY
C     ESTIMATES. A VALUE OF G1 FOR AL IS NEEDED TO RESOLVE OLEARIS
C     INTEGRALS.
C
C     OPTIMIZED MNDO PARAMETERS FOR H, BE, B, C, N, O, F
C                                                     CL
C     ESTIMATED MNDO PARAMETERS FOR       AL,SI, P, S
C
C     ELEMENTS H, C, N, O WERE PARAMETERIZED BY WALTER THIEL
C     ELEMENTS B,SI,P,S   WERE      ..          MICHAEL MCKEE
C     ELEMENTS BE,F,AL,CL WERE      ..          HENRY RZEPA
C
***********************************************************************
*
*    START OF MINDO/3 PARAMETERS
*
***********************************************************************
C *** F03 IS THE ONE CENTER AVERAGED REPULSION INTEGRAL FOR USE IN THE
C        TWO CENTER ELECTRONIC REPULSION INTEGRAL EVALUATION.
      DATA REFM3  ( 1)/'  H: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 5)/'  B: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 6)/'  C: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 7)/'  N: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 8)/'  O: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 9)/'  F: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (14)/' Si: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (15)/'  P: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (16)/'  S: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (17)/' Cl: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA F03              /  12.848D0, 10.0D0, 10.0D0, 0.0D0,
     1  8.958D0, 10.833D0, 12.377D0, 13.985D0, 16.250D0,
     2         10.000D0, 10.000D0, 0.000D0, 0.000D0,7.57D0 ,  9.00D0 ,
     3         10.20D0 , 11.73,10.0D0,35*0.D0,10.D0,53*10.D0/
C *** USS AND UPP ARE THE ONE-CENTER CORE ELECTRON ATTRACTION AND KINETI
C     ENERGY INTEGRALS FOR S AND P ELECTRONS RESPECTIVELY IN E.V.
      DATA USS3             / -12.505D0, 0.000D0, 0.000D0, 0.000D0,
     1                       -33.61D0, -51.79D0, -66.06D0, -91.73D0 ,
     2                       -129.86D0,
     3                        0.0000D0 , 0.000 D0 ,0.000D0 , 0.000D0 ,
     4          -39.82D0 , -56.23D0 , -73.39D0 , -98.99D0 ,.0D0/
      DATA UPP3             /   0.0D0, 0.0D0, 0.0D0, 0.0D0,
     1     -25.11D0 , -39.18D0 , -56.40D0 , -78.80D0 , -105.93D0 ,
     2                        0.000D0 , 0.000D0 , 0.000D0 , 0.000D0 ,
     3         -29.15D0 , -42.31D0 , -57.25D0 , -76.43D0 ,.0D0/
C *** EISOL3 AND EHEAT3 ARE THE GS ELECTRONIC ENERGY OF THE NEUTRAL ATOM
C     (IN E.V.) AND THE HEAT OF FORMATION IF THE FREE ATOM (IN KCAL/MOL)
      DATA EISOL3             /-12.505D0 , 0.0D0 , 0.0D0 ,0.0D0 ,
     1        -61.70D0 ,-119.47D0 , -187.51D0 , -307.07D0 , -475.00D0 ,
     2                         0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
     3          -90.98D0 , -150.81D0 , -229.15D0 , -345.93D0 , 0.0D0/
      DATA EHEAT3             / 52.102D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
     1     135.7 D0 , 170.89D0 ,  113.0 D0 ,  59.559D0 ,  18.86D0 ,
     2                         0.0D0 , 0.0D0 , 0.0D0 , 0.0D0 ,
     3     106.0D0 ,   79.8D0 ,  65.65D0 ,  28.95D0 , 0.0D0 /
C *** BETA3 AND ALP3 ARE THE BOND PARAMETERS USED IN THE
C     RESONANCE INTEGRAL AND THE CORE CORE REPULSION INTEGRAL RESPECTIVE
C     THAT IS ACCORDING TO THE FOLLOWING CONVENTION
C
C     HERE IS THE
C     BOND TYPE DESIGNATION
C
C
C         H   B   C   N   O   F  SI   P   S  CL
C       -----------------------------------------
C      H  1  11  16  22  29  37  92 106 121 137
C      B     15  20  26  33  41
C      C         21  27  34  42  97 111 126 142
C      N             28  35  43         127 143
C      O                 36  44     113 128
C      F                     45     114
C     SI                        105
C      P                            120     151
C      S                                136 152
C     CL                                    153
      DATA BETA3(1),ALP3(1)   /  0.244770D0 ,  1.489450D0 /
      DATA BETA3(11),ALP3(11)   /  0.185347D0 ,  2.090352D0 /
      DATA BETA3(15),ALP3(15)   /  0.151324D0 ,  2.280544D0 /
      DATA BETA3(16),ALP3(16)   /  0.315011D0 ,  1.475836D0 /
      DATA BETA3(20),ALP3(20)   /  0.250031D0 ,  2.138291D0 /
      DATA BETA3(21),ALP3(21)   /  0.419907D0 ,  1.371208D0 /
      DATA BETA3(22),ALP3(22)   /  0.360776D0 ,  0.589380D0 /
      DATA BETA3(26),ALP3(26)   /  0.310959D0 ,  1.909763D0 /
      DATA BETA3(27),ALP3(27)   /  0.410886D0 ,  1.635259D0 /
      DATA BETA3(28),ALP3(28) /  0.377342D0 ,  2.029618D0 /
      DATA BETA3(29),ALP3(29) /  0.417759D0 ,  0.478901D0 /
      DATA BETA3(33),ALP3(33) /  0.349745D0 ,  2.484827D0 /
      DATA BETA3(34),ALP3(34) /  0.464514D0 ,  1.820975D0 /
      DATA BETA3(35),ALP3(35) /  0.458110D0 ,  1.873859D0 /
      DATA BETA3(36),ALP3(36) /  0.659407D0 ,  1.537190D0 /
      DATA BETA3(37),ALP3(37) /  0.195242D0 ,  3.771362D0 /
      DATA BETA3(41),ALP3(41) /  0.219591D0 ,  2.862183D0 /
      DATA BETA3(42),ALP3(42) /  0.247494D0 ,  2.725913D0 /
      DATA BETA3(43),ALP3(43) /  0.205347D0 ,  2.861667D0 /
      DATA BETA3(44),ALP3(44) /  0.334044D0 ,  2.266949D0 /
      DATA BETA3(45),ALP3(45) /  0.197464D0 ,  3.864997D0 /
      DATA BETA3(92),ALP3(92) /  0.289647D0 ,  0.940789D0 /
      DATA BETA3(97),ALP3(97) /  0.411377D0 ,  1.101382D0 /
      DATA BETA3(105),ALP3(105) /  0.291703D0 ,  0.918432D0 /
      DATA BETA3(106),ALP3(106) /  0.320118D0 ,  0.923170D0 /
      DATA BETA3(111),ALP3(111) /  0.457816D0 ,  1.029693D0 /
      DATA BETA3(113),ALP3(113) /  0.470000D0 ,  1.662500D0 /
      DATA BETA3(114),ALP3(114) /  0.300000D0 ,  1.750000D0 /
      DATA BETA3(120),ALP3(120) /  0.311790D0 ,  1.186652D0 /
      DATA BETA3(121),ALP3(121) /  0.220654D0 ,  1.700698D0 /
      DATA BETA3(126),ALP3(126) /  0.284620D0 ,  1.761370D0 /
      DATA BETA3(127),ALP3(127) /  0.313170D0 ,  1.878176D0/
      DATA BETA3(128),ALP3(128) /  0.422890D0 ,  2.077240D0 /
      DATA BETA3(136),ALP3(136) /  0.202489D0 ,  1.751617D0 /
      DATA BETA3(137),ALP3(137) /  0.231653D0 ,  2.089404D0 /
      DATA BETA3(142),ALP3(142) /  0.315480D0 ,  1.676222D0 /
      DATA BETA3(143),ALP3(143) /  0.302298D0 ,  1.817064D0 /
      DATA BETA3(151),ALP3(151) /  0.277322D0 ,  1.543720D0 /
      DATA BETA3(152),ALP3(152) /  0.221764D0 ,  1.950318D0 /
      DATA BETA3(153),ALP3(153) /  0.258969D0 ,  1.792125D0 /
C *** HERE COMES THE OPTIMIZED SLATER_S EXPONENTS FOR THE EVALUATION
C     OF THE OVERLAP INTEGRALS AND MOLECULAR DIPOLE MOMENTS.
      DATA ZS3(1),ZP3(1)      /  1.3D0       ,  0.0D0      /
      DATA ZS3(5),ZP3(5)      /  1.211156D0 ,  0.972826D0 /
      DATA ZS3(6),ZP3(6)      /  1.739391D0 ,  1.709645D0 /
      DATA ZS3(7),ZP3(7)      /  2.704546D0 ,  1.870839D0 /
      DATA ZS3(8),ZP3(8)      /  3.640575D0 ,  2.168448D0 /
      DATA ZS3(9),ZP3(9)      /  3.111270D0 ,  1.41986D0 /
      DATA ZS3(14),ZP3(14)    /  1.629173D0 ,  1.381721D0 /
      DATA ZS3(15),ZP3(15)    /  1.926108D0 ,  1.590665D0 /
      DATA ZS3(16),ZP3(16)    /  1.719480D0 ,  1.403205D0 /
      DATA ZS3(17),ZP3(17)    /  3.430887D0 ,  1.627017D0 /
*************************************************************
*                                                           *
*               DATA FOR THE SPARKLES                       *
*                                                           *
*************************************************************
*                               DATA FOR THE " ++ " SPARKLE
      DATA EHEAT(103)    / 0.0D0/
      DATA VS(103)       /10.0D0/
C
C  START OF MNDO
C
      DATA ALPM(103)     / 1.5D0/
      DATA EISOLM(103)   / 0.0D0/
      DATA AMM(103)      / 0.5D0/
C
C  START OF AM1
C
      DATA ALPAM1(103)   / 1.5D0/
      DATA EISOLA(103)   / 0.0D0/
      DATA AMAM1(103)    / 0.5D0/
C
C  START OF PM3
C
      DATA ALPPM3(103)   / 1.5D0/
      DATA EISOLP(103)   / 0.0D0/
      DATA AMPM3(103)    / 0.5D0/
*                               DATA FOR THE " + " SPARKLE
      DATA EHEAT(104)    / 0.0D0/
      DATA VS(104)       /10.0D0/
      DATA ALPAM1(104)   / 1.5D0/
      DATA EISOLA(104)   / 0.0D0/
      DATA AMAM1(104)    / 0.5D0/
      DATA ALPM(104)     / 1.5D0/
      DATA EISOLM(104)   / 0.0D0/
      DATA AMM(104)      / 0.5D0/
      DATA ALPPM3(104)   / 1.5D0/
      DATA EISOLP(104)   / 0.0D0/
      DATA AMPM3(104)    / 0.5D0/
*                               DATA FOR THE " -- " SPARKLE
      DATA EHEAT(105)    / 0.0D0/
      DATA VS(105)       /10.0D0/
      DATA ALPAM1(105)   / 1.5D0/
      DATA EISOLA(105)   / 0.0D0/
      DATA AMAM1(105)    / 0.5D0/
      DATA ALPM(105)     / 1.5D0/
      DATA EISOLM(105)   / 0.0D0/
      DATA AMM(105)      / 0.5D0/
      DATA ALPPM3(105)   / 1.5D0/
      DATA EISOLP(105)   / 0.0D0/
      DATA AMPM3(105)    / 0.5D0/
*                               DATA FOR THE " - " SPARKLE
      DATA EHEAT(106)    / 0.0D0/
      DATA VS(106)       /10.0D0/
      DATA ALPAM1(106)   / 1.5D0/
      DATA EISOLA(106)   / 0.0D0/
      DATA AMAM1(106)    / 0.5D0/
      DATA ALPM(106)     / 1.5D0/
      DATA EISOLM(106)   / 0.0D0/
      DATA AMM(106)      / 0.5D0/
      DATA ALPPM3(106)   / 1.5D0/
      DATA EISOLP(106)   / 0.0D0/
      DATA AMPM3(106)    / 0.5D0/
***********************************************************************
*
*    START OF MNDO PARAMETERS
*
***********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFMN  ( 1)/'  H: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 1)/     -11.9062760D0/
      DATA BETASM ( 1)/      -6.9890640D0/
      DATA ZSM    ( 1)/       1.3319670D0/
      DATA ALPM   ( 1)/       2.5441341D0/
      DATA EISOLM ( 1)/     -11.9062760D0/
      DATA AMM    ( 1)/       0.4721793D0/
      DATA ADM    ( 1)/       0.4721793D0/
      DATA AQM    ( 1)/       0.4721793D0/
C                    DATA FOR ELEMENT  3        LITHIUM
      DATA REFMN  ( 3)/' Li: (MNDO):  TAKEN FROM MNDOC BY W.THIEL,
     1QCPE NO.438, V. 2, P.63, (1982).'/
      DATA USSM   (  3)/      -5.1280000D0/
      DATA UPPM   (  3)/      -2.7212000D0/
      DATA BETASM (  3)/      -1.3500400D0/
      DATA BETAPM (  3)/      -1.3500400D0/
      DATA ZSM    (  3)/       0.7023800D0/
      DATA ZPM    (  3)/       0.7023800D0/
      DATA ALPM   (  3)/       1.2501400D0/
      DATA EISOLM (  3)/      -5.1280000D0/
      DATA GSSM   (  3)/       7.3000000D0/
      DATA GSPM   (  3)/       5.4200000D0/
      DATA GPPM   (  3)/       5.0000000D0/
      DATA GP2M   (  3)/       4.5200000D0/
      DATA HSPM   (  3)/       0.8300000D0/
      DATA DDM    (  3)/       2.0549783D0/
      DATA QQM    (  3)/       1.7437069D0/
      DATA AMM    (  3)/       0.2682837D0/
      DATA ADM    (  3)/       0.2269793D0/
      DATA AQM    (  3)/       0.2614581D0/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA REFMN  ( 4)/' Be: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
      DATA USSM   ( 4)/     -16.6023780D0/
      DATA UPPM   ( 4)/     -10.7037710D0/
      DATA BETASM ( 4)/      -4.0170960D0/
      DATA BETAPM ( 4)/      -4.0170960D0/
      DATA ZSM    ( 4)/       1.0042100D0/
      DATA ZPM    ( 4)/       1.0042100D0/
      DATA ALPM   ( 4)/       1.6694340D0/
      DATA EISOLM ( 4)/     -24.2047560D0/
      DATA DDM    ( 4)/       1.4373245D0/
      DATA QQM    ( 4)/       1.2196103D0/
      DATA AMM    ( 4)/       0.3307607D0/
      DATA ADM    ( 4)/       0.3356142D0/
      DATA AQM    ( 4)/       0.3846373D0/
C                    DATA FOR ELEMENT  5        BORON
      DATA REFMN  ( 5)/'  B: (MNDO):  M.J.S. DEWAR, M.L. MCKEE, J. AM. C
     1HEM. SOC., 99, 5231, (1977)     '/
      DATA USSM   ( 5)/     -34.5471300D0/
      DATA UPPM   ( 5)/     -23.1216900D0/
      DATA BETASM ( 5)/      -8.2520540D0/
      DATA BETAPM ( 5)/      -8.2520540D0/
      DATA ZSM    ( 5)/       1.5068010D0/
      DATA ZPM    ( 5)/       1.5068010D0/
      DATA ALPM   ( 5)/       2.1349930D0/
      DATA EISOLM ( 5)/     -64.3159500D0/
      DATA DDM    ( 5)/       0.9579073D0/
      DATA QQM    ( 5)/       0.8128113D0/
      DATA AMM    ( 5)/       0.3891951D0/
      DATA ADM    ( 5)/       0.4904730D0/
      DATA AQM    ( 5)/       0.5556979D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFMN  ( 6)/'  C: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 6)/     -52.2797450D0/
      DATA UPPM   ( 6)/     -39.2055580D0/
      DATA BETASM ( 6)/     -18.9850440D0/
      DATA BETAPM ( 6)/      -7.9341220D0/
      DATA ZSM    ( 6)/       1.7875370D0/
      DATA ZPM    ( 6)/       1.7875370D0/
      DATA ALPM   ( 6)/       2.5463800D0/
      DATA EISOLM ( 6)/    -120.5006060D0/
      DATA DDM    ( 6)/       0.8074662D0/
      DATA QQM    ( 6)/       0.6851578D0/
      DATA AMM    ( 6)/       0.4494671D0/
      DATA ADM    ( 6)/       0.6149474D0/
      DATA AQM    ( 6)/       0.6685897D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFMN  ( 7)/'  N: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 7)/     -71.9321220D0/
      DATA UPPM   ( 7)/     -57.1723190D0/
      DATA BETASM ( 7)/     -20.4957580D0/
      DATA BETAPM ( 7)/     -20.4957580D0/
      DATA ZSM    ( 7)/       2.2556140D0/
      DATA ZPM    ( 7)/       2.2556140D0/
      DATA ALPM   ( 7)/       2.8613420D0/
      DATA EISOLM ( 7)/    -202.5662010D0/
      DATA DDM    ( 7)/       0.6399037D0/
      DATA QQM    ( 7)/       0.5429763D0/
      DATA AMM    ( 7)/       0.4994487D0/
      DATA ADM    ( 7)/       0.7843643D0/
      DATA AQM    ( 7)/       0.81264450D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFMN  ( 8)/'  O: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 8)/     -99.6443090D0/
      DATA UPPM   ( 8)/     -77.7974720D0/
      DATA BETASM ( 8)/     -32.6880820D0/
      DATA BETAPM ( 8)/     -32.6880820D0/
      DATA ZSM    ( 8)/       2.6999050D0/
      DATA ZPM    ( 8)/       2.6999050D0/
      DATA ALPM   ( 8)/       3.1606040D0/
      DATA EISOLM ( 8)/    -317.8685060D0/
      DATA DDM    ( 8)/       0.5346024D0/
      DATA QQM    ( 8)/       0.4536252D0/
      DATA AMM    ( 8)/       0.5667034D0/
      DATA ADM    ( 8)/       0.9592562D0/
      DATA AQM    ( 8)/       0.9495934D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFMN  ( 9)/'  F: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
      DATA USSM   ( 9)/    -131.0715480D0/
      DATA UPPM   ( 9)/    -105.7821370D0/
      DATA BETASM ( 9)/     -48.2904660D0/
      DATA BETAPM ( 9)/     -36.5085400D0/
      DATA ZSM    ( 9)/       2.8484870D0/
      DATA ZPM    ( 9)/       2.8484870D0/
      DATA ALPM   ( 9)/       3.4196606D0/
      DATA EISOLM ( 9)/    -476.6837810D0/
      DATA DDM    ( 9)/       0.5067166D0/
      DATA QQM    ( 9)/       0.4299633D0/
      DATA AMM    ( 9)/       0.6218302D0/
      DATA ADM    ( 9)/       1.0850301D0/
      DATA AQM    ( 9)/       1.0343643D0/
*                               DATA FOR THE SODIUM-LIKE SPARKLE
      DATA REFMN  (11)/' Na: (MNDO):  SODIUM-LIKE SPARKLE.   USE WITH CA
     1RE.                             '/
      DATA REFAM  (11)/' Na: (AM1):   SODIUM-LIKE SPARKLE.   USE WITH CA
     1RE.                             '/
      DATA REFPM3 (11)/' Na: (PM3):   SODIUM-LIKE SPARKLE.   USE WITH CA
     1RE.                             '/
      DATA VS(11)       /10.0D0/
      DATA ALPAM1(11)      / 1.668D0/
      DATA ALPM(11)        / 1.660D0/
      DATA ALPPM3(11)      / 1.681D0/
C
      DATA EISOLA(11)      / 0.0D0/
      DATA EISOLM(11)      / 0.0D0/
      DATA EISOLP(11)      / 0.0D0/
C
      DATA AMAM1(11)       / 0.5D0/
      DATA AMM(11)         / 0.5D0/
      DATA AMPM3(11)       / 0.5D0/
C                    DATA FOR ELEMENT 13        ALUMINUM
      DATA REFMN  (13)/' Al: (MNDO):  L.P. DAVIS, ET.AL.  J. COMP. CHEM.
     1, 2, 433, (1981) SEE MANUAL.    '/
      DATA USSM   (13)/     -23.8070970D0/
      DATA UPPM   (13)/     -17.5198780D0/
      DATA BETASM (13)/      -2.6702840D0/
      DATA BETAPM (13)/      -2.6702840D0/
      DATA ZSM    (13)/       1.4441610D0/
      DATA ZPM    (13)/       1.4441610D0/
      DATA ZDM    (13)/       1.0000000D0/
      DATA ALPM   (13)/       1.8688394D0/
      DATA EISOLM (13)/     -44.4840720D0/
      DATA DDM    (13)/       1.3992387D0/
      DATA QQM    (13)/       1.1586797D0/
      DATA AMM    (13)/       0.2973172D0/
      DATA ADM    (13)/       0.2635574D0/
      DATA AQM    (13)/       0.3673560D0/
C                    DATA FOR ELEMENT 14          SILICON
      DATA REFMN  (14)/' Si: (MNDO): M.J.S.DEWAR, ET. AL. ORGANOMETALLIC
     1S  5, 375 (1986)                '/
      DATA USSM   (14)/     -37.0375330D0/
      DATA UPPM   (14)/     -27.7696780D0/
      DATA BETASM (14)/      -9.0868040D0/
      DATA BETAPM (14)/      -1.0758270D0/
      DATA ZSM    (14)/       1.3159860D0/
      DATA ZPM    (14)/       1.7099430D0/
      DATA ZDM    (14)/       1.0000000D0/
      DATA ALPM   (14)/       2.2053160D0/
      DATA EISOLM (14)/     -82.8394220D0/
      DATA DDM    (14)/       1.2580349D0/
      DATA QQM    (14)/       0.9785824D0/
      DATA AMM    (14)/       0.3608967D0/
      DATA ADM    (14)/       0.3664244D0/
      DATA AQM    (14)/       0.4506740D0/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFMN  (15)/'  P: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSM   (15)/     -56.1433600D0/
      DATA UPPM   (15)/     -42.8510800D0/
      DATA BETASM (15)/      -6.7916000D0/
      DATA BETAPM (15)/      -6.7916000D0/
      DATA ZSM    (15)/       2.1087200D0/
      DATA ZPM    (15)/       1.7858100D0/
      DATA ZDM    (15)/       1.0000000D0/
      DATA ALPM   (15)/       2.4152800D0/
      DATA EISOLM (15)/    -152.9599600D0/
      DATA DDM    (15)/       1.0129699D0/
      DATA QQM    (15)/       0.9370090D0/
      DATA AMM    (15)/       0.4248438D0/
      DATA ADM    (15)/       0.4882420D0/
      DATA AQM    (15)/       0.4979406D0/
C                    DATA FOR ELEMENT 16        SULFUR
C
      DATA REFMN  (16)/'  S: (MNDO): M.J.S.DEWAR, C.H. REYNOLDS, J. COM
     1P. CHEM. 7, 140-143 (1986)      '/
      DATA USSM   (16)/     -72.2422810D0/
      DATA UPPM   (16)/     -56.9732070D0/
      DATA BETASM (16)/     -10.7616700D0/
      DATA BETAPM (16)/     -10.1084330D0/
      DATA ZSM    (16)/       2.3129620D0/
      DATA ZPM    (16)/       2.0091460D0/
      DATA ZDM    (16)/       1.0000000D0/
      DATA ALPM   (16)/       2.4780260D0/
      DATA EISOLM (16)/    -226.0123900D0/
      DATA DDM    (16)/       0.9189935D0/
      DATA QQM    (16)/       0.8328514D0/
      DATA AMM    (16)/       0.4733554D0/
      DATA ADM    (16)/       0.5544502D0/
      DATA AQM    (16)/       0.5585244D0/
C                    DATA FOR ELEMENT 17        CHLORINE
      DATA REFMN  (17)/' Cl: (MNDO): M.J.S.DEWAR, H.S.RZEPA, J. COMP. CH
     1EM., 4, 158, (1983)             '/
      DATA USSM   (17)/    -100.2271660D0/
      DATA UPPM   (17)/     -77.3786670D0/
      DATA BETASM (17)/     -14.2623200D0/
      DATA BETAPM (17)/     -14.2623200D0/
      DATA ZSM    (17)/       3.7846450D0/
      DATA ZPM    (17)/       2.0362630D0/
      DATA ZDM    (17)/       1.0000000D0/
      DATA ALPM   (17)/       2.5422010D0/
      DATA EISOLM (17)/    -353.1176670D0/
      DATA DDM    (17)/       0.4986870D0/
      DATA QQM    (17)/       0.8217603D0/
      DATA AMM    (17)/       0.5523705D0/
      DATA ADM    (17)/       0.8061220D0/
      DATA AQM    (17)/       0.6053435D0/
*                               DATA FOR THE POTASSIUM-LIKE SPARKLE
      DATA REFAM   (19)/' K:  (AM1):  POTASSIUM-LIKE SPARKLE.   USE WITH
     1 CARE.                          '/
      DATA REFMN   (19)/' K:  (MNDO): POTASSIUM-LIKE SPARKLE.   USE WITH
     1 CARE.                          '/
      DATA REFPM3  (19)/' K:  (PM3):  POTASSIUM-LIKE SPARKLE.   USE WITH
     1 CARE.                          '/
      DATA VS(19)       /10.0D0/
      DATA ALPAM1(19)      / 1.405D0/
      DATA ALPM(19)        / 1.396D0/
      DATA ALPPM3(19)      / 1.400D0/
C
      DATA EISOLA(19)      / 0.0D0/
      DATA EISOLM(19)      / 0.0D0/
      DATA EISOLP(19)      / 0.0D0/
C
      DATA AMAM1(19)       / 0.5D0/
      DATA AMM(19)         / 0.5D0/
      DATA AMPM3(19)       / 0.5D0/
C                    DATA FOR ELEMENT 24  CHROMIUM
      DATA REFMN  (24)/' Cr: (MNDO):  M.J.S. DEWAR, E.F. HEALY, J.J.P.
     1STEWART (IN PREPARATION)        '/
      DATA USSM   (24)/     -17.5170270D0/
      DATA UPPM  (24)/     -12.5337290D0/
      DATA UDDM  (24)/     -44.1249280D0/
      DATA BETASM (24)/      -0.1000000D0/
      DATA BETAPM (24)/      -0.1000000D0/
      DATA BETADM (24)/      -8.7766360D0/
      DATA ZSM    (24)/       1.5000000D0/
      DATA ZPM    (24)/       1.5000000D0/
      DATA ZDM    (24)/       2.8845490D0/
      DATA ALPM   (24)/       3.0683070D0/
      DATA EISOLM (24)/    -134.8187920D0/
      DATA GSSM   (24)/       6.0000000D0/
      DATA GSPM   (24)/       4.1500000D0/
      DATA GPPM   (24)/       5.0000000D0/
      DATA GP2M   (24)/       3.5000000D0/
      DATA HSPM   (24)/       1.0000000D0/
      DATA GSD    (24)/       2.8746410D0/
      DATA GPD    (24)/       3.0000000D0/
      DATA GDD    (24)/       8.8949670D0/
      DATA DDM    (24)/       1.7320508D0/
      DATA QQM    (24)/       1.4142136D0/
      DATA AMM    (24)/       0.2205072D0/
      DATA ADM    (24)/       0.2711332D0/
      DATA AQM    (24)/       0.4464656D0/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFMN  (30)/' Zn: (MNDO):  M.J.S. DEWAR, K.M. MERZ, ORGANOMET
     1ALLICS, 5, 1494-1496 (1986)     '/
C                    DATA FOR ELEMENT 30
      DATA USSM  ( 30)/     -20.8397160D0/
      DATA UPPM  ( 30)/     -19.6252240D0/
      DATA BETASM( 30)/      -1.0000000D0/
      DATA BETAPM( 30)/      -2.0000000D0/
      DATA ZSM   ( 30)/       2.0473590D0/
      DATA ZPM   ( 30)/       1.4609460D0/
      DATA ZDM   ( 30)/       1.0000000D0/
      DATA ALPM  ( 30)/       1.5064570D0/
      DATA EISOLM( 30)/     -29.8794320D0/
      DATA GSSM  ( 30)/      11.8000000D0/
      DATA GSPM  ( 30)/      11.1820180D0/
      DATA GPPM  ( 30)/      13.3000000D0/
      DATA GP2M  ( 30)/      12.9305200D0/
      DATA HSPM  ( 30)/       0.4846060D0/
      DATA DDM   ( 30)/       1.3037826D0/
      DATA QQM   ( 30)/       1.4520183D0/
      DATA AMM   ( 30)/       0.4336641D0/
      DATA ADM   ( 30)/       0.2375912D0/
      DATA AQM   ( 30)/       0.2738858D0/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFMN  (32)/' Ge: (MNDO): M.J.S.DEWAR, G.L.GRADY, E.F.HEALY,O
     1RGANOMETALLICS 6 186-189, (1987)'/
      DATA USSM  ( 32)/     -33.9493670D0/
      DATA UPPM  ( 32)/     -27.4251050D0/
      DATA BETASM( 32)/      -4.5164790D0/
      DATA BETAPM( 32)/      -1.7555170D0/
      DATA ZSM   ( 32)/       1.2931800D0/
      DATA ZPM   ( 32)/       2.0205640D0/
      DATA ALPM  ( 32)/       1.9784980D0/
      DATA EISOLM( 32)/     -76.2489440D0/
      DATA GSSM  ( 32)/       9.8000000D0/
      DATA GSPM  ( 32)/       8.3000000D0/
      DATA GPPM  ( 32)/       7.3000000D0/
      DATA GP2M  ( 32)/       6.5000000D0/
      DATA HSPM  ( 32)/       1.3000000D0/
      DATA DDM   ( 32)/       1.2556091D0/
      DATA QQM   ( 32)/       1.0498655D0/
      DATA AMM   ( 32)/       0.3601617D0/
      DATA ADM   ( 32)/       0.3643722D0/
      DATA AQM   ( 32)/       0.4347337D0/
C                    DATA FOR ELEMENT 35        BROMINE
      DATA REFMN  (35)/' Br: (MNDO): M.J.S.DEWAR, E.F. HEALY, J. COMP. C
     1HEM., 4, 542, (1983)            '/
      DATA USSM   (35)/     -99.9864405D0/
      DATA UPPM   (35)/     -75.6713075D0/
      DATA BETASM (35)/      -8.9171070D0/
      DATA BETAPM (35)/      -9.9437400D0/
      DATA ZSM    (35)/       3.8543019D0/
      DATA ZPM    (35)/       2.1992091D0/
      DATA ZDM    (35)/       1.0000000D0/
      DATA ALPM   (35)/       2.4457051D0/
      DATA EISOLM (35)/    -346.6812500D0/
      DATA DDM    (35)/       0.6051074D0/
      DATA QQM    (35)/       0.9645873D0/
      DATA AMM    (35)/       0.5526068D0/
      DATA ADM    (35)/       0.7258330D0/
      DATA AQM    (35)/       0.5574589D0/
C                    DATA FOR ELEMENT 50        TIN
      DATA REFMN  (50)/' Sn: (MNDO): M.J.S.DEWAR,G.L.GRADY,J.J.P.STEWART
     1, J.AM.CHEM.SOC.,106 6771 (1984)'/
      DATA USSM  (50)/     -40.8518020D0/
      DATA UPPM   (50)/     -28.5602490D0/
      DATA BETASM (50)/      -3.2351470D0/
      DATA BETAPM (50)/      -4.2904160D0/
      DATA ZSM    (50)/       2.0803800D0/
      DATA ZPM   (50)/       1.9371060D0/
      DATA ALPM   (50)/       1.8008140D0/
      DATA EISOLM (50)/     -92.3241020D0/
      DATA GSSM   (50)/       9.8000000D0/
      DATA GSPM   (50)/       8.3000000D0/
      DATA GPPM   (50)/       7.3000000D0/
      DATA GP2M   (50)/       6.5000000D0/
      DATA HSPM   (50)/       1.3000000D0/
      DATA DDM    (50)/       1.5697766D0/
      DATA QQM    (50)/       1.3262292D0/
      DATA AMM    (50)/       0.3601617D0/
      DATA ADM    (50)/       0.3219998D0/
      DATA AQM    (50)/       0.3713827D0/
C                    DATA FOR ELEMENT 53        IODINE
      DATA REFMN  (53)/'  I: (MNDO): M.J.S.DEWAR, E.F. HEALY, J.J.P. STE
     1WART, J.COMP.CHEM., 5,358,(1984)'/
      DATA USSM   (53)/    -100.0030538D0/
      DATA UPPM   (53)/     -74.6114692D0/
      DATA BETASM (53)/      -7.4144510D0/
      DATA BETAPM (53)/      -6.1967810D0/
      DATA ZSM    (53)/       2.2729610D0/
      DATA ZPM    (53)/       2.1694980D0/
      DATA ZDM    (53)/       1.0000000D0/
      DATA ALPM   (53)/       2.2073200D0/
      DATA EISOLM (53)/    -340.5983600D0/
      DATA DDM    (53)/       1.4253233D0/
      DATA QQM    (53)/       1.1841707D0/
      DATA AMM    (53)/       0.5527541D0/
      DATA ADM    (53)/       0.4593451D0/
      DATA AQM    (53)/       0.4585376D0/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFMN  (80)/' Hg: (MNDO): M.J.S.DEWAR,  ET. AL. ORGANOMETALLI
     1CS 4, 1964, (1985) SEE MANUAL   '/
      DATA USSM   ( 80)/     -19.8095740D0/
      DATA UPPM   ( 80)/     -13.1025300D0/
      DATA BETASM ( 80)/      -0.4045250D0/
      DATA BETAPM ( 80)/      -6.2066830D0/
      DATA ZSM    ( 80)/       2.2181840D0/
      DATA ZPM    ( 80)/       2.0650380D0/
      DATA ALPM   ( 80)/       1.3356410D0/
      DATA EISOLM ( 80)/     -28.8191480D0/
      DATA GSSM   ( 80)/      10.8000000D0/
      DATA GSPM   ( 80)/       9.3000000D0/
      DATA GPPM   ( 80)/      14.3000000D0/
      DATA GP2M   ( 80)/      13.5000000D0/
      DATA HSPM   ( 80)/       1.3000000D0/
      DATA DDM    ( 80)/       1.7378048D0/
      DATA QQM    ( 80)/       1.4608064D0/
      DATA AMM    ( 80)/       0.3969129D0/
      DATA ADM    ( 80)/       0.3047694D0/
      DATA AQM    ( 80)/       0.3483102D0/
C                    DATA FOR ELEMENT 82        LEAD
      DATA REFMN  (82)/' Pb: (MNDO): M.J.S.DEWAR, ET.AL ORGANOMETALLICS
     14 1973-1980 (1985)              '/
      DATA USSM   ( 82)/     -47.3196920D0/
      DATA UPPM   ( 82)/     -28.8475600D0/
      DATA BETASM ( 82)/      -8.0423870D0/
      DATA BETAPM ( 82)/      -3.0000000D0/
      DATA ZSM    ( 82)/       2.4982860D0/
      DATA ZPM    ( 82)/       2.0820710D0/
      DATA ALPM   ( 82)/       1.7283330D0/
      DATA EISOLM ( 82)/    -105.8345040D0/
      DATA GSSM   ( 82)/       9.8000000D0/
      DATA GSPM   ( 82)/       8.3000000D0/
      DATA GPPM   ( 82)/       7.3000000D0/
      DATA GP2M   ( 82)/       6.5000000D0/
      DATA HSPM   ( 82)/       1.3000000D0/
      DATA DDM    ( 82)/       1.5526624D0/
      DATA QQM    ( 82)/       1.4488558D0/
      DATA AMM    ( 82)/       0.3601617D0/
      DATA ADM    ( 82)/       0.3239309D0/
      DATA AQM    ( 82)/       0.3502057D0/
C
C     START OF "OLD" ELEMENTS: THESE ARE OLD PARAMETERS WHICH
C     CAN BE USED, IF DESIRED, BY SPECIFYING "<CHEMICAL SYMBOL>YEAR"
C     AS IN SI1978 OR  S1983.
C
C                    DATA FOR ELEMENT 90        SILICON
      DATA REFMN  (90)/' Si: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSM   (90)/     -40.5682920D0/
      DATA UPPM   (90)/     -28.0891870D0/
      DATA BETASM (90)/      -4.2562180D0/
      DATA BETAPM (90)/      -4.2562180D0/
      DATA ZSM    (90)/       1.4353060D0/
      DATA ZPM    (90)/       1.4353060D0/
      DATA ZDM    (90)/       1.0000000D0/
      DATA ALPM   (90)/       2.1961078D0/
      DATA EISOLM (90)/     -90.5399580D0/
      DATA DDM    (90)/       1.4078712D0/
      DATA QQM    (90)/       1.1658281D0/
      DATA AMM    (90)/       0.3608967D0/
      DATA ADM    (90)/       0.3441817D0/
      DATA AQM    (90)/       0.3999442D0/
      DATA HSPM(90)/1.32D00/
      DATA GP2M(90)/6.54D00/
      DATA GPPM(90)/7.31D00/
      DATA GSPM(90)/8.36D00/
      DATA GSSM(90)/9.82D00/
      DATA REFMN  (91)/'  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE,
     1 J.AM.CHEM.SOC.100, 3607 (1978).'/
      DATA USSM   (91)/     -75.2391520D0/
      DATA UPPM   (91)/     -57.8320130D0/
      DATA BETASM (91)/     -11.1422310D0/
      DATA BETAPM (91)/     -11.1422310D0/
      DATA ZSM    (91)/       2.6135910D0/
      DATA ZPM    (91)/       2.0343930D0/
      DATA ZDM    (91)/       1.0000000D0/
      DATA ALPM   (91)/       2.4916445D0/
      DATA EISOLM (91)/    -235.4413560D0/
      DATA DDM    (91)/       0.8231596D0/
      DATA QQM    (91)/       0.8225156D0/
      DATA AMM    (91)/       0.4733554D0/
      DATA ADM    (91)/       0.5889395D0/
      DATA AQM    (91)/       0.5632724D0/
      DATA REFMN (102)/' Cb: (MNDO):  Capped Bond  (Hydrogen-like, takes
     1 on a  zero charge.)            '/
      DATA USSM  (102)/     -11.9062760D0/
      DATA BETASM(102)/-9999999.0000000D0/
      DATA ZSM   (102)/       4.0000000D0/
      DATA ZPM   (102)/       0.3000000D0/
      DATA ZDM   (102)/       0.3000000D0/
      DATA ALPM  (102)/       2.5441341D0/
      DATA EISOLM(102)/       4.0000000D0/
      DATA GSSM  (102)/      12.8480000D0/
      DATA HSPM  (102)/       0.1000000D0/
      DATA DDM   (102)/       0.0684105D0/
      DATA QQM   (102)/       1.0540926D0/
      DATA AMM   (102)/       0.4721793D0/
      DATA ADM   (102)/       0.9262742D0/
      DATA AQM   (102)/       0.2909059D0/
***********************************************************************
*
*    START OF AM1 PARAMETERS
*
***********************************************************************
C                    DATA FOR ELEMENT  1       AM1:   HYDROGEN
      DATA REFAM  ( 1)/'  H: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 1)/     -11.3964270D0/
      DATA BETASA( 1)/      -6.1737870D0/
      DATA ZS AM1( 1)/       1.1880780D0/
      DATA ALPAM1( 1)/       2.8823240D0/
      DATA EISOLA( 1)/     -11.3964270D0/
      DATA GSSAM1( 1)/      12.8480000D0/
      DATA AM AM1( 1)/       0.4721793D0/
      DATA AD AM1( 1)/       0.4721793D0/
      DATA AQ AM1( 1)/       0.4721793D0/
      DATA GUESA1( 1,1)/       0.1227960D0/
      DATA GUESA2( 1,1)/       5.0000000D0/
      DATA GUESA3( 1,1)/       1.2000000D0/
      DATA GUESA1( 1,2)/       0.0050900D0/
      DATA GUESA2( 1,2)/       5.0000000D0/
      DATA GUESA3( 1,2)/       1.8000000D0/
      DATA GUESA1( 1,3)/      -0.0183360D0/
      DATA GUESA2( 1,3)/       2.0000000D0/
      DATA GUESA3( 1,3)/       2.1000000D0/
C                    DATA FOR ELEMENT  3       AM1:   LITHIUM    *
      DATA REFAM  ( 3)/' Li: (MNDO):  TAKEN FROM MNDOC BY W.THIEL,
     1QCPE NO.438, V. 2, P.63, (1982).'/
      DATA USSAM1(  3)/      -5.1280000D0/
      DATA UPPAM1(  3)/      -2.7212000D0/
      DATA BETASA(  3)/      -1.3500400D0/
      DATA BETAPA(  3)/      -1.3500400D0/
      DATA ZS AM1(  3)/       0.7023800D0/
      DATA ZP AM1(  3)/       0.7023800D0/
      DATA ALPAM1(  3)/       1.2501400D0/
      DATA EISOLA(  3)/      -5.1280000D0/
      DATA GSSAM1(  3)/       7.3000000D0/
      DATA GSPAM1(  3)/       5.4200000D0/
      DATA GPPAM1(  3)/       5.0000000D0/
      DATA GP2AM1(  3)/       4.5200000D0/
      DATA HSPAM1(  3)/       0.8300000D0/
      DATA DD AM1(  3)/       2.0549783D0/
      DATA QQ AM1(  3)/       1.7437069D0/
      DATA AM AM1(  3)/       0.2682837D0/
      DATA AD AM1(  3)/       0.2269793D0/
      DATA AQ AM1(  3)/       0.2614581D0/
C                    DATA FOR ELEMENT  4       AM1:   BERYLLIUM  *
      DATA REFAM  ( 4)/' Be: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
      DATA USSAM1( 4)/     -16.6023780D0/
      DATA UPPAM1( 4)/     -10.7037710D0/
      DATA BETASA( 4)/      -4.0170960D0/
      DATA BETAPA( 4)/      -4.0170960D0/
      DATA ZS AM1( 4)/       1.0042100D0/
      DATA ZP AM1( 4)/       1.0042100D0/
      DATA ALPAM1( 4)/       1.6694340D0/
      DATA EISOLA( 4)/     -24.2047560D0/
      DATA GSSAM1( 4)/       9.0000000D0/
      DATA GSPAM1( 4)/       7.4300000D0/
      DATA GPPAM1( 4)/       6.9700000D0/
      DATA GP2AM1( 4)/       6.2200000D0/
      DATA HSPAM1( 4)/       1.2800000D0/
      DATA DD AM1( 4)/       1.4373245D0/
      DATA QQ AM1( 4)/       1.2196103D0/
      DATA AM AM1( 4)/       0.3307607D0/
      DATA AD AM1( 4)/       0.3356142D0/
      DATA AQ AM1( 4)/       0.3846373D0/
C                    DATA FOR ELEMENT  5       AM1:   BORON  *
      DATA REFAM  ( 5)/'  B: (AM1):  M.J.S. DEWAR, C. JIE, E. G. ZOEBISC
     1H ORGANOMETALLICS 7, 513 (1988) '/
C                    DATA FOR ELEMENT  5
      DATA USSAM1(  5)/     -34.4928700D0/
      DATA UPPAM1(  5)/     -22.6315250D0/
      DATA BETASA(  5)/      -9.5991140D0/
      DATA BETAPA(  5)/      -6.2737570D0/
      DATA ZS AM1(  5)/       1.6117090D0/
      DATA ZP AM1(  5)/       1.5553850D0/
      DATA ALPAM1(  5)/       2.4469090D0/
      DATA EISOLA(  5)/     -63.7172650D0/
      DATA GSSAM1(  5)/      10.5900000D0/
      DATA GSPAM1(  5)/       9.5600000D0/
      DATA GPPAM1(  5)/       8.8600000D0/
      DATA GP2AM1(  5)/       7.8600000D0/
      DATA HSPAM1(  5)/       1.8100000D0/
      DATA DD AM1(  5)/       0.9107622D0/
      DATA QQ AM1(  5)/       0.7874223D0/
      DATA AM AM1(  5)/       0.3891951D0/
      DATA AD AM1(  5)/       0.5045152D0/
      DATA AQ AM1(  5)/       0.5678856D0/
C                    DATA FOR ELEMENT  6       AM1:   CARBON
      DATA REFAM  ( 6)/'  C: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 6)/     -52.0286580D0/
      DATA UPPAM1( 6)/     -39.6142390D0/
      DATA BETASA( 6)/     -15.7157830D0/
      DATA BETAPA( 6)/      -7.7192830D0/
      DATA ZS AM1( 6)/       1.8086650D0/
      DATA ZP AM1( 6)/       1.6851160D0/
      DATA ALPAM1( 6)/       2.6482740D0/
      DATA EISOLA( 6)/    -120.8157940D0/
      DATA GSSAM1( 6)/      12.2300000D0/
      DATA GSPAM1( 6)/      11.4700000D0/
      DATA GPPAM1( 6)/      11.0800000D0/
      DATA GP2AM1( 6)/       9.8400000D0/
      DATA HSPAM1( 6)/       2.4300000D0/
      DATA DD AM1( 6)/       0.8236736D0/
      DATA QQ AM1( 6)/       0.7268015D0/
      DATA AM AM1( 6)/       0.4494671D0/
      DATA AD AM1( 6)/       0.6082946D0/
      DATA AQ AM1( 6)/       0.6423492D0/
      DATA GUESA1( 6,1)/       0.0113550D0/
      DATA GUESA2( 6,1)/       5.0000000D0/
      DATA GUESA3( 6,1)/       1.6000000D0/
      DATA GUESA1( 6,2)/       0.0459240D0/
      DATA GUESA2( 6,2)/       5.0000000D0/
      DATA GUESA3( 6,2)/       1.8500000D0/
      DATA GUESA1( 6,3)/      -0.0200610D0/
      DATA GUESA2( 6,3)/       5.0000000D0/
      DATA GUESA3( 6,3)/       2.0500000D0/
      DATA GUESA1( 6,4)/      -0.0012600D0/
      DATA GUESA2( 6,4)/       5.0000000D0/
      DATA GUESA3( 6,4)/       2.6500000D0/
C                    DATA FOR ELEMENT  7       AM1:   NITROGEN
      DATA REFAM  ( 7)/'  N: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 7)/     -71.8600000D0/
      DATA UPPAM1( 7)/     -57.1675810D0/
      DATA BETASA( 7)/     -20.2991100D0/
      DATA BETAPA( 7)/     -18.2386660D0/
      DATA ZS AM1( 7)/       2.3154100D0/
      DATA ZP AM1( 7)/       2.1579400D0/
      DATA ALPAM1( 7)/       2.9472860D0/
      DATA EISOLA( 7)/    -202.4077430D0/
      DATA GSSAM1( 7)/      13.5900000D0/
      DATA GSPAM1( 7)/      12.6600000D0/
      DATA GPPAM1( 7)/      12.9800000D0/
      DATA GP2AM1( 7)/      11.5900000D0/
      DATA HSPAM1( 7)/       3.1400000D0/
      DATA DD AM1( 7)/       0.6433247D0/
      DATA QQ AM1( 7)/       0.5675528D0/
      DATA AM AM1( 7)/       0.4994487D0/
      DATA AD AM1( 7)/       0.7820840D0/
      DATA AQ AM1( 7)/       0.7883498D0/
      DATA GUESA1( 7,1)/       0.0252510D0/
      DATA GUESA2( 7,1)/       5.0000000D0/
      DATA GUESA3( 7,1)/       1.5000000D0/
      DATA GUESA1( 7,2)/       0.0289530D0/
      DATA GUESA2( 7,2)/       5.0000000D0/
      DATA GUESA3( 7,2)/       2.1000000D0/
      DATA GUESA1( 7,3)/      -0.0058060D0/
      DATA GUESA2( 7,3)/       2.0000000D0/
      DATA GUESA3( 7,3)/       2.4000000D0/
C                    DATA FOR ELEMENT  8       AM1:   OXYGEN
      DATA REFAM  ( 8)/'  O: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 8)/     -97.8300000D0/
      DATA UPPAM1( 8)/     -78.2623800D0/
      DATA BETASA( 8)/     -29.2727730D0/
      DATA BETAPA( 8)/     -29.2727730D0/
      DATA ZS AM1( 8)/       3.1080320D0/
      DATA ZP AM1( 8)/       2.5240390D0/
      DATA ALPAM1( 8)/       4.4553710D0/
      DATA EISOLA( 8)/    -316.0995200D0/
      DATA GSSAM1( 8)/      15.4200000D0/
      DATA GSPAM1( 8)/      14.4800000D0/
      DATA GPPAM1( 8)/      14.5200000D0/
      DATA GP2AM1( 8)/      12.9800000D0/
      DATA HSPAM1( 8)/       3.9400000D0/
      DATA DD AM1( 8)/       0.4988896D0/
      DATA QQ AM1( 8)/       0.4852322D0/
      DATA AM AM1( 8)/       0.5667034D0/
      DATA AD AM1( 8)/       0.9961066D0/
      DATA AQ AM1( 8)/       0.9065223D0/
      DATA GUESA1( 8,1)/       0.2809620D0/
      DATA GUESA2( 8,1)/       5.0000000D0/
      DATA GUESA3( 8,1)/       0.8479180D0/
      DATA GUESA1( 8,2)/       0.0814300D0/
      DATA GUESA2( 8,2)/       7.0000000D0/
      DATA GUESA3( 8,2)/       1.4450710D0/
C                    DATA FOR ELEMENT  9       AM1:   FLUORINE  *
      DATA REFAM  ( 9)/'  F: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1( 9)/    -136.1055790D0/
      DATA UPPAM1( 9)/    -104.8898850D0/
      DATA BETASA( 9)/     -69.5902770D0/
      DATA BETAPA( 9)/     -27.9223600D0/
      DATA ZS AM1( 9)/       3.7700820D0/
      DATA ZP AM1( 9)/       2.4946700D0/
      DATA ALPAM1( 9)/       5.5178000D0/
      DATA EISOLA( 9)/    -482.2905830D0/
      DATA GSSAM1( 9)/      16.9200000D0/
      DATA GSPAM1( 9)/      17.2500000D0/
      DATA GPPAM1( 9)/      16.7100000D0/
      DATA GP2AM1( 9)/      14.9100000D0/
      DATA HSPAM1( 9)/       4.8300000D0/
      DATA DD AM1( 9)/       0.4145203D0/
      DATA QQ AM1( 9)/       0.4909446D0/
      DATA AM AM1( 9)/       0.6218302D0/
      DATA AD AM1( 9)/       1.2088792D0/
      DATA AQ AM1( 9)/       0.9449355D0/
      DATA GUESA1( 9,1)/       0.2420790D0/
      DATA GUESA2( 9,1)/       4.8000000D0/
      DATA GUESA3( 9,1)/       0.9300000D0/
      DATA GUESA1( 9,2)/       0.0036070D0/
      DATA GUESA2( 9,2)/       4.6000000D0/
      DATA GUESA3( 9,2)/       1.6600000D0/
C                    DATA FOR ELEMENT 13       AM1:   ALUMINUM  *
      DATA REFAM  (13)/' Al: (AM1):  M. J. S. Dewar, A. J. Holder, Organ
     1ometallics, 9, 508-511 (1990).  '/
      DATA USSAM1( 13)/     -24.3535850D0/
      DATA UPPAM1( 13)/     -18.3636450D0/
      DATA BETASA( 13)/      -3.8668220D0/
      DATA BETAPA( 13)/      -2.3171460D0/
      DATA ZSAM1 ( 13)/       1.5165930D0/
      DATA ZPAM1 ( 13)/       1.3063470D0/
      DATA ZDAM1 ( 13)/       1.0000000D0/
      DATA ALPAM1( 13)/       1.9765860D0/
      DATA EISOLA( 13)/     -46.4208150D0/
      DATA GSSAM1( 13)/       8.0900000D0/
      DATA GSPAM1( 13)/       6.6300000D0/
      DATA GPPAM1( 13)/       5.9800000D0/
      DATA GP2AM1( 13)/       5.4000000D0/
      DATA HSPAM1( 13)/       0.7000000D0/
      DATA DDAM1 ( 13)/       1.4040443D0/
      DATA QQAM1 ( 13)/       1.2809154D0/
      DATA AMAM1 ( 13)/       0.2973172D0/
      DATA ADAM1 ( 13)/       0.2630229D0/
      DATA AQAM1 ( 13)/       0.3427832D0/
      DATA GUESA1( 13,1)/       0.0900000D0/
      DATA GUESA2( 13,1)/      12.3924430D0/
      DATA GUESA3( 13,1)/       2.0503940D0/
C                    DATA FOR ELEMENT 14       AM1:   SILICON  *
      DATA REFAM  (14)/' Si: (AM1): M.J.S.DEWAR, C. JIE, ORGANOMETALLICS
     1, 6, 1486-1490 (1987).          '/
      DATA USSAM1(14)/     -33.9536220D0/
      DATA UPPAM1(14)/     -28.9347490D0/
      DATA BETASA(14)/      -3.784852D0/
      DATA BETAPA(14)/      -1.968123D0/
      DATA ZS AM1(14)/       1.830697D0/
      DATA ZP AM1(14)/       1.2849530D0/
      DATA ZD AM1(14)/       1.0000000D0/
      DATA ALPAM1(14)/       2.257816D0/
      DATA EISOLA(14)/     -79.0017420D0/
      DATA GSSAM1(14)/       9.8200000D0/
      DATA GSPAM1(14)/       8.3600000D0/
      DATA GPPAM1(14)/       7.3100000D0/
      DATA GP2AM1(14)/       6.5400000D0/
      DATA HSPAM1(14)/       1.3200000D0/
      DATA DD AM1(14)/       1.1631107D0/
      DATA QQ AM1(14)/       1.3022422D0/
      DATA AM AM1(14)/       0.3608967D0/
      DATA AD AM1(14)/       0.3829813D0/
      DATA AQ AM1(14)/       0.3712106D0/
      DATA GUESA1(14,1)/       0.25D0/
      DATA GUESA2(14,1)/       9.000D0/
      DATA GUESA3(14,1)/       0.911453D0/
      DATA GUESA1(14,2)/       0.061513D0/
      DATA GUESA2(14,2)/       5.00D0/
      DATA GUESA3(14,2)/       1.995569D0/
      DATA GUESA1(14,3)/       0.0207890D0/
      DATA GUESA2(14,3)/       5.00D0/
      DATA GUESA3(14,3)/       2.990610D0/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFAM  (15)/'  P: (AM1): M.J.S.DEWAR, JIE, C, THEOCHEM, 187,
     11 (1989)                        '/
      DATA USSAM1( 15)/     -42.0298630D0/
      DATA UPPAM1( 15)/     -34.0307090D0/
      DATA BETASA( 15)/      -6.3537640D0/
      DATA BETAPA( 15)/      -6.5907090D0/
      DATA ZS AM1( 15)/       1.9812800D0/
      DATA ZP AM1( 15)/       1.8751500D0/
      DATA ZD AM1( 15)/       1.0000000D0/
      DATA ALPAM1( 15)/       2.4553220D0/
      DATA EISOLA( 15)/    -124.4368355D0/
      DATA GSSAM1( 15)/      11.5600050D0/
      DATA GSPAM1( 15)/       5.2374490D0/
      DATA GPPAM1( 15)/       7.8775890D0/
      DATA GP2AM1( 15)/       7.3076480D0/
      DATA HSPAM1( 15)/       0.7792380D0/
      DATA DD AM1( 15)/       1.0452022D0/
      DATA QQ AM1( 15)/       0.8923660D0/
      DATA AM AM1( 15)/       0.4248440D0/
      DATA AD AM1( 15)/       0.3275319D0/
      DATA AQ AM1( 15)/       0.4386854D0/
      DATA GUESA1( 15,1)/      -0.0318270D0/
      DATA GUESA2( 15,1)/       6.0000000D0/
      DATA GUESA3( 15,1)/       1.4743230D0/
      DATA GUESA1( 15,2)/       0.0184700D0/
      DATA GUESA2( 15,2)/       7.0000000D0/
      DATA GUESA3( 15,2)/       1.7793540D0/
      DATA GUESA1( 15,3)/       0.0332900D0/
      DATA GUESA2( 15,3)/       9.0000000D0/
      DATA GUESA3( 15,3)/       3.0065760D0/
C                    DATA FOR ELEMENT 16       AM1:   SULFUR  *
C
      DATA REFAM  (16)/'  S: (AM1): M.J.S.DEWAR, Y-C YUAN, THEOCHEM, IN
     1 PRESS                          '/
      DATA USSAM1(16)/     -56.6940560D0/
      DATA UPPAM1(16)/     -48.7170490D0/
      DATA BETASA(16)/      -3.9205660D0/
      DATA BETAPA(16)/      -7.9052780D0/
      DATA ZS AM1(16)/       2.3665150D0/
      DATA ZP AM1(16)/       1.6672630D0/
      DATA ZD AM1(16)/       1.0000000D0/
      DATA ALPAM1(16)/       2.4616480D0/
      DATA EISOLA(16)/    -191.7321930D0/
      DATA GSSAM1(16)/      11.7863290D0/
      DATA GSPAM1(16)/       8.6631270D0/
      DATA GPPAM1(16)/      10.0393080D0/
      DATA GP2AM1(16)/       7.7816880D0/
      DATA HSPAM1(16)/       2.5321370D0/
      DATA DD AM1(16)/       0.9004265D0/
      DATA QQ AM1(16)/       1.0036329D0/
      DATA AM AM1(16)/       0.4331617D0/
      DATA AD AM1(16)/       0.5907115D0/
      DATA AQ AM1(16)/       0.6454943D0/
      DATA GUESA1(16,1)/      -0.5091950D0/
      DATA GUESA2(16,1)/       4.5936910D0/
      DATA GUESA3(16,1)/       0.7706650D0/
      DATA GUESA1(16,2)/      -0.0118630D0/
      DATA GUESA2(16,2)/       5.8657310D0/
      DATA GUESA3(16,2)/       1.5033130D0/
      DATA GUESA1(16,3)/       0.0123340D0/
      DATA GUESA2(16,3)/      13.5573360D0/
      DATA GUESA3(16,3)/       2.0091730D0/
C                    DATA FOR ELEMENT 17       AM1:   CHLORINE  *
      DATA REFAM  (17)/' Cl: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1(17)/    -111.6139480D0/
      DATA UPPAM1(17)/     -76.6401070D0/
      DATA BETASA(17)/     -24.5946700D0/
      DATA BETAPA(17)/     -14.6372160D0/
      DATA ZS AM1(17)/       3.6313760D0/
      DATA ZP AM1(17)/       2.0767990D0/
      DATA ZD AM1(17)/       1.0000000D0/
      DATA ALPAM1(17)/       2.9193680D0/
      DATA EISOLA(17)/    -372.1984310D0/
      DATA GSSAM1(17)/      15.0300000D0/
      DATA GSPAM1(17)/      13.1600000D0/
      DATA GPPAM1(17)/      11.3000000D0/
      DATA GP2AM1(17)/       9.9700000D0/
      DATA HSPAM1(17)/       2.4200000D0/
      DATA DD AM1(17)/       0.5406286D0/
      DATA QQ AM1(17)/       0.8057208D0/
      DATA AM AM1(17)/       0.5523705D0/
      DATA AD AM1(17)/       0.7693200D0/
      DATA AQ AM1(17)/       0.6133369D0/
      DATA GUESA1(17,1)/       0.0942430D0/
      DATA GUESA2(17,1)/       4.0000000D0/
      DATA GUESA3(17,1)/       1.3000000D0/
      DATA GUESA1(17,2)/       0.0271680D0/
      DATA GUESA2(17,2)/       4.0000000D0/
      DATA GUESA3(17,2)/       2.1000000D0/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFAM  (30)/' Zn: (AM1):  M.J.S. DEWAR, K.M. MERZ, ORGANOMET
     1ALLICS, 7, 522-524 (1988)       '/
      DATA USSAM1( 30)/     -21.0400080D0/
      DATA UPPAM1( 30)/     -17.6555740D0/
      DATA BETASA( 30)/      -1.9974290D0/
      DATA BETAPA( 30)/      -4.7581190D0/
      DATA ZS AM1( 30)/       1.9542990D0/
      DATA ZP AM1( 30)/       1.3723650D0/
      DATA ZD AM1( 30)/       1.0000000D0/
      DATA ALPAM1( 30)/       1.4845630D0/
      DATA EISOLA( 30)/     -30.2800160D0/
      DATA GSSAM1( 30)/      11.8000000D0/
      DATA GSPAM1( 30)/      11.1820180D0/
      DATA GPPAM1( 30)/      13.3000000D0/
      DATA GP2AM1( 30)/      12.9305200D0/
      DATA HSPAM1( 30)/       0.4846060D0/
      DATA DD AM1( 30)/       1.3581113D0/
      DATA QQ AM1( 30)/       1.5457406D0/
      DATA AM AM1( 30)/       0.4336641D0/
      DATA AD AM1( 30)/       0.2317423D0/
      DATA AQ AM1( 30)/       0.2621165D0/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFAM  (32)/' Ge: (AM1): M.J.S.Dewar and C.Jie, Organometalli
     1cs, 8, 1544, (1989)             '/
      DATA USSAM1( 32)/     -34.1838890D0/
      DATA UPPAM1( 32)/     -28.6408110D0/
      DATA BETASA( 32)/      -4.3566070D0/
      DATA BETAPA( 32)/      -0.9910910D0/
      DATA ZS AM1( 32)/       1.2196310D0/
      DATA ZP AM1( 32)/       1.9827940D0/
      DATA ALPAM1( 32)/       2.1364050D0/
      DATA EISOLA( 32)/     -78.7084810D0/
      DATA GSSAM1( 32)/      10.1686050D0/
      DATA GSPAM1( 32)/       8.1444730D0/
      DATA GPPAM1( 32)/       6.6719020D0/
      DATA GP2AM1( 32)/       6.2697060D0/
      DATA HSPAM1( 32)/       0.9370930D0/
      DATA DD AM1( 32)/       1.2472095D0/
      DATA QQ AM1( 32)/       1.0698642D0/
      DATA AM AM1( 32)/       0.3737084D0/
      DATA AD AM1( 32)/       0.3180309D0/
      DATA AQ AM1( 32)/       0.3485612D0/
C                    DATA FOR ELEMENT 35       AM1:   BROMINE  *
      DATA REFAM  (35)/' Br: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1(35)/    -104.6560630D0/
      DATA UPPAM1(35)/     -74.9300520D0/
      DATA BETASA(35)/     -19.3998800D0/
      DATA BETAPA(35)/      -8.9571950D0/
      DATA ZS AM1(35)/       3.0641330D0/
      DATA ZP AM1(35)/       2.0383330D0/
      DATA ZD AM1(35)/       1.0000000D0/
      DATA ALPAM1(35)/       2.5765460D0/
      DATA EISOLA(35)/    -352.3142087D0/
      DATA GSSAM1(35)/      15.0364395D0/
      DATA GSPAM1(35)/      13.0346824D0/
      DATA GPPAM1(35)/      11.2763254D0/
      DATA GP2AM1(35)/       9.8544255D0/
      DATA HSPAM1(35)/       2.4558683D0/
      DATA DD AM1(35)/       0.8458104D0/
      DATA QQ AM1(35)/       1.0407133D0/
      DATA AM AM1(35)/       0.5526071D0/
      DATA AD AM1(35)/       0.6024598D0/
      DATA AQ AM1(35)/       0.5307555D0/
      DATA GUESA1(35,1)/       0.0666850D0/
      DATA GUESA2(35,1)/       4.0000000D0/
      DATA GUESA3(35,1)/       1.5000000D0/
      DATA GUESA1(35,2)/       0.0255680D0/
      DATA GUESA2(35,2)/       4.0000000D0/
      DATA GUESA3(35,2)/       2.3000000D0/
C                    DATA FOR ELEMENT 53       AM1:   IODINE  *
      DATA REFAM  (53)/'  I: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1(53)/    -103.5896630D0/
      DATA UPPAM1(53)/     -74.4299970D0/
      DATA BETASA(53)/      -8.4433270D0/
      DATA BETAPA(53)/      -6.3234050D0/
      DATA ZS AM1(53)/       2.1028580D0/
      DATA ZP AM1(53)/       2.1611530D0/
      DATA ZD AM1(53)/       1.0000000D0/
      DATA ALPAM1(53)/       2.2994240D0/
      DATA EISOLA(53)/    -346.8642857D0/
      DATA GSSAM1(53)/      15.0404486D0/
      DATA GSPAM1(53)/      13.0565580D0/
      DATA GPPAM1(53)/      11.1477837D0/
      DATA GP2AM1(53)/       9.9140907D0/
      DATA HSPAM1(53)/       2.4563820D0/
      DATA DD AM1(53)/       1.4878778D0/
      DATA QQ AM1(53)/       1.1887388D0/
      DATA AM AM1(53)/       0.5527544D0/
      DATA AD AM1(53)/       0.4497523D0/
      DATA AQ AM1(53)/       0.4631775D0/
      DATA GUESA1(53,1)/       0.0043610D0/
      DATA GUESA2(53,1)/       2.3000000D0/
      DATA GUESA3(53,1)/       1.8000000D0/
      DATA GUESA1(53,2)/       0.0157060D0/
      DATA GUESA2(53,2)/       3.0000000D0/
      DATA GUESA3(53,2)/       2.2400000D0/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFAM  (80)/' Hg: (AM1): M.J.S.Dewar and C.Jie, Organometalli
     1cs 8, 1547, (1989)              '/
      DATA USSAM1( 80)/     -19.9415780D0/
      DATA UPPAM1( 80)/     -11.1108700D0/
      DATA BETASA( 80)/      -0.9086570D0/
      DATA BETAPA( 80)/      -4.9093840D0/
      DATA ZS AM1( 80)/       2.0364130D0/
      DATA ZP AM1( 80)/       1.9557660D0/
      DATA ALPAM1( 80)/       1.4847340D0/
      DATA EISOLA( 80)/     -29.0831560D0/
      DATA GSSAM1( 80)/      10.8000000D0/
      DATA GSPAM1( 80)/       9.3000000D0/
      DATA GPPAM1( 80)/      14.3000000D0/
      DATA GP2AM1( 80)/      13.5000000D0/
      DATA HSPAM1( 80)/       1.3000000D0/
      DATA DD AM1( 80)/       1.8750829D0/
      DATA QQ AM1( 80)/       1.5424241D0/
      DATA AM AM1( 80)/       0.3969129D0/
      DATA AD AM1( 80)/       0.2926605D0/
      DATA AQ AM1( 80)/       0.3360599D0/
C
C     START OF "OLD" ELEMENTS: THESE ARE OLD PARAMETERS WHICH
C     CAN BE USED, IF DESIRED, BY SPECIFYING "<CHEMICAL SYMBOL>YEAR"
C     AS IN SI1978 OR  S1983.
C
C                    DATA FOR ELEMENT 90        SILICON
      DATA REFAM  (90)/' Si: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSAM1 (90)/     -40.5682920D0/
      DATA UPPAM1 (90)/     -28.0891870D0/
      DATA BETASA (90)/      -4.2562180D0/
      DATA BETAPA (90)/      -4.2562180D0/
      DATA ZSAM1  (90)/       1.4353060D0/
      DATA ZPAM1  (90)/       1.4353060D0/
      DATA ZDAM1  (90)/       1.0000000D0/
      DATA ALPAM1  (90)/       2.1961078D0/
      DATA EISOLA (90)/     -90.5399580D0/
      DATA DDAM1  (90)/       1.4078712D0/
      DATA QQAM1  (90)/       1.1658281D0/
      DATA AMAM1  (90)/       0.3608967D0/
      DATA ADAM1  (90)/       0.3441817D0/
      DATA AQAM1  (90)/       0.3999442D0/
      DATA HSPAM1 (90)/1.32D00/
      DATA GP2AM1 (90)/6.54D00/
      DATA GPPAM1 (90)/7.31D00/
      DATA GSPAM1 (90)/8.36D00/
      DATA GSSAM1 (90)/9.82D00/
      DATA REFAM  (91)/'  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE,
     1 J.AM.CHEM.SOC.100, 3607 (1978).'/
      DATA USSAM1 (91)/     -75.2391520D0/
      DATA UPPAM1 (91)/     -57.8320130D0/
      DATA BETASA (91)/     -11.1422310D0/
      DATA BETAPA (91)/     -11.1422310D0/
      DATA ZSAM1  (91)/       2.6135910D0/
      DATA ZPAM1  (91)/       2.0343930D0/
      DATA ZDAM1  (91)/       1.0000000D0/
      DATA ALPAM1 (91)/       2.4916445D0/
      DATA EISOLA (91)/    -235.4413560D0/
      DATA GSSAM1 (91)/      12.8800000D0/
      DATA GSPAM1 (91)/      11.2600000D0/
      DATA GPPAM1 (91)/       9.9000000D0/
      DATA GP2AM1 (91)/       8.8300000D0/
      DATA HSPAM1 (91)/       2.2600000D0/
      DATA DDAM1  (91)/       0.8231596D0/
      DATA QQAM1  (91)/       0.8225156D0/
      DATA AMAM1  (91)/       0.4733554D0/
      DATA ADAM1  (91)/       0.5889395D0/
      DATA AQAM1  (91)/       0.5632724D0/
      DATA REFAM (102)/' Cb: (AM1):  Capped Bond  (Hydrogen-like, takes
     1 on zero charge.)               '/
C                    DATA FOR ELEMENT102
      DATA USSAM1 (102)/     -11.9062760D0/
      DATA BETASA (102)/-9999999.0000000D0/
      DATA ZSAM1  (102)/       4.0000000D0/
      DATA ZPAM1  (102)/       0.3000000D0/
      DATA ZDAM1  (102)/       0.3000000D0/
      DATA ALPAM1 (102)/       2.5441341D0/
      DATA EISOLA (102)/       4.0000000D0/
      DATA GSSAM1 (102)/      12.8480000D0/
      DATA HSPAM1 (102)/       0.1000000D0/
      DATA DDAM1  (102)/       0.0684105D0/
      DATA QQAM1  (102)/       1.0540926D0/
      DATA AMAM1  (102)/       0.4721793D0/
      DATA ADAM1  (102)/       0.9262742D0/
      DATA AQAM1  (102)/       0.2909059D0/
C
C     START OF MNDO-PM3 PARAMETER SET
C
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFPM3 ( 1)/'  H: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  1)/     -13.0733210D0/
      DATA BETASP(  1)/      -5.6265120D0/
      DATA ZSPM3 (  1)/       0.9678070D0/
      DATA ALPPM3(  1)/       3.3563860D0/
      DATA EISOLP(  1)/     -13.0733210D0/
      DATA GSSPM3(  1)/      14.7942080D0/
      DATA AMPM3 (  1)/       0.5437048D0/
      DATA ADPM3 (  1)/       0.5437048D0/
      DATA AQPM3 (  1)/       0.5437048D0/
      DATA GUESP1(  1,1)/       1.1287500D0/
      DATA GUESP2(  1,1)/       5.0962820D0/
      DATA GUESP3(  1,1)/       1.5374650D0/
      DATA GUESP1(  1,2)/      -1.0603290D0/
      DATA GUESP2(  1,2)/       6.0037880D0/
      DATA GUESP3(  1,2)/       1.5701890D0/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA REFPM3( 4)/ ' Be: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3(  4)/     -17.2647520D0/
      DATA UPPPM3(  4)/     -11.3042430D0/
      DATA BETASP(  4)/      -3.9620530D0/
      DATA BETAPP(  4)/      -2.7806840D0/
      DATA ZSPM3 (  4)/       0.8774390D0/
      DATA ZPPM3 (  4)/       1.5087550D0/
      DATA ALPPM3(  4)/       1.5935360D0/
      DATA EISOLP(  4)/     -25.5166530D0/
      DATA GSSPM3(  4)/       9.0128510D0/
      DATA GSPPM3(  4)/       6.5761990D0/
      DATA GPPPM3(  4)/       6.0571820D0/
      DATA GP2PM3(  4)/       9.0052190D0/
      DATA HSPPM3(  4)/       0.5446790D0/
      DATA DDPM3 (  4)/       1.0090531D0/
      DATA QQPM3 (  4)/       0.8117586D0/
      DATA AMPM3 (  4)/       0.3312330D0/
      DATA ADPM3 (  4)/       0.2908996D0/
      DATA AQPM3 (  4)/       0.3530008D0/
      DATA GUESP1(  4,1)/       1.6315720D0/
      DATA GUESP2(  4,1)/       2.6729620D0/
      DATA GUESP3(  4,1)/       1.7916860D0/
      DATA GUESP1(  4,2)/      -2.1109590D0/
      DATA GUESP2(  4,2)/       1.9685940D0/
      DATA GUESP3(  4,2)/       1.7558710D0/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFPM3 ( 6)/'  C: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  6)/     -47.2703200D0/
      DATA UPPPM3(  6)/     -36.2669180D0/
      DATA BETASP(  6)/     -11.9100150D0/
      DATA BETAPP(  6)/      -9.8027550D0/
      DATA ZSPM3 (  6)/       1.5650850D0/
      DATA ZPPM3 (  6)/       1.8423450D0/
      DATA ALPPM3(  6)/       2.7078070D0/
      DATA EISOLP(  6)/    -111.2299170D0/
      DATA GSSPM3(  6)/      11.2007080D0/
      DATA GSPPM3(  6)/      10.2650270D0/
      DATA GPPPM3(  6)/      10.7962920D0/
      DATA GP2PM3(  6)/       9.0425660D0/
      DATA HSPPM3(  6)/       2.2909800D0/
      DATA DDPM3 (  6)/       0.8332396D0/
      DATA QQPM3 (  6)/       0.6647750D0/
      DATA AMPM3 (  6)/       0.4116394D0/
      DATA ADPM3 (  6)/       0.5885862D0/
      DATA AQPM3 (  6)/       0.7647667D0/
      DATA GUESP1(  6,1)/       0.0501070D0/
      DATA GUESP2(  6,1)/       6.0031650D0/
      DATA GUESP3(  6,1)/       1.6422140D0/
      DATA GUESP1(  6,2)/       0.0507330D0/
      DATA GUESP2(  6,2)/       6.0029790D0/
      DATA GUESP3(  6,2)/       0.8924880D0/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFPM3 ( 7)/'  N: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  7)/     -49.3356720D0/
      DATA UPPPM3(  7)/     -47.5097360D0/
      DATA BETASP(  7)/     -14.0625210D0/
      DATA BETAPP(  7)/     -20.0438480D0/
      DATA ZSPM3 (  7)/       2.0280940D0/
      DATA ZPPM3 (  7)/       2.3137280D0/
      DATA ALPPM3(  7)/       2.8305450D0/
      DATA EISOLP(  7)/    -157.6137755D0/
      DATA GSSPM3(  7)/      11.9047870D0/
      DATA GSPPM3(  7)/       7.3485650D0/
      DATA GPPPM3(  7)/      11.7546720D0/
      DATA GP2PM3(  7)/      10.8072770D0/
      DATA HSPPM3(  7)/       1.1367130D0/
      DATA DDPM3 (  7)/       0.6577006D0/
      DATA QQPM3 (  7)/       0.5293383D0/
      DATA AMPM3 (  7)/       0.4375151D0/
      DATA ADPM3 (  7)/       0.5030995D0/
      DATA AQPM3 (  7)/       0.7364933D0/
      DATA GUESP1(  7,1)/       1.5016740D0/
      DATA GUESP2(  7,1)/       5.9011480D0/
      DATA GUESP3(  7,1)/       1.7107400D0/
      DATA GUESP1(  7,2)/      -1.5057720D0/
      DATA GUESP2(  7,2)/       6.0046580D0/
      DATA GUESP3(  7,2)/       1.7161490D0/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFPM3 ( 8)/'  O: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  8)/     -86.9930020D0/
      DATA UPPPM3(  8)/     -71.8795800D0/
      DATA BETASP(  8)/     -45.2026510D0/
      DATA BETAPP(  8)/     -24.7525150D0/
      DATA ZSPM3 (  8)/       3.7965440D0/
      DATA ZPPM3 (  8)/       2.3894020D0/
      DATA ALPPM3(  8)/       3.2171020D0/
      DATA EISOLP(  8)/    -289.3422065D0/
      DATA GSSPM3(  8)/      15.7557600D0/
      DATA GSPPM3(  8)/      10.6211600D0/
      DATA GPPPM3(  8)/      13.6540160D0/
      DATA GP2PM3(  8)/      12.4060950D0/
      DATA HSPPM3(  8)/       0.5938830D0/
      DATA DDPM3 (  8)/       0.4086173D0/
      DATA QQPM3 (  8)/       0.5125738D0/
      DATA AMPM3 (  8)/       0.5790430D0/
      DATA ADPM3 (  8)/       0.5299630D0/
      DATA AQPM3 (  8)/       0.8179630D0/
      DATA GUESP1(  8,1)/      -1.1311280D0/
      DATA GUESP2(  8,1)/       6.0024770D0/
      DATA GUESP3(  8,1)/       1.6073110D0/
      DATA GUESP1(  8,2)/       1.1378910D0/
      DATA GUESP2(  8,2)/       5.9505120D0/
      DATA GUESP3(  8,2)/       1.5983950D0/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFPM3 ( 9)/'  F: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  9)/    -110.4353030D0/
      DATA UPPPM3(  9)/    -105.6850470D0/
      DATA BETASP(  9)/     -48.4059390D0/
      DATA BETAPP(  9)/     -27.7446600D0/
      DATA ZSPM3 (  9)/       4.7085550D0/
      DATA ZPPM3 (  9)/       2.4911780D0/
      DATA ALPPM3(  9)/       3.3589210D0/
      DATA EISOLP(  9)/    -437.5171690D0/
      DATA GSSPM3(  9)/      10.4966670D0/
      DATA GSPPM3(  9)/      16.0736890D0/
      DATA GPPPM3(  9)/      14.8172560D0/
      DATA GP2PM3(  9)/      14.4183930D0/
      DATA HSPPM3(  9)/       0.7277630D0/
      DATA DDPM3 (  9)/       0.3125302D0/
      DATA QQPM3 (  9)/       0.4916328D0/
      DATA AMPM3 (  9)/       0.3857650D0/
      DATA ADPM3 (  9)/       0.6768503D0/
      DATA AQPM3 (  9)/       0.6120047D0/
      DATA GUESP1(  9,1)/      -0.0121660D0/
      DATA GUESP2(  9,1)/       6.0235740D0/
      DATA GUESP3(  9,1)/       1.8568590D0/
      DATA GUESP1(  9,2)/      -0.0028520D0/
      DATA GUESP2(  9,2)/       6.0037170D0/
      DATA GUESP3(  9,2)/       2.6361580D0/
C                    DATA FOR ELEMENT 12        MAGNESIUM
      DATA REFPM3(12)/ ' Mg: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 12)/     -14.6236880D0/
      DATA UPPPM3( 12)/     -14.1734600D0/
      DATA BETASP( 12)/      -2.0716910D0/
      DATA BETAPP( 12)/      -0.5695810D0/
      DATA ZSPM3 ( 12)/       0.6985520D0/
      DATA ZPPM3 ( 12)/       1.4834530D0/
      DATA ALPPM3( 12)/       1.3291470D0/
      DATA EISOLP( 12)/     -22.5530760D0/
      DATA GSSPM3( 12)/       6.6943000D0/
      DATA GSPPM3( 12)/       6.7939950D0/
      DATA GPPPM3( 12)/       6.9104460D0/
      DATA GP2PM3( 12)/       7.0908230D0/
      DATA HSPPM3( 12)/       0.5433000D0/
      DATA DDPM3 ( 12)/       1.1403950D0/
      DATA QQPM3 ( 12)/       1.1279899D0/
      DATA AMPM3 ( 12)/       0.2460235D0/
      DATA ADPM3 ( 12)/       0.2695751D0/
      DATA AQPM3 ( 12)/       0.2767522D0/
      DATA GUESP1( 12,1)/       2.1170500D0/
      DATA GUESP2( 12,1)/       6.0094770D0/
      DATA GUESP3( 12,1)/       2.0844060D0/
      DATA GUESP1( 12,2)/      -2.5477670D0/
      DATA GUESP2( 12,2)/       4.3953700D0/
      DATA GUESP3( 12,2)/       2.0636740D0/
C                    DATA FOR ELEMENT 13        ALUMINUM
      DATA REFPM3 (13)/' Al: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 13)/     -24.8454040D0/
      DATA UPPPM3( 13)/     -22.2641590D0/
      DATA BETASP( 13)/      -0.5943010D0/
      DATA BETAPP( 13)/      -0.9565500D0/
      DATA ZSPM3 ( 13)/       1.7028880D0/
      DATA ZPPM3 ( 13)/       1.0736290D0/
      DATA ZDPM3 ( 13)/       1.0000000D0/
      DATA ALPPM3( 13)/       1.5217030D0/
      DATA EISOLP( 13)/     -46.8647630D0/
      DATA GSSPM3( 13)/       5.7767370D0/
      DATA GSPPM3( 13)/      11.6598560D0/
      DATA GPPPM3( 13)/       6.3477900D0/
      DATA GP2PM3( 13)/       6.1210770D0/
      DATA HSPPM3( 13)/       4.0062450D0/
      DATA DDPM3 ( 13)/       1.2102799D0/
      DATA QQPM3 ( 13)/       1.5585645D0/
      DATA AMPM3 ( 13)/       0.2123020D0/
      DATA ADPM3 ( 13)/       0.6418584D0/
      DATA AQPM3 ( 13)/       0.2262838D0/
      DATA GUESP1( 13,1)/      -0.4730900D0/
      DATA GUESP2( 13,1)/       1.9158250D0/
      DATA GUESP3( 13,1)/       1.4517280D0/
      DATA GUESP1( 13,2)/      -0.1540510D0/
      DATA GUESP2( 13,2)/       6.0050860D0/
      DATA GUESP3( 13,2)/       2.5199970D0/
C                    DATA FOR ELEMENT 14        SILICON
      DATA REFPM3 (14)/' Si: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 14)/     -26.7634830D0/
      DATA UPPPM3( 14)/     -22.8136350D0/
      DATA BETASP( 14)/      -2.8621450D0/
      DATA BETAPP( 14)/      -3.9331480D0/
      DATA ZSPM3 ( 14)/       1.6350750D0/
      DATA ZPPM3 ( 14)/       1.3130880D0/
      DATA ZDPM3 ( 14)/       1.0000000D0/
      DATA ALPPM3( 14)/       2.1358090D0/
      DATA EISOLP( 14)/     -67.7882140D0/
      DATA GSSPM3( 14)/       5.0471960D0/
      DATA GSPPM3( 14)/       5.9490570D0/
      DATA GPPPM3( 14)/       6.7593670D0/
      DATA GP2PM3( 14)/       5.1612970D0/
      DATA HSPPM3( 14)/       0.9198320D0/
      DATA DDPM3 ( 14)/       1.3144550D0/
      DATA QQPM3 ( 14)/       1.2743396D0/
      DATA AMPM3 ( 14)/       0.1854905D0/
      DATA ADPM3 ( 14)/       0.3060715D0/
      DATA AQPM3 ( 14)/       0.4877432D0/
      DATA GUESP1( 14,1)/      -0.3906000D0/
      DATA GUESP2( 14,1)/       6.0000540D0/
      DATA GUESP3( 14,1)/       0.6322620D0/
      DATA GUESP1( 14,2)/       0.0572590D0/
      DATA GUESP2( 14,2)/       6.0071830D0/
      DATA GUESP3( 14,2)/       2.0199870D0/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFPM3 (15)/'  P: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 15)/     -40.4130960D0/
      DATA UPPPM3( 15)/     -29.5930520D0/
      DATA BETASP( 15)/     -12.6158790D0/
      DATA BETAPP( 15)/      -4.1600400D0/
      DATA ZSPM3 ( 15)/       2.0175630D0/
      DATA ZPPM3 ( 15)/       1.5047320D0/
      DATA ZDPM3 ( 15)/       1.0000000D0/
      DATA ALPPM3( 15)/       1.9405340D0/
      DATA EISOLP( 15)/    -117.9591740D0/
      DATA GSSPM3( 15)/       7.8016150D0/
      DATA GSPPM3( 15)/       5.1869490D0/
      DATA GPPPM3( 15)/       6.6184780D0/
      DATA GP2PM3( 15)/       6.0620020D0/
      DATA HSPPM3( 15)/       1.5428090D0/
      DATA DDPM3 ( 15)/       1.0644947D0/
      DATA QQPM3 ( 15)/       1.1120386D0/
      DATA AMPM3 ( 15)/       0.2867187D0/
      DATA ADPM3 ( 15)/       0.4309446D0/
      DATA AQPM3 ( 15)/       0.3732517D0/
      DATA GUESP1( 15,1)/      -0.6114210D0/
      DATA GUESP2( 15,1)/       1.9972720D0/
      DATA GUESP3( 15,1)/       0.7946240D0/
      DATA GUESP1( 15,2)/      -0.0939350D0/
      DATA GUESP2( 15,2)/       1.9983600D0/
      DATA GUESP3( 15,2)/       1.9106770D0/
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFPM3 (16)/'  S: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 16)/     -49.8953710D0/
      DATA UPPPM3( 16)/     -44.3925830D0/
      DATA BETASP( 16)/      -8.8274650D0/
      DATA BETAPP( 16)/      -8.0914150D0/
      DATA ZSPM3 ( 16)/       1.8911850D0/
      DATA ZPPM3 ( 16)/       1.6589720D0/
      DATA ZDPM3 ( 16)/       1.0000000D0/
      DATA ALPPM3( 16)/       2.2697060D0/
      DATA EISOLP( 16)/    -183.4537395D0/
      DATA GSSPM3( 16)/       8.9646670D0/
      DATA GSPPM3( 16)/       6.7859360D0/
      DATA GPPPM3( 16)/       9.9681640D0/
      DATA GP2PM3( 16)/       7.9702470D0/
      DATA HSPPM3( 16)/       4.0418360D0/
      DATA DDPM3 ( 16)/       1.1214313D0/
      DATA QQPM3 ( 16)/       1.0086488D0/
      DATA AMPM3 ( 16)/       0.3294622D0/
      DATA ADPM3 ( 16)/       0.6679118D0/
      DATA AQPM3 ( 16)/       0.6137472D0/
      DATA GUESP1( 16,1)/      -0.3991910D0/
      DATA GUESP2( 16,1)/       6.0006690D0/
      DATA GUESP3( 16,1)/       0.9621230D0/
      DATA GUESP1( 16,2)/      -0.0548990D0/
      DATA GUESP2( 16,2)/       6.0018450D0/
      DATA GUESP3( 16,2)/       1.5799440D0/
C                    DATA FOR ELEMENT 17        CHLORINE
      DATA REFPM3 (17)/' Cl: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 17)/    -100.6267470D0/
      DATA UPPPM3( 17)/     -53.6143960D0/
      DATA BETASP( 17)/     -27.5285600D0/
      DATA BETAPP( 17)/     -11.5939220D0/
      DATA ZSPM3 ( 17)/       2.2462100D0/
      DATA ZPPM3 ( 17)/       2.1510100D0/
      DATA ZDPM3 ( 17)/       1.0000000D0/
      DATA ALPPM3( 17)/       2.5172960D0/
      DATA EISOLP( 17)/    -315.1949480D0/
      DATA GSSPM3( 17)/      16.0136010D0/
      DATA GSPPM3( 17)/       8.0481150D0/
      DATA GPPPM3( 17)/       7.5222150D0/
      DATA GP2PM3( 17)/       7.5041540D0/
      DATA HSPPM3( 17)/       3.4811530D0/
      DATA DDPM3 ( 17)/       0.9175856D0/
      DATA QQPM3 ( 17)/       0.7779230D0/
      DATA AMPM3 ( 17)/       0.5885190D0/
      DATA ADPM3 ( 17)/       0.6814522D0/
      DATA AQPM3 ( 17)/       0.3643694D0/
      DATA GUESP1( 17,1)/      -0.1715910D0/
      DATA GUESP2( 17,1)/       6.0008020D0/
      DATA GUESP3( 17,1)/       1.0875020D0/
      DATA GUESP1( 17,2)/      -0.0134580D0/
      DATA GUESP2( 17,2)/       1.9666180D0/
      DATA GUESP3( 17,2)/       2.2928910D0/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFPM3(30)/ ' Zn: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 30)/     -18.5321980D0/
      DATA UPPPM3( 30)/     -11.0474090D0/
      DATA BETASP( 30)/      -0.7155780D0/
      DATA BETAPP( 30)/      -6.3518640D0/
      DATA ZSPM3 ( 30)/       1.8199890D0/
      DATA ZPPM3 ( 30)/       1.5069220D0/
      DATA ZDPM3 ( 30)/       1.0000000D0/
      DATA ALPPM3( 30)/       1.3501260D0/
      DATA EISOLP( 30)/     -27.3872000D0/
      DATA GSSPM3( 30)/       9.6771960D0/
      DATA GSPPM3( 30)/       7.7362040D0/
      DATA GPPPM3( 30)/       4.9801740D0/
      DATA GP2PM3( 30)/       4.6696560D0/
      DATA HSPPM3( 30)/       0.6004130D0/
      DATA DDPM3 ( 30)/       1.5005758D0/
      DATA QQPM3 ( 30)/       1.4077174D0/
      DATA AMPM3 ( 30)/       0.3556485D0/
      DATA ADPM3 ( 30)/       0.2375689D0/
      DATA AQPM3 ( 30)/       0.2661069D0/
      DATA GUESP1( 30,1)/      -0.1112340D0/
      DATA GUESP2( 30,1)/       6.0014780D0/
      DATA GUESP3( 30,1)/       1.5160320D0/
      DATA GUESP1( 30,2)/      -0.1323700D0/
      DATA GUESP2( 30,2)/       1.9958390D0/
      DATA GUESP3( 30,2)/       2.5196420D0/
C                    DATA FOR ELEMENT 31        GALLIUM
      DATA REFPM3(31)/ ' Ga: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 31)/     -29.8555930D0/
      DATA UPPPM3( 31)/     -21.8753710D0/
      DATA BETASP( 31)/      -4.9456180D0/
      DATA BETAPP( 31)/      -0.4070530D0/
      DATA ZSPM3 ( 31)/       1.8470400D0/
      DATA ZPPM3 ( 31)/       0.8394110D0/
      DATA ALPPM3( 31)/       1.6051150D0/
      DATA EISOLP( 31)/     -57.3280250D0/
      DATA GSSPM3( 31)/       8.4585540D0/
      DATA GSPPM3( 31)/       8.9256190D0/
      DATA GPPPM3( 31)/       5.0868550D0/
      DATA GP2PM3( 31)/       4.9830450D0/
      DATA HSPPM3( 31)/       2.0512600D0/
      DATA DDPM3 ( 31)/       0.9776692D0/
      DATA QQPM3 ( 31)/       2.5271534D0/
      DATA AMPM3 ( 31)/       0.3108620D0/
      DATA ADPM3 ( 31)/       0.5129360D0/
      DATA AQPM3 ( 31)/       0.1546208D0/
      DATA GUESP1( 31,1)/      -0.5601790D0/
      DATA GUESP2( 31,1)/       5.6232730D0/
      DATA GUESP3( 31,1)/       1.5317800D0/
      DATA GUESP1( 31,2)/      -0.2727310D0/
      DATA GUESP2( 31,2)/       1.9918430D0/
      DATA GUESP3( 31,2)/       2.1838640D0/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFPM3(32)/ ' Ge: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 32)/     -35.4671955D0/
      DATA UPPPM3( 32)/     -31.5863583D0/
      DATA BETASP( 32)/      -5.3250024D0/
      DATA BETAPP( 32)/      -2.2501567D0/
      DATA ZSPM3 ( 32)/       2.2373526D0/
      DATA ZPPM3 ( 32)/       1.5924319D0/
      DATA ALPPM3( 32)/       1.9723370D0/
      DATA EISOLP( 32)/     -84.0156006D0/
      DATA GSSPM3( 32)/       5.3769635D0/
      DATA GSPPM3( 32)/      10.2095293D0/
      DATA GPPPM3( 32)/       7.6718647D0/
      DATA GP2PM3( 32)/       6.9242663D0/
      DATA HSPPM3( 32)/       1.3370204D0/
      DATA DDPM3 ( 32)/       1.1920304D0/
      DATA QQPM3 ( 32)/       1.3321263D0/
      DATA AMPM3 ( 32)/       0.1976098D0/
      DATA ADPM3 ( 32)/       0.3798182D0/
      DATA AQPM3 ( 32)/       0.3620669D0/
      DATA GUESP1( 32,1)/       0.9631726D0/
      DATA GUESP2( 32,1)/       6.0120134D0/
      DATA GUESP3( 32,1)/       2.1633655D0/
      DATA GUESP1( 32,2)/      -0.9593891D0/
      DATA GUESP2( 32,2)/       5.7491802D0/
      DATA GUESP3( 32,2)/       2.1693724D0/
C                    DATA FOR ELEMENT 33        ARSENIC
      DATA REFPM3(33)/ ' As: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 33)/     -38.5074240D0/
      DATA UPPPM3( 33)/     -35.1524150D0/
      DATA BETASP( 33)/      -8.2321650D0/
      DATA BETAPP( 33)/      -5.0173860D0/
      DATA ZSPM3 ( 33)/       2.6361770D0/
      DATA ZPPM3 ( 33)/       1.7038890D0/
      DATA ALPPM3( 33)/       1.7944770D0/
      DATA EISOLP( 33)/    -122.6326140D0/
      DATA GSSPM3( 33)/       8.7890010D0/
      DATA GSPPM3( 33)/       5.3979830D0/
      DATA GPPPM3( 33)/       8.2872500D0/
      DATA GP2PM3( 33)/       8.2103460D0/
      DATA HSPPM3( 33)/       1.9510340D0/
      DATA DDPM3 ( 33)/       0.9679655D0/
      DATA QQPM3 ( 33)/       1.2449874D0/
      DATA AMPM3 ( 33)/       0.3230063D0/
      DATA ADPM3 ( 33)/       0.5042239D0/
      DATA AQPM3 ( 33)/       0.2574219D0/
      DATA GUESP1( 33,1)/      -0.4600950D0/
      DATA GUESP2( 33,1)/       1.9831150D0/
      DATA GUESP3( 33,1)/       1.0867930D0/
      DATA GUESP1( 33,2)/      -0.0889960D0/
      DATA GUESP2( 33,2)/       1.9929440D0/
      DATA GUESP3( 33,2)/       2.1400580D0/
C                    DATA FOR ELEMENT 34        SELENIUM
      DATA REFPM3(34)/ ' Se: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 34)/     -55.3781350D0/
      DATA UPPPM3( 34)/     -49.8230760D0/
      DATA BETASP( 34)/      -6.1578220D0/
      DATA BETAPP( 34)/      -5.4930390D0/
      DATA ZSPM3 ( 34)/       2.8280510D0/
      DATA ZPPM3 ( 34)/       1.7325360D0/
      DATA ALPPM3( 34)/       3.0439570D0/
      DATA EISOLP( 34)/    -192.7748115D0/
      DATA GSSPM3( 34)/       7.4325910D0/
      DATA GSPPM3( 34)/      10.0604610D0/
      DATA GPPPM3( 34)/       9.5683260D0/
      DATA GP2PM3( 34)/       7.7242890D0/
      DATA HSPPM3( 34)/       4.0165580D0/
      DATA DDPM3 ( 34)/       0.8719813D0/
      DATA QQPM3 ( 34)/       1.2244019D0/
      DATA AMPM3 ( 34)/       0.2731566D0/
      DATA ADPM3 ( 34)/       0.7509697D0/
      DATA AQPM3 ( 34)/       0.5283737D0/
      DATA GUESP1( 34,1)/       0.0478730D0/
      DATA GUESP2( 34,1)/       6.0074000D0/
      DATA GUESP3( 34,1)/       2.0817170D0/
      DATA GUESP1( 34,2)/       0.1147200D0/
      DATA GUESP2( 34,2)/       6.0086720D0/
      DATA GUESP3( 34,2)/       1.5164230D0/
C                    DATA FOR ELEMENT 35        BROMINE
      DATA REFPM3 (35)/' Br: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 35)/    -116.6193110D0/
      DATA UPPPM3( 35)/     -74.2271290D0/
      DATA BETASP( 35)/     -31.1713420D0/
      DATA BETAPP( 35)/      -6.8140130D0/
      DATA ZSPM3 ( 35)/       5.3484570D0/
      DATA ZPPM3 ( 35)/       2.1275900D0/
      DATA ZDPM3 ( 35)/       1.0000000D0/
      DATA ALPPM3( 35)/       2.5118420D0/
      DATA EISOLP( 35)/    -352.5398970D0/
      DATA GSSPM3( 35)/      15.9434250D0/
      DATA GSPPM3( 35)/      16.0616800D0/
      DATA GPPPM3( 35)/       8.2827630D0/
      DATA GP2PM3( 35)/       7.8168490D0/
      DATA HSPPM3( 35)/       0.5788690D0/
      DATA DDPM3 ( 35)/       0.2759025D0/
      DATA QQPM3 ( 35)/       0.9970532D0/
      DATA AMPM3 ( 35)/       0.5859399D0/
      DATA ADPM3 ( 35)/       0.6755383D0/
      DATA AQPM3 ( 35)/       0.3823719D0/
      DATA GUESP1( 35,1)/       0.9604580D0/
      DATA GUESP2( 35,1)/       5.9765080D0/
      DATA GUESP3( 35,1)/       2.3216540D0/
      DATA GUESP1( 35,2)/      -0.9549160D0/
      DATA GUESP2( 35,2)/       5.9447030D0/
      DATA GUESP3( 35,2)/       2.3281420D0/
C                    DATA FOR ELEMENT 48        CADMIUM
      DATA REFPM3(48)/ ' Cd: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 48)/     -15.8285840D0/
      DATA UPPPM3( 48)/       8.7497950D0/
      DATA BETASP( 48)/      -8.5819440D0/
      DATA BETAPP( 48)/      -0.6010340D0/
      DATA ZSPM3 ( 48)/       1.6793510D0/
      DATA ZPPM3 ( 48)/       2.0664120D0/
      DATA ALPPM3( 48)/       1.5253820D0/
      DATA EISOLP( 48)/     -22.4502080D0/
      DATA GSSPM3( 48)/       9.2069600D0/
      DATA GSPPM3( 48)/       8.2315390D0/
      DATA GPPPM3( 48)/       4.9481040D0/
      DATA GP2PM3( 48)/       4.6696560D0/
      DATA HSPPM3( 48)/       1.6562340D0/
      DATA DDPM3 ( 48)/       1.5982681D0/
      DATA QQPM3 ( 48)/       1.2432402D0/
      DATA AMPM3 ( 48)/       0.3383668D0/
      DATA ADPM3 ( 48)/       0.3570290D0/
      DATA AQPM3 ( 48)/       0.2820582D0/
C                    DATA FOR ELEMENT 49        INDIUM
      DATA REFPM3(49)/ ' In: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 49)/     -26.1762050D0/
      DATA UPPPM3( 49)/     -20.0058220D0/
      DATA BETASP( 49)/      -2.9933190D0/
      DATA BETAPP( 49)/      -1.8289080D0/
      DATA ZSPM3 ( 49)/       2.0161160D0/
      DATA ZPPM3 ( 49)/       1.4453500D0/
      DATA ALPPM3( 49)/       1.4183850D0/
      DATA EISOLP( 49)/     -51.9750470D0/
      DATA GSSPM3( 49)/       6.5549000D0/
      DATA GSPPM3( 49)/       8.2298730D0/
      DATA GPPPM3( 49)/       6.2992690D0/
      DATA GP2PM3( 49)/       4.9842110D0/
      DATA HSPPM3( 49)/       2.6314610D0/
      DATA DDPM3 ( 49)/       1.5766241D0/
      DATA QQPM3 ( 49)/       1.7774563D0/
      DATA AMPM3 ( 49)/       0.2409004D0/
      DATA ADPM3 ( 49)/       0.4532655D0/
      DATA AQPM3 ( 49)/       0.3689812D0/
      DATA GUESP1( 49,1)/      -0.3431380D0/
      DATA GUESP2( 49,1)/       1.9940340D0/
      DATA GUESP3( 49,1)/       1.6255160D0/
      DATA GUESP1( 49,2)/      -0.1095320D0/
      DATA GUESP2( 49,2)/       5.6832170D0/
      DATA GUESP3( 49,2)/       2.8670090D0/
C                    DATA FOR ELEMENT 50        TIN
      DATA REFPM3(50)/ ' Sn: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 50)/     -34.5501920D0/
      DATA UPPPM3( 50)/     -25.8944190D0/
      DATA BETASP( 50)/      -2.7858020D0/
      DATA BETAPP( 50)/      -2.0059990D0/
      DATA ZSPM3 ( 50)/       2.3733280D0/
      DATA ZPPM3 ( 50)/       1.6382330D0/
      DATA ALPPM3( 50)/       1.6996500D0/
      DATA EISOLP( 50)/     -78.8877790D0/
      DATA GSSPM3( 50)/      10.1900330D0/
      DATA GSPPM3( 50)/       7.2353270D0/
      DATA GPPPM3( 50)/       5.6738100D0/
      DATA GP2PM3( 50)/       5.1822140D0/
      DATA HSPPM3( 50)/       1.0331570D0/
      DATA DDPM3 ( 50)/       1.3120038D0/
      DATA QQPM3 ( 50)/       1.5681814D0/
      DATA AMPM3 ( 50)/       0.3744959D0/
      DATA ADPM3 ( 50)/       0.3218163D0/
      DATA AQPM3 ( 50)/       0.2832529D0/
      DATA GUESP1( 50,1)/      -0.1503530D0/
      DATA GUESP2( 50,1)/       6.0056940D0/
      DATA GUESP3( 50,1)/       1.7046420D0/
      DATA GUESP1( 50,2)/      -0.0444170D0/
      DATA GUESP2( 50,2)/       2.2573810D0/
      DATA GUESP3( 50,2)/       2.4698690D0/
C                    DATA FOR ELEMENT 51        ANTIMONY
      DATA REFPM3(51)/ ' Sb: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 51)/     -56.4321960D0/
      DATA UPPPM3( 51)/     -29.4349540D0/
      DATA BETASP( 51)/     -14.7942170D0/
      DATA BETAPP( 51)/      -2.8179480D0/
      DATA ZSPM3 ( 51)/       2.3430390D0/
      DATA ZPPM3 ( 51)/       1.8999920D0/
      DATA ALPPM3( 51)/       2.0343010D0/
      DATA EISOLP( 51)/    -148.9382890D0/
      DATA GSSPM3( 51)/       9.2382770D0/
      DATA GSPPM3( 51)/       5.2776800D0/
      DATA GPPPM3( 51)/       6.3500000D0/
      DATA GP2PM3( 51)/       6.2500000D0/
      DATA HSPPM3( 51)/       2.4244640D0/
      DATA DDPM3 ( 51)/       1.4091903D0/
      DATA QQPM3 ( 51)/       1.3521354D0/
      DATA AMPM3 ( 51)/       0.3395177D0/
      DATA ADPM3 ( 51)/       0.4589010D0/
      DATA AQPM3 ( 51)/       0.2423472D0/
      DATA GUESP1( 51,1)/       3.0020280D0/
      DATA GUESP2( 51,1)/       6.0053420D0/
      DATA GUESP3( 51,1)/       0.8530600D0/
      DATA GUESP1( 51,2)/      -0.0188920D0/
      DATA GUESP2( 51,2)/       6.0114780D0/
      DATA GUESP3( 51,2)/       2.7933110D0/
C                    DATA FOR ELEMENT 52        TELLURIUM
      DATA REFPM3(52)/ ' Te: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 52)/     -44.9380360D0/
      DATA UPPPM3( 52)/     -46.3140990D0/
      DATA BETASP( 52)/      -2.6651460D0/
      DATA BETAPP( 52)/      -3.8954300D0/
      DATA ZSPM3 ( 52)/       4.1654920D0/
      DATA ZPPM3 ( 52)/       1.6475550D0/
      DATA ALPPM3( 52)/       2.4850190D0/
      DATA EISOLP( 52)/    -168.0945925D0/
      DATA GSSPM3( 52)/      10.2550730D0/
      DATA GSPPM3( 52)/       8.1691450D0/
      DATA GPPPM3( 52)/       7.7775920D0/
      DATA GP2PM3( 52)/       7.7551210D0/
      DATA HSPPM3( 52)/       3.7724620D0/
      DATA DDPM3 ( 52)/       0.3484177D0/
      DATA QQPM3 ( 52)/       1.5593085D0/
      DATA AMPM3 ( 52)/       0.3768862D0/
      DATA ADPM3 ( 52)/       1.1960743D0/
      DATA AQPM3 ( 52)/       0.2184786D0/
      DATA GUESP1( 52,1)/       0.0333910D0/
      DATA GUESP2( 52,1)/       5.9563790D0/
      DATA GUESP3( 52,1)/       2.2775750D0/
      DATA GUESP1( 52,2)/      -1.9218670D0/
      DATA GUESP2( 52,2)/       4.9732190D0/
      DATA GUESP3( 52,2)/       0.5242430D0/
C                    DATA FOR ELEMENT 53        IODINE
      DATA REFPM3 (53)/'  I: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 53)/     -96.4540370D0/
      DATA UPPPM3( 53)/     -61.0915820D0/
      DATA BETASP( 53)/     -14.4942340D0/
      DATA BETAPP( 53)/      -5.8947030D0/
      DATA ZSPM3 ( 53)/       7.0010130D0/
      DATA ZPPM3 ( 53)/       2.4543540D0/
      DATA ZDPM3 ( 53)/       1.0000000D0/
      DATA ALPPM3( 53)/       1.9901850D0/
      DATA EISOLP( 53)/    -288.3160860D0/
      DATA GSSPM3( 53)/      13.6319430D0/
      DATA GSPPM3( 53)/      14.9904060D0/
      DATA GPPPM3( 53)/       7.2883300D0/
      DATA GP2PM3( 53)/       5.9664070D0/
      DATA HSPPM3( 53)/       2.6300350D0/
      DATA DDPM3 ( 53)/       0.1581469D0/
      DATA QQPM3 ( 53)/       1.0467302D0/
      DATA AMPM3 ( 53)/       0.5009902D0/
      DATA ADPM3 ( 53)/       1.6699104D0/
      DATA AQPM3 ( 53)/       0.5153082D0/
      DATA GUESP1( 53,1)/      -0.1314810D0/
      DATA GUESP2( 53,1)/       5.2064170D0/
      DATA GUESP3( 53,1)/       1.7488240D0/
      DATA GUESP1( 53,2)/      -0.0368970D0/
      DATA GUESP2( 53,2)/       6.0101170D0/
      DATA GUESP3( 53,2)/       2.7103730D0/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFPM3(80)/ ' Hg: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 80)/     -17.7622290D0/
      DATA UPPPM3( 80)/     -18.3307510D0/
      DATA BETASP( 80)/      -3.1013650D0/
      DATA BETAPP( 80)/      -3.4640310D0/
      DATA ZSPM3 ( 80)/       1.4768850D0/
      DATA ZPPM3 ( 80)/       2.4799510D0/
      DATA ALPPM3( 80)/       1.5293770D0/
      DATA EISOLP( 80)/     -28.8997380D0/
      DATA GSSPM3( 80)/       6.6247200D0/
      DATA GSPPM3( 80)/      10.6392970D0/
      DATA GPPPM3( 80)/      14.7092830D0/
      DATA GP2PM3( 80)/      16.0007400D0/
      DATA HSPPM3( 80)/       2.0363110D0/
      DATA DDPM3 ( 80)/       1.2317811D0/
      DATA QQPM3 ( 80)/       1.2164033D0/
      DATA AMPM3 ( 80)/       0.2434664D0/
      DATA ADPM3 ( 80)/       0.4515472D0/
      DATA AQPM3 ( 80)/       0.2618394D0/
      DATA GUESP1( 80,1)/       1.0827200D0/
      DATA GUESP2( 80,1)/       6.4965980D0/
      DATA GUESP3( 80,1)/       1.1951460D0/
      DATA GUESP1( 80,2)/      -0.0965530D0/
      DATA GUESP2( 80,2)/       3.9262810D0/
      DATA GUESP3( 80,2)/       2.6271600D0/
C                    DATA FOR ELEMENT 81        THALLIUM
      DATA REFPM3(81)/ ' Tl: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 81)/     -30.0531700D0/
      DATA UPPPM3( 81)/     -26.9206370D0/
      DATA BETASP( 81)/      -1.0844950D0/
      DATA BETAPP( 81)/      -7.9467990D0/
      DATA ZSPM3 ( 81)/       6.8679210D0/
      DATA ZPPM3 ( 81)/       1.9694450D0/
      DATA ALPPM3( 81)/       1.3409510D0/
      DATA EISOLP( 81)/     -56.6492050D0/
      DATA GSSPM3( 81)/      10.4604120D0/
      DATA GSPPM3( 81)/      11.2238830D0/
      DATA GPPPM3( 81)/       4.9927850D0/
      DATA GP2PM3( 81)/       8.9627270D0/
      DATA HSPPM3( 81)/       2.5304060D0/
      DATA DDPM3 ( 81)/       0.0781362D0/
      DATA QQPM3 ( 81)/       1.5317110D0/
      DATA AMPM3 ( 81)/       0.3844326D0/
      DATA ADPM3 ( 81)/       2.5741815D0/
      DATA AQPM3 ( 81)/       0.2213264D0/
      DATA GUESP1( 81,1)/      -1.3613990D0/
      DATA GUESP2( 81,1)/       3.5572260D0/
      DATA GUESP3( 81,1)/       1.0928020D0/
      DATA GUESP1( 81,2)/      -0.0454010D0/
      DATA GUESP2( 81,2)/       2.3069950D0/
      DATA GUESP3( 81,2)/       2.9650290D0/
C                    DATA FOR ELEMENT 82        LEAD
      DATA REFPM3(82)/ ' Pb: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 82)/     -30.3227560D0/
      DATA UPPPM3( 82)/     -24.4258340D0/
      DATA BETASP( 82)/      -6.1260240D0/
      DATA BETAPP( 82)/      -1.3954300D0/
      DATA ZSPM3 ( 82)/       3.1412890D0/
      DATA ZPPM3 ( 82)/       1.8924180D0/
      DATA ALPPM3( 82)/       1.6200450D0/
      DATA EISOLP( 82)/     -73.4660775D0/
      DATA GSSPM3( 82)/       7.0119920D0/
      DATA GSPPM3( 82)/       6.7937820D0/
      DATA GPPPM3( 82)/       5.1837800D0/
      DATA GP2PM3( 82)/       5.0456510D0/
      DATA HSPPM3( 82)/       1.5663020D0/
      DATA DDPM3 ( 82)/       0.9866290D0/
      DATA QQPM3 ( 82)/       1.5940562D0/
      DATA AMPM3 ( 82)/       0.2576991D0/
      DATA ADPM3 ( 82)/       0.4527678D0/
      DATA AQPM3 ( 82)/       0.2150175D0/
      DATA GUESP1( 82,1)/      -0.1225760D0/
      DATA GUESP2( 82,1)/       6.0030620D0/
      DATA GUESP3( 82,1)/       1.9015970D0/
      DATA GUESP1( 82,2)/      -0.0566480D0/
      DATA GUESP2( 82,2)/       4.7437050D0/
      DATA GUESP3( 82,2)/       2.8618790D0/
C                    DATA FOR ELEMENT 83        BISMUTH
      DATA REFPM3(83)/ ' Bi: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 83)/     -33.4959380D0/
      DATA UPPPM3( 83)/     -35.5210260D0/
      DATA BETASP( 83)/      -5.6072830D0/
      DATA BETAPP( 83)/      -5.8001520D0/
      DATA ZSPM3 ( 83)/       4.9164510D0/
      DATA ZPPM3 ( 83)/       1.9349350D0/
      DATA ALPPM3( 83)/       1.8574310D0/
      DATA EISOLP( 83)/    -109.2774910D0/
      DATA GSSPM3( 83)/       4.9894800D0/
      DATA GSPPM3( 83)/       6.1033080D0/
      DATA GPPPM3( 83)/       8.6960070D0/
      DATA GP2PM3( 83)/       8.3354470D0/
      DATA HSPPM3( 83)/       0.5991220D0/
      DATA DDPM3 ( 83)/       0.2798609D0/
      DATA QQPM3 ( 83)/       1.5590294D0/
      DATA AMPM3 ( 83)/       0.1833693D0/
      DATA ADPM3 ( 83)/       0.6776013D0/
      DATA AQPM3 ( 83)/       0.2586520D0/
      DATA GUESP1( 83,1)/       2.5816930D0/
      DATA GUESP2( 83,1)/       5.0940220D0/
      DATA GUESP3( 83,1)/       0.4997870D0/
      DATA GUESP1( 83,2)/       0.0603200D0/
      DATA GUESP2( 83,2)/       6.0015380D0/
      DATA GUESP3( 83,2)/       2.4279700D0/
C                    DATA FOR ELEMENT  103      CAPPED BOND
      DATA REFPM3(102)/' Cb: (PM3):  Capped Bond  (Hydrogen-like, takes
     1on a  zero charge.)             '/
      DATA USSPM3(102)/     -11.9062760D0/
      DATA BETASP(102)/-9999999.0000000D0/
      DATA ZSPM3 (102)/       4.0000000D0/
      DATA ZPPM3 (102)/       0.3000000D0/
      DATA ZDPM3 (102)/       0.3000000D0/
      DATA ALPPM3(102)/       2.5441341D0/
      DATA EISOLP(102)/       4.0000000D0/
      DATA GSSPM3(102)/      12.8480000D0/
      DATA HSPPM3(102)/       0.1000000D0/
      DATA DDPM3 (102)/       0.0684105D0/
      DATA QQPM3 (102)/       1.0540926D0/
      DATA AMPM3 (102)/       0.4721793D0/
      DATA ADPM3 (102)/       0.9262742D0/
      DATA AQPM3 (102)/       0.2909059D0/
      END
