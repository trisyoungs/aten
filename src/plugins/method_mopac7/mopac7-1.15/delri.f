      SUBROUTINE DELRI(DG,NI,NJ,RR,DEL1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DG(22)
************************************************************************
*                                                                      *
*    ON INPUT NI = ATOMIC NUMBER OF FIRST ATOM                         *
*             NJ = ATOMIC NUMBER OF SECOND ATOM                        *
*             RR = INTERATOMIC DISTANCE IN BOHRS                       *
*                                                                      *
************************************************************************
      COMMON  /MULTIP/  DD(107),QQ(107),BDD(107,3)
      COMMON /TWOEL3/ F03(107)
      COMMON /NATORB/ NATORB(107)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      CHARACTER*241 KEYWRD
      DATA ICALCN/0/
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
      ENDIF
      A0=0.529167D0
      TERM=(27.21D0*DEL1)/(RR*A0*A0)
      DA=DD(NI)
      DB=DD(NJ)
      QA=QQ(NI)
      QB=QQ(NJ)
C   HYDROGEN-HYDROGEN
      AEE=0.25D0*(1.0D0/BDD(NI,1)+1.0D0/BDD(NJ,1))**2
      EE    =-RR/(SQRT(RR**2+AEE))**3
      DG(1)=TERM*EE
      IF(NATORB(NI).LE.2.AND.NATORB(NJ).LE.2) RETURN
      IF(NATORB(NI).LE.2) GO TO 10
C   HEAVY ATOM-HYDROGEN
      ADE=0.25D0*(1.0D0/BDD(NI,2)+1.0D0/BDD(NJ,1))**2
      AQE=0.25D0*(1.0D0/BDD(NI,3)+1.0D0/BDD(NJ,1))**2
      DZE   = (RR+DA)/(SQRT((RR+DA)**2+ADE))**3
     1       -(RR-DA)/(SQRT((RR-DA)**2+ADE))**3
      QZZE  =-(RR+2.0D0*QA)/(SQRT((RR+2.0D0*QA)**2+AQE))**3
     1       -(RR-2.0D0*QA)/(SQRT((RR-2.0D0*QA)**2+AQE))**3
     2       +(2.0D0*RR)/(SQRT(RR**2+AQE))**3
      QXXE  =-(2.0D0*RR)/(SQRT(RR**2+4.0D0*QA**2+AQE))**3
     1       +(2.0D0*RR)/(SQRT(RR**2+AQE))**3
      DG(2)=-(TERM*DZE)/2.0D0
      DG(3)=TERM*(EE+QZZE/4.0D0)
      DG(4)=TERM*(EE+QXXE/4.0D0)
      IF(NATORB(NJ).LE.2) RETURN
C   HYDROGEN-HEAVY ATOM
   10 AED=0.25D0*(1.0D0/BDD(NI,1)+1.0D0/BDD(NJ,2))**2
      AEQ=0.25D0*(1.0D0/BDD(NI,1)+1.0D0/BDD(NJ,3))**2
      EDZ   = (RR-DB)/(SQRT((RR-DB)**2+AED))**3
     1       -(RR+DB)/(SQRT((RR+DB)**2+AED))**3
      EQZZ  =-(RR-2.0D0*QB)/(SQRT((RR-2.0D0*QB)**2+AEQ))**3
     1       -(RR+2.0D0*QB)/(SQRT((RR+2.0D0*QB)**2+AEQ))**3
     2       +(2.0D0*RR)/(SQRT(RR**2+AEQ))**3
      EQXX  =-(2.0D0*RR)/(SQRT(RR**2+4.0D0*QB**2+AEQ))**3
     1       +(2.0D0*RR)/(SQRT(RR**2+AEQ))**3
      DG(5)=-(TERM*EDZ)/2.0D0
      DG(11)=TERM*(EE+EQZZ/4.0D0)
      DG(12)=TERM*(EE+EQXX/4.0D0)
      IF(NATORB(NI).LE.2) RETURN
C   HEAVY ATOM-HEAVY ATOM
      ADD=0.25D0*(1.D0/BDD(NI,2)+1.D0/BDD(NJ,2))**2
      ADQ=0.25D0*(1.D0/BDD(NI,2)+1.D0/BDD(NJ,3))**2
      AQD=0.25D0*(1.D0/BDD(NI,3)+1.D0/BDD(NJ,2))**2
      AQQ=0.25D0*(1.D0/BDD(NI,3)+1.D0/BDD(NJ,3))**2
      DXDX  =-(2.D0*RR)/(SQRT(RR**2+(DA-DB)**2+ADD))**3
     1       +(2.D0*RR)/(SQRT(RR**2+(DA+DB)**2+ADD))**3
      DZDZ  =-(RR+DA-DB)/(SQRT((RR+DA-DB)**2+ADD))**3
     1       -(RR-DA+DB)/(SQRT((RR-DA+DB)**2+ADD))**3
     2       +(RR-DA-DB)/(SQRT((RR-DA-DB)**2+ADD))**3
     3       +(RR+DA+DB)/(SQRT((RR+DA+DB)**2+ADD))**3
      DZQXX = 2.D0*(RR+DA)/(SQRT((RR+DA)**2+4.D0*QB**2+ADQ))**3
     1       -2.D0*(RR-DA)/(SQRT((RR-DA)**2+4.D0*QB**2+ADQ))**3
     2       -2.D0*(RR+DA)/(SQRT((RR+DA)**2+ADQ))**3
     3       +2.D0*(RR-DA)/(SQRT((RR-DA)**2+ADQ))**3
      QXXDZ = 2.D0*(RR-DB)/(SQRT((RR-DB)**2+4.D0*QA**2+AQD))**3
     1       -2.D0*(RR+DB)/(SQRT((RR+DB)**2+4.D0*QA**2+AQD))**3
     2       -2.D0*(RR-DB)/(SQRT((RR-DB)**2+AQD))**3
     3       +2.D0*(RR+DB)/(SQRT((RR+DB)**2+AQD))**3
      DZQZZ = (RR+DA-2.D0*QB)/(SQRT((RR+DA-2.D0*QB)**2+ADQ))**3
     1       -(RR-DA-2.D0*QB)/(SQRT((RR-DA-2.D0*QB)**2+ADQ))**3
     2       +(RR+DA+2.D0*QB)/(SQRT((RR+DA+2.D0*QB)**2+ADQ))**3
     3       -(RR-DA+2.D0*QB)/(SQRT((RR-DA+2.D0*QB)**2+ADQ))**3
     4       +2.D0*(RR-DA)/(SQRT((RR-DA)**2+ADQ))**3
     5       -2.D0*(RR+DA)/(SQRT((RR+DA)**2+ADQ))**3
      QZZDZ = (RR+2.D0*QA-DB)/(SQRT((RR+2.D0*QA-DB)**2+AQD))**3
     1       -(RR+2.D0*QA+DB)/(SQRT((RR+2.D0*QA+DB)**2+AQD))**3
     2       +(RR-2.D0*QA-DB)/(SQRT((RR-2.D0*QA-DB)**2+AQD))**3
     3       -(RR-2.D0*QA+DB)/(SQRT((RR-2.D0*QA+DB)**2+AQD))**3
     4       -2.D0*(RR-DB)/(SQRT((RR-DB)**2+AQD))**3
     5       +2.D0*(RR+DB)/(SQRT((RR+DB)**2+AQD))**3
      QXXQXX=-(2.D0*RR)/(SQRT(RR**2+4.D0*(QA-QB)**2+AQQ))**3
     1       -(2.D0*RR)/(SQRT(RR**2+4.D0*(QA+QB)**2+AQQ))**3
     2       +(4.D0*RR)/(SQRT(RR**2+4.D0*QA**2+AQQ))**3
     3       +(4.D0*RR)/(SQRT(RR**2+4.D0*QB**2+AQQ))**3
     4       -(4.D0*RR)/(SQRT(RR**2+AQQ))**3
      QXXQYY=-(4.D0*RR)/(SQRT(RR**2+4.D0*QA**2+4.D0*QB**2+AQQ))**3
     1       +(4.D0*RR)/(SQRT(RR**2+4.D0*QA**2+AQQ))**3
     2       +(4.D0*RR)/(SQRT(RR**2+4.D0*QB**2+AQQ))**3
     3       -(4.D0*RR)/(SQRT(RR**2+AQQ))**3
      QXXQZZ=
     1     -2.D0*(RR-2.D0*QB)/(SQRT((RR-2.D0*QB)**2+4.D0*QA**2+AQQ))**3
     2     -2.D0*(RR+2.D0*QB)/(SQRT((RR+2.D0*QB)**2+4.D0*QA**2+AQQ))**3
     3       +2.D0*(RR-2.D0*QB)/(SQRT((RR-2.D0*QB)**2+AQQ))**3
     4       +2.D0*(RR+2.D0*QB)/(SQRT((RR+2.D0*QB)**2+AQQ))**3
     5       +(4.D0*RR)/(SQRT(RR**2+4.D0*QA**2+AQQ))**3
     6       -(4.D0*RR)/(SQRT(RR**2+AQQ))**3
      QZZQXX=
     1     -2.D0*(RR+2.D0*QA)/(SQRT((RR+2.D0*QA)**2+4.D0*QB**2+AQQ))**3
     2     -2.D0*(RR-2.D0*QA)/(SQRT((RR-2.D0*QA)**2+4.D0*QB**2+AQQ))**3
     3       +2.D0*(RR+2.D0*QA)/(SQRT((RR+2.D0*QA)**2+AQQ))**3
     4       +2.D0*(RR-2.D0*QA)/(SQRT((RR-2.D0*QA)**2+AQQ))**3
     5       +(4.D0*RR)/(SQRT(RR**2+4.D0*QB**2+AQQ))**3
     6       -(4.D0*RR)/(SQRT(RR**2+AQQ))**3
      QZZQZZ=
     1     -(RR+2.D0*QA-2.D0*QB)/(SQRT((RR+2.D0*QA-2.D0*QB)**2+AQQ))**3
     2     -(RR+2.D0*QA+2.D0*QB)/(SQRT((RR+2.D0*QA+2.D0*QB)**2+AQQ))**3
     3     -(RR-2.D0*QA-2.D0*QB)/(SQRT((RR-2.D0*QA-2.D0*QB)**2+AQQ))**3
     4     -(RR-2.D0*QA+2.D0*QB)/(SQRT((RR-2.D0*QA+2.D0*QB)**2+AQQ))**3
     5       +2.D0*(RR-2.D0*QA)/(SQRT((RR-2.D0*QA)**2+AQQ))**3
     6       +2.D0*(RR+2.D0*QA)/(SQRT((RR+2.D0*QA)**2+AQQ))**3
     7       +2.D0*(RR-2.D0*QB)/(SQRT((RR-2.D0*QB)**2+AQQ))**3
     8       +2.D0*(RR+2.D0*QB)/(SQRT((RR+2.D0*QB)**2+AQQ))**3
     9       -(4.D0*RR)/(SQRT(RR**2+AQQ))**3
      DXQXZ = 2.D0*(RR-QB)/(SQRT((RR-QB)**2+(DA-QB)**2+ADQ))**3
     1       -2.D0*(RR+QB)/(SQRT((RR+QB)**2+(DA-QB)**2+ADQ))**3
     2       -2.D0*(RR-QB)/(SQRT((RR-QB)**2+(DA+QB)**2+ADQ))**3
     3       +2.D0*(RR+QB)/(SQRT((RR+QB)**2+(DA+QB)**2+ADQ))**3
      QXZDX = 2.D0*(RR+QA)/(SQRT((RR+QA)**2+(QA-DB)**2+AQD))**3
     1       -2.D0*(RR-QA)/(SQRT((RR-QA)**2+(QA-DB)**2+AQD))**3
     2       -2.D0*(RR+QA)/(SQRT((RR+QA)**2+(QA+DB)**2+AQD))**3
     3       +2.D0*(RR-QA)/(SQRT((RR-QA)**2+(QA+DB)**2+AQD))**3
      QXZQXZ=-2.D0*(RR+QA-QB)/(SQRT((RR+QA-QB)**2+(QA-QB)**2+AQQ))**3
     1       +2.D0*(RR+QA+QB)/(SQRT((RR+QA+QB)**2+(QA-QB)**2+AQQ))**3
     2       +2.D0*(RR-QA-QB)/(SQRT((RR-QA-QB)**2+(QA-QB)**2+AQQ))**3
     3       -2.D0*(RR-QA+QB)/(SQRT((RR-QA+QB)**2+(QA-QB)**2+AQQ))**3
     4       +2.D0*(RR+QA-QB)/(SQRT((RR+QA-QB)**2+(QA+QB)**2+AQQ))**3
     5       -2.D0*(RR+QA+QB)/(SQRT((RR+QA+QB)**2+(QA+QB)**2+AQQ))**3
     6       -2.D0*(RR-QA-QB)/(SQRT((RR-QA-QB)**2+(QA+QB)**2+AQQ))**3
     7       +2.D0*(RR-QA+QB)/(SQRT((RR-QA+QB)**2+(QA+QB)**2+AQQ))**3
      DG(6)=(TERM*DZDZ)/4.0D0
      DG(7)=(TERM*DXDX)/4.0D0
      DG(8)=-TERM*(EDZ/2.0D0+QZZDZ/8.0D0)
      DG(9)=-TERM*(EDZ/2.0D0+QXXDZ/8.0D0)
      DG(10)=-(TERM*QXZDX)/8.0D0
      DG(13)=-TERM*(DZE/2.0D0+DZQZZ/8.0D0)
      DG(14)=-TERM*(DZE/2.0D0+DZQXX/8.0D0)
      DG(15)=-(TERM*DXQXZ)/8.0D0
      DG(16)=TERM*(EE+EQZZ/4.0D0+QZZE/4.0D0+QZZQZZ/16.0D0)
      DG(17)=TERM*(EE+EQZZ/4.0D0+QXXE/4.0D0+QXXQZZ/16.0D0)
      DG(18)=TERM*(EE+EQXX/4.0D0+QZZE/4.0D0+QZZQXX/16.0D0)
      DG(19)=TERM*(EE+EQXX/4.0D0+QXXE/4.0D0+QXXQXX/16.0D0)
      DG(20)=(TERM*QXZQXZ)/16.0D0
      DG(21)=TERM*(EE+EQXX/4.0D0+QXXE/4.0D0+QXXQYY/16.0D0)
      DG(22)=TERM*(QXXQXX-QXXQYY)/32.0D0
      RETURN
      END
