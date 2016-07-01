      SUBROUTINE REPP(NI,NJ,RIJ,RI,CORE)
C***********************************************************************
C
C..VECTOR VERSION WRITTEN BY ERNEST R. DAVIDSON, INDIANA UNIVERSITY
C
C
C  REPP CALCULATES THE TWO-ELECTRON REPULSION INTEGRALS AND THE
C       NUCLEAR ATTRACTION INTEGRALS.
C
C     ON INPUT RIJ     = INTERATOMIC DISTANCE
C              NI      = ATOM NUMBER OF FIRST ATOM
C              NJ      = ATOM NUMBER OF SECOND ATOM
C    (REF)     ADD     = ARRAY OF GAMMA, OR TWO-ELECTRON ONE-CENTER,
C                        INTEGRALS.
C    (REF)     TORE    = ARRAY OF NUCLEAR CHARGES OF THE ELEMENTS
C    (REF)     DD      = ARRAY OF DIPOLE CHARGE SEPARATIONS
C    (REF)     QQ      = ARRAY OF QUADRUPOLE CHARGE SEPARATIONS
C
C     THE COMMON BLOCKS ARE INITIALIZED IN BLOCK-DATA, AND NEVER CHANGED
C
C    ON OUTPUT RI      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS
C              CORE    = 4 X 2 ARRAY OF ELECTRON-CORE ATTRACTION
C                        INTEGRALS
C
C
C *** THIS ROUTINE COMPUTES THE TWO-CENTRE REPULSION INTEGRALS AND THE
C *** NUCLEAR ATTRACTION INTEGRALS.
C *** THE TWO-CENTRE REPULSION INTEGRALS (OVER LOCAL COORDINATES) ARE
C *** STORED AS FOLLOWS (WHERE P-SIGMA = O,  AND P-PI = P AND P* )
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
C     (PP/P*P*)=21,   (P*P/P*P)=22.
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
C     WHERE IJ=1 IF THE ORBITALS CENTRED ON ATOM I,  =2 IF ON ATOM J.
C *** NI AND NJ ARE THE ATOMIC NUMBERS OF THE TWO ELEMENTS.
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL SI,SJ
      COMMON /MULTIP/ DD(107),QQ(107),ADD(107,3)
      COMMON /CORE/ TORE(107)
      COMMON /NATORB/ NATORB(107)
      DIMENSION RI(22),CORE(4,2)
      DIMENSION ARG(72),SQR(72)
      DATA  TD/2.D00/
      DATA PP/0.5D00/
      DATA A0/0.529167D0/ ,EV/27.21D0/, EV1/13.605D0/, EV2/6.8025D0/,
     1 EV3/3.40125D0/, EV4/1.700625D0/
C
C     ATOMIC UNITS ARE USED IN THE CALCULATION,
C     FINAL RESULTS ARE CONVERTED TO EV
C
      R=RIJ/A0
C
      SI = (NATORB(NI).GE.3)
      SJ = (NATORB(NJ).GE.3)
C
      IF ((.NOT.SI) .AND. (.NOT.SJ)) THEN
C
C     HYDROGEN - HYDROGEN  (SS/SS)
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
         RI(1) = EV/SQRT(R*R+AEE)
         CORE(1,1 )= TORE(NJ)*RI(1)
         CORE(1,2) = TORE(NI)*RI(1)
C
      ELSE IF (SI .AND. (.NOT.SJ)) THEN
C
C     HEAVY ATOM - HYDROGEN
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
         DA=DD(NI)
         QA=QQ(NI) * TD
         ADE = PP/ADD(NI,2) + PP/ADD(NJ,1)
         ADE = ADE * ADE
         AQE = PP/ADD(NI,3) + PP/ADD(NJ,1)
         AQE = AQE * AQE
         RSQ = R*R
         ARG(1) = RSQ + AEE
         XXX = R+DA
         ARG(2) = XXX*XXX + ADE
         XXX = R-DA
         ARG(3) = XXX*XXX + ADE
         XXX = R+QA
         ARG(4) = XXX*XXX + AQE
         XXX = R-QA
         ARG(5) = XXX*XXX + AQE
         ARG(6) = RSQ + AQE
         ARG(7) = ARG(6) + QA*QA
C$DOIT ASIS
         DO 10 I = 1,7
            SQR(I) = SQRT(ARG(I))
   10    CONTINUE
         EE = EV/SQR(1)
         RI(1) = EE
         RI(2) = EV1/SQR(2) - EV1/SQR(3)
         RI(3) = EE + EV2/SQR(4) + EV2/SQR(5) - EV1/SQR(6)
         RI(4) = EE + EV1/SQR(7) - EV1/SQR(6)
         CORE(1,1) = TORE(NJ)*RI(1)
         CORE(1,2) = TORE(NI)*RI(1)
         CORE(2,1) = TORE(NJ)*RI(2)
         CORE(3,1) = TORE(NJ)*RI(3)
         CORE(4,1) = TORE(NJ)*RI(4)
C
      ELSE IF ((.NOT.SI).AND.SJ) THEN
C
C     HYDROGEN - HEAVY ATOM
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
         DB=DD(NJ)
         QB=QQ(NJ) * TD
         AED = PP/ADD(NI,1) + PP/ADD(NJ,2)
         AED = AED * AED
         AEQ = PP/ADD(NI,1) + PP/ADD(NJ,3)
         AEQ = AEQ * AEQ
         RSQ = R*R
         ARG(1) = RSQ + AEE
         XXX = R-DB
         ARG(2) = XXX*XXX + AED
         XXX = R+DB
         ARG(3) = XXX*XXX + AED
         XXX = R-QB
         ARG(4) = XXX*XXX + AEQ
         XXX = R+QB
         ARG(5) = XXX*XXX + AEQ
         ARG(6) = RSQ + AEQ
         ARG(7) = ARG(6) + QB*QB
C$DOIT ASIS
         DO 20 I = 1,7
            SQR(I) = SQRT(ARG(I))
   20    CONTINUE
         EE = EV/SQR(1)
         RI(1) = EE
         RI(5) = EV1/SQR(2)  - EV1/SQR(3)
         RI(11) = EE + EV2/SQR(4) + EV2/SQR(5) - EV1/SQR(6)
         RI(12) = EE + EV1/SQR(7) - EV1/SQR(6)
         CORE(1,1) = TORE(NJ)*RI(1)
         CORE(1,2) = TORE(NI)*RI(1)
         CORE(2,2) = TORE(NI)*RI(5)
         CORE(3,2) = TORE(NI)*RI(11)
         CORE(4,2) = TORE(NI)*RI(12)
C
      ELSE
C
C     HEAVY ATOM - HEAVY ATOM
C
C     DEFINE CHARGE SEPARATIONS.
         DA=DD(NI)
         DB=DD(NJ)
         QA=QQ(NI) * TD
         QB=QQ(NJ) * TD
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
C
         ADE = PP/ADD(NI,2) + PP/ADD(NJ,1)
         ADE = ADE * ADE
         AQE = PP/ADD(NI,3) + PP/ADD(NJ,1)
         AQE = AQE * AQE
         AED = PP/ADD(NI,1) + PP/ADD(NJ,2)
         AED = AED * AED
         AEQ = PP/ADD(NI,1) + PP/ADD(NJ,3)
         AEQ = AEQ * AEQ
         AXX = PP/ADD(NI,2) + PP/ADD(NJ,2)
         AXX = AXX * AXX
         ADQ = PP/ADD(NI,2) + PP/ADD(NJ,3)
         ADQ = ADQ * ADQ
         AQD = PP/ADD(NI,3) + PP/ADD(NJ,2)
         AQD = AQD * AQD
         AQQ = PP/ADD(NI,3) + PP/ADD(NJ,3)
         AQQ = AQQ * AQQ
         RSQ = R * R
         ARG(1) = RSQ + AEE
         XXX = R + DA
         ARG(2) = XXX * XXX + ADE
         XXX = R - DA
         ARG(3) = XXX*XXX + ADE
         XXX = R - QA
         ARG(4) = XXX*XXX + AQE
         XXX = R + QA
         ARG(5) = XXX*XXX + AQE
         ARG(6) = RSQ + AQE
         ARG(7) = ARG(6) + QA*QA
         XXX = R-DB
         ARG(8) = XXX*XXX + AED
         XXX = R+DB
         ARG(9) = XXX*XXX + AED
         XXX = R - QB
         ARG(10) = XXX*XXX + AEQ
         XXX = R + QB
         ARG(11) = XXX*XXX + AEQ
         ARG(12) = RSQ + AEQ
         ARG(13) = ARG(12) + QB*QB
         XXX = DA-DB
         ARG(14) = RSQ + AXX + XXX*XXX
         XXX = DA+DB
         ARG(15) = RSQ + AXX + XXX*XXX
         XXX = R + DA - DB
         ARG(16) = XXX*XXX + AXX
         XXX = R - DA + DB
         ARG(17) = XXX*XXX + AXX
         XXX = R - DA - DB
         ARG(18) = XXX*XXX + AXX
         XXX = R + DA + DB
         ARG(19) = XXX*XXX + AXX
         XXX = R + DA
         ARG(20) = XXX*XXX + ADQ
         ARG(21) = ARG(20) + QB*QB
         XXX = R - DA
         ARG(22) = XXX*XXX + ADQ
         ARG(23) = ARG(22) + QB*QB
         XXX = R - DB
         ARG(24) = XXX*XXX + AQD
         ARG(25) = ARG(24) + QA*QA
         XXX = R + DB
         ARG(26) = XXX*XXX + AQD
         ARG(27) = ARG(26) + QA*QA
         XXX = R + DA - QB
         ARG(28) = XXX*XXX + ADQ
         XXX = R - DA - QB
         ARG(29) = XXX*XXX + ADQ
         XXX = R + DA + QB
         ARG(30) = XXX*XXX + ADQ
         XXX = R - DA + QB
         ARG(31) = XXX*XXX + ADQ
         XXX = R + QA - DB
         ARG(32) = XXX*XXX + AQD
         XXX = R + QA + DB
         ARG(33) = XXX*XXX + AQD
         XXX = R - QA - DB
         ARG(34) = XXX*XXX + AQD
         XXX = R - QA + DB
         ARG(35) = XXX*XXX + AQD
         ARG(36) = RSQ + AQQ
         XXX = QA - QB
         ARG(37) = ARG(36) + XXX*XXX
         XXX = QA + QB
         ARG(38) = ARG(36) + XXX*XXX
         ARG(39) = ARG(36) + QA*QA
         ARG(40) = ARG(36) + QB*QB
         ARG(41) = ARG(39) + QB*QB
         XXX = R - QB
         ARG(42) = XXX*XXX + AQQ
         ARG(43) = ARG(42) + QA*QA
         XXX = R + QB
         ARG(44) = XXX*XXX + AQQ
         ARG(45) = ARG(44) + QA*QA
         XXX = R + QA
         ARG(46) = XXX*XXX + AQQ
         ARG(47) = ARG(46) + QB*QB
         XXX = R - QA
         ARG(48) = XXX*XXX + AQQ
         ARG(49) = ARG(48) + QB*QB
         XXX = R + QA - QB
         ARG(50) = XXX*XXX + AQQ
         XXX = R + QA + QB
         ARG(51) = XXX*XXX + AQQ
         XXX = R - QA - QB
         ARG(52) = XXX*XXX + AQQ
         XXX = R - QA + QB
         ARG(53) = XXX*XXX + AQQ
         QA=QQ(NI)
         QB=QQ(NJ)
         XXX = DA - QB
         XXX = XXX*XXX
         YYY = R - QB
         YYY = YYY*YYY
         ZZZ = DA + QB
         ZZZ = ZZZ*ZZZ
         WWW = R + QB
         WWW = WWW*WWW
         ARG(54) = XXX + YYY + ADQ
         ARG(55) = XXX + WWW + ADQ
         ARG(56) = ZZZ + YYY + ADQ
         ARG(57) = ZZZ + WWW + ADQ
         XXX = QA - DB
         XXX = XXX*XXX
         YYY = QA + DB
         YYY = YYY*YYY
         ZZZ = R + QA
         ZZZ = ZZZ*ZZZ
         WWW = R - QA
         WWW = WWW*WWW
         ARG(58) = ZZZ + XXX + AQD
         ARG(59) = WWW + XXX + AQD
         ARG(60) = ZZZ + YYY + AQD
         ARG(61) = WWW + YYY + AQD
         XXX = QA - QB
         XXX = XXX*XXX
         ARG(62) = ARG(36) + TD*XXX
         YYY = QA + QB
         YYY = YYY*YYY
         ARG(63) = ARG(36) + TD*YYY
         ARG(64) = ARG(36) + TD*(QA*QA+QB*QB)
         ZZZ = R + QA - QB
         ZZZ = ZZZ*ZZZ
         ARG(65) = ZZZ + XXX + AQQ
         ARG(66) = ZZZ + YYY + AQQ
         ZZZ = R + QA + QB
         ZZZ = ZZZ*ZZZ
         ARG(67) = ZZZ + XXX + AQQ
         ARG(68) = ZZZ + YYY + AQQ
         ZZZ = R - QA - QB
         ZZZ = ZZZ*ZZZ
         ARG(69) = ZZZ + XXX + AQQ
         ARG(70) = ZZZ + YYY + AQQ
         ZZZ = R - QA + QB
         ZZZ = ZZZ*ZZZ
         ARG(71) = ZZZ + XXX + AQQ
         ARG(72) = ZZZ + YYY + AQQ
         DO 30 I = 1,72
            SQR(I) = SQRT(ARG(I))
   30    CONTINUE
         EE = EV/SQR(1)
         DZE = -EV1/SQR(2) + EV1/SQR(3)
         QZZE = EV2/SQR(4) + EV2/SQR(5) - EV1/SQR(6)
         QXXE = EV1/SQR(7) - EV1/SQR(6)
         EDZ = - EV1/SQR(8) + EV1/SQR(9)
         EQZZ  = EV2/SQR(10) + EV2/SQR(11) - EV1/SQR(12)
         EQXX  = EV1/SQR(13) - EV1/SQR(12)
         DXDX  = EV1/SQR(14) - EV1/SQR(15)
         DZDZ  = EV2/SQR(16) + EV2/SQR(17) - EV2/SQR(18) - EV2/SQR(19)
         DZQXX =  EV2/SQR(20) - EV2/SQR(21) - EV2/SQR(22) + EV2/SQR(23)
         QXXDZ =  EV2/SQR(24) - EV2/SQR(25) - EV2/SQR(26) + EV2/SQR(27)
         DZQZZ = -EV3/SQR(28) + EV3/SQR(29) - EV3/SQR(30) + EV3/SQR(31)
     1       - EV2/SQR(22) + EV2/SQR(20)
         QZZDZ = -EV3/SQR(32) + EV3/SQR(33) - EV3/SQR(34) + EV3/SQR(35)
     1       + EV2/SQR(24) - EV2/SQR(26)
         QXXQXX = EV3/SQR(37) + EV3/SQR(38) - EV2/SQR(39) - EV2/SQR(40)
     1       + EV2/SQR(36)
         QXXQYY = EV2/SQR(41) - EV2/SQR(39) - EV2/SQR(40) + EV2/SQR(36)
         QXXQZZ = EV3/SQR(43) + EV3/SQR(45) - EV3/SQR(42) - EV3/SQR(44)
     1       - EV2/SQR(39) + EV2/SQR(36)
         QZZQXX = EV3/SQR(47) + EV3/SQR(49) - EV3/SQR(46) - EV3/SQR(48)
     1       - EV2/SQR(40) + EV2/SQR(36)
         QZZQZZ = EV4/SQR(50) + EV4/SQR(51) + EV4/SQR(52) + EV4/SQR(53)
     1       - EV3/SQR(48) - EV3/SQR(46) - EV3/SQR(42) - EV3/SQR(44)
     2       + EV2/SQR(36)
         DXQXZ = -EV2/SQR(54) + EV2/SQR(55) + EV2/SQR(56) - EV2/SQR(57)
         QXZDX = -EV2/SQR(58) + EV2/SQR(59) + EV2/SQR(60) - EV2/SQR(61)
         QXZQXZ = EV3/SQR(65) - EV3/SQR(67) - EV3/SQR(69) + EV3/SQR(71)
     1       - EV3/SQR(66) + EV3/SQR(68) + EV3/SQR(70) - EV3/SQR(72)
         RI(1) = EE
         RI(2) = -DZE
         RI(3) = EE + QZZE
         RI(4) = EE + QXXE
         RI(5) = -EDZ
         RI(6) = DZDZ
         RI(7) = DXDX
         RI(8) = -EDZ -QZZDZ
         RI(9) = -EDZ -QXXDZ
         RI(10) = -QXZDX
         RI(11) =  EE + EQZZ
         RI(12) =  EE + EQXX
         RI(13) = -DZE -DZQZZ
         RI(14) = -DZE -DZQXX
         RI(15) = -DXQXZ
         RI(16) = EE +EQZZ +QZZE +QZZQZZ
         RI(17) = EE +EQZZ +QXXE +QXXQZZ
         RI(18) = EE +EQXX +QZZE +QZZQXX
         RI(19) = EE +EQXX +QXXE +QXXQXX
         RI(20) = QXZQXZ
         RI(21) = EE +EQXX +QXXE +QXXQYY
         RI(22) = PP * (QXXQXX -QXXQYY)
C
C     CALCULATE CORE-ELECTRON ATTRACTIONS.
C
         CORE(1,1) = TORE(NJ)*RI(1)
         CORE(2,1) = TORE(NJ)*RI(2)
         CORE(3,1) = TORE(NJ)*RI(3)
         CORE(4,1) = TORE(NJ)*RI(4)
         CORE(1,2) = TORE(NI)*RI(1)
         CORE(2,2) = TORE(NI)*RI(5)
         CORE(3,2) = TORE(NI)*RI(11)
         CORE(4,2) = TORE(NI)*RI(12)
C
      END IF
C
      RETURN
C
      END
