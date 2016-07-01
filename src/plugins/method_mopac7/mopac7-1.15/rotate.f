      SUBROUTINE ROTATE (NI,NJ,XI,XJ,W,KR,E1B,E2A,ENUC,CUTOFF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C
C..IMPROVED SCALAR VERSION
C..WRITTEN BY ERNEST R. DAVIDSON, INDIANA UNIVERSITY.
C
C
C   ROTATE CALCULATES THE TWO-PARTICLE INTERACTIONS.
C
C   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
C             NJ     = ATOMIC NUMBER OF SECOND ATOM.
C             XI     = COORDINATE OF FIRST ATOM.
C             XJ     = COORDINATE OF SECOND ATOM.
C
C ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS.
C           E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
C                    E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
C           ENUC   = NUCLEAR-NUCLEAR REPULSION TERM.
C
C
C *** THIS ROUTINE COMPUTES THE REPULSION AND NUCLEAR ATTRACTION
C     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER
C     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND
C     STORED AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* )
C     IN RI
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
C     (PP/P*P*)=21,   (P*P/P*P)=22.
C
C***********************************************************************
      COMMON /NUMCAL/ NUMCAL
      SAVE ANALYT, ICALCN
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE
      CHARACTER*241 KEYWRD
      LOGICAL SI,SJ, ANALYT
      COMMON /NATORB/ NATORB(107)
      COMMON /TWOEL3/ F03(107)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /ALPHA / ALP(107)
      COMMON /CORE  / TORE(107)
      COMMON /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10)
      COMMON /ALPTM / ALPTM(30), EMUDTM(30)
      COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
      COMMON /ROTDU2/ X(3),Y(3),Z(3)
      COMMON /KEYWRD/ KEYWRD
      DIMENSION XI(3),XJ(3),W(100),E1B(10),E2A(10)
      DIMENSION RI(22),CCORE(4,2), BORON1(3,4), BORON2(3,4), BORON3(3,4)
      EQUIVALENCE (CCORE(1,1),CSS1)
      DATA ICALCN/0/
      DATA BORON1/  0.182613D0,  0.118587D0, -0.073280D0,
     1              0.412253D0, -0.149917D0,  0.000000D0,
     2              0.261751D0,  0.050275D0,  0.000000D0,
     3              0.359244D0,  0.074729D0,  0.000000D0/
      DATA BORON2/  6.D0,  6.D0,  5.D0,
     1             10.D0,  6.D0,  0.D0,
     2              8.D0,  5.D0,  0.D0,
     3              9.D0,  9.D0,  0.D0/
      DATA BORON3/  0.727592D0,  1.466639D0,  1.570975D0,
     1              0.832586D0,  1.186220D0,  0.000000D0,
     2              1.063995D0,  1.936492D0,  0.000000D0,
     3              0.819351D0,  1.574414D0,  0.000000D0/
C
      IF(ICALCN.NE.NUMCAL)THEN
         ICALCN=NUMCAL
         ANALYT=(INDEX(KEYWRD,'ANALYT') .NE. 0)
         IF(ANALYT)THEN
            OPEN(UNIT=2,STATUS='SCRATCH',FORM='UNFORMATTED')
            REWIND 2
         ENDIF
      ENDIF
C
      X(1)=XI(1)-XJ(1)
      X(2)=XI(2)-XJ(2)
      X(3)=XI(3)-XJ(3)
      RIJ=X(1)*X(1)+X(2)*X(2)+X(3)*X(3)
      IF (RIJ.LT.0.00002D0) THEN
C
C     SMALL RIJ CASE
C
         DO 10 I=1,10
            E1B(I)=0.D0
            E2A(I)=0.D0
   10    CONTINUE
         W(KR)=0.D0
         ENUC=0.D0
C
      ELSE IF (ITYPE.EQ.4) THEN
C
C     MINDO CASE
C
         SUM=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2)
         W(1)=SUM
         KR=KR+1
         DO 20 L=1,10
            E1B(L)=0.D0
            E2A(L)=0.D0
   20    CONTINUE
         E1B(1) = -SUM*TORE(NJ)
         E1B(3) = E1B(1)
         E1B(6) = E1B(1)
         E1B(10)= E1B(1)
         E2A(1) = -SUM*TORE(NI)
         E2A(3) = E2A(1)
         E2A(6) = E2A(1)
         E2A(10)= E2A(1)
         II = MAX(NI,NJ)
         NBOND = (II*(II-1))/2+NI+NJ-II
         RIJ = SQRT(RIJ)
         IF(NBOND.LT.154) THEN
            IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) THEN
C              NBOND = 22 IS C-H CASE
C              NBOND = 29 IS N-H CASE
               SCALE=ALP3(NBOND)*EXP(-RIJ)
            ELSE
C              NBOND < 154  IS NI < 18 AND NJ < 18 CASE
               SCALE=EXP(-ALP3(NBOND)*RIJ)
            ENDIF
         ELSE
C              NBOND > 154 INVOLVES NI OR NJ > 18
            SCALE = 0
            IF(NATORB(NI).EQ.0) SCALE=      EXP(-ALP(NI)*RIJ)
            IF(NATORB(NJ).EQ.0) SCALE=SCALE+EXP(-ALP(NI)*RIJ)
         ENDIF
         IF (ABS(TORE(NI)).GT.20.D0 .AND. ABS(TORE(NJ)).GT.20.D0) THEN
            ENUC=0.D0
         ELSE IF (RIJ.LT.1.D0 .AND. NATORB(NI)*NATORB(NJ).EQ.0) THEN
            ENUC=0.D0
         ELSE
            ENUC = TORE(NI)*TORE(NJ)*SUM
     1       + ABS(TORE(NI)*TORE(NJ)*(14.399D0/RIJ-SUM)*SCALE)
         ENDIF
C
C     MNDO AND AM1 CASES
C
C *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
C     ORDER IN WHICH THEY WILL LATER BE USED.  IE.  (I,J/K,L) WHERE
C     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST
C     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)
C
      ELSE
C
         RIJX = SQRT(RIJ)
         RIJ = MIN(RIJX,CUTOFF)
C
C *** COMPUTE INTEGRALS IN DIATOMIC FRAME
C
         CALL REPP(NI,NJ,RIJ,RI,CCORE)
         IF(ANALYT)WRITE(2)(RI(I),I=1,22)
C
         GAM = RI(1)
         A=1.D0/RIJX
         X(1) = X(1)*A
         X(2) = X(2)*A
         X(3) = X(3)*A
         IF (ABS(X(3)).GT.0.99999999D0) THEN
            X(3) = SIGN(1.D0,X(3))
            Y(1) = 0.D0
            Y(2) = 1.D0
            Y(3) = 0.D0
            Z(1) = 1.D0
            Z(2) = 0.D0
            Z(3) = 0.D0
         ELSE
            Z(3)=SQRT(1.D0-X(3)*X(3))
            A=1.D0/Z(3)
            Y(1)=-A*X(2)*SIGN(1.D0,X(1))
            Y(2)=ABS(A*X(1))
            Y(3)=0.D0
            Z(1)=-A*X(1)*X(3)
            Z(2)=-A*X(2)*X(3)
         ENDIF
         SI = (NATORB(NI).GT.1)
         SJ = (NATORB(NJ).GT.1)
         IF ( SI .OR. SJ) THEN
            XX11 = X(1)*X(1)
            XX21 = X(2)*X(1)
            XX22 = X(2)*X(2)
            XX31 = X(3)*X(1)
            XX32 = X(3)*X(2)
            XX33 = X(3)*X(3)
            YY11 = Y(1)*Y(1)
            YY21 = Y(2)*Y(1)
            YY22 = Y(2)*Y(2)
            ZZ11 = Z(1)*Z(1)
            ZZ21 = Z(2)*Z(1)
            ZZ22 = Z(2)*Z(2)
            ZZ31 = Z(3)*Z(1)
            ZZ32 = Z(3)*Z(2)
            ZZ33 = Z(3)*Z(3)
            YYZZ11 = YY11+ZZ11
            YYZZ21 = YY21+ZZ21
            YYZZ22 = YY22+ZZ22
            XY11 = 2.D0*X(1)*Y(1)
            XY21 =      X(1)*Y(2)+X(2)*Y(1)
            XY22 = 2.D0*X(2)*Y(2)
            XY31 =      X(3)*Y(1)
            XY32 =      X(3)*Y(2)
            XZ11 = 2.D0*X(1)*Z(1)
            XZ21 =      X(1)*Z(2)+X(2)*Z(1)
            XZ22 = 2.D0*X(2)*Z(2)
            XZ31 =      X(1)*Z(3)+X(3)*Z(1)
            XZ32 =      X(2)*Z(3)+X(3)*Z(2)
            XZ33 = 2.D0*X(3)*Z(3)
            YZ11 = 2.D0*Y(1)*Z(1)
            YZ21 =      Y(1)*Z(2)+Y(2)*Z(1)
            YZ22 = 2.D0*Y(2)*Z(2)
            YZ31 =      Y(1)*Z(3)
            YZ32 =      Y(2)*Z(3)
         ENDIF
C
C     (S S/S S)
         W(1)=RI(1)
         KI = 1
         IF (SJ) THEN
C     (S S/PX S)
            W(2)=RI(5)*X(1)
C     (S S/PX PX)
            W(3)=RI(11)*XX11+RI(12)*YYZZ11
C     (S S/PY S)
            W(4)=RI(5)*X(2)
C     (S S/PY PX)
            W(5)=RI(11)*XX21+RI(12)*YYZZ21
C     (S S/PY PY)
            W(6)=RI(11)*XX22+RI(12)*YYZZ22
C     (S S/PZ S)
            W(7)=RI(5)*X(3)
C     (S S/PZ PX)
            W(8)=RI(11)*XX31+RI(12)*ZZ31
C     (S S/PZ PY)
            W(9)=RI(11)*XX32+RI(12)*ZZ32
C     (S S/PZ PZ)
            W(10)=RI(11)*XX33+RI(12)*ZZ33
            KI = 10
         ENDIF
C
         IF (SI) THEN
C     (PX S/S S)
            W(11)=RI(2)*X(1)
            IF (SJ) THEN
C     (PX S/PX S)
               W(12)=RI(6)*XX11+RI(7)*YYZZ11
C     (PX S/PX PX)
               W(13)=X(1)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(Y(1)*XY11+Z(1)*XZ11)
C     (PX S/PY S)
               W(14)=RI(6)*XX21+RI(7)*YYZZ21
C     (PX S/PY PX)
               W(15)=X(1)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(Y(1)*XY21+Z(1)*XZ21)
C     (PX S/PY PY)
               W(16)=X(1)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(Y(1)*XY22+Z(1)*XZ22)
C     (PX S/PZ S)
               W(17)=RI(6)*XX31+RI(7)*ZZ31
C     (PX S/PZ PX)
               W(18)=X(1)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(Y(1)*XY31+Z(1)*XZ31)
C     (PX S/PZ PY)
               W(19)=X(1)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(Y(1)*XY32+Z(1)*XZ32)
C     (PX S/PZ PZ)
               W(20)=X(1)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(          Z(1)*XZ33)
C     (PX PX/S S)
               W(21)=RI(3)*XX11+RI(4)*YYZZ11
C     (PX PX/PX S)
               W(22)=X(1)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(Y(1)*XY11+Z(1)*XZ11)
C     (PX PX/PX PX)
               W(23) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX11+RI(18)*XX11*YYZZ11
     2     +RI(19)*(YY11*YY11+ZZ11*ZZ11)
     3     +RI(20)*(XY11*XY11+XZ11*XZ11)
     4     +RI(21)*(YY11*ZZ11+ZZ11*YY11)
     5     +RI(22)*YZ11*YZ11
C     (PX PX/PY S)
               W(24)=X(2)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(Y(2)*XY11+Z(2)*XZ11)
C     (PX PX/PY PX)
               W(25) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX21+RI(18)*XX11*YYZZ21
     2     +RI(19)*(YY11*YY21+ZZ11*ZZ21)
     3     +RI(20)*(XY11*XY21+XZ11*XZ21)
     4     +RI(21)*(YY11*ZZ21+ZZ11*YY21)
     5     +RI(22)*YZ11*YZ21
C     (PX PX/PY PY)
               W(26) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX22+RI(18)*XX11*YYZZ22
     2     +RI(19)*(YY11*YY22+ZZ11*ZZ22)
     3     +RI(20)*(XY11*XY22+XZ11*XZ22)
     4     +RI(21)*(YY11*ZZ22+ZZ11*YY22)
     5     +RI(22)*YZ11*YZ22
C     (PX PX/PZ S)
               W(27)=X(3)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(         +Z(3)*XZ11)
C     (PX PX/PZ PX)
               W(28) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX31
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ31
     3     +RI(20)*(XY11*XY31+XZ11*XZ31)
     4     +RI(22)*YZ11*YZ31
C     (PX PX/PZ PY)
               W(29) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX32
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ32
     3     +RI(20)*(XY11*XY32+XZ11*XZ32)
     4     +RI(22)*YZ11*YZ32
C     (PX PX/PZ PZ)
               W(30) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX33
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ33
     3     +RI(20)*XZ11*XZ33
C     (PY S/S S)
               W(31)=RI(2)*X(2)
C     (PY S/PX S)
               W(32)=RI(6)*XX21+RI(7)*YYZZ21
C     (PY S/PX PX)
               W(33)=X(2)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(Y(2)*XY11+Z(2)*XZ11)
C     (PY S/PY S)
               W(34)=RI(6)*XX22+RI(7)*YYZZ22
C     (PY S/PY PX)
               W(35)=X(2)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(Y(2)*XY21+Z(2)*XZ21)
C     (PY S/PY PY)
               W(36)=X(2)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(Y(2)*XY22+Z(2)*XZ22)
C     (PY S/PZ S)
               W(37)=RI(6)*XX32+RI(7)*ZZ32
C     (PY S/PZ PX)
               W(38)=X(2)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(Y(2)*XY31+Z(2)*XZ31)
C     (PY S/PZ PY)
               W(39)=X(2)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(Y(2)*XY32+Z(2)*XZ32)
C     (PY S/PZ PZ)
               W(40)=X(2)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(         +Z(2)*XZ33)
C     (PY PX/S S)
               W(41)=RI(3)*XX21+RI(4)*YYZZ21
C     (PY PX/PX S)
               W(42)=X(1)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(Y(1)*XY21+Z(1)*XZ21)
C     (PY PX/PX PX)
               W(43) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX11+RI(18)*XX21*YYZZ11
     2     +RI(19)*(YY21*YY11+ZZ21*ZZ11)
     3     +RI(20)*(XY21*XY11+XZ21*XZ11)
     4     +RI(21)*(YY21*ZZ11+ZZ21*YY11)
     5     +RI(22)*YZ21*YZ11
C     (PY PX/PY S)
               W(44)=X(2)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(Y(2)*XY21+Z(2)*XZ21)
C     (PY PX/PY PX)
               W(45) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX21+RI(18)*XX21*YYZZ21
     2     +RI(19)*(YY21*YY21+ZZ21*ZZ21)
     3     +RI(20)*(XY21*XY21+XZ21*XZ21)
     4     +RI(21)*(YY21*ZZ21+ZZ21*YY21)
     5     +RI(22)*YZ21*YZ21
C     (PY PX/PY PY)
               W(46) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX22+RI(18)*XX21*YYZZ22
     2     +RI(19)*(YY21*YY22+ZZ21*ZZ22)
     3     +RI(20)*(XY21*XY22+XZ21*XZ22)
     4     +RI(21)*(YY21*ZZ22+ZZ21*YY22)
     5     +RI(22)*YZ21*YZ22
C     (PY PX/PZ S)
               W(47)=X(3)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(         +Z(3)*XZ21)
C      (PY PX/PZ PX)
               W(48) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX31
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ31
     3     +RI(20)*(XY21*XY31+XZ21*XZ31)
     4     +RI(22)*YZ21*YZ31
C      (PY PX/PZ PY)
               W(49) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX32
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ32
     3     +RI(20)*(XY21*XY32+XZ21*XZ32)
     4     +RI(22)*YZ21*YZ32
C      (PY PX/PZ PZ)
               W(50) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX33
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ33
     3     +RI(20)*XZ21*XZ33
C     (PY PY/S S)
               W(51)=RI(3)*XX22+RI(4)*YYZZ22
C     (PY PY/PX S)
               W(52)=X(1)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(Y(1)*XY22+Z(1)*XZ22)
C      (PY PY/PX PX)
               W(53) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX11+RI(18)*XX22*YYZZ11
     2     +RI(19)*(YY22*YY11+ZZ22*ZZ11)
     3     +RI(20)*(XY22*XY11+XZ22*XZ11)
     4     +RI(21)*(YY22*ZZ11+ZZ22*YY11)
     5     +RI(22)*YZ22*YZ11
C     (PY PY/PY S)
               W(54)=X(2)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(Y(2)*XY22+Z(2)*XZ22)
C      (PY PY/PY PX)
               W(55) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX21+RI(18)*XX22*YYZZ21
     2     +RI(19)*(YY22*YY21+ZZ22*ZZ21)
     3     +RI(20)*(XY22*XY21+XZ22*XZ21)
     4     +RI(21)*(YY22*ZZ21+ZZ22*YY21)
     5     +RI(22)*YZ22*YZ21
C      (PY PY/PY PY)
               W(56) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX22+RI(18)*XX22*YYZZ22
     2     +RI(19)*(YY22*YY22+ZZ22*ZZ22)
     3     +RI(20)*(XY22*XY22+XZ22*XZ22)
     4     +RI(21)*(YY22*ZZ22+ZZ22*YY22)
     5     +RI(22)*YZ22*YZ22
C     (PY PY/PZ S)
               W(57)=X(3)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(         +Z(3)*XZ22)
C      (PY PY/PZ PX)
               W(58) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX31
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ31
     3     +RI(20)*(XY22*XY31+XZ22*XZ31)
     4     +RI(22)*YZ22*YZ31
C      (PY PY/PZ PY)
               W(59) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX32
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ32
     3     +RI(20)*(XY22*XY32+XZ22*XZ32)
     4     +RI(22)*YZ22*YZ32
C      (PY PY/PZ PZ)
               W(60) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX33
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ33
     3     +RI(20)*XZ22*XZ33
C     (PZ S/SS)
               W(61)=RI(2)*X(3)
C     (PZ S/PX S)
               W(62)=RI(6)*XX31+RI(7)*ZZ31
C     (PZ S/PX PX)
               W(63)=X(3)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(         +Z(3)*XZ11)
C     (PZ S/PY S)
               W(64)=RI(6)*XX32+RI(7)*ZZ32
C     (PZ S/PY PX)
               W(65)=X(3)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(         +Z(3)*XZ21)
C     (PZ S/PY PY)
               W(66)=X(3)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(         +Z(3)*XZ22)
C     (PZ S/PZ S)
               W(67)=RI(6)*XX33+RI(7)*ZZ33
C     (PZ S/PZ PX)
               W(68)=X(3)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(         +Z(3)*XZ31)
C     (PZ S/PZ PY)
               W(69)=X(3)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(         +Z(3)*XZ32)
C     (PZ S/PZ PZ)
               W(70)=X(3)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(         +Z(3)*XZ33)
C     (PZ PX/S S)
               W(71)=RI(3)*XX31+RI(4)*ZZ31
C     (PZ PX/PX S)
               W(72)=X(1)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(Y(1)*XY31+Z(1)*XZ31)
C      (PZ PX/PX PX)
               W(73) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX11+RI(18)*XX31*YYZZ11
     2     +RI(19)*ZZ31*ZZ11
     3     +RI(20)*(XY31*XY11+XZ31*XZ11)
     4     +RI(21)*ZZ31*YY11
     5     +RI(22)*YZ31*YZ11
C     (PZ PX/PY S)
               W(74)=X(2)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(Y(2)*XY31+Z(2)*XZ31)
C      (PZ PX/PY PX)
               W(75) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX21+RI(18)*XX31*YYZZ21
     2     +RI(19)*ZZ31*ZZ21
     3     +RI(20)*(XY31*XY21+XZ31*XZ21)
     4     +RI(21)*ZZ31*YY21
     5     +RI(22)*YZ31*YZ21
C      (PZ PX/PY PY)
               W(76) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX22+RI(18)*XX31*YYZZ22
     2     +RI(19)*ZZ31*ZZ22
     3     +RI(20)*(XY31*XY22+XZ31*XZ22)
     4     +RI(21)*ZZ31*YY22
     5     +RI(22)*YZ31*YZ22
C     (PZ PX/PZ S)
               W(77)=X(3)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(         +Z(3)*XZ31)
C     (PZ PX/PZ PX)
               W(78) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX31
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ31
     3     +RI(20)*(XY31*XY31+XZ31*XZ31)
     4     +RI(22)*YZ31*YZ31
C      (PZ PX/PZ PY)
               W(79) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX32
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ32
     3     +RI(20)*(XY31*XY32+XZ31*XZ32)
     4     +RI(22)*YZ31*YZ32
C      (PZ PX/PZ PZ)
               W(80) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX33
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ33
     3     +RI(20)*XZ31*XZ33
C     (PZ PY/S S)
               W(81)=RI(3)*XX32+RI(4)*ZZ32
C     (PZ PY/PX S)
               W(82)=X(1)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(Y(1)*XY32+Z(1)*XZ32)
C      (PZ PY/PX PX)
               W(83) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX11+RI(18)*XX32*YYZZ11
     2     +RI(19)*ZZ32*ZZ11
     3     +RI(20)*(XY32*XY11+XZ32*XZ11)
     4     +RI(21)*ZZ32*YY11
     5     +RI(22)*YZ32*YZ11
C     (PZ PY/PY S)
               W(84)=X(2)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(Y(2)*XY32+Z(2)*XZ32)
C      (PZ PY/PY PX)
               W(85) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX21+RI(18)*XX32*YYZZ21
     2     +RI(19)*ZZ32*ZZ21
     3     +RI(20)*(XY32*XY21+XZ32*XZ21)
     4     +RI(21)*ZZ32*YY21
     5     +RI(22)*YZ32*YZ21
C      (PZ PY/PY PY)
               W(86) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX22+RI(18)*XX32*YYZZ22
     2     +RI(19)*ZZ32*ZZ22
     3     +RI(20)*(XY32*XY22+XZ32*XZ22)
     4     +RI(21)*ZZ32*YY22
     5     +RI(22)*YZ32*YZ22
C     (PZ PY/PZ S)
               W(87)=X(3)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(         +Z(3)*XZ32)
C      (PZ PY/PZ PX)
               W(88) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX31
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ31
     3     +RI(20)*(XY32*XY31+XZ32*XZ31)
     4     +RI(22)*YZ32*YZ31
C      (PZ PY/PZ PY)
               W(89) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX32
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ32
     3     +RI(20)*(XY32*XY32+XZ32*XZ32)
     4     +RI(22)*YZ32*YZ32
C       (PZ PY/PZ PZ)
               W(90) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX33
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ33
     3     +RI(20)*XZ32*XZ33
C     (PZ PZ/S S)
               W(91)=RI(3)*XX33+RI(4)*ZZ33
C     (PZ PZ/PX S)
               W(92)=X(1)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(          Z(1)*XZ33)
C       (PZ PZ/PX PX)
               W(93) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX11+RI(18)*XX33*YYZZ11
     2     +RI(19)*ZZ33*ZZ11
     3     +RI(20)*XZ33*XZ11
     4     +RI(21)*ZZ33*YY11
C     (PZ PZ/PY S)
               W(94)=X(2)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(         +Z(2)*XZ33)
C       (PZ PZ/PY PX)
               W(95) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX21+RI(18)*XX33*YYZZ21
     2     +RI(19)*ZZ33*ZZ21
     3     +RI(20)*XZ33*XZ21
     4     +RI(21)*ZZ33*YY21
C       (PZ PZ/PY PY)
               W(96) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX22+RI(18)*XX33*YYZZ22
     2     +RI(19)*ZZ33*ZZ22
     3     +RI(20)*XZ33*XZ22
     4     +RI(21)*ZZ33*YY22
C     (PZ PZ/PZ S)
               W(97)=X(3)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(         +Z(3)*XZ33)
C       (PZ PZ/PZ PX)
               W(98) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX31
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ31
     3     +RI(20)*XZ33*XZ31
C       (PZ PZ/PZ PY)
               W(99) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX32
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ32
     3     +RI(20)*XZ33*XZ32
C       (PZ PZ/PZ PZ)
               W(100) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX33
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ33
     3     +RI(20)*XZ33*XZ33
               KI = 100
            ELSE
C     (PX S/S S)
               W(2)=RI(2)*X(1)
C     (PX PX/S S)
               W(3)=RI(3)*XX11+RI(4)*YYZZ11
C     (PY S/S S)
               W(4)=RI(2)*X(2)
C     (PY PX/S S)
               W(5)=RI(3)*XX21+RI(4)*YYZZ21
C     (PY PY/S S)
               W(6)=RI(3)*XX22+RI(4)*YYZZ22
C     (PZ S/SS)
               W(7)=RI(2)*X(3)
C     (PZ PX/S S)
               W(8)=RI(3)*XX31+RI(4)*ZZ31
C     (PZ PY/S S)
               W(9)=RI(3)*XX32+RI(4)*ZZ32
C     (PZ PZ/S S)
               W(10)=RI(3)*XX33+RI(4)*ZZ33
               KI = 10
            END IF
         END IF
C
C *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
C
         E1B(1)=-CSS1
         IF(NATORB(NI).EQ.4) THEN
            E1B(2) = -CSP1 *X(1)
            E1B(3) = -CPPS1*XX11-CPPP1*YYZZ11
            E1B(4) = -CSP1 *X(2)
            E1B(5) = -CPPS1*XX21-CPPP1*YYZZ21
            E1B(6) = -CPPS1*XX22-CPPP1*YYZZ22
            E1B(7) = -CSP1 *X(3)
            E1B(8) = -CPPS1*XX31-CPPP1*ZZ31
            E1B(9) = -CPPS1*XX32-CPPP1*ZZ32
            E1B(10)= -CPPS1*XX33-CPPP1*ZZ33
         END IF
         E2A(1)=-CSS2
         IF(NATORB(NJ).EQ.4) THEN
            E2A(2) = -CSP2 *X(1)
            E2A(3) = -CPPS2*XX11-CPPP2*YYZZ11
            E2A(4) = -CSP2 *X(2)
            E2A(5) = -CPPS2*XX21-CPPP2*YYZZ21
            E2A(6) = -CPPS2*XX22-CPPP2*YYZZ22
            E2A(7) = -CSP2 *X(3)
            E2A(8) = -CPPS2*XX31-CPPP2*ZZ31
            E2A(9) = -CPPS2*XX32-CPPP2*ZZ32
            E2A(10)= -CPPS2*XX33-CPPP2*ZZ33
         END IF
         IF(ABS(TORE(NI)).GT.20.D0.AND.ABS(TORE(NJ)).GT.20.D0) THEN
C SPARKLE-SPARKLE INTERACTION
            ENUC=0.D0
            RETURN
         ELSEIF (RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
            ENUC=0.D0
            RETURN
         ENDIF
         SCALE = EXP(-ALP(NI)*RIJ)+EXP(-ALP(NJ)*RIJ)
C
         IF (NI.EQ.24.AND.NJ.EQ.24) THEN
            SCALE = EXP(-ALPTM(NI)*RIJ)+EXP(-ALPTM(NJ)*RIJ)
         ENDIF
C
         NT=NI+NJ
         IF(NT.EQ.8.OR.NT.EQ.9) THEN
            IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NI)*R
     1IJ)
            IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(RIJ-1.D0)*EXP(-ALP(NJ)*R
     1IJ)
         ENDIF
         ENUC = TORE(NI)*TORE(NJ)*GAM
         SCALE=ABS(SCALE*ENUC)
         IF(ITYPE.EQ.2.AND.(NI.EQ.5.OR.NJ.EQ.5))THEN
C
C   LOAD IN AM1 BORON GAUSSIANS
C
            NK=NI+NJ-5
C   NK IS THE ATOMIC NUMBER OF THE NON-BORON ATOM
            NL=1
            IF(NK.EQ.1)NL=2
            IF(NK.EQ.6)NL=3
            IF(NK.EQ.9.OR.NK.EQ.17.OR.NK.EQ.35.OR.NK.EQ.53)NL=4
            DO 30 I=1,3
               FN1(5,I)=BORON1(I,NL)
               FN2(5,I)=BORON2(I,NL)
   30       FN3(5,I)=BORON3(I,NL)
         ENDIF
         IF(ITYPE.EQ.2.OR.ITYPE.EQ.3) THEN
            DO 40 IG=1,10
               IF(ABS(FN1(NI,IG)).GT.0.D0) THEN
                  AX = FN2(NI,IG)*(RIJ-FN3(NI,IG))**2
                  IF(AX .LE. 25.D0) THEN
                     SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*FN1(NI,IG)*EXP(-
     1AX)
                  ENDIF
               ENDIF
               IF(ABS(FN1(NJ,IG)).GT.0.D0) THEN
                  AX = FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2
                  IF(AX .LE. 25.D0) THEN
                     SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*FN1(NJ,IG)*EXP(-
     1AX)
                  ENDIF
               ENDIF
   40       CONTINUE
         ENDIF
         ENUC=ENUC+SCALE
C
         IF(NATORB(NI)*NATORB(NJ).EQ.0)KI=0
         KR=KR+KI
C
C
      ENDIF
      RETURN
      END
