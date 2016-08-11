      SUBROUTINE FORMXY(W,KR,WCA,WCB,CA,NA,CB,NB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION W(100), WCA(NA), WCB(NB), CA(NA), CB(NB)
C***********************************************************************
C
C    EACH OF THE NA ELEMENTS OF WCA WILL ADD ON THE NB ELECTROSTATIC
C    TERMS FROM ATOM B IN CB
C
C    EACH OF THE NB ELEMENTS OF WCB WILL ADD ON THE NA ELECTROSTATIC
C    TERMS FROM ATOM A IN CA
C
C    BOTH SUMS WILL INVOLVE THE NA*NB TERMS IN ARRAY W.  ONCE USED,
C    W WILL BE INCREMENTED BY NA*NB.
C
C NA=1 IF ATOM 'A' IS A HYDROGEN, NA=10 IF ATOM 'A' IS NOT A HYDROGEN
C NB=1 IF ATOM 'B' IS A HYDROGEN, NB=10 IF ATOM 'B' IS NOT A HYDROGEN
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      LOGICAL MINDO3
      CHARACTER KEYWRD*241
      DATA ICALCN/0/
      IF(ICALCN.NE.NUMCAL)THEN
         ICALCN=NUMCAL
         MINDO3=(INDEX(KEYWRD,'MINDO').NE.0)
      ENDIF
      IF(MINDO3)THEN
         W1=W(1)*0.25D0
C
C   CALCULATE THE TOTAL NUMBER OF ELECTRONS ON ATOMS A AND B
C
         PA=CA(1)
         PB=CB(1)
         IF(NA.GT.1.AND.NB.GT.1)THEN
            PWA=(PA+CA(3)+CA(6)+CA(10))*W1
            PWB=(PB+CB(3)+CB(6)+CB(10))*W1
            WCA(1)=WCA(1)+PWB
            WCA(3)=WCA(3)+PWB
            WCA(6)=WCA(6)+PWB
            WCA(10)=WCA(10)+PWB
            WCB(1)=WCB(1)+PWA
            WCB(3)=WCB(3)+PWA
            WCB(6)=WCB(6)+PWA
            WCB(10)=WCB(10)+PWA
         ELSEIF(NA.GT.1)THEN
            PWB=PB*W1
            WCA(1)=WCA(1)+PWB
            WCA(3)=WCA(3)+PWB
            WCA(6)=WCA(6)+PWB
            WCA(10)=WCA(10)+PWB
            WCB(1)=WCB(1)+(PA+CA(3)+CA(6)+CA(10))*W1
         ELSEIF(NB.GT.1)THEN
            PWA=PA*W1
            WCA(1)=WCA(1)+(PB+CB(3)+CB(6)+CB(10))*W1
            WCB(1)=WCB(1)+PWA
            WCB(3)=WCB(3)+PWA
            WCB(6)=WCB(6)+PWA
            WCB(10)=WCB(10)+PWA
         ELSE
            WCA(1)=WCA(1)+PB*W1
            WCB(1)=WCB(1)+PA*W1
         ENDIF
         KR=KR+1
         RETURN
      ENDIF
      IF(NA.GT.1.AND.NB.GT.1)THEN
C
C   BOTH ATOMS 'A' AND 'B' ARE HEAVY ATOMS
C
C
C  THIS COMMENTED-OUT CODE RUNS SLOWER ON A SCALAR MACHINE THAN THE
C  EXPLICIT CODE ACTUALLY USED HERE.  THE CODE COMMENTED OUT IS PROVIDED
C  FOR USERS WHO WANT TO VECTORIZE THE CODE
C
C#         IJP=0
C#         DO 3 II=1,4
C#         DO 4 JJ=1,II
C#         IJ=IJP*10
C#         IJP=IJP+1
C#         KL=-10+IJP
C#         SUM1=0.D0
C#         SUM2=0.D0
C#         JI=0
C#         DO 10 I=1,4
C#            DO 10 J=1,I
C#               JI=JI+1
C#               IJ=IJ+1
C#               KL=KL+10
C#               FACT=1.D0
C#               IF(I.EQ.J)FACT=0.5D0
C#               SUM1=SUM1+CB(JI)*W(IJ)*FACT
C#   10    SUM2=SUM2+CA(JI)*W(KL)*FACT
C#      IF(II.EQ.JJ)THEN
C#      SUM1=SUM1*0.5D0
C#      SUM2=SUM2*0.5D0
C#      ENDIF
C#      WCA(IJP)=WCA(IJP)+SUM1
C#      WCB(IJP)=WCB(IJP)+SUM2
C#   4  CONTINUE
C#   3  CONTINUE
C
C   START OF EXPLICIT CODE
C
         WCA(1)=WCA(1)+
     10.25D0*(CB(1)*W(1) + CB(3)*W(3) + CB(6)*W(6) + CB(10)*W(10)) +
     20.5D0*( CB(2)*W(2) + CB(4)*W(4) + CB(5)*W(5) +
     3        CB(7)*W(7) + CB(8)*W(8) + CB(9)*W(9))
         WCA(2)=WCA(2)+
     10.5D0*(CB(1)*W(11) + CB(3)*W(13) + CB(6)*W(16) + CB(10)*W(20)) +
     2CB(2)*W(12) + CB(4)*W(14) + CB(5)*W(15) +
     3CB(7)*W(17) + CB(8)*W(18) + CB(9)*W(19)
         WCA(3)=WCA(3)+
     10.25D0*(CB(1)*W(21) + CB(3)*W(23) + CB(6)*W(26) + CB(10)*W(30)) +
     20.5D0*(CB(2)*W(22) + CB(4)*W(24) + CB(5)*W(25) +
     3       CB(7)*W(27) + CB(8)*W(28) + CB(9)*W(29))
         WCA(4)=WCA(4)+
     10.5D0*(CB(1)*W(31) + CB(3)*W(33) + CB(6)*W(36) + CB(10)*W(40)) +
     2CB(2)*W(32) + CB(4)*W(34) + CB(5)*W(35) +
     3CB(7)*W(37) + CB(8)*W(38) + CB(9)*W(39)
         WCA(5)=WCA(5)+
     10.5D0*(CB(1)*W(41) + CB(3)*W(43) + CB(6)*W(46) + CB(10)*W(50)) +
     2CB(2)*W(42) + CB(4)*W(44) + CB(5)*W(45) +
     3CB(7)*W(47) + CB(8)*W(48) + CB(9)*W(49)
         WCA(6)=WCA(6)+
     10.25D0*(CB(1)*W(51) + CB(3)*W(53) + CB(6)*W(56) + CB(10)*W(60)) +
     20.5D0*(CB(2)*W(52) + CB(4)*W(54) + CB(5)*W(55) +
     3       CB(7)*W(57) + CB(8)*W(58) + CB(9)*W(59))
         WCA(7)=WCA(7)+
     10.5D0*(CB(1)*W(61) + CB(3)*W(63) + CB(6)*W(66) + CB(10)*W(70)) +
     2CB(2)*W(62) + CB(4)*W(64) + CB(5)*W(65) +
     3CB(7)*W(67) + CB(8)*W(68) + CB(9)*W(69)
         WCA(8)=WCA(8)+
     10.5D0*(CB(1)*W(71) + CB(3)*W(73) + CB(6)*W(76) + CB(10)*W(80)) +
     2CB(2)*W(72) + CB(4)*W(74) + CB(5)*W(75) +
     3CB(7)*W(77) + CB(8)*W(78) + CB(9)*W(79)
         WCA(9)=WCA(9)+
     10.5D0*(CB(1)*W(81) + CB(3)*W(83) + CB(6)*W(86) + CB(10)*W(90)) +
     2CB(2)*W(82) + CB(4)*W(84) + CB(5)*W(85) +
     3CB(7)*W(87) + CB(8)*W(88) + CB(9)*W(89)
         WCA(10)=WCA(10)+
     10.25D0*(CB(1)*W(91) + CB(3)*W(93) + CB(6)*W(96) + CB(10)*W(100)) +
     20.5D0*(CB(2)*W(92) + CB(4)*W(94) + CB(5)*W(95) +
     3       CB(7)*W(97) + CB(8)*W(98) + CB(9)*W(99))
         WCB(1)=WCB(1)+
     10.25D0*(CA(1)*W(1) + CA(3)*W(21) + CA(6)*W(51) + CA(10)*W(91)) +
     20.5D0*(CA(2)*W(11) + CA(4)*W(31) + CA(5)*W(41) +
     3       CA(7)*W(61) + CA(8)*W(71) + CA(9)*W(81))
         WCB(2)=WCB(2)+
     10.5D0*(CA(1)*W(2) + CA(3)*W(22) + CA(6)*W(52) + CA(10)*W(92)) +
     2CA(2)*W(12) + CA(4)*W(32) + CA(5)*W(42) +
     3CA(7)*W(62) + CA(8)*W(72) + CA(9)*W(82)
         WCB(3)=WCB(3)+
     10.25D0*(CA(1)*W(3) + CA(3)*W(23) + CA(6)*W(53) + CA(10)*W(93)) +
     20.5D0*(CA(2)*W(13) + CA(4)*W(33) + CA(5)*W(43) +
     3       CA(7)*W(63) + CA(8)*W(73) + CA(9)*W(83))
         WCB(4)=WCB(4)+
     10.5D0*(CA(1)*W(4) + CA(3)*W(24) + CA(6)*W(54) + CA(10)*W(94)) +
     2CA(2)*W(14) + CA(4)*W(34) + CA(5)*W(44) +
     3CA(7)*W(64) + CA(8)*W(74) + CA(9)*W(84)
         WCB(5)=WCB(5)+
     10.5D0*(CA(1)*W(5) + CA(3)*W(25) + CA(6)*W(55) + CA(10)*W(95)) +
     2CA(2)*W(15) + CA(4)*W(35) + CA(5)*W(45) +
     3CA(7)*W(65) + CA(8)*W(75) + CA(9)*W(85)
         WCB(6)=WCB(6)+
     10.25D0*(CA(1)*W(6) + CA(3)*W(26) + CA(6)*W(56) + CA(10)*W(96)) +
     20.5D0*(CA(2)*W(16) + CA(4)*W(36) + CA(5)*W(46) +
     3       CA(7)*W(66) + CA(8)*W(76) + CA(9)*W(86))
         WCB(7)=WCB(7)+
     10.5D0*(CA(1)*W(7) + CA(3)*W(27) + CA(6)*W(57) + CA(10)*W(97)) +
     2CA(2)*W(17) + CA(4)*W(37) + CA(5)*W(47) +
     3CA(7)*W(67) + CA(8)*W(77) + CA(9)*W(87)
         WCB(8)=WCB(8)+
     10.5D0*(CA(1)*W(8) + CA(3)*W(28) + CA(6)*W(58) + CA(10)*W(98)) +
     2CA(2)*W(18) + CA(4)*W(38) + CA(5)*W(48) +
     3CA(7)*W(68) + CA(8)*W(78) + CA(9)*W(88)
         WCB(9)=WCB(9)+
     10.5D0*(CA(1)*W(9) + CA(3)*W(29) + CA(6)*W(59) + CA(10)*W(99)) +
     2CA(2)*W(19) + CA(4)*W(39) + CA(5)*W(49) +
     3CA(7)*W(69) + CA(8)*W(79) + CA(9)*W(89)
         WCB(10)=WCB(10)+
     10.25D0*(CA(1)*W(10) + CA(3)*W(30) + CA(6)*W(60) + CA(10)*W(100)) +
     20.5D0*(CA(2)*W(20) + CA(4)*W(40) + CA(5)*W(50) +
     3       CA(7)*W(70) + CA(8)*W(80) + CA(9)*W(90))
C
C   END OF EXPLICIT CODE
C
      ELSEIF(NA.GT.1)THEN
C
C   ATOM 'A' IS NOT A HYDROGEN, ATOM 'B' IS A HYDROGEN
C
         SUM=0.D0
         IJ=0
         DO 20 I=1,4
            DO 10 J=1,I-1
               IJ=IJ+1
               SUM=SUM+CA(IJ)*W(IJ)
   10       WCA(IJ)=WCA(IJ)+CB(1)*W(IJ)*0.5D0
            IJ=IJ+1
            SUM=SUM+CA(IJ)*W(IJ)*0.5D0
   20    WCA(IJ)=WCA(IJ)+CB(1)*W(IJ)*0.25D0
         WCB(1) =WCB(1)+SUM*0.5D0
      ELSEIF(NB.GT.1)THEN
C
         SUM=0.D0
         IJ=0
         DO 40 I=1,4
            DO 30 J=1,I-1
               IJ=IJ+1
               SUM=SUM+CB(IJ)*W(IJ)
   30       WCB(IJ)=WCB(IJ)+CA(1)*W(IJ)*0.5D0
            IJ=IJ+1
            SUM=SUM+CB(IJ)*W(IJ)*0.5D0
   40    WCB(IJ)=WCB(IJ)+CA(1)*W(IJ)*0.25D0
         WCA(1) =WCA(1)+SUM*0.5D0
      ELSEIF(NB.GT.1)THEN
C
C   ATOM 'A' IS A HYDROGEN, ATOM 'B' IS NOT A HYDROGEN
C
      ELSE
C
C   BOTH ATOMS 'A' AND 'B' ARE HYDROGENS
C
         WCB(1)=WCB(1)+CA(1)*W(1)*0.25D0
         WCA(1)=WCA(1)+CB(1)*W(1)*0.25D0
      ENDIF
      KR=KR+NA*NB
      RETURN
      END
