      SUBROUTINE DIAT(NI,NJ,XI,XJ,DI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*
*   DIAT CALCULATES THE DI-ATOMIC OVERLAP INTEGRALS BETWEEN ATOMS
*        CENTERED AT XI AND XJ.
*
*   ON INPUT NI  = ATOMIC NUMBER OF THE FIRST ATOM.
*            NJ  = ATOMIC NUMBER OF THE SECOND ATOM.
*            XI  = CARTESIAN COORDINATES OF THE FIRST ATOM.
*            XJ  = CARTESIAN COORDINATES OF THE SECOND ATOM.
*
*  ON OUTPUT DI  = DIATOMIC OVERLAP, IN A 9 * 9 MATRIX. LAYOUT OF
*                  ATOMIC ORBITALS IN DI IS
*                  1   2   3   4   5            6     7       8     9
*                  S   PX  PY  PZ  D(X**2-Y**2) D(XZ) D(Z**2) D(YZ)D(XY)
*
*   LIMITATIONS:  IN THIS FORMULATION, NI AND NJ MUST BE LESS THAN 107
*         EXPONENTS ARE ASSUMED TO BE PRESENT IN COMMON BLOCK EXPONT.
*
************************************************************************
      COMMON /KEYWRD/KEYWRD
      CHARACTER*241 KEYWRD
      INTEGER A,PQ2,B,PQ1,AA,BB
      COMMON /NUMCAL/ NUMCAL
      SAVE NPQ, IVAL
      LOGICAL ANALYT
      COMMON /EXPONT/ EMUS(107),EMUP(107),EMUD(107)
      DIMENSION DI(9,9),S(3,3,3),UL1(3),UL2(3),C(3,5,5),NPQ(107)
     1          ,XI(3),XJ(3), SLIN(27), IVAL(3,5)
     2, C1(3,5), C2(3,5), C3(3,5), C4(3,5), C5(3,5)
     3, S1(3,3), S2(3,3), S3(3,3)
      EQUIVALENCE(SLIN(1),S(1,1,1))
      EQUIVALENCE (C1(1,1),C(1,1,1)), (C2(1,1),C(1,1,2)),
     1            (C3(1,1),C(1,1,3)), (C4(1,1),C(1,1,4)),
     2            (C5(1,1),C(1,1,5)), (S1(1,1),S(1,1,1)),
     3            (S2(1,1),S(1,1,2)), (S3(1,1),S(1,1,3))
      DATA NPQ/1,0, 2,2,2,2,2,2,2,0, 0,3,3,3,3,3,3,0, 0,4,4,4,4,4,4,4,
     14,4,4,4,4,4,4,4,4,0, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     2,32*6,15*0,3,5*0/
      DATA IVAL/1,0,9,1,3,8,1,4,7,1,2,6,0,0,5/
      DATA ICALCN/0/
      IF(ICALCN.NE.NUMCAL)THEN
         ANALYT=(INDEX(KEYWRD,'ANALYT').NE.0)
         ICALCN=NUMCAL
      ENDIF
      X1=XI(1)
      X2=XJ(1)
      Y1=XI(2)
      Y2=XJ(2)
      Z1=XI(3)
      Z2=XJ(3)
      PQ1=NPQ(NI)
      PQ2=NPQ(NJ)
      DO 20 I=1,9
         DO 10 J=1,9
            DI(I,J)=0.0D0
   10    CONTINUE
   20 CONTINUE
      CALL COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)
      IF(PQ1.EQ.0.OR.PQ2.EQ.0.OR.R.GE.10.D0) RETURN
      IF(R.LT.0.001)THEN
         RETURN
      ENDIF
      IA=MIN(PQ1,3)
      IB=MIN(PQ2,3)
      A=IA-1
      B=IB-1
      IF(ANALYT)THEN
         CALL GOVER(NI,NJ,XI,XJ,R,DI)
C#      WRITE(6,*)' OVERLAP FROM GOVER'
C#      WRITE(6,'(4F15.10)')SG
         RETURN
      ENDIF
      IF(NI.LT.18.AND.NJ.LT.18) THEN
         CALL DIAT2(NI,EMUS(NI),EMUP(NI),R,NJ,EMUS(NJ),EMUP(NJ),S)
      ELSE
         UL1(1)=EMUS(NI)
         UL2(1)=EMUS(NJ)
         UL1(2)=EMUP(NI)
         UL2(2)=EMUP(NJ)
         UL1(3)=MAX(EMUD(NI),0.3D0)
         UL2(3)=MAX(EMUD(NJ),0.3D0)
         DO 30 I=1,27
   30    SLIN(I)=0.0D0
         NEWK=MIN(A,B)
         NK1=NEWK+1
         DO 40 I=1,IA
            ISS=I
            IB=B+1
            DO 40 J=1,IB
               JSS=J
               DO 40 K=1,NK1
                  IF(K.GT.I.OR.K.GT.J) GOTO 40
                  KSS=K
                  S(I,J,K)=SS(PQ1,PQ2,ISS,JSS,KSS,UL1(I),UL2(J),R)
   40    CONTINUE
      ENDIF
      DO 50 I=1,IA
         KMIN=4-I
         KMAX=2+I
         DO 50 J=1,IB
            IF(J.EQ.2)THEN
               AA=-1
               BB=1
            ELSE
               AA=1
               IF(J.EQ.3) THEN
                  BB=-1
               ELSE
                  BB=1
               ENDIF
            ENDIF
            LMIN=4-J
            LMAX=2+J
            DO 50 K=KMIN,KMAX
               DO 50 L=LMIN,LMAX
                  II=IVAL(I,K)
                  JJ=IVAL(J,L)
                  DI(II,JJ)=S1(I,J)*(C3(I,K)*C3(J,L))*AA+
     1(C4(I,K)*C4(J,L)+C2(I,K)*C2(J,L))*BB*S2(I,J)+(C5(I,K)*C5(J,L)
     2+C1(I,K)*C1(J,L))*S3(I,J)
   50 CONTINUE
C#      WRITE(6,*)' OVERLAP FROM DIAT2'
C#      DO 12 I=1,4
C#  12  WRITE(6,'(4F15.10)')(DI(J,I),J=1,4)
      RETURN
      END
      DOUBLE PRECISION FUNCTION SS(NA,NB,LA1,LB1,M1,UA,UB,R1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FIRST
      DIMENSION FA(0:13),AFF(0:2,0:2,0:2),AF(0:19),BF(0:19),
     1BI(0:12,0:12)
      SAVE AFF, FA, BI, FIRST
      DATA FIRST /.TRUE./
      DATA AFF/27*0. D0/
      DATA FA/1.D0,1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0,
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0/
      M=M1-1
      LB=LB1-1
      LA=LA1-1
      R=R1/0.529167D0
      IF(FIRST) THEN
         FIRST=.FALSE.
C
C           INITIALISE SOME CONSTANTS
C
C                  BINOMIALS
C
         DO 10 I=0,12
            BI(I,0)=1.D0
            BI(I,I)=1.D0
   10    CONTINUE
         DO 20 I=0,11
            I1=I-1
            DO 20 J=0,I1
               BI(I+1,J+1)=BI(I,J+1)+BI(I,J)
   20    CONTINUE
         AFF(0,0,0)=1.D0
         AFF(1,0,0)=1.D0
         AFF(1,1,0)=1.D0
         AFF(2,0,0)=1.5D0
         AFF(2,1,0)=1.73205D0
         AFF(2,2,0)=1.224745D0
         AFF(2,0,2)=-0.5D0
      ENDIF
      P=(UA+UB)*R*0.5D0
      B=(UA-UB)*R*0.5D0
      QUO=1/P
      AF(0)=QUO*EXP(-P)
      DO 30 N=1,19
         AF(N)=N*QUO*AF(N-1)+AF(0)
   30 CONTINUE
      CALL BFN(B,BF)
      SUM=0.D0
      LAM1=LA-M
      LBM1=LB-M
C
C          START OF OVERLAP CALCULATION PROPER
C
      DO 50 I=0,LAM1,2
         IA=NA+I-LA
         IC=LA-I-M
         DO 50 J=0,LBM1,2
            IB=NB+J-LB
            ID=LB-J-M
            SUM1=0.D0
            IAB=IA+IB
            DO 40 K1=0,IA
               DO 40 K2=0,IB
                  DO 40 K3=0,IC
                     DO 40 K4=0,ID
                        DO 40 K5=0,M
                           IAF=IAB-K1-K2+K3+K4+2*K5
                           DO 40 K6=0,M
                              IBF=K1+K2+K3+K4+2*K6
                              JX=(-1)**(M+K2+K4+K5+K6)
                              SUM1=SUM1+BI(ID,K4)*
     1BI(M,K5)*BI(IC,K3)*BI(IB,K2)*BI(IA,K1)*
     2BI(M,K6)*JX*AF(IAF)*BF(IBF)
   40       CONTINUE
            SUM=SUM+SUM1*AFF(LA,M,I)*AFF(LB,M,J)
   50 CONTINUE
      SS=SUM*R**(NA+NB+1)*UA**NA*UB**NB/(2.D0**(M+1))*
     1SQRT(UA*UB/(FA(NA+NA)*FA(NB+NB))*((LA+LA+1)*(LB+LB+1)))
      RETURN
      END
      SUBROUTINE COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER PQ1,PQ2,PQ
      DIMENSION C(75)
      XY=(X2-X1)**2+(Y2-Y1)**2
      R=SQRT(XY+(Z2-Z1)**2)
      XY=SQRT(XY)
      IF (XY.LT.1.D-10) GO TO 10
      CA=(X2-X1)/XY
      CB=(Z2-Z1)/R
      SA=(Y2-Y1)/XY
      SB=XY/R
      GO TO 50
   10 IF (Z2-Z1) 20,30,40
   20 CA=-1.D0
      CB=-1.D0
      SA=0.D0
      SB=0.D0
      GO TO 50
   30 CA=0.D0
      CB=0.D0
      SA=0.D0
      SB=0.D0
      GO TO 50
   40 CA=1.D0
      CB=1.D0
      SA=0.D0
      SB=0.D0
   50 CONTINUE
      DO 60 I=1,75
   60 C(I)=0.D0
      IF (PQ1.GT.PQ2) GO TO 70
      PQ=PQ2
      GO TO 80
   70 PQ=PQ1
   80 CONTINUE
      C(37)=1.D0
      IF (PQ.LT.2) GO TO 90
      C(56)=CA*CB
      C(41)=CA*SB
      C(26)=-SA
      C(53)=-SB
      C(38)=CB
      C(23)=0.D0
      C(50)=SA*CB
      C(35)=SA*SB
      C(20)=CA
      IF (PQ.LT.3) GO TO 90
      C2A=2*CA*CA-1.D0
      C2B=2*CB*CB-1.D0
      S2A=2*SA*CA
      S2B=2*SB*CB
      C(75)=C2A*CB*CB+0.5D0*C2A*SB*SB
      C(60)=0.5D0*C2A*S2B
      C(45)=0.8660254037841D0*C2A*SB*SB
      C(30)=-S2A*SB
      C(15)=-S2A*CB
      C(72)=-0.5D0*CA*S2B
      C(57)=CA*C2B
      C(42)=0.8660254037841D0*CA*S2B
      C(27)=-SA*CB
      C(12)=SA*SB
      C(69)=0.5773502691894D0*SB*SB*1.5D0
      C(54)=-0.8660254037841D0*S2B
      C(39)=CB*CB-0.5D0*SB*SB
      C(66)=-0.5D0*SA*S2B
      C(51)=SA*C2B
      C(36)=0.8660254037841D0*SA*S2B
      C(21)=CA*CB
      C(6)=-CA*SB
      C(63)=S2A*CB*CB+0.5D0*S2A*SB*SB
      C(48)=0.5D0*S2A*S2B
      C(33)=0.8660254037841D0*S2A*SB*SB
      C(18)=C2A*SB
      C(3)=C2A*CB
   90 CONTINUE
      RETURN
      END
      SUBROUTINE BFN(X,BF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION BF(13)
C**********************************************************************
C
C     BINTGS FORMS THE "B" INTEGRALS FOR THE OVERLAP CALCULATION.
C
C**********************************************************************
      DIMENSION FACT(17)
      SAVE FACT
      DATA FACT/1.D0,2.D0,6.D0,24.D0,120.D0,720.D0,5040.D0,40320.D0,
     1362880.D0,3628800.D0,39916800.D0,479001600.D0,6227020800.D0,
     28.71782912D10,1.307674368D12,2.092278989D13,3.556874281D14/
      K=12
      IO=0
      ABSX = ABS(X)
      IF (ABSX.GT.3.D00) GO TO 40
      IF (ABSX.LE.2.D00) GO TO 10
      LAST=15
      GO TO 60
   10 IF (ABSX.LE.1.D00) GO TO 20
      LAST=12
      GO TO 60
   20 IF (ABSX.LE.0.5D00) GO TO 30
      LAST=7
      GO TO 60
   30 IF (ABSX.LE.1.D-6) GOTO 90
      LAST=6
      GO TO 60
   40 EXPX=EXP(X)
      EXPMX=1.D00/EXPX
      BF(1)=(EXPX-EXPMX)/X
      DO 50 I=1,K
   50 BF(I+1)=(I*BF(I)+(-1.D00)**I*EXPX-EXPMX)/X
      GO TO 110
   60 DO 80 I=IO,K
         Y=0.0D00
         DO 70 M=IO,LAST
            XF=1.0D00
            IF(M.NE.0) XF=FACT(M)
   70    Y=Y+(-X)**M*(2*MOD(M+I+1,2))/(XF*(M+I+1))
   80 BF(I+1)=Y
      GO TO 110
   90 DO 100 I=IO,K
  100 BF(I+1)=(2*MOD(I+1,2))/(I+1.D0)
  110 CONTINUE
      RETURN
C
      END
