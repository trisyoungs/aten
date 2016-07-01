      SUBROUTINE ANALYT(PSUM,PALPHA,PBETA,COORD,NAT,JJA,JJD,
     1IIA,IID,ENG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*),ENG(3), PSUM(*), PALPHA(*), PBETA(*),NAT(*)
************************************************************************
*                                                                      *
*         CALCULATION OF ANALYTICAL DERIVATIVES                        *
*                                                                      *
************************************************************************
C
C COMMON BLOCKS 'OWNED' BY REST OF PROGRAM.
C
      COMMON /CORE  / CORE(107)
     1       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     2       /EXPONT/ ZS(107),ZP(107),ZD(107)
     3       /ALPHA / ALPA(107)
      COMMON /TWOEL3/ F03(107)
      COMMON /NATORB/ NATORB(107)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10)
      COMMON /WMATRX/ W(N2ELEC*2)
      COMMON /NATYPE/ NZTYPE(107),MTYPE(30),LTYPE
      COMMON /BETA3 / BETA3(153)
      COMMON /VSIPS / VS(107),VP(107),VD(107)
      COMMON /KEYWRD/ KEYWRD
C
C COMMON BLOCKS 'OWNED' BY ANT
C
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22), TXYZ(9)
C
C ON RETURN, ENG HOLDS ANALYTICAL DERIVATIVES
C
      COMMON /FORCE3/ IDMY(5),I3N,IX
      COMMON /NUMCAL/ NUMCAL
      DIMENSION EAA(3),EAB(3),ENUC(3), BI(4), BJ(4)
      CHARACTER*241 KEYWRD
      SAVE AM1, MINDO3
      LOGICAL AM1, MINDO3
      DATA ICALCN/0/
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         AM1=(INDEX(KEYWRD,'AM1')+INDEX(KEYWRD,'PM3').NE.0)
         MINDO3=(INDEX(KEYWRD,'MINDO').NE.0)
      ENDIF
      A0=0.529167D0
      JD=JJD-JJA+1
      JA=1
      ID=IID-IIA+1+JD
      IA=JD+1
      DO 10 J=1,3
         EAA(J)=0.0D0
         EAB(J)=0.0D0
         ENUC(J)=0.0D0
         ENG(J)=0.0D0
   10 CONTINUE
      I=2
      NI=NAT(I)
      ISTART=NZTYPE(NI)*4-3
      J=1
      NJ=NAT(J)
      JSTART=NZTYPE(NJ)*4-3
      R2=(COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     1   + (COORD(3,I)-COORD(3,J))**2
      RIJ=SQRT(R2)
      R0=RIJ/A0
      RR=R2/(A0*A0)
      DO 150 IX=1,3
         DEL1=COORD(IX,I)-COORD(IX,J)
         TERMAA=0.0D0
         TERMAB=0.0D0
         ISP=0
         IOL=0
C   THE FIRST DERIVATIVES OF OVERLAP INTEGRALS
         DO 30 K=IA,ID
            KA=K-IA
            KG=ISTART+KA
            DO 30 L=JA,JD
               LA=L-JA
               LG=JSTART+LA
               IOL=IOL+1
               DS(IOL)=0.0D0
               IF(KA.EQ.0.AND.LA.EQ.0) THEN
C   (S/S) TERM
                  IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                  IS=1
               ELSEIF(KA.EQ.0.AND.LA.GT.0) THEN
C   (S/P) TERM
                  IS=3
                  IF(IX.EQ.LA) GO TO 20
                  IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                  IS=2
                  DEL2=COORD(LA,I)-COORD(LA,J)
               ELSEIF(KA.GT.0.AND.LA.EQ.0) THEN
C   (P/S) TERM
                  IS=5
                  IF(IX.EQ.KA) GO TO 20
                  IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                  IS=4
                  DEL2=COORD(KA,I)-COORD(KA,J)
               ELSE
C   (P/P) TERM
                  IF(KA.EQ.LA) THEN
C    P/P
                     IS=9
                     IF(IX.EQ.KA) GO TO 20
                     IF(ABS(DEL1).LE.1.0D-6) GO TO 30
C    P'/P'
                     IS=8
                     DEL2=COORD(KA,I)-COORD(KA,J)
                  ELSEIF(IX.NE.KA.AND.IX.NE.LA) THEN
C    P'/P"
                     IF(ABS(DEL1).LE.1.0D-6) GO TO 30
                     IS=7
                     DEL2=COORD(KA,I)-COORD(KA,J)
                     DEL3=COORD(LA,I)-COORD(LA,J)
                  ELSE
C    P/P' OR P'/P
                     DEL2=COORD(KA+LA-IX,I)-COORD(KA+LA-IX,J)
                     IS=6
                  ENDIF
               ENDIF
C
C        CALCULATE OVERLAP DERIVATIVES, STORE RESULTS IN DS
C
   20          CALL DERS(KG,LG,RR,DEL1,DEL2,DEL3,IS,IOL)
   30    CONTINUE
         IF(.NOT.MINDO3.AND.IX.EQ.1) READ (2) (G(I22),I22=1,22)
         IF(.NOT.MINDO3) CALL DELRI(DG,NI,NJ,R0,DEL1)
         CALL DELMOL(COORD,I,J,NI,NJ,IA,ID,JA,JD,IX,RIJ,DEL1,ISP)
C
C   THE FIRST DERIVATIVE OF NUCLEAR REPULSION TERM
         IF(MINDO3)THEN
            II=MAX(NI,NJ)
            NBOND=(II*(II-1))/2+NI+NJ-II
            ALPHA=0
            IF(NBOND.LT.154)THEN
               ALPHA=ALP3(NBOND)
            ELSE
               ALPH1=100
               ALPH2=100
               IF(NATORB(NI).EQ.0)ALPH1=ALPA(NI)
               IF(NATORB(NJ).EQ.0)ALPH2=ALPA(NJ)
            ENDIF
            C2=(7.1995D0/F03(NI)+7.1995D0/F03(NJ))**2
            C1=DEL1/RIJ*CORE(NI)*CORE(NJ)*14.399D0
            C3=DEL1/RIJ*ABS(CORE(NI)*CORE(NJ))*14.399D0
            IF(NBOND.EQ.22.OR.NBOND.EQ.29)THEN
               TERMNC=-C1*ALPHA*(1.D0/RIJ**2 - RIJ*(RIJ**2+C2)**(-1.5D0)
     1 +  1.D0/RIJ - 1.D0/SQRT(RIJ**2+C2)) * EXP(-RIJ) -
     2C1*RIJ*(RIJ**2+C2)**(-1.5D0)
            ELSEIF (RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
               TERMNC=0.D0
            ELSEIF(NBOND.GE.154) THEN
C
C  SPECIAL CASE INVOLVING SPARKLES
C
               EXP1=EXP(-MIN(ALPH1*RIJ,20.D0))
               EXP2=EXP(-MIN(ALPH2*RIJ,20.D0))
               PART1=-C3*(1.D0/RIJ**2 - RIJ*(RIJ**2+C2)**(-1.5D0))
     1*(EXP1+EXP2)
               PART2=-C3*(1.D0/RIJ -1.D0/SQRT(RIJ**2+C2))
     1*(ALPH1*EXP1 + ALPH2*EXP2)
               PART3=-C1*RIJ*(RIJ**2+C2)**(-1.5D0)
               TERMNC=PART1+PART2+PART3
C#            WRITE(6,'(4F13.6)')PART1,PART2,PART3,TERMNC
            ELSE
               TERMNC=-C1*(1.D0/RIJ**2 - RIJ*(RIJ**2+C2)**(-1.5D0) +
     1ALPHA/RIJ - ALPHA/SQRT(RIJ**2+C2)) * EXP(-ALPHA*RIJ) -
     2C1*RIJ*(RIJ**2+C2)**(-1.5D0)
            ENDIF
            DR1=DEL1/RIJ*14.399D0*RIJ*(RIJ**2+C2)**(-1.5D0)
         ELSE
C
C      CORE-CORE TERMS, MNDO AND AM1
C
C
C  SPECIAL TREATMENT FOR N-H AND O-H TERMS
C
            IF(RIJ.LT.1.D0.AND.NATORB(NI)*NATORB(NJ).EQ.0)THEN
               TERMNC=0.D0
               GOTO 50
            ENDIF
            C1=CORE(NI)*CORE(NJ)
            IF(NI.EQ.1.AND.(NJ.EQ.7.OR.NJ.EQ.8)) THEN
               F3=1.0D0+EXP(-ALPA(1)*RIJ)+RIJ*EXP(-ALPA(NJ)*RIJ)
               DD=(DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(1)*EXP(-ALPA(1)*RIJ)
     1 +(ALPA(NJ)*RIJ-1.0D0)*EXP(-ALPA(NJ)*RIJ)))*C1
            ELSEIF((NI.EQ.7.OR.NI.EQ.8).AND.NJ.EQ.1) THEN
               F3=1.0D0+EXP(-ALPA(1)*RIJ)+RIJ*EXP(-ALPA(NI)*RIJ)
               DD=(DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(1)*EXP(-ALPA(1)*RIJ)
     1 +(ALPA(NI)*RIJ-1.0D0)*EXP(-ALPA(NI)*RIJ)))*C1
            ELSE
C#            ELSEIF(NATORB(NI)+NATORB(NJ).EQ.0) THEN
C
C  SPECIAL CASE OF TWO SPARKLES
C
               PART1=DG(1)*C1
               PART2=-(G(1)*(DEL1/RIJ)*(ALPA(NI)*EXP(-ALPA(NI)*RI
     1J) +ALPA(NJ)*EXP(-ALPA(NJ)*RIJ)))*ABS(C1)
               PART3=DG(1)*(EXP(-ALPA(NI)*RIJ)+EXP(-ALPA(NJ)*RIJ))*ABS(C
     11)
               DD=PART1+PART2+PART3
C#            WRITE(6,'(4F13.6)')PART1,PART2,PART3,DD
C#            ELSE
C
C   THE GENERAL CASE
C
C#               F3=1.0D0+EXP(-ALPA(NI)*RIJ)+EXP(-ALPA(NJ)*RIJ)
C#               DD=(DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(NI)*EXP(-ALPA(NI)*RI
C#     1J) +ALPA(NJ)*EXP(-ALPA(NJ)*RIJ)))*C1
            ENDIF
            TERMNC=DD
         ENDIF
C
C   ****   START OF THE AM1 SPECIFIC DERIVATIVE CODE   ***
C
C      ANALYT=-A*(1/(R*R)+2.D0*B*(R-C)/R)*EXP(-B*(R-C)**2)
C
C    ANALYTICAL DERIVATIVES
C
         IF( AM1 )THEN
            ANAM1=0.D0
            DO 40 IG=1,10
               IF(ABS(FN1(NI,IG)).GT.0.D0)
     1ANAM1=ANAM1+FN1(NI,IG)*
     2(1.D0/(RIJ*RIJ)+2.D0*FN2(NI,IG)*(RIJ-FN3(NI,IG))/RIJ)*
     3EXP(MAX(-30.D0,-FN2(NI,IG)*(RIJ-FN3(NI,IG))**2))
               IF(ABS(FN1(NJ,IG)).GT.0.D0)
     1ANAM1=ANAM1+FN1(NJ,IG)*
     2(1.D0/(RIJ*RIJ)+2.D0*FN2(NJ,IG)*(RIJ-FN3(NJ,IG))/RIJ)*
     3EXP(MAX(-30.D0,-FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2))
   40       CONTINUE
            ANAM1=ANAM1*CORE(NI)*CORE(NJ)
            TERMNC=TERMNC-ANAM1*DEL1/RIJ
         ENDIF
C
C   ****   END OF THE AM1 SPECIFIC DERIVATIVE CODE   ***
C
   50    CONTINUE
C
C   COMBINE TOGETHER THE OVERLAP DERIVATIVE PARTS
C
         IF(MINDO3)THEN
            II=MAX(NI,NJ)
            NBOND=(II*(II-1))/2+NI+NJ-II
            IF(NBOND.GT.153)GOTO 60
            BI(1)=BETA3(NBOND)*VS(NI)*2.D0
            BI(2)=BETA3(NBOND)*VP(NI)*2.D0
            BI(3)=BI(2)
            BI(4)=BI(2)
            BJ(1)=BETA3(NBOND)*VS(NJ)*2.D0
            BJ(2)=BETA3(NBOND)*VP(NJ)*2.D0
            BJ(3)=BJ(2)
            BJ(4)=BJ(2)
   60       CONTINUE
         ELSE
            BI(1)=BETAS(NI)
            BI(2)=BETAP(NI)
            BI(3)=BI(2)
            BI(4)=BI(2)
            BJ(1)=BETAS(NJ)
            BJ(2)=BETAP(NJ)
            BJ(3)=BJ(2)
            BJ(4)=BJ(2)
         ENDIF
C
C       CODE COMMON TO MINDO/3, MNDO, AND AM1
C
         IOL=0
         DO 70 K=IA,ID
            DO 70 L=JA,JD
               LK=L+K*(K-1)/2
               TERMK=BI(K-IA+1)
               TERML=BJ(L-JA+1)
               IOL=IOL+1
               TERMAB=TERMAB+(TERMK+TERML)
     1*PSUM(LK)*DS(IOL)
   70    CONTINUE
         IF(MINDO3)THEN
C
C        FIRST, CORE-ELECTRON ATTRACTION DERIVATIVES (MINDO/3)
C
C          ATOM CORE I AFFECTING A.O.S ON J
            DO 80 M=JA,JD
               MN=(M*(M+1))/2
   80       TERMAB=TERMAB+CORE(NI)*PSUM(MN)*DR1
C          ATOM CORE J AFFECTING A.O.S ON I
            DO 90 M=IA,ID
               MN=(M*(M+1))/2
   90       TERMAB=TERMAB+CORE(NJ)*PSUM(MN)*DR1
C
C   NOW FOR COULOMB AND EXCHANGE TERMS (MINDO/3)
C
            DO 100 I1=IA,ID
               II=(I1*(I1+1))/2
               DO 100 J1=JA,JD
                  JJ=(J1*(J1+1))/2
                  IJ=J1+(I1*(I1-1))/2
C
C           COULOMB TERM
C
                  TERMAA=TERMAA-PSUM(II)*DR1*PSUM(JJ)
C
C           EXCHANGE TERM
C
                  TERMAA=TERMAA+(PALPHA(IJ)*PALPHA(IJ)+PBETA(IJ)*PBETA(I
     1J))*DR1
  100       CONTINUE
         ELSE
C
C        FIRST, CORE-ELECTRON ATTRACTION DERIVATIVES (MNDO AND AM1)
C
C          ATOM CORE I AFFECTING A.O.S ON J
            ISP=0
            DO 110 M=JA,JD
               BB=1.D0
               DO 110 N=M,JD
                  MN=M+N*(N-1)/2
                  ISP=ISP+1
                  TERMAB=TERMAB-BB*CORE(NI)*PSUM(MN)*DR(ISP)
  110       BB=2.D0
C          ATOM CORE J AFFECTING A.O.S ON I
            K=MAX(JD-JA+1,1)
            K=(K*(K+1))/2
            ISP=-K+1
            DO 120 M=IA,ID
               BB=1.D0
               DO 120 N=M,ID
                  MN=M+N*(N-1)/2
                  ISP=ISP+K
                  TERMAB=TERMAB-BB*CORE(NJ)*PSUM(MN)*DR(ISP)
  120       BB=2.D0
            ISP=0
C
C   NOW FOR COULOMB AND EXCHANGE TERMS (MNDO AND AM1)
C
            DO 140 K=IA,ID
               AA=1.D0
               KK=(K*(K-1))/2
               DO 140 L=K,ID
                  LL=(L*(L-1))/2
                  DO 130 M=JA,JD
                     BB=1.D0
                     DO 130 N=M,JD
                        ISP=ISP+1
                        KL=K+LL
                        MN=M+N*(N-1)/2
C
C    COULOMB TERM
C
                        TERMAA=TERMAA+AA*BB*PSUM(KL)*PSUM(MN)*DR(ISP)
                        MK=M+KK
                        NK=N+KK
                        ML=M+LL
                        NL=N+LL
C
C    EXCHANGE TERM
C
                        TERMAA= TERMAA-0.5D0*AA*BB*(PALPHA(M
     1K)*PALPHA(NL)+PALPHA(NK)*PALPHA(ML)+PBETA(MK)*PBETA(NL)+PBETA(NK)*
     2PBETA(ML))*DR(ISP)
  130             BB=2.D0
  140       AA=2.D0
C           END OF MNDO AND AM1 SPECIFIC CODE
         ENDIF
         EAA(IX)=EAA(IX)+TERMAA
         EAB(IX)=EAB(IX)+TERMAB
         ENUC(IX)=ENUC(IX)+TERMNC
  150 CONTINUE
C#            WRITE(6,*)EAA,EAB,ENUC,NAT(1),NAT(2)
  160 CONTINUE
  170 CONTINUE
      DO 180 J=1,3
         ENG(J)=EAA(J)+EAB(J)+ENUC(J)
         ENG(J) = -ENG(J)*23.061D0
  180 CONTINUE
      RETURN
      END
