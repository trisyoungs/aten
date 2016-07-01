      SUBROUTINE ENPART(UHF,H,ALPHA,BETA,P,Q,COORD)
C----------------------------------------------------------*
C
C     NEW SUB. ENPART,  MODIFIED BY TSUNEO HIRANO 1986/6/3/
C
C---------------------------------------------------------*
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      PARAMETER (NATMS2 = (NUMATM*(NUMATM+1))/2)
      COMMON /ELEMTS/ ELEMNT(107)
      DIMENSION H(*), ALPHA(*), BETA(*), P(*), Q(*), COORD(3,*)
C--- DEFINED HERE, AND TO BE USED FOR ENPART-PRINT ONLY ---*
      DIMENSION EX(NATMS2,3)
C--- END OF DIMENSION DEFINITION ----------------- BY TH --*
      LOGICAL UHF, MINDO3, AM1
      CHARACTER*241 KEYWRD
      CHARACTER*2 ELEMNT
C***********************************************************************
C
C *** ENERGY PARTITIONING WITHIN THE UMINDO/3 AND UMNDO SCHEME
C     ROUTINE WRITTEN BY S.OLIVELLA, BARCELONA NOV. 1979.
C     EXTENDED TO AM1 AND PM3 BY JJPS.
C
C   ON INPUT UHF     = .TRUE. IF A U.H.F. CALCULATION.
C            H       = ONE-ELECTRON MATRIX.
C            ALPHA   = ALPHA ELECTRON DENSITY.
C            BETA    = BETA ELECTRON DENSITY.
C            P       = TOTAL ELECTRON DENSITY.
C            Q       = ATOM ELECTRON DENSITIES.
C
C    NOTHING IS CHANGED ON EXIT.
C
C***********************************************************************
      COMMON /ONELEC/ USS(107), UPP(107), UDD(107)
      COMMON /CORE  / CORE(107)
      COMMON /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /TWOEL3/ F03(107)
      COMMON /ALPHA / ALP(107)
      COMMON /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     1                ,GSD(107),GPD(107),GDD(107)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM)
     1               ,NLAST(NUMATM),NORBS,NELECS,
     2                NALPHA,NBETA,NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /WMATRX/ W(N2ELEC*2)
      COMMON /KEYWRD/ KEYWRD
      PARAMETER (MDUMY=MAXPAR*MAXPAR-NUMATM*3-NATMS2*4)
      COMMON /SCRACH/ EA(NUMATM,2),EAT(NUMATM), E(NATMS2,4),
     1XDUMY(MDUMY)
      MINDO3=(INDEX(KEYWRD,'MINDO').NE.0)
C *** RECALCULATE THE DENSITY MATRICES IN THE UHF SCHEME
C
      LINEAR=NORBS*(NORBS+1)/2
      IF( .NOT. UHF) THEN
         DO 20 I=1,LINEAR
   20    BETA(I)=ALPHA(I)
      ENDIF
C
C *** ONE-CENTER ENERGIES
      K=0
      DO 40 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         NI=NAT(I)
         EA(I,1)=0.0D0
         DO 30 J=IA,IB
            K=K+J
            T=UPP(NI)
            IF(J.EQ.IA) T=USS(NI)
   30    EA(I,1)=EA(I,1)+P(K)*T
         ISS=(IA*(IA+1))/2
         EA(I,2)=0.5D0*GSS(NI)*P(ISS)*P(ISS)
     1  -0.5D0*GSS(NI)*(ALPHA(ISS)*ALPHA(ISS)+BETA(ISS)*BETA(ISS))
         IF(IA.EQ.IB) GO TO 40
         IA1=IA+1
         IA2=IA+2
         IXX=IA1*IA2/2
         IYY=IA2*IB/2
         IZZ=(IB*(IB+1))/2
         IXY=IA1+IA2*IA1/2
         IXZ=IA1+IB*IA2/2
         IYZ=IA2+IB*IA2/2
         ISX=IA+IA1*IA/2
         ISY=IA+IA2*IA1/2
         ISZ=IA+IB*IA2/2
         SS1=P(IXX)*P(IXX)+P(IYY)*P(IYY)+P(IZZ)*P(IZZ)
         SS2=P(ISS)*(P(IXX)+P(IYY)+P(IZZ))
         SS3=P(IXX)*P(IYY)+P(IXX)*P(IZZ)+P(IYY)*P(IZZ)
         SS4=P(ISX)*P(ISX)+P(ISY)*P(ISY)+P(ISZ)*P(ISZ)
         SS5=P(IXY)*P(IXY)+P(IXZ)*P(IXZ)+P(IYZ)*P(IYZ)
         TT1=ALPHA(IXX)*ALPHA(IXX)+ALPHA(IYY)*ALPHA(IYY)
     1+ALPHA(IZZ)*ALPHA(IZZ)+BETA(IXX)*BETA(IXX)
     2+BETA(IYY)*BETA(IYY)+BETA(IZZ)*BETA(IZZ)
         TT2=ALPHA(ISS)*(ALPHA(IXX)+ALPHA(IYY)+ALPHA(IZZ))
     1   +BETA(ISS)*(BETA(IXX)+BETA(IYY)+BETA(IZZ))
         TT3=ALPHA(IXX)*ALPHA(IYY)+ALPHA(IXX)*ALPHA(IZZ)
     1+ALPHA(IYY)*ALPHA(IZZ)+BETA(IXX)*BETA(IYY)
     2+BETA(IXX)*BETA(IZZ)+BETA(IYY)*BETA(IZZ)
         TT4=ALPHA(ISX)*ALPHA(ISX)+ALPHA(ISY)*ALPHA(ISY)
     1+ALPHA(ISZ)*ALPHA(ISZ)+BETA(ISX)*BETA(ISX)
     2+BETA(ISY)*BETA(ISY)+BETA(ISZ)*BETA(ISZ)
         TT5=ALPHA(IXY)*ALPHA(IXY)+ALPHA(IXZ)*ALPHA(IXZ)
     1+ALPHA(IYZ)*ALPHA(IYZ)+BETA(IXY)*BETA(IXY)
     2+BETA(IXZ)*BETA(IXZ)+BETA(IYZ)*BETA(IYZ)
         EA(I,2)=EA(I,2)+0.5D0*GPP(NI)*SS1+GSP(NI)*SS2
     1+GP2(NI)*SS3+HSP(NI)*SS4*2.0D0+0.5D0*(GPP(NI)-GP2(NI))*SS5*2.0D0
     2                -0.5D0*GPP(NI)*TT1-GSP(NI)*TT4-GP2(NI)*TT5-
     3        HSP(NI)*(TT2+TT4)-0.5D0*(GPP(NI)-GP2(NI))*(TT3+TT5)
   40 CONTINUE
      AM1=(INDEX(KEYWRD,'AM1')+INDEX(KEYWRD,'PM3').NE.0)
      IF(MINDO3) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN MINDO/3'')')
      ELSEIF( INDEX(KEYWRD,'PM3').NE.0 ) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN PM3'')')
      ELSEIF( INDEX(KEYWRD,'AM1').NE.0 ) THEN
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN AM1'')')
      ELSE
         WRITE(6,'(///,10X,''TOTAL ENERGY PARTITIONING IN MNDO'')')
      ENDIF
      WRITE(6,'(/10X,''ALL ENERGIES ARE IN ELECTRON VOLTS'')')
      KL=0
   50 K=KL+1
      KL=KL+10
      KL=MIN0(KL,NUMAT)
      DO 60 I=K,KL
   60 EAT(I)=EA(I,1)+EA(I,2)
      IF(NUMAT.GT.KL) GO TO 50
   70 EAU=0.0D0
      EAE=0.0D0
      DO 80 I=1,NUMAT
         EAU=EAU+EA(I,1)
   80 EAE=EAE+EA(I,2)
      TONE=EAU+EAE
C *** TWO-CENTER ENERGIES
C     RESONANCE (E(N,1)) TERMS
      N=1
      DO 100 II=2,NUMAT
         E(N,1)=0.0D0
         IA=NFIRST(II)
         IB=NLAST(II)
         IMINUS=II-1
         ONEII=1.D0
         IF(NAT(II).EQ.102)ONEII=0.D0
         DO 90 JJ=1,IMINUS
            N=N+1
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            ONEJJ=1.D0
            IF(NAT(JJ).EQ.102)ONEJJ=0.D0
            E(N,1)=0.0D0
            DO 90 I=IA,IB
               KA=(I*(I-1))/2
               DO 90 K=JA,JB
                  IK=KA+K
   90    E(N,1)=E(N,1)+2.0D0*P(IK)*H(IK)*ONEII*ONEJJ
  100 N=N+1
C
C     THE CODE THAT FOLLOWS APPLIES ONLY TO MNDO
C
      IF(.NOT.MINDO3) THEN
C     CORE-CORE REPULSION (E(N,2)) AND CORE-ELEC. ATTRACTION (E(N,3)).
         N=1
         KK=0
         DO 180 II=2,NUMAT
            E(N,2)=0.0D0
            E(N,3)=0.0D0
            IA=NFIRST(II)
            IB=NLAST(II)
            NI=NAT(II)
            ISS=(IA*(IA+1))/2
            IMINUS=II-1
            DO 170 JJ=1,IMINUS
               N=N+1
               JA=NFIRST(JJ)
               JB=NLAST(JJ)
               NJ=NAT(JJ)
               JSS=(JA*(JA+1))/2
               KK=KK+1
               G=W(KK)
               R=SQRT((COORD(1,II)-COORD(1,JJ))**2+(COORD(2,II)-COORD(2,
     1JJ))**2+  (COORD(3,II)-COORD(3,JJ))**2)
               SCALE=1.0D0+EXP(-ALP(NI)*R)+EXP(-ALP(NJ)*R)
               NT=NI+NJ
               IF(NT.LT.8.OR.NT.GT.9) GO TO 110
               IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(R-1.0D0)*EXP(-ALP(NI)
     1*R)
               IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(R-1.0D0)*EXP(-ALP(NJ)
     1*R)
  110          E(N,2)=CORE(NI)*CORE(NJ)*G*SCALE
               IF( AM1 )THEN
                  SCALE=0.0D0
                  DO 120 IG=1,10
                     IF(ABS(FN1(NI,IG)).GT.0.D0)
     1SCALE=SCALE +CORE(NI)*CORE(NJ)/R*
     2FN1(NI,IG)*EXP(-FN2(NI,IG)*(R-FN3(NI,IG))**2)
                     IF(ABS(FN1(NJ,IG)).GT.0.D0)
     1SCALE=SCALE +CORE(NI)*CORE(NJ)/R*
     2FN1(NJ,IG)*EXP(-FN2(NJ,IG)*(R-FN3(NJ,IG))**2)
  120             CONTINUE
                  E(N,2)=E(N,2)+SCALE
               ENDIF
               E(N,3)=-(P(ISS)*CORE(NJ)+P(JSS)*CORE(NI))*G
               IF(NJ.LT.3) GO TO 140
               KINC=9
               JAP1=JA+1
               DO 130 K=JAP1,JB
                  KC=(K*(K-1))/2
                  DO 130 L=JA,K
                     KL=KC+L
                     BB=2.0D0
                     IF(K.EQ.L) BB=1.0D0
                     KK=KK+1
  130          E(N,3)=E(N,3)-P(KL)*CORE(NI)*BB*W(KK)
               GO TO 150
  140          KINC=0
  150          IF(NI.LT.3) GO TO 170
               IAP1=IA+1
               DO 160 I=IAP1,IB
                  KA=(I*(I-1))/2
                  DO 160 J=IA,I
                     IJ=KA+J
                     AA=2.0D0
                     IF(I.EQ.J) AA=1.0D0
                     KK=KK+1
                     E(N,3)=E(N,3)-P(IJ)*CORE(NJ)*AA*W(KK)
  160          KK=KK+KINC
  170       CONTINUE
  180    N=N+1
C     COULOMB (E(N,4)) AND EXCHANGE (EX(N)) TERMS
         N=1
         KK=0
         DO 200 II=2,NUMAT
            E(N,4)=0.0D0
            EX(N,1)=0.0D0
            IA=NFIRST(II)
            IB=NLAST(II)
            IMINUS=II-1
            DO 190 JJ=1,IMINUS
               JA=NFIRST(JJ)
               JB=NLAST(JJ)
               N=N+1
               E(N,4)=0.0D0
               EX(N,1)=0.0D0
               DO 190 I=IA,IB
                  KA=(I*(I-1))/2
                  DO 190 J=IA,I
                     KB=(J*(J-1))/2
                     IJ=KA+J
                     AA=2.0D0
                     IF(I.EQ.J) AA=1.0D0
                     PIJ=P(IJ)
                     DO 190 K=JA,JB
                        KC=(K*(K-1))/2
                        IK=KA+K
                        JK=KB+K
                        DO 190 L=JA,K
                           IL=KA+L
                           JL=KB+L
                           KL=KC+L
                           BB=2.0D0
                           IF(K.EQ.L) BB=1.0D0
                           KK=KK+1
                           G=W(KK)
                           E(N,4)=E(N,4)+AA*BB*G*PIJ*P(KL)
  190       EX(N,1) = EX(N,1)
     1    -0.5D0*AA*BB*G*(ALPHA(IK)*ALPHA(JL)+ALPHA(IL)*ALPHA(JK)+
     2    BETA(IK)*BETA(JL)+BETA(IL)*BETA(JK))
  200    N=N+1
      ELSE
         N=1
         DO 260 I=2,NUMAT
            E(N,2)=0.0D0
            E(N,3)=0.0D0
            E(N,4)=0.0D0
            EX(N,1)=0.0D0
            IA=NFIRST(I)
            IB=NLAST(I)
            NI=NAT(I)
            IMINUS=I-1
            DO 250 J=1,IMINUS
               N=N+1
               JA=NFIRST(J)
               JB=NLAST(J)
               NJ=NAT(J)
               RIJ=(COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     1+  (COORD(3,I)-COORD(3,J))**2
               GIJ=14.399D0/SQRT(RIJ+(7.1995D0/F03(NI)+7.1995D0/F03(NJ))
     1**2)
               PAB2=0.0D0
               IJ=MAX(NI,NJ)
               NBOND=(IJ*(IJ-1))/2+NI+NJ-IJ
               RIJ=SQRT(RIJ)
               IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) GO TO 210
               GO TO 220
  210          SCALE=ALP3(NBOND)*EXP(-RIJ)
               GO TO 230
  220          SCALE=EXP(-ALP3(NBOND)*RIJ)
  230          CONTINUE
               E(N,2)=CORE(NI)*CORE(NJ)*GIJ+
     1     ABS(CORE(NI)*CORE(NJ)*(14.399D0/RIJ-GIJ)*SCALE)
               E(N,3)=(-Q(I)*CORE(NJ)-Q(J)*CORE(NI))*GIJ
               E(N,4)=Q(I)*Q(J)*GIJ
               DO 240 K=IA,IB
                  KK=(K*(K-1))/2
                  DO 240 L=JA,JB
                     LK=KK+L
  240          PAB2=PAB2+ALPHA(LK)*ALPHA(LK)+BETA(LK)*BETA(LK)
  250       EX(N,1) = -PAB2*GIJ
  260    N=N+1
      ENDIF
      NUMAT1=(NUMAT*(NUMAT+1))/2
      DO 270 I=1,4
  270 E(NUMAT1,I)=0.0D0
      DO 280 I=1,3
  280 EX(NUMAT1,I)=0.0D0
C@ --------------------------*
C-----PRINT OUT ONE AND TWO CENTER ENERGIES
C
C     E(I,1):     RESONANCE ENERGY
C     E(I,2):     NUCLEAR-NUCLEAR REPULSION ENERGY
C     E(I,3):     ELECTRON-NUCLEAR ATTRACTION ENERGY
C     E(I,4):     ELECTRON-ELECTRON REPULSION ENERGY
C     EX(I,1):    EXCHANGE  ENERGY
C     EX(I,2):    EXCHANGE + RESONANCE ENERGY
C#      WRITE(6,'(//,''       ONE AND TWO CENTER ENERGIES (EV) '')')
C
C#      WRITE(6,'(/,''  [RESONANCE TERM] (EV)'')')
C#      CALL VECPRT(E,NUMAT)
C
C#      WRITE(6,'(/,''  [EXCHANGE TERM] (EV)'')')
C#      CALL VECPRT(EX,NUMAT)
C
C#      WRITE(6,'(/,''  [RESONANCE + EXCHANGE] (EV)'')')
      DO 290 N=1,NUMAT1
  290 EX(N,2) =E(N,1) + EX(N,1)
C
C   ADD IN MONOCENTRIC EXCHANGE AND COULOMBIC TERM
C
      DO 300 I=1,NUMAT
  300 EX((I*(I+1))/2,2)=EA(I,2)
C#      CALL VECPRT(EX(1,2),NUMAT)
C
C#      WRITE(6,'(/,''  [ELECTRON - ELECTRON REPULSION] (EV)'')')
C#      CALL VECPRT(E(1,4),NUMAT)
C
C#      WRITE(6,'(/,''  [ELECTRON-NUCLEAR ATTRACTION] (EV)'')')
      DO 310 I=1,NUMAT
  310 E((I*(I+1))/2,3)=EA(I,1)
C#      CALL VECPRT(E(1,3),NUMAT)
C
C#      WRITE(6,'(/,''  [NUCLEAR-NUCLEAR REPULSION] (EV)'')')
C#      CALL VECPRT(E(1,2),NUMAT)
C
      DO 320 N=1,NUMAT1
  320 EX(N,3) =E(N,4) + E(N,3) + E(N,2)
C     PRINT OUT OF TOTAL COULOMB TERM
C#      WRITE(6,'(/,''  [TOTAL COULOMB TERM (E-E, E-N, AND N-N)] (EV)'')
C#      CALL VECPRT(EX(1,3),NUMAT)
C     PRINT OUT OF TWO-CENTER SUM(OFF-DIAGONAL) +
C                  ONE-CENTER SUM(DIAGONAL).
C#      WRITE(6,'(/,''  [TWO-CENTER SUM (OFF-DIAGONAL), AND  '',
C#     1''ONE-CENTER SUM (DIAGONAL)] (EV)'')')
C#      DO 340 N=1,NUMAT1
C#  340 EX(N,3)=EX(N,3)+EX(N,2)
C#      CALL VECPRT(EX(1,3),NUMAT)
      WRITE(6,'(/,8(10X,A,/))')
     1'  ONE-CENTER TERMS',' ',
     2'E-E:  ELECTRON-ELECTRON REPULSION',
     3'E-N:  ELECTRON-NUCLEAR ATTRACTION'
      WRITE(6,'(/,''   ATOM     E-E       E-N    (E-E + E-N)'')')
      DO 330 I=1,NUMAT
         J=(I*(I+1))/2
         WRITE(6,'(2X,A2,I3,1X,2F10.4,F10.4)')
     1ELEMNT(NAT(I)),I, EX(J,2), E(J,3), EX(J,2)+E(J,3)
  330 CONTINUE
      WRITE(6,'(/,8(10X,A,/))')
     1'    TWO-CENTER TERMS',' ',
     2'J:   RESONANCE ENERGY          E-E: ELECTRON-ELECTRON REPULSION',
     3'K:   EXCHANGE ENERGY           E-N: ELECTRON-NUCLEAR ATTRACTION',
     4'                               N-N: NUCLEAR-NUCLEAR REPULSION',
     5'C:   COULOMBIC INTERACTION = E-E + E-N + N-N',
     6'EE:  TOTAL OF ELECTRONIC AND NUCLEAR ENERGIES'
      WRITE(6,'(/,''   ATOM          J        K       E-'',
     1''E       E-N      N-N      C        EE'')')
      WRITE(6,'(''   PAIR'')')
      IJ=0
      DO 340 I=1,NUMAT
         DO 340 J=1,I
            IJ=IJ+1
            IF(I.NE.J) THEN
               WRITE(6,'(1X,A2,I3,1X,A2,I3,1X,2F9.4,F9.4,F10.4,F9.4,F8.4
     1,F9.4)')ELEMNT(NAT(I)),I,ELEMNT(NAT(J)),J, E(IJ,1), EX(IJ,1),
     2E(IJ,4), E(IJ,3), E(IJ,2), EX(IJ,3), EX(IJ,2)+EX(IJ,3)
            ELSE
               IF(I.LT.6.OR.I.EQ.NUMAT)THEN
                  WRITE(6,*)
               ELSE
                  WRITE(6,'(/,''   ATOM          J        K       E-'',
     1''E       E-N      N-N      C        EE'')')
                  WRITE(6,'(''   PAIR'')')
               ENDIF
            ENDIF
  340 CONTINUE
C
C     ++++   TOTALS   ++++
C
      EABR=0.0D0
      EABX=0.0D0
      EABEE=0.0D0
      EABEN=0.0D0
      EABNN=0.0D0
      DO 350 I=1,NUMAT
  350 E((I*(I+1))/2,3)=0.D0
      DO 360 I=1,NUMAT1
         EABR=EABR+E(I,1)
         EABX=EABX+EX(I,1)
         EABEE=EABEE+E(I,4)
         EABEN=EABEN+E(I,3)
         EABNN=EABNN+E(I,2)
  360 CONTINUE
      EABRX=EABR+EABX
      EABE=EABEE+EABEN+EABNN
      TTWO=EABRX+EABE
      ET=TONE+TTWO
C@ ***************************************************************
      WRITE(6,370)
  370 FORMAT(///,'***  SUMMARY OF ENERGY PARTITION  ***')
      WRITE(6,380)
  380 FORMAT(1H ,'---------------------------------------')
      WRITE(6,'(''     ONE-CENTER TERMS'')')
      WRITE(6,390) EAU
  390 FORMAT(/,' ELECTRON-NUCLEAR  (ONE-ELECTRON) ',F17.4,' EV')
      WRITE(6,400) EAE
  400 FORMAT(' ELECTRON-ELECTRON (TWO-ELECTRON) ',F17.4,' EV')
      WRITE(6,410) TONE
  410 FORMAT(/,' TOTAL OF ONE-CENTER TERMS        ',18X,F15.4,' EV')
      WRITE(6,380)
      WRITE(6,'(''     TWO-CENTER TERMS'')')
      WRITE(6,420) EABR
  420 FORMAT(/,' RESONANCE ENERGY',8X,F15.4,' EV')
      WRITE(6,430) EABX
  430 FORMAT(' EXCHANGE ENERGY ',8X,F15.4,' EV')
      WRITE(6,440) EABRX
  440 FORMAT(/,' EXCHANGE + RESONANCE ENERGY:       ',F15.4,' EV')
      WRITE(6,450) EABEE
  450 FORMAT(/,' ELECTRON-ELECTRON REPULSION',F12.4,' EV')
      WRITE(6,460) EABEN
  460 FORMAT(  ' ELECTRON-NUCLEAR ATTRACTION',F12.4,' EV')
      WRITE(6,470) EABNN
  470 FORMAT(  ' NUCLEAR-NUCLEAR REPULSION  ',F12.4,' EV')
      WRITE(6,480) EABE
  480 FORMAT(/,' TOTAL ELECTROSTATIC INTERACTION    ',F15.4,' EV',/)
      WRITE(6,490) TTWO
  490 FORMAT(' GRAND TOTAL OF TWO-CENTER TERMS   ',17X,F15.4,' EV')
      WRITE(6,380)
      WRITE(6,500) ET
  500 FORMAT(' ETOT (EONE + ETWO)   ',30X,F15.4,' EV'//)
      RETURN
      END
