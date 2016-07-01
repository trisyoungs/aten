      SUBROUTINE FFHPOL (HEAT0,ATPOL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1 AXIS(3)
      LOGICAL DEBUG
      INCLUDE 'SIZES'
C***********************************************************************
C  SUBROUTINE FOR THE FINITE FIELD CALCULATION OF ELECTRIC RESPONSE
C  PROPERTIES (DIPOLE MOMENT, POLARIZABILITY, AND 1ST AND 2ND
C  HYPERPOLARIZABILITY.
C
C  HENRY A. KURTZ, DEPARTMENT OF CHEMISTRY
C                  MEMPHIS STATE UNIVERSITY
C                  MEMPHIS, TN   38152
C
C***********************************************************************
      COMMON /CORE  / CORE(107)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM),NORS,NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /KEYWRD/ KEYWRD
      COMMON /FIELD / EFIELD(3)
      COMMON /EULER / TVEC(3,3),IDTVEC
      CHARACTER*241 KEYWRD
C
C
C     DIPE4 AND DIPDP HOLD THE CALCULATED DIPOLE MOMENTS
C
C     APOLE4 AND APOLDP HOLD THE POLARIZABILITY TENSOR AS
C                                A PACKED ARRAY XX,XY,YY,XZ,YZ,ZZ
C
C     BETAE4 AND BETAEP HOLD THE FIRST HYPERPOLARIZABILITY
C                                1. XXX
C                                2. YYY     6. YXX
C                                3. ZZZ     7. YZZ
C                                4. XYY     8. ZXX
C                                5. XZZ     9. ZYY
C
      DIMENSION HEATE(3,2),
     1          DIPE4(3),APOLE4(6),BETAE4(9),GAMME4(6),
     2          DIPDP(3),APOLDP(6),BETADP(9),GAMMDP(6),
     3          DIP1P(3),DIP1M(3),DIP2P(3),DIP2M(3)
      DIMENSION IPTBD(6), GRAD(MAXPAR)
      DATA IPTBD /5,7,4,9,6,8/
C Energy: a.u. to kcal/mole
      AUTOKC = 23.061D+00*27.2107D+00
C Dipole: a.u. to debye
      AUTODB = 2.541563D+00
C Electric Field: a.u. to volt/meter
      AUTOVM = 51.4257D+00
      NBDIP = 1
      NBCNT = 4
      NGCNT = 4
C
      DATA AXIS/'X','Y','Z'/
      DEBUG = (INDEX(KEYWRD,'DEBUG').NE.0)
C
C  FIELD STRENGTH IN A.U.
C
      EFVAL=0.001D0
      IDIP=1
C modification for variable field strength
      IF(INDEX(KEYWRD,'POLAR=').NE.0)
     1EFVAL=READA(KEYWRD,INDEX(KEYWRD,'POLAR='))
      WRITE (6,10) EFVAL
   10 FORMAT (//' APPLIED ELECTRIC FIELD MAGNITUDE: ',F15.5)
      SFE = 1.D00/EFVAL
      WRITE (6,20) 6.74834D0*ATPOL
   20 FORMAT (//' ATOMIC CONTRIBUTION TO THE POLARIZABILITY: ',F15.6,/,
     1          '  (IT IS ONLY APPLIED TO THE E4 RESULT)')
C.......................................................................
C  CALCULATE THE POLARIZABILITY AND HYPERPOLARIZABILITIES ALONG
C  THE THREE PRINCIPLE AXES.  (THESE AXES DEPEND ON YOUR ARBITRARY
C  ORIENTATION AND MAY NOT BE THE TRUE PRINCIPLE AXES.)
C.......................................................................
      DO 150 ID = 1,3
         IF (DEBUG) THEN
            WRITE (6,30) AXIS(ID)
   30       FORMAT (//,' ****** FIELD IN ',A1,' DIRECTION *****',/)
         ENDIF
C
C ZERO THE FIELD
C
         DO 40 I = 1,3
            EFIELD(I) = 0.0D00
   40    CONTINUE
         HNUC = 0.0D00
         DO 50 I = 1,NUMAT
            HNUC = HNUC + EFVAL*GEO(ID,I)*CORE(NAT(I))*AUTOVM
   50    CONTINUE
         HNUC = HNUC*23.061D00
C +E(ID)
         EFIELD(ID) = EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT1P,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP1P)
         DIIP = DIP1P(ID)
C -E(ID)
         EFIELD(ID) = -EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT1M,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP1M)
         DIIM = DIP1M(ID)
C +2E(ID)
         EFIELD(ID) = 2.0D00*EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT2P,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP2P)
C -2E(ID)
         EFIELD(ID) = -2.0D00*EFVAL
         CALL COMPFG(GEO,.TRUE.,HEAT2M,.TRUE.,GRAD,.FALSE.)
         CALL DIPIND (DIP2M)
C
C  CORRECT FOR ELECTRIC FIELD - NUCLEAR INTERACTIONS
C
         HEAT1P = HEAT1P + HNUC
         HEATE(ID,1) = HEAT1P
         HEAT1M = HEAT1M - HNUC
         HEATE(ID,2) = HEAT1M
         HEAT2P = HEAT2P + HNUC*2.D00
         HEAT2M = HEAT2M - HNUC*2.D00
C
         IF (DEBUG) THEN
            WRITE (6,60)
   60       FORMAT (' FIELDS OF: ',5X,'F',21X,'2F')
            WRITE (6,70) HEAT1P,HEAT2P,HEAT1M,HEAT2M,
     1                DIP1P(ID),DIP2P(ID),DIP1M(ID),DIP2M(ID)
   70       FORMAT ('  ENERGY:'/,
     1           '   + ',2(F20.10,3X),/,'   - ',2(F20.10,3X),/,
     2           '  DIPOLE:'/,
     3           '   + ',2(F20.10,3X),/,'   - ',2(F20.10,3X))
         ENDIF
C
C DIPOLE
C
         ETERM = (1.0D00/12.D00)*(HEAT2P - HEAT2M)
     1        - (2.0D00/3.0D00)*(HEAT1P - HEAT1M)
         DIPE4(ID) = ETERM*SFE/AUTOKC
C
C ALPHA
C
         IVL = (ID*(ID+1))/2
         ETERM = 2.5D00*HEAT0 - (4.D00/3.D00)*(HEAT1P + HEAT1M)
     1        + (1.D00/12.0D00)*(HEAT2P + HEAT2M)
         APOLE4(IVL) = ETERM*SFE*SFE/AUTOKC + ATPOL*6.74834D0
C
C BETA
C
         ETERM = (HEAT1P - HEAT1M) - 0.5D00*(HEAT2P - HEAT2M)
         BETAE4(ID) = ETERM*SFE*SFE*SFE/AUTOKC
C
C GAMMA
C
         ETERM = 4.0D00*(HEAT1P + HEAT1M) - (HEAT2P + HEAT2M)
     1        - 6.0D00*HEAT0
         GAMME4(ID) = ETERM*SFE*SFE*SFE*SFE/AUTOKC
C
C DIPOLE CALCULATIONS
C
         DMU = (2.0D00/3.0D00)*(DIP1P(ID) + DIP1M(ID))
     1      - (1.D00/6.0D00)*(DIP2P(ID) + DIP2M(ID))
         DIPDP(ID) = DMU/AUTODB
         AE = (2.0D00/3.0D00)*(DIP1P(ID) - DIP1M(ID))
     1     - (1.0D00/12.D00)*(DIP2P(ID) - DIP2M(ID))
         APOLDP(IVL) = AE*SFE/AUTODB
         BE = (1.D00/3.0D00)*(DIP2P(ID) + DIP2M(ID)
     1                     - DIP1P(ID) - DIP1M(ID))
         BETADP(ID) = BE*SFE*SFE/AUTODB
         GE = 0.5D00*(DIP2P(ID) - DIP2M(ID))
     1     - (DIP1P(ID) - DIP1M(ID))
         GAMMDP(ID) = GE*SFE*SFE*SFE/AUTODB
         DO 80 KD = 1,3
            IF (KD.LT.ID) THEN
               KVL = (ID*(ID-1))/2 + KD
               AKI = (2.0D00/3.0D00)*(DIP1P(KD) - DIP1M(KD))
     1         - (1.0D00/12.0D00)*(DIP2P(KD) - DIP2M(KD))
               APOLDP(KVL) = AKI*SFE/AUTODB
            ENDIF
            IF (KD.NE.ID) THEN
               BKII = (1.0D00/3.0D00)*(DIP2P(KD) + DIP2M(KD)
     1                           - DIP1P(KD) - DIP1M(KD))
               NBD = IPTBD(NBDIP)
               BETADP(NBD) = BKII*SFE*SFE/AUTODB
               NBDIP = NBDIP + 1
            ENDIF
   80    CONTINUE
C.......................................................................
C
C  NOW CALCULATE THE OFF AXIS RESULTS.
C
C.......................................................................
         IDM1 = ID - 1
         DO 140 JD = 1,IDM1
            HNUCJ = 0.0D00
            DO 90 I = 1,NUMAT
               HNUCJ = HNUCJ + EFVAL*GEO(JD,I)*CORE(NAT(I))*51.4257D0
   90       CONTINUE
            HNUCJ = HNUCJ*23.061D0
            DO 100 I = 1,3
               EFIELD(I) = 0.0D00
  100       CONTINUE
C
C DIAGONAL FIELDS WITH COMPONENTS EQUAL TO EFVAL
C
            EFIELD(ID) = EFVAL
            EFIELD(JD) = EFVAL
            CALL COMPFG(GEO,.TRUE.,HPP,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DPP = DIP1P(ID)
            EFIELD(JD) = -EFVAL
            CALL COMPFG(GEO,.TRUE.,HPM,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DPM = DIP1P(ID)
            EFIELD(ID) = -EFVAL
            CALL COMPFG(GEO,.TRUE.,HMM,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DMM = DIP1P(ID)
            EFIELD(JD) = EFVAL
            CALL COMPFG(GEO,.TRUE.,HMP,.TRUE.,GRAD,.FALSE.)
            CALL DIPIND (DIP1P)
            DMP = DIP1P(ID)
            HPP = HPP + HNUC + HNUCJ
            HPM = HPM + HNUC - HNUCJ
            HMM = HMM - HNUC - HNUCJ
            HMP = HMP - HNUC + HNUCJ
            IF (DEBUG) THEN
               WRITE (6,110)
  110          FORMAT (/,' ',12X,'+,+',15X,'+,-',15X,'-,+',15X,'-,-')
               WRITE (6,120) HPP,HPM,HMP,HMM
  120          FORMAT ('  E ',4F15.6)
            ENDIF
C
C  DIAGONAL FIELDS WITH COMPONENTS EQUAL TO 2*EFVAL
C
            EFIELD(ID) = EFVAL*2.D00
            EFIELD(JD) = EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2PP,.TRUE.,GRAD,.FALSE.)
            EFIELD(JD) = -EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2PM,.TRUE.,GRAD,.FALSE.)
            EFIELD(ID) = -EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2MM,.TRUE.,GRAD,.FALSE.)
            EFIELD(JD) = EFVAL*2.D00
            CALL COMPFG(GEO,.TRUE.,H2MP,.TRUE.,GRAD,.FALSE.)
            H2PP = H2PP + 2.0D00*(HNUC + HNUCJ)
            H2PM = H2PM + 2.0D00*(HNUC - HNUCJ)
            H2MM = H2MM - 2.0D00*(HNUC + HNUCJ)
            H2MP = H2MP - 2.0D00*(HNUC - HNUCJ)
            IF (DEBUG) THEN
               WRITE (6,130) H2PP,H2PM,H2MP,H2MM
  130          FORMAT (' 2E ',4F15.6)
            ENDIF
C
            ATERM = (1.0D00/48.0D00)*(H2PP - H2PM - H2MP + H2MM)
     1          - (1.0D00/3.0D00)*(HPP - HPM - HMP + HMM)
            AIJ = ATERM*SFE*SFE/AUTOKC
            IVL = (ID*(ID-1))/2 + JD
            APOLE4(IVL) = AIJ
            BTERM = 0.5D00*(HMM - HPP + HPM - HMP)
     1          + HEATE(JD,1) - HEATE(JD,2)
            BJII = BTERM*SFE*SFE*SFE/AUTOKC
            BETAE4(NBCNT) = BJII
            NBCNT = NBCNT + 1
            BTERM = 0.5D00*(HMM - HPP + HMP - HPM)
     1          + HEATE(ID,1) - HEATE(ID,2)
            BIJJ = BTERM*SFE*SFE*SFE/AUTOKC
            BETAE4(NBCNT) = BIJJ
            NBCNT = NBCNT + 1
C
            GTERM = -(HPP + HMM + HPM + HMP) - 4.0D00*HEAT0
     1           + 2.0D00*(HEATE(ID,1) + HEATE(ID,2))
     2           + 2.0D00*(HEATE(JD,1) + HEATE(JD,2))
            GIIJJ = GTERM*SFE*SFE*SFE*SFE/AUTOKC
            GAMME4(NGCNT) = GIIJJ
            GDIP = 0.5D00*(DPP - DMP + DPM - DMM) - (DIIP - DIIM)
            GAMMDP(NGCNT) = GDIP*SFE*SFE*SFE/AUTODB
            NGCNT = NGCNT + 1
  140    CONTINUE
C
  150 CONTINUE
C-----------------------------------------------------------------------
C  SUMMARIZE THE RESULTS
C-----------------------------------------------------------------------
      WRITE (6,160)
  160 FORMAT (//,' ',30('*'),' DIPOLE ',30('*'),//)
      DIPE4T = SQRT(DIPE4(1)*DIPE4(1) + DIPE4(2)*DIPE4(2)
     1              + DIPE4(3)*DIPE4(3))
      DIPE4D = DIPE4T*AUTODB
      DIPDPT = SQRT(DIPDP(1)*DIPDP(1) + DIPDP(2)*DIPDP(2)
     1              + DIPDP(3)*DIPDP(3))
      DIPDPD = DIPDPT*AUTODB
      WRITE (6,170)
  170 FORMAT (21X,'E4',13X,'DIP',/)
      WRITE (6,180) 'X',DIPE4(1),DIPDP(1)
      WRITE (6,180) 'Y',DIPE4(2),DIPDP(2)
      WRITE (6,180) 'Z',DIPE4(3),DIPDP(3)
  180 FORMAT (5X,A1,7X,2F15.6)
      WRITE (6,190) DIPE4T,DIPDPT,
     1               DIPE4D,DIPDPD
  190 FORMAT (//' MAGNITUDE:  ',2F15.6,'  (A.U.)',/,
     1          ' ',12X,2F15.6,'  (DEBYE)')
C
C FIND EIGENVALUES AND EIGENVECTORS OF POLARIZATION MATRIX.
C
      WRITE (6,200)
  200 FORMAT (//,' ',22('*'),' POLARIZABILITY (ALPHA)',21('*'),//)
      AVGPE4 = (APOLE4(1)+APOLE4(3)+APOLE4(6))/3.0D00
      AVGA3 = AVGPE4*0.14818D00
      AVGESU = AVGPE4*0.296352D-24
      AVGPDP = (APOLDP(1)+APOLDP(3)+APOLDP(6))/3.0D00
      AVGA3D = AVGPDP*0.14818D00
      AVGESD = AVGPDP*0.296352D-24
      WRITE (6,210)
  210 FORMAT ('  COMPONENT',12X,'E4',13X,'DIP',/)
      WRITE (6,220) 'XX',APOLE4(1),APOLDP(1),
     1              'YY',APOLE4(3),APOLDP(3),
     2              'ZZ',APOLE4(6),APOLDP(6),
     3              'XY',APOLE4(2),APOLDP(2),
     4              'XZ',APOLE4(4),APOLDP(4),
     5              'YZ',APOLE4(5),APOLDP(5)
  220 FORMAT (' ',5X,A4,5X,2F15.6)
      WRITE (6,230) AVGPE4,AVGPDP,AVGA3,AVGA3D,AVGESU,AVGESD
  230 FORMAT (//,' AVERAGE POLARIZABILITY:',8X,'E4',13X,'DIP',/,
     1           ' ',24X,2F15.6,'  A.U.',/,
     2           ' ',24X,2F15.6,'  ANG.**3',/,
     3           ' ',24X,2(1PD15.6),'  ESU')
C
C  CALCULATE "EXPERIMENTAL" HYPERPOLARIZABILITIES
C
C   8.65710D-33 is a.u. to e.s.u. conversion
      WRITE (6,240)
  240 FORMAT (//,' ',30('*'),' SECOND-ORDER (BETA)',25('*'),//)
      BX4 = 0.6D00*(BETAE4(1) + BETAE4(4) + BETAE4(6))
      BY4 = 0.6D00*(BETAE4(2) + BETAE4(5) + BETAE4(8))
      BZ4 = 0.6D00*(BETAE4(3) + BETAE4(7) + BETAE4(9))
      B4MU = (BX4*DIPE4(1) + BY4*DIPE4(2) + BZ4*DIPE4(3))/DIPE4T
      B4ESU = B4MU*8.65710D-03
      BXD = 0.6D00*(BETADP(1) + BETADP(4) + BETADP(6))
      BYD = 0.6D00*(BETADP(2) + BETADP(5) + BETADP(8))
      BZD = 0.6D00*(BETADP(3) + BETADP(7) + BETADP(9))
      BDMU = (BXD*DIPDP(1) + BYD*DIPDP(2) + BZD*DIPDP(3))/DIPDPT
      BDESU = BDMU*8.65710D-03
C
      WRITE(6,'(29X,A2,25X,A6)')'1X','(1/2)X'
      WRITE (6,250)
  250 FORMAT ('  COMPONENT',2(12X,'E4',10X,'DIP',2X),/)
      WRITE (6,260) 'XXX',BETAE4(1),BETADP(1),BETAE4(1)/2,BETADP(1)/2
      WRITE (6,260) 'XYY',BETAE4(4),BETADP(4),BETAE4(4)/2,BETADP(4)/2
      WRITE (6,260) 'XZZ',BETAE4(6),BETADP(6),BETAE4(6)/2,BETADP(6)/2
      WRITE (6,260) 'YYY',BETAE4(2),BETADP(2),BETAE4(2)/2,BETADP(2)/2
      WRITE (6,260) 'YXX',BETAE4(5),BETADP(5),BETAE4(5)/2,BETADP(5)/2
      WRITE (6,260) 'YZZ',BETAE4(8),BETADP(8),BETAE4(8)/2,BETADP(8)/2
      WRITE (6,260) 'ZZZ',BETAE4(3),BETADP(3),BETAE4(3)/2,BETADP(3)/2
      WRITE (6,260) 'ZXX',BETAE4(7),BETADP(7),BETAE4(7)/2,BETADP(7)/2
      WRITE (6,260) 'ZYY',BETAE4(9),BETADP(9),BETAE4(9)/2,BETADP(9)/2
  260 FORMAT (' ',5X,A4,2(5X,2F12.3))
      WRITE (6,270)
  270 FORMAT (//,' VECTOR COMPONENTS GIVEN BY:',/,
     1          '      BI = (2/5)*(BI11+BI22+BI33)'/)
      WRITE (6,280) 'BX',BX4,BXD,BX4/2,BXD/2
      WRITE (6,280) 'BY',BY4,BYD,BY4/2,BYD/2
      WRITE (6,280) 'BZ',BZ4,BZD,BZ4/2,BZD/2
  280 FORMAT (' ',6X,A2,2(6X,2F12.3))
      WRITE (6,290)
  290 FORMAT (//'  VALUE OF BETA ALONG THE DIPOLE MOMENT:'/)
      WRITE (6,300) B4MU,BDMU,B4MU/2,BDMU/2,B4ESU,BDESU,B4ESU/2,BDESU/2
  300 FORMAT (' ',4X,'B(AU)',2(5X,2F12.3,2X),/,
     1        ' ',4X,'B(ESU)',4X,2F12.3,7X,2F12.3,' (X10-30)')
C
      WRITE (6,310)
  310 FORMAT (//' ',24('*'),' THIRD-ORDER (GAMMA)',24('*'),//)
      GAMVAL = (GAMME4(1) + GAMME4(2) + GAMME4(3))
      GAMVAL = GAMVAL + 2.0D00*(GAMME4(4) + GAMME4(5) + GAMME4(6))
      GAMVAL = GAMVAL/5.0D00
C  5.05116D-40 is the a.u. to e.s.u. conversion
      GAMESU = GAMVAL*5.05116D-04
      GAMDIP = (GAMMDP(1) + GAMMDP(2) + GAMMDP(3))
      GAMDIP = GAMDIP + 2.0D00*(GAMMDP(4) + GAMMDP(5) + GAMMDP(6))
      GAMDIP = GAMDIP/5.0D00
      GAMDES = GAMDIP*5.05116D-04
      WRITE(6,'(23X,A2,25X,A6)')'1X','(1/6)X'
      WRITE (6,320)
  320 FORMAT (' ',17X,'E4',8X,'DIP',16X,'E4',8X,'DIP',/)
      WRITE (6,330) 'XXXX',GAMME4(1),GAMMDP(1),GAMME4(1)/6,GAMMDP(1)/6
      WRITE (6,330) 'YYYY',GAMME4(2),GAMMDP(2),GAMME4(2)/6,GAMMDP(2)/6
      WRITE (6,330) 'ZZZZ',GAMME4(3),GAMMDP(3),GAMME4(3)/6,GAMMDP(3)/6
      WRITE (6,330) 'XXYY',GAMME4(4),GAMMDP(4),GAMME4(4)/6,GAMMDP(4)/6
      WRITE (6,330) 'XXZZ',GAMME4(5),GAMMDP(5),GAMME4(5)/6,GAMMDP(5)/6
      WRITE (6,330) 'YYZZ',GAMME4(6),GAMMDP(6),GAMME4(6)/6,GAMMDP(6)/6
  330 FORMAT (5X,A4,2F12.3,5X,2F12.3)
      WRITE (6,340)
  340 FORMAT (//' AVERAGE GAMMA GIVEN BY:',/,
     1 '    (1/5)*[GXXX + GYYY + GZZZ + 2.0*(GXXYY + GXXZZ + GYYZZ)]')
      WRITE(6,'(/,20X,A2,22X,A6)')'1X','(1/6)X'
      WRITE (6,350) GAMVAL,GAMDIP,GAMVAL/6,GAMDIP/6,
     1              GAMESU,GAMDES,GAMESU/6,GAMDES/6
  350 FORMAT (/' <GAMMA> ',1PD12.5,1PD12.5,5X,1PD12.5,1PD12.5,'  A.U.'/,
     1       ' ',8X,1PD12.5,1PD12.5,5X,1PD12.5,1PD12.5,'  ESU (X10-36)')
C
      RETURN
      END
