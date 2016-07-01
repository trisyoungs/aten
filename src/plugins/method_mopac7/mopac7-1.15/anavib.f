      SUBROUTINE ANAVIB(COORD,EIGS,DIPT,N3,VIBS,RIJ,HESS,TRAVEL,REDMAS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,NUMATM),EIGS(N3),VIBS(N3,N3), DIPT(N3),
     1RIJ(MAXHES), TRAVEL(N3), HESS(*), REDMAS(*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /ELEMTS/ ELEMNT(107)
      LOGICAL VIB1, VIB2, VIB3, VIB4
      COMMON /FOKMAT/ F(MPACK*2)
      CHARACTER*2 ELEMNT
      DIMENSION VANRAD(107), IJF(10), FIJ(10)
      SAVE VANRAD
      DATA VANRAD/
     1   0.32,0.93,
     2   1.23, 0.90, 0.82, 0.77, 0.75, 0.73, 0.72, 0.71,
     3   1.54, 1.36, 1.18, 1.11, 1.06, 1.02, 0.99, 0.98,
     4   2.03, 1.74, 1.44, 1.32, 1.22, 1.18, 1.17, 1.17, 1.16,
     5   1.15, 1.17, 1.25, 1.26, 1.22, 1.20, 1.16, 1.14, 1.12,
     6   2.16, 1.91, 1.62, 1.45, 1.34, 1.30, 1.27, 1.25, 1.25,
     7   1.28, 1.34, 1.48, 1.44, 1.41, 1.40, 1.36, 1.33, 1.31,
     8   2.35, 1.98, 1.69,
     9   1.65, 1.65, 1.64, 1.63, 1.62, 1.85, 1.61, 1.59, 1.59, 1.58,
     1   1.57, 1.56, 1.56, 1.56,
     2   1.44, 1.34, 1.30, 1.28, 1.26, 1.27, 1.30, 1.34,
     3   1.49, 1.48, 1.47, 1.46, 1.46, 1.45,1.45,21*1.45/
      N3=NUMAT*3
C
C    COMPUTE INTERATOMIC DISTANCES.
C
      L=0
      DO 10 I=1,NUMAT
         DO 10 J=1,I-1
            L=L+1
   10 RIJ(L)=SQRT((COORD(1,J)-COORD(1,I))**2+
     1            (COORD(2,J)-COORD(2,I))**2+
     2            (COORD(3,J)-COORD(3,I))**2)+1.D-10
C
C     ANALYSE VIBRATIONS
C
      WRITE(6,'(''1'',//10X,''DESCRIPTION OF VIBRATIONS'',/)')
      ILINE=0
      DO 120 K=1,N3
         IF(ABS(EIGS(K)).LT.50) GOTO 120
         VIB1=.TRUE.
         VIB2=.TRUE.
         VIB3=.TRUE.
         VIB4=.TRUE.
         J3=0
         L=0
         TOT=0.D0
         LINEAR=0
         J1=-2
         DO 70 J=1,NUMAT
            J1=J1+3
            I1=-2
            DO 70 I=1,J-1
               I1=I1+3
               VDW=(VANRAD(NAT(I))+VANRAD(NAT(J)))*1.5D0
               L=L+1
               F(L)=0.D0
               IF(   RIJ(L)  .LT.  VDW) THEN
C
C CALCULATE ENERGY TERM BETWEEN THE TWO ATOMS
C
                  EAB=0.D0
                  DO 20 JJ=J1,J1+2
                     DO 20 II=I1,I1+2
   20             EAB=EAB+VIBS(JJ,K)*HESS((JJ*(JJ-1))/2+II)*VIBS(II,K)
                  EB=0.D0
                  DO 40 JJ=J1,J1+2
                     DO 30 II=J1,JJ
   30                EB=EB+VIBS(JJ,K)*HESS((JJ*(JJ-1))/2+II)*VIBS(II,K)*
     12.D0
   40             EB=EB-VIBS(JJ,K)*HESS((JJ*(JJ+1))/2)*VIBS(JJ,K)
                  EA=0.D0
                  DO 60 JJ=I1,I1+2
                     DO 50 II=I1,JJ
   50                EA=EA+VIBS(JJ,K)*HESS((JJ*(JJ-1))/2+II)*VIBS(II,K)*
     12.D0
   60             EA=EA-VIBS(JJ,K)*HESS((JJ*(JJ+1))/2)*VIBS(JJ,K)
                  LINEAR=LINEAR+1
                  F(L)=EA+EAB*2.D0+EB
                  TOT=TOT+F(L)
               ENDIF
   70    CONTINUE
C
C  NOW TO SORT F INTO DECENDING ORDER
C
         DO 90 I=1,10
            SUM=-100.D0
            DO 80 J=1,L
               IF(ABS(F(J)).GT.SUM)THEN
                  JJ=J
                  SUM=ABS(F(J))
               ENDIF
   80       CONTINUE
            IF(SUM.LT.0.D0)GOTO 100
            FIJ(I)=SUM
            F(JJ)=-1.D-5
            IJF(I)=JJ
C#      WRITE(6,*)FIJ(I),IJF(I)
   90    CONTINUE
         I=10
  100    LINEAR=I
         SUM=1.D0/(TOT+1.D-8)
         DO 110 IJ=1,LINEAR
            J=0.5D0*(0.99D0+SQRT(1.D0+8.D0*IJF(IJ)))
            I=IJF(IJ)-(J*(J-1))/2
            J=J+1
            XJ=COORD(1,J)
            YJ=COORD(2,J)
            ZJ=COORD(3,J)
            J1=3*J-2
            J2=J1+1
            J3=J2+1
            I3=0
            XI=COORD(1,I)
            YI=COORD(2,I)
            ZI=COORD(3,I)
            I1=3*I-2
            I2=I1+1
            I3=I2+1
            X= VIBS(J1,K)-VIBS(I1,K)
            Y= VIBS(J2,K)-VIBS(I2,K)
            Z= VIBS(J3,K)-VIBS(I3,K)
            E=FIJ(IJ)*SUM*100.D0
C#            IF(ABS(E).GT.110)GOTO 120
            SHIFT=X*X+Y*Y+Z*Z+1.D-30
            IF(ABS(E) .GT. 10.D0.OR.IJ.LT.5.AND.ABS(E).GT.0.1) THEN
               SHIFT=SQRT(SHIFT)
               RADIAL=((X*(XI-XJ)+Y*(YI-YJ)+Z*(ZI-ZJ))
     1                  /(SHIFT*RIJ(IJF(IJ))))**2*100.D0
               IF (VIB1) THEN
                  WRITE(6,'(/,'' VIBRATION'',I4,''            ATOM PAIR
     1 '',''    ENERGY CONTRIBUTION              RADIAL'')')K
                  ANS=100.D0*SQRT(FIJ(IJ)*1.D5*6.023D23)/(2.998D10*3.141
     159D0*2.D0)/EIGS(K)
                  ANS=MIN(999.9D0,MAX(-99.9D0,ANS))
                  WRITE(6,'('' FREQ.   '',F9.2,6X,1A2,I2,
     1'' -- '',A2,I2, ''         '',F6.1,''% ('',F5.1,''%)'',F18.1
     2,''%'')')
     3EIGS(K),ELEMNT(NAT(I)),I,ELEMNT(NAT(J)),J,E,ANS,RADIAL
C#      WRITE(6,*)ANS
                  VIB1=.FALSE.
               ELSEIF (VIB2) THEN
                  VIB2=.FALSE.
                  WRITE(6,'('' T-DIPOLE'',F9.4,6X,1A2,I2,
     1'' -- '',A2,I2, ''         '',F6.1,''%'',F27.1,''%'')')
     2DIPT(K),ELEMNT(NAT(I)),I,ELEMNT(NAT(J)),J,E,RADIAL
               ELSEIF (VIB3) THEN
                  VIB3=.FALSE.
                  WRITE(6,'('' TRAVEL  '',F9.4,6X,1A2,I2,
     1'' -- '',A2,I2, ''         '',F6.1,''%'',F27.1,''%'')')
     2TRAVEL(K),ELEMNT(NAT(I)),I,ELEMNT(NAT(J)),J,E,RADIAL
               ELSEIF (VIB4) THEN
                  VIB4=.FALSE.
                  WRITE(6,'('' RED. MASS'',F8.4,6X,1A2,I2,
     1'' -- '',A2,I2, ''         '',F6.1,''%'',F27.1,''%'')')
     2REDMAS(K),ELEMNT(NAT(I)),I,ELEMNT(NAT(J)),J,E,RADIAL
               ELSE
                  ILINE=ILINE+1
                  WRITE(6,'(''                        '',1A2,I2,
     1'' -- '',A2,I2, ''         '',F6.1,''%'',F27.1,''%'')')
     2ELEMNT(NAT(I)),I,ELEMNT(NAT(J)),J,E,RADIAL
               ENDIF
            ENDIF
  110    CONTINUE
         ILINE=ILINE+6
         IF(VIB1)WRITE(6,'(/,'' VIBRATION'',I4)')K
         IF(VIB1)WRITE(6,'(  '' FREQ.    '',F8.2)')EIGS(K)
         IF(VIB2)WRITE(6,'(  '' T-DIPOLE '',F8.4)')DIPT(K)
         IF(VIB3)WRITE(6,'(  '' TRAVEL   '',F8.4)')TRAVEL(K)
         IF(VIB4)WRITE(6,'(  '' RED. MASS'',F8.4)')REDMAS(K)
         IF(ILINE.GT.52)THEN
            ILINE=0
            WRITE(6,'(''1'')')
         ENDIF
  120 CONTINUE
      RETURN
      END
