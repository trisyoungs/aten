      SUBROUTINE BTOC (COORD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XX(3),XA(3),COORD(3,*)
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                BH(LENABC)
     4       /SOLVI/  IATSP(LENABC+1),NAR(LENABC), NN(2,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
     3       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
      DO 10 I=1,NPS
         J=IATSP(I)
         RI=SRAD(J)-RDS
         DO 10 IX=1,3
   10 COSURF(IX,I)=COSURF(IX,I)*RI+COORD(IX,J)
C FILLING B-MATRIX
      I0=NPS2-NDEN
      IDEN=0
      DO 50 I=1,NUMAT
         IA=NFIRST(I)
         IDEL=NLAST(I)-IA
         NATI=NAT(I)
         DDI=DD(NATI)*2*.529177D0
         QQI2=(.529177D0*QQ(NATI))**2
         DO 20 IX=1,3
            XX(IX)=COORD(IX,I)
   20    CONTINUE
         DO 40 IPS=1,NPS
            I1=I0+IPS*NDEN
            DIST=0.D0
            DO 30 IX=1,3
               XA(IX)=COSURF(IX,IPS)-XX(IX)
               DIST=DIST+XA(IX)**2
   30       CONTINUE
            RM1=1.D0/DSQRT(DIST)
            ABCMAT(IDEN+1+I1)=RM1
            IF (IDEL .EQ. 0) GO TO 40
            RM3=RM1**3
            RM5=RM1**5
            ABCMAT(IDEN+3+I1)=RM1+3*XA(1)**2*QQI2*RM5-QQI2*RM3
            ABCMAT(IDEN+6+I1)=RM1+3*XA(2)**2*QQI2*RM5-QQI2*RM3
            ABCMAT(IDEN+10+I1)=RM1+3*XA(3)**2*QQI2*RM5-QQI2*RM3
            ABCMAT(IDEN+2+I1)=XA(1)*DDI*RM3
            ABCMAT(IDEN+4+I1)=XA(2)*DDI*RM3
            ABCMAT(IDEN+7+I1)=XA(3)*DDI*RM3
            ABCMAT(IDEN+5+I1)=6*XA(1)*XA(2)*QQI2*RM5
            ABCMAT(IDEN+8+I1)=6*XA(1)*XA(3)*QQI2*RM5
            ABCMAT(IDEN+9+I1)=6*XA(3)*XA(2)*QQI2*RM5
   40    CONTINUE
   50 IDEN=IDEN+1+IDEL**2
      I1=NPS2+NDEN*NPS
C  FILLING C-MATRIX
      FACT=-.5D0*2*13.6058D0*.5292D0*FEPSI
      DO 110 I=1,NDEN
         DO 80 K=1,NPS
            BHK=0.D0
            KK2=(K*(K-1))/2
            DO 60 L=1,K
   60       BHK=BHK+ABCMAT(I+L*NDEN+I0)*ABCMAT(KK2+L)
            DO 70 L=K+1,NPS
   70       BHK=BHK+ABCMAT(I+L*NDEN+I0)*ABCMAT((L*(L-1))/2+K)
            BH(K)=BHK
   80    CONTINUE
         DO 100 J=1,I
            CIJ=0.D0
            DO 90 K=1,NPS
   90       CIJ=CIJ+BH(K)*ABCMAT(J+K*NDEN+I0)
            I1=I1+1
            ABCMAT(I1)=FACT*CIJ
  100    CONTINUE
  110 CONTINUE
      I1=NPS2+NDEN*NPS
      DO 120 I=1,NDEN
  120 I1=I1+I
      DO 130 I=1,NPS
         J=IATSP(I)
         RM=SRAD(J)-RDS
         DO 130 IX=1,3
  130 COSURF(IX,I)=(COSURF(IX,I)-COORD(IX,J))/RM
C      CALL DIELEN(EDIE)
      RETURN
      END
