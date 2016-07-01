      SUBROUTINE DIEGRD (COORD,DXYZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                QS(LENABC)
     4       /SOLVI/  IATSP(LENABC+1),NAR(LENABC), NNX(2,NUMATM)
      COMMON /DIRVEC/ DIRVEC(3,NPPA), NN(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
     3       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
      DIMENSION DXYZ(3,*),DB(0:3,10),XK(3),XL(3),XX(3),COORD(3,NUMATM)
      DO 10 I=1,NPS
         J=IATSP(I)
         RI=SRAD(J)-RDS
         DO 10 IX=1,3
   10 COSURF(IX,I)=COSURF(IX,I)*RI+COORD(IX,J)
      DO 20 I=1,10
         DO 20 IX=1,3
   20 DB(IX,I)=0.D0
      DB(0,1)=1.D0
      CALL CQDEN()
      FACT=-2*13.6058D0*.5292D0*FEPSI*23.061D0
      DO 30 I=1,NPS
   30 QS(I)=0.D0
      I0=NPS2-NDEN
      DO 70 I=1,NPS
         I2=(I*(I-1))/2
         I1=I0+I*NDEN
         POSI=0.D0
         DO 40 J=1,NDEN
   40    POSI=POSI+QDEN(J)*ABCMAT(J+I1)
         DO 50 K=1,I
   50    QS(K)=QS(K)+POSI*ABCMAT(K+I2)
         DO 60 K=I+1,NPS
   60    QS(K)=QS(K)+POSI*ABCMAT(I+(K*(K-1))/2)
   70 CONTINUE
      DO 120 K=1,NPS
         IAK=IATSP(K)
         DO 80 IX=1,3
   80    XK(IX)=COSURF(IX,K)
         QSK=QS(K)
         DO 110 L=1,K-1
            IAL=IATSP(L)
            IF(IAL .EQ. IAK) GO TO 110
            DIST2=0.D0
            DO 90 IX=1,3
               XXX=COSURF(IX,L)-XK(IX)
               XL(IX)=XXX
               DIST2=DIST2+XXX*XXX
   90       CONTINUE
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C           FF=QSK*QS(L)*FACT*DIST2**-1.5D0
            FF=QSK*QS(L)*FACT*DIST2**(-1.5D0)
C ***************************** at 1994-05-25 *****
            DO 100 IX=1,3
               DXYZ(IX,IAK)=DXYZ(IX,IAK)-XL(IX)*FF
               DXYZ(IX,IAL)=DXYZ(IX,IAL)+XL(IX)*FF
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
      DO 190 K=1,NPS
         IAK=IATSP(K)
         DO 130 IX=1,3
  130    XK(IX)=COSURF(IX,K)
         QSK=QS(K)
         IDEN=0
         DO 180 I=1,NUMAT
            IDEL=NLAST(I)-NFIRST(I)
            IF(I .EQ. IAK) GO TO 180
            NATI=NAT(I)
            DIST2=0.D0
            DO 140 IX=1,3
               XXX=XK(IX)-COORD(IX,I)
               XX(IX)=XXX
               DIST2=DIST2+XXX*XXX
  140       CONTINUE
            DDI=DD(NATI)*2*.529177D0
            QQI2=(.529177D0*QQ(NATI))**2
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C           FF0=-QSK*FACT*DIST2**-1.5D0
            FF0=-QSK*FACT*DIST2**(-1.5D0)
C ***************************** at 1994-05-25 *****
            IF(IDEL .EQ. 0) GO TO 150
            RM2=1.D0/DIST2
            RM4=RM2**2
            DB(0,2)=DDI*3*XX(1)*RM2
            DB(0,4)=DDI*3*XX(2)*RM2
            DB(0,7)=DDI*3*XX(3)*RM2
            DB(0,3)=1.D0+QQI2*(15*XX(1)**2*RM2-3.D0)*RM2
            DB(0,6)=1.D0+QQI2*(15*XX(2)**2*RM2-3.D0)*RM2
            DB(0,10)=1.D0+QQI2*(15*XX(3)**2*RM2-3.D0)*RM2
            DB(0,5)=QQI2*30*XX(1)*XX(2)*RM4
            DB(0,8)=QQI2*30*XX(1)*XX(3)*RM4
            DB(0,9)=QQI2*30*XX(3)*XX(2)*RM4
            DB(1,2)=DDI
            DB(2,4)=DB(1,2)
            DB(3,7)=DB(1,2)
            DB(1,3)=6*QQI2*XX(1)*RM2
            DB(2,6)=6*QQI2*XX(2)*RM2
            DB(3,10)=6*QQI2*XX(3)*RM2
            DB(1,5)=DB(2,6)
            DB(2,5)=DB(1,3)
            DB(1,8)=DB(3,10)
            DB(3,8)=DB(1,3)
            DB(2,9)=DB(3,10)
            DB(3,9)=DB(2,6)
  150       DO 170 J=1,1+IDEL*IDEL
               FF=FF0*QDEN(IDEN+J)
               DO 160 IX=1,3
                  DX=(XX(IX)*DB(0,J)-DB(IX,J))*FF
                  DXYZ(IX,IAK)=DXYZ(IX,IAK)+DX
                  DXYZ(IX,I)=DXYZ(IX,I)-DX
  160          CONTINUE
  170       CONTINUE
  180    IDEN=IDEN+1+IDEL**2
  190 CONTINUE
      DO 200 I=1,NPS
         J=IATSP(I)
         RM=SRAD(J)-RDS
         DO 200 IX=1,3
  200 COSURF(IX,I)=(COSURF(IX,I)-COORD(IX,J))/RM
      RETURN
      END
