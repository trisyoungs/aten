      SUBROUTINE CONSTS (COORD)
C THIS ROUTINE CONSTRUCTS OR UPDATES THE SOLVENT-ACCESSIBLE
C SURFACE (SAS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XX(3),XA(3),XI(3),XJ(3),XSP(3,LENABC),COORD(3,*)
      DIMENSION NSET(NPPA*NUMATM/2),NSETF(LENABC), N0(2)
      LOGICAL DIN(NPPA),ISUP
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                BH(LENABC)
     4       /SOLVI/  IATSP(LENABC+1),NAR(LENABC), NNX(2,NUMATM)
     x       /SOLVPS/ NPSX, NPS2X
      COMMON /DIRVEC/ DIRVEC(3,NPPA), NN(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      DIMENSION IPIV(LENABC)
      COMMON /AREAVD/ AREA
      COMMON /CHANEL/ IFILES(30)
      EQUIVALENCE(IW,IFILES(6))
      EQUIVALENCE (ABCMAT(LENABC*LENABC+1),XSP)

      NPS  = NPSX
      NPS2 = NPS2X

      ISUP=(NPS.GT.0)
      N0(1)=NPS2
      N0(2)=-NPS
      MAXNPS=SQRT(2*LENAB2+.251)-NDEN-.5
      MAXNPS=MIN(MAXNPS,LENABC)
      IF (MAXNPS .LT. 3*NUMAT) THEN
         WRITE(IW,*)' PARAMETER LENABC MUST BE INCREASED FOR THIS SYSTEM
     1'
         STOP       ' PARAMETER LENABC MUST BE INCREASED FOR THIS SYSTEM
     1'
      ENDIF
      IF (ISUP) THEN
         NPS3=LENABC-NPS
         DO 10 I=NPS,1,-1
            IATSP(NPS3+I)=IATSP(I)
            DO 10 IX=1,3
               COSURF(IX,NPS3+I)=COSURF(IX,I)
   10    CONTINUE
         NPS3=NPS3+1
      END IF
      SDIS=0.D0
      FDIAG=1.05D0*SQRT(NPPA+0.D0)
      INSET=1
      IATSP(LENABC+1)=0
      NPS = 0
      AREA=0.D0
      DO 340 I=1,NUMAT
         DS=SQRT(4.D0/NSPA)
         IF (NAT(I) .EQ. 1) DS=2*DS
         C2DS=COS(2.D0*DS)
         R=SRAD(I)
         RI=R-RDS
         DO 20 IX=1,3
   20    XA(IX)=COORD(IX,I)
         NPS0=NPS+1
         IF(ISUP) THEN
            IF (NPS .GE. NPS3) STOP 'NPS .GT. NPS3'
            NPS2=NPS3
*           IF (IATSP(NPS0) .NE. I) GO TO 340
            DO 30 IPS=NPS2,LENABC+1
   30       IF(IATSP(IPS) .NE. I) GO TO 40
   40       NPS3=IPS
C TRANSFORM COSURF ACCORDING TO TM(INV)
            DO 50 J=NPS2,NPS3-1
               XX(1)=COSURF(1,J)
               XX(2)=COSURF(2,J)
               XX(3)=COSURF(3,J)
               COSURF(1,J)=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,
     1I)
               COSURF(2,J)=XX(1)*TM(2,1,I)+XX(2)*TM(2,2,I)+XX(3)*TM(2,3,
     1I)
               COSURF(3,J)=XX(1)*TM(3,1,I)+XX(2)*TM(3,2,I)+XX(3)*TM(3,3,
     1I)
   50       CONTINUE
            NN1=NN(1,I)
            NN2=NN(2,I)
            NN3=NN(3,I)
         ELSE
C SEARCH FOR 3 NEAREST NEIGHBOR ATOMS
            DIST1=1.D20
            DIST2=1.D20
            DIST3=1.D20
            NN1=0
            NN2=0
            NN3=0
            DO 70 J=1,NUMAT
               IF (J.EQ. I) GO TO 70
               DIST=0.D0
               DO 60 IX=1,3
   60          DIST=DIST+(XA(IX)-COORD(IX,J))**2
               IF (DIST+0.05D0 .LT. DIST3) THEN
                  DIST3=DIST
                  NN3=J
               END IF
               IF (DIST3+0.05D0 .LT. DIST2) THEN
                  DIST=DIST2
                  DIST2=DIST3
                  DIST3=DIST
                  NN3=NN2
                  NN2=J
               END IF
               IF (DIST2+0.05D0 .LT. DIST1) THEN
                  DIST=DIST1
                  DIST1=DIST2
                  DIST2=DIST
                  NN2=NN1
                  NN1=J
               END IF
   70       CONTINUE
            NN(1,I)=NN1
            NN(2,I)=NN2
            NN(3,I)=NN3
         ENDIF
C BUILD NEW TRANSFORMATION MATRIX
         IF (NN1 .EQ. 0) THEN
            TM(1,1,I)=1.D0
            TM(1,2,I)=0.D0
            TM(1,3,I)=0.D0
         ELSE
            DIST1=0.D0
            DO 80 IX=1,3
   80       DIST1=DIST1+(XA(IX)-COORD(IX,NN1))**2
            DIST=1.D0/SQRT(DIST1)
            TM(1,1,I)=(COORD(1,NN1)-XA(1))*DIST
            TM(1,2,I)=(COORD(2,NN1)-XA(2))*DIST
            TM(1,3,I)=(COORD(3,NN1)-XA(3))*DIST
         END IF
   90    IF (NN2 .EQ. 0) THEN
            DIST=SQRT(TM(1,3,I)**2+TM(1,2,I)**2+TM(1,1,I)**2)
            TM(2,1,I)=-TM(1,2,I)/DIST
            TM(2,2,I)=TM(1,1,I)/DIST
            TM(2,3,I)=0.D0
         ELSE
            DIST2=0.D0
            DO 100 IX=1,3
  100       DIST2=DIST2+(XA(IX)-COORD(IX,NN2))**2
            DIST=1.D0/SQRT(DIST2)
            XX(1)=(COORD(1,NN2)-XA(1))*DIST
            XX(2)=(COORD(2,NN2)-XA(2))*DIST
            XX(3)=(COORD(3,NN2)-XA(3))*DIST
            SP=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,I)
            IF (SP*SP .GT. 0.99D0) THEN
               NN2=NN3
               NN3=0
               DIST2=DIST3
               GO TO 90
            END IF
            SININV=1.D0/SQRT(1.D0-SP*SP)
            TM(2,1,I)=(XX(1)-SP*TM(1,1,I))*SININV
            TM(2,2,I)=(XX(2)-SP*TM(1,2,I))*SININV
            TM(2,3,I)=(XX(3)-SP*TM(1,3,I))*SININV
         END IF
         TM(3,1,I)=TM(1,2,I)*TM(2,3,I)-TM(2,2,I)*TM(1,3,I)
         TM(3,2,I)=TM(1,3,I)*TM(2,1,I)-TM(2,3,I)*TM(1,1,I)
         TM(3,3,I)=TM(1,1,I)*TM(2,2,I)-TM(2,1,I)*TM(1,2,I)
C TRANSFORM DIRVEC ACCORDING TO TM
         DO 110 J=1,NPPA
            XX(1)=DIRVEC(1,J)
            XX(2)=DIRVEC(2,J)
            XX(3)=DIRVEC(3,J)
            DO 110 IX=1,3
               X=XX(1)*TM(1,IX,I)+XX(2)*TM(2,IX,I)+XX(3)*TM(3,IX,I)
               DIRTM(IX,J)=X
  110    CONTINUE
C FIND THE POINTS OF THE BASIC GRID ON THE SAS
         NAREA=0
         DO 160 J = 1,NPPA
            DIN(J)=.FALSE.
            DO 130 IX=1,3
               XX(IX) = XA(IX) + DIRTM(IX,J)* R
  130       CONTINUE
            DO 150 K = 1, NUMAT
               IF (K . EQ. I) GO TO 150
               DIST=0.D0
               DO 140 IX=1,3
                  DIST = DIST + (XX(IX) - COORD(IX,K))**2
  140          CONTINUE
               DIST=SQRT(DIST)-SRAD(K)
               IF (DIST .LT. 0) GO TO 160
  150       CONTINUE
            NAREA=NAREA+1
            DIN(J)=.TRUE.
  160    CONTINUE
         IF( NAREA.EQ.0 ) GOTO 340
         AREA=AREA+NAREA*RI*RI
         IF (ISUP) THEN
            DO 120 J=NPS2,NPS3-1
               NPS=NPS+1
               IATSP(NPS)=I
               XX(1)=COSURF(1,J)
               XX(2)=COSURF(2,J)
               XX(3)=COSURF(3,J)
               COSURF(1,NPS)=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,
     11,I)
               COSURF(2,NPS)=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,
     12,I)
               COSURF(3,NPS)=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,
     13,I)
  120       CONTINUE
         ELSE
         I0=2-1/NAT(I)
         JMAX=N0(I0)
         I0=3*(I0-1)*NPPA-3
         DO 45 J=1,JMAX
         NPS=NPS+1
         IATSP(NPS)=I
         XX(1)=ABCMAT(I0+J*3+1)
         XX(2)=ABCMAT(I0+J*3+2)
         XX(3)=ABCMAT(I0+J*3+3)
         COSURF(1,NPS)=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,1,I)
         COSURF(2,NPS)=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,2,I)
         COSURF(3,NPS)=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,3,I)
  45     CONTINUE
         ENDIF
  200    SDIS0=SDIS
         DO 210 IPS=NPS0,NPS
            NAR(IPS)=0
            XSP(1,IPS)=0.D0
            XSP(2,IPS)=0.D0
            XSP(3,IPS)=0.D0
  210    CONTINUE
         DO 250 J=1,NPPA
            IF (.NOT. DIN(J)) GO TO 250
            SPM=-1.D0
            X1=DIRTM(1,J)
            X2=DIRTM(2,J)
            X3=DIRTM(3,J)
            DO 220 IPS=NPS0,NPS
               SP=X1*COSURF(1,IPS)+X2*COSURF(2,IPS)+X3*COSURF(3,IPS)
               IF (SP .LT. SPM) GO TO 220
               SPM=SP
               IPM=IPS
  220       CONTINUE
            IF (SPM .LT. C2DS) THEN
               NPS=NPS+1
               IF (NPS .GT. MAXNPS) THEN
                  WRITE(IW,*) 'NPS IS GREATER THAN MAXNPS-USE SMALLER NS
     1PA'
                  STOP 'NPS GREATER THAN MAXNPS'
               END IF
               DO 230 IX=1,3
  230          COSURF(IX,NPS)=DIRTM(IX,J)
               IATSP(NPS)=I
               GO TO 200
            END IF
            NAR(IPM)=NAR(IPM)+1
            DO 240 IX=1,3
  240       XSP(IX,IPM)=XSP(IX,IPM)+DIRTM(IX,J)
  250    CONTINUE
         SDIS=0.D0
         IPS=NPS0-1
         IF(NPS.LT.IPS) GOTO 200
  260    IPS=IPS+1
  352  IF(NAR(IPS).EQ.0)THEN
       NPS=NPS-1
       IF(NPS.LT.IPS) GOTO 200
       DO 369 JPS=IPS,NPS
       NAR(JPS)=NAR(JPS+1)
       XSP(1,JPS)=XSP(1,JPS+1)
       XSP(2,JPS)=XSP(2,JPS+1)
  369  XSP(3,JPS)=XSP(3,JPS+1)
       GOTO 352
       ENDIF
         DIST=0.D0
         DO 280 IX=1,3
            X=XSP(IX,IPS)
            DIST=DIST+X*X
  280    CONTINUE
         SDIS=SDIS+DIST
         DIST=1.D0/SQRT(DIST)
         DO 290 IX=1,3
  290    COSURF(IX,IPS)=XSP(IX,IPS)*DIST
         IF(IPS.LT.NPS) GOTO 260
         IF (ABS(SDIS-SDIS0) .GT. 1.D-5) GO TO 200
         DO 310 IPS=NPS0,NPS
            NSETF(IPS)=INSET
            INSET=INSET+NAR(IPS)
            NAR(IPS)=0
            DO 300 IX=1,3
  300       XSP(IX,IPS)=XA(IX)+COSURF(IX,IPS)*RI
  310    CONTINUE
         DO 330 J=1,NPPA
            IF (.NOT. DIN(J)) GO TO 330
            SPM=-1.D0
            X1=DIRTM(1,J)
            X2=DIRTM(2,J)
            X3=DIRTM(3,J)
            DO 320 IPS=NPS0,NPS
               SP=X1*COSURF(1,IPS)+X2*COSURF(2,IPS)+X3*COSURF(3,IPS)
               IF (SP .LT. SPM) GO TO 320
               SPM=SP
               IPM=IPS
  320       CONTINUE
            IF (SPM .LT. C2DS) GO TO 330
            NARA=NAR(IPM)
            NSET(NSETF(IPM)+NARA)=J
            NAR(IPM)=NARA+1
  330    CONTINUE
  340 CONTINUE
      AREA=AREA*4.D0*3.14159D0/NPPA
C FILLING AAMAT
      DO 450 IPS=1,NPS
         I=IATSP(IPS)
         RI=SRAD(I)-RDS
         NARI=NAR(IPS)
         NSETFI=NSETF(IPS)
         AA=0.D0
         DO 350 K=NSETFI,NSETFI+NARI-1
            J1=NSET(K)
            AA=AA+FDIAG
            X1=DIRVEC(1,J1)
            X2=DIRVEC(2,J1)
            X3=DIRVEC(3,J1)
            DO 350 L=NSETFI,K-1
               J2=NSET(L)
               AA=AA+2.D0/SQRT((X1-DIRVEC(1,J2))**2+
     1             (X2-DIRVEC(2,J2))**2+(X3-DIRVEC(3,J2))**2)
  350    CONTINUE
         AA=AA/RI/NARI**2
         ABCMAT(IPS+(IPS-1)*NPS)=AA
         DO 360 IX=1,3
            XI(IX)=COORD(IX,I)
  360    XA(IX)=XSP(IX,IPS)
         DO 440 JPS=IPS+1,NPS
            NARJ=NAR(JPS)
            NSETFJ=NSETF(JPS)
            J=IATSP(JPS)
            DIST=0.D0
            DO 370 IX=1,3
               XJ(IX)=COORD(IX,J)-XI(IX)
  370       DIST=DIST+(XSP(IX,JPS)-XA(IX))**2
            IF (DIST .LT. DISEX2) THEN
               RJ=SRAD(J)-RDS
               AIJ=0.D0
               DO 430 K=NSETFI,NSETFI+NARI-1
                  J1=NSET(K)
                  DO 380 IX=1,3
  380             XX(IX)=DIRVEC(IX,J1)*RI
                  IF (I .NE. J) THEN
                     X1=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,1,I)-
     1XJ(1)
                     X2=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,2,I)-
     1XJ(2)
                     X3=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,3,I)-
     1XJ(3)
                     DO 400 L=NSETFJ,NSETFJ+NARJ-1
                        J2=NSET(L)
                        DO 390 IX=1,3
  390                   XX(IX)=DIRVEC(IX,J2)*RJ
                        Y1=XX(1)*TM(1,1,J)+XX(2)*TM(2,1,J)+XX(3)*TM(3,1,
     1J)-X1
                        Y2=XX(1)*TM(1,2,J)+XX(2)*TM(2,2,J)+XX(3)*TM(3,2,
     1J)-X2
                        Y3=XX(1)*TM(1,3,J)+XX(2)*TM(2,3,J)+XX(3)*TM(3,3,
     1J)-X3
                        AIJ=AIJ+1.D0/SQRT(Y1*Y1+Y2*Y2+Y3*Y3)
  400                CONTINUE
                  ELSE
  410                DO 420 L=NSETFJ,NSETFJ+NARJ-1
                        J2=NSET(L)
C                  AA=((DIRVEC(1,J2)*RJ-XX(1))**2+(DIRVEC(2,J2)*RJ
C     &                   -XX(2))**2+(DIRVEC(3,J2)*RJ-XX(3))**2)
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C                       AIJ=AIJ+((DIRVEC(1,J2)*RJ-XX(1))**2+(DIRVEC(2,J2
C    1)*RJ                   -XX(2))**2+(DIRVEC(3,J2)*RJ-XX(3))**2)**-.5
C    2D0
                        AIJ=AIJ+((DIRVEC(1,J2)*RJ-XX(1))**2
     1                          +(DIRVEC(2,J2)*RJ-XX(2))**2
     2                          +(DIRVEC(3,J2)*RJ-XX(3))**2)**(-.5D0)
C ***************************** at 1994-05-25 *****
  420                CONTINUE
                  END IF
  430          CONTINUE
               AIJ=AIJ/NARI/NARJ
            ELSE
               AIJ=1.D0/SQRT(DIST)
            END IF
            ABCMAT(IPS+(JPS-1)*NPS)=AIJ
            ABCMAT(JPS+(IPS-1)*NPS)=AIJ
  440    CONTINUE
  450 CONTINUE
C INVERT A-MATRIX
      CALL DGETRF(NPS,NPS,ABCMAT,NPS,IPIV,INFO)
      IF( INFO.NE.0 ) THEN
          WRITE(*,*) ' DGETRF FAILED WITH ERROR CODE ', INFO
          STOP 'CONSTS'
      ENDIF
      CALL DGETRI(NPS,ABCMAT,NPS,IPIV,XSP, 3*LENABC,INFO)
      IF( INFO.NE.0 ) THEN
          WRITE(*,*) ' DGETRI FAILED WITH ERROR CODE ', INFO
          STOP 'CONSTS'
      ENDIF
C  STORE INV. A-MATRIX AS LOWER TRIANGLE
      II=0
      DO 460 I=1,NPS
         DO 460 J=1,I
            II=II+1
            ABCMAT(II)=ABCMAT(J+(I-1)*NPS)
  460 CONTINUE
      NPS2=II
      RETURN
      END
