      SUBROUTINE UPSURF (COORD)
C This routine is called alternatively to CONSTS during geometry
C optimization. It updates the segments on the SCAS , i.e. their 
C areas and centers, according to a new geometry, but it doesnot create
C a new segmentation. The reason for using UPSURF is to smooth the total
C energy with respect to fluctations arisig from a fluctuating segmentation.
C Be aware that the use of UPSURF brings some 'hysteresis' into the
C Hamiltonian. 
C UPSURF fills the A-matrix and calls the inversion routine. 
C written by A. Klamt, Burscheider Str. 524, 5090 Leverkusen 3, Germany 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*)
      DIMENSION XX(3),XA(3),COBAS(3,NPPA,NUMATM)
      DIMENSION XSP(4,MAXNSS),NSET(MAXNSS,MAXNSS)
C     Added to satisfy the LAPACK call to DGETRI. SJC 1/10/93
      DIMENSION IPIV(MAXNSS)
      DIMENSION DIRTM(3,NPPA)
      LOGICAL DIN(NPPA)
      COMMON / SOLVI / NSPA,NSS,IATSP(MAXNSS),NAR(MAXNSS),NN(2,NUMATM)
      COMMON / SOLVR / DIPL,FEPSI,RDS,DISEX2,TM(3,3,NUMATM),
     &                AAMAT(MAXNSS,MAXNSS),ADMAT(MAXNSS,MAXNSS), 
     &                CCMAT(MAXORB,MAXORB), BBMAT(MAXORB,MAXNSS),
     &                COSURF(3,MAXNSS), SRAD(NUMATM)
      COMMON /DIRVEC/ DIRVEC(3,NPPA), NNX(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /CHANEL/ IFILES(30)
      EQUIVALENCE (IW,IFILES(6))
      EQUIVALENCE (ADMAT,COBAS),(BBMAT,XSP)
      FDIAG=1.05D0*SQRT(NPPA+0.D0)
      AREA=0.0D0
      DS=SQRT(4.D0/NSPA)
      C2DS=COS(2.0D0*DS)
      NSS1=0
      DO 1000 I=1,NUMAT
        DO 4 IX=1,3
4       XA(IX)=COORD(IX,I)
        R=SRAD(I)
        RM=R-RDS
        NSS0=NSS1+1
        IF (IATSP(NSS0) .NE. I) GO TO 1000
        IPS=NSS0
304     IF (IPS .GT. NSS .OR. IATSP(IPS) .NE. I) GO TO 306
        NAR(IPS)=0
        XSP(1,IPS)=0.D0
        XSP(2,IPS)=0.D0
        XSP(3,IPS)=0.D0
        XSP(4,IPS)=ADMAT(IPS,IPS)
        IPS=IPS+1
        GO TO 304
306     NSS1=IPS-1
C TRANSFORM COSURF ACCORDING TO TM(INV)
        DO 5 J=NSS0,NSS1
          XX(1)=COSURF(1,J)
          XX(2)=COSURF(2,J)
          XX(3)=COSURF(3,J)
          COSURF(1,J)=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,I)
          COSURF(2,J)=XX(1)*TM(2,1,I)+XX(2)*TM(2,2,I)+XX(3)*TM(2,3,I)
          COSURF(3,J)=XX(1)*TM(3,1,I)+XX(2)*TM(3,2,I)+XX(3)*TM(3,3,I)
5       CONTINUE
C UPDATE TM
        NN1=NN(1,I)
        NN2=NN(2,I)
        IF (NN1 .EQ. 0) THEN
          TM(1,1,I)=1.D0
          TM(1,2,I)=0.D0
          TM(1,3,I)=0.D0
        ELSE
          DIST1=0.D0
          DO 6 IX=1,3
6         DIST1=DIST1+(XA(IX)-COORD(IX,NN1))**2
          DIST=1.D0/DSQRT(DIST1)
          TM(1,1,I)=(COORD(1,NN1)-XA(1))*DIST
          TM(1,2,I)=(COORD(2,NN1)-XA(2))*DIST
          TM(1,3,I)=(COORD(3,NN1)-XA(3))*DIST
        END IF
        IF (NN2 .EQ. 0) THEN
          TM(2,1,I)=-TM(1,2,I)
          TM(2,2,I)=TM(1,1,I)
          TM(2,3,I)=0.D0
        ELSE
          DIST2=0.D0
          DO 7 IX=1,3
7         DIST2=DIST2+(XA(IX)-COORD(IX,NN2))**2
          DIST=1.D0/DSQRT(DIST2)
          XX(1)=(COORD(1,NN2)-XA(1))*DIST
          XX(2)=(COORD(2,NN2)-XA(2))*DIST
          XX(3)=(COORD(3,NN2)-XA(3))*DIST
          SP=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,I)
          SININV=1.D0/DSQRT(1.D0-SP*SP)
          TM(2,1,I)=(XX(1)-SP*TM(1,1,I))*SININV
          TM(2,2,I)=(XX(2)-SP*TM(1,2,I))*SININV
          TM(2,3,I)=(XX(3)-SP*TM(1,3,I))*SININV
        END IF
        TM(3,1,I)=TM(1,2,I)*TM(2,3,I)-TM(2,2,I)*TM(1,3,I)
        TM(3,2,I)=TM(1,3,I)*TM(2,1,I)-TM(2,3,I)*TM(1,1,I)
        TM(3,3,I)=TM(1,1,I)*TM(2,2,I)-TM(2,1,I)*TM(1,2,I)
C TRANSFORM COSURF ACCORDING TO TM
        DO 35 J=NSS0,NSS1
          XX(1)=COSURF(1,J)
          XX(2)=COSURF(2,J)
          XX(3)=COSURF(3,J)
          COSURF(1,J)=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,1,I)
          COSURF(2,J)=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,2,I)
          COSURF(3,J)=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,3,I)
35      CONTINUE
        DO 45 J=1,NPPA
          XX(1)=DIRVEC(1,J)
          XX(2)=DIRVEC(2,J)
          XX(3)=DIRVEC(3,J)
          DO 45 IX=1,3
            X=XX(1)*TM(1,IX,I)+XX(2)*TM(2,IX,I)+XX(3)*TM(3,IX,I)
            DIRTM(IX,J)=X
            COBAS(IX,J,I)=RM*X+XA(IX)
45      CONTINUE
        DO 2080 J = 1,NPPA
          DIN(J)=.FALSE.
          DO 2020 IX=1,3            
            XX(IX) = XA(IX) + DIRTM(IX,J)* R
2020      CONTINUE        
          DO 2040 K = 1, NUMAT
            IF (K . EQ. I) GO TO 2040
            DIST=0.D0
            DO 2030 IX=1,3
              DIST = DIST + (XX(IX) - COORD(IX,K))**2
2030        CONTINUE
            DIST=SQRT(DIST)-SRAD(K)
            IF (DIST .LT. 0.D0) GO TO 2080
2040      CONTINUE
          DIN(J)=.TRUE.
2080    CONTINUE
        DO 350 J=1,NPPA
          IF (.NOT. DIN(J)) GO TO 350
          SPM=-1.D0
          X1=DIRTM(1,J)
          X2=DIRTM(2,J)
          X3=DIRTM(3,J)
          DO 340 IPS=NSS0,NSS1
            SP=X1*COSURF(1,IPS)+X2*COSURF(2,IPS)+X3*COSURF(3,IPS)
            IF (SP .LT. SPM) GO TO 340
            SPM=SP
            IPM=IPS
340       CONTINUE
          IF (SPM .LT. C2DS) GO TO 350
          NARA=NAR(IPM)+1
          NSET(IPM,NARA)=J
          NAR(IPM)=NARA
          DO 345 IX=1,3
345       XSP(IX,IPM)=XSP(IX,IPM)+DIRTM(IX,J)
350     CONTINUE
        DO 400 IPS=NSS0,NSS1
          DIST=0.D0
          IF (NAR(IPS) .EQ. 0) THEN
            GO TO 400
          END IF
          DO 390 IX=1,3
            X=XSP(IX,IPS)
            DIST=DIST+X*X
390       CONTINUE
          DIST=1.D0/SQRT(DIST)
          DO 391 IX=1,3
391       COSURF(IX,IPS)=XSP(IX,IPS)*DIST
400     CONTINUE
        DO 490 IPS=NSS0,NSS1  
          DO 405 IX=1,3
405       XSP(IX,IPS)=XA(IX)+COSURF(IX,IPS)*RM
490     CONTINUE
1000  CONTINUE
C    
C filling AAMAT
      DO 170 IPS=1,NSS
        I=IATSP(IPS)
        RI=SRAD(I)-RDS
        NARI=NAR(IPS)
        AA=0.D0
        DO 140 K=1,NARI
          J1=NSET(IPS,K)
          AA=AA+.5D0*FDIAG
          X1=DIRVEC(1,J1)
          X2=DIRVEC(2,J1)
          X3=DIRVEC(3,J1)
          DO 140 L=1,K-1
            J2=NSET(IPS,L)
            AA=AA+1.D0/SQRT((X1-DIRVEC(1,J2))**2+
     &             (X2-DIRVEC(2,J2))**2+(X3-DIRVEC(3,J2))**2)
140     CONTINUE
        AA=2*AA/RI 
        IF (NARI .EQ. 0) THEN
          AAMAT(IPS,IPS)=XSP(4,IPS)
        ELSE
          AAMAT(IPS,IPS)=AA
        ENDIF
        DO 141 IX=1,3
141     XA(IX)=XSP(IX,IPS)
        DO 169 JPS=1,IPS-1
          NARJ=NAR(JPS)
          DIST=0.D0
          DO 143 IX=1,3
143       DIST=DIST+(XSP(IX,JPS)-XA(IX))**2
          IF (DIST .LT. DISEX2) THEN
            J=IATSP(JPS)
            AIJ=0.D0
            DO 149 K=1,NARI
              J1=NSET(IPS,K)
              X1=COBAS(1,J1,I)
              X2=COBAS(2,J1,I)
              X3=COBAS(3,J1,I)
              DO 149 L=1,NARJ
                J2=NSET(JPS,L)
                AIJ=AIJ+1.D0/SQRT((COBAS(1,J2,J)-X1)**2+
     &          (COBAS(2,J2,J)-X2)**2+(COBAS(3,J2,J)-X3)**2)
149         CONTINUE
          ELSE
            AIJ=NARI*NARJ/SQRT(DIST)
          END IF
          AAMAT(IPS,JPS)=AIJ
          AAMAT(JPS,IPS)=AIJ
169     CONTINUE
170   CONTINUE
      DO 175 I=1,NSS
        DO 175 J=1,NSS
175   ADMAT(I,J)=AAMAT(I,J)
C invert AAMAX
C     CALL MA22BD(AAMAT,MAXNSS,NSS,BBMAT,ERROR)
C     CALL INVAA(AAMAT,MAXNSS,NSS)
      CALL DGETRF(NSS,NSS,AAMAT,MAXNSS,IPIV,INFO)
      IF (INFO .NE. 0) THEN
         WRITE (IW,*) 'UPSURF: Factorization failed. INFO=',INFO
         STOP
      ENDIF
      CALL DGETRI(NSS,AAMAT,MAXNSS,IPIV,BBMAT,MAXORB*MAXNSS,INFO)
      IF (INFO .NE. 0) THEN
         WRITE (IW,*) 'UPSURF: Inversion failed. INFO=',INFO
         STOP
      ENDIF
      RETURN
      END
