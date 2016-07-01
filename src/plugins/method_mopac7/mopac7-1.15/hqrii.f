      SUBROUTINE HQRII(A,N,M,E,V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION A(*), E(N), V(N,M)
*************************************************************
*
* HQRII IS A DIAGONALISATION ROUTINE, WRITTEN BY YOSHITAKA BEPPU OF
*       NAGOYA UNIVERSITY, JAPAN.
*       FOR DETAILS SEE 'COMPUTERS & CHEMISTRY' VOL.6 1982. PAGE 000.
*
* ON INPUT    A       = MATRIX TO BE DIAGONALISED (PACKED CANONICAL)
*             N       = SIZE OF MATRIX TO BE DIAGONALISED.
*             M       = NUMBER OF EIGENVECTORS NEEDED.
*             E       = ARRAY OF SIZE AT LEAST N
*             V       = ARRAY OF SIZE AT LEAST NMAX*M
*
* ON OUTPUT   E       = EIGENVALUES
*             V       = EIGENVECTORS IN ARRAY OF SIZE NMAX*M
*
************************************************************************
      DIMENSION W(5,MAXPAR)
      IF(N.LE.1 .OR. M .LE.1 .OR. M .GT. N) THEN
         IF(N.EQ.1 .AND. M.EQ.1) THEN
            E(1)=A(1)
            V(1,1)=1.D0
            RETURN
         ENDIF
         WRITE(6,'(////10X,''IN HQRII, N ='',I4,'' M ='',I4)')N,M
         STOP
      ENDIF
*
* EPS3 AND EPS ARE MACHINE-PRECISION DEPENDENT
*
      EPS3=1.D-30
      LL=(N*(N+1))/2+1
      EPS=1.D-8
      IORD=-1
      NM1=N-1
      IF(N.EQ.2) GOTO 90
      NM2=N-2
      KRANK=0
C     HOUSEHOLDER TRANSFORMATION
      DO 80 K=1,NM2
         KP1=K+1
         KRANK=KRANK+K
         W(2,K)=A(KRANK)
         SUM=0.D0
         JRANK=KRANK
         DO 10 J=KP1,N
            W(2,J)=A(JRANK+K)
            JRANK=JRANK+J
   10    SUM=W(2,J)**2+SUM
         S=SIGN(SQRT(SUM),W(2,KP1))
         W(1,K)=-S
         W(2,KP1)=W(2,KP1)+S
         A(K+KRANK)=W(2,KP1)
         H=W(2,KP1)*S
         IF(ABS(H).LT.EPS3) GOTO 80
         SUMM=0.D0
         IRANK=KRANK
         DO 50 I=KP1,N
            SUM=0.D0
            DO 20 J=KP1,I
   20       SUM=SUM+A(J+IRANK)*W(2,J)
            IF(I.GE.N) GOTO 40
            IP1=I+1
            JRANK=I*(I+3)/2
            DO 30 J=IP1,N
               SUM=SUM+A(JRANK)*W(2,J)
   30       JRANK=JRANK+J
   40       W(1,I)=SUM/H
            IRANK=IRANK+I
   50    SUMM=W(1,I)*W(2,I)+SUMM
         U=SUMM*0.5D0/H
         JRANK=KRANK
         DO 70 J=KP1,N
            W(1,J)=W(2,J)*U-W(1,J)
            DO 60 I=KP1,J
   60       A(I+JRANK)=W(1,I)*W(2,J)+W(1,J)*W(2,I)+A(I+JRANK)
   70    JRANK=JRANK+J
   80 A(KRANK)=H
   90 W(2,NM1)=A((NM1*(NM1+1))/2)
      W(2,N)=A((N*(N+1))/2)
      W(1,NM1)=A(NM1+(N*(N-1))/2)
      W(1,N)=0.D0
      GERSCH=ABS(W(2,1))+ABS(W(1,1))
      DO 100 I=1,NM1
  100 GERSCH=MAX(ABS(W(2,I+1))+ABS(W(1,I))+ABS(W(1,I+1)),GERSCH)
      DEL=EPS*GERSCH
      DO 110 I=1,N
         W(3,I)=W(1,I)
         E(I)=W(2,I)
  110 V(I,M)=E(I)
      IF(ABS(DEL).LT.EPS3)  GOTO  220
C     QR-METHOD WITH ORIGIN SHIFT
      K=N
  120 L=K
  130 IF(ABS(W(3,L-1)).LT.DEL) GOTO 140
      L=L-1
      IF(L.GT.1)  GOTO 130
  140 IF(L.EQ.K)  GOTO 170
      WW=(E(K-1)+E(K))*0.5D0
      R=E(K)-WW
      Z=SIGN(SQRT(W(3,K-1)**2+R*R),R)+WW
      EE=E(L)-Z
      E(L)=EE
      FF=W(3,L)
      R=SQRT(EE*EE+FF*FF)
      J=L
      GOTO 160
  150 R=SQRT(E(J)**2+W(3,J)**2)
      W(3,J-1)=S*R
      EE=E(J)*C
      FF=W(3,J)*C
  160 C=E(J)/R
      S=W(3,J)/R
      WW=E(J+1)-Z
      E(J)=(FF*C+WW*S)*S+EE+Z
      E(J+1)=C*WW-S*FF
      J=J+1
      IF(J.LT.K) GOTO 150
      W(3,K-1)=E(K)*S
      E(K)=E(K)*C+Z
      GOTO 120
  170 K=K-1
      IF(K.GT.1) GOTO 120
*    *    *    *    *    *    *    *    *    *    *    *    *
*
*   AT THIS POINT THE ARRAY 'E' CONTAINS THE UN-ORDERED EIGENVALUES
*
*    *    *    *    *    *    *    *    *    *    *    *    *
C     STRAIGHT SELECTION SORT OF EIGENVALUES
      SORTER=1.D0
      IF(IORD.LT.0) SORTER=-1.D0
      J=N
  180 L=1
      II=1
      LL=1
      DO 200 I=2,J
         IF((E(I)-E(L))*SORTER .GT. 0.D0) GOTO 190
         L=I
         GOTO 200
  190    II=I
         LL=L
  200 CONTINUE
      IF(II.EQ.LL) GOTO 210
      WW=E(LL)
      E(LL)=E(II)
      E(II)=WW
  210 J=II-1
      IF(J.GE.2) GOTO 180
  220 IF(M.EQ.0) RETURN
***************
*  ORDERING OF EIGENVALUES COMPLETE.
***************
C      INVERSE-ITERATION FOR EIGENVECTORS
      EPS1=1.D-5
      EPS2=0.05D0
      RN=0.D0
      RA=EPS*0.6180339887485D0
C    0.618... IS THE FIBONACCI NUMBER (-1+SQRT(5))/2.
      IG=1
      DO 450 I=1,M
         IM1=I-1
         DO 230 J=1,N
            W(3,J)=0.D0
            W(4,J)=W(1,J)
            W(5,J)=V(J,M)-E(I)
            RN=RN+RA
            IF(RN.GE.EPS) RN=RN-EPS
  230    V(J,I)=RN
         DO 260 J=1,NM1
            IF(ABS(W(5,J)).GE.ABS(W(1,J))) GOTO 240
            W(2,J)=-W(5,J)/W(1,J)
            W(5,J)=W(1,J)
            T=W(5,J+1)
            W(5,J+1)=W(4,J)
            W(4,J)=T
            W(3,J)=W(4,J+1)
            IF(ABS(W(3,J)).LT.EPS3) W(3,J)=DEL
            W(4,J+1)=0.D0
            GOTO 250
  240       IF(ABS(W(5,J)).LT.EPS3) W(5,J)=DEL
            W(2,J)=-W(1,J)/W(5,J)
  250       W(4,J+1)=W(3,J)*W(2,J)+W(4,J+1)
  260    W(5,J+1)=W(4,J)*W(2,J)+W(5,J+1)
         IF(ABS(W(5,N)) .LT. EPS3) W(5,N)=DEL
         DO 320 ITERE=1,5
            IF(ITERE.EQ.1) GOTO 280
            DO 270 J=1,NM1
               IF(ABS(W(3,J)).LT.EPS3) GOTO 270
               T=V(J,I)
               V(J,I)=V(J+1,I)
               V(J+1,I)=T
  270       V(J+1,I)=V(J,I)*W(2,J)+V(J+1,I)
  280       V(N,I)=V(N,I)/W(5,N)
            V(NM1,I)=(V(NM1,I)-V(N,I)*W(4,NM1))/W(5,NM1)
            VN=MAX(ABS(V(N,I)),ABS(V(NM1,I)),1.D-20)
            IF(N.EQ.2) GOTO 300
            K=NM2
  290       V(K,I)=(V(K,I)-V(K+1,I)*W(4,K)-V(K+2,I)*W(3,K))/W(5,K)
            VN=MAX(ABS(V(K,I)),VN,1.D-20)
            K=K-1
            IF(K.GE.1) GOTO 290
  300       S=EPS1/VN
            DO 310 J=1,N
  310       V(J,I)=V(J,I)*S
            IF(ITERE.GT.1 .AND. VN.GT.1) GOTO 330
  320    CONTINUE
C     TRANSFORMATION OF EIGENVECTORS
  330    IF(N.EQ.2) GOTO 380
         KRANK=NM2*(N+1)/2
         KPIV=NM2*NM1/2
         DO 370 K=NM2,1,-1
            KP1=K+1
            IF(ABS(A(KPIV)).LE.EPS3) GOTO 360
            SUM=0.D0
            DO 340 KK=KP1,N
               SUM=SUM+A(KRANK)*V(KK,I)
  340       KRANK=KRANK+KK
            S=-SUM/A(KPIV)
            DO 350 KK=N,KP1,-1
               KRANK=KRANK-KK
  350       V(KK,I)=A(KRANK)*S+V(KK,I)
  360       KPIV=KPIV-K
  370    KRANK=KRANK-KP1
  380    DO 390 J=IG,I
            IF(ABS(E(J)-E(I)) .LT. EPS2) GOTO 400
  390    CONTINUE
         J=I
  400    IG=J
         IF(IG .EQ. I) GOTO 430
C     RE-ORTHOGONALISATION
         DO 420 K=IG,IM1
            SUM=0.D0
            DO 410 J=1,N
  410       SUM=V(J,K)*V(J,I)+SUM
            S=-SUM
            DO 420 J=1,N
  420    V(J,I)=V(J,K)*S+V(J,I)
C     NORMALISATION
  430    SUM=1.D-24
         DO 440 J=1,N
  440    SUM=SUM+V(J,I)**2
         SINV=1.D0/SQRT(SUM)
         DO 450 J=1,N
  450 V(J,I)=V(J,I)*SINV
      RETURN
      END
