      SUBROUTINE DFOCK2(F, PTOT, P, W, NUMAT, NFIRST,
     1NMIDLE, NLAST, NATI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION F(*), PTOT(*),  NFIRST(*), NMIDLE(*),
     1          NLAST(*), P(*), W(*)
C***********************************************************************
C
C     DFOCK2 ADDS THE 2-ELECTRON 2-CENTER REPULSION CONTRIBUTION TO
C     THE FOCK MATRIX DERIVATIVE WITHIN THE NDDO OR MINDO FORMALISMS.
C  INPUT
C     F    : 1-ELECTRON CONTRIBUTIONS DERIVATIVES.
C     PTOT : TOTAL DENSITY MATRIX.
C     P    : ALPHA OR BETA DENSITY MATRIX. = 0.5 * PTOT
C     W    : NON VANISHING TWO-ELECTRON INTEGRAL DERIVATIVES
C            (ORDERED AS DEFINED IN DHCORE).
C     NATI : # OF THE ATOM SUPPORTING THE VARYING CARTESIAN COORDINATE.
C  OUTPUT
C     F    : FOCK MATRIX DERIVATIVE WITH RESPECT TO THE CART. COORD.
C
C***********************************************************************
      COMMON /NUMCAL/ NUMCAL
      COMMON /WORK4 / PTOT2
      COMMON /KEYWRD/ KEYWRD
      SAVE IFACT,I1FACT, ITYPE
      DIMENSION IFACT(MAXORB),
     1I1FACT(MAXORB), JINDEX(256), KINDEX(256), IJPERM(10), LLPERM(10),
     2PK(16), PJA(16), PJB(16), MMPERM(10),
     3PTOT2(NUMATM,16), JJNDEX(256)
      CHARACTER*241 KEYWRD
      DATA ITYPE /1/
      DATA ICALCN/0/
      IF(ICALCN.NE.NUMCAL)THEN
         ICALCN=NUMCAL
         ITYPE=0
      ENDIF
   10 CONTINUE
      GOTO (20,270,70) ITYPE
   20 CONTINUE
C
C   SET UP ARRAY OF LOWER HALF TRIANGLE INDICES (PASCAL'S TRIANGLE)
C
      DO 30 I=1,MAXORB
         IFACT(I)=(I*(I-1))/2
   30 I1FACT(I)=IFACT(I)+I
C
C   SET UP GATHER-SCATTER TYPE ARRAYS FOR USE WITH TWO-ELECTRON
C   INTEGRALS.  JINDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM I
C   INTEGRALS.  JJNDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM J
C               KINDEX ARE THE INDICES OF THE K-INTEGRALS
C
      M=0
      DO 40 I=1,4
         DO 40 J=1,4
            IJ=MIN(I,J)
            JI=I+J-IJ
            DO 40 K=1,4
               IK=MIN(I,K)
               KI=I+K-IK
               DO 40 L=1,4
                  M=M+1
                  KL=MIN(K,L)
                  LK=K+L-KL
                  JL=MIN(J,L)
                  LJ=J+L-JL
                  KINDEX(M)= IFACT(LJ) +JL + 10*( IFACT(KI) +IK) -10
   40 JINDEX(M)=(IFACT(JI) + IJ)*10 + IFACT(LK) + KL - 10
      L=0
      DO 50 I=1,4
         I1=(I-1)*4
         DO 50 J=1,I
            I1=I1+1
            L=L+1
            IJPERM(L)=I1
            MMPERM(L)=IJPERM(L)-16
            LLPERM(L)=(I1-1)*16
   50 CONTINUE
      L=0
      DO 60 I=1,10
         M=MMPERM(I)
         L=LLPERM(I)
         DO 60 K=1,16
            L=L+1
            M=M+16
   60 JJNDEX(L)=JINDEX(M)
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         ITYPE=2
      ELSE
         ITYPE=3
      ENDIF
      GOTO 10
   70 KK=0
      L=0
      DO 90 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         M=0
         DO 80 J=IA,IB
            DO 80 K=IA,IB
               M=M+1
               JK=MIN(J,K)
               KJ=K+J-JK
               JK=JK+(KJ*(KJ-1))/2
               PTOT2(I,M)=PTOT(JK)
   80    CONTINUE
   90 CONTINUE
      II=NATI
      IA=NFIRST(II)
      IB=NLAST(II)
      DO 260 JJ=1,NUMAT
         IF(II.EQ.JJ) GOTO 260
         JA=NFIRST(JJ)
         JB=NLAST(JJ)
*        JC=NMIDLE(JJ)
            IF(IB-IA.GE.3.AND.JB-JA.GE.3)THEN
C
C                         HEAVY-ATOM  - HEAVY-ATOM
C
C   EXTRACT COULOMB TERMS
C
               DO 100 I=1,16
                  PJA(I)=PTOT2(II,I)
  100          PJB(I)=PTOT2(JJ,I)
C
C  COULOMB TERMS
C
               CALL JAB(IA,JA,LLPERM,JINDEX, JJNDEX, PJA,PJB,W(KK+1),
     1F)
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
               IF(IA.GT.JA)THEN
                  L=0
                  DO 110 I=IA,IB
                     DO 110 J=JA,JB
                        L=L+1
  110             PK(L)=P(IFACT(I)+J)
               ELSE
                  L=0
                  DO 120 I=IA,IB
                     DO 120 J=JA,JB
                        L=L+1
  120             PK(L)=P(IFACT(J)+I)
               ENDIF
               I1=IA
               J1=JA
               CALL KAB(IA,JA, PK, W(KK+1), KINDEX, F)
               IA=I1
               JA=J1
               KK=KK+100
            ELSEIF(IB-IA.GE.3)THEN
C
C                         LIGHT-ATOM  - HEAVY-ATOM
C
C
C   COULOMB TERMS
C
               SUMDIA=0.D0
               SUMOFF=0.D0
               LL=I1FACT(JA)
               K=0
               DO 140 I=0,3
                  J1=IFACT(IA+I)+IA-1
                  DO 130 J=0,I-1
                     K=K+1
                     J1=J1+1
                     F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  130             SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                  J1=J1+1
                  K=K+1
                  F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  140          SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
               F(LL)=F(LL)+SUMOFF*2.D0+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
               IF(IA.GT.JA)THEN
                  K=0
                  DO 160 I=IA,IB
                     I1=IFACT(I)+JA
                     SUM=0.D0
                     DO 150 J=IA,IB
                        K=K+1
                        J1=IFACT(J)+JA
  150                SUM=SUM+P(J1)*W(KK+JINDEX(K))
  160             F(I1)=F(I1)-SUM
               ELSE
                  K=0
                  DO 180 I=IA,IB
                     I1=IFACT(JA)+I
                     SUM=0.D0
                     DO 170 J=IA,IB
                        K=K+1
                        J1=IFACT(JA)+J
  170                SUM=SUM+P(J1)*W(KK+JINDEX(K))
  180             F(I1)=F(I1)-SUM
               ENDIF
               KK=KK+10
            ELSEIF(JB-JA.GE.3)THEN
C
C                         HEAVY-ATOM - LIGHT-ATOM
C
C
C   COULOMB TERMS
C
               SUMDIA=0.D0
               SUMOFF=0.D0
               LL=I1FACT(IA)
               K=0
               DO 200 I=0,3
                  J1=IFACT(JA+I)+JA-1
                  DO 190 J=0,I-1
                     K=K+1
                     J1=J1+1
                     F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  190             SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                  J1=J1+1
                  K=K+1
                  F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  200          SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
               F(LL)=F(LL)+SUMOFF*2.D0+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
               IF(IA.GT.JA)THEN
                  K=IFACT(IA)+JA
                  J=0
                  DO 220 I=K,K+3
                     SUM=0.D0
                     DO 210 L=K,K+3
                        J=J+1
  210                SUM=SUM+P(L)*W(KK+JINDEX(J))
  220             F(I)=F(I)-SUM
               ELSE
                  J=0
                  DO 240 K=JA,JA+3
                     I=IFACT(K)+IA
                     SUM=0.D0
                     DO 230 LL=JA,JA+3
                        L=IFACT(LL)+IA
                        J=J+1
  230                SUM=SUM+P(L)*W(KK+JINDEX(J))
  240             F(I)=F(I)-SUM
               ENDIF
               KK=KK+10
            ELSE
C
C                         LIGHT-ATOM - LIGHT-ATOM
C
               I1=I1FACT(IA)
               J1=I1FACT(JA)
               F(I1)=F(I1)+PTOT(J1)*W(KK+1)
               F(J1)=F(J1)+PTOT(I1)*W(KK+1)
               IF(IA.GT.JA)THEN
                  IJ=I1+JA-IA
                  F(IJ)=F(IJ)-P   (IJ)*W(KK+1)
               ELSE
                  IJ=J1+IA-JA
                  F(IJ)=F(IJ)-P   (IJ)*W(KK+1)
               ENDIF
               KK=KK+1
            ENDIF
  260 CONTINUE
C
      RETURN
  270 KR=0
      II=NATI
      IA=NFIRST(II)
      IB=NLAST(II)
      DO 290 JJ=1,NUMAT
         IF (JJ.EQ.II) GO TO 290
         KR=KR+1
         ELREP=W(KR)
         JA=NFIRST(JJ)
         JB=NLAST(JJ)
         DO 280 I=IA,IB
            KA=IFACT(I)
            KK=KA+I
            DO 280 K=JA,JB
               LL=I1FACT(K)
               IF (JA.LT.IA) THEN
                  IK=KA+K
               ELSE
                  IK=LL+I-K
               ENDIF
               F(KK)=F(KK)+PTOT(LL)*ELREP
               F(LL)=F(LL)+PTOT(KK)*ELREP
  280    F(IK)=F(IK)-P(IK)*ELREP
  290 CONTINUE
      RETURN
      END
