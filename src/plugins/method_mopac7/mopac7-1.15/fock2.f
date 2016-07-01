      SUBROUTINE FOCK2(F, PTOT, P, W, WJ, WK, NUMAT, NAT, NFIRST,
     1NMIDLE, NLAST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION F(*), PTOT(*), WJ(*), WK(*), NFIRST(*), NMIDLE(*),
     1          NLAST(*), P(*), W(*), NAT(*)
      DOUBLE PRECISION WJ,WK
C***********************************************************************
C
C FOCK2 FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK
C MATRIX
C ON INPUT  PTOT = TOTAL DENSITY MATRIX.
C           P    = ALPHA OR BETA DENSITY MATRIX.
C           W    = TWO-ELECTRON INTEGRAL MATRIX.
C
C  ON OUTPUT F   = PARTIAL FOCK MATRIX
C***********************************************************************
      COMMON /EULER / TVEC(3,3), ID
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE
      COMMON /NUMCAL/ NUMCAL
      COMMON /KEYWRD/ KEYWRD
      COMMON /WORK4 / PTOT2
C COSMO change
      LOGICAL ISEPS, USEPS, UPDA
      COMMON /ISEPS/  ISEPS, USEPS, UPDA
C end of COSMO change
      SAVE IFACT,I1FACT, IONE, LID
      DIMENSION IFACT(MAXORB),
     1I1FACT(MAXORB), JINDEX(256), KINDEX(256), IJPERM(10), LLPERM(10),
     2PK(16), PJA(16), PJB(16), MMPERM(10),
     3PTOT2(NUMATM,16), JJNDEX(256)
      CHARACTER*241 KEYWRD
      LOGICAL LID
      DATA ICALCN/0/
      IF(ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
C
C   SET UP ARRAY OF LOWER HALF TRIANGLE INDICES (PASCAL'S TRIANGLE)
C
         DO 10 I=1,MAXORB
            IFACT(I)=(I*(I-1))/2
   10    I1FACT(I)=IFACT(I)+I
C
C   SET UP GATHER-SCATTER TYPE ARRAYS FOR USE WITH TWO-ELECTRON
C   INTEGRALS.  JINDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM I
C   INTEGRALS.  JJNDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM J
C               KINDEX ARE THE INDICES OF THE K-INTEGRALS
C
         M=0
         DO 20 I=1,4
            DO 20 J=1,4
               IJ=MIN(I,J)
               JI=I+J-IJ
               DO 20 K=1,4
                  IK=MIN(I,K)
                  KI=I+K-IK
                  DO 20 L=1,4
                     M=M+1
                     KL=MIN(K,L)
                     LK=K+L-KL
                     JL=MIN(J,L)
                     LJ=J+L-JL
                     KINDEX(M)= IFACT(LJ) +JL + 10*( IFACT(KI) +IK) -10
   20    JINDEX(M)=(IFACT(JI) + IJ)*10 + IFACT(LK) + KL - 10
         L=0
         DO 30 I=1,4
            I1=(I-1)*4
            DO 30 J=1,I
               I1=I1+1
               L=L+1
               IJPERM(L)=I1
               MMPERM(L)=IJPERM(L)-16
               LLPERM(L)=(I1-1)*16
   30    CONTINUE
         L=0
         DO 40 I=1,10
            M=MMPERM(I)
            L=LLPERM(I)
            DO 40 K=1,16
               L=L+1
               M=M+16
   40    JJNDEX(L)=JINDEX(M)
         LID=(ID.EQ.0)
         IONE=1
         IF(ID.NE.0)IONE=0
C
C      END OF INITIALIZATION
C
      ENDIF
      IF(ITYPE.EQ.4) GOTO 200
C
C     START OF MNDO, AM1, OR PM3 OPTION
C
      KK=0
      L=0
      DO 60 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         M=0
         DO 50 J=IA,IB
            DO 50 K=IA,IB
               M=M+1
               JK=MIN(J,K)
               KJ=K+J-JK
               JK=JK+(KJ*(KJ-1))/2
               PTOT2(I,M)=PTOT(JK)
   50    CONTINUE
   60 CONTINUE
      DO 190 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
C
C  IF NUMAT=2 THEN WE ARE IN A DERIVATIVE OR IN A MOLECULE CALCULATION
C
         IF(NUMAT.NE.2)THEN
            IMINUS=II-IONE
         ELSE
            IMINUS=II-1
         ENDIF
         DO 180 JJ=1,IMINUS
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            JC=NMIDLE(JJ)
            IF(LID) THEN
               IF(IB-IA.GE.3.AND.JB-JA.GE.3)THEN
C
C                         HEAVY-ATOM  - HEAVY-ATOM
C
C   EXTRACT COULOMB TERMS
C
                  DO 70 I=1,16
                     PJA(I)=PTOT2(II,I)
   70             PJB(I)=PTOT2(JJ,I)
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
                  L=0
                  DO 80 I=IA,IB
                     I1=IFACT(I)+JA
                     DO 80 J=I1,I1+3
                        L=L+1
   80             PK(L)=P(J)
                  CALL KAB(IA,JA, PK, W(KK+1), KINDEX, F)
                  KK=KK+100
               ELSEIF(IB-IA.GE.3.AND.JA.EQ.JB)THEN
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
                  DO 100 I=0,3
                     J1=IFACT(IA+I)+IA-1
                     DO 90 J=0,I-1
                        K=K+1
                        J1=J1+1
                        F(J1)=F(J1)+PTOT(LL)*W(KK+K)
   90                SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                     J1=J1+1
                     K=K+1
                     F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  100             SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
                  F(LL)=F(LL)+SUMOFF*2.D0+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  K=0
                  DO 120 I=IA,IB
                     I1=IFACT(I)+JA
                     SUM=0.D0
                     DO 110 J=IA,IB
                        K=K+1
                        J1=IFACT(J)+JA
  110                SUM=SUM+P(J1)*W(KK+JINDEX(K))
  120             F(I1)=F(I1)-SUM
                  KK=KK+10
               ELSEIF(JB-JA.GE.3.AND.IA.EQ.IB)THEN
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
                  DO 140 I=0,3
                     J1=IFACT(JA+I)+JA-1
                     DO 130 J=0,I-1
                        K=K+1
                        J1=J1+1
                        F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  130                SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                     J1=J1+1
                     K=K+1
                     F(J1)=F(J1)+PTOT(LL)*W(KK+K)
  140             SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
                  F(LL)=F(LL)+SUMOFF*2.D0+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  K=IFACT(IA)+JA
                  J=0
                  DO 160 I=K,K+3
                     SUM=0.D0
                     DO 150 L=K,K+3
                        J=J+1
  150                SUM=SUM+P(L)*W(KK+JINDEX(J))
  160             F(I)=F(I)-SUM
                  KK=KK+10
               ELSEIF(JB.EQ.JA.AND.IA.EQ.IB)THEN
C
C                         LIGHT-ATOM - LIGHT-ATOM
C
                  I1=I1FACT(IA)
                  J1=I1FACT(JA)
                  IJ=I1+JA-IA
                  F(I1)=F(I1)+PTOT(J1)*W(KK+1)
                  F(J1)=F(J1)+PTOT(I1)*W(KK+1)
                  F(IJ)=F(IJ)-P   (IJ)*W(KK+1)
                  KK=KK+1
               ENDIF
            ELSE
               DO 170 I=IA,IB
                  KA=IFACT(I)
                  DO 170 J=IA,I
                     KB=IFACT(J)
                     IJ=KA+J
                     AA=2.0D00
                     IF (I.EQ.J) AA=1.0D00
                     DO 170 K=JA,JC
                        KC=IFACT(K)
                        IF(I.GE.K) THEN
                           IK=KA+K
                        ELSE
                           IK=0
                        ENDIF
                        IF(J.GE.K) THEN
                           JK=KB+K
                        ELSE
                           JK=0
                        ENDIF
                        DO 170 L=JA,K
                           IF(I.GE.L) THEN
                              IL=KA+L
                           ELSE
                              IL=0
                           ENDIF
                           IF(J.GE.L) THEN
                              JL=KB+L
                           ELSE
                              JL=0
                           ENDIF
                           KL=KC+L
                           BB=2.0D00
                           IF (K.EQ.L) BB=1.0D00
                           KK=KK+1
                           AJ=WJ(KK)
                           AK=WK(KK)
C
C     A  IS THE REPULSION INTEGRAL (I,J/K,L) WHERE ORBITALS I AND J ARE
C     ON ATOM II, AND ORBITALS K AND L ARE ON ATOM JJ.
C     AA AND BB ARE CORRECTION FACTORS SINCE
C     (I,J/K,L)=(J,I/K,L)=(I,J/L,K)=(J,I/L,K)
C     IJ IS THE LOCATION OF THE MATRIX ELEMENTS BETWEEN ATOMIC ORBITALS
C     I AND J.  SIMILARLY FOR IK ETC.
C
C THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK
C MATRIX.  THE CODE HERE IS HARD TO FOLLOW, AND IMPOSSIBLE TO MODIFY!,
C BUT IT WORKS,
                           IF(KL.LE.IJ)THEN
                              IF(I.EQ.K.AND.AA+BB.LT.2.1D0)THEN
                                 BB=BB*0.5D0
                                 AA=AA*0.5D0
                                 F(IJ)=F(IJ)+BB*AJ*PTOT(KL)
                                 F(KL)=F(KL)+AA*AJ*PTOT(IJ)
                              ELSE
                                 F(IJ)=F(IJ)+BB*AJ*PTOT(KL)
                                 F(KL)=F(KL)+AA*AJ*PTOT(IJ)
                                 A=AK*AA*BB*0.25D0
                                 F(IK)=F(IK)-A*P(JL)
                                 F(IL)=F(IL)-A*P(JK)
                                 F(JK)=F(JK)-A*P(IL)
                                 F(JL)=F(JL)-A*P(IK)
                              ENDIF
                           ENDIF
  170          CONTINUE
            ENDIF
  180    CONTINUE
  190 CONTINUE
C COSMO change
C The following routine adds the dielectric corretion to F
      IF (USEPS) CALL ADDFCK (F,P,NUMAT,NAT,NFIRST,NLAST)
C A. Klamt 18.7.91
C end of COSMO change
      RETURN
C
C                    START OF MINDO/3 OPTION
C
  200 KR=0
      DO 230 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IM1=II-IONE
         DO 220 JJ=1,IM1
            KR=KR+1
            IF(LID)THEN
               ELREP=W(KR)
               ELEXC=ELREP
            ELSE
               ELREP=WJ(KR)
               ELEXC=WK(KR)
            ENDIF
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            DO 210 I=IA,IB
               KA=IFACT(I)
               KK=KA+I
               DO 210 K=JA,JB
                  LL=I1FACT(K)
                  IK=KA+K
                  F(KK)=F(KK)+PTOT(LL)*ELREP
                  F(LL)=F(LL)+PTOT(KK)*ELREP
  210       F(IK)=F(IK)-P(IK)*ELEXC
  220    CONTINUE
  230 CONTINUE
      RETURN
      END
      SUBROUTINE JAB(IA,JA,LLPERM,JINDEX, JJNDEX,PJA,PJB,W, F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LLPERM(10), PJA(16), PJB(16), W(*), F(*),
     1JINDEX(256), JJNDEX(256), SUMA(10), SUMB(10)
C
C  FOR VECTOR MACHINES, REMOVE THE ARRAYS  SUMA AND SUMB, UNCOMMENT
C  THE LINES MARKED CVECTOR, AND COMMENT OUT THE SECOND WHOLE PART
C  OF THE SUBROUTINE
CVECTOR                  I=0
CVECTOR                  DO 100 I5=1,4
CVECTOR                  IIA=IA+I5-1
CVECTOR                  IJA=JA+I5-1
CVECTOR                  IOFF=(IIA*(IIA-1))/2+IA-1
CVECTOR                  JOFF=(IJA*(IJA-1))/2+JA-1
CVECTOR                  DO 100 I6=1,I5
CVECTOR                  IOFF=IOFF+1
CVECTOR                  JOFF=JOFF+1
CVECTOR                        I=I+1
CVECTOR                        L=LLPERM(I)
CVECTOR                        SUMA=0
CVECTOR                        SUMB=0
CVECTOR                        DO 90 K=1,16
CVECTOR                           L=L+1
CVECTOR                           SUMB=SUMB+PJA(K)*W(JJNDEX(L))
CVECTOR   90                   SUMA=SUMA+PJB(K)*W(JINDEX(L))
CVECTOR                        F(IOFF)=F(IOFF)+SUMA
CVECTOR  100             F(JOFF)=F(JOFF)+SUMB
      SUMA( 1)=
     1+PJA( 1)*W(  1)+PJA( 2)*W( 11)+PJA( 3)*W( 31)+PJA( 4)*W( 61)
     2+PJA( 5)*W( 11)+PJA( 6)*W( 21)+PJA( 7)*W( 41)+PJA( 8)*W( 71)
     3+PJA( 9)*W( 31)+PJA(10)*W( 41)+PJA(11)*W( 51)+PJA(12)*W( 81)
     4+PJA(13)*W( 61)+PJA(14)*W( 71)+PJA(15)*W( 81)+PJA(16)*W( 91)
      SUMA( 2)=
     1+PJA( 1)*W(  2)+PJA( 2)*W( 12)+PJA( 3)*W( 32)+PJA( 4)*W( 62)
     2+PJA( 5)*W( 12)+PJA( 6)*W( 22)+PJA( 7)*W( 42)+PJA( 8)*W( 72)
     3+PJA( 9)*W( 32)+PJA(10)*W( 42)+PJA(11)*W( 52)+PJA(12)*W( 82)
     4+PJA(13)*W( 62)+PJA(14)*W( 72)+PJA(15)*W( 82)+PJA(16)*W( 92)
      SUMA( 3)=
     1+PJA( 1)*W(  3)+PJA( 2)*W( 13)+PJA( 3)*W( 33)+PJA( 4)*W( 63)
     2+PJA( 5)*W( 13)+PJA( 6)*W( 23)+PJA( 7)*W( 43)+PJA( 8)*W( 73)
     3+PJA( 9)*W( 33)+PJA(10)*W( 43)+PJA(11)*W( 53)+PJA(12)*W( 83)
     4+PJA(13)*W( 63)+PJA(14)*W( 73)+PJA(15)*W( 83)+PJA(16)*W( 93)
      SUMA( 4)=
     1+PJA( 1)*W(  4)+PJA( 2)*W( 14)+PJA( 3)*W( 34)+PJA( 4)*W( 64)
     2+PJA( 5)*W( 14)+PJA( 6)*W( 24)+PJA( 7)*W( 44)+PJA( 8)*W( 74)
     3+PJA( 9)*W( 34)+PJA(10)*W( 44)+PJA(11)*W( 54)+PJA(12)*W( 84)
     4+PJA(13)*W( 64)+PJA(14)*W( 74)+PJA(15)*W( 84)+PJA(16)*W( 94)
      SUMA( 5)=
     1+PJA( 1)*W(  5)+PJA( 2)*W( 15)+PJA( 3)*W( 35)+PJA( 4)*W( 65)
     2+PJA( 5)*W( 15)+PJA( 6)*W( 25)+PJA( 7)*W( 45)+PJA( 8)*W( 75)
     3+PJA( 9)*W( 35)+PJA(10)*W( 45)+PJA(11)*W( 55)+PJA(12)*W( 85)
     4+PJA(13)*W( 65)+PJA(14)*W( 75)+PJA(15)*W( 85)+PJA(16)*W( 95)
      SUMA( 6)=
     1+PJA( 1)*W(  6)+PJA( 2)*W( 16)+PJA( 3)*W( 36)+PJA( 4)*W( 66)
     2+PJA( 5)*W( 16)+PJA( 6)*W( 26)+PJA( 7)*W( 46)+PJA( 8)*W( 76)
     3+PJA( 9)*W( 36)+PJA(10)*W( 46)+PJA(11)*W( 56)+PJA(12)*W( 86)
     4+PJA(13)*W( 66)+PJA(14)*W( 76)+PJA(15)*W( 86)+PJA(16)*W( 96)
      SUMA( 7)=
     1+PJA( 1)*W(  7)+PJA( 2)*W( 17)+PJA( 3)*W( 37)+PJA( 4)*W( 67)
     2+PJA( 5)*W( 17)+PJA( 6)*W( 27)+PJA( 7)*W( 47)+PJA( 8)*W( 77)
     3+PJA( 9)*W( 37)+PJA(10)*W( 47)+PJA(11)*W( 57)+PJA(12)*W( 87)
     4+PJA(13)*W( 67)+PJA(14)*W( 77)+PJA(15)*W( 87)+PJA(16)*W( 97)
      SUMA( 8)=
     1+PJA( 1)*W(  8)+PJA( 2)*W( 18)+PJA( 3)*W( 38)+PJA( 4)*W( 68)
     2+PJA( 5)*W( 18)+PJA( 6)*W( 28)+PJA( 7)*W( 48)+PJA( 8)*W( 78)
     3+PJA( 9)*W( 38)+PJA(10)*W( 48)+PJA(11)*W( 58)+PJA(12)*W( 88)
     4+PJA(13)*W( 68)+PJA(14)*W( 78)+PJA(15)*W( 88)+PJA(16)*W( 98)
      SUMA( 9)=
     1+PJA( 1)*W(  9)+PJA( 2)*W( 19)+PJA( 3)*W( 39)+PJA( 4)*W( 69)
     2+PJA( 5)*W( 19)+PJA( 6)*W( 29)+PJA( 7)*W( 49)+PJA( 8)*W( 79)
     3+PJA( 9)*W( 39)+PJA(10)*W( 49)+PJA(11)*W( 59)+PJA(12)*W( 89)
     4+PJA(13)*W( 69)+PJA(14)*W( 79)+PJA(15)*W( 89)+PJA(16)*W( 99)
      SUMA(10)=
     1+PJA( 1)*W( 10)+PJA( 2)*W( 20)+PJA( 3)*W( 40)+PJA( 4)*W( 70)
     2+PJA( 5)*W( 20)+PJA( 6)*W( 30)+PJA( 7)*W( 50)+PJA( 8)*W( 80)
     3+PJA( 9)*W( 40)+PJA(10)*W( 50)+PJA(11)*W( 60)+PJA(12)*W( 90)
     4+PJA(13)*W( 70)+PJA(14)*W( 80)+PJA(15)*W( 90)+PJA(16)*W(100)
      SUMB( 1)=
     1+PJB( 1)*W(  1)+PJB( 2)*W(  2)+PJB( 3)*W(  4)+PJB( 4)*W(  7)
     2+PJB( 5)*W(  2)+PJB( 6)*W(  3)+PJB( 7)*W(  5)+PJB( 8)*W(  8)
     3+PJB( 9)*W(  4)+PJB(10)*W(  5)+PJB(11)*W(  6)+PJB(12)*W(  9)
     4+PJB(13)*W(  7)+PJB(14)*W(  8)+PJB(15)*W(  9)+PJB(16)*W( 10)
      SUMB( 2)=
     1+PJB( 1)*W( 11)+PJB( 2)*W( 12)+PJB( 3)*W( 14)+PJB( 4)*W( 17)
     2+PJB( 5)*W( 12)+PJB( 6)*W( 13)+PJB( 7)*W( 15)+PJB( 8)*W( 18)
     3+PJB( 9)*W( 14)+PJB(10)*W( 15)+PJB(11)*W( 16)+PJB(12)*W( 19)
     4+PJB(13)*W( 17)+PJB(14)*W( 18)+PJB(15)*W( 19)+PJB(16)*W( 20)
      SUMB( 3)=
     1+PJB( 1)*W( 21)+PJB( 2)*W( 22)+PJB( 3)*W( 24)+PJB( 4)*W( 27)
     2+PJB( 5)*W( 22)+PJB( 6)*W( 23)+PJB( 7)*W( 25)+PJB( 8)*W( 28)
     3+PJB( 9)*W( 24)+PJB(10)*W( 25)+PJB(11)*W( 26)+PJB(12)*W( 29)
     4+PJB(13)*W( 27)+PJB(14)*W( 28)+PJB(15)*W( 29)+PJB(16)*W( 30)
      SUMB( 4)=
     1+PJB( 1)*W( 31)+PJB( 2)*W( 32)+PJB( 3)*W( 34)+PJB( 4)*W( 37)
     2+PJB( 5)*W( 32)+PJB( 6)*W( 33)+PJB( 7)*W( 35)+PJB( 8)*W( 38)
     3+PJB( 9)*W( 34)+PJB(10)*W( 35)+PJB(11)*W( 36)+PJB(12)*W( 39)
     4+PJB(13)*W( 37)+PJB(14)*W( 38)+PJB(15)*W( 39)+PJB(16)*W( 40)
      SUMB( 5)=
     1+PJB( 1)*W( 41)+PJB( 2)*W( 42)+PJB( 3)*W( 44)+PJB( 4)*W( 47)
     2+PJB( 5)*W( 42)+PJB( 6)*W( 43)+PJB( 7)*W( 45)+PJB( 8)*W( 48)
     3+PJB( 9)*W( 44)+PJB(10)*W( 45)+PJB(11)*W( 46)+PJB(12)*W( 49)
     4+PJB(13)*W( 47)+PJB(14)*W( 48)+PJB(15)*W( 49)+PJB(16)*W( 50)
      SUMB( 6)=
     1+PJB( 1)*W( 51)+PJB( 2)*W( 52)+PJB( 3)*W( 54)+PJB( 4)*W( 57)
     2+PJB( 5)*W( 52)+PJB( 6)*W( 53)+PJB( 7)*W( 55)+PJB( 8)*W( 58)
     3+PJB( 9)*W( 54)+PJB(10)*W( 55)+PJB(11)*W( 56)+PJB(12)*W( 59)
     4+PJB(13)*W( 57)+PJB(14)*W( 58)+PJB(15)*W( 59)+PJB(16)*W( 60)
      SUMB( 7)=
     1+PJB( 1)*W( 61)+PJB( 2)*W( 62)+PJB( 3)*W( 64)+PJB( 4)*W( 67)
     2+PJB( 5)*W( 62)+PJB( 6)*W( 63)+PJB( 7)*W( 65)+PJB( 8)*W( 68)
     3+PJB( 9)*W( 64)+PJB(10)*W( 65)+PJB(11)*W( 66)+PJB(12)*W( 69)
     4+PJB(13)*W( 67)+PJB(14)*W( 68)+PJB(15)*W( 69)+PJB(16)*W( 70)
      SUMB( 8)=
     1+PJB( 1)*W( 71)+PJB( 2)*W( 72)+PJB( 3)*W( 74)+PJB( 4)*W( 77)
     2+PJB( 5)*W( 72)+PJB( 6)*W( 73)+PJB( 7)*W( 75)+PJB( 8)*W( 78)
     3+PJB( 9)*W( 74)+PJB(10)*W( 75)+PJB(11)*W( 76)+PJB(12)*W( 79)
     4+PJB(13)*W( 77)+PJB(14)*W( 78)+PJB(15)*W( 79)+PJB(16)*W( 80)
      SUMB( 9)=
     1+PJB( 1)*W( 81)+PJB( 2)*W( 82)+PJB( 3)*W( 84)+PJB( 4)*W( 87)
     2+PJB( 5)*W( 82)+PJB( 6)*W( 83)+PJB( 7)*W( 85)+PJB( 8)*W( 88)
     3+PJB( 9)*W( 84)+PJB(10)*W( 85)+PJB(11)*W( 86)+PJB(12)*W( 89)
     4+PJB(13)*W( 87)+PJB(14)*W( 88)+PJB(15)*W( 89)+PJB(16)*W( 90)
      SUMB(10)=
     1+PJB( 1)*W( 91)+PJB( 2)*W( 92)+PJB( 3)*W( 94)+PJB( 4)*W( 97)
     2+PJB( 5)*W( 92)+PJB( 6)*W( 93)+PJB( 7)*W( 95)+PJB( 8)*W( 98)
     3+PJB( 9)*W( 94)+PJB(10)*W( 95)+PJB(11)*W( 96)+PJB(12)*W( 99)
     4+PJB(13)*W( 97)+PJB(14)*W( 98)+PJB(15)*W( 99)+PJB(16)*W(100)
      I=0
      DO 10 I5=1,4
         IIA=IA+I5-1
         IJA=JA+I5-1
         IOFF=(IIA*(IIA-1))/2+IA-1
         JOFF=(IJA*(IJA-1))/2+JA-1
         DO 10 I6=1,I5
            IOFF=IOFF+1
            JOFF=JOFF+1
            I=I+1
            F(IOFF)=F(IOFF)+SUMB(I)
   10 F(JOFF)=F(JOFF)+SUMA(I)
      RETURN
      END
      SUBROUTINE KAB(IA,JA, PK, W, KINDEX, F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PK(*), W(*), F(*),KINDEX(256), SUM(16)
C
C  FOR VECTOR MACHINES, REMOVE THE ARRAY SUM, UNCOMMENT THE LINES
C  MARKED CVECTOR, AND COMMENT OUT THE SECOND WHOLE PART OF THE
C  SUBROUTINE
C
CVECTOR                  L=0
CVECTOR                  M=0
CVECTOR                  DO 130 J1=IA,IA+3
CVECTOR                  J=(J1*(J1-1))/2
CVECTOR                  DO 130 J2=JA,JA+3
CVECTOR                  M=M+1
CVECTOR                  IF(IA.GT.JA)THEN
CVECTOR                  J3=J+J2
CVECTOR                  ELSE
CVECTOR                  J3=J1+(J2*(J2-1))/2
CVECTOR                  ENDIF
CVECTOR                     SUM=0
CVECTOR                     DO 120 I=1,16
CVECTOR                        L=L+1
CVECTOR  120                SUM=SUM+PK(I)*W(KINDEX(L))
CVECTOR  130             F(J3)=F(J3)-SUM
      SUM( 1)=
     1+PK( 1)*W(  1)+PK( 2)*W(  2)+PK( 3)*W(  4)+PK( 4)*W(  7)
     2+PK( 5)*W( 11)+PK( 6)*W( 12)+PK( 7)*W( 14)+PK( 8)*W( 17)
     3+PK( 9)*W( 31)+PK(10)*W( 32)+PK(11)*W( 34)+PK(12)*W( 37)
     4+PK(13)*W( 61)+PK(14)*W( 62)+PK(15)*W( 64)+PK(16)*W( 67)
      SUM( 2)=
     1+PK( 1)*W(  2)+PK( 2)*W(  3)+PK( 3)*W(  5)+PK( 4)*W(  8)
     2+PK( 5)*W( 12)+PK( 6)*W( 13)+PK( 7)*W( 15)+PK( 8)*W( 18)
     3+PK( 9)*W( 32)+PK(10)*W( 33)+PK(11)*W( 35)+PK(12)*W( 38)
     4+PK(13)*W( 62)+PK(14)*W( 63)+PK(15)*W( 65)+PK(16)*W( 68)
      SUM( 3)=
     1+PK( 1)*W(  4)+PK( 2)*W(  5)+PK( 3)*W(  6)+PK( 4)*W(  9)
     2+PK( 5)*W( 14)+PK( 6)*W( 15)+PK( 7)*W( 16)+PK( 8)*W( 19)
     3+PK( 9)*W( 34)+PK(10)*W( 35)+PK(11)*W( 36)+PK(12)*W( 39)
     4+PK(13)*W( 64)+PK(14)*W( 65)+PK(15)*W( 66)+PK(16)*W( 69)
      SUM( 4)=
     1+PK( 1)*W(  7)+PK( 2)*W(  8)+PK( 3)*W(  9)+PK( 4)*W( 10)
     2+PK( 5)*W( 17)+PK( 6)*W( 18)+PK( 7)*W( 19)+PK( 8)*W( 20)
     3+PK( 9)*W( 37)+PK(10)*W( 38)+PK(11)*W( 39)+PK(12)*W( 40)
     4+PK(13)*W( 67)+PK(14)*W( 68)+PK(15)*W( 69)+PK(16)*W( 70)
      SUM( 5)=
     1+PK( 1)*W( 11)+PK( 2)*W( 12)+PK( 3)*W( 14)+PK( 4)*W( 17)
     2+PK( 5)*W( 21)+PK( 6)*W( 22)+PK( 7)*W( 24)+PK( 8)*W( 27)
     3+PK( 9)*W( 41)+PK(10)*W( 42)+PK(11)*W( 44)+PK(12)*W( 47)
     4+PK(13)*W( 71)+PK(14)*W( 72)+PK(15)*W( 74)+PK(16)*W( 77)
      SUM( 6)=
     1+PK( 1)*W( 12)+PK( 2)*W( 13)+PK( 3)*W( 15)+PK( 4)*W( 18)
     2+PK( 5)*W( 22)+PK( 6)*W( 23)+PK( 7)*W( 25)+PK( 8)*W( 28)
     3+PK( 9)*W( 42)+PK(10)*W( 43)+PK(11)*W( 45)+PK(12)*W( 48)
     4+PK(13)*W( 72)+PK(14)*W( 73)+PK(15)*W( 75)+PK(16)*W( 78)
      SUM( 7)=
     1+PK( 1)*W( 14)+PK( 2)*W( 15)+PK( 3)*W( 16)+PK( 4)*W( 19)
     2+PK( 5)*W( 24)+PK( 6)*W( 25)+PK( 7)*W( 26)+PK( 8)*W( 29)
     3+PK( 9)*W( 44)+PK(10)*W( 45)+PK(11)*W( 46)+PK(12)*W( 49)
     4+PK(13)*W( 74)+PK(14)*W( 75)+PK(15)*W( 76)+PK(16)*W( 79)
      SUM( 8)=
     1+PK( 1)*W( 17)+PK( 2)*W( 18)+PK( 3)*W( 19)+PK( 4)*W( 20)
     2+PK( 5)*W( 27)+PK( 6)*W( 28)+PK( 7)*W( 29)+PK( 8)*W( 30)
     3+PK( 9)*W( 47)+PK(10)*W( 48)+PK(11)*W( 49)+PK(12)*W( 50)
     4+PK(13)*W( 77)+PK(14)*W( 78)+PK(15)*W( 79)+PK(16)*W( 80)
      SUM( 9)=
     1+PK( 1)*W( 31)+PK( 2)*W( 32)+PK( 3)*W( 34)+PK( 4)*W( 37)
     2+PK( 5)*W( 41)+PK( 6)*W( 42)+PK( 7)*W( 44)+PK( 8)*W( 47)
     3+PK( 9)*W( 51)+PK(10)*W( 52)+PK(11)*W( 54)+PK(12)*W( 57)
     4+PK(13)*W( 81)+PK(14)*W( 82)+PK(15)*W( 84)+PK(16)*W( 87)
      SUM(10)=
     1+PK( 1)*W( 32)+PK( 2)*W( 33)+PK( 3)*W( 35)+PK( 4)*W( 38)
     2+PK( 5)*W( 42)+PK( 6)*W( 43)+PK( 7)*W( 45)+PK( 8)*W( 48)
     3+PK( 9)*W( 52)+PK(10)*W( 53)+PK(11)*W( 55)+PK(12)*W( 58)
     4+PK(13)*W( 82)+PK(14)*W( 83)+PK(15)*W( 85)+PK(16)*W( 88)
      SUM(11)=
     1+PK( 1)*W( 34)+PK( 2)*W( 35)+PK( 3)*W( 36)+PK( 4)*W( 39)
     2+PK( 5)*W( 44)+PK( 6)*W( 45)+PK( 7)*W( 46)+PK( 8)*W( 49)
     3+PK( 9)*W( 54)+PK(10)*W( 55)+PK(11)*W( 56)+PK(12)*W( 59)
     4+PK(13)*W( 84)+PK(14)*W( 85)+PK(15)*W( 86)+PK(16)*W( 89)
      SUM(12)=
     1+PK( 1)*W( 37)+PK( 2)*W( 38)+PK( 3)*W( 39)+PK( 4)*W( 40)
     2+PK( 5)*W( 47)+PK( 6)*W( 48)+PK( 7)*W( 49)+PK( 8)*W( 50)
     3+PK( 9)*W( 57)+PK(10)*W( 58)+PK(11)*W( 59)+PK(12)*W( 60)
     4+PK(13)*W( 87)+PK(14)*W( 88)+PK(15)*W( 89)+PK(16)*W( 90)
      SUM(13)=
     1+PK( 1)*W( 61)+PK( 2)*W( 62)+PK( 3)*W( 64)+PK( 4)*W( 67)
     2+PK( 5)*W( 71)+PK( 6)*W( 72)+PK( 7)*W( 74)+PK( 8)*W( 77)
     3+PK( 9)*W( 81)+PK(10)*W( 82)+PK(11)*W( 84)+PK(12)*W( 87)
     4+PK(13)*W( 91)+PK(14)*W( 92)+PK(15)*W( 94)+PK(16)*W( 97)
      SUM(14)=
     1+PK( 1)*W( 62)+PK( 2)*W( 63)+PK( 3)*W( 65)+PK( 4)*W( 68)
     2+PK( 5)*W( 72)+PK( 6)*W( 73)+PK( 7)*W( 75)+PK( 8)*W( 78)
     3+PK( 9)*W( 82)+PK(10)*W( 83)+PK(11)*W( 85)+PK(12)*W( 88)
     4+PK(13)*W( 92)+PK(14)*W( 93)+PK(15)*W( 95)+PK(16)*W( 98)
      SUM(15)=
     1+PK( 1)*W( 64)+PK( 2)*W( 65)+PK( 3)*W( 66)+PK( 4)*W( 69)
     2+PK( 5)*W( 74)+PK( 6)*W( 75)+PK( 7)*W( 76)+PK( 8)*W( 79)
     3+PK( 9)*W( 84)+PK(10)*W( 85)+PK(11)*W( 86)+PK(12)*W( 89)
     4+PK(13)*W( 94)+PK(14)*W( 95)+PK(15)*W( 96)+PK(16)*W( 99)
      SUM(16)=
     1+PK( 1)*W( 67)+PK( 2)*W( 68)+PK( 3)*W( 69)+PK( 4)*W( 70)
     2+PK( 5)*W( 77)+PK( 6)*W( 78)+PK( 7)*W( 79)+PK( 8)*W( 80)
     3+PK( 9)*W( 87)+PK(10)*W( 88)+PK(11)*W( 89)+PK(12)*W( 90)
     4+PK(13)*W( 97)+PK(14)*W( 98)+PK(15)*W( 99)+PK(16)*W(100)
      IF(IA.GT.JA)THEN
         M=0
         DO 10 J1=IA,IA+3
            J=(J1*(J1-1))/2
            DO 10 J2=JA,JA+3
               M=M+1
               J3=J+J2
   10    F(J3)=F(J3)-SUM(M)
      ELSE
C
C   IA IS LESS THAN JA, THEREFORE USE OTHER HALF OF TRIANGLE
C
         M=0
         DO 20 J1=IA,IA+3
            DO 20 J2=JA,JA+3
               M=M+1
               J3=(J2*(J2-1))/2+J1
   20    F(J3)=F(J3)-SUM(M)
      ENDIF
      RETURN
      END
