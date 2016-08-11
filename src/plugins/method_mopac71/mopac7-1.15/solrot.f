      SUBROUTINE SOLROT (NI,NJ,XI,XJ,WJ,WK,KR,E1B,E2A,ENUC,CUTOFF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XI(3), XJ(3), WJ(100), WK(100), E1B(10), E2A(10)
************************************************************************
*
*   SOLROT FORMS THE TWO-ELECTRON TWO-ATOM J AND K INTEGRAL STRINGS.
*          ON EXIT WJ = "J"-TYPE INTEGRALS
*                  WK = "K"-TYPE INTEGRALS
*
*      FOR MOLECULES, WJ = WK.
************************************************************************
      COMMON /EULER / TVEC(3,3), ID
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /NUMCAL/ NUMCAL
      DIMENSION WSUM(100), WBITS(100), LIMS(3,2), XJUC(3), E1BITS(10),
     1E2BITS(10), WMAX(100)
      SAVE ICALCN
      EQUIVALENCE (L1L,LIMS(1,1))
      DATA ICALCN/0/
      IF(ICALCN.NE.NUMCAL)THEN
         ICALCN=NUMCAL
C$DOIT ASIS
         DO 10 I=1,ID
            LIMS(I,1)=-1
   10    LIMS(I,2)= 1
C$DOIT ASIS
         DO 20 I=ID+1,3
            LIMS(I,1)=0
   20    LIMS(I,2)=0
      ENDIF
      ONE=1.D0
      IF(XI(1).EQ.XJ(1) .AND. XI(2).EQ.XJ(2) .AND. XI(3).EQ. XJ(3))
     1ONE=0.5D0
      DO 30 I=1,100
         WMAX(I)=0.D0
         WSUM(I)=0.D0
   30 WBITS(I)=0.D0
      DO 40 I=1,10
         E1B(I)=0.D0
   40 E2A(I)=0.D0
      ENUC=0.D0
      DO 90 I=L1L,L1U
         DO 90 J=L2L,L2U
            DO 90 K=L3L,L3U
C$DOIT ASIS
               DO 50 L=1,3
   50          XJUC(L)=XJ(L)+TVEC(L,1)*I+TVEC(L,2)*J+TVEC(L,3)*K
               KB=1
               CALL ROTATE (NI,NJ,XI,XJUC,WBITS,KB,E1BITS,E2BITS,
     1ENUBIT,CUTOFF)
               KB=KB-1
               DO 60 II=1,KB
   60          WSUM(II)=WSUM(II)+WBITS(II)
               IF(WMAX(1).LT.WBITS(1))THEN
                  DO 70 II=1,KB
   70             WMAX(II)=WBITS(II)
               ENDIF
               DO 80 II=1,10
                  E1B(II)=E1B(II)+E1BITS(II)
   80          E2A(II)=E2A(II)+E2BITS(II)
               ENUC=ENUC+ENUBIT*ONE
   90 CONTINUE
      IF(ONE.LT.0.9D0) THEN
         DO 100 I=1,KB
  100    WMAX(I)=0.D0
      ENDIF
      DO 110 I=1,KB
         WK(I)=WMAX(I)
  110 WJ(I)=WSUM(I)
      KR=KB+KR
      RETURN
      END
