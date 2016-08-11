      SUBROUTINE MECIH(DIAG,CIMAT,NMOS,LAB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION DIAG(*),CIMAT(*)
C
C     BUILD THE C.I. MATRIX 'CIMAT' IN PACKED CANONICAL FORM.
C
      COMMON /SPQR  / ISPQR(NMECI**2,NMECI),IS,I,K
      COMMON /NALMAT/ NALPHA(NMECI**2)
      COMMON /MICROS/ MICROA(NMECI,4*NMECI**2), MICROB(NMECI,4*NMECI**2)
C
      IK=0
C
C     OUTER LOOP TO FILL C.I. MATRIX.
      DO 30 I=1,LAB
         IS=2
C
C     INNER LOOP.
         DO 20 K=1,I
            IK=IK+1
            CIMAT(IK)=0.D0
            IX=0
            IY=0
            DO 10 J=1,NMOS
               IX=IX+ABS(MICROA(J,I)-MICROA(J,K))
   10       IY=IY+ABS(MICROB(J,I)-MICROB(J,K))
C
C                              CHECK IF MATRIX ELEMENT HAS TO BE ZERO
C
            IF(IX+IY.GT.4 .OR. NALPHA(I).NE.NALPHA(K)) GO TO 20
            IF(IX+IY.EQ.4) THEN
               IF(IX.EQ.0)THEN
                  CIMAT(IK)=BABBCD(MICROA(1,I),MICROB(1,I)
     1                            ,MICROA(1,K),MICROB(1,K),NMOS)
               ELSE IF(IX.EQ.2) THEN
                  CIMAT(IK)=AABBCD(MICROA(1,I),MICROB(1,I)
     1                            ,MICROA(1,K),MICROB(1,K),NMOS)
               ELSE
                  CIMAT(IK)=AABACD(MICROA(1,I),MICROB(1,I)
     1                            ,MICROA(1,K),MICROB(1,K),NMOS)
               ENDIF
            ELSE IF(IX.EQ.2) THEN
               CIMAT(IK)=AABABC(MICROA(1,I),MICROB(1,I)
     1                         ,MICROA(1,K),NMOS)
            ELSE IF(IY.EQ.2) THEN
               CIMAT(IK)=BABBBC(MICROA(1,I),MICROB(1,I)
     1                         ,MICROB(1,K),NMOS)
            ELSE
               CIMAT(IK)=DIAG(I)
            ENDIF
   20    CONTINUE
   30 ISPQR(I,1)=IS-1
      RETURN
      END
