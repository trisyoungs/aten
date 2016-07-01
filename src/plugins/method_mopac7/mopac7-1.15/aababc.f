C
C         Notice of Public Domain nature of this Program
C
C      'This computer program is a work of the United States
C       Government and as such is not subject to protection by
C       copyright (17 U.S.C. # 105.)  Any person who fraudulently
C       places a copyright notice or does any other act contrary
C       to the provisions of 17 U.S. Code 506(c) shall be subject
C       to the penalties provided therein.  This notice shall not
C       be altered or removed from this software and is to be on
C       all reproductions.'
C
      FUNCTION AABABC(IOCCA1, IOCCB1, IOCCA2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS)
***********************************************************************
*
* AABABC EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY BETA ELECTRON. THAT IS, ONE MICROSTATE HAS A BETA ELECTRON
*       IN PSI(I) WHICH, IN THE OTHER MICROSTATE IS IN PSI(J)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /BASEOC/ OCCA(NMECI)
      DO 10 I=1,NMOS
   10 IF(IOCCA1(I).NE.IOCCA2(I)) GOTO 20
   20 IJ=IOCCB1(I)
      DO 30 J=I+1,NMOS
         IF(IOCCA1(J).NE.IOCCA2(J)) GOTO 40
   30 IJ=IJ+IOCCA1(J)+IOCCB1(J)
   40 SUM=0.D0
      DO 50 K=1,NMOS
   50 SUM=SUM+ (XY(I,J,K,K)-XY(I,K,J,K))*(IOCCA1(K)-OCCA(K)) +
     1          XY(I,J,K,K)             *(IOCCB1(K)-OCCA(K))
      IF(MOD(IJ,2).EQ.1)SUM=-SUM
      AABABC=SUM
      RETURN
      END
      FUNCTION AABBCD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* AABBCD EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY TWO SETS OF M.O.S. ONE MICROSTATE HAS AN ALPHA ELECTRON
*       IN PSI(I) AND A BETA ELECTRON IN PSI(K) FOR WHICH THE OTHER
*       MICROSTATE HAS AN ALPHA ELECTRON IN PSI(J) AND A BETA ELECTRON
*       IN PSI(L)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /SPQR/ ISPQR(NMECI*NMECI,NMECI),IS,ILOOP, JLOOP
      DO 10 I=1,NMOS
   10 IF(IOCCA1(I) .NE. IOCCA2(I)) GOTO 20
   20 DO 30 J=I+1,NMOS
   30 IF(IOCCA1(J) .NE. IOCCA2(J)) GOTO 40
   40 DO 50 K=1,NMOS
   50 IF(IOCCB1(K) .NE. IOCCB2(K)) GOTO 60
   60 DO 70 L=K+1,NMOS
   70 IF(IOCCB1(L) .NE. IOCCB2(L)) GOTO 80
   80 IF( I.EQ.K .AND. J.EQ.L .AND. IOCCA1(I).NE.IOCCB1(I)) THEN
         ISPQR(ILOOP,IS)=JLOOP
         IS=IS+1
      ENDIF
      IF(IOCCA1(I) .LT. IOCCA2(I)) THEN
         M=I
         I=J
         J=M
      ENDIF
      IF(IOCCB1(K) .LT. IOCCB2(K)) THEN
         M=K
         K=L
         L=M
      ENDIF
      XR=XY(I,J,K,L)
C#      WRITE(6,'(4I5,F12.6)')I,J,K,L,XR
C
C   NOW UNTANGLE THE MICROSTATES
C
      IJ=1
      IF( I.GT.K .AND. J.GT.L .OR. I.LE.K .AND. J.LE.L)IJ=0
      IF( I.GT.K ) IJ=IJ+IOCCA1(K)+IOCCB1(I)
      IF( J.GT.L ) IJ=IJ+IOCCA2(L)+IOCCB2(J)
      IF(I.GT.K)THEN
         M=I
         I=K
         K=M
      ENDIF
      DO 90 M=I,K
   90 IJ=IJ+IOCCB1(M)+IOCCA1(M)
      IF(J.GT.L)THEN
         M=J
         J=L
         L=M
      ENDIF
      DO 100 M=J,L
  100 IJ=IJ+IOCCB2(M)+IOCCA2(M)
C
C   IJ IN THE PERMUTATION NUMBER, .EQUIV. -1 IF IJ IS ODD.
C
      IF(MOD(IJ,2).EQ.1)XR=-XR
      AABBCD=XR
      RETURN
      END
      FUNCTION AABACD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* AABACD EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY TWO ALPHA MOS. ONE MICROSTATE HAS ALPHA ELECTRONS IN
*       M.O.S PSI(I) AND PSI(J) FOR WHICH THE OTHER MICROSTATE HAS
*       ELECTRONS IN PSI(K) AND PSI(L)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      IJ=0
      DO 10 I=1,NMOS
   10 IF(IOCCA1(I) .LT. IOCCA2(I)) GOTO 20
   20 DO 30 J=I+1,NMOS
         IF(IOCCA1(J) .LT. IOCCA2(J)) GOTO 40
   30 IJ=IJ+IOCCA2(J)+IOCCB2(J)
   40 DO 50 K=1,NMOS
   50 IF(IOCCA1(K) .GT. IOCCA2(K)) GOTO 60
   60 DO 70 L=K+1,NMOS
         IF(IOCCA1(L) .GT. IOCCA2(L)) GOTO 80
   70 IJ=IJ+IOCCA1(L)+IOCCB1(L)
   80 IJ=IJ+IOCCB2(I)+IOCCB1(K)
      SUM=(XY(I,K,J,L)-XY(I,L,K,J))
      IF(MOD(IJ,2).EQ.1)SUM=-SUM
      AABACD=SUM
      RETURN
      END
      FUNCTION BABBBC(IOCCA1, IOCCB1, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* BABBBC EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY ONE BETA ELECTRON. THAT IS, ONE MICROSTATE HAS A BETA
*       ELECTRON IN PSI(I) AND THE OTHER MICROSTATE HAS AN ELECTRON IN
*       PSI(J).
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /BASEOC/ OCCA(NMECI)
      DO 10 I=1,NMOS
   10 IF(IOCCB1(I).NE.IOCCB2(I)) GOTO 20
   20 IJ=0
      DO 30 J=I+1,NMOS
         IF(IOCCB1(J).NE.IOCCB2(J)) GOTO 40
   30 IJ=IJ+IOCCA1(J)+IOCCB1(J)
   40 IJ=IJ+IOCCA1(J)
C
C   THE UNPAIRED M.O.S ARE I AND J
      SUM=0.D0
      DO 50 K=1,NMOS
   50 SUM=SUM+ (XY(I,J,K,K)-XY(I,K,J,K))*(IOCCB1(K)-OCCA(K)) +
     1          XY(I,J,K,K)             *(IOCCA1(K)-OCCA(K))
      IF(MOD(IJ,2).EQ.1)SUM=-SUM
      BABBBC=SUM
      RETURN
      END
      FUNCTION BABBCD(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* BABBCD EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY TWO BETA MOS. ONE MICROSTATE HAS BETA ELECTRONS IN
*       M.O.S PSI(I) AND PSI(J) FOR WHICH THE OTHER MICROSTATE HAS
*       ELECTRONS IN PSI(K) AND PSI(L)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      IJ=0
      DO 10 I=1,NMOS
   10 IF(IOCCB1(I) .LT. IOCCB2(I)) GOTO 20
   20 DO 30 J=I+1,NMOS
         IF(IOCCB1(J) .LT. IOCCB2(J)) GOTO 40
   30 IJ=IJ+IOCCA2(J)+IOCCB2(J)
   40 IJ=IJ+IOCCA2(J)
      DO 50 K=1,NMOS
   50 IF(IOCCB1(K) .GT. IOCCB2(K)) GOTO 60
   60 DO 70 L=K+1,NMOS
         IF(IOCCB1(L) .GT. IOCCB2(L)) GOTO 80
   70 IJ=IJ+IOCCA1(L)+IOCCB1(L)
   80 IJ=IJ+IOCCA1(L)
      IF((IJ/2)*2.EQ.IJ) THEN
         ONE=1.D0
      ELSE
         ONE=-1.D0
      ENDIF
      BABBCD=(XY(I,K,J,L)-XY(I,L,J,K))*ONE
      RETURN
      END
      FUNCTION DIAGI(IALPHA,IBETA,EIGA,XY,NMOS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XY(NMECI,NMECI,NMECI,NMECI), EIGA(NMECI),
     1IALPHA(NMOS), IBETA(NMOS)
************************************************************************
*
*  CALCULATES THE ENERGY OF A MICROSTATE DEFINED BY IALPHA AND IBETA
*
************************************************************************
      X=0.0D0
      DO 20 I=1,NMOS
         IF (IALPHA(I).NE.0)THEN
            X=X+EIGA(I)
            DO 10  J=1,NMOS
               X=X+((XY(I,I,J,J)-XY(I,J,I,J))*IALPHA(J)*0.5D0 +
     1        (XY(I,I,J,J)            )*IBETA(J))
   10       CONTINUE
         ENDIF
   20 CONTINUE
      DO 40 I=1,NMOS
         IF (IBETA(I).NE.0) THEN
            X=X+EIGA(I)
            DO 30 J=1,I
   30       X=X+(XY(I,I,J,J)-XY(I,J,I,J))*IBETA(J)
         ENDIF
   40 CONTINUE
      DIAGI=X
      RETURN
      END
