      SUBROUTINE H1ELEC(NI,NJ,XI,XJ,SMAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XI(3),XJ(3),SMAT(9,9), BI(9), BJ(9)
C***********************************************************************
C
C  H1ELEC FORMS THE ONE-ELECTRON MATRIX BETWEEN TWO ATOMS.
C
C   ON INPUT    NI   = ATOMIC NO. OF FIRST ATOM.
C               NJ   = ATOMIC NO. OF SECOND ATOM.
C               XI   = COORDINATES OF FIRST ATOM.
C               XJ   = COORDINATES OF SECOND ATOM.
C
C   ON OUTPUT   SMAT = MATRIX OF ONE-ELECTRON INTERACTIONS.
C
C***********************************************************************
      COMMON /BETAS / BETAS(107),BETAP(107),BETAD(107)
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE
      COMMON /BETA3 / BETA3(153)
      COMMON /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
      COMMON /VSIPS / VS(107),VP(107),VD(107)
      COMMON /NATORB/ NATORB(107)
      COMMON /NUMCAL/ NUMCAL
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      SAVE SBITS, XJUC
      DIMENSION SBITS(9,9), LIMS(3,2), XJUC(3)
      CHARACTER*241 KEYWRD
      EQUIVALENCE (L1L,LIMS(1,1))
      DATA ICALCN/0/
      IF(NI.EQ.102.OR.NJ.EQ.102)THEN
         IF(SQRT((XI(1)-XJ(1))**2+
     1        (XI(2)-XJ(2))**2+
     2        (XI(3)-XJ(3))**2) .GT.1.8)THEN
            DO 10 I=1,9
               DO 10 J=1,9
   10       SMAT(I,J)=0.D0
            RETURN
         ENDIF
      ENDIF
      IF(ID.EQ.0) THEN
         IF (ICALCN.NE.NUMCAL) ICALCN=NUMCAL
         CALL DIAT(NI,NJ,XI,XJ,SMAT)
      ELSE
         IF (ICALCN.NE.NUMCAL) THEN
            ICALCN=NUMCAL
            DO 20 I=1,ID
               LIMS(I,1)=-1
   20       LIMS(I,2)= 1
            DO 30 I=ID+1,3
               LIMS(I,1)=0
   30       LIMS(I,2)=0
         ENDIF
         DO 40 I=1,9
            DO 40 J=1,9
   40    SMAT(I,J)=0
         DO 70 I=L1L,L1U
            DO 70 J=L2L,L2U
               DO 70 K=L3L,L3U
                  DO 50 L=1,3
   50             XJUC(L)=XJ(L)+TVEC(L,1)*I+TVEC(L,2)*J+TVEC(L,3)*K
                  CALL DIAT(NI,NJ,XI,XJUC,SBITS)
                  DO 60 L=1,9
                     DO 60 M=1,9
   60             SMAT(L,M)=SMAT(L,M)+SBITS(L,M)
   70    CONTINUE
      ENDIF
      IF(ITYPE.NE.4) GOTO 80
C
C     START OF MNDO, AM1, OR PM3 OPTION
C
      II=MAX(NI,NJ)
      NBOND=(II*(II-1))/2+NI+NJ-II
      IF(NBOND.GT.153)GOTO 90
      BI(1)=BETA3(NBOND)*VS(NI)
      BI(2)=BETA3(NBOND)*VP(NI)
      BI(3)=BI(2)
      BI(4)=BI(2)
      BJ(1)=BETA3(NBOND)*VS(NJ)
      BJ(2)=BETA3(NBOND)*VP(NJ)
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      GOTO 90
   80 CONTINUE
      BI(1)=BETAS(NI)*0.5D0
      BI(2)=BETAP(NI)*0.5D0
      BI(3)=BI(2)
      BI(4)=BI(2)
      BI(5)=BETAD(NI)*0.5D0
      BI(6)=BI(5)
      BI(7)=BI(5)
      BI(8)=BI(5)
      BI(9)=BI(5)
      BJ(1)=BETAS(NJ)*0.5D0
      BJ(2)=BETAP(NJ)*0.5D0
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      BJ(5)=BETAD(NJ)*0.5D0
      BJ(6)=BJ(5)
      BJ(7)=BJ(5)
      BJ(8)=BJ(5)
      BJ(9)=BJ(5)
   90 CONTINUE
      NORBI=NATORB(NI)
      NORBJ=NATORB(NJ)
      IF(NORBI.EQ.9.OR.NORBJ.EQ.9) THEN
C
C    IN THE CALCULATION OF THE ONE-ELECTRON TERMS THE GEOMETRIC MEAN
C    OF THE TWO BETA VALUES IS BEING USED IF ONE OF THE ATOMS
C    CONTAINS D-ORBITALS.
         DO 100 J=1,NORBJ
            DO 100 I=1,NORBI
  100    SMAT(I,J)=-2.0D0*SMAT(I,J)*SQRT(BI(I)*BJ(J))
      ELSE
         DO 110 J=1,NORBJ
            DO 110 I=1,NORBI
  110    SMAT(I,J)=SMAT(I,J)*(BI(I)+BJ(J))
      ENDIF
      RETURN
      END
