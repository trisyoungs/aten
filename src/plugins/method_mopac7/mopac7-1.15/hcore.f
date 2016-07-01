      SUBROUTINE HCORE (COORD,H,W, WJ,WK,ENUCLR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FLDON
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*),H(*), WJ(N2ELEC), WK(N2ELEC), W(N2ELEC)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
     3       /MOLORB/ USPD(MAXORB),DUMY(MAXORB)
     4       /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
      COMMON /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
      COMMON /CORE  / CORE(107)
      COMMON /FIELD / EFIELD(3)
      COMMON /NUMCAL/ NUMCAL
C COSMO change
      LOGICAL ISEPS, USEPS, UPDA
      COMMON /ISEPS/  ISEPS, USEPS, UPDA
C end of COSMO change
************************************************************************
C
C   HCORE GENERATES THE ONE-ELECTRON MATRIX AND TWO ELECTRON INTEGRALS
C         FOR A GIVEN MOLECULE WHOSE GEOMETRY IS DEFINED IN CARTESIAN
C         COORDINATES.
C
C  ON INPUT  COORD   = COORDINATES OF THE MOLECULE.
C
C  ON OUTPUT  H      = ONE-ELECTRON MATRIX.
C             W      = TWO-ELECTRON INTEGRALS.
C             ENUCLR = NUCLEAR ENERGY
************************************************************************
      CHARACTER*241 KEYWRD, TMPKEY
      LOGICAL FIRST,DEBUG
      SAVE FIRST, IONE, CUTOFF, DEBUG
      DIMENSION E1B(10),E2A(10),DI(9,9), WJD(100), WKD(100)
      DATA ICALCN/0/
      FIRST=(ICALCN.NE.NUMCAL)
      ICALCN=NUMCAL
      IF (FIRST) THEN
         IONE=1
         CUTOFF=1.D10
         IF(ID.NE.0)CUTOFF=60.D0
         IF(ID.NE.0)IONE=0
         DEBUG=(INDEX(KEYWRD,'HCORE') .NE. 0)
*******************************************************************
         XF=0.D0
         YF=0.D0
         ZF=0.D0
         TMPKEY=KEYWRD
         I=INDEX(TMPKEY,' FIELD(')
         IF(I.EQ.0) GOTO 6 
C
C   ERASE ALL TEXT FROM TMPKEY EXCEPT FIELD DATA
C
         TMPKEY(:I)=' '
         TMPKEY(INDEX(TMPKEY,')'):)=' '
C
C   READ IN THE EFFECTIVE FIELD IN X,Y,Z COORDINATES
C
         XF=READA(TMPKEY,I)
         I=INDEX(TMPKEY,',')
         IF(I.EQ.0) GOTO 5 
         TMPKEY(I:I)=' '
         YF=READA(TMPKEY,I)
         I=INDEX(TMPKEY,',')
         IF(I.EQ.0) GOTO 5 
         TMPKEY(I:I)=' '
         ZF=READA(TMPKEY,I)
    5    CONTINUE
         WRITE(6,'(/10X,''THE ELECTRIC FIELD IS'',3F10.5)')XF,YF,ZF
         WRITE(6,'(10X,''IN 8*A.U. (8*27.21/0.529 VOLTS/ANGSTROM)'',/)')
    6    CONTINUE
         EFIELD(1)=XF
         EFIELD(2)=YF
         EFIELD(3)=ZF
C**********************************************************************
      ENDIF
      FLDON = .FALSE.
      IF ((EFIELD(1).NE.0.0D00).OR.(EFIELD(2).NE.0.0D00).OR.
     1    (EFIELD(3).NE.0.0D00)) THEN
         FLDCON = 51.4257D00
         FLDON = .TRUE.
      ENDIF
      DO 10 I=1,(NORBS*(NORBS+1))/2
   10 H(I)=0.D0
      ENUCLR=0.D0
      KR=1
      DO 110 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         IC=NMIDLE(I)
         NI=NAT(I)
C
C FIRST WE FILL THE DIAGONALS, AND OFF-DIAGONALS ON THE SAME ATOM
C
         DO 30 I1=IA,IB
            I2=I1*(I1-1)/2+IA-1
            DO 20 J1=IA,I1
               I2=I2+1
               H(I2)=0.D0
               IF (FLDON) THEN
                  IO1 = I1 - IA
                  JO1 = J1 - IA
                  IF ((JO1.EQ.0).AND.(IO1.EQ.1)) THEN
                     HTERME = -0.529177D00*DD(NI)*EFIELD(1)*FLDCON
                     H(I2) = HTERME
                  ENDIF
                  IF ((JO1.EQ.0).AND.(IO1.EQ.2)) THEN
                     HTERME = -0.529177D00*DD(NI)*EFIELD(2)*FLDCON
                     H(I2) = HTERME
                  ENDIF
                  IF ((JO1.EQ.0).AND.(IO1.EQ.3)) THEN
                     HTERME = -0.529177D00*DD(NI)*EFIELD(3)*FLDCON
                     H(I2) = HTERME
                  ENDIF
               ENDIF
   20       CONTINUE
            H(I2) = USPD(I1)
            IF (FLDON) THEN
               FNUC = -(EFIELD(1)*COORD(1,I) + EFIELD(2)*COORD(2,I) +
     1              EFIELD(3)*COORD(3,I))*FLDCON
               H(I2) = H(I2) + FNUC
            ENDIF
   30    CONTINUE
C
C   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX<PSI(LAMBDA)|PSI(SIGMA)>
C
         IM1=I-IONE
         DO 100 J=1,IM1
            HALF=1.D0
            IF(I.EQ.J)HALF=0.5D0
            JA=NFIRST(J)
            JB=NLAST(J)
            JC=NMIDLE(J)
            NJ=NAT(J)
            CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI)
            I2=0
            DO 40 I1=IA,IB
               II=I1*(I1-1)/2+JA-1
               I2=I2+1
               J2=0
               JJ=MIN(I1,JB)
               DO 40 J1=JA,JJ
                  II=II+1
                  J2=J2+1
   40       H(II)=H(II)+DI(I2,J2)
C
C   CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERMS
C   E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
C
            IF(ID.EQ.0) THEN
               CALL ROTATE(NI,NJ,COORD(1,I),COORD(1,J),
     1 W(KR), KR,E1B,E2A,ENUC,CUTOFF)
            ELSE
               KRO=KR
               CALL SOLROT(NI,NJ,COORD(1,I),COORD(1,J),
     1                WJD, WKD,KR,E1B,E2A,ENUC,CUTOFF)
               JJ=0
               DO 50 II=KRO,KR-1
                  JJ=JJ+1
                  WJ(II)=WJD(JJ)
   50          WK(II)=WKD(JJ)
            ENDIF
            ENUCLR = ENUCLR + ENUC
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
C
            I2=0
            DO 60 I1=IA,IC
               II=I1*(I1-1)/2+IA-1
               DO 60 J1=IA,I1
                  II=II+1
                  I2=I2+1
   60       H(II)=H(II)+E1B(I2)*HALF
            DO  70 I1=IC+1,IB
               II=(I1*(I1+1))/2
   70       H(II)=H(II)+E1B(1)*HALF
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
C
            I2=0
            DO 80 I1=JA,JC
               II=I1*(I1-1)/2+JA-1
               DO 80 J1=JA,I1
                  II=II+1
                  I2=I2+1
   80       H(II)=H(II)+E2A(I2)*HALF
            DO 90 I1=JC+1,JB
               II=(I1*(I1+1))/2
   90       H(II)=H(II)+E2A(1)*HALF
  100    CONTINUE
  110 CONTINUE
C COSMO change
C A. KLAMT 16.7.91
      IF (USEPS) THEN
C The following routine adds the dielectric correction for the electron-core
C interaction to the diagonal elements of H
         CALL ADDHCR (H)
C In the following routine the dielectric correction to the core-core-
C interaction is added to ENUCLR
         CALL ADDNUC (ENUCLR)
      ENDIF
C end of COSMO change
      IF( .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''ONE-ELECTRON MATRIX FROM HCORE'')')
      CALL VECPRT(H,NORBS)
      J=MIN(400,KR)
      IF(ID.EQ.0) THEN
         WRITE(6,'(//10X,''TWO-ELECTRON MATRIX IN HCORE''/)')
         WRITE(6,120)(W(I),I=1,J)
      ELSE
         WRITE(6,'(//10X,''TWO-ELECTRON J MATRIX IN HCORE''/)')
         WRITE(6,120)(WJ(I),I=1,J)
         WRITE(6,'(//10X,''TWO-ELECTRON K MATRIX IN HCORE''/)')
         WRITE(6,120)(WK(I),I=1,J)
      ENDIF
  120 FORMAT(10F8.4)
      RETURN
      END
