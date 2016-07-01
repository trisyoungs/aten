      SUBROUTINE DCART (COORD,DXYZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,*), DXYZ(3,*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
C***********************************************************************
C
C    DCART CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
C          CARTESIAN COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
C
C    THE MAIN ARRAYS IN DCART ARE:
C        DXYZ   ON EXIT CONTAINS THE CARTESIAN DERIVATIVES.
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /DCARTC/ K1L,K2L,K3L,K1U,K2U,K3U
      COMMON /NUMCAL/ NUMCAL
C COSMO change
      LOGICAL ISEPS, USEPS , UPDA
      COMMON /ISEPS/  ISEPS, USEPS, UPDA
C end of COSMO change
      CHARACTER*241 KEYWRD
      DIMENSION PDI(171),PADI(171),PBDI(171),
     1CDI(3,2),NDI(2),LSTOR1(6), LSTOR2(6), ENG(3)
      LOGICAL DEBUG, FORCE, MAKEP, ANADER, LARGE
      EQUIVALENCE (LSTOR1(1),L1L), (LSTOR2(1), K1L)
      SAVE CHNGE, CHNGE2, ANADER, DEBUG, FORCE
      DATA ICALCN/0/
      DATA CHNGE /1.D-4/
      CHNGE2=CHNGE*0.5D0
*
* CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
* CHNGE2=CHNGE/2
*
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         LARGE = (INDEX(KEYWRD,'LARGE') .NE. 0)
         ANADER= (INDEX(KEYWRD,'ANALYT') .NE. 0)
         DEBUG = (INDEX(KEYWRD,'DCART') .NE. 0)
         FORCE = (INDEX(KEYWRD,'PREC')+INDEX(KEYWRD,'FORCE') .NE. 0)
      ENDIF
      NCELLS=(L1U-L1L+1)*(L2U-L2L+1)*(L3U-L3L+1)
      DO 10 I=1,6
         LSTOR2(I)=LSTOR1(I)
   10 LSTOR1(I)=0
      IOFSET=(NCELLS+1)/2
      NUMTOT=NUMAT*NCELLS
      DO 20 I=1,NUMTOT
         DO 20 J=1,3
   20 DXYZ(J,I)=0.D0
      IF(ANADER) REWIND 2
      DO 130 II=1,NUMAT
         III=NCELLS*(II-1)+IOFSET
         IM1=II
         IF=NFIRST(II)
         IM=NMIDLE(II)
         IL=NLAST(II)
         NDI(2)=NAT(II)
         DO 30 I=1,3
   30    CDI(I,2)=COORD(I,II)
         DO 130 JJ=1,IM1
            JJJ=NCELLS*(JJ-1)
C  FORM DIATOMIC MATRICES
            JF=NFIRST(JJ)
            JM=NMIDLE(JJ)
            JL=NLAST(JJ)
C   GET FIRST ATOM
            NDI(1)=NAT(JJ)
            MAKEP=.TRUE.
            DO 120 IK=K1L,K1U
               DO 120 JK=K2L,K2U
                  DO 120 KL=K3L,K3U
                     JJJ=JJJ+1
*                    KKK=KKK-1
                     DO 40 L=1,3
   40                CDI(L,1)=COORD(L,JJ)+TVEC(L,1)*IK+TVEC(L,2)*JK+TVEC
     1(L,3)*KL
                     IF(.NOT. MAKEP) GOTO 90
                     MAKEP=.FALSE.
                     IJ=0
                     DO 50 I=JF,JL
                        K=I*(I-1)/2+JF-1
                        DO 50 J=JF,I
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   50                PDI(IJ)=P(K)
C GET SECOND ATOM FIRST ATOM INTERSECTION
                     DO 80 I=IF,IL
                        L=I*(I-1)/2
                        K=L+JF-1
                        DO 60 J=JF,JL
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   60                   PDI(IJ)=P(K)
                        K=L+IF-1
                        DO 70 L=IF,I
                           K=K+1
                           IJ=IJ+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   70                   PDI(IJ)=P(K)
   80                CONTINUE
   90                CONTINUE
                     IF(II.EQ.JJ) GOTO  120
                     IF(ANADER)THEN
                        CALL ANALYT(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL
     1,                 ENG)
                        DO 100 K=1,3
                           DXYZ(K,III)=DXYZ(K,III)-ENG(K)
  100                   DXYZ(K,JJJ)=DXYZ(K,JJJ)+ENG(K)
                     ELSE
                        IF( .NOT. FORCE) THEN
                           CDI(1,1)=CDI(1,1)+CHNGE2
                           CDI(2,1)=CDI(2,1)+CHNGE2
                           CDI(3,1)=CDI(3,1)+CHNGE2
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 AA,1)
                        ENDIF
                        DO 110 K=1,3
                           IF( FORCE )THEN
                              CDI(K,2)=CDI(K,2)-CHNGE2
                              CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF
     1,IM,IL,                 AA,1)
                           ENDIF
                           CDI(K,2)=CDI(K,2)+CHNGE
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,IM
     1,IL,                 EE,2)
                           CDI(K,2)=CDI(K,2)-CHNGE2
                           IF( .NOT. FORCE) CDI(K,2)=CDI(K,2)-CHNGE2
                           DERIV=(AA-EE)*23.061D0/CHNGE
                           DXYZ(K,III)=DXYZ(K,III)-DERIV
                           DXYZ(K,JJJ)=DXYZ(K,JJJ)+DERIV
  110                   CONTINUE
                     ENDIF
  120       CONTINUE
  130 CONTINUE
      IF(NNHCO.NE.0)THEN
C
C   NOW ADD IN MOLECULAR-MECHANICS CORRECTION TO THE H-N-C=O TORSION
C
         DEL=1.D-8
         DO 160 I=1,NNHCO
            DO 150 J=1,4
               DO 140 K=1,3
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE)
                  REFH=HTYPE(ITYPE)*SIN(ANGLE)**2
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))+DEL*2.D0
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE)
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  HEAT=HTYPE(ITYPE)*SIN(ANGLE)**2
                  SUM=(REFH-HEAT)/(2.D0*DEL)
                  DXYZ(K,NHCO(J,I))=DXYZ(K,NHCO(J,I))-SUM
  140          CONTINUE
  150       CONTINUE
  160    CONTINUE
      ENDIF
C COSMO change A. Klamt
C analytic calculation of the gradient of the dielectric energy A.Klamt
      IF (USEPS) CALL DIEGRD(COORD,DXYZ)
C     DO 170 I=1,6
C 170 LSTOR1(I)=LSTOR2(I)
      IF (  .NOT. DEBUG) RETURN
      IW = 6
      WRITE(IW,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'',//3X,
     1''NUMBER  ATOM '',5X,''X'',12X,''Y'',12X,''Z'',/)')
      IF(NCELLS.EQ.1)THEN
         WRITE(IW,'(2I6,F13.6,2F13.6)')
     1 (I,NAT(I),(DXYZ(J,I),J=1,3),I=1,NUMTOT)
      ELSEIF(LARGE)THEN
         WRITE(IW,'(2I6,F13.6,2F13.6)')
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I),J=1,3),I=1,NUMTOT)
      ELSE
         WRITE(IW,'(2I6,F13.6,2F13.6)')
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I)+DXYZ(J,I+1)+DXYZ(J,I+2)
     2,J=1,3),I=1,NUMTOT,3)
      ENDIF
      IROT = 2
      IF (ANADER) REWIND IROT
C end of COSMO (A. Klamt) changes
      IF (  .NOT. DEBUG) RETURN
      WRITE(6,'(//10X,''CARTESIAN COORDINATE DERIVATIVES'',//3X,
     1''NUMBER  ATOM '',5X,''X'',12X,''Y'',12X,''Z'',/)')
      IF(NCELLS.EQ.1)THEN
         WRITE(6,'(2I6,F13.6,2F13.6)')
     1 (I,NAT(I),(DXYZ(J,I),J=1,3),I=1,NUMTOT)
      ELSEIF(LARGE)THEN
         WRITE(6,'(2I6,F13.6,2F13.6)')
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I),J=1,3),I=1,NUMTOT)
      ELSE
         WRITE(6,'(2I6,F13.6,2F13.6)')
     1 (I,NAT((I-1)/NCELLS+1),(DXYZ(J,I)+DXYZ(J,I+1)+DXYZ(J,I+2)
     2,J=1,3),I=1,NUMTOT,3)
      ENDIF
      IF (ANADER) REWIND 2
      RETURN
      END
      SUBROUTINE DHC (P,PA,PB,XI,NAT,IF,IM,IL,JF,JM,JL,DENER,MODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), PA(*), PB(*)
      DIMENSION XI(3,*),NFIRST(2),NMIDLE(2),NLAST(2),NAT(*)
C***********************************************************************
C
C  DHC CALCULATES THE ENERGY CONTRIBUTIONS FROM THOSE PAIRS OF ATOMS
C         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV.
C
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
     1       /ONELEC/ USS(107),UPP(107),UDD(107)
      COMMON /EULER / TVEC(3,3), ID
      COMMON /NUMCAL/ NUMCAL
      SAVE ICALCN, WLIM, UHF
      CHARACTER*241 KEYWRD
      LOGICAL UHF, CUTOFF
      DIMENSION H(171), SHMAT(9,9), F(171),
     1          WJ(100), E1B(10), E2A(10), WK(100), W(100),
     2          WJS(100), WKS(100)
      DOUBLE PRECISION WJS, WKS
      DATA ICALCN /0/
      IF( ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         WLIM=4.D0
         IF(ID.EQ.0)WLIM=0.D0
         UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
      ENDIF
      NFIRST(1)=1
      NMIDLE(1)=IM-IF+1
      NLAST(1)=IL-IF+1
      NFIRST(2)=NLAST(1)+1
      NMIDLE(2)=NFIRST(2)+JM-JF
      NLAST(2)=NFIRST(2)+JL-JF
      LINEAR=(NLAST(2)*(NLAST(2)+1))/2
      DO 10 I=1,LINEAR
         F(I)=0.D0
   10 H(I)=0.0D00
      DO 20 I=1,LINEAR
   20 F(I)=H(I)
      JA=NFIRST(2)
      JB=NLAST(2)
      JC=NMIDLE(2)
      IA=NFIRST(1)
      IB=NLAST(1)
      IC=NMIDLE(1)
      J=2
      I=1
      NJ=NAT(2)
      NI=NAT(1)
      CALL H1ELEC(NI,NJ,XI(1,1),XI(1,2),SHMAT)
      IF(NAT(1).EQ.102.OR.NAT(2).EQ.102) THEN
         K=(JB*(JB+1))/2
         DO 30 J=1,K
   30    H(J)=0.D0
      ELSE
         J1=0
         DO 40 J=JA,JB
            JJ=J*(J-1)/2
            J1=J1+1
            I1=0
            DO 40 I=IA,IB
               JJ=JJ+1
               I1=I1+1
               H(JJ)=SHMAT(I1,J1)
               F(JJ)=SHMAT(I1,J1)
   40    CONTINUE
      ENDIF
      KR=1
      IF(ID.EQ.0)THEN
         CALL ROTATE (NJ,NI,XI(1,2),XI(1,1),W(KR),KR,E2A,E1B,ENUCLR,100.
     1D0)
      ELSE
         CALL SOLROT (NJ,NI,XI(1,2),XI(1,1),WJ,WK,KR,E2A,E1B,ENUCLR,100.
     1D0)
      IF(MODE.EQ.1)CUTOFF=(WJ(1).LT.WLIM)
         IF(CUTOFF)THEN
            DO 50 I=1,KR-1
   50       WK(I)=0.D0
         ENDIF
         DO 60 I=1,KR-1
            WJS(I)=WJ(I)
            WKS(I)=WK(I)
   60    CONTINUE
      ENDIF
C
C    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS.
C
      I2=0
      DO 70 I1=IA,IC
         II=I1*(I1-1)/2+IA-1
         DO 70 J1=IA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E1B(I2)
   70 F(II)=F(II)+E1B(I2)
      DO  80 I1=IC+1,IB
         II=(I1*(I1+1))/2
         F(II)=F(II)+E1B(1)
   80 H(II)=H(II)+E1B(1)
      I2=0
      DO 90 I1=JA,JC
         II=I1*(I1-1)/2+JA-1
         DO 90 J1=JA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E2A(I2)
   90 F(II)=F(II)+E2A(I2)
      DO 100 I1=JC+1,JB
         II=(I1*(I1+1))/2
         F(II)=F(II)+E2A(1)
  100 H(II)=H(II)+E2A(1)
      CALL FOCK2(F,P,PA,W, WJS, WKS,2,NAT,NFIRST,NMIDLE,NLAST)
      EE=HELECT(NLAST(2),PA,H,F)
      IF( UHF ) THEN
         DO 110 I=1,LINEAR
  110    F(I)=H(I)
         CALL FOCK2(F,P,PB,W, WJS, WKS,2,NAT,NFIRST,NMIDLE,NLAST)
         EE=EE+HELECT(NLAST(2),PB,H,F)
      ELSE
         EE=EE*2.D0
      ENDIF
      DENER=EE+ENUCLR
      RETURN
C
      END
