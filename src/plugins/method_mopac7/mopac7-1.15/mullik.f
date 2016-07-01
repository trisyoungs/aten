      SUBROUTINE MULLIK(C,H,F,NORBS,VECS,STORE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION C(*), H(*), VECS(*), STORE(*), F(*)
**********************************************************************
*
*  MULLIK DOES A MULLIKEN POPULATION ANALYSIS
* ON INPUT     C      =  SQUARE ARRAY OF EIGENVECTORS.
*              H      =  PACKED ARRAY OF ONE-ELECTRON MATRIX
*              F      =  WORKSTORE OF SIZE AT LEAST NORBS*NORBS
*              VECS   =  WORKSTORE OF SIZE AT LEAST NORBS*NORBS
*              STORE  =  WORKSTORE OF SIZE AT LEAST (NORBS*(NORBS+1))/2
*
**********************************************************************
      COMMON
     1       /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     2                NLAST(NUMATM), NORBX, NELECS,NALPHA,NBETA,
     3                NCLOSE,NOPEN,NDUMY,FRACT
     4       /KEYWRD/ KEYWRD
     5       /BETAS / BETAS(107),BETAP(107),BETAD(107)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /EXPONT/ ZS(107),ZP(107),ZD(107)
      CHARACTER KEYWRD*241, GETNAM*80
      LOGICAL GRAPH
**********************************************************************
*
*  FIRST, RE-CALCULATE THE OVERLAP MATRIX
*
**********************************************************************
      DIMENSION EIGS(MAXORB), IFACT(MAXORB), XYZ(3,NUMATM)
      GRAPH=(INDEX(KEYWRD,'GRAPH').NE.0)
      DO 10 I=1,NORBS
   10 IFACT(I)=(I*(I-1))/2
      IFACT(NORBS+1)=(NORBS*(NORBS+1))/2
      DO 50 I=1,NUMAT
         IF=NFIRST(I)
         IL=NLAST(I)
         IM1=I-1
         BI=BETAS(NAT(I))
         DO 50 K=IF,IL
            II=(K*(K-1))/2
            DO 30 J=1,IM1
               JF=NFIRST(J)
               JL=NLAST(J)
               BJ=BETAS(NAT(J))
               DO 20 JJ=JF,JL
                  IJ=II+JJ
                  H(IJ)=2.D0*H(IJ)/(BI+BJ)     +1.D-14
C  THE  +1.D-14 IS TO PREVENT POSSIBLE ERRORS IN THE DIAGONALIZATION.
                  STORE(IJ)=H(IJ)
   20          BJ=BETAP(NAT(J))
   30       CONTINUE
            DO 40 JJ=IF,K
               IJ=II+JJ
               STORE(IJ)=0.D0
   40       H(IJ)=0.D0
   50 BI=BETAP(NAT(I))
      DO 60 I=1,NORBS
         STORE(IFACT(I+1))=1.D0
   60 H(IFACT(I+1))=1.D0
      CALL RSP(H,NORBS,NORBS,EIGS,VECS)
      DO 70 I=1,NORBS
   70 EIGS(I)=1.D0/SQRT(ABS(EIGS(I)))
      IJ=0
      DO 90 I=1,NORBS
         DO 90 J=1,I
            IJ=IJ+1
            SUM=0.D0
            DO 80 K=1,NORBS
   80       SUM=SUM+VECS(I+(K-1)*NORBS)*EIGS(K)
     1                *VECS(J+(K-1)*NORBS)
            F(I+(J-1)*NORBS)=SUM
   90 F(J+(I-1)*NORBS)=SUM
      IF (GRAPH) THEN
         CALL GMETRY(GEO,XYZ)
*
* WRITE TO DISK THE FOLLOWING DATA FOR GRAPHICS CALCULATION, IN ORDER:
*
*      NUMBER OF ATOMS, ORBITAL, ELECTRONS
*      ALL ATOMIC COORDINATES
*      ORBITAL COUNTERS
*      ORBITAL EXPONENTS, S, P, AND D, AND ATOMIC NUMBERS
*      EIGENVECTORS (M.O.S NOT RE-NORMALIZED)
*      INVERSE-SQUARE ROOT OF THE OVERLAP MATRIX.
*
      OPEN(UNIT=13,FILE=GETNAM('FOR013'),FORM='UNFORMATTED',
     +STATUS='NEW',ERR=31)
      GOTO 32
  31  OPEN(UNIT=13,FILE=GETNAM('FOR013'),STATUS='OLD',
     +FORM='UNFORMATTED')
  32  CONTINUE
         WRITE(13)NUMAT,NORBS,NELECS,((XYZ(I,J),J=1,NUMAT),I=1,3)
         WRITE(13)(NLAST(I),NFIRST(I),I=1,NUMAT)
         WRITE(13)(ZS(NAT(I)),I=1,NUMAT),(ZP(NAT(I)),I=1,NUMAT),
     1         (ZD(NAT(I)),I=1,NUMAT),(NAT(I),I=1,NUMAT)
         LINEAR=NORBS*NORBS
         WRITE(13)(C(I),I=1,LINEAR)
         WRITE(13)(F(I),I=1,LINEAR)
         IF(INDEX(KEYWRD,'MULLIK').EQ.0)RETURN
      ENDIF
*
* OTHERWISE PERFORM MULLIKEN ANALYSIS
*
      CALL MULT(C,F,VECS,NORBS)
      I=-1
      CALL DENSIT(VECS,NORBS,NORBS,NCLOSE,NOPEN,FRACT,C,2)
      LINEAR=(NORBS*(NORBS+1))/2
      DO 100 I=1,LINEAR
  100 C(I)=C(I)*STORE(I)
      SUMM=0.D0
      DO 130 I=1,NORBS
         SUM=0
         DO 110 J=1,I
  110    SUM=SUM+C(IFACT(I)+J)
         DO 120 J=I+1,NORBS
  120    SUM=SUM+C(IFACT(J)+I)
         SUMM=SUMM+SUM
  130 C(IFACT(I+1))=SUM
      CALL VECPRT(C,NORBS)
      RETURN
      END
