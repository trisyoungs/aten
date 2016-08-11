      SUBROUTINE PARTXY(C34,PQ34)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION C34(*),PQ34(*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC)
     1       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     2               ,GSD(107),GPD(107),GDD(107)
     3       /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      DIMENSION W(N2ELEC*2)
      EQUIVALENCE (W,WJ)
      CHARACTER*241 KEYWRD
C------------------------------------------------------------------
C
C    PARTXY WORKS OUT  IN MNDO FORMALISM THE FIRST 2-INDICES TRANSFO.
C          REQUIRED IN THE COMPUTATION OF 2-ELECTRONS REPULSION OVER M.O
C  INPUT
C     C34   : VECTOR OF THE CURRENT CHARGE DISTRIBUTION BETWEEN TWO M.O.
C  OUTPUT
C     PQ34(PQ) : <P(1),Q(1)|C3(2),C4(2)> WHERE P ,Q  ARE A.O.
C                                          AND C3,C4 ARE M.O.
C                P AND Q RUN IN CANONICAL ORDER OVER THE A.O BELONGING
C                TO AN ATOM 'A' ONLY (BASIC ASSUMPTION OF MNDO SCHEME)
C                AND 'A' RUNS OVER THE ATOMS OF THE SYSTEM.
C     D.L. (DEWAR GROUP) 1986
C----------------------------------------------------------------------
      DIMENSION LD(9),PTOT(NUMATM), NB(0:8), INDX(NUMATM)
      DATA LD /0,2,5,9,14,20,27,35,44/
      DATA NB /1,0,0,10,0,0,0,0,45/
      DATA ICALCN/0/
      IF(NUMCAL.NE.ICALCN)THEN
         ICALCN=NUMCAL
         INDX(1)=1
         DO 10 I=2,NUMAT
   10    INDX(I)=INDX(I-1)+NB(NLAST(I-1)-NFIRST(I-1))
      ENDIF
C     IJ    : POINTER OF CANONICAL PACKED LOCATION OF COUPLE IJ.
C     KK    : POINTER OF SUPPORTING ATOM, SPARKLES SKIPPED OUT.
C     IPQRS : CURRENT ENTRY POINT IN THE <PQ|RS> FILE.
      KK=0
      IPQRS=1
      IJ=0
      IJOLD=0
C
C     LOOP OVER OUTER ATOM A, SPARKLES EXCLUDED.
C     ------------------------------------------
      NBAND=1
      KR=1
      LS=0
      DO 30 II=1,NUMAT
         IA=NFIRST(II)
         IB=NMIDLE(II)
         IC=NLAST (II)
         IF(IC.LT.IA) GO TO 30
         KK=KK+1
         LS=LS+NBAND
         NBAND=NB(IC-IA)
         IJ=IJ+NBAND
C
C     PQ34(IJ) = <IJ|KL> * C34(KL)  , 1-CENTRE CONTRIBUTIONS.
         IZN=NAT(II)
C     BLOCK SS
         PTOT(KK)=C34(LS)
         PQ34(LS)=C34(LS)*GSS(IZN)*0.25D0
         IF(IB.GT.IA) THEN
C        BLOCK SP AND PP
            HPP=0.5D0*(GPP(IZN)-GP2(IZN))
            LX=LS+LD(2)
            LY=LS+LD(3)
            LZ=LS+LD(4)
            PP=C34(LX)+C34(LY)+C34(LZ)
            PQ34(LS+1)=HSP(IZN)*C34(LS+1)
            PQ34(LX  )=GPP(IZN)*C34(LX  )*0.25D0
            PQ34(LS+3)=HSP(IZN)*C34(LS+3)
            PQ34(LS+4)=HPP     *C34(LS+4)
            PQ34(LY  )=GPP(IZN)*C34(LY  )*0.25D0
            PQ34(LS+6)=HSP(IZN)*C34(LS+6)
            PQ34(LS+7)=HPP     *C34(LS+7)
            PQ34(LS+8)=HPP     *C34(LS+8)
            PQ34(LZ  )=GPP(IZN)*C34(LZ  )*0.25D0
            GSPSS=     GSP(IZN)*C34(LS  )*0.25D0
            PQ34(LS)=PQ34(LS)+GSP(IZN)*PP*0.25D0
            PQ34(LX)=PQ34(LX)+GP2(IZN)*(C34(LY)+C34(LZ))*0.25D0+GSPSS
            PQ34(LY)=PQ34(LY)+GP2(IZN)*(C34(LZ)+C34(LX))*0.25D0+GSPSS
            PQ34(LZ)=PQ34(LZ)+GP2(IZN)*(C34(LX)+C34(LY))*0.25D0+GSPSS
            PTOT(KK)=PTOT(KK)+PP
            IF(IC.GT.IB) THEN
C           BLOCK SD, PD AND DD
C           --- WAITING FOR 'D' PARAMETERS ---
C               TAKE CARE : DIAGONAL ELEMENTS OF C34 ARE DOUBLED.
            ENDIF
         ENDIF
         IF(KK.GT.1)THEN
C
C        LOOP OVER CHARGE DISTRIBUTION OF INNER ATOMS  B < A .
C        -----------------------------------------------------
C        PQ34(IJ)=<IJ|KL>*C34(KL) 2-CENTRES CONTRIBUTIONS.
C
            JBAND=1
            JS=0
            DO 20 JJ=1,II-1
               JS=JS+JBAND
               JBAND=NB(NLAST(JJ)-NFIRST(JJ))
C
C   NBAND AND JBAND ARE EITHER 1 OR 10
C
               CALL FORMXY
     1(W(KR), KR, PQ34(LS), PQ34(JS), C34(LS), NBAND, C34(JS), JBAND)
   20       IPQRS=IPQRS+IJOLD
         ENDIF
         IJOLD=IJ
   30 CONTINUE
      RETURN
      END
