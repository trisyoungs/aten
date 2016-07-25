      SUBROUTINE GREENF
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*******************************************************************
*                                                                 *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATION              *
*******************************************************************
      INCLUDE 'SIZES'
      COMMON/MOLKST/NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1 NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,NOPEN,NDUMY,
     2 FRACT
      COMMON/DOD/ITL(200),IT(200)
      COMMON/DOD1/NMUX,LM6,LM7
      COMMON /CHANEL/ IFILES(30)
      EQUIVALENCE(IW,IFILES(6))
*
      IW=6
      WRITE(IW,10)
   10 FORMAT(///)
      WRITE(IW,20)
   20 FORMAT(10X,'************************************************'/
     1       10X,'*                                              *'/
     2       10X,'*   SEMIEMPIRICAL GREEN FUNCTION CALCULATION   *'/
     3       10X,'*                                              *'/
     4       10X,'*  This package was written by Dr. D.Danovich  *'/
     5       10X,'*            Chemistry Department              *'/
     6       10X,'*    TECHNION-Israel Institute of Technology   *'/
     7       10X,'*      Technion City, Haifa  32000, ISRAEL     *'/
     8       10X,'*                                              *'/
     9       10X,'************************************************')
      K=0
      DO 30 I=1,200
         IT(I)=K
   30 K=K+I
      DO 40 I=1,200
         N=IT(I)
   40 ITL(I)=N*(N-1)/2
      MMM=NORBS
      NMUX=ITL(MMM)+IT(MMM)*MMM+IT(MMM)+IT(MMM)+MMM
      LM6=0
      DO 50 I=1,NUMAT
         IAO=NLAST(I)-NFIRST(I)+1
   50 LM6=LM6+(IAO*(IAO+1))/2
      LM7=LM6*LM6
      MEMORY=NMUX+2*LM6+LM7+8
      CALL INSYMC
      CALL MO(MEMORY)
      RETURN
      END
C
C==================================================================
C
      SUBROUTINE GSTORE(II,JJ,KK,LL,WERT,PGR,NMUX)
************************************************************
*                                                          *
*     STORE TWO-ELECTRON MO INTEGRALS ON FILE NTP3.        *
*     SUBROUTINE WRITTEN BY DR.DAVID DANOVICH,             *
*     COMPUTATIONAL CHEMISTRY CENTRE, CHEMISTRY DEPARTMENT *
*     TECHNION-ISRAEL INSTITUTE OF TECHNOLOGY, HAIFA,      *
*     32000, ISRAEL,          VERSION     02.09.90         *
************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PGR(*)
      COMMON/DOD/ITL(200),IT(200)
      III=II
      JJJ=JJ
      KKK=KK
      LLL=LL
      IF(III.GE.JJJ) GO TO 10
      LT=III
      III=JJJ
      JJJ=LT
   10 IF(KKK.GE.LLL) GO TO 20
      LT=KKK
      KKK=LLL
      LLL=LT
   20 IF(III.GT.KKK) GO TO 40
      IF(III.EQ.KKK) GO TO 30
      LT=III
      III=KKK
      KKK=LT
      LT=JJJ
      JJJ=LLL
      LLL=LT
      GO TO 40
   30 IF(JJJ.GE.LLL) GO TO 40
      LT=JJJ
      JJJ=LLL
      LLL=LT
   40 NNNN=ITL(III)+IT(III)*JJJ+IT(JJJ)+IT(KKK)+LLL
      PGR(NNNN)=WERT
      RETURN
      END
C
C======================================================================
C
      SUBROUTINE INSYMC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
******************************************************************
*                                                                *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATIONS            *
******************************************************************
      INCLUDE 'SIZES'
      COMMON/CIMOS /IMOCI(200)
      COMMON/CIPARM/ICI1,ICI2,IOUT2
      COMMON/MOLKST/NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     * NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,NOPEN,
     * NDUMY,FRACT
      CHARACTER LINE*80
      DIMENSION VALUE(40)
      NRA=6
      NAMB=NORBS-NCLOSE
      READ(5,'(A)') LINE
      CALL NUCHAR(LINE,VALUE,NVALUE)
      ICI1=VALUE(1)
      ICI2=VALUE(2)
      IOUTCI=VALUE(3)
      IF(ICI1.EQ.0) ICI1=20
      IF(ICI1.GT.NCLOSE) ICI1=NCLOSE
      IF(ICI2.EQ.0) ICI2=20
      IF(ICI2.GT.NAMB) ICI2=NAMB
      WRITE(NRA,3185) ICI1,ICI2,IOUTCI
      IOUT2=IOUTCI
C *** DEFINITION OF ORBITALS INVOLVED IN THE GREEN TREATMENT.
      IA=ICI1+1
      IB=ICI1+ICI2
      DO 101 I=1,ICI1
  101 IMOCI(I)=NCLOSE+1-I
      DO 102 I=IA,IB
  102 IMOCI(I)=NCLOSE+1+I-IA
      RETURN
 3185 FORMAT(///1X,'NUMBER OF OCCUPIED MOS',4X,I4,
     1       /1X,'NUMBER OF UNOCCUPIED MOS',2X,I4,
     2       /1X,'PRINTING FLAG',13X,I4)
      END
C
C==============================================================
C
      SUBROUTINE MO(MEMORY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*******************************************************************
*                                                                 *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATIONS             *
*******************************************************************
      INCLUDE 'SIZES'
      COMMON/MOLKST/NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1 NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,NOPEN,NDUMY,
     2 FRACT
      COMMON/DOD/ITL(200),IT(200)
      COMMON/VECTOR/C(MORB2),EIG(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON/WMATRX/W(N2ELEC),WK(N2ELEC)
      COMMON/CIPARM/ICI1,ICI2,IOUT2
      COMMON/DOD1/NMUX,LM6,LM7
      COMMON/FMCOM/X(IGREEN)
      COMMON /CHANEL/ IFILES(30)
      EQUIVALENCE(IW,IFILES(6))
      IF(MEMORY.GT.IGREEN)THEN
         WRITE(IW,'(A)')' AVAILABLE MEMORY IN '//
     1'GREENS FUNCTION CALCULATION'
         WRITE(IW,'(A,I8,A,I8)')' TOO SMALL.  AVAILABLE:'
     1,IGREEN,' NEEDED:',MEMORY
         WRITE(IW,'(A)')' TO RUN THIS JOB, MODIFY IGREEN IN SUBROUTINE'
     1//' MO (CALLED BY GREENF) AND RECOMPILE'
      STOP
      ENDIF
      LOADFM=0
      I10=1+LOADFM
      NNMM=I10+NMUX
      NNNN1=NNMM+LM7
      NNMM1=NNNN1+LM6
      LAST=NNMM1+LM6
      NEED=LAST-I10
      CALL WORDER(X(NNMM),LM7,IOUT2)
      CALL MOINT(C,X(I10),X(NNMM),X(NNNN1),X(NNMM1),NORBS,NORBS,LM7,LM6,
     1 NMUX)
      CALL FCNPP(X(I10),NMUX)
      RETURN
      END
      SUBROUTINE MOINT(C,PGR,CC,W,C12,NORBS,LM2,LM8,KMAX,NMUX)
********************************************************************
*     TRANSFORMATION OF TWO-ELECTRON INTEGRALS FROM AO TO MO BASIS *
*     SUBROUTINE WRITTEN BY DR.DAVID DANOVICH, COMPUTATIONAL       *
*     CHEMISTRY CENTRE, DEPARTMENT OF CHEMISTRY, TECHNION -        *
*     ISRAEL INSTITUTE OF TECHNOLOGY, HAIFA, 32000, ISRAEL         *
*     VERSION     02.09.90                                         *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATIONS              *
********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PGR(*)
      COMMON/CIMOS /IMOCI(200)
      COMMON/CIPARM/ICI1,ICI2,IOUT2
      COMMON/DAVI/NINTG
      COMMON /CHANEL/ IFILES(30)
      EQUIVALENCE(IW,IFILES(6))
      DIMENSION C(LM2,NORBS),CC(*),W(*),C12(*)
      DATA OFF/1.0D-8/,ZERO/0.0D0/
C     SET CONTROL VARIABLES.
C     LMAX   = STORAGE CAPACITY OF CC(LM8)
      LMAX   = LM8/KMAX
C     COMPUTE (IJ,KL) INTEGRALS
      IEND=ICI1+ICI2
      NINTG=0
C     OUTER IJ-LOOP
      DO 10 I=1,IEND
         II = IMOCI(I)
         DO 10 J=1,I
            IJ = IMOCI(J)
C     COMPUTE SET OF (IJ,AB) INTEGRALS
            CALL CCPROD(C(1,II),C(1,IJ),C12,LM2,KMAX)
            CALL WWSTEP(C12,CC,W,LM8,KMAX,LMAX)
C     INNER KL-LOOP
            DO 10 K=1,I
               IK = IMOCI(K)
               DO 10  L=1,K
                  IF(I.EQ.K.AND.J.LT.L) GO TO 10
                  IL = IMOCI(L)
                  WNN = ZERO
C     COMPUTE THE INTEGRAL
                  NINTG = NINTG+1
                  CALL CCPROD(C(1,IK),C(1,IL),C12,LM2,KMAX)
                  WNN = SISMS(C12,W,KMAX)
                  IF(DABS(WNN).LT.OFF) WNN=ZERO
                  CALL GSTORE(II,IJ,IK,IL,WNN,PGR,NMUX)
   10 CONTINUE
      IF(IOUT2.GT.-5) WRITE(IW,20) NINTG
   20 FORMAT(///1X,'THERE ARE',I12,' NONZERO INTEGRALS.'/)
      RETURN
      END
C
C=======================================================================
C
      FUNCTION SISMS (C12,CC,LM6)
C     SCALAR PRODUCT.
*******************************************************************
*     FUNCTION NEED FOR GREEN FUNCTION CALCULATIONS               *
*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C12(*),CC(*)
      SISMS=0.D0
      DO 10 I=1,LM6
   10 SISMS=SISMS+C12(I)*CC(I)
      RETURN
      END
C
C===================================================================
C
      SUBROUTINE SUMA2(P,NDIS,EP)
********************************************************************
*     CALCULATION SECOND-ORDER CONTRIBUTION IN THE SELF-ENERGY     *
*     FUNCTION                                                     *
*     SUBROUTINE WRITTEN BY DR. DAVID DANOVICH, COMPUTATIONAL      *
*     CHEMISTRY CENTRE, DEPARTMENT OF CHEMISTRY, TECHNION -        *
*     ISRAEL INSTITUTE OF TECHNOLOGY, HAIFA, 32000, ISRAEL         *
*     VERSION    02.09.90                                          *
********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION P(*)
      COMMON/DOD/ITL(200),IT(200)
      COMMON/VECTOR/C(MORB2),EIG(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON/PEREM/NYR,IY,IGGV,IGGW
      COMMON/SUMA/SU2R,EPS,SUM1,SUM2
      KYR2=NYR+IGGV
      KYR1=NYR+1
      MYR=NYR-IGGW
      SUM1=0.D0
      SUM2=0.D0
      DO 70 M=KYR1,KYR2
         DO 70 I=MYR,NYR
            DO 70 J=MYR,NYR
               IYR=IY
               II=I
               MM=M
               JJ=J
               IF(IYR.GE.II) GO TO 10
               LT=IYR
               IYR=II
               II=LT
   10          IF(IYR.GT.MM) GO TO 30
               IF(IYR.EQ.MM) GO TO 20
               LT=IYR
               IYR=MM
               MM=LT
               LT=II
               II=JJ
               JJ=LT
               GO TO 30
   20          IF(II.GE.JJ) GO TO 30
               LT=II
               II=JJ
               JJ=LT
   30          NUMB=ITL(IYR)+IT(IYR)*II+IT(II)+IT(MM)+JJ
               IYR=IY
               JJ=J
               MM=M
               II=I
               IF(IYR.GE.JJ) GO TO 40
               LT=IYR
               IYR=JJ
               JJ=LT
   40          IF(IYR.GT.MM) GO TO 60
               IF(IYR.EQ.MM) GO TO 50
               LT=IYR
               IYR=MM
               MM=LT
               LT=JJ
               JJ=II
               II=LT
               GO TO 60
   50          IF(JJ.GE.II) GO TO 60
               LT=JJ
               JJ=II
               II=LT
   60          NUMB1=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(MM)+II
               SUM1=SUM1+(2.D0*P(NUMB)-P(NUMB1))*P(NUMB)/
     1(EP+EIG(M)-EIG(I)-EIG(J))
   70 CONTINUE
      DO 140 M=KYR1,KYR2
         DO 140 N=KYR1,KYR2
            DO 140 I=MYR,NYR
               IYR=IY
               II=I
               MM=M
               NN=N
               IF(IYR.GE.MM) GO TO 80
               LT=IYR
               IYR=MM
               MM=LT
   80          IF(IYR.GT.NN) GO TO 100
               IF(IYR.EQ.NN) GO TO 90
               LT=IYR
               IYR=NN
               NN=LT
               LT=MM
               MM=II
               II=LT
               GO TO 100
   90          IF(MM.GE.II) GO TO 100
               LT=MM
               MM=II
               II=LT
  100          NUMB=ITL(IYR)+IT(IYR)*MM+IT(MM)+IT(NN)+II
               IYR=IY
               NN=N
               MM=M
               II=I
               IF(IYR.GE.NN) GO TO 110
               LT=IYR
               IYR=NN
               NN=LT
  110          IF(IYR.GT.MM) GO TO 130
               IF(IYR.EQ.MM) GO TO 120
               LT=IYR
               IYR=MM
               MM=LT
               LT=NN
               NN=II
               II=LT
               GO TO 130
  120          IF(NN.GE.II) GO TO 130
               LT=NN
               NN=II
               II=LT
  130          NUMB1=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(MM)+II
               SUM2=SUM2+(2.D0*P(NUMB)-P(NUMB1))*P(NUMB)/
     1(EP+EIG(I)-EIG(M)-EIG(N))
  140 CONTINUE
      SU2R=SUM1+SUM2
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE WORDER(CC,LM7,IOUT2)
*********************************************************
*                                                       *
*     ORDERING OF AO REPULSION INTEGRALS.               *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATION    *
*********************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON/TWOELE/GSS(107),GSP(107),GPP(107),GP2(107),HSP(107),
     1 GSD(107),GPD(107),GDD(107)
      COMMON/MOLKST/NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,NOPEN,NDUMY,
     2 FRACT
      COMMON/WMATRX/W(N2ELEC),WK(N2ELEC)
      COMMON /CHANEL/ IFILES(30)
      EQUIVALENCE(IW,IFILES(6))
      DIMENSION CC(*)
      DIMENSION INDX(NUMATM),INDY(NUMATM)
      DATA ZERO/0.0D0/
      KK=1
      DO 10 I=1,NUMAT
         INDX(I)=KK
         KK=KK+1
         IF(NAT(I).GT.2) KK=KK+9
         INDY(I)=KK-1
   10 CONTINUE
C     STORAGE CONTROL.
      KMAX=KK-1
      LMAX=LM7/KMAX
      KTOT=KMAX*KMAX
      IF(KMAX.GT.LMAX) KTOT=KMAX*LMAX
C     SORTING OF INTEGRALS FOR MNDO
C     NUMBER OF RECORDS.
      LREC=1+(KMAX-1)/LMAX
      DO 190 L=1,LREC
         LA=LMAX*(L-1)+1
         LB=LMAX*L
         KP=KMAX*(LA-1)
C     INITIALIZE INTEGRALS.
         DO 20 KK=1,KTOT
   20    CC(KK)=ZERO
C     LOOP OVER ONE-CENTER INTEGRALS.
         DO 90 II=1,NUMAT
            IA=INDX(II)
            IF(IA.GT.LB) GO TO 100
            IB=INDY(II)
            IF(IB.LT.LA) GO TO 90
            NI=NAT(II)
            IF(NI.GT.2) GO TO 30
C     HYDROGEN.
            KK=KMAX*(IA-1)+IA-KP
            CC(KK)=GSS(NI)
            GO TO 90
C     HEAVY ATOM.
   30       DO 80 I=IA,IB
               IF(I.LT.LA.OR.I.GT.LB) GO TO 80
               KS =KMAX*(I-1)+IA-1-KP
               KGO=I-IA+1
               KK =KS+KGO
               GO TO (40,50,60,50,70,60,50,70,70,60),KGO
   40          CC(KK   )=GSS(NI)
               CC(KS+3 )=GSP(NI)
               CC(KS+6 )=GSP(NI)
               CC(KS+10)=GSP(NI)
               GO TO 80
   50          CC(KK   )=HSP(NI)
               GO TO 80
   60          CC(KS+1 )=GSP(NI)
               CC(KS+3 )=GP2(NI)
               CC(KS+6 )=GP2(NI)
               CC(KS+10)=GP2(NI)
               CC(KK   )=GPP(NI)
               GO TO 80
   70          CC(KK   )=0.5D0*(GPP(NI)-GP2(NI))
   80       CONTINUE
   90    CONTINUE
C     LOOP OVER TWO-CENTER INTEGRALS, MNDO.
  100    IF(NUMAT.EQ.1) GO TO 180
         NA=0
         DO 170 II=2,NUMAT
            IA=INDX(II)
            IB=INDY(II)
            IW1=IB-IA+1
            IMINUS=II-1
            DO 170 JJ=1,IMINUS
               JA=INDX(JJ)
               JB=INDY(JJ)
               JW=JB-JA+1
               NO=IW1*JW
C     CASE II.GT.JJ.
               IF(IA.GT.LB.OR.IB.LT.LA) GO TO 130
               DO 120 I=IA,IB
                  IF(I.LT.LA.OR.I.GT.LB) GO TO 120
                  KS=KMAX*(I-1)+JA-1-KP
                  NS=NA+JW*(I-IA)
                  DO 110 J=1,JW
                     KK=KS+J
                     NN=NS+J
  110             CC(KK)=W(NN)
  120          CONTINUE
C     CASE II.LT.JJ.
  130          IF(JA.GT.LB.OR.JB.LT.LA) GO TO 160
               DO 150 J=JA,JB
                  IF(J.LT.LA.OR.J.GT.LB) GO TO 150
                  KS=KMAX*(J-1)+IA-1-KP
                  NS=NA+J-JA+1-JW
                  DO 140 I=1,IW1
                     KK=KS+I
                     NN=NS+JW*I
  140             CC(KK)=W(NN)
  150          CONTINUE
  160          NA=NA+NO
  170    CONTINUE
  180    CONTINUE
  190 CONTINUE
C     DEBUG PRINT.
      IF(IOUT2.LT.1) RETURN
      WRITE(IW,220)
      WRITE(IW,230) KMAX
      IF(LREC.GT.1) WRITE(IW,240) LREC,LMAX,KTOT
      IF(IOUT2.LT.4) RETURN
      IMAX=KMAX
      IF(LREC.EQ.1) GO TO 200
      IMAX=LMAX
  200 WRITE(IW,250)
      KK=1
      DO 210 I=1,IMAX
         KA=KK
         KB=KA+KMAX-1
         WRITE(IW,260) (CC(K),K=KA,KB)
  210 KK=KA+KMAX
      RETURN
  220 FORMAT(1H1,//1X,'AO INTEGRALS IN NEW ORDER.'/)
  230 FORMAT(//1X,'THE AO INTEGRALS ARE STORED IN A MATRIX WITH',I4,
     1  ' ROWS AND COLUMNS.')
  240 FORMAT(  1X,'THERE ARE',I4,' RECORDS EACH OF WHICH ',
     1  'CONTAINS',I4/2X,' COLUMNS AND',I6,' INTEGRALS.')
  250 FORMAT(//1X,'INTEGRALS IN THE FIRST RECORD.'/)
  260 FORMAT(  1X,10F7.3)
      END
C
C=======================================================================
C
      SUBROUTINE WWSTEP(C12,CC,WW,LM7,KMAX,LMAX)
*********************************************************
*                                                       *
*     CALCULATION OF A SET OF (IJ,AB) INTEGRALS.        *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATION    *
*********************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C12(*),CC(*),WW(*)
      IF(KMAX.GT.LMAX) GO TO 20
C     AO INTEGRALS IN CC(LM7).
      KK=1-KMAX
      DO 10 NN=1,KMAX
         KK=KK+KMAX
   10 WW(NN) = SISMS(C12,CC(KK),KMAX)
      RETURN
   20 CONTINUE
      KK=1-KMAX
      LL=0
      DO 40 NN=1,KMAX
         LL=LL+1
         IF(LL.LE.LMAX) GO TO 30
         KK=1-KMAX
         LL=1
   30    KK=KK+KMAX
   40 WW(NN) = SISMS(C12,CC(KK),KMAX)
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE ASUM(P,NDIS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*****************************************************************
*  CALCULATION  "AI" VALUES FOR GREEN'S FUNCTION METHOD         *
*  SUBROUTINE WRITTEN BY DR. DAVID DANOVICH, DEPARTMENT OF      *
*  OF CHEMISTRY, TECHNION-ISRAEL INSTITUTE OF TECHNOLOGY,       *
*  HAIFA, 32000, ISRAEL.       VERSION: 2.09.90                 *
*****************************************************************
      INCLUDE 'SIZES'
      DIMENSION P(*)
      COMMON/DOD/ITL(200),IT(200)
      COMMON/VECTOR/C(MORB2),EIG(MAXORB),DUMY(MORB2+MAXORB)
      COMMON/PEREM/NYR,IY,IGGV,IGGW
      COMMON/ASMA/AS1,AS2,AS3,AS4,AS5,AS6
      KYR2=NYR+IGGV
      KYR1=NYR+1
      MYR1=NYR-IGGW
      AS1=0.D0
      AS2=0.D0
      AS3=0.D0
      AS4=0.D0
      AS5=0.D0
      AS6=0.D0
      DO 200 M=KYR1,KYR2
         DO 200 N=KYR1,KYR2
            DO 200 I=MYR1,NYR
               DO 200 J=MYR1,NYR
                  DO 200 K=MYR1,NYR
                     IYR=IY
                     KK=K
                     JJ=J
                     IF(KK.GE.JJ) GO TO 10
                     LT=KK
                     KK=JJ
                     JJ=LT
   10                IF(IYR.GT.NYR) GO TO 20
                     IF(IYR.GE.KK) GO TO 20
                     NUMB=ITL(KK)+IT(KK)*JJ+IT(JJ)+IT(IYR)+IYR
                     GO TO 30
   20                NUMB=ITL(IYR)+IT(IYR)*IYR+IT(IYR)+IT(KK)+JJ
   30                IYR=IY
                     KK=K
                     JJ=J
                     IF(IYR.LE.NYR) GO TO 50
                     IF(JJ.GE.KK) GO TO 40
                     LT=JJ
                     JJ=KK
                     KK=LT
   40                NUMB1=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(IYR)+KK
                     GO TO 100
   50                IF(IYR.GE.JJ.AND.IYR.GE.KK) GO TO 60
                     IF(IYR.LT.JJ.AND.IYR.LT.KK) GO TO 70
                     IF(IYR.GE.JJ.AND.IYR.LT.KK) GO TO 90
                     NUMB1=ITL(JJ)+IT(JJ)*IYR+IT(IYR)+IT(IYR)+KK
                     GO TO 100
   60                IF(JJ.GE.KK) GO TO 40
                     LT=JJ
                     JJ=KK
                     KK=LT
                     GO TO 40
   70                IF(JJ.GE.KK) GO TO 80
                     LT=JJ
                     JJ=KK
                     KK=LT
   80                NUMB1=ITL(JJ)+IT(JJ)*IYR+IT(IYR)+IT(KK)+IYR
                     GO TO 100
   90                NUMB1=ITL(KK)+IT(KK)*IYR+IT(IYR)+IT(IYR)+JJ
  100                JJ=J
                     II=I
                     MM=M
                     NN=N
                     IF(MM.GT.NN) GO TO 110
                     IF(MM.EQ.NN) GO TO 120
                     NUMB2=ITL(NN)+IT(NN)*II+IT(II)+IT(MM)+JJ
                     GO TO 130
  110                NUMB2=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+II
                     GO TO 130
  120                IF(JJ.GE.II) GO TO 110
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 110
  130                JJ=J
                     NN=N
                     II=I
                     MM=M
                     IF(NN.GT.MM) GO TO 140
                     IF(NN.EQ.MM) GO TO 150
                     NUMB3=ITL(MM)+IT(MM)*II+IT(II)+IT(NN)+JJ
                     GO TO 160
  140                NUMB3=ITL(NN)+IT(NN)*JJ+IT(JJ)+IT(MM)+II
                     GO TO 160
  150                IF(JJ.GE.II) GO TO 140
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 140
  160                MM=M
                     KK=K
                     NN=N
                     II=I
                     IF(MM.GT.NN) GO TO 170
                     IF(MM.EQ.NN) GO TO 180
                     NUMB4=ITL(NN)+IT(NN)*II+IT(II)+IT(MM)+KK
                     GO TO 190
  170                NUMB4=ITL(MM)+IT(MM)*KK+IT(KK)+IT(NN)+II
                     GO TO 190
  180                IF(KK.GE.II) GO TO 170
                     LT=KK
                     KK=II
                     II=LT
                     GO TO 170
  190                AS1=AS1+(2.D0*P(NUMB)-P(NUMB1))*
     1(2.D0*P(NUMB2)-P(NUMB3))*P(NUMB4)/
     2((EIG(J)+EIG(I)-EIG(M)-EIG(N))*(EIG(K)+EIG(I)-EIG(M)-EIG(N)))
  200 CONTINUE
      AS1=-AS1
      DO 400 M=KYR1,KYR2
         DO 400 N=KYR1,KYR2
            DO 400 L=KYR1,KYR2
               DO 400 I=MYR1,NYR
                  DO 400 J=MYR1,NYR
                     IYR=IY
                     NN=N
                     LL=L
                     IF(LL.GE.NN) GO TO 210
                     LT=LL
                     LL=NN
                     NN=LT
  210                IF(IYR.LE.NYR) GO TO 220
                     IF(IYR.LT.LL) GO TO 220
                     NUMB=ITL(IYR)+IT(IYR)*IYR+IT(IYR)+IT(LL)+NN
                     GO TO 230
  220                NUMB=ITL(LL)+IT(LL)*NN+IT(NN)+IT(IYR)+IYR
  230                IYR=IY
                     NN=N
                     LL=L
                     IF(IYR.GT.NYR) GO TO 250
                     IF(NN.GE.LL) GO TO 240
                     LT=NN
                     NN=LL
                     LL=LT
  240                NUMB1=ITL(NN)+IT(NN)*IYR+IT(IYR)+IT(LL)+IYR
                     GO TO 300
  250                IF(IYR.GE.NN.AND.IYR.GE.LL) GO TO 260
                     IF(IYR.LT.NN.AND.IYR.LT.LL) GO TO 280
                     IF(IYR.GE.NN.AND.IYR.LT.LL) GO TO 290
                     NUMB1=ITL(NN)+IT(NN)*IYR+IT(IYR)+IT(IYR)+LL
                     GO TO 300
  260                IF(NN.GE.LL) GO TO 270
                     LT=NN
                     NN=LL
                     LL=LT
  270                NUMB1=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(IYR)+LL
                     GO TO 300
  280                IF(NN.GE.LL) GO TO 240
                     LT=NN
                     NN=LL
                     LL=LT
                     GO TO 240
  290                NUMB1=ITL(LL)+IT(LL)*IYR+IT(IYR)+IT(IYR)+NN
  300                JJ=J
                     MM=M
                     II=I
                     NN=N
                     IF(MM.GT.NN) GO TO 310
                     IF(MM.EQ.NN) GO TO 320
                     NUMB2=ITL(NN)+IT(NN)*II+IT(II)+IT(MM)+JJ
                     GO TO 330
  310                NUMB2=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+II
                     GO TO 330
  320                IF(JJ.GE.II) GO TO 310
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 310
  330                JJ=J
                     NN=N
                     II=I
                     MM=M
                     IF(NN.GT.MM) GO TO 340
                     IF(NN.EQ.MM) GO TO 350
                     NUMB3=ITL(MM)+IT(MM)*II+IT(II)+IT(NN)+JJ
                     GO TO 360
  340                NUMB3=ITL(NN)+IT(NN)*JJ+IT(JJ)+IT(MM)+II
                     GO TO 360
  350                IF(JJ.GE.II) GO TO 340
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 340
  360                JJ=J
                     LL=L
                     II=I
                     MM=M
                     IF(LL.GT.MM) GO TO 370
                     IF(LL.EQ.MM) GO TO 380
                     NUMB4=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(LL)+II
                     GO TO 390
  370                NUMB4=ITL(LL)+IT(LL)*II+IT(II)+IT(MM)+JJ
                     GO TO 390
  380                IF(II.GE.JJ) GO TO 370
                     LT=II
                     II=JJ
                     JJ=LT
                     GO TO 370
  390                AS2=AS2+(2.D0*P(NUMB)-P(NUMB1))*
     1(2.D0*P(NUMB2)-P(NUMB3))*P(NUMB4)/
     2((EIG(J)+EIG(I)-EIG(M)-EIG(N))*(EIG(J)+EIG(I)-EIG(M)-EIG(L)))
  400 CONTINUE
      DO 560 M=KYR1,KYR2
         DO 560 N=KYR1,KYR2
            DO 560 L=KYR1,KYR2
               DO 560 I=MYR1,NYR
                  DO 560 J=MYR1,NYR
                     IYR=IY
                     LL=L
                     JJ=J
                     IF(IYR.GE.LL) GO TO 410
                     NUMB=ITL(LL)+IT(LL)*JJ+IT(JJ)+IT(IYR)+IYR
                     GO TO 420
  410                NUMB=ITL(IYR)+IT(IYR)*IYR+IT(IYR)+IT(LL)+JJ
  420                IYR=IY
                     JJ=J
                     LL=L
                     IF(IYR.GT.NYR) GO TO 440
                     IF(IYR.GE.JJ) GO TO 430
                     NUMB1=ITL(LL)+IT(LL)*IYR+IT(IYR)+IT(JJ)+IYR
                     GO TO 460
  430                NUMB1=ITL(LL)+IT(LL)*IYR+IT(IYR)+IT(IYR)+JJ
                     GO TO 460
  440                IF(IYR.LT.LL) GO TO 430
                     IF(IYR.GT.LL) GO TO 450
                     NUMB1=ITL(IYR)+IT(IYR)*IYR+IT(IYR)+IT(LL)+JJ
                     GO TO 460
  450                NUMB1=ITL(IYR)+IT(IYR)*LL+IT(LL)+IT(IYR)+JJ
  460                JJ=J
                     MM=M
                     II=I
                     NN=N
                     IF(MM.GT.NN) GO TO 470
                     IF(MM.EQ.NN) GO TO 480
                     NUMB2=ITL(NN)+IT(NN)*II+IT(II)+IT(MM)+JJ
                     GO TO 490
  470                NUMB2=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+II
                     GO TO 490
  480                IF(JJ.GE.II) GO TO 470
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 470
  490                JJ=J
                     NN=N
                     II=I
                     MM=M
                     IF(NN.GT.MM) GO TO 500
                     IF(NN.EQ.MM) GO TO 510
                     NUMB3=ITL(MM)+IT(MM)*II+IT(II)+IT(NN)+JJ
                     GO TO 520
  500                NUMB3=ITL(NN)+IT(NN)*JJ+IT(JJ)+IT(MM)+II
                     GO TO 520
  510                IF(JJ.GE.II) GO TO 500
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 500
  520                MM=M
                     LL=L
                     II=I
                     NN=N
                     IF(MM.GE.LL) GO TO 530
                     LT=MM
                     MM=LL
                     LL=LT
  530                IF(MM.GE.NN) GO TO 540
                     NUMB4=ITL(NN)+IT(NN)*II+IT(II)+IT(MM)+LL
                     GO TO 550
  540                NUMB4=ITL(MM)+IT(MM)*LL+IT(LL)+IT(NN)+II
  550                AS3=AS3+(2.D0*P(NUMB)-P(NUMB1))*
     1(2.D0*P(NUMB2)-P(NUMB3))*P(NUMB4)/
     2((EIG(J)+EIG(I)-EIG(M)-EIG(N))*(EIG(J)-EIG(L)))
  560 CONTINUE
      AS4=AS3
      DO 700 M=KYR1,KYR2
         DO 700 N=KYR1,KYR2
            DO 700 I=MYR1,NYR
               DO 700 J=MYR1,NYR
                  DO 700 K=MYR1,NYR
                     IYR=IY
                     NN=N
                     KK=K
                     IF(IYR.GE.NN) GO TO 570
                     NUMB=ITL(NN)+IT(NN)*KK+IT(KK)+IT(IYR)+IYR
                     GO TO 580
  570                NUMB=ITL(IYR)+IT(IYR)*IYR+IT(IYR)+IT(NN)+KK
  580                IYR=IY
                     KK=K
                     NN=N
                     IF(IYR.GT.NYR) GO TO 600
                     IF(IYR.GE.KK) GO TO 590
                     NUMB1=ITL(NN)+IT(NN)*IYR+IT(IYR)+IT(KK)+IYR
                     GO TO 620
  590                NUMB1=ITL(NN)+IT(NN)*IYR+IT(IYR)+IT(IYR)+KK
                     GO TO 620
  600                IF(IYR.LT.NN) GO TO 590
                     IF(IYR.GT.NN) GO TO 610
                     NUMB1=ITL(IYR)+IT(IYR)*IYR+IT(IYR)+IT(NN)+KK
                     GO TO 620
  610                NUMB1=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(IYR)+KK
  620                JJ=J
                     MM=M
                     II=I
                     NN=N
                     IF(MM.GT.NN) GO TO 630
                     IF(MM.EQ.NN) GO TO 640
                     NUMB2=ITL(NN)+IT(NN)*II+IT(II)+IT(MM)+JJ
                     GO TO 650
  630                NUMB2=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+II
                     GO TO 650
  640                IF(JJ.GE.II) GO TO 630
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 630
  650                JJ=J
                     NN=N
                     II=I
                     MM=M
                     IF(NN.GT.MM) GO TO 660
                     IF(NN.EQ.MM) GO TO 670
                     NUMB3=ITL(MM)+IT(MM)*II+IT(II)+IT(NN)+JJ
                     GO TO 680
  660                NUMB3=ITL(NN)+IT(NN)*JJ+IT(JJ)+IT(MM)+II
                     GO TO 680
  670                IF(JJ.GE.II) GO TO 660
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 660
  680                II=I
                     KK=K
                     JJ=J
                     MM=M
                     IF(II.GE.KK) GO TO 690
                     LT=II
                     II=KK
                     KK=LT
  690                NUMB4=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(II)+KK
                     AS5=AS5+(2.D0*P(NUMB)-P(NUMB1))*
     1(2.D0*P(NUMB2)-P(NUMB3))*P(NUMB4)/
     2((EIG(J)+EIG(I)-EIG(M)-EIG(N))*(EIG(K)-EIG(N)))
  700 CONTINUE
      AS5=-AS5
      AS6=AS5
      RETURN
      END
C
C=====================================================================
C
      SUBROUTINE CCPROD(C1,C2,C12,LM2,LM6)
**********************************************************
*     PRODUCTS OF COEFFICIENTS.                          *
*     SUBROUTINE FROM QCPE 438,  MNDOC                   *
*     WRITTEN BY W.THIEL                                 *
*     SUBROUTINE NEED FOR GREEN FUNCTION CALCULATIONS    *
*     SUBROUTINE REWRITTEN BY D.DANOVICH                 *
**********************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON/MOLKST/NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1 NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,NOPEN,NDUMY,
     2 FRACT
      DIMENSION C1(LM2),C2(LM2),C12(*)
      KK=0
      DO 10 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         KK=KK+1
         C12(KK)=C1(IA)*C2(IA)
         IF(IA.EQ.IB) GO TO 10
         CS1  = C1(IA)
         CPX1 = C1(IA+1)
         CPY1 = C1(IA+2)
         CPZ1 = C1(IA+3)
         CS2  = C2(IA)
         CPX2 = C2(IA+1)
         CPY2 = C2(IA+2)
         CPZ2 = C2(IA+3)
         C12(KK+1) = CPX1*CS2  + CPX2*CS1
         C12(KK+2) = CPX1*CPX2
         C12(KK+3) = CPY1*CS2  + CPY2*CS1
         C12(KK+4) = CPY1*CPX2 + CPY2*CPX1
         C12(KK+5) = CPY1*CPY2
         C12(KK+6) = CPZ1*CS2  + CPZ2*CS1
         C12(KK+7) = CPZ1*CPX2 + CPZ2*CPX1
         C12(KK+8) = CPZ1*CPY2 + CPZ2*CPY1
         C12(KK+9) = CPZ1*CPZ2
         KK = KK+9
   10 CONTINUE
      RETURN
      END
C
C=================================================================
C
      SUBROUTINE CSUM(P,NDIS,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************
*  CALCULATION 'CI' VALUES FOR GREEN'S FUNCTION METHOD     *
*  SUBROUTINE WRITTEN BY DR. DAVID DANOVICH, DEPARTMENT OF *
*  CHEMISTRY, TECHNION-ISRAEL INSTITUTE OF TECHNOLOGY,     *
*  HAIFA, 32000, ISRAEL.      VERSION:  2.09.90            *
************************************************************
      INCLUDE 'SIZES'
      DIMENSION P(*)
      COMMON/DOD/ITL(200),IT(200)
      COMMON/VECTOR/C(MORB2),EIG(MAXORB),CBETA(MORB2),EIGBET(MAXORB)
      COMMON/PEREM/NYR,IY,IGGV,IGGW
      COMMON/CSUC/CS1,CS2,CS3,CS4,CS5,CS6
      KYR2=NYR+IGGV
      KYR1=NYR+1
      MYR1=NYR-IGGW
      CS1=0.D0
      CS2=0.D0
      CS3=0.D0
      CS4=0.D0
      CS5=0.D0
      CS6=0.D0
      DO 1 M=KYR1,KYR2
      DO 1 N=KYR1,KYR2
      DO 1 L=KYR1,KYR2
      DO 1 K=KYR1,KYR2
      DO 1 I=MYR1,NYR
      IYR=IY
      MM=M
      II=I
      NN=N
      IF(IYR.GE.MM) GO TO 2
      LT=IYR
      IYR=MM
      MM=LT
   2  IF(IYR.GT.NN) GO TO 3
      IF(IYR.EQ.NN) GO TO 4
      LT=IYR
      IYR=NN
      NN=LT
      LT=MM
      MM=II
      II=LT
      GO TO 3
   4  IF(MM.GE.II) GO TO 3
      LT=MM
      MM=II
      II=LT
  3   NUMB=ITL(IYR)+IT(IYR)*MM+IT(MM)+IT(NN)+II
      IYR=IY
      NN=N
      II=I
      MM=M
      IF(IYR.GE.NN) GO TO 5
      LT=IYR
      IYR=NN
      NN=LT
   5  IF(IYR.GT.MM) GO TO 6
      IF(IYR.EQ.MM) GO TO 7
      LT=IYR
      IYR=MM
      MM=LT
      LT=NN
      NN=II
      II=LT
      GO TO 6
   7  IF(NN.GE.II) GO TO 6
      LT=NN
      NN=II
      II=LT
   6  NUMB1=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(MM)+II
      KK=K
      LL=L
      MM=M
      NN=N
      IF(MM.GE.LL) GO TO 8
      LT=MM
      MM=LL
      LL=LT
   8  IF(NN.GE.KK) GO TO 9
      LT=NN
      NN=KK
      KK=LT
   9  IF(MM.GT.NN) GO TO 10
      IF(MM.EQ.NN) GO TO 11
      LT=MM
      MM=NN
      NN=LT
      LT=LL
      LL=KK
      KK=LT
      GO TO 10
   11 IF(LL.GE.KK) GO TO 10
      LT=LL
      LL=KK
      KK=LT
  10  NUMB2=ITL(MM)+IT(MM)*LL+IT(LL)+IT(NN)+KK
      IYR=IY
      LL=L
      II=I
      KK=K
      IF(IYR.GE.LL) GO TO 12
      LT=IYR
      IYR=LL
      LL=LT
  12  IF(IYR.GT.KK) GO TO 13
      IF(IYR.EQ.KK) GO TO 14
      LT=IYR
      IYR=KK
      KK=LT
      LT=LL
      LL=II
      II=LT
      GO TO 13
  14  IF(LL.GE.II) GO TO 13
      LT=LL
      LL=II
      II=LT
   13 NUMB3=ITL(IYR)+IT(IYR)*LL+IT(LL)+IT(KK)+II
      CS1=CS1+(2.D0*P(NUMB)-P(NUMB1))*P(NUMB2)*P(NUMB3)/((EP+EIG(I)-
     *EIG(M)-EIG(N))*(EP+EIG(I)-EIG(L)-EIG(K)))
   1  CONTINUE
      DO 21 M=KYR1,KYR2
      DO 21 N=KYR1,KYR2
      DO 21 I=MYR1,NYR
      DO 21 J=MYR1,NYR
      DO 21 K=MYR1,NYR
      IYR=IY
      MM=M
      II=I
      NN=N
      IF(IYR.GE.MM) GO TO 22
      LT=IYR
      IYR=MM
      MM=LT
   22 IF(IYR.GT.NN) GO TO 23
      IF(IYR.EQ.NN) GO TO 24
      LT=IYR
      IYR=NN
      NN=LT
      LT=MM
      MM=II
      II=LT
      GO TO 23
   24 IF(MM.GE.II) GO TO 23
      LT=MM
      MM=II
      II=LT
  23  NUMB=ITL(IYR)+IT(IYR)*MM+IT(MM)+IT(NN)+II
      IYR=IY
      NN=N
      II=I
      MM=M
      IF(IYR.GE.NN) GO TO 25
      LT=IYR
      IYR=NN
      NN=LT
  25  IF(IYR.GT.MM) GO TO 26
      IF(IYR.EQ.MM) GO TO 27
      LT=IYR
      IYR=MM
      MM=LT
      LT=NN
      NN=II
      II=LT
      GO TO 26
   27 IF(NN.GE.II) GO TO 26
      LT=NN
      NN=II
      II=LT
   26 NUMB1=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(MM)+II
      MM=M
      JJ=J
      NN=N
      KK=K
      IF(MM.GT.NN) GO TO 28
      IF(MM.EQ.NN) GO TO 29
      LT=MM
      MM=NN
      NN=LT
      LT=JJ
      JJ=KK
      KK=LT
      GO TO 28
   29 IF(JJ.GE.KK) GO TO 28
      LT=JJ
      JJ=KK
      KK=LT
  28  NUMB2=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+KK
      IYR=IY
      JJ=J
      II=I
      KK=K
      IF(IYR.GE.JJ) GO TO 30
      LT=IYR
      IYR=JJ
      JJ=LT
  30  IF(II.GE.KK) GO TO 31
      LT=II
      II=KK
      KK=LT
  31  IF(IYR.GT.II) GO TO 32
      IF(IYR.EQ.II) GO TO 33
      LT=IYR
      IYR=II
      II=LT
      LT=JJ
      JJ=KK
      KK=LT
      GO TO 32
  33  IF(JJ.GE.KK) GO TO 32
      LT=JJ
      JJ=KK
      KK=LT
   32 NUMB3=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(II)+KK
      CS2=CS2+(2.D0*P(NUMB)-P(NUMB1))*P(NUMB2)*P(NUMB3)/((EP+EIG(I)-
     *EIG(M)-EIG(N))*(EIG(J)+EIG(K)-EIG(M)-EIG(N)))
  21  CONTINUE
      CS3=CS2
      DO 41 M=KYR1,KYR2
      DO 41 N=KYR1,KYR2
      DO 41 L=KYR1,KYR2
      DO 41 I=MYR1,NYR
      DO 41 J=MYR1,NYR
      IYR=IY
      MM=M
      II=I
      JJ=J
      IF(IYR.GE.II) GO TO 42
      LT=IYR
      IYR=II
      II=LT
   42 IF(IYR.GT.MM) GO TO 43
      IF(IYR.EQ.MM) GO TO 44
      LT=IYR
      IYR=MM
      MM=LT
      LT=II
      II=JJ
      JJ=LT
      GO TO 43
   44 IF(II.GE.JJ) GO TO 43
      LT=II
      II=JJ
      JJ=LT
  43  NUMB=ITL(IYR)+IT(IYR)*II+IT(II)+IT(MM)+JJ
      IYR=IY
      JJ=J
      II=I
      MM=M
      IF(IYR.GE.JJ) GO TO 45
      LT=IYR
      IYR=JJ
      JJ=LT
   45 IF(IYR.GT.MM) GO TO 46
      IF(IYR.EQ.MM) GO TO 47
      LT=IYR
      IYR=MM
      MM=LT
      LT=JJ
      JJ=II
      II=LT
      GO TO 46
   47 IF(JJ.GE.II) GO TO 46
      LT=JJ
      JJ=II
      II=LT
   46 NUMB1=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(MM)+II
      II=I
      JJ=J
      NN=N
      LL=L
      IF(NN.GT.LL) GO TO 48
      IF(NN.EQ.LL) GO TO 49
      LT=NN
      NN=LL
      LL=LT
      LT=II
      II=JJ
      JJ=LT
      GO TO 48
   49 IF(II.GE.JJ) GO TO 48
      LT=II
      II=JJ
      JJ=LT
  48  NUMB2=ITL(NN)+IT(NN)*II+IT(II)+IT(LL)+JJ
      IYR=IY
      NN=N
      MM=M
      LL=L
      IF(IYR.GE.NN) GO TO 50
      LT=IYR
      IYR=NN
      NN=LT
  50  IF(MM.GE.LL) GO TO 51
      LT=MM
      MM=LL
      LL=LT
  51  IF(IYR.GT.MM) GO TO 52
      IF(IYR.EQ.MM) GO TO 53
      LT=IYR
      IYR=MM
      MM=LT
      LT=NN
      NN=LL
      LL=LT
      GO TO 52
  53  IF(NN.GE.LL) GO TO 52
      LT=NN
      NN=LL
      LL=LT
   52 NUMB3=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(MM)+LL
      CS4=CS4+(2.D0*P(NUMB)-P(NUMB1))*P(NUMB2)*P(NUMB3)/((EP+EIG(M)-
     *EIG(I)-EIG(J))*(EIG(I)+EIG(J)-EIG(N)-EIG(L)))
  41  CONTINUE
      CS5=CS4
      DO 61 M=KYR1,KYR2
      DO 61 I=MYR1,NYR
      DO 61 J=MYR1,NYR
      DO 61 K=MYR1,NYR
      DO 61 L=MYR1,NYR
      IYR=IY
      MM=M
      KK=K
      LL=L
      IF(IYR.GE.KK) GO TO 62
      LT=IYR
      IYR=KK
      KK=LT
   62 IF(IYR.GT.MM) GO TO 63
      IF(IYR.EQ.MM) GO TO 64
      LT=IYR
      IYR=MM
      MM=LT
      LT=KK
      KK=LL
      LL=LT
      GO TO 63
   64 IF(KK.GE.LL) GO TO 63
      LT=KK
      KK=LL
      LL=LT
  63  NUMB=ITL(IYR)+IT(IYR)*KK+IT(KK)+IT(MM)+LL
      IYR=IY
      LL=L
      KK=K
      MM=M
      IF(IYR.GE.LL) GO TO 65
      LT=IYR
      IYR=LL
      LL=LT
   65 IF(IYR.GT.MM) GO TO 66
      IF(IYR.EQ.MM) GO TO 67
      LT=IYR
      IYR=MM
      MM=LT
      LT=LL
      LL=KK
      KK=LT
      GO TO 66
   67 IF(LL.GE.KK) GO TO 66
      LT=LL
      LL=KK
      KK=LT
   66 NUMB1=ITL(IYR)+IT(IYR)*LL+IT(LL)+IT(MM)+KK
      KK=K
      II=I
      LL=L
      JJ=J
      IF(KK.GE.II) GO TO 68
      LT=KK
      KK=II
      II=LT
  68  IF(LL.GE.JJ) GO TO 69
      LT=LL
      LL=JJ
      JJ=LT
   69 IF(KK.GT.LL) GO TO 70
      IF(KK.EQ.LL) GO TO 71
      LT=KK
      KK=LL
      LL=LT
      LT=II
      II=JJ
      JJ=LT
      GO TO 70
   71 IF(II.GE.JJ) GO TO 70
      LT=II
      II=JJ
      JJ=LT
  70  NUMB2=ITL(KK)+IT(KK)*II+IT(II)+IT(LL)+JJ
      IYR=IY
      II=I
      MM=M
      JJ=J
      IF(IYR.GE.II) GO TO 72
      LT=IYR
      IYR=II
      II=LT
  72  IF(IYR.GT.MM) GO TO 73
      IF(IYR.EQ.MM) GO TO 74
      LT=IYR
      IYR=MM
      MM=LT
      LT=II
      II=JJ
      JJ=LT
      GO TO 73
  74  IF(II.GE.JJ) GO TO 73
      LT=II
      II=JJ
      JJ=LT
   73 NUMB3=ITL(IYR)+IT(IYR)*II+IT(II)+IT(MM)+JJ
      CS6=CS6+(2.D0*P(NUMB)-P(NUMB1))*P(NUMB2)*P(NUMB3)/((EP+EIG(M)-
     *EIG(I)-EIG(J))*(EP+EIG(M)-EIG(K)-EIG(L)))
  61  CONTINUE
      CS6=-CS6
      RETURN
      END
C
C===================================================================
C
      SUBROUTINE DSUM(P,NDIS,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
**************************************************************
*  CALCULATION 'DI' VALUES FOR GREEN'S FUNCTION METHOD       *
*  SUBROUTINE WRITTEN BY DR. DAVID DANOVICH, DEPARTMENT OF   *
*  CHEMISTRY, TECHNION-ISRAEL INSTITUTE OF TECHNOLOGY,       *
*  HAIFA, 32000, ISRAEL.            VERSION: 2.09.90         *
**************************************************************
      INCLUDE 'SIZES'
      DIMENSION P(*)
      COMMON/DOD/ITL(200),IT(200)
      COMMON/VECTOR/C(MORB2),EIG(MAXORB),DUMY(MORB2+MAXORB)
      COMMON/PEREM/NYR,IY,IGGV,IGGW
      COMMON/DSMD/DS1,DS2,DS3,DS4,DS5,DS6
      KYR2=NYR+IGGV
      KYR1=NYR+1
      MYR1=NYR-IGGW
      DS1=0.D0
      DS2=0.D0
      DS3=0.D0
      DS4=0.D0
      DS5=0.D0
      DS6=0.D0
      DO 170 M=KYR1,KYR2
         DO 170 N=KYR1,KYR2
            DO 170 L=KYR1,KYR2
               DO 170 I=MYR1,NYR
                  DO 170 J=MYR1,NYR
                     IYR=IY
                     MM=M
                     II=I
                     NN=N
                     IF(IYR.GE.MM) GO TO 10
                     LT=IYR
                     IYR=MM
                     MM=LT
   10                IF(IYR.GT.NN) GO TO 30
                     IF(IYR.EQ.NN) GO TO 20
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=MM
                     MM=II
                     II=LT
                     GO TO 30
   20                IF(MM.GE.II) GO TO 30
                     LT=MM
                     MM=II
                     II=LT
   30                NUMB=ITL(IYR)+IT(IYR)*MM+IT(MM)+IT(NN)+II
                     MM=M
                     II=I
                     JJ=J
                     LL=L
                     IF(MM.GT.LL) GO TO 50
                     IF(MM.EQ.LL) GO TO 40
                     LT=MM
                     MM=LL
                     LL=LT
                     LT=II
                     II=JJ
                     JJ=LT
                     GO TO 50
   40                IF(II.GE.JJ) GO TO 50
                     LT=II
                     II=JJ
                     JJ=LT
   50                NUMB1=ITL(MM)+IT(MM)*II+IT(II)+IT(LL)+JJ
                     IYR=IY
                     LL=L
                     JJ=J
                     NN=N
                     IF(IYR.GE.LL) GO TO 60
                     LT=IYR
                     IYR=LL
                     LL=LT
   60                IF(IYR.GT.NN) GO TO 80
                     IF(IYR.EQ.NN) GO TO 70
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=LL
                     LL=JJ
                     JJ=LT
                     GO TO 80
   70                IF(LL.GE.JJ) GO TO 80
                     LT=LL
                     LL=JJ
                     JJ=LT
   80                NUMB2=ITL(IYR)+IT(IYR)*LL+IT(LL)+IT(NN)+JJ
                     IYR=IY
                     LL=L
                     NN=N
                     JJ=J
                     IF(IYR.GE.NN) GO TO 90
                     LT=IYR
                     IYR=NN
                     NN=LT
   90                IF(IYR.GT.LL) GO TO 110
                     IF(IYR.EQ.LL) GO TO 100
                     LT=IYR
                     IYR=LL
                     LL=LT
                     LT=NN
                     NN=JJ
                     JJ=LT
                     GO TO 110
  100                IF(NN.GE.JJ) GO TO 110
                     LT=NN
                     NN=JJ
                     JJ=LT
  110                NUMB3=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(LL)+JJ
                     MM=M
                     LL=L
                     JJ=J
                     II=I
                     IF(MM.GE.LL) GO TO 120
                     LT=MM
                     MM=LL
                     LL=LT
  120                IF(JJ.GE.II) GO TO 130
                     LT=JJ
                     JJ=II
                     II=LT
  130                NUMB4=ITL(MM)+IT(MM)*LL+IT(LL)+IT(JJ)+II
                     IYR=IY
                     NN=N
                     II=I
                     MM=M
                     IF(IYR.GE.NN) GO TO 140
                     LT=IYR
                     IYR=NN
                     NN=LT
  140                IF(IYR.GT.MM) GO TO 160
                     IF(IYR.EQ.MM) GO TO 150
                     LT=IYR
                     IYR=MM
                     MM=LT
                     LT=NN
                     NN=II
                     II=LT
                     GO TO 160
  150                IF(NN.GE.II) GO TO 160
                     LT=NN
                     NN=II
                     II=LT
  160                NUMB6=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(MM)+II
                     DS1=DS1+(P(NUMB)*(P(NUMB1)*
     1(P(NUMB2)-2.D0*P(NUMB3))+P(NUMB4)*(P(NUMB3)-2.D0*P(NUMB2)))/
     2((EP+EIG(I)-EIG(M)-EIG(N))*(EP+EIG(J)-EIG(N)-EIG(L)))+
     3P(NUMB6)*(P(NUMB1)*(4.D0*P(NUMB3)-2.D0*P(NUMB2))+
     4P(NUMB4)*(P(NUMB2)-2.D0*P(NUMB3)))/
     5((EP+EIG(I)-EIG(M)-EIG(N))*(EP+EIG(J)-EIG(N)-EIG(L))))
  170 CONTINUE
      DO 350 M=KYR1,KYR2
         DO 350 N=KYR1,KYR2
            DO 350 L=KYR1,KYR2
               DO 350 I=MYR1,NYR
                  DO 350 J=MYR1,NYR
                     IYR=IY
                     MM=M
                     II=I
                     LL=L
                     IF(IYR.GE.LL) GO TO 180
                     LT=IYR
                     IYR=LL
                     LL=LT
  180                IF(IYR.GT.MM) GO TO 200
                     IF(IYR.EQ.MM) GO TO 190
                     LT=IYR
                     IYR=MM
                     MM=LT
                     LT=LL
                     LL=II
                     II=LT
                     GO TO 200
  190                IF(LL.GE.II) GO TO 200
                     LT=LL
                     LL=II
                     II=LT
  200                NUMB=ITL(IYR)+IT(IYR)*LL+IT(LL)+IT(MM)+II
                     MM=M
                     II=I
                     JJ=J
                     NN=N
                     IF(MM.GT.NN) GO TO 220
                     IF(MM.EQ.NN) GO TO 210
                     LT=MM
                     MM=NN
                     NN=LT
                     LT=II
                     II=JJ
                     JJ=LT
                     GO TO 220
  210                IF(II.GE.JJ) GO TO 220
                     LT=II
                     II=JJ
                     JJ=LT
  220                NUMB1=ITL(MM)+IT(MM)*II+IT(II)+IT(NN)+JJ
                     IYR=IY
                     LL=L
                     JJ=J
                     NN=N
                     IF(IYR.GE.LL) GO TO 230
                     LT=IYR
                     IYR=LL
                     LL=LT
  230                IF(IYR.GT.NN) GO TO 250
                     IF(IYR.EQ.NN) GO TO 240
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=LL
                     LL=JJ
                     JJ=LT
                     GO TO 250
  240                IF(LL.GE.JJ) GO TO 250
                     LT=LL
                     LL=JJ
                     JJ=LT
  250                NUMB2=ITL(IYR)+IT(IYR)*LL+IT(LL)+IT(NN)+JJ
                     IYR=IY
                     LL=L
                     NN=N
                     JJ=J
                     IF(IYR.GE.JJ) GO TO 260
                     LT=IYR
                     IYR=JJ
                     JJ=LT
  260                IF(NN.GE.LL) GO TO 270
                     LT=NN
                     NN=LL
                     LL=LT
  270                IF(IYR.GT.NN) GO TO 290
                     IF(IYR.EQ.NN) GO TO 280
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=JJ
                     JJ=LL
                     LL=LT
                     GO TO 290
  280                IF(JJ.GE.LL) GO TO 290
                     LT=JJ
                     JJ=LL
                     LL=LT
  290                NUMB3=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(NN)+LL
                     MM=M
                     NN=N
                     JJ=J
                     II=I
                     IF(MM.GT.NN) GO TO 310
                     IF(MM.EQ.NN) GO TO 300
                     LT=MM
                     MM=NN
                     NN=LT
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 310
  300                IF(JJ.GE.II) GO TO 310
                     LT=JJ
                     JJ=II
                     II=LT
  310                NUMB4=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+II
                     IYR=IY
                     LL=L
                     II=I
                     MM=M
                     IF(IYR.GE.MM) GO TO 320
                     LT=IYR
                     IYR=MM
                     MM=LT
  320                IF(IYR.GT.LL) GO TO 340
                     IF(IYR.EQ.LL) GO TO 330
                     LT=IYR
                     IYR=LL
                     LL=LT
                     LT=MM
                     MM=II
                     II=LT
                     GO TO 340
  330                IF(MM.GE.II) GO TO 340
                     LT=MM
                     MM=II
                     II=LT
  340                NUMB5=ITL(IYR)+IT(IYR)*MM+IT(MM)+IT(LL)+II
                     DS2=DS2+(P(NUMB)*(P(NUMB1)*(4.D0*P(NUMB2)-
     12.D0*P(NUMB3))+P(NUMB4)*(P(NUMB3)-2.D0*P(NUMB2)))/
     2((EP+EIG(I)-EIG(M)-EIG(L))*(EIG(I)+EIG(J)-EIG(M)-EIG(N)))+
     3P(NUMB5)*(P(NUMB1)*(P(NUMB3)-2.D0*P(NUMB2))+
     4P(NUMB4)*(P(NUMB2)-2.D0*P(NUMB3)))/
     5((EP+EIG(I)-EIG(M)-EIG(L))*(EIG(I)+EIG(J)-EIG(M)-EIG(N))))
  350 CONTINUE
      DS3=DS2
      DO 530 M=KYR1,KYR2
         DO 530 N=KYR1,KYR2
            DO 530 I=MYR1,NYR
               DO 530 J=MYR1,NYR
                  DO 530 K=MYR1,NYR
                     IYR=IY
                     MM=M
                     KK=K
                     JJ=J
                     IF(IYR.GE.KK) GO TO 360
                     LT=IYR
                     IYR=KK
                     KK=LT
  360                IF(IYR.GT.MM) GO TO 380
                     IF(IYR.EQ.MM) GO TO 370
                     LT=IYR
                     IYR=MM
                     MM=LT
                     LT=KK
                     KK=JJ
                     JJ=LT
                     GO TO 380
  370                IF(KK.GE.JJ) GO TO 380
                     LT=KK
                     KK=JJ
                     JJ=LT
  380                NUMB=ITL(IYR)+IT(IYR)*KK+IT(KK)+IT(MM)+JJ
                     MM=M
                     II=I
                     JJ=J
                     NN=N
                     IF(MM.GT.NN) GO TO 400
                     IF(MM.EQ.NN) GO TO 390
                     LT=MM
                     MM=NN
                     NN=LT
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 400
  390                IF(JJ.GE.II) GO TO 400
                     LT=JJ
                     JJ=II
                     II=LT
  400                NUMB1=ITL(MM)+IT(MM)*JJ+IT(JJ)+IT(NN)+II
                     IYR=IY
                     KK=K
                     II=I
                     NN=N
                     IF(IYR.GE.KK) GO TO 410
                     LT=IYR
                     IYR=KK
                     KK=LT
  410                IF(IYR.GT.NN) GO TO 430
                     IF(IYR.EQ.NN) GO TO 420
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=KK
                     KK=II
                     II=LT
                     GO TO 430
  420                IF(KK.GE.II) GO TO 430
                     LT=KK
                     KK=II
                     II=LT
  430                NUMB2=ITL(IYR)+IT(IYR)*KK+IT(KK)+IT(NN)+II
                     IYR=IY
                     II=I
                     NN=N
                     KK=K
                     IF(IYR.GE.NN) GO TO 440
                     LT=IYR
                     IYR=NN
                     NN=LT
  440                IF(II.GE.KK) GO TO 450
                     LT=II
                     II=KK
                     KK=LT
  450                IF(IYR.GT.II) GO TO 470
                     IF(IYR.EQ.II) GO TO 460
                     LT=IYR
                     IYR=II
                     II=LT
                     LT=NN
                     NN=KK
                     KK=LT
                     GO TO 470
  460                IF(NN.GE.KK) GO TO 470
                     LT=NN
                     NN=KK
                     KK=LT
  470                NUMB3=ITL(IYR)+IT(IYR)*NN+IT(NN)+IT(II)+KK
                     MM=M
                     NN=N
                     JJ=J
                     II=I
                     IF(NN.GT.MM) GO TO 490
                     IF(NN.EQ.MM) GO TO 480
                     LT=NN
                     NN=MM
                     MM=LT
                     LT=JJ
                     JJ=II
                     II=LT
                     GO TO 490
  480                IF(JJ.GE.II) GO TO 490
                     LT=JJ
                     JJ=II
                     II=LT
  490                NUMB4=ITL(NN)+IT(NN)*JJ+IT(JJ)+IT(MM)+II
                     IYR=IY
                     JJ=J
                     KK=K
                     MM=M
                     IF(IYR.GE.JJ) GO TO 500
                     LT=IYR
                     IYR=JJ
                     JJ=LT
  500                IF(IYR.GT.MM) GO TO 520
                     IF(IYR.EQ.MM) GO TO 510
                     LT=IYR
                     IYR=MM
                     MM=LT
                     LT=JJ
                     JJ=KK
                     KK=LT
                     GO TO 520
  510                IF(JJ.GE.KK) GO TO 520
                     LT=JJ
                     JJ=KK
                     KK=LT
  520                NUMB5=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(MM)+KK
                     DS4=DS4+(P(NUMB)*(P(NUMB1)*(4.D0*P(NUMB2)-
     12.D0*P(NUMB3))+P(NUMB4)*(P(NUMB3)-2.D0*P(NUMB2)))/
     2((EP+EIG(M)-EIG(J)-EIG(K))*(EIG(I)+EIG(J)-EIG(M)-EIG(N)))+
     3P(NUMB5)*(P(NUMB1)*(P(NUMB3)-2.D0*P(NUMB2))+
     4P(NUMB4)*(P(NUMB2)-2*P(NUMB3)))/
     5((EP+EIG(M)-EIG(J)-EIG(K))*(EIG(I)+EIG(J)-EIG(M)-EIG(N))))
  530 CONTINUE
      DS5=DS4
      DO 720 M=KYR1,KYR2
         DO 720 N=KYR1,KYR2
            DO 720 I=MYR1,NYR
               DO 720 J=MYR1,NYR
                  DO 720 K=MYR1,NYR
                     IYR=IY
                     MM=M
                     KK=K
                     II=I
                     IF(IYR.GE.KK) GO TO 540
                     LT=IYR
                     IYR=KK
                     KK=LT
  540                IF(IYR.GT.MM) GO TO 560
                     IF(IYR.EQ.MM) GO TO 550
                     LT=IYR
                     IYR=MM
                     MM=LT
                     LT=KK
                     KK=II
                     II=LT
                     GO TO 560
  550                IF(KK.GE.II) GO TO 560
                     LT=KK
                     KK=II
                     II=LT
  560                NUMB=ITL(IYR)+IT(IYR)*KK+IT(KK)+IT(MM)+II
                     MM=M
                     II=I
                     JJ=J
                     NN=N
                     IF(MM.GT.NN) GO TO 580
                     IF(MM.EQ.NN) GO TO 570
                     LT=MM
                     MM=NN
                     NN=LT
                     LT=II
                     II=JJ
                     JJ=LT
                     GO TO 580
  570                IF(II.GE.JJ) GO TO 580
                     LT=II
                     II=JJ
                     JJ=LT
  580                NUMB1=ITL(MM)+IT(MM)*II+IT(II)+IT(NN)+JJ
                     IYR=IY
                     KK=K
                     JJ=J
                     NN=N
                     IF(IYR.GE.KK) GO TO 590
                     LT=IYR
                     IYR=KK
                     KK=LT
  590                IF(IYR.GT.NN) GO TO 610
                     IF(IYR.EQ.NN) GO TO 600
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=KK
                     KK=JJ
                     JJ=LT
                     GO TO 610
  600                IF(KK.GE.JJ) GO TO 610
                     LT=KK
                     KK=JJ
                     JJ=LT
  610                NUMB2=ITL(IYR)+IT(IYR)*KK+IT(KK)+IT(NN)+JJ
                     IYR=IY
                     JJ=J
                     NN=N
                     KK=K
                     IF(IYR.GE.JJ) GO TO 620
                     LT=IYR
                     IYR=JJ
                     JJ=LT
  620                IF(IYR.GT.NN) GO TO 640
                     IF(IYR.EQ.NN) GO TO 630
                     LT=IYR
                     IYR=NN
                     NN=LT
                     LT=JJ
                     JJ=KK
                     KK=LT
                     GO TO 640
  630                IF(JJ.GE.KK) GO TO 640
                     LT=JJ
                     JJ=KK
                     KK=LT
  640                NUMB3=ITL(IYR)+IT(IYR)*JJ+IT(JJ)+IT(NN)+KK
                     MM=M
                     NN=N
                     JJ=J
                     II=I
                     IF(II.GE.JJ) GO TO 650
                     LT=II
                     II=JJ
                     JJ=LT
  650                IF(NN.GE.MM) GO TO 660
                     LT=NN
                     NN=MM
                     MM=LT
  660                IF(II.GT.NN) GO TO 680
                     IF(II.EQ.NN) GO TO 670
                     LT=II
                     II=NN
                     NN=LT
                     LT=JJ
                     JJ=MM
                     MM=LT
                     GO TO 680
  670                IF(JJ.GE.MM) GO TO 680
                     LT=JJ
                     JJ=MM
                     MM=LT
  680                NUMB4=ITL(II)+IT(II)*JJ+IT(JJ)+IT(NN)+MM
                     IYR=IY
                     II=I
                     KK=K
                     MM=M
                     IF(IYR.GE.II) GO TO 690
                     LT=IYR
                     IYR=II
                     II=LT
  690                IF(IYR.GT.MM) GO TO 710
                     IF(IYR.EQ.MM) GO TO 700
                     LT=IYR
                     IYR=MM
                     MM=LT
                     LT=II
                     II=KK
                     KK=LT
                     GO TO 710
  700                IF(II.GE.KK) GO TO 710
                     LT=II
                     II=KK
                     KK=LT
  710                NUMB5=ITL(IYR)+IT(IYR)*II+IT(II)+IT(MM)+KK
                     DS6=DS6+(P(NUMB)*(P(NUMB1)*(4.D0*P(NUMB2)-
     12.D0*P(NUMB3))+P(NUMB4)*(P(NUMB3)-2.D0*P(NUMB2)))/
     2((EP+EIG(M)-EIG(I)-EIG(K))*(EP+EIG(N)-EIG(J)-EIG(K)))+
     3P(NUMB5)*(P(NUMB1)*(P(NUMB3)-2.D0*P(NUMB2))+
     4P(NUMB4)*(P(NUMB2)-2.D0*P(NUMB3)))/
     5((EP+EIG(M)-EIG(I)-EIG(K))*(EP+EIG(N)-EIG(J)-EIG(K))))
  720 CONTINUE
      DS6=-DS6
      RETURN
      END
C
C====================================================================
C
      SUBROUTINE FCNPP(P,NDIS)
*****************************************************************
*     MAIN PROGRAM FOR GREEN'S FUNCTION CALCULATIONS            *
*     SUBROUTINE WRITTEN BY DR. D.DANOVICH, COMPUTATION         *
*     CHEMISTRY CENTRE, DEPARTMENT OF CHEMISTRY, TECHNION-      *
*     ISRAEL INSTITUTE OF TECHNOLOGY, HAIFA, 32000, ISRAEL      *
*     VERSION   2.09.90                                         *
*****************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION P(*),EG2(20),EG23(20),EGA(20),IG2(20),IG23(20),
     .IGA(20),RIR(20),PRIR(20)
      COMMON/MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     * NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,NOPEN,NDUMY,
     * FRACT
      COMMON/DOD/ITL(200),IT(200)
      COMMON/DAVI/INTSO
      COMMON/PEREM/NYR,IYR,IGGV,IGGW
      COMMON/CSUC/CS1,CS2,CS3,CS4,CS5,CS6
      COMMON/ASMA/AS1,AS2,AS3,AS4,AS5,AS6
      COMMON/DSMD/DS1,DS2,DS3,DS4,DS5,DS6
      COMMON/SUMA/SU2R,EPS,SUM1,SUM2
      COMMON/VECTOR/C(MORB2),EIG(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /OUTFIL/ WU
      INTEGER WU
      DIMENSION VALUE(40)
      CHARACTER LINE*80
      WRITE(WU,1) INTSO,NORBS
   1  FORMAT(//5X,'number of nonzero integrals =',I10,5X,
     *'Total number of MO  =',I4)
      WRITE(WU,2)
   2  FORMAT(//35X,'  Orbital energy ')
      IF(NORBS.GT.200) STOP
      WRITE(WU,3) (I,I=1,NORBS)
   3  FORMAT(/5X,10I7)
      WRITE(WU,4) (EIG(I),I=1,NORBS)
  4   FORMAT(//5X,10F7.3)
      NUMB=NELECS/2
      WRITE(WU,5) NUMB
   5  FORMAT(//5X,' Higher occupied MO  is  ',I4,'  MO ')
      DO 2000 I=1,20
      EG2(I)=0.D0
      EG23(I)=0.D0
      EGA(I)=0.D0
      RIR(I)=0.D0
      PRIR(I)=0.D0
      IG2(I)=0
      IG23(I)=0
 2000 IGA(I)=0
      READ(5,'(A)') LINE
      CALL NUCHAR(LINE,VALUE,NVALUE)
      IGGWW=VALUE(1)
      IGGV=VALUE(2)
      NIZ=VALUE(3)
      IVER=VALUE(4)
      IEPS=VALUE(5)
      ITER2=VALUE(6)
      ITER3=VALUE(7)
      IFULIT=VALUE(8)
      IPRINT=VALUE(9)
      IT23=VALUE(10)
      EPS=10.D0**(-IEPS)
      IGGW=IGGWW-1
      WRITE(WU,120) IGGWW,IGGV,NIZ,IVER,EPS,ITER2,ITER3,IFULIT,
     *IPRINT,IT23
 120  FORMAT(///5X,'TOTAL NUMBER OF OCCUPIED ORBITALS INVOLVED IN THE'/
     *5X,' OVGF CALCULATION =',I3/5X,'TOTAL NUMBER OF UNOCCUPIED ',
     *'ORBITALS INVOLVED IN THE'/5X,' OVGF CALCULATION =',I3/5X,
     *'THE NUMBER OF FIRST OCCUPIED MO FOR WHICH OVGF CORRECTION '/5X,
     *'MUST BE STARTED =',I3/5X,
     *'THE NUMBER OF LAST OCCUPIED MO FOR WHICH OVGF CORRECTION '/5X,
     *'MUST BE  FINISHED =',I3/5X,
     *'TOLERANCE FACTOR = ',F12.8/5X,
     *'SECOND-ORDER CONTRIBUTION = ',I3/5X,
     *'THIRD-ORDER CONTRIBUTION = ',I3/5X,
     *'FULL EXPRESSION = ',I3/5X,
     *'PRINT FACTOR = ',I3/5X,
     *'ONLY SECOND-ORDER CONTRIBUTION = ',I3)
      NYR=NUMB
      NUMH=NIZ
      NUMV=IVER
      IF(ITER2.NE.0) GO TO 16
      III=0
      IF(NUMH.LE.0) NUMH=1
      DO 10 IYR=NUMH,NUMV
      EP=EIG(IYR)
      IF(IPRINT.NE.0) PRINT 122, EP
      CALL SUMA2(P,NDIS,EP)
      IR=0
      W=EP+SU2R
  11  IR=IR+1
      CALL SUMA2(P,NDIS,W)
      W1=EP+SU2R
      IF(DABS(W1-W).LT.EPS) GO TO 12
      W=W1
      GO TO 11
  12  III=III+1
      EG2(III)=W1
      IG2(III)=IR
      IF(IPRINT.NE.0) WRITE(WU,154) W1,IR
  10  CONTINUE
   16 NUMH=NIZ
      NUMV=IVER
      IF(IT23.NE.0) GO TO 1000
      IF(ITER3.NE.0) GO TO 17
      III=0
      IF(NUMH.LE.0) NUMH=1
      DO 18 IYR=NUMH,NUMV
      EP=EIG(IYR)
      IF(IPRINT.NE.0) WRITE(WU,122) EP
      CALL SUMA2(P,NDIS,EP)
      CALL ASUM(P,NDIS)
      CALL CSUM(P,NDIS,EP)
      CALL DSUM(P,NDIS,EP)
      IR=0
      SU3R=AS1+CS1+DS1+AS2+CS2+DS2+AS3+CS3+DS3+AS4+CS4+DS4+AS5+CS5+
     *DS5+AS6+CS6+DS6
      W=EP+SU2R+SU3R
  19  IR=IR+1
      CALL SUMA2(P,NDIS,W)
      CALL CSUM(P,NDIS,W)
      CALL DSUM(P,NDIS,W)
      SU3R=AS1+CS1+DS1+AS2+CS2+DS2+AS3+CS3+DS3+AS4+CS4+DS4+AS5+CS5+
     *DS5+AS6+CS6+DS6
      W1=EP+SU2R+SU3R
      IF(DABS(W1-W).LT.EPS) GO TO 20
      W=W1
      GO TO 19
   20 III=III+1
      EG23(III)=W1
      IG23(III)=IR
      IF(IPRINT.NE.0) WRITE(WU,154) W1,IR
   18 CONTINUE
   17 NUMH=NIZ
      NUMV=IVER
      IF(NUMH.LE.0) NUMH=1
      WRITE(WU,121) NUMH,NUMV
 121  FORMAT(/5X,'NUMH =',I4,5X,'NUMV =',I4)
      IF(IFULIT.GT.0) GO TO 23
      IF(IFULIT.LT.0) GO TO 24
      III=0
      DO 25 IYR=NUMH,NUMV
      EP=EIG(IYR)
      IF(IPRINT.NE.0) WRITE(WU,122) EP
 122  FORMAT(/5X,'EP =',F18.8)
      CALL SUMA2(P,NDIS,EP)
      CALL ASUM(P,NDIS)
      CALL CSUM(P,NDIS,EP)
      CALL DSUM(P,NDIS,EP)
      IR=0
      SU3R=AS1+CS1+DS1+AS2+CS2+DS2+AS3+CS3+DS3+AS4+CS4+DS4+AS5+CS5+DS5+
     *AS6+CS6+DS6
      IF(SU2R.EQ.0.D0) GO TO 26
      FACA=-(CS2+CS3+CS4+CS5+DS2+DS3+DS4+DS5)/SU2R
      GO TO 27
   26 WRITE(WU,28)
   28 FORMAT(/5X,' SU2R=0 ON 0  STEP ')
      FACA=0.D0
   27 W=EP+SU2R+(1+FACA)**(-1)*SU3R
   29 IR=IR+1
      CALL SUMA2(P,NDIS,W)
      CALL CSUM(P,NDIS,W)
      CALL DSUM(P,NDIS,W)
      SU3R=AS1+CS1+DS1+AS2+CS2+DS2+AS3+CS3+DS3+AS4+CS4+DS4+AS5+CS5+DS5+
     *AS6+CS6+DS6
      IF(SU2R.EQ.0.D0) GO TO 30
      FACA=-(CS2+CS3+CS4+CS5+DS2+DS3+DS4+DS5)/SU2R
      GO TO 31
   30 WRITE(WU,32) IR
  32  FORMAT(/5X,' SU2R =0 ON ',I5,' STEP ')
      FACA=0.D0
  31  W1=EP+SU2R+(1+FACA)**(-1)*SU3R
      IF(DABS(W1-W).LT.EPS) GO TO 33
      W=W1
      GO TO 29
  33  III=III+1
      EGA(III)=W1
      IGA(III)=IR
      IF(IPRINT.NE.0) WRITE(WU,154) W1,IR
 154  FORMAT(/5X,' W1 = ',F12.8,5X,' IR = ',I5)
  25  CONTINUE
      GO TO 1000
  23  III=0
      DO 127 IYR=NUMH,NUMV
      EP=EIG(IYR)
      IF(IPRINT.NE.0) WRITE(WU,122) EP
      CALL SUMA2(P,NDIS,EP)
      CALL ASUM(P,NDIS)
      CALL CSUM(P,NDIS,EP)
      CALL DSUM(P,NDIS,EP)
      IR=0
      IF(SUM1.EQ.0.D0) GO TO 34
      G1=-(CS4+CS5+DS4+DS5)/SUM1
      GO TO 35
  34  WRITE(WU,36)
  36  FORMAT(/5X,'SUM1=0 ON 0  STEP  ')
      G1=0.D0
   35 IF(SUM2.EQ.0.D0) GO TO 37
      G2=-(CS2+CS3+DS2+DS3)/SUM2
      GO TO 38
   37 WRITE(WU,39)
   39 FORMAT(/5X,' SUM2 = 0 ON 0  STEP ')
      G2=0.D0
   38 W=EP+SU2R+(1+G1)**(-1)*(CS4+CS5+CS6+DS4+DS5+DS6)+(1+G2)**(-1)*
     *(CS1+CS2+CS3+DS1+DS2+DS3)+(AS1+AS2+AS3+AS4+AS5+AS6)
  40  IR=IR+1
      CALL SUMA2(P,NDIS,W)
      CALL CSUM(P,NDIS,W)
      CALL DSUM(P,NDIS,W)
      IF(SUM1.EQ.0.D0) GO TO 41
      G1=-(CS4+CS5+DS4+DS5)/SUM1
      GO TO 42
  41  WRITE(WU,43) IR
  43  FORMAT(/5X,'SUM1=0 ON',I5,' STEP  ')
      G1=0.D0
   42 IF(SUM2.EQ.0.D0) GO TO 44
      G2=-(CS2+CS3+DS2+DS3)/SUM2
      GO TO 45
   44 WRITE(WU,46) IR
   46 FORMAT(/5X,' SUM2 = 0 ON',I5,'  STEP ')
      G2=0.D0
   45 W1=EP+SU2R+(1+G1)**(-1)*(CS4+CS5+CS6+DS4+DS5+DS6)+(1+G2)**(-1)*
     *(CS1+CS2+CS3+DS1+DS2+DS3)+(AS1+AS2+AS3+AS4+AS5+AS6)
      IF(DABS(W1-W).LT.EPS) GO TO 47
      W=W1
      GO TO 40
   47 III=III+1
      EGA(III)=W1
      IGA(III)=IR
      IF(IPRINT.NE.0) WRITE(WU,154) W1,IR
 127  CONTINUE
      GO TO 1000
   24 III=0
      DO 48 IYR=NUMH,NUMV
      EP=EIG(IYR)
      IF(IPRINT.NE.0) WRITE(WU,122) EP
      CALL SUMA2(P,NDIS,EP)
      CALL ASUM(P,NDIS)
      CALL CSUM(P,NDIS,EP)
      CALL DSUM(P,NDIS,EP)
      IR=0
      IF(SUM1.EQ.0.D0) GO TO 49
      G1=-(CS4+CS5+DS4+DS5)/SUM1
      GO TO 50
  49  WRITE(WU,51)
  51  FORMAT(/5X,'SUM1 B 3 =0 ON 0  STEP  ')
      G1=0.D0
   50 IF(SUM2.EQ.0.D0) GO TO 52
      G2=-(CS2+CS3+DS2+DS3)/SUM2
      GO TO 53
   52 WRITE(WU,54)
   54 FORMAT(/5X,' SUM2 B 3 = 0 ON 0  STEP ')
      G2=0.D0
   53 SZAM=CS1+DS1+CS2+DS2+CS3+DS3+CS4+DS4+CS5+DS5+CS6+DS6
      IF(SZAM.NE.0.D0) GO TO 55
      WRITE(WU,56)
  56  FORMAT(/5X,' SZAM 0 ON 0  STEP ')
      A=0.D0
      GO TO 57
  55  A=(G1*(CS4+CS5+CS6+DS4+DS5+DS6)+G2*(CS1+CS2+CS3+DS1+DS2+DS3))/SZAM
  57  SU3R=AS1+CS1+DS1+AS2+CS2+DS2+AS3+CS3+DS3+AS4+CS4+DS4+AS5+CS5+DS5+
     *AS6+CS6+DS6
      W=EP+SU2R+(1+A)**(-1)*SU3R
  58  IR=IR+1
      CALL SUMA2(P,NDIS,W)
      CALL CSUM(P,NDIS,W)
      CALL DSUM(P,NDIS,W)
      IF(SUM1.EQ.0.D0) GO TO 59
      G1=-(CS4+CS5+DS4+DS5)/SUM1
      GO TO 60
  59  WRITE(WU,61) IR
  61  FORMAT(/5X,'SUM1=0 ON',I5,'  STEP ')
      G1=0.D0
   60 IF(SUM2.EQ.0.D0) GO TO 62
      G2=-(CS2+CS3+DS2+DS3)/SUM2
      GO TO 63
   62 WRITE(WU,64) IR
   64 FORMAT(/5X,' SUM2 = 0 ON',I5,'  STEP ')
      G2=0.D0
   63 SZAM=CS1+DS1+CS2+DS2+CS3+DS3+CS4+DS4+CS5+DS5+CS6+DS6
      IF(SZAM.NE.0.D0) GO TO 65
      WRITE(WU,66) IR
  66  FORMAT(/5X,' SZAM 0 ON',I5,'  STEP ')
      A=0.D0
      GO TO 67
  65  A=(G1*(CS4+CS5+CS6+DS4+DS5+DS6)+G2*(CS1+CS2+CS3+DS1+DS2+DS3))/SZAM
  67  SU3R=AS1+CS1+DS1+AS2+CS2+DS2+AS3+CS3+DS3+AS4+CS4+DS4+AS5+CS5+DS5+
     *AS6+CS6+DS6
      W1=EP+SU2R+(1+A)**(-1)*SU3R
      IF(DABS(W1-W).LT.EPS) GO TO 68
      W=W1
      GO TO 58
   68 III=III+1
      EGA(III)=W1
      IGA(III)=IR
      IF(IPRINT.NE.0) WRITE(WU,154) W1,IR
  48  CONTINUE
 1000 WRITE(WU,13)
   13 FORMAT(///5X,' RESULTS OF CALCULATION PES  ')
      WRITE(WU,14)
  14  FORMAT(/1X,'LEVEL ',3X,' 2   ',2X,' IR ',3X,' 3   ',2X,
     *' IR ',3X,' FINAL ',2X,' IR ',3X,' SCF')
      DO 81 II=1,III
      WRITE(WU,82) NUMH,EG2(II),IG2(II),EG23(II),IG23(II),EGA(II),
     *IGA(II),EIG(NUMH)
      NUMH=NUMH+1
  81  CONTINUE
  82  FORMAT(/I3,5X,F7.3,3X,I2,2X,F7.3,3X,I2,2X,F7.3,4X,I2,4X,
     *F7.3)
      WRITE(WU,83) IGGWW,IGGV,EPS,IFULIT
  83  FORMAT(//5X,' IN CALCULATED USED ',I5,' HOMO  and ',
     *I5,' LUMO '/5X,' ACCURACY SELFCONSISTENT  = ',F12.8/5X,
     *' FINAL RESULT CALCULATED BY ',I3,'  METHOD ')
      WRITE(WU,84)
   84 FORMAT(//6X,'FINAL=0, CALCULATED BY  FORMULA W=EP+SUM2+(1+A)',
     1'**(-1)*SU3R'/6X,
     *'FINAL=1, CALCULATED BY FORMULA W=EP+SUM2+(1+G1)**-1)'/3X,
     1'*(CS4+CS5+CS6+DS4+DS5+DS6)+(1+G2)**(-1)*(CS1+CS2+CS3+DS1'/3X,
     2'+DS2+DS3)+AA'/6X,
     *'FINAL= -1, CALCULATED BY FORMULA W=EP+SUM2+(1+A)**(-1)*SUM3')
      NUMH=NIZ
      NUMV=IVER
      NUM1=NUMB-IGGW
      NUM2=NUMB+IGGV
      KIL=0
      DO 350 I=NUMH,NUMV
      ZSUMA=0.D0
      NAMB=NUMB+1
      DO 301 J=NUM1,NUMB
      DO 301 L=NAMB,NUM2
      JJ=J
      LL=L
      II=I
      IF(JJ.GE.LL) GO TO 302
      LT=JJ
      JJ=LL
      LL=LT
 302  IF(JJ.GT.II) GO TO 303
      IF(JJ.EQ.II) GO TO 304
 306  NYMB=ITL(II)+IT(II)*II+IT(II)+IT(JJ)+LL
      GO TO 305
 304  IF(LL.GE.II) GO TO 303
      GO TO 306
 303  NYMB=ITL(JJ)+IT(JJ)*LL+IT(LL)+IT(II)+II
 305  JJ=J
      LL=L
      II=I
      IF(JJ.GE.II.AND.II.GE.LL) GO TO 307
      IF(JJ.GE.II.AND.LL.GE.II.AND.JJ.GE.LL) GO TO 308
      IF(JJ.GE.II.AND.LL.GE.II.AND.LL.GE.JJ) GO TO 309
      IF(II.GE.JJ.AND.II.GE.LL.AND.JJ.GE.LL) GO TO 310
      IF(II.GE.JJ.AND.II.GE.LL.AND.LL.GE.JJ) GO TO 311
      IF(II.GE.JJ.AND.LL.GE.II) GO TO 312
      WRITE(WU,320) JJ,II,LL
 320  FORMAT(5X,'JJ=',I5,5X,'II=',I5,5X,'LL=',I5)
      STOP
 307  NYMB1=ITL(JJ)+IT(JJ)*II+IT(II)+IT(II)+LL
      GO TO 313
 308  NYMB1=ITL(JJ)+IT(JJ)*II+IT(II)+IT(LL)+II
      GO TO 313
 309  NYMB1=ITL(LL)+IT(LL)*II+IT(II)+IT(JJ)+II
      GO TO 313
 310  NYMB1=ITL(II)+IT(II)*JJ+IT(JJ)+IT(II)+LL
      GO TO 313
 311  NYMB1=ITL(II)+IT(II)*LL+IT(LL)+IT(II)+JJ
      GO TO 313
 312  NYMB1=ITL(LL)+IT(LL)*II+IT(II)+IT(II)+JJ
 313  ZVERX=(P(NYMB)-0.5D0*P(NYMB1))**2
      ZNIZ=EIG(L)-EIG(J)
      PS=ZVERX/ZNIZ
      ZSUMA=ZSUMA+PS
 301  CONTINUE
      KIL=KIL+1
      RIR(KIL)=2.D0*ZSUMA
 350  CONTINUE
      WRITE(WU,355)
 355  FORMAT(//5X,' CALCULATION RELAXATION EFFECTS ONLY  ')
      DO 356 I=1,III
      PRIR(I)=-EIG(NUMH)-RIR(I)
      NUMH=NUMH+1
 356  CONTINUE
      NUMH=NIZ
      WRITE(WU,359)
 359  FORMAT(/3X,'LEVEL',2X,'R.effect',3X,'ENERGY SCF',5X,'ENERGY')
      DO 357 I=1,III
      WRITE(WU,358) NUMH,RIR(I),EIG(NUMH),PRIR(I)
      NUMH=NUMH+1
 357  CONTINUE
 358  FORMAT(/3X,I3,2X,F10.6,3X,F8.4,8X,F8.4)
      RETURN
      END
C
C===================================================================
C
