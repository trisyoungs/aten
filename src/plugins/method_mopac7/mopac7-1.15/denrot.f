      SUBROUTINE DENROT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /SCRACH/ B(MAXORB*MAXORB), BONDAB(MAXPAR**2-MAXORB*MAXORB)
************************************************************************
*
* DENROT PRINTS THE DENSITY MATRIX AS (S-SIGMA, P-SIGMA, P-PI) RATHER
*        THAN (S, PX, PY, PZ).
*
************************************************************************
      DIMENSION AROT(9,9), C(3,5,5), PAB(9,9), VECT(9,9)
      DIMENSION NATOM(MAXORB)
      DIMENSION XYZ(3,NUMATM), IROT(5,35), ISP(9)
      CHARACTER * 6 LINE(21)
      CHARACTER ELEMNT*2,ATORBS(9)*7,ITEXT(MAXORB)*7,JTEXT(MAXORB)*2
      SAVE ATORBS, IROT, ISP
      DATA ATORBS/'S-SIGMA','P-SIGMA','  P-PI ','  P-PI ','D-SIGMA',
     1            '  D-PI ','  D-PI ',' D-DELL',' D-DELL'/
***********************************************************************
* IROT IS A MAPPING LIST. FOR EACH ELEMENT OF AROT 5 NUMBERS ARE
* NEEDED. THESE ARE, IN ORDER, FIRST AND SECOND SUBSCRIPTS OF AROT,
* AND FIRST,SECOND, AND THIRD SUBSCRIPTS OF C, THUS THE FIRST
* LINE OF IROT DEFINES AROT(1,1)=C(1,3,3)
*
***********************************************************************
      DATA IROT/1,1,1,3,3, 2,2,2,4,3, 3,2,2,2,3, 4,2,2,3,3, 2,3,2,4,2,
     1          3,3,2,2,2, 4,3,2,3,2, 2,4,2,4,4, 3,4,2,2,4, 4,4,2,3,4,
     2          5,5,3,1,5, 6,5,3,4,3, 7,5,3,3,3, 8,5,3,2,3, 9,5,3,5,3,
     3          5,6,3,1,2, 6,6,3,4,2, 7,7,3,3,2, 8,6,3,2,2, 9,6,3,5,2,
     4          5,7,3,1,4, 6,7,3,4,4, 7,7,3,3,4, 8,7,3,2,4, 9,7,3,5,4,
     5          5,8,3,1,1, 6,8,3,4,1, 7,8,3,3,1, 8,8,3,2,1, 9,8,3,5,1,
     6          5,9,3,1,5, 6,9,3,4,5, 7,9,3,3,5, 8,9,3,2,5, 9,9,3,5,5/
      DATA ISP /1,2,3,3,4,5,5,6,6/
      CALL GMETRY(GEO,XYZ)
      IPRT=0
      DO 120 I=1,NUMAT
         IF=NFIRST(I)
         IL=NLAST(I)
         IPQ=IL-IF-1
         II=IPQ+2
         IF(II.EQ.0)GOTO 120
         DO 10 I1=1,II
            J1=IPRT+ISP(I1)
            ITEXT(J1)=ATORBS(I1)
            JTEXT(J1)=ELEMNT(NAT(I))
            NATOM(J1)=I
   10    CONTINUE
         IPRT=J1
         IF(IPQ.NE.2)IPQ=MIN(MAX(IPQ,1),3)
         DO 110 J=1,I
            JF=NFIRST(J)
            JL=NLAST(J)
            JPQ=JL-JF-1
            JJ=JPQ+2
            IF(JJ.EQ.0)GOTO 110
            IF(JPQ.NE.2)JPQ=MIN(MAX(JPQ,1),3)
            DO 20 I1=1,9
               DO 20 J1=1,9
   20       PAB(I1,J1)=0.D0
            KK=0
            DO 30 K=IF,IL
               KK=KK+1
               LL=0
               DO 30 L=JF,JL
                  LL=LL+1
   30       PAB(KK,LL)=P(L+(K*(K-1))/2)
            CALL COE(XYZ(1,I),XYZ(2,I),XYZ(3,I),
     1                 XYZ(1,J),XYZ(2,J),XYZ(3,J),IPQ,JPQ,C,R)
            DO 40 I1=1,9
               DO 40 J1=1,9
   40       AROT(I1,J1)=0.D0
            DO 50 I1=1,35
   50       AROT(IROT(1,I1),IROT(2,I1))=
     1            C(IROT(3,I1),IROT(4,I1),IROT(5,I1))
            L1=ISP(II)
            L2=ISP(JJ)
            DO 60 I1=1,9
               DO 60 J1=1,9
   60       VECT(I1,J1)=-1.D0
            DO 70 I1=1,L1
               DO 70 J1=1,L2
   70       VECT(I1,J1)=0.D0
            IF(I.NE.J) THEN
               IJ=MAX(II,JJ)
               DO 90 I1=1,II
                  DO 90 J1=1,JJ
                     SUM=0.D0
                     DO 80 L1=1,IJ
                        DO 80 L2=1,IJ
   80                SUM=SUM+AROT(L1,I1)*PAB(L1,L2)*AROT(L2,J1)
   90          VECT(ISP(I1),ISP(J1))=
     1                        VECT(ISP(I1),ISP(J1))+SUM**2
            ENDIF
            K=0
            DO  100 I1=IF,IL
               K=K+1
               L=0
               DO 100 J1=JF,JL
                  L=L+1
  100       IF(J1.LE.I1) B(J1+(I1*(I1-1))/2)=VECT(K,L)
  110    CONTINUE
  120 CONTINUE
C
C NOW TO REMOVE ALL THE DEAD SPACE IN P, CHARACTERIZED BY -1.0
C
      LINEAR=(NORBS*(NORBS+1))/2
      L=0
      DO 130 I=1,LINEAR
         IF(B(I).GT.-0.1) THEN
            L=L+1
            B(L)=B(I)
         ENDIF
  130 CONTINUE
C
C   PUT ATOMIC ORBITAL VALENCIES ONTO THE DIAGONAL
C
      DO 160 I=1,IPRT
         SUM=0.D0
         II=(I*(I-1))/2
         DO 140 J=1,I
  140    SUM=SUM+B(J+II)
         DO 150 J=I+1,IPRT
  150    SUM=SUM+B((J*(J-1))/2+I)
  160 B((I*(I+1))/2)=SUM
      DO 170 I=1,21
  170 LINE(I)='------'
      LIMIT=(IPRT*(IPRT+1))/2
      KK=8
      NA=1
  180 LL=0
      M=MIN0((IPRT+1-NA),6)
      MA=2*M+1
      M=NA+M-1
      WRITE(6,'(/16X,10(1X,A7,3X))')(ITEXT(I),I=NA,M)
      WRITE(6,'(15X,10(2X,A2,I3,4X))')(JTEXT(I),NATOM(I),I=NA,M)
      WRITE (6,'(20A6)') (LINE(K),K=1,MA)
      DO 200 I=NA,IPRT
         LL=LL+1
         K=(I*(I-1))/2
         L=MIN0((K+M),(K+I))
         K=K+NA
         IF ((KK+LL).LE.50) GO TO 190
         WRITE (6,'(''1'')')
         WRITE(6,'(/17X,10(1X,A7,3X))')(ITEXT(N),N=NA,M)
         WRITE(6,'( 17X,10(2X,A2,I3,4X))')(JTEXT(N),NATOM(N),N=NA,M)
         WRITE (6,'(20A6)') (LINE(N),N=1,MA)
         KK=4
         LL=0
  190    WRITE (6,'(1X,A7,1X,A2,I3,10F11.6)')
     1   ITEXT(I),JTEXT(I),NATOM(I),(B(N),N=K,L)
  200 CONTINUE
      IF (L.GE.LIMIT) GO TO 210
      KK=KK+LL+4
      NA=M+1
      IF ((KK+IPRT+1-NA).LE.50) GO TO 180
      KK=4
      WRITE (6,'(''1'')')
      GO TO 180
  210 RETURN
      END
