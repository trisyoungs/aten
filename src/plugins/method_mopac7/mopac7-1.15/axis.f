      SUBROUTINE AXIS(COORD,NUMAT,A,B,C,SUMW, MASS,EVEC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION COORD(3,NUMAT)
      COMMON /KEYWRD/ KEYWRD
************************************************************************
*
*  AXIS CALCULATES THE THREE MOMENTS OF INERTIA AND THE MOLECULAR
*       WEIGHT.  THE MOMENTS OF INERTIA ARE RETURNED IN A, B, AND C.
*       THE MOLECULAR WEIGHT IN SUMW.
*       THE UNITS OF INERTIA ARE 10**(-40)GRAM-CM**2,
*       AND MOL.WEIGHT IN ATOMIC-MASS-UNITS. (AMU'S)
************************************************************************
      COMMON /NUMCAL/ NUMCAL
      COMMON /ATMASS/ ATMASS(NUMATM)
      DIMENSION T(6), X(NUMATM), Y(NUMATM),
     1          Z(NUMATM), ROT(3), XYZMOM(3), EIG(3), EVEC(3,3)
      LOGICAL FIRST
      CHARACTER*241 KEYWRD
      SAVE ICALCN, T, FIRST, EIG, ROT, XYZMOM
      DATA T /6*0.D0/
      DATA ICALCN /0/
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         FIRST=.TRUE.
      ENDIF
************************************************************************
*     CONST1 =  10**40/(N*A*A)
*               N = AVERGADRO'S NUMBER
*               A = CM IN AN ANGSTROM
*               10**40 IS TO ALLOW UNITS TO BE 10**(-40)GRAM-CM**2
*
************************************************************************
      CONST1 = 1.66053D0
************************************************************************
*
*     CONST2 = CONVERSION FACTOR FROM ANGSTROM-AMU TO CM**(-1)
*
*            = (PLANCK'S CONSTANT*N*10**16)/(8*PI*PI*C)
*            = 6.62618*10**(-27)[ERG-SEC]*6.02205*10**23*10**16/
*              (8*(3.1415926535)**2*2.997925*10**10[CM/SEC])
*
************************************************************************
      CONST2=16.8576522D0
C    FIRST WE CENTRE THE MOLECULE ABOUT THE CENTRE OF GRAVITY,
C    THIS DEPENDS ON THE ISOTOPIC MASSES, AND THE CARTESIAN GEOMETRY.
C
      SUMW=1.D-20
      SUMWX=0.D0
      SUMWY=0.D0
      SUMWZ=0.D0
C
      IF(MASS.GT.0) THEN
         DO 10 I=1,NUMAT
            SUMW=SUMW+ATMASS(I)
            SUMWX=SUMWX+ATMASS(I)*COORD(1,I)
            SUMWY=SUMWY+ATMASS(I)*COORD(2,I)
            SUMWZ=SUMWZ+ATMASS(I)*COORD(3,I)
   10    CONTINUE
      ELSE
         SUMW=SUMW+DBLE(NUMAT)
         DO 20 I=1,NUMAT
            SUMWX=SUMWX+COORD(1,I)
            SUMWY=SUMWY+COORD(2,I)
            SUMWZ=SUMWZ+COORD(3,I)
   20    CONTINUE
      ENDIF
C
      IF(MASS.GT.0.AND.FIRST)
     1 WRITE(6,'(/10X,''MOLECULAR WEIGHT ='',F8.2,/)')
     2MIN(99999.99D0,SUMW)
      SUMWX=SUMWX/SUMW
      SUMWY=SUMWY/SUMW
      SUMWZ=SUMWZ/SUMW
      DO 30 I=1,NUMAT
         X(I)=COORD(1,I)-SUMWX
         Y(I)=COORD(2,I)-SUMWY
   30 Z(I)=COORD(3,I)-SUMWZ
************************************************************************
*
*    MATRIX FOR MOMENTS OF INERTIA IS OF FORM
*
*           |   Y**2+Z**2                         |
*           |    -Y*X       Z**2+X**2             | -I =0
*           |    -Z*X        -Z*Y       X**2+Y**2 |
*
************************************************************************
C
C$DOIT ASIS
      DO 40 I=1,6
   40 T(I)=DBLE(I)*1.0D-10
C
      IF(MASS.GT.0) THEN
         DO 50 I=1,NUMAT
            T(1)=T(1)+ATMASS(I)*(Y(I)**2+Z(I)**2)
            T(2)=T(2)-ATMASS(I)*X(I)*Y(I)
            T(3)=T(3)+ATMASS(I)*(Z(I)**2+X(I)**2)
            T(4)=T(4)-ATMASS(I)*Z(I)*X(I)
            T(5)=T(5)-ATMASS(I)*Y(I)*Z(I)
            T(6)=T(6)+ATMASS(I)*(X(I)**2+Y(I)**2)
   50    CONTINUE
      ELSE
         DO 60 I=1,NUMAT
            T(1)=T(1)+(Y(I)**2+Z(I)**2)
            T(2)=T(2)-X(I)*Y(I)
            T(3)=T(3)+(Z(I)**2+X(I)**2)
            T(4)=T(4)-Z(I)*X(I)
            T(5)=T(5)-Y(I)*Z(I)
            T(6)=T(6)+(X(I)**2+Y(I)**2)
   60    CONTINUE
      ENDIF
C
      CALL RSP(T,3,3,EIG,EVEC)
      IF(MASS.GT.0.AND. FIRST.AND.INDEX(KEYWRD,'RC=').EQ.0) THEN
         WRITE(6,'(//10X,'' PRINCIPAL MOMENTS OF INERTIA IN CM(-1)'',/)'
     1)
C$DOIT ASIS
         DO 70 I=1,3
            IF(EIG(I).LT.3.D-4) THEN
               EIG(I)=0.D0
               ROT(I)=0.D0
            ELSE
               ROT(I)=CONST2/EIG(I)
            ENDIF
   70    XYZMOM(I)=EIG(I)*CONST1
         WRITE(6,'(10X,''A ='',F12.6,''   B ='',F12.6,
     1''   C ='',F12.6,/)')(ROT(I),I=1,3)
         IF(INDEX(KEYWRD,'RC=').EQ.0)
     1WRITE(6,'(//10X,'' PRINCIPAL MOMENTS OF INERTIA IN '',
     2''UNITS OF 10**(-40)*GRAM-CM**2'',/)')
         WRITE(6,'(10X,''A ='',F12.6,''   B ='',F12.6,
     1''   C ='',F12.6,/)')(XYZMOM(I),I=1,3)
         C=ROT(1)
         B=ROT(2)
         A=ROT(3)
      ENDIF
C
C   NOW TO ORIENT THE MOLECULE SO THE CHIRALITY IS PRESERVED
C
      SUM=EVEC(1,1)*(EVEC(2,2)*EVEC(3,3)-EVEC(3,2)*EVEC(2,3)) +
     1    EVEC(1,2)*(EVEC(2,3)*EVEC(3,1)-EVEC(2,1)*EVEC(3,3)) +
     2    EVEC(1,3)*(EVEC(2,1)*EVEC(3,2)-EVEC(2,2)*EVEC(3,1))
      IF( SUM .LT. 0) THEN
C$DOIT ASIS
         DO 80 J=1,3
   80    EVEC(J,1)=-EVEC(J,1)
      ENDIF
      DO 90 I=1,NUMAT
         COORD(1,I)=X(I)
         COORD(2,I)=Y(I)
         COORD(3,I)=Z(I)
   90 CONTINUE
      IF(MASS.GT.0)FIRST=.FALSE.
      END
