      SUBROUTINE DRCOUT(XYZ3,GEO3,VEL3,NVAR,TIME,ESCF3,EKIN3,
     1GTOT3,ETOT3,XTOT3,ILOOP,CHARGE,FRACT,TEXT1,TEXT2,II,JLOOP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      CHARACTER TEXT1*3,TEXT2*2
      DIMENSION XYZ3(3,NVAR), VEL3(3,NVAR), ESCF3(3), EKIN3(3)
      DIMENSION XYZ(3,NUMATM), VEL(3,NUMATM), CHARGE(NUMATM),
     1 GEO3(3,NUMATM), ETOT3(3), GTOT3(3), XTOT3(3)
*************************************************************
*                                                           *
*    DRCOUT PRINTS THE GEOMETRY, ETC. FOR A DRC AT A        *
*    POSITION DETERMINED BY FRACT.                          *
*    ON INPUT XYZ3  = QUADRATIC EXPRESSION FOR THE GEOMETRY *
*             VEL3  = QUADRATIC EXPRESSION FOR THE VELOCITY *
*             ESCF3 = QUADRATIC EXPRESSION FOR THE P.E.     *
*             EKIN3 = QUADRATIC EXPRESSION FOR THE K.E.     *
*                                                           *
*************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /GEOVAR/ IIVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,XRACT
      COMMON /NUMCAL/ NUMCAL
      DIMENSION  IEL1(3), GG(3)
      CHARACTER KEYWRD*241,KOMENT*81,TITLE*81, ALPHA*2, ELEMNT*2
      SAVE PRTKEY, PRTKOM, PRTITL, I, DRC
      INTEGER PRTKOM, PRTITL, PRTKEY
      LOGICAL LARGE, DRC
      DATA ICALCN/0/
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         IF( INDEX(KEYWRD,'RESTART').EQ.0.OR.INDEX(KEYWRD,'IRC=').NE.0)
     1THEN
            JLOOP=0
         ENDIF
         DO 10 I=80,1,-1
   10    IF(KEYWRD(I:I).NE.' ')GOTO 20
         I=1
   20    PRTKEY=I
         DO 30 I=80,1,-1
   30    IF(KOMENT(I:I).NE.' ')GOTO 40
         I=1
   40    PRTKOM=I
         DO 50 I=80,1,-1
   50    IF(TITLE(I:I).NE.' ')GOTO 60
         I=1
   60    PRTITL=I
         DRC=(INDEX(KEYWRD,'DRC').NE.0)
         I=INDEX(KEYWRD,'LARGE')
         IPRINT=10000
         IF(I.NE.0)THEN
            IPRINT=1
            I=I+5
            LARGE=(KEYWRD(I:I).EQ.' '.OR.KEYWRD(I+1:I+1).EQ.'-')
            IF(KEYWRD(I:I).EQ.'=') IPRINT=ABS(READA(KEYWRD,I))
         ENDIF
      ENDIF
      IF(JLOOP.EQ.0.OR.(JLOOP/IPRINT)*IPRINT.EQ.JLOOP) THEN
         IF(DRC) THEN
            WRITE(6,'(//,'' FEMTOSECONDS  POINT  POTENTIAL +''
     1,'' KINETIC  =  TOTAL     ERROR    REF%   MOVEMENT'')')
         ELSE
            WRITE(6,'(//,''     POINT   POTENTIAL  +  ''
     1,''ENERGY LOST   =   TOTAL      ERROR    REF%   MOVEMENT'')')
         ENDIF
      ENDIF
      JLOOP=JLOOP+1
C#      FRACT=0.D0
      ESCF=ESCF3(1)+ESCF3(2)*FRACT+ESCF3(3)*FRACT**2
      EKIN=EKIN3(1)+EKIN3(2)*FRACT+EKIN3(3)*FRACT**2
      ETOT=ETOT3(1)+ETOT3(2)*FRACT+ETOT3(3)*FRACT**2
C#      GTOT=GTOT3(1)+GTOT3(2)*FRACT+GTOT3(3)*FRACT**2
      XTOT=XTOT3(1)+XTOT3(2)*FRACT+XTOT3(3)*FRACT**2
      ERRR=MIN(9999.99999D0,MAX(-999.99999D0,ESCF+EKIN-ETOT))
      IF(II.NE.0)THEN
         IF(DRC) THEN
            WRITE(6,'(F10.3,I8,F12.5,F11.5,F11.5,
     1F10.5,'' '',I5,3X,''%'',A,A,I3)')TIME, ILOOP-2, ESCF, EKIN,
     2 ESCF+EKIN,ERRR,JLOOP,TEXT1,TEXT2,II
            WRITE(6,'(9X,A,F9.4,A)')' MOVEMENT FROM START =',XTOT,' ANGS
     1TROMS'
         ELSE
            WRITE(6,'(I8,F14.5,F13.5,F17.5,
     1F10.5,'' '',I5,3X,''%'',A,A,I3)') ILOOP-2, ESCF, EKIN,
     2 ESCF+EKIN,ERRR,JLOOP,TEXT1,TEXT2,II
C#      WRITE(6,'(24X,'' INTEGRATED GRADIENT ERROR ='',G10.3,
C#     1'' KCALS/ANGSTROM'')')GTOT
            WRITE(6,'(9X,A,F9.4,A)')' MOVEMENT FROM START =',XTOT,' ANGS
     1TROMS'
         ENDIF
      ELSE
         IF(DRC) THEN
            IF(TEXT1.EQ.' '.AND.TEXT2.EQ.' ')THEN
               WRITE(6,'(F10.3,I8,F12.5,F11.5,F11.5,
     1F10.5,'' '',I5,3X,''%'',F8.4)')TIME, ILOOP-2, ESCF, EKIN,
     2 ESCF+EKIN,ERRR,JLOOP,XTOT
            ELSE
               WRITE(6,'(F10.3,I8,F12.5,F11.5,F11.5,
     1F10.5,'' '',I5,3X,''%'',A,A,I3)')TIME, ILOOP-2, ESCF, EKIN,
     2 ESCF+EKIN,ERRR,JLOOP,TEXT1,TEXT2
            ENDIF
         ELSE
            IF(TEXT1.EQ.' '.AND.TEXT2.EQ.' ')THEN
               WRITE(6,'(I8,F14.5,F13.5,F17.5,
     1F10.5,'' '',I5,3X,''%'',F8.4)') ILOOP-2, ESCF, EKIN,
     2 ESCF+EKIN,ERRR,JLOOP,XTOT
            ELSE
               WRITE(6,'(I8,F14.5,F13.5,F17.5,
     1F10.5,'' '',I5,3X,''%'',A,A,I3)') ILOOP-2, ESCF, EKIN,
     2 ESCF+EKIN,ERRR,JLOOP,TEXT1,TEXT2
            ENDIF
         ENDIF
      ENDIF
      NATOMS=NVAR/3
      L=0
      DO 80 I=1,NATOMS
         DO 70 J=1,3
            L=L+1
            VEL(J,I)=VEL3(1,L)+VEL3(2,L)*FRACT+VEL3(3,L)*FRACT**2
   70    XYZ(J,I)=XYZ3(1,L)+XYZ3(2,L)*FRACT+XYZ3(3,L)*FRACT**2
   80 CONTINUE
      IF(LARGE.AND.(JLOOP/IPRINT)*IPRINT.EQ.JLOOP)THEN
         WRITE(6,*)'                CARTESIAN GEOMETRY           '
     1//'VELOCITY (IN CM/SEC)'
         WRITE(6,*)'  ATOM        X          Y          Z'
     1      //'                X          Y          Z'
         DO 90 I=1,NUMAT
            LL=(I-1)*3+1
            WRITE(6,'(I4,3X,A2,3F11.5,2X,3F11.1)')
     1I, ELEMNT(NAT(I)),(XYZ(J,I),J=1,3),(VEL(J,I),J=1,3)
   90    CONTINUE
      ENDIF
      IF((JLOOP/IPRINT)*IPRINT.EQ.JLOOP)THEN
         IVAR=1
         NA(1)=0
         L=0
         WRITE(6,'(//10X,''FINAL GEOMETRY OBTAINED'',33X,''CHARGE'')'
     1)
         WRITE(6,'(A)')KEYWRD(:PRTKEY),KOMENT(:PRTKOM),TITLE(:PRTITL)
         L=0
         DO 120 I=1,NUMAT
            J=I/26
            ALPHA(1:1)=CHAR(ICHAR('A')+J)
            J=I-J*26
            ALPHA(2:2)=CHAR(ICHAR('A')+J-1)
C$DOIT ASIS
            DO 100 J=1,3
  100       IEL1(J)=0
  110       CONTINUE
            IF(LOC(1,IVAR).EQ.I) THEN
               IEL1(LOC(2,IVAR))=1
               IVAR=IVAR+1
               GOTO 110
            ENDIF
            IF(I.LT.4) THEN
               IEL1(3)=0
               IF(I.LT.3) THEN
                  IEL1(2)=0
                  IF(I.LT.2) THEN
                     IEL1(1)=0
                  ENDIF
               ENDIF
            ENDIF
            IF(LABELS(I).LT.99)THEN
               L=L+1
               GG(1)=GEO3(1,I*3-2)+GEO3(2,I*3-2)*FRACT+GEO3(3,I*3-2)*FRA
     1CT**2
               GG(2)=GEO3(1,I*3-1)+GEO3(2,I*3-1)*FRACT+GEO3(3,I*3-1)*FRA
     1CT**2
               GG(3)=GEO3(1,I*3  )+GEO3(2,I*3  )*FRACT+GEO3(3,I*3  )*FRA
     1CT**2
               WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3,F13.4,I5,A)')
     1    ELEMNT(LABELS(I)),(GG(K),IEL1(K),K=1,3),
     2    NA(I),NB(I),NC(I),CHARGE(L),JLOOP,ALPHA//'*'
            ELSE
               WRITE(6,'(2X,A2,3(F12.6,I3),I4,2I3,13X,I5,A)')
     1    ELEMNT(LABELS(I)),(GG(K),IEL1(K),K=1,3),
     2NA(I),NB(I),NC(I),JLOOP,ALPHA//'%'
            ENDIF
  120    CONTINUE
         NA(1)=99
      ENDIF
      RETURN
      END
