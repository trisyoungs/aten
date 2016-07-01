      SUBROUTINE GEOUT(MODE1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
**********************************************************************
*
*   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME,
*         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING.
*
**********************************************************************
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,XPARAM(MAXPAR)
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /CORE  / CORE(107)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /ATOMTX/ LTXT, TXTATM(NUMATM)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON /ELEMTS/ ELEMNT(107)
      DIMENSION COORD(3,NUMATM), Q2(NUMATM), LOCTMP(2,MAXPAR)
      CHARACTER Q(3)*2, ELEMNT*2, FLAG1*2, FLAG0*2, FLAGN*2, TXTATM*8
      CHARACTER KEYWRD*241, KOMENT*81, TITLE*81, BLANK*80, LTXT*1
      LOGICAL CART
      MODE=MODE1
      IF(MODE.EQ.1)THEN
         FLAG1=' *'
         FLAG0='  '
         FLAGN=' +'
         IPRT=6
      ELSE
         FLAG1=' 1'
         FLAG0=' 0'
         FLAGN='-1'
         IPRT=ABS(MODE)
      ENDIF
C
C *** OUTPUT THE PARAMETER DATA.
C
      CART=.FALSE.
      IF(NA(1).NE.0) THEN
         CART=.TRUE.
         CALL XYZINT(GEO,NATOMS,NA,NB,NC,1.D0,COORD)
         LOCTMP(1,1)=2
         LOCTMP(2,1)=1
         LOCTMP(1,2)=3
         LOCTMP(2,2)=1
         LOCTMP(1,3)=3
         LOCTMP(2,3)=2
         NVARTM=0
         DO 10 I=4,NATOMS
            NVARTM=NVARTM+3
            DO 10 J=1,3
               LOCTMP(1,NVARTM+J)=I
   10    LOCTMP(2,NVARTM+J)=J
         NVARTM=NVARTM+3
      ELSE
         DO 20 I=1,NVAR
            LOCTMP(1,I)=LOC(1,I)
   20    LOCTMP(2,I)=LOC(2,I)
         NVARTM=NVAR
         DO 30 J=1,3
C$DOUT VBEST
            DO 30 I=1,NATOMS
   30    COORD(J,I)=GEO(J,I)
      ENDIF
      DEGREE=57.29577951D00
      MAXTXT=ICHAR(LTXT)
      BLANK=' '
      IF(MODE.EQ.1)THEN
         WRITE (6,40)BLANK(:MAX(2,MAXTXT-4)),
     1               BLANK(:MAX(4,MAXTXT-2)),
     2               BLANK(:MAX(18,MAXTXT+12))
   40    FORMAT (/4X,'ATOM',3X,'CHEMICAL',A ,'BOND LENGTH',4X,'BOND ANGL
     1E',4X ,' TWIST ANGLE',/3X,'NUMBER',2X,'SYMBOL', A,'(ANGSTROMS)',5
     2X,'(DEGREES)',5X,' (DEGREES)',/4X,'(I)',A,'NA:I',10X,'NB:NA:I',5
     3X,' NC:NB:NA:I',5X,'NA',3X,'NB',3X,'NC',/)
      ELSE
         IF(MODE.GT.0)CALL WRTTXT(IPRT)
      ENDIF
      IF(MODE.NE.1)THEN
         CALL CHRGE(P,Q2)
         DO 50 I=1,NUMAT
            L=NAT(I)
   50    Q2(I)=CORE(L) - Q2(I)
      ENDIF
      N=1
      IA=LOCTMP(1,1)
      II=0
      DO 80 I=1,NATOMS
         DO 60 J=1,3
            Q(J)=FLAG0
            IF (IA.NE.I) GO TO 60
            IF (J.NE.LOCTMP(2,N).OR.N.GT.NVARTM) GO TO 60
            Q(J)=FLAG1
            N=N+1
            IA=LOCTMP(1,N)
   60    CONTINUE
         W = COORD(2,I) * DEGREE
         X = COORD(3,I) * DEGREE
C
C  CONSTRAIN ANGLE TO DOMAIN 0 - 180 DEGREES
C
         W=W - AINT(W/360.D0)*360.D0
         IF(W.LT.0)W=W+360.D0
         IF(W .GT. 180.D0) THEN
            X=X+180.D0
            W=360.D0-W
         ENDIF
C
C  CONSTRAIN DIHEDRAL TO DOMAIN -180 - 180 DEGREES
C
         X=X - AINT(X/360.D0+SIGN(0.5D0-1.D-9,X)-1.D-9)*360.D0
         IF (LATOM.NE.I) GO TO 70
         J=LPARAM
         Q(J)=FLAGN
   70    CONTINUE
         BLANK=ELEMNT(LABELS(I))//TXTATM(I)//'  '
         IF(MODE.NE.1)THEN
            J=MAX(4,MAXTXT+2)
            K=MAX(0,8-J)
         ELSE
            J=MAX(9,MAXTXT+3)
         ENDIF
         IF(LABELS(I).NE.0)THEN
            IF(MODE.NE.1)THEN
               IF(LABELS(I).NE.99.AND.LABELS(I).NE.107)THEN
                  II=II+1
                  WRITE (IPRT,'(1X,A,F11.7,1X,A2,F14.6,1X,A2,F14.6,1X,
     1A2,3I5,A,F7.4)') BLANK(:J),COORD(1,I),Q(1),W,Q(2),X,Q(3),
     2NA(I),NB(I),NC(I),BLANK(20:20+K),Q2(II)
               ELSE
                  WRITE (IPRT,'(1X,A,F11.7,1X,A2,F14.6,1X,A2,F14.6,1X,
     1A2,3I5)') BLANK(:J),COORD(1,I),Q(1),W,Q(2),X,Q(3),
     2NA(I),NB(I),NC(I)
               ENDIF
            ELSEIF(I.GT.3)THEN
               WRITE (6,'(3X,I4 ,5X,A,F9.5,1X,A2,F14.5,1X,A2,F11.5,1X,
     1A2,I4,2I5)') I,BLANK(:J),COORD(1,I),Q(1),W,Q(2),X,Q(3),
     2NA(I),NB(I),NC(I)
            ELSEIF(I.EQ.3)THEN
               WRITE (6,'(''      3'',5X,A,F9.5,1X,A2,F14.5,1X,A2,13X,
     12I5)') BLANK(:J),COORD(1,3),Q(1),W,Q(2),NA(3),NB(3)
            ELSEIF(I.EQ.2)THEN
               WRITE (6,'(''      2'',5X,A,F9.5,1X,A2,30X,I5)')
     1 BLANK(:J),COORD(1,2),Q(1),NA(2)
            ELSE
               WRITE (6,'(''      1'',5X,A)') BLANK(:J)
            ENDIF
         ENDIF
   80 CONTINUE
      IF(CART) NA(1)=99
      IF(MODE.EQ.1) RETURN
      WRITE (IPRT,*)
      IF(NDEP.EQ.0) RETURN
C
C   OUTPUT SYMMETRY DATA.
C
      I=1
   90 J=I
  100 IF(J.EQ.NDEP) GOTO 110
      IF(LOCPAR(J).EQ.LOCPAR(J+1).AND.IDEPFN(J).EQ.IDEPFN(J+1)
     1.AND.J-I.LT.15)THEN
         J=J+1
         GOTO 100
      ELSE
         WRITE(IPRT,'(I4,I3,I5,15I4)')
     1LOCPAR(I),IDEPFN(I),(LOCDEP(K),K=I,J)
      ENDIF
      I=J+1
      GOTO 90
  110 CONTINUE
      WRITE(IPRT,'(I4,I3,I5,15I4)')
     1LOCPAR(I),IDEPFN(I),(LOCDEP(K),K=I,J)
      RETURN
      END
