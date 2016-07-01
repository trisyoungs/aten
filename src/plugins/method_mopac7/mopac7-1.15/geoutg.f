      SUBROUTINE GEOUTG(IPRT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,DUMY(MAXPAR)
      COMMON /SIMBOL/ SIMBOL(MAXPAR)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /ATOMTX/ LTXT, TXTATM(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
************************************************************************
*
*   GEOUTG WRITES OUT THE GEOMETRY IN GAUSSIAN-8X STYLE
*
************************************************************************
      DIMENSION IGEO(3,NUMATM)
      CHARACTER LINE(3,NUMATM)*15, TYPE(3)*1, OPTDAT(MAXPAR)*14
      CHARACTER TXTATM*8, BLANK*80, ELEMNT(107)*2, SIMBOL*10, LTXT*1
      DATA ELEMNT/' H','He',
     1 'Li','Be',' B',' C',' N',' O',' F','Ne',
     2 'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     3 ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu',
     4 'Zn','Ga','Ge','As','Se','Br','Kr',
     5 'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
     6 'Cd','In','Sn','Sb','Te',' I','Xe',
     7 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
     8 'Ho','Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt',
     9 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
     1 'Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','XX',
     2 'Fm','Md','Cb','++',' +','--',' -','Tv'/
      DATA TYPE/'r','a','d'/
      DO 10 I=1,NATOMS
         DO 10 J=1,3
   10 IGEO(J,I)=-1
      DO 20 I=1,NVAR
   20 IGEO(LOC(2,I),LOC(1,I))=-2
      DO 30 I=1,NDEP
         IF(IDEPFN(I).EQ.14)THEN
            IGEO(3,LOCDEP(I))=-LOCPAR(I)
         ELSE
            IF(IDEPFN(I).GT.3) GOTO 30
            IGEO(IDEPFN(I),LOCDEP(I))=LOCPAR(I)
         ENDIF
   30 CONTINUE
      OPEN(UNIT=21,STATUS='SCRATCH')
      DEGREE=90.D0/ASIN(1.D0)
      MAXTXT=ICHAR(LTXT)
      NOPT=0
      DO 50 I=1,NATOMS
         DO 40 J=1,3
            LINE(J,I)=' '
            IF(IGEO(J,I).EQ.-1)THEN
               REWIND 21
               IF(J.NE.1)THEN
                  WRITE(21,'(F12.6)')GEO(J,I)*DEGREE
               ELSE
                  WRITE(21,'(F12.6)')GEO(J,I)
               ENDIF
               REWIND 21
               READ(21,'(A)')LINE(J,I)
            ELSEIF(IGEO(J,I).EQ.-2)THEN
               NOPT=NOPT+1
               IF(SIMBOL(NOPT).NE.'---')THEN
               IF(SIMBOL(NOPT)(1:1).EQ.'-') THEN
                 LINE(J,I)(4:)=SIMBOL(NOPT)(2:)
               ELSE
                 LINE(J,I)(4:)=SIMBOL(NOPT)
               ENDIF
               ELSE
                  NBI=NB(I)
                  NCI=NC(I)
                  IF(J.NE.3)NCI=0
                  IF(J.EQ.1)NBI=0
                  CALL XXX(TYPE(J),I,NA(I),NBI,NCI,LINE(J,I)(4:))
               ENDIF
               OPTDAT(NOPT)=LINE(J,I)
            ELSEIF(IGEO(J,I).LT.0)THEN
               LINE(3,I)=LINE(3,-IGEO(J,I))
               LINE(3,I)(3:3)='-'
            ELSE
               LINE(J,I)=LINE(J,IGEO(J,I))
            ENDIF
   40    CONTINUE
         BLANK=ELEMNT(LABELS(I))//TXTATM(I)//'  '
         IF(LABELS(I).EQ.99)BLANK(1:1)=' '
         J=MAX(4,MAXTXT+2)
         IF(I.EQ.1)THEN
            WRITE(IPRT,'(1X,A,I4,A,I4,A,I4,A,I4)')BLANK(:J)
         ELSEIF(I.EQ.2)THEN
            WRITE(IPRT,'(1X,A,I4,A,I4,A,I4,A,I4)')BLANK(:J),NA(I),LINE(1
     1,I)
         ELSEIF(I.EQ.3)THEN
            WRITE(IPRT,'(1X,A,I4,A,I4,A,I4,A,I4)')BLANK(:J),
     1NA(I),LINE(1,I),NB(I), LINE(2,I)
         ELSE
            L=0
            WRITE(IPRT,'(1X,A,I4,A,I4,A,I4,A,I4)')BLANK(:J),
     1NA(I),LINE(1,I),NB(I), LINE(2,I), NC(I), LINE(3,I), L
         ENDIF
   50 CONTINUE
      WRITE(IPRT,*)
      DO 70 L=1,3
         DO 60 I=1,NOPT
            IF(LOC(2,I).EQ.L)THEN
               IF(LOC(2,I).NE.1)THEN
                  WRITE(IPRT,'(A,F12.6)')OPTDAT(I),GEO(LOC(2,I),LOC(1,I)
     1)*DEGREE
               ELSE
                  WRITE(IPRT,'(A,F12.6)')OPTDAT(I),GEO(LOC(2,I),LOC(1,I)
     1)
               ENDIF
            ENDIF
   60    CONTINUE
   70 CONTINUE
      END
      SUBROUTINE XXX(TYPE,I,J,K,L,R)
      CHARACTER TYPE*1, R*(*)
************************************************************************
*
*    XXX WILL FORM A UNIQUE STRING LABEL 'R' FOR GAUSSIAN-TYPE INPUT
*    THE LABEL WILL BE LETTER (EITHER R, P, OR F, NORMALLY), FOLLOWED
*    BY THE CONNECTIVITY, IN THE ORDER I, J, K, L.
*    'R' IS 13 CHARACTERS LONG IN ORDER TO ACCOMMODATE 3 DIGITS PER
*    LABEL, WHEN NECESSARY
*
************************************************************************
      DIMENSION IJK(4)
      R=TYPE
      IJK(1)=I
      IJK(2)=J
      IJK(3)=K
      IJK(4)=L
      M=1
      DO 10 LOOP=1,4
         II=IJK(LOOP)
         IF(II.EQ.0) GOTO 10
C
C   IF LABELS GREATER THAN 99 ARE USED, UNCOMMENT THE FOLLOWING CODE
C
C#         I2=II/100
C#         IF(I2.NE.0) THEN
C#            M=M+1
C#            R(M:M)=CHAR(ICHAR('0')+I2)
C#            II=II-I2*100
C#         ENDIF
         I2=II/10
         IF(I2.NE.0) THEN
            M=M+1
            R(M:M)=CHAR(ICHAR('0')+I2)
            II=II-I2*10
         ENDIF
         M=M+1
         R(M:M)=CHAR(ICHAR('0')+II)
   10 CONTINUE
      RETURN
      END

