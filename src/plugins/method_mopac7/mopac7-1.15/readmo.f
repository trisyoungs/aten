      LOGICAL FUNCTION READMO(DATAFILE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
C
C MODULE TO READ IN GEOMETRY FILE, OUTPUT IT TO THE USER,
C AND CHECK THE DATA TO SEE IF IT IS REASONABLE.
C EXIT IF NECESSARY.
C
C MODIFIED 07/2016 BY TGAY TO READ FROM INTERNAL FILE SOURCE
C CONVERTED 07/2016 BY TGAY FROM SUBROUTINE TO LOGICAL FUNCTION
C
C  ON EXIT NATOMS    = NUMBER OF ATOMS PLUS DUMMY ATOMS (IF ANY).
C          KEYWRD    = KEYWORDS TO CONTROL CALCULATION
C          KOMENT    = COMMENT CARD
C          TITLE     = TITLE CARD
C          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS.
C          GEO       = ARRAY OF INTERNAL COORDINATES.
C          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
C          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
C          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
C          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
C          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
C          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
C          REACT(200)= REACTION COORDINATE PARAMETERS
C          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMIZED.
C          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
C          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMIZED.
C          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMIZED.
C
************************************************************************
C *** INPUT THE TRIAL GEOMETRY  \IE.  KGEOM=0\
C   LABEL(I) = THE ATOMIC NUMBER OF ATOM\I\.
C            = 99, THEN THE I-TH ATOM IS A DUMMY ATOM USED ONLY TO
C              SIMPLIFY THE DEFINITION OF THE MOLECULAR GEOMETRY.
C   GEO(1,I) = THE INTERNUCLEAR SEPARATION \IN ANGSTROMS\ BETWEEN ATOMS
C              NA(I) AND (I).
C   GEO(2,I) = THE ANGLE NB(I):NA(I):(I) INPUT IN DEGREES; STORED IN
C              RADIANS.
C   GEO(3,I) = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND NA(I):(I)
C              INPUT IN DEGREES - STORED IN RADIANS.
C  LOPT(J,I) = -1 IF GEO(J,I) IS THE REACTION COORDINATE.
C            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMIZED
C            =  0 OTHERWISE.
C *** NOTE:    MUCH OF THIS DATA IS NOT INCLUDED FOR THE FIRST 3 ATOMS.
C     ATOM1  INPUT LABELS(1) ONLY.
C     ATOM2  INPUT LABELS(2) AND GEO(1,2) SEPARATION BETWEEN ATOMS 1+2
C     ATOM3  INPUT LABELS(3), GEO(1,3)    SEPARATION BETWEEN ATOMS 2+3
C              AND GEO(2,3)              ANGLE ATOM1 : ATOM2 : ATOM3
C
************************************************************************
C
      DIMENSION LOPT(3,NUMATM)
      CHARACTER KEYWRD*241, KOMENT*81, TITLE*81, LINE*80, BANNER*80
      CHARACTER KEYS(80)*1, SPACE*1, SPACE2*2, CH*1, CH2*2
      CHARACTER ELEMNT*2, IDATE*30, GETNAM*80, NAME*4, NAMO*4, ISTA*4
      CHARACTER mydate*8, mytime*10
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     * NMIDLE(NUMATM),NLAST(NUMATM),NORBS,NELECS,NALPHA,NBETA,
     * NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /OKMANY/ ISOK
      PARAMETER (MXDIM=MAXPAR+NUMATM)
      COMMON /SYMRES/ TRANS,RTR,SIG,NAME,NAMO(MXDIM),INDX(MXDIM),
     * ISTA(2)
      COMMON /ISTOPE/ AMS(107)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /NUMCAL/ NUMCAL
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                      LOCDEP(MAXPAR)
      COMMON /OUTFIL/ WU
      INTEGER WU
      LOGICAL INT, AIGEO, ISOK
      SAVE SPACE, SPACE2, IREACT, INT
      DIMENSION COORD(3,NUMATM),VALUE(40),C(1)
      EQUIVALENCE (KEYS(1),KEYWRD)
      DATA SPACE, SPACE2/' ','  '/
c ******* ADDED BY TGAY 07/2016
      COMMON /DATFIL/ IREC,NUMREC
      CHARACTER DATAFILE(NUMREC)*80
      LOGICAL GETTXT
c *******

      CONVTR=2.D0*ASIN(1.D0)/180.D0
      AIGEO=.FALSE.
      READMO=.TRUE.
   10 CONTINUE
C
      IF (.NOT.GETTXT(DATAFILE)) THEN
        READMO=.FALSE.
        RETURN
      ENDIF
!       IF(INDEX(KEYWRD,'ECHO').NE.0)THEN
!          IREC=0
!          ISOK=.FALSE.
!          DO 50 I=1,1000
!             READ(5,'(A)',END=60)KEYWRD
!             DO 20 J=80,2,-1
!    20       IF(KEYWRD(J:J).NE.' ')GOTO 30
!             J=1
!    30       DO 40 K=1,J
!    40       IF(ICHAR(KEYWRD(K:K)).LT.32)KEYWRD(K:K)='*'
!             WRITE(WU,'(1X,A)')KEYWRD(1:J)
!    50    CONTINUE
!    60    CONTINUE
!          REWIND 5
!         IF (.NOT.GETTXT(DATAFILE)) THEN
!           READMO=.FALSE.
!           RETURN
!         ENDIF
!       ENDIF
      write(0,*) "NOW HERE1"
      IF(INDEX(KEYWRD,'ECHO').NE.0)WRITE(WU,'(''1'')')
      IF(KEYWRD(1:1) .NE. SPACE) THEN
         CH=KEYWRD(1:1)
         KEYWRD(1:1)=SPACE
         DO 70 I=2,239
            CH2=KEYWRD(I:I)
            KEYWRD(I:I)=CH
            CH=CH2
            IF(KEYWRD(I+1:I+2) .EQ. SPACE2) THEN
               KEYWRD(I+1:I+1)=CH
               GOTO 80
            ENDIF
   70    CONTINUE
         CH2=KEYWRD(240:240)
         KEYWRD(240:240)=CH
         KEYWRD(241:241)=CH2
   80    CONTINUE
      ENDIF
      write(0,*) "NOW HERE2"
      IF(KOMENT(1:1) .NE. SPACE) THEN
         CH=KOMENT(1:1)
         KOMENT(1:1)=SPACE
         DO 90 I=2,79
            CH2=KOMENT(I:I)
            KOMENT(I:I)=CH
            CH=CH2
            IF(KOMENT(I+1:I+2) .EQ. SPACE2) THEN
               KOMENT(I+1:I+1)=CH
               GOTO 100
            ENDIF
   90    CONTINUE
         CH2=KOMENT(80:80)
         KOMENT(80:80)=CH
         KOMENT(81:81)=CH2
  100    CONTINUE
      ENDIF
      write(0,*) "NOW HERE3"

      IF(TITLE(1:1) .NE. SPACE) THEN
         CH=TITLE(1:1)
         TITLE(1:1)=SPACE
         DO 110 I=2,79
            CH2=TITLE(I:I)
            TITLE(I:I)=CH
            CH=CH2
            IF(TITLE(I+1:I+2) .EQ. SPACE2) THEN
               TITLE(I+1:I+1)=CH
               GOTO 120
            ENDIF
  110    CONTINUE
         CH2=TITLE(80:80)
         TITLE(80:80)=CH
         TITLE(81:81)=CH2
  120    CONTINUE
      ENDIF
      write(0,*) "NOW HERE4"

      DO 121 I=1,200
  121 REACT(I)=0.D0
      LATOM=0
      LPARAM=0
      IF(INDEX(KEYWRD,'OLDGEO').EQ.0) THEN
         NVAR=0
         NDEP=0
         IF(AIGEO.OR.INDEX(KEYWRD,'AIGIN').NE.0)THEN
         write(0,*) "READING GEO1"
            CALL GETGEG(DATAFILE,LABELS,GEO,NA,NB,NC,AMS,NATOMS,
     1INT)
         IF(NVAR.EQ.0)THEN
         DO 122 J=1,3
         DO 122 I=1,NATOMS
  122    LOPT(J,I)=0
         ENDIF
         ELSE
         write(0,*) "READING GEO2"
            CALL GETGEO(DATAFILE,LABELS,GEO,LOPT,NA,NB,NC,AMS,
     1NATOMS,INT)
            IF(NATOMS.LT.0)THEN
               REWIND 5
               IF(NUMCAL.NE.1)THEN
                  WRITE(WU,'(//,A)')'   GAUSSIAN INPUT REQUIRES STAND-ALO
     1NE JOB'
                  WRITE(WU,'(/,A)')'   OR KEYWORD "AIGIN"'
                  READMO = .FALSE.
                  RETURN
               ENDIF
               AIGEO=.TRUE.
               GOTO 10
            ENDIF
         ENDIF
         IF(NATOMS.EQ.0) THEN
           READMO = .FALSE.
           RETURN
         ENDIF
      ELSE
      write(0,*) "NOW HERE5"
      DEGREE=90.D0/ASIN(1.D0)
      IF(NA(1).EQ.99)THEN
      DO 128 I=1,NATOMS
      DO 128 J=1,3
      LOPT(J,I)=1
  128 COORD(J,I)=GEO(J,I)
      LOPT(1,1)=0
      LOPT(2,1)=0
      LOPT(3,1)=0
      LOPT(2,2)=0
      LOPT(3,2)=0
      LOPT(3,3)=0
      CALL XYZINT(COORD,NATOMS,NA,NB,NC,DEGREE,GEO)
      ELSE 
      DO 130 I=1,NATOMS
      DO 130 J=2,3
 130  GEO(J,I)=GEO(J,I)*DEGREE
      ENDIF
      ENDIF
      IF(INDEX(KEYWRD,'FORCE').NE.0 .AND. LABELS(NATOMS).EQ.107) THEN
      DO 131 I=1,NA(NATOMS)
      IF(LABELS(I).EQ.99)THEN
      WRITE(WU,'(A)')' NO DUMMY ATOMS ALLOWED BEFORE TRANSLATION'
      WRITE(WU,'(A)')' ATOM IN A FORCE CALCULATION'
      READMO = .FALSE.
      RETURN
      ENDIF
  131 CONTINUE
      ENDIF
C
C
C OUTPUT FILE TO UNIT 6
C
C    WRITE HEADER
      IDATE=' '
!      CALL fdate(IDATE)
      call DATE_AND_TIME(mydate,mytime)
      write(idate,'("at: ",a,":",a,":",a," on ",a,"-",a,"-",a)') 
     +mytime(1:2),mytime(3:4),mytime(5:10)
     +,mydate(7:8),mydate(5:6),mydate(1:4)
      WRITE(WU,'(1X,15(''*****''),''****'')')
C
C     CHANGE THE FOLLOWING LINE TO SUIT LOCAL ENVIRONMENT, IF DESIRED
C
      BANNER=' **  OpenMOPAC v7.15 (PUBLIC DOMAIN VERSION) '//
     1' PLUGIN FOR ATEN, TGAY, 07-2016  **'
      WRITE(WU,'(A)')BANNER
C
C    THE BANNER DOES NOT APPEAR ANYWHERE ELSE.
C
      WRITE(WU,'(1X,79(''*''))')
      LINE='   MNDO'
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) LINE='MINDO/3'
      IF(INDEX(KEYWRD,'AM1')   .NE. 0) LINE='    AM1'
      IF(INDEX(KEYWRD,'PM3')   .NE. 0) LINE='    PM3'
      WRITE(WU,'(/29X,A,'' CALCULATION RESULTS'',28X,///1X,15(''*****'')
     1,''****'' )')LINE(:7)
      WRITE(WU,'('' *'',6X,''MOPAC:  VERSION '',F5.2,
     113X,''CALC''''D. '',A)') VERSON, IDATE
C
C CONVERT ANGLES TO RADIANS
      DO 140 J=2,3
C$DOIT VBEST
         DO 140 I=1,NATOMS
            GEO(J,I) = GEO(J,I) * CONVTR
  140 CONTINUE
C
C CHECK DATA
C
      NA(1)=0
      NB(1)=0
      NC(1)=0
      DO 150 I=1,NATOMS
         IF (LABELS(I) .LE. 0 ) THEN
            WRITE(WU,'('' ATOMIC NUMBER OF '',I3,'' ?'')') LABELS(I)
            IF(I.EQ.1) THEN
               WRITE(WU,'(A)')' THIS WAS THE FIRST ATOM'
            ELSE
               WRITE(WU,'(A)')'    GEOMETRY UP TO, BUT NOT INCLUDING, THE
     1'//' FAULTY ATOM'
               NATOMS=I-1
               CALL GEOUT(6)
            ENDIF
            READMO = .FALSE.
            RETURN
         ENDIF
         IF (  NA(I).GE.I.OR. NB(I).GE.I.OR. NC(I).GE.I
     1  .OR. (NA(I).EQ.NB(I))   .AND. I.GT.1
     2  .OR. (NA(I).EQ.NC(I).OR.NB(I).EQ.NC(I))  .AND. I.GT.2
     3  .OR.  NA(I)*NB(I)*NC(I).EQ.0  .AND. I.GT.3) THEN
            WRITE(WU,'('' ATOM NUMBER '',I3,'' IS ILL-DEFINED'')') I
            IF(I.EQ.1) THEN
              READMO = .FALSE.
              RETURN
            ENDIF
            WRITE(WU,'(/,''  GEOMETRY READ IN'',/)')
            CALL GEOUT(6)
            READMO = .FALSE.
            RETURN
         ENDIF
  150 CONTINUE
C
C WRITE KEYWORDS BACK TO USER AS FEEDBACK
      CALL WRTKEY(KEYWRD)
      WRITE(WU,'(1X,14(''*****''),''*'',I3.3,''BY'',I3.3)')MAXHEV,MAXLIT
C
C FILL IN GEO MATRIX IF NEEDED
      IF(INDEX(KEYWRD,'OLDGEO').EQ.0.AND.INDEX(KEYWRD,'SYM') .NE. 0
     1.AND. NDEP.EQ.0) CALL GETSYM
      IF(NDEP.NE.0) CALL SYMTRY
C
C INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      IFLAG = 0
      LATOM = 0
      NUMAT=0
      IF(NVAR.NE.0)THEN
         NUMAT=NATOMS
      ELSE
         DO 180 I=1,NATOMS
            IF(LABELS(I).NE.99.AND.LABELS(I).NE.107)NUMAT=NUMAT+1
            DO 180 J=1,3
               IF (LOPT(J,I) ) 160, 180, 170
C    FLAG FOR PATH
  160          CONVRT=1.D0
               IF ( IFLAG .NE. 0 ) THEN
                  IF(INDEX(KEYWRD,'STEP1').NE.0)THEN
                     LPARA1=LPARAM
                     LATOM1=LATOM
                     LPARA2=J
                     LATOM2=I
                     LATOM=0
                     IFLAG=0
                     GOTO 180
                  ELSE
                     WRITE(WU,'('' ONLY ONE REACTION COORDINATE PERMITTED
     1'')')
                     READMO = .FALSE.
                     RETURN
                  ENDIF
               ENDIF
               LATOM  = I
               LPARAM = J
               IF(J.GT.1) CONVRT=0.01745329252D00
               REACT(1)  = GEO(J,I)
               IREACT=1
               IFLAG = 1
               GO TO 180
C    FLAG FOR OPTIMIZE
  170          NVAR = NVAR + 1
               LOC(1,NVAR) = I
               LOC(2,NVAR) = J
               XPARAM(NVAR)   = GEO(J,I)
  180    CONTINUE
      ENDIF
C READ IN PATH VALUES
      IF(IFLAG.EQ.0) GO TO 220
      IF(INDEX(KEYWRD,'NLLSQ').NE.0)THEN
         WRITE(WU,'(A)')' NLLSQ USED WITH REACTION PATH; '//
     1'THIS OPTION IS NOT ALLOWED'
         READMO = .FALSE.
         RETURN
      ENDIF
      IF(INDEX(KEYWRD,'SIGMA').NE.0)THEN
         WRITE(WU,'(A)')' SIGMA USED WITH REACTION PATH; '//
     1'THIS OPTION IS NOT ALLOWED'
         READMO = .FALSE.
         RETURN
      ENDIF
      IF(INDEX(KEYWRD,'STEP')+INDEX(KEYWRD,'POINTS').NE.0)THEN
         STEP=READA(KEYWRD,INDEX(KEYWRD,'STEP=')+5)
         NPTS=READA(KEYWRD,INDEX(KEYWRD,'POINT=')+6)
         IF(NPTS.GT.200)THEN
            WRITE(WU,'(///,''    ONLY TWO HUNDRED POINTS ALLOWED IN REACT
     1'',''ION COORDINATE'')')
            READMO = .FALSE.
            RETURN
         ENDIF
         IF(LPARAM.EQ.1.AND.STEP.LE.0)THEN
            WRITE(WU,'(///,''    STEP FOR BOND LENGTH SHOULD BE SET POSIT
     1IVE '',''TO PREVENT TWO ATOMS COLLAPSE'')')
            READMO = .FALSE.
            RETURN
         ENDIF
         GO TO 220
      ENDIF
  190 READ(5,'(A)',END=210) LINE
      CALL NUCHAR(LINE,VALUE,NREACT)
      IF(NREACT.EQ.0)GOTO 210
      DO 200 I=1,NREACT
         IJ=IREACT+I
         IF(IJ.GT.200)THEN
           WRITE(WU,'(///,''    ONLY TWO HUNDRED POINTS ALLOWED IN REACT
     1ION'','' COORDINATE'')')
            READMO = .FALSE.
            RETURN
         ENDIF
         REACT(IJ)=VALUE(I)*CONVRT
         IF(ABS(REACT(IJ)-REACT(IJ-1)).LT.1.D-5)THEN
            DUM1 = REACT(IJ)/CONVRT
            DUM2 = REACT(IJ-1)/CONVRT
            WRITE(WU,'(///,'' TWO ADJACENT POINTS ARE IDENTICAL:  '',
     1 F7.3,2X,F7.3,/,'' THIS IS NOT ALLOWED IN A PATH CALCULATION'')')
     2 DUM1,DUM2
            READMO = .FALSE.
            RETURN
         ENDIF
  200 CONTINUE
      IREACT=IREACT+NREACT
      GO TO 190
  210 CONTINUE
      DEGREE=1.D0
      IF(LPARAM.GT.1)DEGREE=90.D0/ASIN(1.D0)
      IF(IREACT.LE.1) THEN
         WRITE(WU,'(//10X,'' NO POINTS SUPPLIED FOR REACTION PATH'')')
         WRITE(WU,'(//10X,'' GEOMETRY AS READ IN IS AS FOLLOWS'')')
         CALL GEOUT(1)
         READMO = .FALSE.
         RETURN
      ELSE
         WRITE(WU,'(//10X,'' POINTS ON REACTION COORDINATE'')')
         WRITE(WU,'(10X,8F8.2)')(REACT(I)*DEGREE,I=1,IREACT)
      ENDIF
      IEND=IREACT+1
      REACT(IEND)=-1.D12
C
C OUTPUT GEOMETRY AS FEEDBACK
C
  220 CALL WRTTXT(WU)
      IF(INDEX(KEYWRD,'NOLOG').EQ.0)THEN
!         OPEN(UNIT=11, FORM='FORMATTED', STATUS='UNKNOWN',
!     +FILE=GETNAM('FOR011'))
         CALL WRTTXT(11)
      ENDIF
      CALL GEOUT(1)
      CALL GMETRY(GEO,COORD)
C
C  IF A POLYMER, EXPAND TO MERS
C
      IF(INDEX(KEYWRD,' MERS').NE.0)CALL MAKPOL(COORD)
      IF (INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN
         IF(INDEX(KEYWRD,'0SCF').NE.0)THEN
C
C  WRITE OUT CARTESIAN COORDINATES FOR USE AS A DATA-SET
C
            WRITE(WU,'(A)')'   GEOMETRY IN CARTESIAN COORDINATE FORMAT'
            CALL WRTTXT(WU)
            J=0
            DO 230 I=1,NATOMS
               IF(LABELS(I).NE.99)THEN
                  J=J+1
                  WRITE(WU,'(2X,A,3(F19.13,I3))')
     1    ELEMNT(LABELS(I)),(COORD(K,J),1,K=1,3)
               ENDIF
  230       CONTINUE
         ELSE
            WRITE(WU,'(//10X,''CARTESIAN COORDINATES '',/)')
            WRITE(WU,'(4X,''NO.'',7X,''ATOM'',9X,''X'',
     1  9X,''Y'',9X,''Z'',/)')
            L=0
            DO 240 I=1,NATOMS
               IF(LABELS(I) .EQ. 99.OR.LABELS(I).EQ.107) GOTO 240
               L=L+1
               WRITE(WU,'(I6,8X,A2,4X,3F10.4)')
     1  L,ELEMNT(LABELS(I)),(COORD(J,L),J=1,3)
  240       CONTINUE
         ENDIF
      ENDIF
C
C    C is not actually used in this call.
C
      CALL SYMTRZ(COORD,C,NORBS,NORBS,.FALSE.,.FALSE.)
      WRITE(WU,'(//''     MOLECULAR POINT GROUP   :   '',A4)') NAME
      IF(   INDEX(KEYWRD,' XYZ') .NE. 0 )THEN
         IF( NVAR .NE. 0 .AND.
     1 INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(NDEP.NE.0)
     1WRITE(WU,'(//10X,'' INTERNAL COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN CARTESIAN ''
     3,''COORDINATES'')')
            IF(NVAR.LT.3*NUMAT-6)
     1WRITE(WU,'(//10X,'' INTERNAL COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN CARTESIAN COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')')
           WRITE(WU,'(//10X,'' THIS INVOLVES A LOGICALLLY ABSURD CHOICE'
     1',/10X,'' SO THE CALCULATION IS TERMINATED AT THIS POINT'')')
            READMO = .FALSE.
            RETURN
         ENDIF
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 250 J=1,NUMAT
            SUMX=SUMX+COORD(1,J)
            SUMY=SUMY+COORD(2,J)
  250    SUMZ=SUMZ+COORD(3,J)
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 260 J=1,NUMAT
            GEO(1,J)=COORD(1,J)-SUMX
            GEO(2,J)=COORD(2,J)-SUMY
  260    GEO(3,J)=COORD(3,J)-SUMZ
         NA(1)=99
         J=0
         NVAR=0
         DO 280 I=1,NATOMS
            IF(LABELS(I).NE.99)THEN
               J=J+1
               IF(J.EQ.1)THEN
                  K=0
               ELSEIF(J.LT.4)THEN
                  K=MIN(3,I-1)
               ELSE
                  K=3
               ENDIF
               DO 270 L=1,K
                  NVAR=NVAR+1
                  LOC(1,NVAR)=J
                  LOC(2,NVAR)=L
  270          XPARAM(NVAR)=GEO(L,J)
               LABELS(J)=LABELS(I)
            ENDIF
  280    CONTINUE
         NATOMS=NUMAT
      ELSE
         IF(NVAR.EQ.0) RETURN
         IF( .NOT. INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(NDEP.NE.0)
     1WRITE(WU,'(//10X,'' CARTESIAN COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN INTERNAL ''
     3,''COORDINATES'')')
            IF(NVAR.LT.3*NUMAT-6)
     1WRITE(WU,'(//10X,'' CARTESIAN COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN INTERNAL COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')')
            WRITE(WU,'(//10X,''MOPAC, BY DEFAULT, USES INTERNAL COORDINAT
     1ES'',/10X,''TO SPECIFY CARTESIAN COORDINATES USE KEY-WORD :XYZ:'')
     2')
            WRITE(WU,'(10X,''YOUR CURRENT CHOICE OF KEY-WORDS INVOLVES''
     1,'' A LOGICALLLY'',/10X,''ABSURD CHOICE SO THE CALCULATION IS'',
     2'' TERMINATED AT THIS POINT'')')
            READMO = .FALSE.
            RETURN
         ENDIF
      ENDIF
      RETURN
      END
