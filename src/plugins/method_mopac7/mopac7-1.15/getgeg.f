      SUBROUTINE GETGEG(IREAD,LABELS,GEO,NA,NB,NC,AMS,NATOMS,INT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION GEO(3,*),NA(*),NB(*),NC(*),AMS(*)
     1,LABELS(*)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                LOCDEP(MAXPAR)
      COMMON /SIMBOL/ SIMBOL(MAXPAR)
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /ATOMTX/ LTXT, TXTATM(NUMATM)
      COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      DIMENSION ISTART(20), LGEO(3,100)
      CHARACTER LINE*80, TGEO(3,100)*12, TXTATM*8, STRING*80, SIMBOL*10
      LOGICAL LEADSP, INT
      CHARACTER ELEMNT(107)*2, LTXT*1
      DATA (ELEMNT(I),I=1,107)/' H','HE',
     1 'LI','BE',' B',' C',' N',' O',' F','NE',
     2 'NA','MG','AL','SI',' P',' S','CL','AR',
     3 'K ','CA','SC','TI',' V','CR','MN','FE','CO','NI','CU',
     4 'ZN','GA','GE','AS','SE','BR','KR',
     5 'RB','SR',' Y','ZR','NB','MO','TC','RU','RH','PD','AG',
     6 'CD','IN','SN','SB','TE',' I','XE',
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',
     8 'HO','ER','TM','YB','LU','HF','TA',' W','RE','OS','IR','PT',
     9 'AU','HG','TL','PB','BI','PO','AT','RN',
     1 'FR','RA','AC','TH','PA','U','NP','PU','AM','CM','BK','CF','XX',
     2 'FM','MD','CB','++',' +','--',' -','TV'/
      NERR=0
      INT=.TRUE.
      NUMAT=0
      NA(1)=0
      NB(1)=0
      NC(1)=0
      NB(2)=0
      NC(2)=0
      NC(3)=0
      MAXTXT=0
      DO 60 NATOMS=1,100
         READ(IREAD,'(A)',END=70,ERR=70)LINE
         IF(LINE.EQ.' ')GOTO 70
C
C   SEE IF TEXT IS ASSOCIATED WITH THIS ELEMENT
C
         I=INDEX(LINE,'(')
         IF(I.NE.0)THEN
C
C  YES, ELEMENT IS LABELLED.
C
            K=INDEX(LINE,')')
            TXTATM(NATOMS)=LINE(I:K)
            MAXTXT=MAX(MAXTXT,K-I+1)
            STRING=LINE(1:I-1)//LINE(K+1:)
            LINE=STRING
         ELSE
            TXTATM(NATOMS)=' '
         ENDIF
************************************************************************
         DO 10 I=1,80
            ILINE=ICHAR(LINE(I:I))
            IF(ILINE.GE.ICHAR('a').AND.ILINE.LE.ICHAR('z')) THEN
               LINE(I:I)=CHAR(ILINE+ICHAR('A')-ICHAR('a'))
            ENDIF
   10    CONTINUE
************************************************************************
         NVALUE=0
         LEADSP=.TRUE.
         DO 20 I=1,80
            IF (LEADSP.AND.LINE(I:I).NE.' ') THEN
               NVALUE=NVALUE+1
               ISTART(NVALUE)=I
            END IF
            LEADSP=(LINE(I:I).EQ.' ')
   20    CONTINUE
         DO 30 J=1,107
   30    IF(INDEX(' '//LINE(ISTART(1):ISTART(1)+2),ELEMNT(J)//' ').NE.0)
     1 GOTO 40
         IF(INDEX(' '//LINE(ISTART(1):ISTART(1)+2),' X').NE.0) THEN
            J=99
            GOTO 40
         ENDIF
         WRITE(6,'(2A)')' ELEMENT NOT RECOGNIZED: ',
     1LINE(ISTART(1):ISTART(1)+2)
         NERR=NERR+1
   40    LABELS(NATOMS)=J
         IF(J.NE.99)THEN
            NUMAT=NUMAT+1
            ATMASS(NUMAT)=READA(LINE(1:MAX(ISTART(2)-1,1)),ISTART(1))
            IF(ATMASS(NUMAT).GT.1.D-15)THEN
               WRITE(6,'('' FOR ATOM'',I4,''  ISOTOPIC MASS:''
     1    ,F15.5)')NATOMS, ATMASS(NUMAT)
            ELSE
               ATMASS(NUMAT)=AMS(J)
            ENDIF
C#         WRITE(6,*)NATOMS,NUMAT,ATMASS(NUMAT)
         ENDIF
         TGEO(1,NATOMS)=' '
         TGEO(2,NATOMS)=' '
         TGEO(3,NATOMS)=' '
         IF(NATOMS.EQ.1) GOTO 50
         NA(NATOMS)=READA(LINE,ISTART(2))
         CALL GETVAL(LINE(ISTART(3):),GEO(1,NATOMS),TGEO(1,NATOMS))
         IF(NATOMS.EQ.2) GOTO 50
         NB(NATOMS)=READA(LINE,ISTART(4))
         CALL GETVAL(LINE(ISTART(5):),GEO(2,NATOMS),TGEO(2,NATOMS))
         IF(NATOMS.EQ.3) GOTO 50
         NC(NATOMS)=READA(LINE,ISTART(6))
         CALL GETVAL(LINE(ISTART(7):),GEO(3,NATOMS),TGEO(3,NATOMS))
   50    CONTINUE
   60 CONTINUE
   70 NATOMS=NATOMS-1
      DO 80 I=1,NATOMS
         DO 80 J=1,3
   80 LGEO(J,I)=-1
      IVAR=-1
      NVAR=0
      NDEP=0
      KERR=0
   90 READ(IREAD,'(A)',END=180,ERR=180)LINE
      IF(LINE.EQ.' ')THEN
         IF(IVAR.EQ.-1)THEN
            MERR=0
            DO 110 I=1,NATOMS
               DO 100 J=1,3
  100          IF(GEO(J,I).LT.-998)MERR=MERR+1
  110       CONTINUE
C
C  IF ALL SYMBOLS ARE DEFINED, THEN DO NOT READ 'FIXED' SYMBOLS
C
            IF(MERR.EQ.0) GOTO 180
            IVAR=NVAR
            GOTO 90
         ELSE
            GOTO 180
         ENDIF
      ENDIF
************************************************************************
      DO 120 I=1,80
         ILINE=ICHAR(LINE(I:I))
         IF(ILINE.GE.ICHAR('a').AND.ILINE.LE.ICHAR('z')) THEN
            LINE(I:I)=CHAR(ILINE+ICHAR('A')-ICHAR('a'))
         ENDIF
  120 CONTINUE
************************************************************************
      DO 130 I=1,80
  130 IF(LINE(I:I).NE.' ')GOTO 140
  140 DO 150 L=I,I+12
  150 IF(LINE(L:L).EQ.' ')GOTO 160
  160 SUM=READA(LINE,L)
      N=0
      LERR=0
      DO 170 J=1,NATOMS
         DO 170 K=1,3
            IF(TGEO(K,J).EQ.LINE(I:L) .OR.
     1TGEO(K,J)(2:).EQ.LINE(I:L).AND.TGEO(K,J)(1:1).EQ.'-')THEN
               IF(LGEO(K,J).NE.-1)LERR=1
               LGEO(K,J)=LGEO(K,J)+1
               N=N+1
               GEO(K,J)=SUM
               IF(N.EQ.1)THEN
                  NVAR=NVAR+1
                  LOC(1,NVAR)=J
                  LOC(2,NVAR)=K
                  XPARAM(NVAR)=SUM
                  SIMBOL(NVAR)=TGEO(K,J)
                  IF(SIMBOL(NVAR)(1:1).EQ.'-')THEN
      WRITE(6,'(A)')' NEGATIVE SYMBOLICS MUST BE PRECEEDED BY '
     +//' THE POSITIVE EQUIVALENT'
                  WRITE(6,'(A)')' FAULTY SYMBOLIC:  '//SIMBOL(NVAR)
               STOP
               ENDIF
               ENDIF
               IF(N.GT.1)THEN
                  NDEP=NDEP+1
                  LOCPAR(NDEP)=LOC(1,NVAR)
                  IDEPFN(NDEP)=LOC(2,NVAR)
                  IF(TGEO(K,J)(1:1).EQ.'-')THEN
                     IDEPFN(NDEP)=14
                     IF(LOC(2,NVAR).NE.3) THEN
                        KERR=KERR+1
                        WRITE(6,'(2A)')' ONLY DIHEDRAL SYMBOLICS CAN BE'
     1//' PRECEEDED BY A "-" SIGN'
                     ENDIF
                  ENDIF
                  LOCDEP(NDEP)=J
               ENDIF
            ENDIF
  170 CONTINUE
      KERR=KERR+LERR
      IF(LERR.EQ.1)THEN
         WRITE(6,'(2A)')' THE FOLLOWING SYMBOL HAS BEEN DEFINED MORE'//
     1' THAN ONCE:',LINE(I:L)
         NERR=NERR+1
      ENDIF
      IF(N.EQ.0)THEN
         WRITE(6,'(2A)')' THE FOLLOWING SYMBOLIC WAS NOT USED:',LINE(I:L
     1)
         NERR=NERR+1
      ENDIF
      GOTO 90
  180 CONTINUE
      MERR=0
      DO 200 I=1,NATOMS
         DO 190 J=1,3
  190    IF(GEO(J,I).LT.-998)MERR=MERR+1
C#     WRITE(6,'(2X,A,3F12.6,3I4)')ELEMNT(LABELS(I)),
C#     1(GEO(J,I),J=1,3), NA(I), NB(I), NC(I)
  200 CONTINUE
      IF(MERR.NE.0)WRITE(6,'(I4,A)')MERR, ' GEOMETRY VARIABLES WERE NOT'
     1//' DEFINED'
      IF(MERR+KERR+NERR.NE.0)THEN
         WRITE(6,'(A,I3,A)')
     1' THE GEOMETRY DATA-SET CONTAINED',MERR+KERR+NERR,' ERRORS'
         STOP
      ENDIF
C
C  SORT PARAMETERS TO BE OPTIMIZED INTO INCREASING ORDER OF ATOMS
C
      IF(IVAR.NE.-1)NVAR=IVAR
      DO 230 I=1,NVAR
         J=100000
         DO 210 L=I,NVAR
            IF(J.GT.LOC(1,L)*4+LOC(2,L))THEN
               K=L
               J=LOC(1,L)*4+LOC(2,L)
            ENDIF
  210    CONTINUE
         STRING(1:10)=SIMBOL(I)
         SIMBOL(I)=SIMBOL(K)
         SIMBOL(K)=STRING
         SUM=XPARAM(I)
         XPARAM(I)=XPARAM(K)
         XPARAM(K)=SUM
         DO 220 J=1,2
            L=LOC(J,I)
            LOC(J,I)=LOC(J,K)
  220    LOC(J,K)=L
  230 CONTINUE
C#      IF(NVAR.NE.0)WRITE(6,'(//,''    PARAMETERS TO BE OPTIMIZED'')')
      DEGREE=ASIN(1.D0)/90
      DO 240 I=1,NVAR
C#      WRITE(6,'(2I6,F12.6)')LOC(1,I),LOC(2,I),XPARAM(I)
  240 IF(LOC(2,I).NE.1)XPARAM(I)=XPARAM(I)*DEGREE
C#      IF(NDEP.NE.0)WRITE(6,'(//,''   SYMMETRY FUNCTIONS  '')')
C#      DO 28 I=1,NDEP
C#   28 WRITE(6,'(3I6)')LOCPAR(I),IDEPFN(I),LOCDEP(I)
      LTXT=CHAR(MAXTXT)
      RETURN
      END
      SUBROUTINE GETVAL(LINE,X,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER LINE*80, T*12, CH1*1, CH2*1
      CH1=LINE(1:1)
      CH2=LINE(2:2)
      IF((ICHAR(CH1).LT.ICHAR('A').OR.ICHAR(CH1).GT.ICHAR('Z')) .AND.
     1(ICHAR(CH2).LT.ICHAR('A').OR.ICHAR(CH2).GT.ICHAR('Z')))THEN
C
C   IS A NUMBER
C
         X=READA(LINE,1)
         T=' '
      ELSE
         I=INDEX(LINE,' ')
         T=LINE(:I)
         X=-999.D0
      ENDIF
      RETURN
      END
