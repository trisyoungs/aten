      SUBROUTINE GETTXT
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      DIMENSION IS(3)
      CHARACTER KEYWRD*241, KOMENT*81, TITLE*81, CH*1, CH2*1, FILEN*50
     +,GETNAM*80, OLDKEY*80
      IS(1)=161
      IS(2)=81
      IS(3)=1
      KEYWRD=' '
      KOMENT='    NULL  '
      TITLE ='    NULL  '
      READ(5,'(A)',END=100,ERR=90)KEYWRD(:80)
      OLDKEY=KEYWRD
      CALL UPCASE(KEYWRD(1:80))
      IF(INDEX(KEYWRD,'SETUP').NE.0)THEN
         I=INDEX(KEYWRD,'SETUP=')
         IF(I.NE.0)THEN
            J=INDEX(KEYWRD(I:),' ')
            FILEN=OLDKEY(I+6:I+J-1)
         ELSE
            FILEN='SETUP'
         ENDIF
         OPEN(UNIT=4,FILE=GETNAM(FILEN),
     +        STATUS='UNKNOWN',FORM='FORMATTED')
         REWIND 4
         READ(4,'(A)',END=40,ERR=40)KEYWRD(81:160)
         CALL UPCASE(KEYWRD(81:160))
         READ(4,'(A)',END=10,ERR=10)KEYWRD(161:240)
         CALL UPCASE(KEYWRD(161:240))
   10    CONTINUE
         READ(5,'(A)',END=100,ERR=90)KOMENT,TITLE
      ELSEIF(INDEX(KEYWRD(1:80),' +') .NE.0)THEN
C
C  READ SECOND KEYWORD LINE
C
         READ(5,'(A)',END=100,ERR=90)KEYWRD(81:160)
         OLDKEY=KEYWRD(81:160)
         CALL UPCASE(KEYWRD(81:160))
         IF(INDEX(KEYWRD(81:160),'SETUP').NE.0)THEN
            I=INDEX(KEYWRD,'SETUP=')
            IF(I.NE.0)THEN
               J=INDEX(KEYWRD(I:),' ')
               FILEN=OLDKEY(I-74:I+J-80)
            ELSE
               FILEN='SETUP'
            ENDIF
            OPEN(UNIT=4,FILE=GETNAM(FILEN)
     +          ,STATUS='UNKNOWN',FORM='FORMATTED')
            REWIND 4
            READ(4,'(A)',END=20,ERR=20)KEYWRD(161:240)
            CALL UPCASE(KEYWRD(161:240))
   20       CONTINUE
         ELSEIF(INDEX(KEYWRD(81:160),' +') .NE.0)THEN
C
C  READ THIRD KEYWORD LINE
C
            READ(5,'(A)',END=100,ERR=90)KEYWRD(161:240)
            CALL UPCASE(KEYWRD(161:240))
         ENDIF
C
C  READ TITLE LINE
C
         READ(5,'(A)',END=100,ERR=90)KOMENT,TITLE
      ELSEIF(INDEX(KEYWRD(:80),'&').NE.0)THEN
         READ(5,'(A)',END=100,ERR=90)KEYWRD(81:160)
         OLDKEY=KEYWRD(81:160)
         CALL UPCASE(KEYWRD(81:160))
         IF(INDEX(KEYWRD(81:160),'SETUP').NE.0)THEN
            I=INDEX(KEYWRD,'SETUP=')
            IF(I.NE.0)THEN
               J=INDEX(KEYWRD(I:),' ')
               FILEN=OLDKEY(I-74:I+J-80) 
C               write(*,*)' <'//FILEN//'>'
C               stop
            ELSE
               FILEN='SETUP'
            ENDIF
            OPEN(UNIT=4,FILE=GETNAM(FILEN),
     +           STATUS='UNKNOWN',FORM='FORMATTED')
            REWIND 4
            READ(4,'(A)',END=30,ERR=30)KEYWRD(161:240)
            CALL UPCASE(KEYWRD(161:240))
            READ(5,'(A)',END=100,ERR=90)TITLE
   30       CONTINUE
         ELSEIF(INDEX(KEYWRD(81:160),'&').NE.0)THEN
            READ(5,'(A)',END=100,ERR=90)KEYWRD(161:240)
         ELSE
            READ(5,'(A)',END=100,ERR=90)TITLE
         ENDIF
      ELSE
         READ(5,'(A)',END=100,ERR=90)KOMENT,TITLE
      ENDIF
      GOTO 50
   40 WRITE(6,'(A)')' SETUP FILE MISSING OR CORRUPT'
   50 DO 80 J=1,3
         IF(KEYWRD(IS(J):IS(J)) .NE. ' ') THEN
            CH=KEYWRD(IS(J):IS(J))
            KEYWRD(IS(J):IS(J))=' '
            DO 60 I=IS(J)+1,239
               CH2=KEYWRD(I:I)
               KEYWRD(I:I)=CH
               CH=CH2
               IF(KEYWRD(I+1:I+2) .EQ. '  ') THEN
                  KEYWRD(I+1:I+1)=CH
                  GOTO 70
               ENDIF
   60       CONTINUE
            WRITE(6,'(A,I2,A)')' LINE',J,' OF KEYWORDS DOES NOT HAVE ENO
     1UGH'
            WRITE(6,'(A)')' SPACES FOR PARSING.  PLEASE CORRECT LINE.'
            STOP
   70       CONTINUE
         ENDIF
   80 CONTINUE
      RETURN
   90 WRITE(6,'(A)')' ERROR IN READ OF FIRST THREE LINES'
  100 STOP
      END
      SUBROUTINE UPCASE(KEYWRD)
      CHARACTER*80 KEYWRD
      ICAPA=ICHAR('A')
      ILOWA=ICHAR('a')
      ILOWZ=ICHAR('z')
      DO 10 I=1,80
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            KEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   10 CONTINUE
      RETURN
      END
