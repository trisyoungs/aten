      SUBROUTINE DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XPARAM(*), GD(*), XLAST(*), MDFP(9),XDFP(9)
**********************************************************************
*
* DFPSAV STORES AND RESTORES DATA USED IN THE D-F-P GEOMETRY
*        OPTIMISATION.
*
*  ON INPUT TOTIME = TOTAL CPU TIME ELAPSED DURING THE CALCULATION.
*           XPARAM = CURRENT VALUE OF PARAMETERS.
*           GD     = OLD GRADIENT.
*           XLAST  = OLD VALUE OF PARAMETERS.
*           FUNCT1 = CURRENT VALUE OF HEAT OF FORMATION.
*           MDFP   = INTEGER CONSTANTS USED IN D-F-P.
*           XDFP   = REAL CONSTANTS USED IN D-F-P.
*           MDFP(9)= 1 FOR DUMP, 0 FOR RESTORE.
**********************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, DUMY(MAXPAR)
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, ILOOP
      COMMON /PPARAM/ CURRT
      COMMON /GPARAM/ CURRT1,CURRT2
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C     COMMON /PROFIL/ PROFIL
      COMMON /PROFIC/ PROFIL
C ***************************** at 1994-05-25 *****
      COMMON /SURF  / SURF
      COMMON /KLOOP / KLOOP
      COMMON /IJLP  / IJLP, ILP, JLP, JLP1, IONE
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),CALCST
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /ELEMTS/ ELEMNT(107)
      CHARACTER KOMENT*81, TITLE*81
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      COMMON /MESH  / LATOM1,LPARA1,LATOM2,LPARA2
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /FMATRX/ HESINV(MAXPAR**2+MAXPAR*3+1), IDUMY2(4)
      COMMON /ERRFN / ERRFN(MAXPAR), AICORR(MAXPAR)
      COMMON /NUMCAL/ NUMCAL
      DIMENSION COORD(3,NUMATM)
      DIMENSION PROFIL(200)
      DIMENSION SURF(23*23)
      CHARACTER ELEMNT*2, KEYWRD*241, GETNAM*80
      SAVE FIRST
      LOGICAL FIRST, INTXYZ
      DATA ICALCN/0/
      FIRST=(ICALCN.EQ.NUMCAL)
!      OPEN(UNIT=9,FILE=GETNAM('FOR009')
!     +                     ,STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 9
!      OPEN(UNIT=10,FILE=GETNAM('FOR010')
!     +                     ,STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND 10
      IR=9
      IF(MDFP(9) .NE. 0) THEN
         IF(MDFP(9) .EQ. 1) THEN
            WRITE(6,'(//10X,''- - - - - - - TIME UP - - - - - - -'',//)'
     1)
            IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
               WRITE(6,'(//10X,'' NO RESTART EXISTS FOR SADDLE'',//
     1  10X,'' HERE IS A DATA-FILE FILES THAT MIGHT BE SUITABLE'',/
     2  10X,'' FOR RESTARTING THE CALCULATION'',///)')
               WRITE(6,'(A)')KEYWRD,KOMENT,TITLE
               INTXYZ=(NA(1).EQ.0)
               DO 30 ILOOP=1,2
                  IF(INTXYZ)THEN
                     GEO(2,1)=0.D0
                     GEO(3,1)=0.D0
                     GEO(1,1)=0.D0
                     GEO(2,2)=0.D0
                     GEO(3,2)=0.D0
                     GEO(3,3)=0.D0
                     DO 10 I=1,NATOMS
                        DO 10 J=1,3
   10                COORD(J,I)=GEO(J,I)
                  ELSE
                     CALL XYZINT(GEO,NUMAT,NA,NB,NC,1.D0,COORD)
                  ENDIF
                  CALL GEOUT(-6)
                  DO 20 I=1,NATOMS
                     DO 20 J=1,3
   20             GEO(J,I)=GEOA(J,I)
                  NA(1)=99
   30          CONTINUE
               WRITE(6,'(///10X,''CALCULATION TERMINATED HERE'')')
               STOP
            ENDIF
            WRITE(6,'(//10X,'' - THE CALCULATION IS BEING DUMPED TO DISK
     1'',  /10X,''   RESTART IT USING THE MAGIC WORD "RESTART"'')')
            WRITE(6,'(//10X,''CURRENT VALUE OF HEAT OF FORMATION =''
     1  ,F12.6)')FUNCT1
         ENDIF
         IF(MDFP(9) .EQ. 1)THEN
            IF(NA(1) .EQ. 99) THEN
C
C  CONVERT FROM CARTESIAN COORDINATES TO INTERNAL
C
               DO 40 I=1,NATOMS
                  DO 40 J=1,3
   40          COORD(J,I)=GEO(J,I)
               CALL XYZINT(COORD,NUMAT,NA,NB,NC,1.D0,GEO)
            ENDIF
            GEO(2,1)=0.D0
            GEO(3,1)=0.D0
            GEO(1,1)=0.D0
            GEO(2,2)=0.D0
            GEO(3,2)=0.D0
            GEO(3,3)=0.D0
            NA(1)=0
            CALL GEOUT(6)
         ENDIF
         WRITE(IR)MDFP,XDFP,TOTIME,FUNCT1
         WRITE(IR)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR)
         WRITE(IR)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR)
         LINEAR=(NVAR*(NVAR+1))/2
         WRITE(IR)(HESINV(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         WRITE(10)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)
         IF(LATOM.NE.0)THEN
            IF(INDEX(KEYWRD,'STEP').NE.0)THEN
               WRITE(IR) KLOOP
               WRITE(IR) CURRT
               WRITE(IR) (PROFIL(I),I=1,KLOOP)
            ELSE
               WRITE(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
               WRITE(IR)ILOOP,X0, X1, X2
            ENDIF
         ENDIF	
         IF(INDEX(KEYWRD,'STEP1').NE.0)THEN
            WRITE(IR)IJLP, ILP,JLP,JLP1,IONE
            WRITE(IR) CURRT1,CURRT2
            WRITE(IR) (SURF(I),I=1,IJLP)
         ENDIF
         WRITE(IR)(ERRFN(I),I=1,NVAR)
!         CLOSE (9)
!         CLOSE (10)
      ELSE
         IF (FIRST) WRITE(6,'(//10X,'' RESTORING DATA FROM DISK''/)')
         READ(IR,END=60,ERR=60)MDFP,XDFP,TOTIME,FUNCT1
         IF (FIRST) WRITE(6,'(10X,''FUNCTION ='',F13.6//)')FUNCT1
         READ(IR)(XPARAM(I),I=1,NVAR),(GD(I),I=1,NVAR)
         READ(IR)(XLAST(I),I=1,NVAR),(GRAD(I),I=1,NVAR)
         LINEAR=(NVAR*(NVAR+1))/2
         READ(IR)(HESINV(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         READ(10)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)READ(10)(PB(I),I=1,LINEAR)
         IF(LATOM.NE.0)THEN
            IF(INDEX(KEYWRD,'STEP').NE.0)THEN
               READ(IR) KLOOP
               READ(IR) CURRT
               READ(IR) (PROFIL(I),I=1,KLOOP)
            ELSE
               READ(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
               READ(IR)ILOOP,X0, X1, X2
            ENDIF
         ENDIF	
         IF(INDEX(KEYWRD,'STEP1').NE.0)THEN
            READ(IR)IJLP, ILP,JLP,JLP1,IONE
            READ(IR) CURRT1,CURRT2
            READ(IR) (SURF(I),I=1,IJLP)
         ENDIF
         READ(IR)(ERRFN(I),I=1,NVAR)
   50    FIRST=.FALSE.
         RETURN
   60    WRITE(6,'(//10X,''NO RESTART FILE EXISTS!'')')
         STOP
      ENDIF
      END
