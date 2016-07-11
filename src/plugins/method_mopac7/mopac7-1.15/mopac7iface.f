C
C         Notice of Public Domain nature of MOPAC
C
C      'This computer program is a work of the United States
C       Government and as such is not subject to protection by
C       copyright (17 U.S.C. # 105.)  Any person who fraudulently
C       places a copyright notice or does any other act contrary
C       to the provisions of 17 U.S. Code 506(c) shall be subject
C       to the penalties provided therein.  This notice shall not
C       be altered or removed from this software and is to be on
C       all reproductions.'
C

C TGAY FOLLOWING SUBROUTINES ARE BASED ON MOPAC7LIB.F

      LOGICAL FUNCTION OM7SETUP(DATAFILE,NLINES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /SCFTYP/ EMIN, LIMSCF
      COMMON /KEYWRD/ KEYWRD
      COMMON /OKMANY/ ISOK
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /MESAGE/ IFLEPO,ISCF
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /ATHEAT/ ATHEAT
      COMMON /LAST  / LAST
      COMMON /ATOMIC/ EISOL(107),EHEAT(107)
      COMMON /NUMCAL/ NUMCAL
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C     COMMON /TIME  / TIME0
      COMMON /TIMEC / TIME0
C ***************************** at 1994-05-25 *****
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
C COSMO change
      LOGICAL ISEPS, USEPS , UPDA
      COMMON /ISEPS/  ISEPS, USEPS, UPDA
C end of COSMO change
      CHARACTER*241 KEYWRD, GETNAM*80
      LOGICAL ISOK, LIMSCF
      DATA NUMCAL /0/
      DATA ISOK /.TRUE./
C ****** Internal Input File
      CHARACTER(*) DATAFILE
      INTEGER IREC,NUMREC
      COMMON /DATFIL/ IREC,NUMREC
C function declarations
      LOGICAL READMO

C determine number of lines in input file
      NUMREC=LEN(DATAFILE)/80
      IF (MOD(LEN(DATAFILE),80).NE.0) THEN
        write(0,*) "WARNING"
      ENDIF
      WRITE(0,*) "Number of records in internal file is ", NUMREC
      IREC=0

      ISOK=.TRUE.
   10 NUMCAL=1

      TIME0=SECOND()

C READ AND CHECK INPUT FILE, EXIT IF NECESSARY.
C     WRITE INPUT FILE TO UNIT 6 AS FEEDBACK TO USER
C
      IF (.NOT.READMO(DATAFILE)) THEN
        write(0,*) "READMO FAILED"
        OM7SETUP=.FALSE.
        RETURN
      ENDIF

      EMIN=0.D0
      CALL TIMER('AFTER READ')
      WRITE(6,'(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',
     1/         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'')')
         WRITE(6,'(10X,''     BY JAMES J. P. STEWART AT THE'',/
     1         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
     2         ,10X,''          AUSTIN, TEXAS, 78712'')')
         WRITE(6,'(10X,''  MODIFIED TO DO ESP CALCULATIONS BY''
     1          ,10X,''    BRENT H. BESLER AND K. M. MERZ JR. 1989'')')


C
C INITIALIZE CALCULATION AND WRITE CALCULATION INDEPENDENT INFO
C
C      IF(INDEX(KEYWRD,'0SCF') .NE. 0) THEN
C         WRITE(6,'(A)')' GEOMETRY IN MOPAC Z-MATRIX FORMAT'
      CALL GEOUT(6)
C         IF(INDEX(KEYWRD,' AIGOUT').NE.0)THEN
C            WRITE(6,'(//,A)')'  GEOMETRY IN GAUSSIAN Z-MATRIX FORMAT'
C            CALL WRTTXT(6)
C            CALL GEOUTG(6)
C         ENDIF
C         GOTO 50
C      ENDIF
      CALL MOLDAT(0)
C *** ok, initialization is now done, and we can stop here...
      RETURN
      END

      SUBROUTINE LM7STOP()
C *** this is the end part of the original MOPAC main program.
C *** this is a subroutine that will run the mopac down...
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /SCFTYP/ EMIN, LIMSCF
      COMMON /KEYWRD/ KEYWRD
      COMMON /OKMANY/ ISOK
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /MESAGE/ IFLEPO,ISCF
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /ATHEAT/ ATHEAT
      COMMON /LAST  / LAST
      COMMON /ATOMIC/ EISOL(107),EHEAT(107)
      COMMON /NUMCAL/ NUMCAL
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C     COMMON /TIME  / TIME0
      COMMON /TIMEC / TIME0
C ***************************** at 1994-05-25 *****
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
C COSMO change
      LOGICAL ISEPS, USEPS , UPDA
      COMMON /ISEPS/  ISEPS, USEPS, UPDA
C end of COSMO change
      CHARACTER*241 KEYWRD, GETNAM*80
      LOGICAL ISOK, LIMSCF
   40 LAST=1
C      IF(IFLEPO.GE.0)CALL WRITMO(TIME0, ESCF)
C      IF(INDEX(KEYWRD,'POLAR') .NE. 0) THEN
C         CALL POLAR
C      ENDIF
C      IF(INDEX(KEYWRD,' ESP') .NE. 0)THEN
C  IF YOU WANT TO USE THE ESP PROGRAM, UNCOMMENT THE LINE
C  "C#      CALL ESP", ADD "ESP, " TO MOPAC.OPT, THEN COMPILE ESP AND
C  MNDO, AND RELINK.
C          CALL ESP
C      ENDIF
   50 TIM=SECOND()-TIME0
      LIMSCF=.FALSE.
      WRITE(6,'(///,'' TOTAL CPU TIME: '',F16.2,'' SECONDS'')') TIM
      WRITE(6,'(/,'' == MOPAC DONE =='')')
C      IF(ISOK) GOTO 10
      END
