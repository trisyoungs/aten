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
      SUBROUTINE LM7START()
C *** this is the original MOPAC main program.
C *** since we already have a main program, we have changed it into a subroutine.
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
      CALL GETDAT
C
C   CLOSE UNIT 6 IN CASE IT WAS ALREADY PRE-ASSIGNED
C
C *** don't open unit 6; the output is done to STDOUT instead of file...
C          CLOSE (6)
C          OPEN(UNIT=6,FILE=GETNAM('FOR006'),STATUS='NEW')
C          REWIND 6
C#      CALL TIMER('FIRST LINE')
C *** update logic for NUMCAL and ISOK is changed; initialized using DATA commands above.
C      NUMCAL=0
C      ISOK=.TRUE.
   10 NUMCAL=NUMCAL+1
C
      TIME0=SECOND()
C
C READ AND CHECK INPUT FILE, EXIT IF NECESSARY.
C     WRITE INPUT FILE TO UNIT 6 AS FEEDBACK TO USER
C
   20 CALL READMO
      EMIN=0.D0
C#      CALL TIMER('AFTER READ')
C *** text outputs and jumps in code are removed here...
C      IF(NATOMS.EQ.0) GOTO 50
C      IF(INDEX(KEYWRD,'AUTHOR') .NE. 0) THEN
C         WRITE(6,'(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',
C     1/         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'')')
C         WRITE(6,'(10X,''     BY JAMES J. P. STEWART AT THE'',/
C     1         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
C     2         ,10X,''          AUSTIN, TEXAS, 78712'')')
C         WRITE(6,'(10X,''  MODIFIED TO DO ESP CALCULATIONS BY''
C     1          ,10X,''    BRENT H. BESLER AND K. M. MERZ JR. 1989'')')
C      ENDIF
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
      SUBROUTINE GETDAT
      CHARACTER*80 LINE, GETNAM
************************************************************************
*
*   GETDAT READS ALL THE DATA IN ON CHANNEL 2, AND WRITES IT TO SCRATCH
*   CHANNEL 5.  THIS WAY THE ORIGINAL DATA-SET IS FREED UP AS SOON AS
*   THE JOB STARTS.
*
************************************************************************
      SAVE I
      DATA I/0/
C#      WRITE(6,*)GETNAM('FOR005')
      OPEN(UNIT=2,FILE=GETNAM('FOR005'),STATUS='UNKNOWN')
C
C  CLOSE UNIT 5 IN CASE IT WAS ALREADY PRE-ASSIGNED.
C
      CLOSE (5)
      OPEN(UNIT=5,STATUS='SCRATCH')
      REWIND 5
      REWIND 2
   10 READ(2,'(A80)',END=20,ERR=20)LINE
      I=I+1
      WRITE(5,'(A80)')LINE
      GOTO 10
   20 REWIND 5
      IF(I.LT.3)THEN
         WRITE(6,'(A)')' INPUT FILE MISSING OR EMPTY'
         STOP
      ENDIF
      CLOSE (2)
      END
