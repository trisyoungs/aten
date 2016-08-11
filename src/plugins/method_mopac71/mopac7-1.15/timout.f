      SUBROUTINE TIMOUT(NOUT,TIM)
C
C     CONVERT THE TIME FROM SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION MINS, MINPHR
C
C
      DATA HRSPD /24.0D0/,    MINPHR /60.0D0/
      DATA SECPD /86400.0D0/, SECPMI /60.0D0/
C
      DAYS = TIM / SECPD
      IDAYS = INT(DAYS)
      HOURS = (DAYS - FLOAT(IDAYS)) * HRSPD
      IHOURS = INT(HOURS)
      MINS = (HOURS - FLOAT(IHOURS)) * MINPHR
      IMINS = INT(MINS)
      SECS = (MINS - FLOAT(IMINS)) * SECPMI
C
      IF (IDAYS .GT. 1) THEN
         WRITE (NOUT,10) IDAYS,IHOURS,IMINS,SECS
      ELSE IF (IDAYS .EQ. 1) THEN
         WRITE (NOUT,20) IDAYS,IHOURS,IMINS,SECS
      ELSE IF (IHOURS .GT. 0) THEN
         WRITE (NOUT,30) IHOURS,IMINS,SECS
      ELSE IF (IMINS .GT. 0) THEN
         WRITE (NOUT,40) IMINS,SECS
      ELSE
         WRITE (NOUT,50) SECS
      END IF
C
   10 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'DAYS',2X,I2,1X,'HOURS',
     1        1X,I2,1X,'MINUTES AND',1X,F7.3,1X,'SECONDS')
   20 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'DAY',2X,I2,1X,'HOURS',
     1        1X,I2,1X,'MINUTES AND',1X,F7.3,1X,'SECONDS')
   30 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'HOURS',
     1        1X,I2,1X,'MINUTES AND',1X,F7.3,1X,'SECONDS')
   40 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'MINUTES AND',
     1        1X,F7.3,1X,'SECONDS')
   50 FORMAT (10X,'COMPUTATION TIME = ',F7.3,1X,'SECONDS')
      END
      SUBROUTINE MPCPOP(C,ICOK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
C
C This subroutine calculates the total Mulliken populations on the
C   atoms by summing the diagonal elements from the  Mulliken
C   population analysis.
C
      COMMON / MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /CORE/ CORE(107)
      COMMON /OUTFIL/ WU
      INTEGER WU
      DIMENSION C(MORB2),POP(NUMATM),CHRG(NUMATM)
      WRITE(16,'(I4,5X'' MULLIKEN POPULATION AND CHARGE'')',ERR=40)ICOK
C
C ICOK = 1 ==> PRINT POPULATIONS
C ICOK = 0 ==> KEYWORD mulliken = .f.
C         NO POPULATION ANALYSIS PERFORMED
C
      IF (ICOK.NE.0) THEN
         DO 20 I = 1,NUMAT
            IF = NFIRST(I)
            IL = NLAST(I)
            SUM = 0.0D0
            POP(I) = 0.0D0
            CHRG(I) = 0.0D0
            DO 10 J = IF,IL
C
C    Diagonal element of mulliken matrix
C
               SUM = SUM + C((J*(J+1))/2)
   10       CONTINUE
            K = NAT(I)
C
C    Mulliken population for i'th atom
C
            POP(I) = SUM
            CHRG(I) = CORE(K) - POP(I)
   20    CONTINUE
         WRITE(WU,'(///10X,''MULLIKEN POPULATIONS AND CHARGES'')')
         DO 30 J = 1,NUMAT
            WRITE(WU,60) J, POP(J), CHRG(J)
            WRITE(16,70,ERR=40) POP(J), CHRG(J)
   30    CONTINUE
      ENDIF
      RETURN
   40 WRITE(WU,'(A)') 'Error writing SYBYL Mulliken population output'
      RETURN
   50 FORMAT(//,5X,'ATOM',8X,'POPULATION',6X,'CHARGE')
   60 FORMAT(5X,I4,4X,F11.6,6X,F11.6)
   70 FORMAT(2F12.6)
      END
C
C This subroutine writes out the optimized geometry and atomic charges
C   for a MOPAC run.
C
      SUBROUTINE MPCSYB(NUMAT,COORD,CHR,ICOK,EIGS,NCLOSE,FUNCT
     1                       ,EIONIS,KCHRGE,DIP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /OUTFIL/ WU
      INTEGER WU
      DIMENSION COORD(3, NUMAT), CHR(NUMAT),EIGS(MAXORB)
C  Write out the charge flag and number of atoms
      WRITE(16,'(2I4)', ERR=30) ICOK,NUMAT
C  Write out the coordinates and charges
      DO 10 I=1, NUMAT
         WRITE(16,'(4F12.6)', ERR=30) (COORD(J, I), J=1, 3), CHR(I)
   10 CONTINUE
      I1 = MAX(1,NCLOSE - 1)
      I2 = MIN(MAXORB,NCLOSE + 2)
C
C  Write out the 2 highest and 2 lowest orbital energies
C
      WRITE(16,20,ERR=30)(EIGS(J),J=I1,I2),NCLOSE
   20 FORMAT(4F12.6,2X,I4,2X,'HOMOs,LUMOs,# of occupied MOs')
C
C  Write out the Heat of Formation and Ionisation Potential
C
      WRITE(16,'(2F12.6,4X,''HF and IP'')',ERR=30) FUNCT,EIONIS
C
C  Write out the Dipole Moment
C
      IF(KCHRGE.NE.0) DIP = 0.0D0
      WRITE(16, '(I4,F10.3,''  Charge,Dipole Moment'')', ERR=30)
     1KCHRGE, DIP
      RETURN
   30 WRITE(WU,'(A)') 'Error writing SYBYL MOPAC output'
      RETURN
      END
