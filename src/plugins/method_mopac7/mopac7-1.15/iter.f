      SUBROUTINE ITER  (H, W, WJ, WK, EE, FULSCF,RAND)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DOUBLE PRECISION MECI
      DIMENSION H(*), W(*), WJ(*), WK(*)
      COMMON /FOKMAT/ F(MPACK), FB(MPACK)
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /GRADNT/ DUMY(MAXPAR),GNORM
      COMMON /LAST  / LAST
      COMMON /MESAGE/ IFLEPO,IITER
      COMMON /ATHEAT/ ATHEAT
      COMMON /ENUCLR/ ENUCLR
      COMMON /CITERM/ XI,XJ,XK
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      COMMON /NUMCAL/ NUMCAL
      COMMON /SCFTYP/ EMIN, LIMSCF
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C     COMMON /TIME  / TIME0
      COMMON /TIMEC / TIME0
C ***************************** at 1994-05-25 *****
      LOGICAL FULSCF, RAND, LIMSCF
      DOUBLE PRECISION WJ, WK
C***********************************************************************
C
C     ITER GENERATES A SCF FIELD AND RETURNS THE ENERGY IN "ENERGY"
C
C THE MAIN ARRAYS USED IN ITER ARE:
C            P      ONLY EVER CONTAINS THE TOTAL DENSITY MATRIX
C            PA     ONLY EVER CONTAINS THE ALPHA DENSITY MATRIX
C            PB     ONLY EVER CONTAINS THE BETA DENSITY MATRIX
C            C      ONLY EVER CONTAINS THE EIGENVECTORS
C            H      ONLY EVER CONTAINS THE ONE-ELECTRON MATRIX
C            F      STARTS OFF CONTAINING THE ONE-ELECTRON MATRIX,
C                   AND IS USED TO HOLD THE FOCK MATRIX
C            W      ONLY EVER CONTAINS THE TWO-ELECTRON MATRIX
C
C THE MAIN INTEGERS CONSTANTS IN ITER ARE:
C
C            LINEAR SIZE OF PACKED TRIANGLE = NORBS*(NORBS+1)/2
C
C THE MAIN INTEGER VARIABLES ARE
C            NITER  NUMBER OF ITERATIONS EXECUTED
C
C  PRINCIPAL REFERENCES:
C
C   ON MNDO: "GROUND STATES OF MOLECULES. 38. THE MNDO METHOD.
C             APPROXIMATIONS AND PARAMETERS."
C             DEWAR, M.J.S., THIEL,W., J. AM. CHEM. SOC.,99,4899,(1977).
C   ON SHIFT: "THE DYNAMIC 'LEVEL SHIFT' METHOD FOR IMPROVING THE
C             CONVERGENCE OF THE SCF PROCEDURE", A. V. MITIN, J. COMP.
C             CHEM. 9, 107-110 (1988)
C   ON HALF-ELECTRON: "MINDO/3 COMPARISON OF THE GENERALIZED S.C.F.
C             COUPLING OPERATOR AND "HALF-ELECTRON" METHODS FOR
C             CALCULATING THE ENERGIES AND GEOMETRIES OF OPEN SHELL
C             SYSTEMS"
C             DEWAR, M.J.S., OLIVELLA, S., J. CHEM. SOC. FARA. II,
C             75,829,(1979).
C   ON PULAY'S CONVERGER: "CONVERGANCE ACCELERATION OF ITERATIVE
C             SEQUENCES. THE CASE OF SCF ITERATION", PULAY, P.,
C             CHEM. PHYS. LETT, 73, 393, (1980).
C   ON CNVG:  IT ENCORPORATES THE IMPROVED ITERATION SCHEME (IIS) BY
C             PIOTR BADZIAG & FRITZ SOLMS. ACCEPTED FOR PUBLISHING
C             IN COMPUTERS & CHEMISTRY
C   ON PSEUDODIAGONALISATION: "FAST SEMIEMPIRICAL CALCULATIONS",
C             STEWART. J.J.P., CSASZAR, P., PULAY, P., J. COMP. CHEM.,
C             3, 227, (1982)
C
C***********************************************************************
      DIMENSION POLD(MPACK), POLD2(MPACK), POLD3(MAXORB+400)
      DIMENSION PBOLD(MPACK), PBOLD2(MPACK), PBOLD3(MAXORB+400)
************************************************************************
*                                                                      *
*   PACK ALL THE ARRAYS USED BY PULAY INTO A COMMON BLOCK SO THAT THEY *
*   CAN BE USED BY THE C.I. DERIVATIVE, IF NEEDED                      *
*                                                                      *
************************************************************************
      COMMON /WORK3/POLD,POLD2,PBOLD,PBOLD2
      COMMON /WORK1/ AR1,AR2,AR3,AR4,BR1,BR2,BR3,BR4
      DIMENSION  AR1(2*NPULAY), AR2(2*NPULAY), AR3(2*NPULAY),
     1 AR4(2*NPULAY)
      DIMENSION  BR1(2*NPULAY), BR2(2*NPULAY), BR3(2*NPULAY),
     1 BR4(7*NPULAY)
      DIMENSION ESCF0(10)
      COMMON /PRECI / SELCON
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,
     2                NALPHA, NBETA, NCLOSE, NOPEN, NDUMY, FRACT
     3      /MOLORB/ DUMMY(MAXORB),PDIAG(MAXORB)
     4      /KEYWRD/ KEYWRD
     5      /NUMSCF/ NSCF
      SAVE ICALCN, DEBUG, PRTFOK, PRTEIG, PRTDEN,  PRT1EL, ABPRT
      SAVE LINEAR, MINPRT, NEWDG, SCFCRT, PRTPL, PRTVEC, PL
      SAVE BSHIFT, PLTEST, ITRMAX, NA2EL, NA1EL, NB2EL,NB1EL
      SAVE IFILL, CAMKIN, CI, OKPULY, UHF, SCF1, OKNEWD, TIMES
      SAVE FORCE, ALLCON, TRANS, HALFE, W1, W2, RANDOM
      CHARACTER KEYWRD*241, ABPRT(3)*5, GETNAM*80
      LOGICAL PRTFOK,PRTEIG,PRTDEN, DEBUG, TIMES, CI
     1,UHF, NEWDG, SCF1, HALFE, FORCE, PRT1EL,PRTPL, OKNEWD
     2,MINPRT, FRST, BFRST, OKPULY, READY, PRTVEC,
     3CAMKIN, ALLCON, MAKEA, MAKEB, INCITR, CAPPS, TIMITR
      DATA ICALCN/0/, DEBUG/.FALSE./, PRTFOK/.FALSE./
      DATA PRTEIG/.FALSE./,PRTDEN/.FALSE./
      DATA PRT1EL/.FALSE./
      DATA ABPRT/'     ','ALPHA',' BETA'/
C
C  INITIALIZE
C
      IFILL=0
      IHOMO=MAX(1,NCLOSE+NALPHA)
      IHOMOB=MAX(1,NCLOSE+NBETA)
      EOLD=1.D2
      READY=.FALSE.
      IF (ICALCN.NE.NUMCAL) THEN
         CALL EPSETA(EPS,ETA)
C
C  ULTIMATE SCF CRITERION: HEAT OF FORMATION CONVERGED WITHIN A FACTOR
C  OF 10 OF THE LIMITING PRECISION OF THE COMPUTER
C
         EPS=23.061D0*EPS*10.D0
         IRRR=5
         SHIFT=0.D0
         ICALCN=NUMCAL
         SHFMAX=20.D0
         LINEAR=(NORBS*(NORBS+1))/2
C
C    DEBUG KEY-WORDS WORKED OUT
C
         DEBUG=( INDEX(KEYWRD,'DEBUG') .NE. 0 )
         MINPRT=(INDEX(KEYWRD,'SADDLE')+
     1      LATOM .EQ.0 .OR. DEBUG)
         PRTEIG=( INDEX(KEYWRD,'EIGS') .NE. 0 )
         PRTPL =( INDEX(KEYWRD,' PL ')  .NE.0 )
         PRT1EL=( INDEX(KEYWRD,'1ELE') .NE.0 .AND. DEBUG)
         PRTDEN=( INDEX(KEYWRD,' DENS').NE.0 .AND. DEBUG)
         PRTFOK=( INDEX(KEYWRD,'FOCK') .NE. 0  .AND. DEBUG)
         PRTVEC=( INDEX(KEYWRD,'VECT') .NE. 0  .AND. DEBUG)
         DEBUG=( INDEX(KEYWRD,'ITER') .NE. 0 )
C
C INITIALIZE SOME LOGICALS AND CONSTANTS
C
         NEWDG =.FALSE.
         PLCHEK=0.005D0
         PL    =1.D0
         BSHIFT=0.D0
         SHIFT=1.D0
*
* SCFCRT IS MACHINE-PRECISION DEPENDENT
*
         SCFCRT=1.D-4
         ITRMAX = 200
         NA2EL=NCLOSE
         NA1EL=NALPHA+NOPEN
         NB2EL=0
         NB1EL=NBETA+NOPEN
C
C  USE KEY-WORDS TO ASSIGN VARIOUS CONSTANTS
C
         IF(INDEX(KEYWRD,'FILL').NE.0)
     1      IFILL=-READA(KEYWRD,INDEX(KEYWRD,'FILL'))
         IF(INDEX(KEYWRD,'SHIFT').NE.0)
     1      BSHIFT=-READA(KEYWRD,INDEX(KEYWRD,'SHIFT'))
         IF(BSHIFT.NE.0)TEN=BSHIFT
         IF(INDEX(KEYWRD,'ITRY').NE.0)
     1      ITRMAX=READA(KEYWRD,INDEX(KEYWRD,'ITRY'))
         CAMKIN=(INDEX(KEYWRD,'KING')+INDEX(KEYWRD,'CAMP') .NE. 0)
         CI=(INDEX(KEYWRD,'MICROS')+INDEX(KEYWRD,'C.I.') .NE. 0)
         OKPULY=.FALSE.
         OKPULY=(INDEX(KEYWRD,'PULAY').NE.0)
         UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
         SCF1=(INDEX(KEYWRD,'1SCF') .NE. 0)
         OKNEWD=ABS(BSHIFT) .LT. 0.001D0
         IF(CAMKIN.AND.ABS(BSHIFT).GT.1.D-5) BSHIFT=4.44D0
         TIMES=(INDEX(KEYWRD,'TIMES') .NE. 0)
         TIMITR=(TIMES.AND.DEBUG)
         FORCE=(INDEX(KEYWRD,'FORCE') .NE. 0)
         ALLCON=(OKPULY.OR.CAMKIN)
C
C   DO WE NEED A CAPPED ATOM CORRECTION?
C
         J=0
         DO 10 I=1,NUMAT
   10    IF(NAT(I).EQ.102)J=J+1
         CAPPS=(J.GT.0)
         IITER=1
         TRANS=0.1D0
         IF(INDEX(KEYWRD,'RESTART')+INDEX(KEYWRD,'OLDENS')
     1      .NE. 0) THEN
            IF(INDEX(KEYWRD,'OLDENS').NE.0)
     1   OPEN(UNIT=10,FILE=GETNAM('FOR010'),
     +        STATUS='UNKNOWN',FORM='UNFORMATTED')
            REWIND 10
            READ(10)(PA(I),I=1,LINEAR)
            IF( UHF) THEN
               READ(10)(PB(I),I=1,LINEAR)
               DO 20 I=1,LINEAR
                  POLD(I)=PA(I)
                  PBOLD(I)=PB(I)
   20          P(I)=PA(I)+PB(I)
            ELSE
               DO 30 I=1,LINEAR
                  PB(I)=PA(I)
                  PBOLD(I)=PA(I)
                  POLD(I)=PA(I)
   30          P(I)=PA(I)*2.D0
            ENDIF
         ELSE
            NSCF=0
            DO 40 I=1,LINEAR
               P(I)=0.D0
               PA(I)=0.D0
   40       PB(I)=0.D0
            W1=NA1EL/(NA1EL+1.D-6+NB1EL)
            W2=1.D0-W1
            IF(W1.LT.1.D-6)W1=0.5D0
            IF(W2.LT.1.D-6)W2=0.5D0
C
C  SLIGHTLY PERTURB THE DENSITY MATRIX IN CASE THE SYSTEM IS
C  TRAPPED IN A S**2 = 0 STATE.
C
            RANDOM=1.0D0
            IF(UHF.AND.NA1EL.EQ.NB1EL) RANDOM=1.1D0
            DO 50 I=1,NORBS
               J=(I*(I+1))/2
               P(J)=PDIAG(I)
               PA(J)=P(J)*W1*RANDOM
               RANDOM=1.D0/RANDOM
   50       PB(J)=P(J)*W2*RANDOM
            DO 60 I=1,LINEAR
               PBOLD(I)=PB(I)
   60       POLD(I)=PA(I)
         ENDIF
         HALFE=(NOPEN .NE. NCLOSE.AND.FRACT.NE.2.D0.AND.FRACT.NE.0.D0)
C
C   DETERMINE THE SELF-CONSISTENCY CRITERION
C
         IF(INDEX(KEYWRD,'PREC') .NE. 0)
     1                               SCFCRT=SCFCRT*0.01D0
         IF( INDEX(KEYWRD,'POLAR') + INDEX(KEYWRD,'NLLSQ') +
     1 INDEX(KEYWRD,'SIGMA') .NE. 0) SCFCRT=SCFCRT*0.001D0
         IF(FORCE)                   SCFCRT=SCFCRT*0.0001D0
         IF(NOPEN-NCLOSE.GT.4)       SCFCRT=SCFCRT*0.1D0
         SCFCRT=MAX(SCFCRT,1.D-12)
         IF(INDEX(KEYWRD,'POLAR').NE.0)SCFCRT=1.D-11
C
C  THE USER CAN STATE THE SCF CRITERION, IF DESIRED.
C
         I=INDEX(KEYWRD,'SCFCRT')
         IF(I.NE.0) THEN
            SCFCRT=READA(KEYWRD,I)
            WRITE(6,'(''  SCF CRITERION ='',G14.4)')SCFCRT
            IF(SCFCRT.LT.1.D-12)
     1 WRITE(6,'(//2X,'' THERE IS A RISK OF INFINITE LOOPING WITH'',
     2'' THE SCFCRT LESS THAN 1.D-12'')')
         ELSE
            IF(DEBUG)WRITE(6,'(''  SCF CRITERION ='',G14.4)')SCFCRT
         ENDIF
         IF(.NOT.SCF1)LAST=0
C
C   END OF INITIALIZATION SECTION.
C
      ELSEIF(FORCE.AND.NSCF.GT.0.AND..NOT.UHF)THEN
C
C   RESET THE DENSITY MATRIX IF MECI HAS FORMED AN EXCITED STATE.  THIS
C   PREVENTS THE SCF GETTING TRAPPED ON AN EXCITED STATE, PARTICULARLY
C   IF THE PULAY CONVERGER IS USED.
C
         DO 70 I=1,LINEAR
   70    P(I)=2.D0*PA(I)
      ENDIF
C
C   INITIALIZATION OPERATIONS DONE EVERY TIME ITER IS CALLED
C
      MAKEA=.TRUE.
      MAKEB=.TRUE.
      IEMIN=0
      IEMAX=0
C
C  TURN OFF SHIFT IF NOT A FULL SCF.
C
      IF(.NOT.FULSCF) SHIFT=0.D0
      IF(NEWDG) NEWDG=(ABS(BSHIFT).LT.0.001D0)
      IF(LAST.EQ.1) NEWDG=.FALSE.
C
C   SELF-CONSISTENCY CRITERIA: SELCON IS IN KCAL/MOL, PLTEST IS
C   A LESS IMPORTANT TEST TO MAKE SURE THAT THE SELCON TEST IS NOT
C   PASSED 'BY ACCIDENT'
C                              IF GNORM IS LARGE, MAKE SELCON BIGGER
C
      SELCON=SCFCRT
      IF(.NOT. FORCE .AND. .NOT. HALFE) THEN
         IF(GNORM.GT.5.D0) SELCON=SCFCRT*GNORM*0.2D0
         IF(GNORM.GT.200.D0) SELCON=SCFCRT*40.D0
      ENDIF
      PLTEST=0.05D0*SQRT(SELCON)
C
C  SOMETIMES HEAT GOES SCF BUT DENSITY IS STILL FLUCTUATING IN UHF
C  IN WHICH CASE PAY LESS ATTENTION TO DENSITY MATRIX
C
      IF(NALPHA.NE.NBETA.AND.UHF)PLTEST=0.001D0
      IF(DEBUG)THEN
         WRITE(6,'(''  SELCON, PLTEST'',3G16.7)')SELCON, PLTEST
      ENDIF
      IF(PRT1EL) THEN
         WRITE(6,'(//10X,''ONE-ELECTRON MATRIX AT ENTRANCE TO ITER'')')
         CALL VECPRT(H,NORBS)
      ENDIF
      IREDY=1
   80 NITER=0
      FRST=.TRUE.
      IF(CAMKIN) THEN
         MODEA=1
         MODEB=1
      ELSE
         MODEA=0
         MODEB=0
      ENDIF
      BFRST=.TRUE.
**********************************************************************
*                                                                    *
*                                                                    *
*                START THE SCF LOOP HERE                             *
*                                                                    *
*                                                                    *
**********************************************************************
      INCITR=.TRUE.
   90 INCITR=(MODEA.NE.3.AND.MODEB.NE.3)
      IF(INCITR)NITER=NITER+1
      IF(TIMITR)THEN
         TITER=SECOND()
         WRITE(6,*)
         WRITE(6,'(A,F7.2)')'     TIME FOR ITERATION:', TITER-TITER0
         TITER0=TITER
      ENDIF
      IF(NITER.GT.ITRMAX-10.AND..NOT.ALLCON) THEN
************************************************************************
*                                                                      *
*                   SWITCH ON ALL CONVERGERS                           *
*                                                                      *
************************************************************************
         WRITE(6,'(//,'' ALL CONVERGERS ARE NOW FORCED ON'',/
     1          '' SHIFT=10, PULAY ON, CAMP-KING ON'',/
     2          '' AND ITERATION COUNTER RESET'',//)')
         ALLCON=.TRUE.
         BSHIFT=4.44D0
         IREDY=-4
         EOLD=100.D0
         OKPULY=.TRUE.
         NEWDG=.FALSE.
         CAMKIN=(.NOT.HALFE)
         GOTO 80
      ENDIF
************************************************************************
*                                                                      *
*                        MAKE THE ALPHA FOCK MATRIX                    *
*                                                                      *
************************************************************************
      IF(ABS(SHIFT).GT.1.D-10.AND.BSHIFT .NE. 0.D0) THEN
         L=0
         IF(NITER.GT.1)THEN
            IF(NEWDG.AND..NOT.(HALFE.OR.CAMKIN))THEN
C
C  SHIFT WILL APPLY TO THE VIRTUAL ENERGY LEVELS USED IN THE
C  PSEUDODIAGONALIIZATION. IF DIFF IS -VE, GOOD, THEN LOWER THE
C  HOMO-LUMO GAP BY 0.1EV, OTHERWISE INCREASE IT.
               IF(DIFF.GT.0)THEN
                  SHIFT=1.D0
C
C IF THE PSEUDODIAGONALIZATION APPROXIMATION -- THAT THE WAVEFUNCTION
C IS ALMOST STABLE -- IS INVALID, TURN OFF NEWDG
                  IF(DIFF.GT.1)NEWDG=.FALSE.
               ELSE
                  SHIFT=-0.1D0
               ENDIF
            ELSE
               SHIFT=TEN+EIGS(IHOMO+1)-EIGS(IHOMO)+SHIFT
            ENDIF
            IF(DIFF.GT.0.D0) THEN
               IF(SHIFT.GT.4.D0)SHFMAX=4.5D0
               IF(SHIFT.GT.SHFMAX)SHFMAX=MAX(SHFMAX-0.5D0,0.D0)
            ENDIF
C
C   IF SYSTEM GOES UNSTABLE, LIMIT SHIFT TO THE RANGE -INFINITY - SHFMAX
C   BUT IF SYSTEM IS STABLE, LIMIT SHIFT TO THE RANGE -INFINITY - +20
C
            SHIFT=MAX(-20.D0,MIN(SHFMAX,SHIFT))
            IF(ABS(SHIFT-SHFMAX).LT.1.D-5)SHFMAX=SHFMAX+0.01D0
C
C  THE CAMP-KING AND PULAY CONVERGES NEED A CONSTANT SHIFT.
C  IF THE SHIFT IS ALLOWED TO VARY, THESE CONVERGERS WILL NOT
C  WORK PROPERLY.
C
            IF(OKPULY.OR.ABS(BSHIFT-4.44D0).LT.1.D-5)THEN
               SHIFT=-8.D0
               IF(NEWDG) SHIFT=0
            ENDIF
            IF(UHF)THEN
               IF(NEWDG.AND..NOT.(HALFE.OR.CAMKIN))THEN
                  SHIFTB=TEN-TENOLD
               ELSE
                  SHIFTB=TEN+EIGS(IHOMOB+1)-EIGS(IHOMOB)+SHIFTB
               ENDIF
               IF(DIFF.GT.0.D0)SHIFTB=MIN(4.D0,SHIFTB)
               SHIFTB=MAX(-20.D0,MIN(SHFMAX,SHIFTB))
               IF(OKPULY.OR.ABS(BSHIFT-4.44D0).LT.1.D-5)THEN
                  SHIFTB=-8.D0
                  IF(NEWDG)SHIFT=0
               ENDIF
               DO 100 I=IHOMOB+1,NORBS
  100          EIGB(I)=EIGB(I)+SHIFTB
            ELSE
            ENDIF
         ENDIF
         TENOLD=TEN
         IF(PL.GT.PLCHEK)THEN
            SHFTBO=SHIFTB
            SHFTO=SHIFT
         ELSE
            SHIFTB=SHFTBO
            SHIFT=SHFTO
         ENDIF
         DO 110 I=IHOMO+1,NORBS
  110    EIGS(I)=EIGS(I)+SHIFT
         DO 130 I=1,NORBS
            DO 120 J=1,I
               L=L+1
  120       F(L)=H(L)+SHIFT*PA(L)
  130    F(L)=F(L)-SHIFT
      ELSEIF (I.EQ.77.AND.LAST.EQ.0.AND.NITER.LT.2.AND.FULSCF)THEN
C
C  SLIGHTLY PERTURB THE FOCK MATRIX IN CASE THE SYSTEM IS
C  TRAPPED IN A METASTABLE EXCITED ELECTRONIC STATE
C
         RANDOM=0.001D0
         DO 140 I=1,LINEAR
            RANDOM=-RANDOM
  140    F(I)=H(I)+RANDOM
      ELSE
         DO 150 I=1,LINEAR
  150    F(I)=H(I)
      ENDIF
  160 CONTINUE
      IF(TIMITR)THEN
         T0=SECOND()
         WRITE(6,'(A,F7.2)')' LOAD FOCK MAT. INTEGRAL',T0-TITER0
      ENDIF
C#      CALL TIMER('BEFORE FOCK2')
      CALL FOCK2(F,P,PA,W, WJ, WK,NUMAT,NAT,NFIRST,NMIDLE,NLAST)
C#      CALL TIMER('AFTER FOCK2')
C#      CALL TIMER('BEFORE FOCK1')
      CALL FOCK1(F,P,PA,PB)
C#      CALL TIMER('AFTER FOCK1')
      IF(TIMITR)THEN
         T0=SECOND()
         TF1=TF1+T0-T1
         WRITE(6,'(2(A,F7.2))')'  FOCK1:',T0-T1,'INTEGRAL:',T0-TITER0
      ENDIF
************************************************************************
*                                                                      *
*                        MAKE THE BETA FOCK MATRIX                     *
*                                                                      *
************************************************************************
      IF (UHF) THEN
         IF(SHIFTB .NE. 0.D0) THEN
            L=0
            DO 180 I=1,NORBS
               DO 170 J=1,I
                  L=L+1
  170          FB(L)=H(L)+SHIFTB*PB(L)
  180       FB(L)=FB(L)-SHIFTB
         ELSEIF (RAND.AND.LAST.EQ.0.AND.NITER.LT.2.AND.FULSCF)THEN
            RANDOM=0.001D0
            DO 190 I=1,LINEAR
               RANDOM=-RANDOM
  190       FB(I)=H(I)+RANDOM
         ELSE
            DO 200 I=1,LINEAR
  200       FB(I)=H(I)
         ENDIF
         CALL FOCK2(FB,P,PB,W, WJ, WK,NUMAT,NAT,NFIRST,NMIDLE,NLAST)
         CALL FOCK1(FB,P,PB,PA)
      ENDIF
      IF( .NOT. FULSCF) GOTO 380
      IF(PRTFOK) THEN
         WRITE(6,210)NITER
  210    FORMAT('   FOCK MATRIX ON ITERATION',I3)
         CALL VECPRT (F,NORBS)
      ENDIF
C
C   CODE THE FOLLOWING LINE IN PROPERLY SOMETIME
C   THIS OPERATION IS BELIEVED TO GIVE RISE TO A BETTER FOCK MATRIX
C   THAN THE CONVENTIONAL GUESS.
C
      IF(IRRR.EQ.0)THEN
         DO 220 I=1,NORBS
  220    F((I*(I+1))/2)=F((I*(I+1))/2)*0.5D0
         IRRR=2
      ENDIF
************************************************************************
*                                                                      *
*                        CALCULATE THE ENERGY IN KCAL/MOLE             *
*                                                                      *
************************************************************************
      IF (NITER .GE. ITRMAX) THEN
         IF(DIFF.LT.1.D-3.AND.PL.LT.1.D-4.AND..NOT.FORCE)THEN
            WRITE(6,'('' """""""""""""""UNABLE TO ACHIEVE SELF-CONSISTEN
     1CE, JOB CONTINUING'')')
            GOTO 380
         ENDIF
         IF(MINPRT)WRITE (6,230)
  230    FORMAT (//10X,'"""""""""""""UNABLE TO ACHIEVE SELF-CONSISTENCE'
     1,/)
         WRITE (6,240) DIFF,PL
  240    FORMAT (//,10X,'DELTAE= ',E12.4,5X,'DELTAP= ',E12.4,///)
C *** here we failed to calculate a valid energy, but we don't want to close the whole program either.
C *** instead of calling STOP, continue like in the above case where GOTO 380 is called...
         GOTO 380
C         IFLEPO=9
C         IITER=2
C         CALL WRITMO(TIME0,ESCF)
C         STOP
      ENDIF
      EE=HELECT(NORBS,PA,H,F)
      IF(UHF)THEN
         EE=EE+HELECT(NORBS,PB,H,FB)
      ELSE
         EE=EE*2.D0
      ENDIF
      IF(CAPPS)EE=EE+CAPCOR(NAT,NFIRST,NLAST,NUMAT,P,H)
      IF(BSHIFT.NE.0.D0)
     1SCORR=SHIFT*(NOPEN-NCLOSE)*23.061D0*0.25D0*(FRACT*(2.D0-FRACT))
      ESCF=(EE+ENUCLR)*23.061D0+ATHEAT+SCORR
      IF(INCITR)THEN
         DIFF=ESCF-EOLD
         IF(DIFF.GT.0)THEN
            TEN=TEN-1.D0
         ELSE
            TEN=TEN*0.975D0+0.05D0
         ENDIF
C
C MAKE SURE SELF-CONSISTENCY TEST IS NOT MORE STRINGENT THAN THE
C COMPUTER CAN HANDLE
C
         SELLIM=MAX(SELCON,EPS*MAX(ABS(EE),1.D0))
C
C SCF TEST:  CHANGE IN HEAT OF FORMATION IN KCAL/MOL SHOULD BE
C            LESS THAN SELLIM.  THE OTHER TESTS ARE SAFETY MEASURES
C
         IF(.NOT.(NITER.GT.4.AND.(PL.EQ.0.D0.OR.PL.LT.PLTEST.AND.
     1   ABS(DIFF).LT.SELLIM) .AND. READY)) GOTO 270
************************************************************************
*                                                                      *
*          SELF-CONSISTENCY TEST, EXIT MODE FROM ITERATIONS            *
*                                                                      *
************************************************************************
  250    IF (ABS(SHIFT) .LT. 1.D-10) GOTO 380
         SHIFT=0.D0
         SHIFTB=0.D0
         DO 260 I=1,LINEAR
  260    F(I)=H(I)
         MAKEA=.TRUE.
         MAKEB=.TRUE.
         GOTO 160
  270    CONTINUE
***********************************************************************
***********************************************************************
         IF(LIMSCF.AND.EMIN.NE.0.D0.AND..NOT.(CI.OR.HALFE))THEN
C
C  THE FOLLOWING TESTS ARE INTENDED TO ALLOW A FAST EXIT FROM ITER
C  IF THE RESULT IS 'GOOD ENOUGH' FOR THE CURRENT STEP IN THE GEOMETRY
C  OPTIMIZATION
C
            IF(ESCF.LT.EMIN)THEN
C
C  THE ENERGY IS LOWER THAN THE PREVIOUS MINIMUM.  NOW CHECK THAT
C  IT IS CONSISTENTLY LOWER.
C
               IEMAX=0
               IEMIN=MIN(5,IEMIN+1)
               DO 280 I=2,IEMIN
  280          ESCF0(I-1)=ESCF0(I)
               ESCF0(IEMIN)=ESCF
C
C  IS THE DIFFERENCE IN ENERGY BETWEEN TWO ITERATIONS LESS THAN 5%
C  OF THE ENERGY GAIN FOR THIS GEOMETRY RELATIVE TO THE PREVIOUS
C  MINIMUM.
C
               IF(IEMIN.GT.3)THEN
                  DO 290 I=2,IEMIN
                     IF(ABS(ESCF0(I)-ESCF0(I-1)).GT.0.05D0*(EMIN-ESCF))
     1GOTO 320
  290             CONTINUE
C
C IS GOOD ENOUGH -- RAPID EXIT
C
                  IF(DEBUG) WRITE(6,*)
     1' RAPID EXIT BECAUSE ENERGY IS CONSISTENTLY LOWER'
                  GOTO 250
               ENDIF
            ELSE
C
C  THE ENERGY HAS RISEN ABOVE THAT OF THE PREVIOUS MINIMUM.
C  WE NEED TO CHECK WHETHER THIS IS A FLUKE OR IS THIS REALLY
C  A BAD GEOMETRY.
C
               IEMIN=0
               IEMAX=MIN(5,IEMAX+1)
               DO 300 I=2,IEMAX
  300          ESCF0(I-1)=ESCF0(I)
               ESCF0(IEMAX)=ESCF
C
C  IS THE DIFFERENCE IN ENERGY BETWEEN TWO ITERATIONS LESS THAN 5%
C  OF THE ENERGY LOST FOR THIS GEOMETRY RELATIVE TO THE PREVIOUS
C  MINIMUM.
C
               IF(IEMAX.GT.3)THEN
                  DO 310 I=2,IEMAX
                     IF(ABS(ESCF0(I)-ESCF0(I-1)).GT.0.05D0*(ESCF-EMIN))
     1GOTO 320
  310             CONTINUE
C
C IS GOOD ENOUGH -- RAPID EXIT
C
                  IF(DEBUG) WRITE(6,*)
     1' RAPID EXIT BECAUSE ENERGY IS CONSISTENTLY HIGHER'
                  GOTO 250
               ENDIF
            ENDIF
         ENDIF
  320    READY=(IREDY.GT.0.AND.(ABS(DIFF).LT.SELLIM*10.D0.OR.PL.EQ.0.D0)
     1)
         IREDY=IREDY+1
      ENDIF
      IF(PRTPL.OR.DEBUG.AND.NITER.GT.ITRMAX-20) THEN
         IF(ABS(ESCF).GT.99999.D0) ESCF=SIGN(9999.D0,ESCF)
         IF(ABS(DIFF).GT.9999.D0)DIFF=0.D0
         IF(INCITR)
     1    WRITE(6,'('' ITERATION'',I3,'' PLS='',2E10.3,'' ENERGY  '',
     2F14.7,'' DELTAE'',F13.7)')NITER,PL,PLB,ESCF,DIFF
      close (6)
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C      OPEN(UNIT=6,FILE=GETNAM('FOR006'),ACCESS='APPEND')
C *** exactly why do we want to open unit 6??? it's already open?!?!?!
C *** we also remove this because we want use STDOUT for output...
C      OPEN(UNIT=6,FILE=GETNAM('FOR006'))
C 9990 read (6,'()',end=9999)
C         goto 9990
C 9999 continue
C ***************************** at 1994-05-25 *****
      ENDIF
      IF(INCITR)EOLD=ESCF
************************************************************************
*                                                                      *
*                        INVOKE THE CAMP-KING CONVERGER                *
*                                                                      *
************************************************************************
      IF(NITER.GT.2 .AND. CAMKIN .AND. MAKEA)
     1CALL INTERP(NORBS,NA1EL,NORBS-NA1EL, MODEA, ESCF/23.061D0,
     2F, C, AR1, AR2, AR3, AR4, AR1)
      MAKEB=.FALSE.
      IF(MODEA.EQ.3)GOTO 340
      MAKEB=.TRUE.
      IF(TIMITR)THEN
         T0=SECOND()
         WRITE(6,'(2(A,F7.2))')' ADJUST DAMPER  INTEGRAL',T0-TITER0
      ENDIF
      IF( NEWDG ) THEN
************************************************************************
*                                                                      *
*                        INVOKE PULAY'S CONVERGER                      *
*                                                                      *
************************************************************************
         IF(OKPULY.AND.MAKEA.AND.IREDY.GT.1)
     1CALL PULAY(F,PA,NORBS,POLD,POLD2,POLD3,JALP,IALP,MPACK,FRST,PL)
************************************************************************
*                                                                      *
*           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
* WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
*                                                                      *
************************************************************************
         IF (HALFE.OR.CAMKIN) THEN
            CALL HQRII(F,NORBS,NORBS,EIGS,C)
         ELSE
C#      CALL TIMER('BEFORE DIAG')
            CALL DIAG (F,C,NA1EL,EIGS,NORBS,NORBS)
C#      CALL TIMER('AFTER DIAG')
         ENDIF
      ELSE
C#      CALL TIMER('BEFORE HQRII')
         CALL HQRII(F,NORBS,NORBS,EIGS,C)
C#      CALL TIMER('AFTER HQRII')
         IF(TIMITR)THEN
            T1=SECOND()
            WRITE(6,'(2(A,F7.2))')'  HQRII:',T1-T0,' INTEGRAL',T1-TITER0
         ENDIF
      ENDIF
      J=1
      IF(PRTVEC) THEN
         J=1
         IF(UHF)J=2
         WRITE(6,'(//10X,A,
     1'' EIGENVECTORS AND EIGENVALUES ON ITERATION'',I3)')
     2   ABPRT(J),NITER
         CALL MATOUT(C,EIGS,NORBS,NORBS,NORBS)
      ELSE
         IF (PRTEIG) WRITE(6,330)ABPRT(J),NITER,(EIGS(I),I=1,NORBS)
      ENDIF
  330 FORMAT(10X,A,'  EIGENVALUES ON ITERATION',I3,/10(6G13.6,/))
  340 IF(IFILL.NE.0)CALL SWAP(C,NORBS,NORBS,NA2EL,IFILL)
************************************************************************
*                                                                      *
*            CALCULATE THE ALPHA OR RHF DENSITY MATRIX                 *
*                                                                      *
************************************************************************
      IF(UHF)THEN
         CALL DENSIT( C,NORBS, NORBS, NA2EL,NA1EL, FRACT, PA, 1)
         IF(MODEA.NE.3.AND..NOT. (NEWDG.AND.OKPULY))
     1    CALL CNVG(PA, POLD, POLD2, NORBS, NITER, PL)
      ELSE
C#      CALL TIMER('BEFORE DENSIT')
         CALL DENSIT( C,NORBS, NORBS, NA2EL,NA1EL, FRACT, P, 1)
C#      CALL TIMER('AFTER DENSIT')
         IF(MODEA.NE.3.AND..NOT. (NEWDG.AND.OKPULY))THEN
C#      CALL TIMER('BEFORE CNVG')
            CALL CNVG(P, POLD, POLD2, NORBS, NITER, PL)
C#      CALL TIMER('AFTER CNVG')
         ENDIF
      ENDIF
************************************************************************
*                                                                      *
*                       UHF-SPECIFIC CODE                              *
*                                                                      *
************************************************************************
      IF( UHF )THEN
************************************************************************
*                                                                      *
*                        INVOKE THE CAMP-KING CONVERGER                *
*                                                                      *
************************************************************************
         IF(NITER.GT.2 .AND. CAMKIN .AND. MAKEB )
     1CALL INTERP(NORBS,NB1EL,NORBS-NB1EL, MODEB, ESCF/23.061D0,
     2FB, CBETA, BR1, BR2, BR3, BR4, BR1)
         MAKEA=.FALSE.
         IF(MODEB.EQ.3) GOTO 350
         MAKEA=.TRUE.
         IF( NEWDG ) THEN
************************************************************************
*                                                                      *
*                        INVOKE PULAY'S CONVERGER                      *
*                                                                      *
************************************************************************
            IF( OKPULY.AND.MAKEB.AND.IREDY.GT.1)
     1CALL PULAY(FB,PB,NORBS,PBOLD,PBOLD2,
     2PBOLD3,JBET,IBET,MPACK,BFRST,PLB)
************************************************************************
*                                                                      *
*           DIAGONALIZE THE ALPHA OR RHF SECULAR DETERMINANT           *
* WHERE POSSIBLE, USE THE PULAY-STEWART METHOD, OTHERWISE USE BEPPU'S  *
*                                                                      *
************************************************************************
            IF (HALFE.OR.CAMKIN) THEN
               CALL HQRII(FB,NORBS,NORBS,EIGB,CBETA)
            ELSE
               CALL DIAG (FB,CBETA,NB1EL,EIGB,NORBS,NORBS)
            ENDIF
         ELSE
            CALL HQRII(FB,NORBS,NORBS,EIGB,CBETA)
         ENDIF
         IF(PRTVEC) THEN
            WRITE(6,'(//10X,A,'' EIGENVECTORS AND EIGENVALUES ON '',
     1''ITERATION'',I3)')ABPRT(3),NITER
            CALL MATOUT(CBETA,EIGB,NORBS,NORBS,NORBS)
         ELSE
            IF (PRTEIG) WRITE(6,330)ABPRT(3),NITER,(EIGB(I),I=1,NORBS)
         ENDIF
************************************************************************
*                                                                      *
*                CALCULATE THE BETA DENSITY MATRIX                     *
*                                                                      *
************************************************************************
  350    CALL DENSIT( CBETA,NORBS, NORBS, NB2EL, NB1EL, FRACT, PB, 1)
         IF( .NOT. (NEWDG.AND.OKPULY))
     1CALL CNVG(PB, PBOLD, PBOLD2, NORBS, NITER, PLB)
      ENDIF
************************************************************************
*                                                                      *
*                   CALCULATE THE TOTAL DENSITY MATRIX                 *
*                                                                      *
************************************************************************
      IF(UHF) THEN
         DO 360 I=1,LINEAR
  360    P(I)=PA(I)+PB(I)
      ELSE
         DO 370 I=1,LINEAR
            PA(I)=P(I)*0.5D0
  370    PB(I)=PA(I)
      ENDIF
      IF(PRTDEN) THEN
         WRITE(6,'('' DENSITY MATRIX ON ITERATION'',I4)')NITER
         CALL VECPRT (P,NORBS)
      ENDIF
      OKNEWD=(PL.LT.SELLIM .OR. OKNEWD)
      NEWDG=(PL.LT.TRANS .AND. OKNEWD .OR. NEWDG)
      IF(PL.LT.TRANS*0.3333D0)OKNEWD=.TRUE.
      GO TO 90
**********************************************************************
*                                                                    *
*                                                                    *
*                      END THE SCF LOOP HERE                         *
*                NOW CALCULATE THE ELECTRONIC ENERGY                 *
*                                                                    *
*                                                                    *
**********************************************************************
*          SELF-CONSISTENCE ACHEIVED.
*
  380 EE=HELECT(NORBS,PA,H,F)
      IF(UHF) THEN
         EE=EE+HELECT(NORBS,PB,H,FB)
      ELSE
         EE=EE*2.D0 +
     1SHIFT*(NOPEN-NCLOSE)*23.061D0*0.25D0*(FRACT*(2.D0-FRACT))
      ENDIF
      IF(CAPPS)EE=EE+CAPCOR(NAT,NFIRST,NLAST,NUMAT,P,H)
C
C   NORMALLY THE EIGENVALUES ARE INCORRECT BECAUSE THE
C   PSEUDODIAGONALIZATION HAS BEEN USED.  IF THIS
C   IS THE LAST SCF, THEN DO AN EXACT DIAGONALIZATION
      IF( NSCF.EQ.0 .OR. LAST.EQ.1 .OR. CI .OR. HALFE ) THEN
C
C  PUT F AND FB INTO POLD IN ORDER TO NOT DESTROY F AND FB
C  AND DO EXACT DIAGONALISATIONS
         DO 390 I=1,LINEAR
  390    POLD(I)=F(I)
         CALL HQRII(POLD,NORBS,NORBS,EIGS,C)
         IF(UHF) THEN
            DO 400 I=1,LINEAR
  400       POLD(I)=FB(I)
            CALL HQRII(POLD,NORBS,NORBS,EIGB,CBETA)
            DO 410 I=1,LINEAR
  410       POLD(I)=PA(I)
         ELSE
            DO 420 I=1,LINEAR
  420       POLD(I)=P(I)
         ENDIF
         IF(CI.OR.HALFE) THEN
C#        CALL TIMER('BEFORE MECI')
            SUM=MECI(EIGS,C)
C#        CALL TIMER('AFTER MECI')
            EE=EE+SUM
            IF(PRTPL)THEN
               ESCF=(EE+ENUCLR)*23.061D0+ATHEAT
               WRITE(6,'(27X,''AFTER MECI, ENERGY  '',F14.7)')ESCF
            ENDIF
         ENDIF
      ENDIF
      NSCF=NSCF+1
      IF(DEBUG)WRITE(6,'('' NO. OF ITERATIONS ='',I6)')NITER
C            IF(FORCE)  SCFCRT=1.D-5
      IF(ALLCON.AND.ABS(BSHIFT-4.44D0).LT.1.D-7)THEN
         CAMKIN=.FALSE.
         ALLCON=.FALSE.
         NEWDG=.FALSE.
         BSHIFT=-10.D0
         OKPULY=.FALSE.
      ENDIF
      SHIFT=1.D0
      IF(EMIN.EQ.0.D0)THEN
         EMIN=ESCF
      ELSE
         EMIN=MIN(EMIN,ESCF)
      ENDIF
      RETURN
      END
