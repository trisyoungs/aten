      SUBROUTINE CNVG(PNEW, P, P1,NORBS, NITER, PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(*), P(*), PNEW(*)
      LOGICAL EXTRAP
C***********************************************************************
C
C  CNVG IS A TWO-POINT INTERPOLATION ROUTINE FOR SPEEDING CONVERGENCE
C       OF THE DENSITY MATRIX.
C
C ON OUTPUT P      = NEW DENSITY MATRIX
C           P1     = DIAGONAL OF OLD DENSITY MATRIX
C           PL     = LARGEST DIFFERENCE BETWEEN OLD AND NEW DENSITY
C                    MATRIX DIAGONAL ELEMENTS
C***********************************************************************
      COMMON/KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      SAVE RHFUHF
      CHARACTER*241 KEYWRD
      DATA ICALCN/0/
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         IF(INDEX(KEYWRD,'UHF').NE.0)THEN
            RHFUHF=1.D0
         ELSE
            RHFUHF=2.D0
         ENDIF
      ENDIF
      PL=0.0D00
      FACA=0.0D00
      DAMP=1.D10
      IF(NITER.GT.3)DAMP=0.05D0
      FACB=0.0D00
      FAC=0.0D00
      II=MOD(NITER,3)
      EXTRAP=II.NE.0
      SUM1=0.D0
      K=0
      DO 20 I=1,NORBS
         K=K+I
         A=PNEW(K)
         SUM1=SUM1+A
         SA=ABS(A-P(K))
         IF (SA.GT.PL) PL=SA
         IF (EXTRAP) GO TO 10
         FACA=FACA+SA**2
         FACB=FACB+(A-2.D00*P(K)+P1(I))**2
   10    P1(I)=P(K)
   20 P(K)=A
      IF (FACB.LE.0.0D00) GO TO 30
      IF (FACA.LT.(100.D00*FACB)) FAC=SQRT(FACA/FACB)
   30 IE=0
      SUM2=0.D0
      DO 50 I=1,NORBS
         II=I-1
         DO 40 J=1,II
            IE=IE+1
            A=PNEW(IE)
            P(IE)=A+FAC*(A-P(IE))
            PNEW(IE)=P(IE)
   40    CONTINUE
         IE=IE+1
         IF(ABS(P(IE)-P1(I)) .GT. DAMP) THEN
            P(IE)=P1(I)+SIGN(DAMP,P(IE)-P1(I))
         ELSE
            P(IE)=P(IE)+FAC*(P(IE)-P1(I))
         ENDIF
         P(IE)=MIN(RHFUHF,MAX(P(IE),0.D0))
         SUM2=SUM2+P(IE)
   50 PNEW(IE)=P(IE)
C
C   RE-NORMALIZE IF ANY DENSITY MATRIX ELEMENTS HAVE BEEN TRUNCATED
C
      SUM0=SUM1
   60 IF(SUM2.GT.1.D-3)THEN
         SUM=SUM1/SUM2
      ELSE
         SUM=0.D0
      ENDIF
      SUM1=SUM0
      IF(SUM2.GT.1.D-3.AND.ABS(SUM-1.D0).GT.1.D-5)THEN
C#      WRITE(6,'(6F12.6)')(P((I*(I+1))/2),I=1,NORBS)
         SUM2=0.D0
         DO 70 I=1,NORBS
            J=(I*(I+1))/2
C
C   ADD ON A SMALL NUMBER IN CASE AN OCCUPANCY IS EXACTLY ZERO
C
            P(J)=P(J)*SUM+1.D-20
            P(J)=MAX(P(J),0.D0)
C
C  SET UP RENORMALIZATION OVER PARTLY OCCUPIED M.O.'S ONLY.  FULL M.O.'S
C  CAN'T BE FILLED ANY MORE
C
            IF(P(J).GT.RHFUHF)THEN
               P(J)=RHFUHF
               SUM1=SUM1-RHFUHF
            ELSE
               SUM2=SUM2+P(J)
            ENDIF
   70    PNEW(J)=P(J)
         GOTO 60
      ENDIF
      RETURN
      END
