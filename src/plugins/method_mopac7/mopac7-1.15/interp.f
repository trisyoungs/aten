      SUBROUTINE INTERP(N,NP,NQ,MODE,E,FP,CP,VEC,FOCK,P,H,VECL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION FP(MPACK), CP(N,N)
      DIMENSION VEC(N,N), FOCK(N,N),
     1          P(N,N), H(N*N), VECL(N*N)
**********************************************************************
*
* INTERP: AN INTERPOLATION PROCEDURE FOR FORCING SCF CONVERGANCE
*         ORIGINAL THEORY AND FORTRAN WRITTEN BY R.N. CAMP AND
*         H.F. KING, J. CHEM. PHYS. 75, 268 (1981)
**********************************************************************
*
* ON INPUT N     = NUMBER OF ORBITALS
*          NP    = NUMBER OF FILLED LEVELS
*          NQ    = NUMBER OF EMPTY LEVELS
*          MODE  = 1, DO NOT RESET.
*          E     = ENERGY
*          FP    = FOCK MATRIX, AS LOWER HALF TRIANGLE, PACKED
*          CP    = EIGENVECTORS OF FOCK MATRIX OF ITERATION -1
*                  AS PACKED ARRAY OF N*N COEFFICIENTS
*
* ON OUTPUT CP   = BEST GUESSED SET OF EIGENVECTORS
*           MODE = 2 OR 3 - USED BY CALLING PROGRAM
**********************************************************************
      DIMENSION THETA(MAXORB)
      COMMON /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      COMMON/FIT/NPNTS,IDUM2,XLOW,XHIGH,XMIN,EMIN,DEMIN,X(12),F(12),
     1 DF(12)
      LOGICAL DEBUG
      CHARACTER*241 KEYWRD
      SAVE ZERO, FF, RADMAX, ICALCN, DEBUG
      DATA ICALCN/0/
      DATA ZERO,FF,RADMAX/0.0D0,0.9D0,1.5708D0/
      IF(ICALCN.NE.NUMCAL)THEN
         DEBUG=(INDEX(KEYWRD,'INTERP').NE.0)
         ICALCN=NUMCAL
      ENDIF
C
C     RADMAX=MAXIMUM ROTATION ANGLE (RADIANS).  1.5708 = 90 DEGREES.
C         FF=FACTOR FOR CONVERGENCE TEST FOR 1D SEARCH.
C
      MINPQ=MIN0(NP,NQ)
      NP1=NP+1
      NP2=MAX0(1,NP/2)
      IF(MODE.EQ.2) GO TO 110
C
C     (MODE=1 OR 3 ENTRY)
C     TRANSFORM FOCK MATRIX TO CURRENT MO BASIS.
C     ONLY THE OFF DIAGONAL OCC-VIRT BLOCK IS COMPUTED.
C     STORE IN FOCK ARRAY
C
      II=0
      DO 50 I=1,N
         I1=I+1
         DO 40 J=1,NQ
            DUM=ZERO
            DO 20 K=1,I
   20       DUM=DUM+FP(II+K)*CP(K,J+NP)
            IF(I.EQ.N) GO TO 40
            IK=II+I+I
            DO 30 K=I1,N
               DUM=DUM+FP(IK)*CP(K,J+NP)
   30       IK=IK+K
   40    P(I,J)=DUM
   50 II=II+I
      DO 80 I=1,NP
         DO 70 J=1,NQ
            DUM=ZERO
            DO 60 K=1,N
   60       DUM=DUM+CP(K,I)*P(K,J)
            FOCK(I,J)=DUM
   70    CONTINUE
   80 CONTINUE
      IF(MODE.EQ.3) GO TO 100
C
C     CURRENT POINT BECOMES OLD POINT (MODE=1 ENTRY)
C
      DO 90 I=1,N
         DO 90 J=1,N
   90 VEC(I,J)=CP(I,J)
      EOLD=E
      XOLD=1.0D0
      MODE=2
      RETURN
C
C     (MODE=3 ENTRY)
C     FOCK CORRESPONDS TO CURRENT POINT IN CORRESPONDING REPRESENTATION.
C     VEC DOES NOT HOLD CURRENT VECTORS. VEC SET IN LAST MODE=2 ENTRY.
C
  100 NPNTS=NPNTS+1
      IF(DEBUG)WRITE(6,'(''   INTERPOLATED ENERGY:'',F13.6)')E*23.061D0
      IPOINT=NPNTS
      GO TO 500
C
C    (MODE=2 ENTRY) CALCULATE THETA, AND U, V, W MATRICES.
C                   U ROTATES CURRENT INTO OLD MO.
C                   V ROTATES CURRENT INTO CORRESPONDING CURRENT MO.
C                   W ROTATES OLD INTO CORRESPONDING OLD MO.
C
  110 J1=1
      DO 140 I=1,N
         IF(I.EQ.NP1) J1=NP1
         DO 130 J=J1,N
            P(I,J)=ZERO
            DO 120 K=1,N
  120       P(I,J)=P(I,J)+CP(K,I)*VEC(K,J)
  130    CONTINUE
  140 CONTINUE
C
C     U = CP(DAGGER)*VEC IS NOW IN P ARRAY.
C     VEC IS NOW AVAILABLE FOR TEMPORARY STORAGE.
C
      IJ=0
      DO 170 I=1,NP
         DO 160 J=1,I
            IJ=IJ+1
            H(IJ)=0.D0
            DO 150 K=NP1,N
  150       H(IJ)=H(IJ)+P(I,K)*P(J,K)
  160    CONTINUE
  170 CONTINUE
      CALL HQRII(H,NP,NP,THETA,VECL)
      DO 180 I=NP,1,-1
         IL=I*NP-NP
         DO 180 J=NP,1,-1
  180 VEC(J,I)=VECL(J+IL)
      DO 200 I=1,NP2
         DUM=THETA(NP1-I)
         THETA(NP1-I)=THETA(I)
         THETA(I)=DUM
         DO 190 J=1,NP
            DUM=VEC(J,NP1-I)
            VEC(J,NP1-I)=VEC(J,I)
  190    VEC(J,I)=DUM
  200 CONTINUE
      DO 210 I=1,MINPQ
         THETA(I)=MAX(THETA(I),ZERO)
         THETA(I)=MIN(THETA(I),1.D0)
  210 THETA(I)=ASIN(SQRT(THETA(I)))
C
C     THETA MATRIX HAS NOW BEEN CALCULATED, ALSO UNITARY VP MATRIX
C     HAS BEEN CALCULATED AND STORED IN FIRST NP COLUMNS OF VEC MATRIX.
C     NOW COMPUTE WQ
C
      DO 240 I=1,NQ
         DO 230 J=1,MINPQ
            VEC(I,NP+J)=ZERO
            DO 220 K=1,NP
  220       VEC(I,NP+J)=VEC(I,NP+J)+P(K,NP+I)*VEC(K,J)
  230    CONTINUE
  240 CONTINUE
      CALL SCHMIT(VEC(1,NP1),NQ,N)
C
C     UNITARY WQ MATRIX NOW IN LAST NQ COLUMNS OF VEC MATRIX.
C     TRANSPOSE NP BY NP BLOCK OF U STORED IN P
C
      DO 260 I=1,NP
         DO 250 J=1,I
            DUM=P(I,J)
            P(I,J)=P(J,I)
  250    P(J,I)=DUM
  260 CONTINUE
C
C     CALCULATE WP MATRIX AND HOLD IN FIRST NP COLUMNS OF P
C
      DO 300 I=1,NP
         DO 270 K=1,NP
  270    H(K)=P(I,K)
         DO 290 J=1,NP
            P(I,J)=ZERO
            DO 280 K=1,NP
  280       P(I,J)=P(I,J)+H(K)*VEC(K,J)
  290    CONTINUE
  300 CONTINUE
      CALL SCHMIB(P,NP,N)
C
C     CALCULATE VQ MATRIX AND HOLD IN LAST NQ COLUMNS OF P MATRIX.
C
      DO 340 I=1,NQ
         DO 310 K=1,NQ
  310    H(K)=P(NP+I,NP+K)
         DO 330 J=NP1,N
            P(I,J)=ZERO
            DO 320 K=1,NQ
  320       P(I,J)=P(I,J)+H(K)*VEC(K,J)
  330    CONTINUE
  340 CONTINUE
      CALL SCHMIB(P(1,NP1),NQ,N)
C
C     CALCULATE (DE/DX) AT OLD POINT
C
      DEDX=ZERO
      DO 370 I=1,NP
         DO 360 J=1,NQ
            DUM=ZERO
            DO 350 K=1,MINPQ
  350       DUM=DUM+THETA(K)*P(I,K)*VEC(J,NP+K)
  360    DEDX=DEDX+DUM*FOCK(I,J)
  370 CONTINUE
C
C     STORE OLD POINT INFORMATION FOR SPLINE FIT
C
      DEOLD=-4.0D0*DEDX
      X(2)=XOLD
      F(2)=EOLD
      DF(2)=DEOLD
C
C     MOVE VP OUT OF VEC ARRAY INTO FIRST NP COLUMNS OF P MATRIX.
C
      DO 380 I=1,NP
         DO 380 J=1,NP
  380 P(I,J)=VEC(I,J)
      K1=0
      K2=NP
      DO 410 J=1,N
         IF(J.EQ.NP1) K1=NP
         IF(J.EQ.NP1) K2=NQ
         DO 400 I=1,N
            DUM=ZERO
            DO 390 K=1,K2
  390       DUM=DUM+CP(I,K1+K)*P(K,J)
  400    VEC(I,J)=DUM
  410 CONTINUE
C
C     CORRESPONDING CURRENT MO VECTORS NOW HELD IN VEC.
C     COMPUTE VEC(DAGGER)*FP*VEC
C     STORE OFF-DIAGONAL BLOCK IN FOCK ARRAY.
C
  420 II=0
      DO 460 I=1,N
         I1=I+1
         DO 450 J=1,NQ
            DUM=ZERO
            DO 430 K=1,I
  430       DUM=DUM+FP(II+K)*VEC(K,J+NP)
            IF(I.EQ.N) GO TO 450
            IK=II+I+I
            DO 440 K=I1,N
               DUM=DUM+FP(IK)*VEC(K,J+NP)
  440       IK=IK+K
  450    P(I,J)=DUM
  460 II=II+I
      DO 490 I=1,NP
         DO 480 J=1,NQ
            DUM=ZERO
            DO 470 K=1,N
  470       DUM=DUM+VEC(K,I)*P(K,J)
            FOCK(I,J)=DUM
  480    CONTINUE
  490 CONTINUE
C
C     SET LIMITS ON RANGE OF 1-D SEARCH
C
      NPNTS=2
      IPOINT=1
      XNOW=ZERO
      XHIGH=RADMAX/THETA(1)
      XLOW=-0.5D0*XHIGH
C
C     CALCULATE (DE/DX) AT CURRENT POINT AND
C     STORE INFORMATION FOR SPLINE FIT
C     ***** JUMP POINT FOR MODE=3 ENTRY *****
C
  500 DEDX=ZERO
      DO 510 K=1,MINPQ
  510 DEDX=DEDX+THETA(K)*FOCK(K,K)
      DENOW=-4.0D0*DEDX
      ENOW=E
      IF(IPOINT.LE.12) GO TO 530
  520 FORMAT(//,'EXCESSIVE DATA PNTS FOR SPLINE.',/
     1,'IPOINT =',I3,'MAXIMUM IS 12.')
C
C     PERFORM 1-D SEARCH AND DETERMINE EXIT MODE.
C
  530 X(IPOINT)=XNOW
      F(IPOINT)=ENOW
      DF(IPOINT)=DENOW
      CALL SPLINE
      IF((EOLD-ENOW).GT.FF*(EOLD-EMIN).OR.IPOINT.GT.10) GO TO 560
C
C     (MODE=3 EXIT) RECOMPUTE CP VECTORS AT PREDICTED MINIMUM.
C
      XNOW=XMIN
      DO 550 K=1,MINPQ
         CK=COS(XNOW*THETA(K))
         SK=SIN(XNOW*THETA(K))
         IF(DEBUG)WRITE(6,'('' ROTATION ANGLE:'',F12.4)')SK*57.29578D0
         DO 540 I=1,N
            CP(I,K)   =CK*VEC(I,K)-SK*VEC(I,NP+K)
  540    CP(I,NP+K)=SK*VEC(I,K)+CK*VEC(I,NP+K)
  550 CONTINUE
      MODE=3
      RETURN
C
C     (MODE=2 EXIT) CURRENT VECTORS GIVE SATISFACTORY ENERGY IMPROVEMENT
C     CURRENT POINT BECOMES OLD POINT FOR THE NEXT 1-D SEARCH.
C
  560 IF(MODE.EQ.2) GO TO 580
      DO 570 I=1,N
         DO 570 J=1,N
  570 VEC(I,J)=CP(I,J)
      MODE=2
  580 ROLD=XOLD*THETA(1)*57.29578D0
      RNOW=XNOW*THETA(1)*57.29578D0
      RMIN=XMIN*THETA(1)*57.29578D0
      IF(DEBUG)WRITE(6,600) XOLD,EOLD*23.061D0,DEOLD,ROLD
     1,             XNOW,ENOW*23.061D0,DENOW,RNOW
     2,             XMIN,EMIN*23.061D0,DEMIN,RMIN
      EOLD=ENOW
      IF(NPNTS.LE.200) RETURN
      WRITE(6,610)
      DO 590 K=1,NPNTS
  590 WRITE(6,620) K,X(K),F(K),DF(K)
      WRITE(6,630)
      RETURN
  600 FORMAT(
     1/14X,3H X ,10X,6H F(X) ,9X,7H DF/DX ,21H   ROTATION (DEGREES),
     2/10H      OLD ,F10.5,3F15.10,
     3/10H  CURRENT ,F10.5,3F15.10,
     4/10H PREDICTED,F10.5,3F15.10/)
  610 FORMAT(3H  K,10H     X(K) ,15H       F(K)    ,10H     DF(K))
  620 FORMAT(I3,F10.5,2F15.10)
  630 FORMAT(10X)
      END
      SUBROUTINE SPLINE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      LOGICAL SKIP1,SKIP2
C
C     FIT F(X) BY A CUBIC SPLINE GIVEN VALUES OF THE FUNCTION
C     AND ITS FIRST DERIVATIVE AT N PNTS.
C     SUBROUTINE RETURNS VALUES OF XMIN,FMIN, AND DFMIN
C     AND MAY REORDER THE DATA.
C     CALLING PROGRAM SUPPLIES ALL OTHER VALUES IN THE
C     COMMON BLOCK.
C     XLOW AND XHIGH SET LIMITS ON THE INTERVAL WITHIN WHICH
C     TO SEARCH.  SUBROUTINE MAY FURTHER REDUCE THIS INTERVAL.
C
      COMMON/FIT/N,IDUM2,XLOW,XHIGH,XMIN,FMIN,DFMIN,X(12),F(12),DF(12)
      SAVE CLOSE, BIG, HUGE, USTEP, DSTEP
      DATA CLOSE, BIG, HUGE, USTEP, DSTEP/1.0E-8,500.0,1.0E+10,1.0,2.0/
C
C     SUBROUTINE ASSUMES THAT THE FIRST N-1 DATA PNTS HAVE BEEN
C     PREVIOUSLY ORDERED,  X(I).LT.X(I+1) FOR I=1,2,...,N-2
C     NOW MOVE NTH POINT TO ITS PROPER PLACE.
C
      XMIN=X(N)
      FMIN=F(N)
      DFMIN=DF(N)
      N1=N-1
      K=N1
   10 IF(X(K).LT.XMIN) GO TO 20
      X(K+1)=X(K)
      F(K+1)=F(K)
      DF(K+1)=DF(K)
      K=K-1
      IF(K.GT.0) GO TO 10
   20 X(K+1)=XMIN
      F(K+1)=FMIN
      DF(K+1)=DFMIN
C
C     DEFINE THE INTERVAL WITHIN WHICH WE TRUST THE SPLINE FIT.
C     USTEP =  UP HILL STEP SIZE FACTOR
C     DSTEP = DOWN HILL STEP SIZE FACTOR
C
      IF(DF(1).GT.0.0) STEP=DSTEP
      IF(DF(1).LE.0.0) STEP=USTEP
      XSTART=X(1)-STEP*(X(2)-X(1))
      XSTART=MAX(XSTART,XLOW)
      IF(DF(N).GT.0.0) STEP=USTEP
      IF(DF(N).LE.0.0) STEP=DSTEP
      XSTOP=X(N)+STEP*(X(N)-X(N1))
      XSTOP=MIN(XSTOP,XHIGH)
C
C     SEARCH FOR MINIMUM
C
      DO 110 K=1,N1
         SKIP1=K.NE.1
         SKIP2=K.NE.N1
         IF(F(K).GE.FMIN) GO TO 30
         XMIN=X(K)
         FMIN=F(K)
         DFMIN=DF(K)
   30    DX=X(K+1)-X(K)
C
C     SKIP INTERVAL IF PNTS ARE TOO CLOSE TOGETHER
C
         IF(DX.LE.CLOSE) GO TO 110
         X1=0.0D0
         IF(K.EQ.1) X1=XSTART-X(1)
         X2=DX
         IF(K.EQ.N1) X2=XSTOP-X(N1)
C
C     (A,B,C)=COEF OF (CUBIC,QUADRATIC,LINEAR) TERMS
C
         DUM=(F(K+1)-F(K))/DX
         A=(DF(K)+DF(K+1)-DUM-DUM)/(DX*DX)
         B=(DUM+DUM+DUM-DF(K)-DF(K)-DF(K+1))/DX
         C=DF(K)
C
C     XK = X-X(K) AT THE MINIMUM WITHIN THE KTH SUBINTERVAL
C     TEST FOR PATHOLOGICAL CASES.
C
         BB=B*B
         AC3=(A+A+A)*C
         IF(BB.LT.AC3) GO TO 90
         IF( B.GT.0.0) GO TO 40
         IF(ABS(B).GT.HUGE*ABS(A)) GO TO 90
         GO TO 50
   40    IF(BB.GT.BIG*ABS(AC3)) GO TO 60
C
C     WELL BEHAVED CUBIC
C
   50    XK=(-B+SQRT(BB-AC3))/(A+A+A)
         GO TO 70
C
C     CUBIC IS DOMINATED BY QUADRATIC TERM
C
   60    R=AC3/BB
         XK=-(((0.039063D0*R+0.0625D0)*R+0.125D0)*R+0.5D0)*C/B
   70    IF(XK.LT.X1.OR.XK.GT.X2) GO TO 90
   80    FM=((A*XK+B)*XK+C)*XK+F(K)
         IF(FM.GT.FMIN) GO TO 90
         XMIN=XK+X(K)
         FMIN=FM
         DFMIN=((A+A+A)*XK+B+B)*XK+C
C
C     EXTRAPOLATE TO END OF INTERVAL IF K=1 AND/OR K=N1
C
   90    IF(SKIP1) GO TO 100
         SKIP1=.TRUE.
         XK=X1
         GO TO 80
  100    IF(SKIP2) GO TO 110
         SKIP2=.TRUE.
         XK=X2
         GO TO 80
  110 CONTINUE
      RETURN
      END
      SUBROUTINE SCHMIT(U,N,NDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION U(NDIM,NDIM)
      SAVE ZERO, SMALL, ONE
      DATA ZERO,SMALL,ONE/0.0,0.01,1.0/
      II=0
      DO 110 K=1,N
         K1=K-1
C
C     NORMALIZE KTH COLUMN VECTOR
C
         DOT = ZERO
         DO 10 I=1,N
   10    DOT=DOT+U(I,K)*U(I,K)
         IF(DOT.EQ.ZERO) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 20 I=1,N
   20    U(I,K)=SCALE*U(I,K)
   30    IF(K1.EQ.0) GO TO 110
         NPASS=0
C
C     PROJECT OUT K-1 PREVIOUS ORTHONORMAL VECTORS FROM KTH VECTOR
C
   40    NPASS=NPASS+1
         DO 70 J=1,K1
            DOT=ZERO
            DO 50 I=1,N
   50       DOT=DOT+U(I,J)*U(I,K)
            DO 60 I=1,N
   60       U(I,K)=U(I,K)-DOT*U(I,J)
   70    CONTINUE
C
C     SECOND NORMALIZATION (AFTER PROJECTION)
C     IF KTH VECTOR IS SMALL BUT NOT ZERO THEN NORMALIZE
C     AND PROJECT AGAIN TO CONTROL ROUND-OFF ERRORS.
C
         DOT=ZERO
         DO 80 I=1,N
   80    DOT=DOT+U(I,K)*U(I,K)
         IF(DOT.EQ.ZERO) GO TO 100
         IF(DOT.LT.SMALL.AND.NPASS.GT.2) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 90 I=1,N
   90    U(I,K)=SCALE*U(I,K)
         IF(DOT.LT.SMALL) GO TO 40
         GO TO 110
C
C     REPLACE LINEARLY DEPENDENT KTH VECTOR BY A UNIT VECTOR.
C
  100    II=II+1
C     IF(II.GT.N) STOP
         U(II,K)=ONE
         GO TO 30
  110 CONTINUE
      RETURN
      END
      SUBROUTINE SCHMIB(U,N,NDIM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
C
C     SAME AS SCHMIDT BUT WORKS FROM RIGHT TO LEFT.
C
      DIMENSION U(NDIM,NDIM)
      SAVE ZERO, SMALL, ONE
      DATA ZERO,SMALL,ONE/0.0,0.01,1.0/
      N1=N+1
      II=0
      DO 110 K=1,N
         K1=K-1
C
C     NORMALIZE KTH COLUMN VECTOR
C
         DOT = ZERO
         DO 10 I=1,N
   10    DOT=DOT+U(I,N1-K)*U(I,N1-K)
         IF(DOT.EQ.ZERO) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 20 I=1,N
   20    U(I,N1-K)=SCALE*U(I,N1-K)
   30    IF(K1.EQ.0) GO TO 110
         NPASS=0
C
C     PROJECT OUT K-1 PREVIOUS ORTHONORMAL VECTORS FROM KTH VECTOR
C
   40    NPASS=NPASS+1
         DO 70 J=1,K1
            DOT=ZERO
            DO 50 I=1,N
   50       DOT=DOT+U(I,N1-J)*U(I,N1-K)
            DO 60 I=1,N
   60       U(I,N1-K)=U(I,N1-K)-DOT*U(I,N1-J)
   70    CONTINUE
C
C     SECOND NORMALIZATION (AFTER PROJECTION)
C     IF KTH VECTOR IS SMALL BUT NOT ZERO THEN NORMALIZE
C     AND PROJECT AGAIN TO CONTROL ROUND-OFF ERRORS.
C
         DOT=ZERO
         DO 80 I=1,N
   80    DOT=DOT+U(I,N1-K)*U(I,N1-K)
         IF(DOT.EQ.ZERO) GO TO 100
         IF(DOT.LT.SMALL.AND.NPASS.GT.2) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 90 I=1,N
   90    U(I,N1-K)=SCALE*U(I,N1-K)
         IF(DOT.LT.SMALL) GO TO 40
         GO TO 110
C
C     REPLACE LINEARLY DEPENDENT KTH VECTOR BY A UNIT VECTOR.
C
  100    II=II+1
C     IF(II.GT.N) STOP
         U(II,N1-K)=ONE
         GO TO 30
  110 CONTINUE
      RETURN
      END
      SUBROUTINE PULAY(F,P,N,FPPF,FOCK,EMAT,LFOCK,NFOCK,MSIZE,START,PL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION F(*), P(*), FPPF(*), FOCK(*)
      LOGICAL START
************************************************************************
*
*   PULAY USES DR. PETER PULAY'S METHOD FOR CONVERGENCE.
*         A MATHEMATICAL DESCRIPTION CAN BE FOUND IN
*         "P. PULAY, J. COMP. CHEM. 3, 556 (1982).
*
* ARGUMENTS:-
*         ON INPUT F      = FOCK MATRIX, PACKED, LOWER HALF TRIANGLE.
*                  P      = DENSITY MATRIX, PACKED, LOWER HALF TRIANGLE.
*                  N      = NUMBER OF ORBITALS.
*                  FPPF   = WORKSTORE OF SIZE MSIZE, CONTENTS WILL BE
*                           OVERWRITTEN.
*                  FOCK   =      "       "              "         "
*                  EMAT   = WORKSTORE OF AT LEAST 15**2 ELEMENTS.
*                  START  = LOGICAL, = TRUE TO START PULAY.
*                  PL     = UNDEFINED ELEMENT.
*      ON OUTPUT   F      = "BEST" FOCK MATRIX, = LINEAR COMBINATION
*                           OF KNOWN FOCK MATRICES.
*                  START  = FALSE
*                  PL     = MEASURE OF NON-SELF-CONSISTENCY
*                         = [F*P] = F*P - P*F.
*
************************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      DIMENSION EMAT(20,20), EVEC(1000), COEFFS(20)
      CHARACTER*241 KEYWRD
      LOGICAL  DEBUG
      DATA ICALCN/0/
      IF (ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
         MAXLIM=6
         DEBUG=(INDEX(KEYWRD,'DEBUGPULAY') .NE.0)
      ENDIF
      IF(START) THEN
         LINEAR=(N*(N+1))/2
         MFOCK=MSIZE/LINEAR
         IF(MFOCK.GT.MAXLIM)MFOCK=MAXLIM
         IF(DEBUG)
     1    WRITE(6,'('' MAXIMUM SIZE:'',I5)')MFOCK
         NFOCK=1
         LFOCK=1
         START=.FALSE.
      ELSE
         IF(NFOCK.LT.MFOCK)      NFOCK=NFOCK+1
         IF(LFOCK.NE.MFOCK)THEN
            LFOCK=LFOCK+1
         ELSE
            LFOCK=1
         ENDIF
      ENDIF
      LBASE=(LFOCK-1)*LINEAR
*
*   FIRST, STORE FOCK MATRIX FOR FUTURE REFERENCE.
*
      DO 10 I=1,LINEAR
   10 FOCK((I-1)*MFOCK+LFOCK)=F(I)
*
*   NOW FORM /FOCK*DENSITY-DENSITY*FOCK/, AND STORE THIS IN FPPF
*
      CALL MAMULT(P,F,FPPF(LBASE+1),N,0.D0)
      CALL MAMULT(F,P,FPPF(LBASE+1),N,-1.D0)
*
*   FPPF NOW CONTAINS THE RESULT OF FP - PF.
*
      NFOCK1=NFOCK+1
      DO 20 I=1,NFOCK
         EMAT(NFOCK1,I)=-1.D0
         EMAT(I,NFOCK1)=-1.D0
         EMAT(LFOCK,I)=DOT(FPPF((I-1)*LINEAR+1),FPPF(LBASE+1),LINEAR)
   20 EMAT(I,LFOCK)=EMAT(LFOCK,I)
      PL=EMAT(LFOCK,LFOCK)/LINEAR
      EMAT(NFOCK1,NFOCK1)=0.D0
      CONST=1.D0/EMAT(LFOCK,LFOCK)
      DO 30 I=1,NFOCK
         DO 30 J=1,NFOCK
   30 EMAT(I,J)=EMAT(I,J)*CONST
      IF(DEBUG) THEN
         WRITE(6,'('' EMAT'')')
         DO 40 I=1,NFOCK1
   40    WRITE(6,'(6E13.6)')(EMAT(J,I),J=1,NFOCK1)
      ENDIF
      L=0
      DO 50 I=1,NFOCK1
         DO 50 J=1,NFOCK1
            L=L+1
   50 EVEC(L)=EMAT(I,J)
      CONST=1.D0/CONST
      DO 60 I=1,NFOCK
         DO 60 J=1,NFOCK
   60 EMAT(I,J)=EMAT(I,J)*CONST
*********************************************************************
*   THE MATRIX EMAT SHOULD HAVE FORM
*
*      |<E(1)*E(1)>  <E(1)*E(2)> ...   -1.0|
*      |<E(2)*E(1)>  <E(2)*E(2)> ...   -1.0|
*      |<E(3)*E(1)>  <E(3)*E(2)> ...   -1.0|
*      |<E(4)*E(1)>  <E(4)*E(2)> ...   -1.0|
*      |     .            .      ...     . |
*      |   -1.0         -1.0     ...    0. |
*
*   WHERE <E(I)*E(J)> IS THE SCALAR PRODUCT OF [F*P] FOR ITERATION I
*   TIMES [F*P] FOR ITERATION J.
*
*********************************************************************
      CALL OSINV(EVEC,NFOCK1,D)
      IF(ABS(D).LT.1.D-6)THEN
         START=.TRUE.
         RETURN
      ENDIF
      IF(NFOCK.LT.2) RETURN
      IL=NFOCK*NFOCK1
      DO 70 I=1,NFOCK
   70 COEFFS(I)=-EVEC(I+IL)
      IF(DEBUG) THEN
         WRITE(6,'('' EVEC'')')
         WRITE(6,'(6F12.6)')(COEFFS(I),I=1,NFOCK)
         WRITE(6,'(''    LAGRANGIAN MULTIPLIER (ERROR) =''
     1             ,F13.6)')EVEC(NFOCK1*NFOCK1)
      ENDIF
      DO 90 I=1,LINEAR
         SUM=0
         L=0
         II=(I-1)*MFOCK
         DO 80 J=1,NFOCK
   80    SUM=SUM+COEFFS(J)*FOCK(J+II)
   90 F(I)=SUM
      RETURN
      END
      SUBROUTINE OSINV (A,N,D)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION A(*)
************************************************************************
*
*    OSINV INVERTS A GENERAL SQUARE MATRIX OF ORDER UP TO MAXORB. SEE
*          DIMENSION STATEMENTS BELOW.
*
*   ON INPUT       A = GENERAL SQUARE MATRIX STORED LINEARLY.
*                  N = DIMENSION OF MATRIX A.
*                  D = VARIABLE, NOT DEFINED ON INPUT.
*
*   ON OUTPUT      A = INVERSE OF ORIGINAL A.
*                  D = DETERMINANT OF ORIGINAL A, UNLESS A WAS SINGULAR,
*                      IN WHICH CASE D = 0.0
*
************************************************************************
      DIMENSION L(MAXORB), M(MAXORB)
************************************************************************
*
*    IF THE VALUE OF TOL GIVEN HERE IS UNSUITABLE, IT CAN BE CHANGED.
      TOL=1.D-8
*
*
************************************************************************
      D=1.0D0
      NK=-N
      DO 180 K=1,N
         NK=NK+N
         L(K)=K
         M(K)=K
         KK=NK+K
         BIGA=A(KK)
         DO 20 J=K,N
            IZ=N*(J-1)
            DO 20 I=K,N
               IJ=IZ+I
C
C     10 FOLLOWS
C
               IF (ABS(BIGA)-ABS(A(IJ))) 10,20,20
   10          BIGA=A(IJ)
               L(K)=I
               M(K)=J
   20    CONTINUE
         J=L(K)
         IF (J-K) 50,50,30
   30    KI=K-N
         DO 40 I=1,N
            KI=KI+N
            HOLO=-A(KI)
            JI=KI-K+J
            A(KI)=A(JI)
   40    A(JI)=HOLO
   50    I=M(K)
         IF (I-K) 80,80,60
   60    JP=N*(I-1)
         DO 70 J=1,N
            JK=NK+J
            JI=JP+J
            HOLO=-A(JK)
            A(JK)=A(JI)
   70    A(JI)=HOLO
   80    IF (ABS(BIGA)-TOL) 90,100,100
   90    D=0.0D0
         RETURN
  100    DO 120 I=1,N
            IF (I-K) 110,120,110
  110       IK=NK+I
            A(IK)=A(IK)/(-BIGA)
  120    CONTINUE
         DO 150 I=1,N
            IK=NK+I
            IJ=I-N
            DO 150 J=1,N
               IJ=IJ+N
               IF (I-K) 130,150,130
  130          IF (J-K) 140,150,140
  140          KJ=IJ-I+K
               A(IJ)=A(IK)*A(KJ)+A(IJ)
  150    CONTINUE
         KJ=K-N
         DO 170 J=1,N
            KJ=KJ+N
            IF (J-K) 160,170,160
  160       A(KJ)=A(KJ)/BIGA
  170    CONTINUE
         D=MIN(D*BIGA,1.D10)
         A(KK)=1.0D0/BIGA
  180 CONTINUE
      K=N
  190 K=K-1
      IF (K) 260,260,200
  200 I=L(K)
      IF (I-K) 230,230,210
  210 JQ=N*(K-1)
      JR=N*(I-1)
      DO 220 J=1,N
         JK=JQ+J
         HOLO=A(JK)
         JI=JR+J
         A(JK)=-A(JI)
  220 A(JI)=HOLO
  230 J=M(K)
      IF (J-K) 190,190,240
  240 KI=K-N
      DO 250 I=1,N
         KI=KI+N
         HOLO=A(KI)
         JI=KI+J-K
         A(KI)=-A(JI)
  250 A(JI)=HOLO
      GO TO 190
  260 RETURN
C
      END
