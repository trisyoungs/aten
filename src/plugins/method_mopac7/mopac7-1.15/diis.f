      SUBROUTINE DIIS(XP, XPARAM, GP, GRAD, HP, HEAT, HS, NVAR, FRST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XP(NVAR), XPARAM(NVAR), GP(NVAR),
     1GRAD(NVAR), HS(NVAR*NVAR)
      LOGICAL FRST
************************************************************************
*                                                                      *
*     DIIS PERFORMS DIRECT INVERSION IN THE ITERATIVE SUBSPACE         *
*                                                                      *
*     THIS INVOLVES SOLVING FOR C IN XPARAM(NEW) = XPARAM' - HG'       *
*                                                                      *
*  WHERE XPARAM' = SUM(C(I)XPARAM(I), THE C COEFFICIENTES COMING FROM  *
*                                                                      *
*                   | B   1 | . | C | = | 0 |                          *
*                   | 1   0 |   |-L |   | 1 |                          *
*                                                                      *
* WHERE B(I,J) =GRAD(I)H(T)HGRAD(J)  GRAD(I) = GRADIENT ON CYCLE I     *
*                              H    = INVERSE HESSIAN                  *
*                                                                      *
*                          REFERENCE                                   *
*                                                                      *
*  P. CSASZAR, P. PULAY, J. MOL. STRUCT. (THEOCHEM), 114, 31 (1984)    *
*                                                                      *
************************************************************************
************************************************************************
*                                                                      *
*     GEOMETRY OPTIMIZATION USING THE METHOD OF DIRECT INVERSION IN    *
*     THE ITERATIVE SUBSPACE (GDIIS), COMBINED WITH THE BFGS OPTIMIZER *
*     (A VARIABLE METRIC METHOD)                                       *
*                                                                      *
*     WRITTEN BY PETER L. CUMMINS, UNIVERSITY OF SYDNEY, AUSTRALIA     *
*                                                                      *
*                              REFERENCE                               *
*                                                                      *
*      "COMPUTATIONAL STRATEGIES FOR THE OPTIMIZATION OF EQUILIBRIUM   *
*     GEOMETRIES AND TRANSITION-STATE STRUCTURES AT THE SEMIEMPIRICAL  *
*     LEVEL", PETER L. CUMMINS, JILL E. GREADY, J. COMP. CHEM., 10,    *
*     939-950 (1989).                                                  *
*                                                                      *
*     MODIFIED BY JJPS TO CONFORM TO EXISTING MOPAC CONVENTIONS        *
*                                                                      *
************************************************************************
      COMMON /KEYWRD/ KEYWRD
      PARAMETER (MRESET=15, M2=(MRESET+1)*(MRESET+1))
      DIMENSION  XSET(MRESET*MAXPAR),GSET(MRESET*MAXPAR), ESET(MRESET)
      DIMENSION DX(MAXPAR),GSAVE(MAXPAR),
     1 ERR(MRESET*MAXPAR),B(M2),BS(M2),BST(M2)
      LOGICAL DEBUG, PRINT
      CHARACTER*241 KEYWRD
      DEBUG=.FALSE.
      PRINT=(INDEX(KEYWRD,' DIIS').NE.0)
      IF (PRINT) DEBUG=(INDEX(KEYWRD,'DEBUG').NE.0)
      IF (PRINT)  WRITE(6,'(/,''      ***** BEGIN GDIIS ***** '')')
C
C  SPACE SIMPLY LOADS THE CURRENT VALUES OF XPARAM AND GNORM INTO
C  THE ARRAYS XSET AND GSET
C
      CALL SPACE(MRESET,MSET,XPARAM, GRAD, HEAT, NVAR, XSET, GSET, ESET
     1, FRST)
C
C     INITIALIZE SOME VARIABLES AND CONSTANTS
C
      NDIIS = MSET
      MPLUS = MSET + 1
      MM = MPLUS * MPLUS
C
C     COMPUTE THE APPROXIMATE ERROR VECTORS
C
      INV=-NVAR
      DO 30 I=1,MSET
         INV = INV + NVAR
         DO 30 J=1,NVAR
            S = 0.D0
            KJ=(J*(J-1))/2
            DO 10 K=1,J
               KJ = KJ+1
   10       S = S - HS(KJ) * GSET(INV+K)
            DO 20 K=J+1,NVAR
               KJ = (K*(K-1))/2+J
   20       S = S - HS(KJ) * GSET(INV+K)
   30 ERR(INV+J) = S
C
C     CONSTRUCT THE GDIIS MATRIX
C
      DO 40 I=1,MM
   40 B(I) = 1.D0
      JJ=0
      INV=-NVAR
      DO 50 I=1,MSET
         INV=INV+NVAR
         JNV=-NVAR
         DO 50 J=1,MSET
            JNV=JNV+NVAR
            JJ = JJ + 1
            B(JJ)=0.D0
            DO 50 K=1,NVAR
   50 B(JJ) = B(JJ) + ERR(INV+K) * ERR(JNV+K)
C
      DO 60 I=MSET-1,1,-1
         DO 60 J=MSET,1,-1
   60 B(I*MSET+J+I) = B(I*MSET+J)
      DO 70 I=1,MPLUS
         B(MPLUS*I) = 1.D0
   70 B(MPLUS*MSET+I) = 1.D0
      B(MM) = 0.D0
C
C     ELIMINATE ERROR VECTORS WITH THE LARGEST NORM
C
   80 CONTINUE
      DO 90 I=1,MM
   90 BS(I) = B(I)
      IF (NDIIS .EQ. MSET) GO TO 140
      DO 130 II=1,MSET-NDIIS
         XMAX = -1.D10
         ITERA = 0
         DO 110 I=1,MSET
            XNORM = 0.D0
            INV = (I-1) * MPLUS
            DO 100 J=1,MSET
  100       XNORM = XNORM + ABS(B(INV + J))
            IF (XMAX.LT.XNORM .AND. XNORM.NE.1.0D0) THEN
               XMAX = XNORM
               ITERA = I
               IONE = INV + I
            ENDIF
  110    CONTINUE
         DO 120 I=1,MPLUS
            INV = (I-1) * MPLUS
            DO 120 J=1,MPLUS
               JNV = (J-1) * MPLUS
               IF (J.EQ.ITERA) B(INV + J) = 0.D0
               B(JNV + I) = B(INV + J)
  120    CONTINUE
         B(IONE) = 1.0D0
  130 CONTINUE
  140 CONTINUE
C
      IF (DEBUG) THEN
C
C     OUTPUT THE GDIIS MATRIX
C
         WRITE(*,'(/5X,'' GDIIS MATRIX'')')
         IJ = 0
         DO 150 I=1,MPLUS
            DO 150 J=1,I
               IJ = IJ + 1
  150    BST(IJ) = B( MPLUS * (J-1) + I)
         CALL VECPRT(BST,MPLUS)
      ENDIF
C
C     SCALE DIIS MATRIX BEFORE INVERSION
C
      DO 160 I=1,MPLUS
         II = MPLUS * (I-1) + I
  160 GSAVE(I) = 1.D0 / DSQRT(1.D-20+DABS(B(II)))
      GSAVE(MPLUS) = 1.D0
      DO 170 I=1,MPLUS
         DO 170 J=1,MPLUS
            IJ = MPLUS * (I-1) + J
  170 B(IJ) = B(IJ) * GSAVE(I) * GSAVE(J)
C
      IF (DEBUG) THEN
C
C     OUTPUT SCALED GDIIS MATRIX
C
         WRITE(*,'(/5X,'' GDIIS MATRIX (SCALED)'')')
         IJ = 0
         DO 180 I=1,MPLUS
            DO 180 J=1,I
               IJ = IJ + 1
  180    BST(IJ) = B( MPLUS * (J-1) + I)
         CALL VECPRT(BST,MPLUS)
      ENDIF
C
C     INVERT THE GDIIS MATRIX
C
      CALL MINV(B,MPLUS,DET)
C
      DO 190 I=1,MPLUS
         DO 190 J=1,MPLUS
            IJ = MPLUS * (I-1) + J
  190 B(IJ) = B(IJ) * GSAVE(I) * GSAVE(J)
C
C     COMPUTE THE INTERMEDIATE INTERPOLATED PARAMETER AND GRADIENT
C     VECTORS
C
      DO 200 K=1,NVAR
         XP(K) = 0.D0
         GP(K) = 0.D0
         DO 200 I=1,MSET
            INK = (I-1) * NVAR + K
            XP(K) = XP(K) + B(MPLUS*MSET+I) * XSET(INK)
  200 GP(K) = GP(K) + B(MPLUS*MSET+I) * GSET(INK)
      HP=0.D0
      DO 210 I=1,MSET
  210 HP=HP+B(MPLUS*MSET+I)*ESET(I)
C
      DO 220 K=1,NVAR
  220 DX(K) = XPARAM(K) - XP(K)
      XNORM = SQRT(DOT(DX,DX,NVAR))
      IF (PRINT) THEN
         WRITE (6,'(/10X,''DEVIATION IN X '',F7.4,8X,''DETERMINANT '',
     1 G9.3)') XNORM,DET
         WRITE(6,'(10X,''GDIIS COEFFICIENTS'')')
         WRITE(6,'(10X,5F12.5)') (B(MPLUS*MSET+I),I=1,MSET)
      ENDIF
C
C     THE FOLLOWING TOLERENCES FOR XNORM AND DET ARE SOMEWHAT ARBITRARY!
C
      THRES = MAX(10.D0**(-NVAR), 1.D-25)
      IF (XNORM.GT.2.D0 .OR. DABS(DET).LT. THRES) THEN
         IF (PRINT) WRITE(6,'(10X,''THE DIIS MATRIX IS ILL CONDITIONED''
     1, /10X,'' - PROBABLY, VECTORS ARE LINEARLY DEPENDENT - '',
     2 /10X,''THE DIIS STEP WILL BE REPEATED WITH A SMALLER SPACE'')')
         DO 230 K=1,MM
  230    B(K) = BS(K)
         NDIIS = NDIIS - 1
         IF (NDIIS .GT. 0) GO TO 80
         IF (PRINT) WRITE(*,'(10X,''NEWTON-RAPHSON STEP TAKEN'')')
         DO 240 K=1,NVAR
            XP(K) = XPARAM(K)
  240    GP(K) = GRAD(K)
C
      ENDIF
      IF (PRINT)  WRITE(6,'(/,''      ***** END GDIIS ***** '',/)')
C
      RETURN
      END
      SUBROUTINE SPACE(MRESET, MSET, XPARAM, GRAD, HEAT, NVAR,
     1XSET, GSET, ESET, FRST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XPARAM(NVAR), GRAD(NVAR)
      DIMENSION XSET(MRESET*NVAR),GSET(MRESET*NVAR), ESET(MRESET)
      LOGICAL FRST
C
C     UPDATE PARAMETER AND GRADIENT SUBSPACE
C
      IF(FRST)THEN
         NRESET=MIN(NVAR/2,MRESET)
         FRST=.FALSE.
         MSET=0
      ENDIF
C
      IF (MSET .EQ. NRESET) THEN
         DO 10 I=1,MSET-1
            MI = NVAR*(I-1)
            NI = NVAR*I
            ESET(I)=ESET(I+1)
            DO 10 K=1,NVAR
               XSET(MI+K) = XSET(NI+K)
   10    GSET(MI+K) = GSET(NI+K)
         MSET=NRESET-1
      ENDIF
C
C     STORE THE CURRENT POINT
C
      DO 20 K=1,NVAR
         NMK = NVAR*MSET+K
         XSET(NMK) = XPARAM(K)
   20 GSET(NMK) = GRAD(K)
      MSET=MSET+1
      ESET(MSET)=HEAT
C
      RETURN
      END
      SUBROUTINE MINV(A,N,D)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION A(*)
**********************************************************************
*
*     INVERT A MATRIX USING GAUSS-JORDAN METHOD.  PART OF DIIS
*     A - INPUT MATRIX (MUST BE A GENERAL MATRIX), DESTROYED IN
*        COMPUTATION AND REPLACED BY RESULTANT INVERSE.
*     N - ORDER OF MATRIX A
*     D - RESULTANT DETERMINANT
*
**********************************************************************
      DIMENSION M(MAXPAR), L(MAXPAR)
C
C     SEARCH FOR LARGEST ELEMENT
C
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
   10          IF (ABS(BIGA).LT.ABS(A(IJ)))THEN
                  BIGA=A(IJ)
                  L(K)=I
                  M(K)=J
               ENDIF
   20    CONTINUE
C
C     INTERCHANGE ROWS
C
         J=L(K)
         IF (J-K) 50,50,30
   30    KI=K-N
         DO 40 I=1,N
            KI=KI+N
            HOLD=-A(KI)
            JI=KI-K+J
            A(KI)=A(JI)
   40    A(JI)=HOLD
C
C     INTERCHANGE COLUMNS
C
   50    I=M(K)
         IF (I-K) 80,80,60
   60    JP=N*(I-1)
         DO 70 J=1,N
            JK=NK+J
            JI=JP+J
            HOLD=-A(JK)
            A(JK)=A(JI)
   70    A(JI)=HOLD
C
C     DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C     CONTAINED IN BIGA)
C
   80    IF (BIGA) 100,90,100
   90    D=0.0D0
         RETURN
  100    DO 120 I=1,N
            IF (I-K) 110,120,110
  110       IK=NK+I
            A(IK)=A(IK)/(-BIGA)
  120    CONTINUE
C  REDUCE MATRIX
         DO 150 I=1,N
            IK=NK+I
            HOLD=A(IK)
            IJ=I-N
            DO 150 J=1,N
               IJ=IJ+N
               IF (I-K) 130,150,130
  130          IF (J-K) 140,150,140
  140          KJ=IJ-I+K
               A(IJ)=HOLD*A(KJ)+A(IJ)
  150    CONTINUE
C
C     DIVIDE ROW BY PIVOT
C
         KJ=K-N
         DO 170 J=1,N
            KJ=KJ+N
            IF (J-K) 160,170,160
  160       A(KJ)=A(KJ)/BIGA
  170    CONTINUE
C
C     PRODUCT OF PIVOTS
C
         D=MAX(-1.D25,MIN(1.D25,D))
         D=D*BIGA
C
C     REPLACE PIVOT BY RECIPROCAL
C
         A(KK)=1.0D0/BIGA
  180 CONTINUE
C
C     FINAL ROW AND COLUMN INTERCHANGE
C
      K=N
  190 K=(K-1)
      IF (K) 260,260,200
  200 I=L(K)
      IF (I-K) 230,230,210
  210 JQ=N*(K-1)
      JR=N*(I-1)
      DO 220 J=1,N
         JK=JQ+J
         HOLD=A(JK)
         JI=JR+J
         A(JK)=-A(JI)
  220 A(JI)=HOLD
  230 J=M(K)
      IF (J-K) 190,190,240
  240 KI=K-N
      DO 250 I=1,N
         KI=KI+N
         HOLD=A(KI)
         JI=KI-K+J
         A(KI)=-A(JI)
  250 A(JI) =HOLD
      GO TO 190
  260 RETURN
      END
