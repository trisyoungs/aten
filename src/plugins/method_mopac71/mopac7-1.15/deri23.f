      SUBROUTINE DERI23 (F,FD,E,FCI,CMO,EMO,NORBS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION F(*), FD(*), E(*), FCI(*), CMO(NORBS,*), EMO(*)
***********************************************************************
*  1) UNPACK THE C.I-ACTIVE M.O. DERIVATIVES IN M.O. BASIS,
*     DIAGONAL BLOCKS INCLUDED.
*  2) EXTRACT THE FOCK EIGENVALUES RELAXATION OVER C.I-ACTIVE M.O.
*   INPUT
*     F           : UNSCALED SOLUTIONS VECTOR IN M.O. BASIS,
*                   OFF-DIAGONAL BLOCKS PACKED AS DEFINED IN 'DERI21'.
*     FD          : DIAGONAL BLOCKS OF NON-RELAXED FOCK MATRIX
*                   AS DEFINED IN 'DERI1'.
*     E(NORBS)    : FOCK EIGENVALUES.
*     FCI         : DIAGONAL BLOCKS OF RELAXATION OF THE FOCK MATRIX.
*     NORBS       : NUMBER OF M.O
*     NELEC,NMOS  : # OF LAST FROZEN CORE M.O , C.I-ACTIVE BAND LENGTH.
*   OUTPUT
*     CMO(N,NELEC+1,...,NELEC+NMOS): C.I-ACTIVE M.O DERIVATIVES
*                                  IN M.O BASIS.
*     EMO(  NELEC+1,...,NELEC+NMOS): C.I-ACTIVE FOCK EIGENVALUE RELAXATI
*
***********************************************************************
      COMMON /FOKMAT/ FDUMY(MPACK), SCALAR(MPACK)
      COMMON /NVOMAT/ DIAG(MPACK/2)
      COMMON /CIBITS/ NMOS,LAB,NELEC,NBO(3)
     1       /MOLKST/ NDUMY(4*NUMATM+8),FRACT
C
      NOPEN  =NBO(1)+NBO(2)
      CONST=1.D-3
C
C     PART 1.
C     -------
C     COMPUTE AND UNPACK DIAGONAL BLOCKS, DIAGONAL TERMS INCLUDED,
C     ACCORDING TO CMO(I,J) = (FD(I,J)-FCI(I,J))/(E(I)-E(J))
C     AND TAKING   CMO(I,J)=0 IF E(I)=E(J) (THRESHOLD 1D-4 EV),
C                             I.E WHEN M.O. DEGENERACY OCCURS.
      L=1
      NEND=0
      DO 30 LOOP=1,3
         NINIT=NEND+1
         NEND =NEND+NBO(LOOP)
         N1=MAX(NINIT,NELEC+1   )
         N2=MIN(NEND ,NELEC+NMOS)
         IF(N2.LT.N1) GO TO 30
         DO 20 I=N1,N2
            IF(I.GT.NINIT) THEN
               DO 10 J=NINIT,I-1
                  DIFFE=E(I)-E(J)
                  IF(ABS(DIFFE).GT.1.D-4) THEN
                     COM=(FD(L)-FCI(L))/DIFFE
                  ELSE
                     COM=0.D0
                  ENDIF
                  CMO(I,J)=-COM
                  CMO(J,I)= COM
   10          L=L+1
            ENDIF
   20    CMO(I,I)= 0.D0
   30 CONTINUE
C
C     C.I-ACTIVE EIGENVALUES RELAXATION.
      CALL SCOPY(NMOS,FCI(L),1,EMO(NELEC+1),1)
C
C     PART 2.
C     -------
C     UNPACK THE ANTISYMMETRIC MATRIX F IN CMO, (OFF-DIAGONAL BLOCKS).
C
      L=1
      IF(NBO(2).GT.0 .AND. NBO(1).GT.0) THEN
C        OPEN-CLOSED
         SCAL=1.D0/(2.D0-FRACT+CONST)
         DO 40 J=1       ,NBO(1)
            DO 40 I=NBO(1)+1,NOPEN
               COM=F(L)*SCAL
               CMO(I,J)=-COM
               CMO(J,I)= COM
   40    L=L+1
      ENDIF
      IF(NBO(3).GT.0 .AND. NBO(1).GT.0) THEN
C	 VIRTUAL-CLOSED
         SCAL=0.5D0
         DO 50 J=1     ,NBO(1)
            DO 50 I=NOPEN+1,NORBS
               COM=F(L)*SCAL
               CMO(I,J)=-COM
               CMO(J,I)= COM
   50    L=L+1
      ENDIF
      IF(NBO(3).NE.0 .AND. NBO(2).NE.0) THEN
C        VIRTUAL-OPEN
         SCAL=1.D0/(FRACT+CONST)
         DO 60 J=NBO(1)+1,NOPEN
            DO 60 I=NOPEN+1  ,NORBS
               COM=F(L)*SCAL
               CMO(I,J)=-COM
               CMO(J,I)= COM
   60    L=L+1
      ENDIF
      RETURN
      END
