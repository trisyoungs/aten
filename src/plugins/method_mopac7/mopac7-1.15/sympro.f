      SUBROUTINE SYMPOP(H,I,ISKIP,DELDIP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION H(*), DELDIP(3,*)
      COMMON /SYMOPS/ R(14,120), NSYM, IPO(NUMATM,120), NENT
      COMMON /SYMOPC/ ISYMT(6)
      CHARACTER*10 ISYMT
      COMMON /ATOMS/ COORD, NATOMS, NVAR
      DO 10 J = 1, NSYM
         IF (IPO(I,J).LT.I) THEN
            CALL SYMH(H, DELDIP, I, J)
            ISKIP=3
C  atom ipo(i,j) is suitable for transition dipole calc'n
C
            K=I*3-2
C
C   INSERT DELDIP ROTATION HERE
C
            GOTO 20
         ENDIF
   10 CONTINUE
      ISKIP=0
   20 CONTINUE
      RETURN
      END
      SUBROUTINE SYMR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /SYMOPS/ R(14,120), NSYM, IPO(NUMATM,120), NENT
      COMMON /SYMOPC/ ISYMT(6)
      CHARACTER*10 ISYMT
*****************************************************************
*
*   ON INPUT   NONE
*
*   ON OUTPUT  R    = SYMMETRY OPERATIONS
*              IPO  = PERMUTATION OPERATOR FOR SYMMETRY OPERATIONS
*              NSYM = NUMBER OF SYMMETRY OPERATIONS
*              NENT = NUMBER OF SYMMETRY OPERATIONS ENTERED
*
*****************************************************************
C
C  A SUBROUTINE THAT WILL READ IN THE PRIMATIVE SYMMETRY OPERATIONS
C     AND DETERMINE IF THEY ARE VALID FOR THIS MOLECULE.  THIS
C     INFORMATION IS THEN EXPANDED TO THE COMPLETE SET AND USED FOR
C     SYMMATRIZING THE HESSIAN.
C
C  THE CORRECT FORMAT FOR DESCRIBING A SYMMETRY FUNCTION IS:
C  LABEL  IFNCN  AXIS
C
C  WHERE:
C       LABEL  -  MUST BE INCLUDED AND IS THE LABEL THAT WILL
C                 BE USED TO IDENTIFY THAT FUNCTION
C       IFNCN  -  THE NUMBER OF THE SYMMETRY FUNCTION TO BE USED:
C                 0   -  INVERSION OPERATOR
C                 1   -  REFLECTION PLANE PERPENDICULAR TO THE AXIS
C                 2-X -  A C(N) AXIS
C                -3-X -  A S(N) AXIS
C       AXIS   -  THE AXIS FOR THE OPERATION.  MAY BE SPECIFIED AS:
C                 X, Y, Z COORDINATES -> MUST USE 3 VALUES.  AT LEAST ONE
C                     MUST BE NON-INTEGER OR TWO MUST BE IDENTICAL
C                 AN ATOM LIST -> THE COORDINATES OF THE ATOMS LISTED WILL
C                     BE SUMMED TO GENERATE THE AXIS.
C                 A -B ->  THE VECTOR FROM ATOM B TO ATOM A WILL BE USED AS
C                     THE AXIS.
C
C   A MAXIMUM OF 6 SYMMETRY OPERATIONS CAN BE INPUTTED.  THESE SHOULD BE THE
C      UNIQUE GENERATING FUNCTIONS FROM WHICH ALL THE OPERATIONS OF THE GROUP
C      CAN BE CONSTRUCTED.  E.G.  ONLY C5 NEEDS TO BE SPECIFIED SINCE C5(2) 
C      THROUGH C5(4) CAN BE GENERATED FROM THIS SINGLE FUNCTION.
C
C   A MAXIMUM OF 8 UNIQUE ATOMS CAN BE USED TO SPECIFY AN AXIS.
C
C   THE E FUNCTION IS BY DEFAULT THE FIRST SYMMETRY FUNCTION.  THIS FUNCTION
C      NEVER NEEDS TO BE EXPLICTLY INCLUDED IN YOUR LIST.  IT CANNOT BE 
C      ENTERED.
C
C   IF YOU ENTER A GIVEN SYMMETRY FUNCTION MORE THAN ONCE, ONLY THE FIRST
C      OCCURANCE WILL BE USED.  ALL DUPLICATES WILL BE DELETED.
C
      DIMENSION   TEMP(9), TEMP2(9),  ISTART(7)
      INTEGER    ITEMP(9)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /KEYWRD/ KEYWRD
      CHARACTER KEYWRD*241
      CHARACTER*80 LINE
      LOGICAL LEADSP, PROB, ALLINT
C  THE NEXT PARAMETERS ARE THE MAX NUMBER OF SYMM FUNCTIONS, THE 
C     MAX NUMBER OF SYMM FUNCTIONS TO READ IN, AND THE
C     TOLERENCE USED TO DETERMINE IF TWO FUNCTIONS ARE IDENTICAL
      PARAMETER (MAXFUN=120)
      PARAMETER (MAXENT=6)  
      PARAMETER (TOL   =1D-3)
C
C
C  Variables used:  (n represents the number of atomic centers)
C     TEMP(9), TEMP2(9):   Temporary matricies used to hold small parts
C          larger matricies for specific matrix operations.
C
C    For the next two items, the last index represents the symmetry
C        operation number.
C     R(9,*):   The 9 elements of each record are a packed 3 by 3
C          array of a given symmetry operations. 
C    IPO(n,*):  A vector that contains the symmetry mapping of atomic ce
C
      PROB = .FALSE.
C  Get the symmetry functions: (NOTE: THE FIRST IS ALWAY E)
      R(1,1) = 1.D0
      R(2,1) = 0.D0
      R(3,1) = 0.D0
      R(4,1) = 0.D0
      R(5,1) = 1.D0
      R(6,1) = 0.D0
      R(7,1) = 0.D0
      R(8,1) = 0.D0
      R(9,1) = 1.D0
C
      X = 0.D0
      Y = 0.D0
      Z = 0.D0
      DO 10 I=1,NUMAT
         X = X + COORD(1,I)
         Y = Y + COORD(2,I)
         Z = Z + COORD(3,I)
         IPO(I,1) = I
   10 CONTINUE
      XA = X/FLOAT(NUMAT)
      YA = Y/FLOAT(NUMAT)
      ZA = Z/FLOAT(NUMAT)
      DO 20 I=1,NUMAT
         COORD(1,I) = -XA + COORD(1,I)
         COORD(2,I) = -YA + COORD(2,I)
         COORD(3,I) = -ZA + COORD(3,I)
   20 CONTINUE
      WRITE(6,'(/''   SYMMETRY OPERATIONS USED FOR SYMMETRIZING'',
     1'' THE HESSIAN'')')
      WRITE(6,'(/,'' OPERATOR  TYPE     AXIS DEFINITION '')')
C
      NENT = 1
      NSYM = 0
      ISYMT(1)='E'
   30 NSYM = NSYM + 1
      READ(5,'(A)',END=120,ERR=120)LINE
      LEADSP=.TRUE.
      NVALUE=0
      ALLINT=.TRUE.
      DO 40 I=1,80
         IF (LEADSP.AND.LINE(I:I).NE.' ') THEN
            NVALUE=NVALUE+1
            ISTART(NVALUE)=I
         ENDIF
         LEADSP=(LINE(I:I).EQ.' ')
   40 CONTINUE
      IF (NVALUE.EQ.0) GOTO 120
      IF (NVALUE.EQ.1) THEN
        WRITE(6,200)
 200    FORMAT(' NOT A VALID LINE. ONLY HAS ONE ENTRY')
        PROB = .TRUE.
        GOTO 120
      ENDIF
      ISYMT(1+NENT)=LINE(ISTART(1):ISTART(2)-1)
      DO 50 I=2,NVALUE
        TEMP(I-1)=READA(LINE,ISTART(I))
        ITEMP(I-1)=NINT(TEMP(I-1))
        IF((ABS(ITEMP(I-1)-TEMP(I-1)).GT.TOL).AND.(I.NE.2)) 
     +    ALLINT=.FALSE.
   50 CONTINUE
      IF (ALLINT) THEN
        WRITE(6,210)ISYMT(1+NENT),(ITEMP(I),I=1,NVALUE-1)
 210  FORMAT(X,A10,I7,8I7)
      ELSE
        WRITE(6,220)ISYMT(1+NENT),ITEMP(1),(TEMP(I),I=2,NVALUE-1)
 220  FORMAT(X,A10,I7,8F7.3)
      ENDIF
      SIGMA = 1
      IF (ITEMP(1) .LE. -3) SIGMA = -1
      TEMP(1) = ABS( TEMP(1))
      ITEMP(1)= ABS(ITEMP(1))
      IF (ABS(ITEMP(1)-TEMP(1)) .GE. TOL) THEN
        WRITE(6,230)
 230    FORMAT(' THE SYMMETRY FUNCTION MUST BE INTEGER')
        PROB = .TRUE.
        GOTO 120
      ENDIF
      IF (ITEMP(1) .EQ. 0) THEN
C  WITH INVERSION, THE AXIS IS UNIMPORTANT
        R(1,1+NENT) = -1.D0
        R(2,1+NENT) =  0.D0
        R(3,1+NENT) =  0.D0
        R(4,1+NENT) =  0.D0
        R(5,1+NENT) = -1.D0
        R(6,1+NENT) =  0.D0
        R(7,1+NENT) =  0.D0
        R(8,1+NENT) =  0.D0
        R(9,1+NENT) = -1.D0
        GOTO 70
      ENDIF
C  WITH ANYTHING ELSE, THE AXIS MUST BE DETERMINED.  IF NO AXIS IS DEFINED
C    FLAG IT AS A PROBLEM
      IF (NVALUE .EQ. 2) THEN
        PROB = .TRUE.
        WRITE(6,240) NSYM
 240    FORMAT(' NO AXIS INFORMATION WAS ENTERED FOR FUNCTION',I2)
        GOTO 120
      ENDIF
      IF ((NVALUE .EQ. 5).AND.((TEMP(2).EQ.TEMP(3))
     +     .OR.(TEMP(2).EQ.TEMP(4)).OR.
     +     (TEMP(3).EQ.TEMP(4)).OR.(.NOT. ALLINT).OR.
     +     (ABS(ITEMP(2)).LT. 1).OR.(ABS(ITEMP(2)).GT.NUMAT) .OR.
     +     (ABS(ITEMP(3)).LT. 1).OR.(ABS(ITEMP(3)).GT.NUMAT) .OR.
     +     (ABS(ITEMP(4)).LT. 1).OR.(ABS(ITEMP(4)).GT.NUMAT))) THEN
C  IT APPEARS TO BE XYZ INPUT
        X = TEMP(2)
        Y = TEMP(3)
        Z = TEMP(4)
      ELSE
C  APPEARS TO BE ATOM NUMBER INPUT
        IF (.NOT. ALLINT) THEN
          PROB = .TRUE.
          WRITE(6,250)
 250      FORMAT(' YOU MUST HAVE ALL INTEGER INPUT WHEN NOT',
     +     ' USING XYZ INPUT')
          GOTO 120
        ENDIF
        X = 0.D0
        Y = 0.D0
        Z = 0.D0
        DO 60 I = 2, NVALUE-1
          IF ((ABS(ITEMP(I)).LT.1).OR.(ABS(ITEMP(I)).GT.NUMAT)) THEN
            WRITE(6,260)ITEMP(I)
 260        FORMAT(' ATOM NUMBER',I3,' IS OUT OF RANGE')
            PROB=.TRUE.
          ENDIF
          X = X + ITEMP(I)/ABS(ITEMP(I))*COORD(1,ABS(ITEMP(I)))
          Y = Y + ITEMP(I)/ABS(ITEMP(I))*COORD(2,ABS(ITEMP(I)))
          Z = Z + ITEMP(I)/ABS(ITEMP(I))*COORD(3,ABS(ITEMP(I)))
  60    CONTINUE
      ENDIF
C
C  TIME TO DECIPHER THE SYMMETRY FUNCTION
C
      IF (ITEMP(1) .GT. 10) THEN
        WRITE(6,270)
 270    FORMAT(' A C-10 AXIS IS THE HIGHEST THAT CAN BE SPECIFIED')
        PROB = .TRUE.
        GOTO 120
      ENDIF
      ROT=4.D0*ASIN(1.D0)/ITEMP(1)
      IF(ITEMP(1).EQ. 1) SIGMA = -1
C
C  First, construct the matrix defining the rotation axis
      XY=X**2+Y**2
      RA=SQRT(XY+Z**2)
      IF (RA.LT. TOL) THEN
        PROB = .TRUE.
        WRITE(6,280)
  280   FORMAT('  YOUR VECTOR AXIS MUST HAVE A NON-ZERO LENGTH ')
        GOTO 120
      ENDIF
      XY=SQRT(XY)
      IF (XY.GT.1.D-10) THEN
        CA=Y/XY
        CB=Z/RA
        SA=X/XY
        SB=XY/RA
      ELSEIF (Z.GT. 0.D0) THEN
        CA=1.D0
        CB=1.D0
        SA=0.D0
        SB=0.D0
      ELSE
        CA=-1.D0
        CB=-1.D0
        SA=0.D0
        SB=0.D0
      ENDIF
C  GENERATE THE MATRIX ELEMENTS BY DOING THE EULER TRANSFORM
      TEMP( 1)=CA
      TEMP( 2)=-SA
      TEMP( 3)=0.D0
      TEMP( 4)=SA*CB
      TEMP( 5)=CA*CB
      TEMP( 6)=-SB
      TEMP( 7)=SA*SB
      TEMP( 8)=CA*SB
      TEMP( 9)=CB
C
C
      CA = DCOS(ROT)
      SA = DSIN(ROT)
C
C  The construct the actual R matrix to be used
C
C
      TEMP2(1) = CA
      TEMP2(2) = SA
      TEMP2(3) = 0.D0
      TEMP2(4) = -SA
      TEMP2(5) = CA
      TEMP2(6) = 0.D0
      TEMP2(7) = 0.D0
      TEMP2(8) = 0.D0
      TEMP2(9) = SIGMA  
      CALL MAT33(TEMP, TEMP2, R(1,1+NENT))
C
C  Now, verify that this is a unique and valid function
 70   CONTINUE
C
      RES = 10.D0
      DO 90 I = 2, NENT
        RESO = 0.D0
        DO 80 J  = 1, 9
  80      RESO= ABS( R(J,I) - R(J,1+NENT)) + RESO
        RES = MIN(RES, RESO)
  90  CONTINUE
      IF (RES .LT. TOL) THEN
C  THIS IS NOT VALID FUNCTION
        WRITE(6,290)
 290    FORMAT(' THIS FUNCTION IS IDENTICAL TO AN EARLIER ONE')
        GOTO 120
      ENDIF
C  NOW, TO CALCULATE THE IPO OF THIS FUNCTION
      NENT = 1+NENT
      N = NENT
C  Now, to initialize IPO(n) and
C  Perform R on each atomic center and determine where it maps to.
      DO 110 I = 1, NUMAT
        X=COORD(1,I)*R(1,N) + COORD(2,I)*R(2,N) + COORD(3,I)*R(3,N)
        Y=COORD(1,I)*R(4,N) + COORD(2,I)*R(5,N) + COORD(3,I)*R(6,N)
        Z=COORD(1,I)*R(7,N) + COORD(2,I)*R(8,N) + COORD(3,I)*R(9,N)
        IPO(I,N) = 0
        DO 100 J = 1, NUMAT
          DIST=ABS(X-COORD(1,J))+ABS(Y-COORD(2,J))+ABS(Z-COORD(3,J))
          IF (DIST .LT. 5.D-2) THEN
            IF (IPO(I,N) .EQ. 0) THEN
              IPO(I,N) = J
            ELSE
              WRITE(6,300)
              PROB = .TRUE.
              GOTO 120
  300         FORMAT('  ONE ATOM MAPS ONTO TWO DIFFERENT ATOMIC C',
     1'ENTERS')
            ENDIF
          ENDIF
  100   CONTINUE
        IF (IPO(I,N) .EQ. 0)  THEN
          WRITE(6,310)
  310     FORMAT('  ONE ATOM MAPS ONTO NO OTHER ATOM ')
          PROB = .TRUE.
          GOTO 120
        ENDIF
  110 CONTINUE
C
C  IF THIS POINT IS REACHED, THE FUNCTION IS VALID
C  CHECK IF THE R MATRIX SHOULD BE PRINTED
C
      IF (INDEX(KEYWRD,' RMAT') .NE. 0) THEN
        WRITE(6,320)(R(I,N),I=1,3)
        WRITE(6,330)N,(R(I,N),I=4,6)
        WRITE(6,340)(R(I,N),I=7,9)
  320   FORMAT(/,10X,'| ',3F10.6,' |')
  330   FORMAT(I5,' =   | ',3F10.6,' |')
  340   FORMAT(10X,'| ',3F10.6,' |',/)
      ENDIF
C
 120  IF((NVALUE.NE.0).AND.(NSYM.LT.MAXENT)) GOTO 30
C
C  If a problem exists.  Stop the program.
C
      IF (PROB) THEN
        CLOSE (6)
        STOP 'PROBLEM IN SYMR'
      ENDIF
C
C  NOW, ALL USER FUNCTIONS ARE IN WITH NO ERRORS (JUST ELIMINATION OF DUPS)
C
      IF(NVALUE.NE.0) READ(5,'(A)',END=130)LINE
 130  CONTINUE
      NSYM = NENT
C
C  NEXT, EXPAND THE EXISTING OPERATORS TO THE FULL SET
C
      CALL SYMP
C
      RETURN
      END
      SUBROUTINE SYMH(H, DIP, I, N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION H(*), DIP(3,*)
      COMMON /SYMOPS/ R(14,120), NSYM, IPO(NUMATM,120), NENT
      COMMON /SYMOPC/ ISYMT(6)
      CHARACTER*10 ISYMT
*****************************************************************
*
C  INPUT:   H()   A packed lower triangular hessian
C           DIP(,) A MATRIX OF DIPOLE TENSORS TO BE SYMM
C           R(,)  A matrix of symmetry operations
C           IPO(,) A matrix of atomic mapping according to R
C           I     The atom (row and column) to add to H()
C           N     The symmetry operation to use to generate I
C
C  OUTPUT:  H()   A packed lower triangular Hessian with information
C                   about atom I added
C           DIP(,) A MATRIX OF DIPOLE TENSORS THAT HAVE BEEN SYMM
C
*****************************************************************
C
C
C  This subroutine will add all necessary information to the Hessian con
C    atom I.  Since the Hessian is a packed lower half triangle, the exi
C    information for atom pair (K,L) where K,L < I is fully known, (K >
C    L < I) or (vice versa) is half known, K,L > I is completely unknown
C    Therefore, start in unknown region and make it half known.  Double
C    known values, and move in the diagonal element at full strength.
C
C
      DIMENSION   TEMP(9), TEMP2(9)
      COMMON /FOKMAT/ HA(MPACK*2)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /COORD / COORD(3,NUMATM)
C
C
C
C  Variables used:  (n represents the number of atomic centers)
C     H(3n,3n):  Input/output matrix.  It is a packed lower half triangu
C        matrix.  Commonly, the Hessian.
C     TEMP(9), TEMP2(9):   Temporary matricies used to hold small parts
C          larger matricies for specific matrix operations.
C
C    For the next two items, the last indicy represents the symmetry
C        operation number.
C     R(14,*):   The first 9 elements of each record are a packed 3 by 3
C          array of a given symmetry operations.  Elements 10 - 14 are t
C          users input describing the symmetry operation.
C     IPO(n,*):  A vector that contains the symmetry mapping of atomic c
C
C
      K = IPO(I,N)
C
C  Now, to climb up the matrix
      DO 10 J = NUMAT, I+1, -1
         L = IPO(J,N)
C
C  Now, to actually perform R H R
C
C  Do this multiplication in a 3 by 3 block at a time.  Store H(i,j) in
C    H( IPO(I,N), IPO(J,N))
C
         IF (K .GT. L) THEN
            IEL33 = (3*K*(3*K-1))/2 + 3*L
            TEMP(9) = 0.5D0 * H(IEL33)
            TEMP(8) = 0.5D0 * H(IEL33-1)
            TEMP(7) = 0.5D0 * H(IEL33-2)
            TEMP(6) = 0.5D0 * H(IEL33-K*3+1)
            TEMP(5) = 0.5D0 * H(IEL33-K*3)
            TEMP(4) = 0.5D0 * H(IEL33-K*3-1)
            TEMP(3) = 0.5D0 * H(IEL33-6*K+3)
            TEMP(2) = 0.5D0 * H(IEL33-6*K+2)
            TEMP(1) = 0.5D0 * H(IEL33-6*K+1)
         ELSE
            IEL33 = (3*L*(3*L-1))/2 + 3*K
            FACT = 1.0D0
            IF (L .LT. I) FACT = 0.5D0
            TEMP(9) = FACT * H(IEL33)
            TEMP(6) = FACT * H(IEL33-1)
            TEMP(3) = FACT * H(IEL33-2)
            TEMP(8) = FACT * H(IEL33-L*3+1)
            TEMP(5) = FACT * H(IEL33-L*3)
            TEMP(2) = FACT * H(IEL33-L*3-1)
            TEMP(7) = FACT * H(IEL33-6*L+3)
            TEMP(4) = FACT * H(IEL33-6*L+2)
            TEMP(1) = FACT * H(IEL33-6*L+1)
         ENDIF
C
         CALL MAT33(R(1,N), TEMP, TEMP2)
C
         IEL33 = J*3*(J*3-1)/2 + I*3
         H(IEL33)       = TEMP2(9)
         H(IEL33-3*J+1) = TEMP2(8)
         H(IEL33-6*J+3) = TEMP2(7)
         H(IEL33-1)     = TEMP2(6)
         H(IEL33-3*J)   = TEMP2(5)
         H(IEL33-6*J+2) = TEMP2(4)
         H(IEL33-2)     = TEMP2(3)
         H(IEL33-3*J-1) = TEMP2(2)
         H(IEL33-6*J+1) = TEMP2(1)
   10 CONTINUE
C
C  Now, to do the diagonal term
C
      IEL33 = (3*K*(3*K+1))/2
      TEMP(9) = 0.5D0 * H(IEL33)
      TEMP(8) = 0.5D0 * H(IEL33-1)
      TEMP(7) = 0.5D0 * H(IEL33-2)
      TEMP(6) = TEMP(8)
      TEMP(5) = 0.5D0 * H(IEL33-K*3)
      TEMP(4) = 0.5D0 * H(IEL33-K*3-1)
      TEMP(3) = TEMP(7)
      TEMP(2) = TEMP(4)
      TEMP(1) = 0.5D0 * H(IEL33-6*K+1)
C
      CALL MAT33(R(1,N), TEMP, TEMP2)
C
      IEL33 = I*3*(I*3+1)/2
      H(IEL33)       = TEMP2(9)
      H(IEL33-1)     = TEMP2(8)
      H(IEL33-2)     = TEMP2(7)
      H(IEL33-I*3)   = TEMP2(5)
      H(IEL33-I*3-1) = TEMP2(4)
      H(IEL33-6*I+1) = TEMP2(1)
C
C   NOW, TO ROTATE THE DIPOLE TENSOR TERM
C
      TEMP(9) = DIP(3,K*3  )
      TEMP(8) = DIP(2,K*3  )
      TEMP(7) = DIP(1,K*3  )
      TEMP(6) = DIP(3,K*3-1)
      TEMP(5) = DIP(2,K*3-1)
      TEMP(4) = DIP(1,K*3-1)
      TEMP(3) = DIP(3,K*3-2)
      TEMP(2) = DIP(2,K*3-2)
      TEMP(1) = DIP(1,K*3-2)
C
      CALL MAT33(R(1,N), TEMP, TEMP2)
C
      DIP(3,I*3  ) = TEMP2(9)
      DIP(2,I*3  ) = TEMP2(8)
      DIP(1,I*3  ) = TEMP2(7)
      DIP(3,I*3-1) = TEMP2(6)
      DIP(2,I*3-1) = TEMP2(5)
      DIP(1,I*3-1) = TEMP2(4)
      DIP(3,I*3-2) = TEMP2(3)
      DIP(2,I*3-2) = TEMP2(2)
      DIP(1,I*3-2) = TEMP2(1)
C
C
C   Now, to double all existing values going across
      ISTART = (I-1)*3*((I-1)*3+1)/2+1
      DO 20 J = ISTART, IEL33
         H(J) = H(J) + H(J)
   20 CONTINUE
C  Everything is now done for this symmetry element.
C
      RETURN
      END
      SUBROUTINE SYMA(E, V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION  E(NUMAT*3), V(NUMAT*NUMAT*9)
      COMMON /SYMOPS/ R(14,120), NSYM, IPO(NUMATM,120), NENT
      COMMON /SYMOPC/ ISYMT(6)
      CHARACTER*10 ISYMT
*********************************************************************
*
*  ON INPUT E    = FREQUENCIES IN CM(-1)
*           V    = EIGENVECTORS OF NORMAL MODES, NORMALIZED
*           R    = SYMMETRY OPERATIONS
*           IPO  = MAP OF ATOMS BEING MOVED
*           NSYM = NUMBER OF SYMMETRY OPERATION
*
*********************************************************************
C
C  THIS SUBROUTINE DETERMINES THE SYMMETRY FUNCTION VALUE OF EACH
C    VIBRATIONAL MODE.  IT DOES IT BY DOING <EV R EV>
C
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /KEYWRD/ KEYWRD
      DIMENSION  T1(MAXPAR), T2(MAXPAR,7)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      CHARACTER KEYWRD*241
C
      TOL=1.D-3
C
      NVAR=NUMAT*3
C
C  T1(NVAR) AND T2(NVAR,NSYM) ARE THE ONLY ADDITIONAL ARRAYS NEEDED.  TH
C    ARE TEMPORARY ARRAYS.
      DO 30 K = 0, NVAR-1
         DO 20 N = 1, NENT
            DO 10 I = 1, NUMAT
C
               J=IPO(I,N)
               T1(I*3-2)=V(J*3-2+K*NVAR)*R(1,N)+
     1              V(J*3-1+K*NVAR)*R(4,N)+
     2              V(J*3  +K*NVAR)*R(7,N)
               T1(I*3-1)=V(J*3-2+K*NVAR)*R(2,N)+
     1              V(J*3-1+K*NVAR)*R(5,N)+
     2              V(J*3  +K*NVAR)*R(8,N)
               T1(I*3  )=V(J*3-2+K*NVAR)*R(3,N)+
     1              V(J*3-1+K*NVAR)*R(6,N)+
     2              V(J*3  +K*NVAR)*R(9,N)
   10       CONTINUE
            T2(K+1,N) = 0.0D0
            DO 20 I = 1, NVAR
               T2(K+1,N) = T2(K+1,N) + T1(I)*V(I+K*NVAR)
   20    CONTINUE
   30 CONTINUE
      WRITE(6,100)
      WRITE(6,'(''                    '',7A9)')(ISYMT(I),I=1,NENT)
  100 FORMAT('  FREQ.',/,'  NO.   FREQ.         CHARACTER TABLE ')
      I=1
      J=I+1
      IF (INDEX(KEYWRD,' NODEGEN') .NE. 0) TOL = -1.D0
      EREF = E(1)
  110 IF(ABS((E(J)-EREF)) .LE. TOL) THEN
         DO 120 K = 1, NENT
  120    T2(I,K) = T2(I,K) + T2(J,K)
         E(I) = (E(I) + E(J))
         J = J+1
      ELSE
         E(I)=E(I)/FLOAT(J-I)
         WRITE(6,130)I,E(I),(T2(I,K),K=1,NENT)
         I=J
         J=J+1
         EREF=E(I)
      ENDIF
      IF (J .LE. NVAR) GOTO 110
      E(I)=E(I)/FLOAT(J-I)
      WRITE(6,130)I,E(I),(T2(I,K),K=1,NENT)
  130 FORMAT(I4,F9.3,3X,7F9.4)
      END
      SUBROUTINE SYMT(H, DELDIP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION H(*), DELDIP(3,*)
      COMMON /SYMOPS/ R(14,120), NSYM, IPO(NUMATM,120), NENT
      COMMON /SYMOPC/ ISYMT(6)
      CHARACTER*10 ISYMT
*****************************************************************
*
*   ON INPUT   H    = HESSIAN MATRIX, PACKED LOWER HALF TRIANGLE
*              R    = SYMMETRY OPERATIONS
*              IPO  = MAP OF ATOMS MOVED
*              NSYM = NUMBER OF SYMMETRY OPERATIONS
*
*   ON OUTPUT  H    = SYMMETRIZED HESSIAN MATRIX
*
*****************************************************************
C  A subroutine that will symmatrize the Hamiltonian, or other matrix
C     by successive application of group operations.  The method used
C     is R H R  added to HA then divided by the total number of symmetry
C     operations used.  This in effects averages all the values in a
C     symmetry correct fashion.
C
      DIMENSION   TEMP(9), TEMP2(9), DELTMP(3,MAXPAR)
      COMMON /FOKMAT/ HA(MPACK*2)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /COORD / COORD(3,NUMATM)
C
C
C  Variables used:  (n represents the number of atomic centers)
C     H(3n,3n):  Input/output matrix.  It is a packed lower half triangu
C        matrix.  Commonly, the Hessian.
C     HA(3n,3n): An internal matrix used to sum the symatrized Hessian
C     NSYM:      Input, the value of this symmetry operation.
C     TEMP(9), TEMP2(9):   Temporary matricies used to hold small parts
C          larger matricies for specific matrix operations.
C
C    For the next two items, the last indicy represents the symmetry
C        operation number.
C     IPO(n,*):  A vector that contains the symmetry mapping of atomic c
C
C   Skip this subroutine if NSYMM <= 0.  This implies that only E is pre
      IF (NSYM .LT. 2) RETURN
C
      DO 10 I=1,(3*NUMAT*(NUMAT*3+1))/2
   10 HA(I)=0.D0
C
      DO 15 I = 1, NUMAT*3
        DELTMP(1,I) = 0.D0
        DELTMP(2,I) = 0.D0
   15   DELTMP(3,I) = 0.D0
C
      DO 40 N = 1, NSYM
C
C  Now, to actually perform R H R
         DO 30 I = 1, NUMAT
            DO 20 J = 1, I-1
C
C  Do this multiplication in a 3 by 3 block at a time.  Store H(i,j) in
C    HA( IPO(I,N), IPO(J,N)) or HS( IPO(I,N), IPO(J,N))
C
               K = IPO(I,N)
               L = IPO(J,N)
               IF (K .GT. L) THEN
                  IEL33 = (3*K*(3*K-1))/2 + 3*L
                  TEMP(9) = H(IEL33)
                  TEMP(8) = H(IEL33-1)
                  TEMP(7) = H(IEL33-2)
                  TEMP(6) = H(IEL33-K*3+1)
                  TEMP(5) = H(IEL33-K*3)
                  TEMP(4) = H(IEL33-K*3-1)
                  TEMP(3) = H(IEL33-6*K+3)
                  TEMP(2) = H(IEL33-6*K+2)
                  TEMP(1) = H(IEL33-6*K+1)
               ELSE
                  IEL33 = (3*L*(3*L-1))/2 + 3*K
                  TEMP(9) = H(IEL33)
                  TEMP(6) = H(IEL33-1)
                  TEMP(3) = H(IEL33-2)
                  TEMP(8) = H(IEL33-L*3+1)
                  TEMP(5) = H(IEL33-L*3)
                  TEMP(2) = H(IEL33-L*3-1)
                  TEMP(7) = H(IEL33-6*L+3)
                  TEMP(4) = H(IEL33-6*L+2)
                  TEMP(1) = H(IEL33-6*L+1)
               ENDIF
C
               CALL MAT33(R(1,N), TEMP, TEMP2)
C
               IEL33 = I*3*(I*3-1)/2 + J*3
               HA(IEL33)       = TEMP2(9) + HA(IEL33)
               HA(IEL33-1)     = TEMP2(8) + HA(IEL33-1)
               HA(IEL33-2)     = TEMP2(7) + HA(IEL33-2)
               HA(IEL33-I*3+1) = TEMP2(6) + HA(IEL33-I*3+1)
               HA(IEL33-I*3)   = TEMP2(5) + HA(IEL33-I*3)
               HA(IEL33-I*3-1) = TEMP2(4) + HA(IEL33-I*3-1)
               HA(IEL33-6*I+3) = TEMP2(3) + HA(IEL33-6*I+3)
               HA(IEL33-6*I+2) = TEMP2(2) + HA(IEL33-6*I+2)
               HA(IEL33-6*I+1) = TEMP2(1) + HA(IEL33-6*I+1)
   20       CONTINUE
            K = IPO(I,N)
            IEL33 = (3*K*(3*K+1))/2
            TEMP(9) = H(IEL33)
            TEMP(8) = H(IEL33-1)
            TEMP(7) = H(IEL33-2)
            TEMP(6) = TEMP(8)
            TEMP(5) = H(IEL33-K*3)
            TEMP(4) = H(IEL33-K*3-1)
            TEMP(3) = TEMP(7)
            TEMP(2) = TEMP(4)
            TEMP(1) = H(IEL33-6*K+1)
C
            CALL MAT33(R(1,N), TEMP, TEMP2)
C
            IEL33 = I*3*(I*3+1)/2
            HA(IEL33)       = TEMP2(9) + HA(IEL33)
            HA(IEL33-1)     = TEMP2(8) + HA(IEL33-1)
            HA(IEL33-2)     = TEMP2(7) + HA(IEL33-2)
            HA(IEL33-I*3)   = TEMP2(5) + HA(IEL33-I*3)
            HA(IEL33-I*3-1) = TEMP2(4) + HA(IEL33-I*3-1)
            HA(IEL33-6*I+1) = TEMP2(1) + HA(IEL33-6*I+1)
C
C  APPLY SYMMETRY TO DIPOLE TERM AS WELL
C
            TEMP(9) = DELDIP(3,K*3  )
            TEMP(8) = DELDIP(2,K*3  )
            TEMP(7) = DELDIP(1,K*3  )
            TEMP(6) = DELDIP(3,K*3-1)
            TEMP(5) = DELDIP(2,K*3-1)
            TEMP(4) = DELDIP(1,K*3-1)
            TEMP(3) = DELDIP(3,K*3-2)
            TEMP(2) = DELDIP(2,K*3-2)
            TEMP(1) = DELDIP(1,K*3-2)
C
            CALL MAT33(R(1,N), TEMP, TEMP2)
C
            DELTMP(3,I*3  ) = TEMP2(9) + DELTMP(3,I*3  )
            DELTMP(2,I*3  ) = TEMP2(8) + DELTMP(2,I*3  )
            DELTMP(1,I*3  ) = TEMP2(7) + DELTMP(1,I*3  )
            DELTMP(3,I*3-1) = TEMP2(6) + DELTMP(3,I*3-1)
            DELTMP(2,I*3-1) = TEMP2(5) + DELTMP(2,I*3-1)
            DELTMP(1,I*3-1) = TEMP2(4) + DELTMP(1,I*3-1)
            DELTMP(3,I*3-2) = TEMP2(3) + DELTMP(3,I*3-2)
            DELTMP(2,I*3-2) = TEMP2(2) + DELTMP(2,I*3-2)
            DELTMP(1,I*3-2) = TEMP2(1) + DELTMP(1,I*3-2)
C
   30    CONTINUE
   40 CONTINUE
C
      DO 50 I = 1, (NUMAT*3*(NUMAT*3+1))/2
   50 H(I) = HA(I)/NSYM
C
      DO 60 I = 1, 3*NUMAT
        DELDIP(1,I) = DELTMP(1,I)/NSYM
        DELDIP(2,I) = DELTMP(2,I)/NSYM
   60   DELDIP(3,I) = DELTMP(3,I)/NSYM
C
      RETURN
      END
      SUBROUTINE MAT33(A, B, C)
C  A subroutine that will multiply two 3 by 3 matricies in the following
C     fashion:    C = A(transpose) B A
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(9), B(9), C(9), T(9)
C
C
      T(1) = B(1)*A(1) + B(2)*A(4) + B(3)*A(7)
      T(2) = B(1)*A(2) + B(2)*A(5) + B(3)*A(8)
      T(3) = B(1)*A(3) + B(2)*A(6) + B(3)*A(9)
      T(4) = B(4)*A(1) + B(5)*A(4) + B(6)*A(7)
      T(5) = B(4)*A(2) + B(5)*A(5) + B(6)*A(8)
      T(6) = B(4)*A(3) + B(5)*A(6) + B(6)*A(9)
      T(7) = B(7)*A(1) + B(8)*A(4) + B(9)*A(7)
      T(8) = B(7)*A(2) + B(8)*A(5) + B(9)*A(8)
      T(9) = B(7)*A(3) + B(8)*A(6) + B(9)*A(9)
C
      C(1) = A(1)*T(1) + A(4)*T(4) + A(7)*T(7)
      C(2) = A(1)*T(2) + A(4)*T(5) + A(7)*T(8)
      C(3) = A(1)*T(3) + A(4)*T(6) + A(7)*T(9)
      C(4) = A(2)*T(1) + A(5)*T(4) + A(8)*T(7)
      C(5) = A(2)*T(2) + A(5)*T(5) + A(8)*T(8)
      C(6) = A(2)*T(3) + A(5)*T(6) + A(8)*T(9)
      C(7) = A(3)*T(1) + A(6)*T(4) + A(9)*T(7)
      C(8) = A(3)*T(2) + A(6)*T(5) + A(9)*T(8)
      C(9) = A(3)*T(3) + A(6)*T(6) + A(9)*T(9)
C
      RETURN
      END
      SUBROUTINE SYMP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /SYMOPS/ R(14,120), NSYM, IPO(NUMATM,120), NENT
      COMMON /SYMOPC/ ISYMT(6)
      CHARACTER*10 ISYMT
      CHARACTER*5 OPER
*****************************************************************
*
*   ON INPUT   R    = SYMMETRY OPERATIONS (7 MAX)
*              IPO  = PERM OPR FOR ABOVE OPERATIONS
*              NSYM = CURRENT NUMBER OF SYMMETRY OPERATIONS
*              NENT = NUMBER OF USER SUPPLIED OPERATIONS
*
*   ON OUTPUT  R    = SYMMETRY OPERATIONS (120 MAX)
*              IPO  = PERMUTATION OPERATOR FOR SYMMETRY OPERATIONS
*              NSYM = NUMBER OF SYMMETRY OPERATIONS
*
*****************************************************************
C
C  A SUBROUTINE THAT WILL EXPAND THE SYMMETRY OPERATIONS READ IN INTO
C     THE COMPLETE SET.  NOTE: VERY FEW OPERATIONS ARE REQUIRED TO
C     GENERATE EVEN VERY LARGE GROUPS OF OPERATIONS.
C
C
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /COORD / COORD(3,NUMATM)
      COMMON /KEYWRD/ KEYWRD
      CHARACTER KEYWRD*241
C  THE NEXT PARAMETERS ARE THE MAX NUMBER OF SYMM FUNCTIONS, THE 
C     MAX NUMBER OF SYMM FUNCTIONS TO READ IN, AND THE
C     TOLERENCE USED TO DETERMINE IF TWO FUNCTIONS ARE IDENTICAL
      PARAMETER (MAXFUN=120)
      PARAMETER (TOL   =1D-2)
C
C
C  Variables used:  (n represents the number of atomic centers)
C
C    For the next two items, the last index represents the symmetry
C        operation number.
C     R(9,*):   The 9 elements of each record are a packed 3 by 3
C          array of a given symmetry operations. 
C    IPO(n,*):  A vector that contains the symmetry mapping of atomic ce
C
C  NSYM IS ALWAYS THE UPPER BOUND OF THE VALID FUNCTIONS.  QUIT IF IT 
C     REACHES 120.
C  I IS THE SLOW INDEX OF FUNCTIONS TO MULTIPLY
C  J IS THE FAST INDEX OF FUNCTIONS TO MULTIPLY
C  ALWAYS DO R(I)*R(J) AND TAKE I,J FROM 2 TO NSYM
C
      I = 2
      J = 1
C
C  IF MORE INFORMATION IS WANTED, PRINT HEADDER.
C
      IF (INDEX(KEYWRD,' RMAT') .NE. 0)WRITE(6,100)
 100  FORMAT(/,' ENTERING THE SYMMETRY GENERATING ROUTINE ',
     +/,'    NUMBER  SYMM. OPER.   * ',
     +   '   NUMBER  SYMM. OPER.   = ',
     +   '   NUMBER  SYMM. OPER.')
C  DETERMINE IF IT IS TIME TO STOP
C
  10  J = J+1
      IF (J .GT. NSYM) THEN
        J = 2
        I = I+1
        IF (I .GT. NSYM) GOTO 50  
      ENDIF
      IF(NSYM .EQ. MAXFUN) GOTO 50  
C
C  NOW TO START THE MULTIPLICATION
C
      R(1,NSYM+1)=R(1,I)*R(1,J)+R(2,I)*R(4,J)+R(3,I)*R(7,J)
      R(2,NSYM+1)=R(1,I)*R(2,J)+R(2,I)*R(5,J)+R(3,I)*R(8,J)
      R(3,NSYM+1)=R(1,I)*R(3,J)+R(2,I)*R(6,J)+R(3,I)*R(9,J)
      R(4,NSYM+1)=R(4,I)*R(1,J)+R(5,I)*R(4,J)+R(6,I)*R(7,J)
      R(5,NSYM+1)=R(4,I)*R(2,J)+R(5,I)*R(5,J)+R(6,I)*R(8,J)
      R(6,NSYM+1)=R(4,I)*R(3,J)+R(5,I)*R(6,J)+R(6,I)*R(9,J)
      R(7,NSYM+1)=R(7,I)*R(1,J)+R(8,I)*R(4,J)+R(9,I)*R(7,J)
      R(8,NSYM+1)=R(7,I)*R(2,J)+R(8,I)*R(5,J)+R(9,I)*R(8,J)
      R(9,NSYM+1)=R(7,I)*R(3,J)+R(8,I)*R(6,J)+R(9,I)*R(9,J)
C
C  IS IT UNIQUE?
C
      DO 30 N = 1, NSYM
        RES = 0.D0
        DO 20 M = 1, 9
  20    RES = RES + ABS(R(M,N) - R(M,NSYM+1))
      IF (RES .LT. TOL) GOTO 10
  30  CONTINUE
C
C  YES, IT IS UNIQUE.  NOW, GENERATE THE NEW IPO(,NSYM)
C
      NSYM = NSYM + 1
      DO 40 N = 1, NUMAT
  40  IPO(N,NSYM) = IPO(IPO(N,J),I)
C
C     ALL DONE ADDING THE NEW FUNCTION.  GO TRY TO FIND A NEW ONE.
C     BUT FIRST, SEE IF WE NEED TO PRINT THIS.
C
      IF (INDEX(KEYWRD,' RMAT') .NE. 0) 
     +   WRITE(6,110)I,OPER(R(1,I)),J,OPER(R(1,J)),NSYM,OPER(R(1,NSYM))
 110  FORMAT(8X,I3,6X,A5,4X,'*',8X,I3,6X,A5,4X,'=',8X,I3,6X,A5)
      IF (INDEX(KEYWRD,' RMAT') .NE. 0) THEN
        WRITE(6,120)(R(K,I),K=1,3),(R(K,J),K=1,3),(R(K,NSYM),K=1,3)
        WRITE(6,130)(R(K,I),K=4,6),(R(K,J),K=4,6),(R(K,NSYM),K=4,6)
        WRITE(6,140)(R(K,I),K=7,9),(R(K,J),K=7,9),(R(K,NSYM),K=7,9)
 120    FORMAT(' |',3F7.3,' |   |',3F7.3,' |   |',3F7.3,' |')
 130    FORMAT(' |',3F7.3,' | * |',3F7.3,' | = |',3F7.3,' |')
 140    FORMAT(' |',3F7.3,' |   |',3F7.3,' |   |',3F7.3,' |',/)
      ENDIF
C
      GOTO 10
C
C
  50  CONTINUE
C
C  NOW, TO DO FINAL WRAPUP
C
      WRITE(6,150)NSYM
 150  FORMAT(/,' THERE ARE ',I3,' UNIQUE SYMMETRY FUNCTIONS.',/)
C
C  PRINT THE IPO MATRIX IF ASKED FOR.
C
      IF(INDEX(KEYWRD,' IPO') .NE. 0) THEN
        WRITE(6,160)
 160  FORMAT(/,20X,'THE PERMUTATION MATRIX')
        I = 1
        J = MIN(12,NSYM)
  60    WRITE(6,170)(K,K=I,J)
 170  FORMAT(/,/,5X,'OPER. NO. ',12I5)
        WRITE(6,175)(OPER(R(1,K)),K=I,J)
 175  FORMAT(5X,'SYMM. OPER. ',12A5)
        WRITE(6,180)
 180  FORMAT(5X,'ATOM NO.')
        DO 70 K = 1, NUMAT
  70    WRITE(6,190)K,(IPO(K,L),L=I,J)
 190  FORMAT(I10,5X,12I5)
        IF (J .LT. NSYM) THEN
          I = J+1
          J = MIN(J+12,NSYM)
          GOTO 60
        ENDIF
      ENDIF
      RETURN
      END
      CHARACTER*5 FUNCTION OPER(R)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER OPR*5, NUM*10
      DIMENSION R(9)
C
C
      OPR = ' '
      NUM = '0123456789'
      TRACE = R(1) + R(5) + R(9)
      DET = R(1)*R(5)*R(9) + R(2)*R(6)*R(7) + R(3)*R(4)*R(8)
     +    - R(1)*R(6)*R(8) - R(2)*R(4)*R(9) - R(3)*R(5)*R(7)
      TRACE = (TRACE - DET)/2.D0
      IF (DET .GT. 0.D0) THEN
        OPR(1:1) = 'C'
        IF (TRACE .GT. 0.97D0) THEN
          OPR(1:1) = 'E'
          GOTO 20
        ENDIF
      ELSE
        OPR(1:1) = 'S'
        IF (TRACE .GT. 0.97D0) THEN
          OPR(1:5) = 'Sigma'
          GOTO 20
        ENDIF
        IF (TRACE .LT. -0.97D0) THEN
          OPR(1:5) = ' Inv '
          GOTO 20
        ENDIF
      ENDIF
      IF (TRACE .LT. -0.97D0) THEN
        OPR(2:2) = NUM(3:3) 
        GOTO 20
      ENDIF
      ANG = ACOS(TRACE)
      AFULL = ACOS(-1.0D0)*2.D0
      DO 10 I = 3, 18
        ANS = I*ANG/AFULL
        IF (ABS(ANS - NINT(ANS)) .LE. 2.5D-3) THEN
          IF(I .GE.10) THEN
            OPR(2:2) = NUM(2:2)
            OPR(3:3) = NUM(I-9:I-9)
          ELSE
            OPR(2:2) = NUM(I+1:I+1)
          ENDIF
          IF (NINT(ANS) .NE. 1) THEN
            OPR(4:5) = '* '
            OPR(5:5) = NUM(NINT(ANS)+1:NINT(ANS)+1)
          ENDIF
          GOTO 20
        ENDIF
  10  CONTINUE
      OPR(2:5) = 'Unkn'
C
  20  OPER = OPR
      RETURN
      END
