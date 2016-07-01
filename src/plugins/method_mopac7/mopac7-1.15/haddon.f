      SUBROUTINE HADDON (W,L,M,LOC,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(3,*)
C**********************************************************************
C
C   HADDON CALCULATES THE VALUE OF A SYMMETRY-DEPENDENT VARIABLE
C
C  ON INPUT: M   = NUMBER SPECIFYING THE SYMMETRY OPERATION
C            LOC = ADDRESS OF REFERENCE ATOM
C            A   = ARRAY OF INTERNAL COORDINATES
C  ON OUTPUT W   = VALUE OF DEPENDENT FUNCTION
C            L   = 1 (FOR BOND LENGTH), 2 (ANGLE), OR 3 (DIHEDRAL)
C**********************************************************************
      PI = 3.1415926536D00
      IF (M.GT.18 .OR. M.LT.1) THEN
         WRITE(6,'(///10X,''UNDEFINED SYMMETRY FUNCTION USED'')')
         STOP
      ENDIF
      I=LOC
      GO TO
     1(140,160,10,20,30,40,50,60,70,80,90,100,110,120,150,170,180,190),
     2M
   10 W=A(3,I)
      GO TO 130
   20 W=(PI/2.0D00)-A(3,I)
      GO TO 130
   30 W=(PI/2.0D00)+A(3,I)
      GO TO 130
   40 W=(2.0D00*PI/3.0D00)-A(3,I)
      GO TO 130
   50 W=(2.0D00*PI/3.0D00)+A(3,I)
      GO TO 130
   60 W=(PI)-A(3,I)
      GO TO 130
   70 W=(PI)+A(3,I)
      GO TO 130
   80 W=(4.0D00*PI/3.0D00)-A(3,I)
      GO TO 130
   90 W=(4.0D00*PI/3.0D00)+A(3,I)
      GO TO 130
  100 W=(3.0D00*PI/2.0D00)-A(3,I)
      GO TO 130
  110 W=(3.0D00*PI/2.0D00)+A(3,I)
      GO TO 130
  120 W=-A(3,I)
  130 L=3
      RETURN
  140 L=1
      W=A(1,I)
      RETURN
  150 L=1
      W=A(1,I)/2.0D00
      RETURN
  160 L=2
      W=A(2,I)
      RETURN
  170 L=2
      W=A(2,I)/2.0D00
      RETURN
  180 L=2
      W=PI-A(2,I)
      RETURN
  190 CALL DEPVAR (A,I,W,L)
      RETURN
C
      END
