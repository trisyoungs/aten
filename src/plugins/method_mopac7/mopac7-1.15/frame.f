      SUBROUTINE FRAME(FMAT,NUMAT,MODE,SHIFT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FMAT(*), SHIFT(6)
      INCLUDE 'SIZES'
      COMMON /COORD /COORD(3,NUMATM)
      COMMON /ATMASS/ ATMASS(NUMATM)
      DIMENSION VIB(6,MAXPAR), ROT(3,3), COORD1(3,NUMATM)
***********************************************************************
*
*   FRAME APPLIES AN RIGID ORIENTATION TO THE MOLECULE IN A FORCE
*         CALCULATION. THE TRANSLATIONS ARE GIVEN A 'FORCE CONSTANT'
*         OF T(X)=500 MILLIDYNES/ANGSTROM
*            T(Y)=600 MILLIDYNES/ANGSTROM
*            T(Z)=700 MILLIDYNES/ANGSTROM
*         AND THE ROTATIONS ARE GIVEN A 'FORCE CONSTANT' OF
*            R(X)=800 MILLIDYNES/ANGSTROM
*            R(Y)=900 MILLIDYNES/ANGSTROM
*            R(Z)=1000 MILLIDYNES/ANGSTROM,
*    THE ROTATIONS ARE MADE ABOUT AXES DETERMINED BY THE MOMENTS
*    OF INERTIA, WHICH IN TURN DEPEND ON THE ISOTOPIC MASSES. FOR
*    THE NORMAL FREQUENCY CALCULATION THESE ARE THE REAL MASSES,
*    FOR THE FORCE CALCULATION THEY ARE ALL UNITY.
***********************************************************************
      COMMON /EULER / TVEC(3,3), ID
      CALL AXIS(COORD,NUMAT,A,B,C,SUMW, MODE,ROT )
      DO 20 I=1,NUMAT
         DO 20 J=1,3
            SUM=0.D0
            DO 10 K=1,3
   10       SUM=SUM+COORD(K,I)*ROT(K,J)
   20 COORD1(J,I)=SUM
      N3=NUMAT*3
      J=0
      WTMASS=1.D0
      DO 30 I=1,NUMAT
         IF(MODE.EQ.1)  WTMASS=SQRT(ATMASS(I))
         J=J+1
         VIB(1,J)=WTMASS
         VIB(2,J)=0.D0
         VIB(3,J)=0.D0
         VIB(4,J)=0.D0
         VIB(5,J)=COORD1(3,I)*WTMASS
         VIB(6,J)=COORD1(2,I)*WTMASS
         J=J+1
         VIB(1,J)=0.D0
         VIB(2,J)=WTMASS
         VIB(3,J)=0.D0
         VIB(4,J)=COORD1(3,I)*WTMASS
         VIB(5,J)=0.D0
         VIB(6,J)=-COORD1(1,I)*WTMASS
         J=J+1
         VIB(1,J)=0.D0
         VIB(2,J)=0.D0
         VIB(3,J)=WTMASS
         VIB(4,J)=-COORD1(2,I)*WTMASS
         VIB(5,J)=-COORD1(1,I)*WTMASS
         VIB(6,J)=0.D0
   30 CONTINUE
      J=1
      DO 50 I=1,NUMAT
         DO 40 K=4,6
            X=VIB(K,J)
            Y=VIB(K,J+1)
            Z=VIB(K,J+2)
            VIB(K,J  )=X*ROT(1,1)+Y*ROT(1,2)+Z*ROT(1,3)
            VIB(K,J+1)=X*ROT(2,1)+Y*ROT(2,2)+Z*ROT(2,3)
            VIB(K,J+2)=X*ROT(3,1)+Y*ROT(3,2)+Z*ROT(3,3)
   40    CONTINUE
         J=J+3
   50 CONTINUE
      SUM1=0.D0
      SUM2=0.D0
      SUM3=0.D0
      SUM4=0.D0
      SUM5=0.D0
      SUM6=0.D0
      DO 60 I=1,N3
         SUM1=SUM1+VIB(1,I)**2
         SUM2=SUM2+VIB(2,I)**2
         SUM3=SUM3+VIB(3,I)**2
         SUM4=SUM4+VIB(4,I)**2
         SUM5=SUM5+VIB(5,I)**2
   60 SUM6=SUM6+VIB(6,I)**2
      IF(SUM1.GT.1.D-5)SUM1=SQRT(1.D0/SUM1)
      IF(SUM2.GT.1.D-5)SUM2=SQRT(1.D0/SUM2)
      IF(SUM3.GT.1.D-5)SUM3=SQRT(1.D0/SUM3)
      IF(SUM4.GT.1.D-5)SUM4=SQRT(1.D0/SUM4)
      IF(SUM5.GT.1.D-5)SUM5=SQRT(1.D0/SUM5)
      IF(SUM6.GT.1.D-5)SUM6=SQRT(1.D0/SUM6)
      IF(ID.NE.0)THEN
         SUM4=0.D0
         SUM5=0.D0
         SUM6=0.D0
      ENDIF
      DO 70 I=1,N3
         VIB(1,I)=VIB(1,I)*SUM1
         VIB(2,I)=VIB(2,I)*SUM2
         VIB(3,I)=VIB(3,I)*SUM3
         VIB(4,I)=VIB(4,I)*SUM4
         VIB(5,I)=VIB(5,I)*SUM5
   70 VIB(6,I)=VIB(6,I)*SUM6
      DO 80 I=1,6
   80 SHIFT(I)=400.D0+I*100.D0
      L=0
      DO 100 I=1,N3
         DO 100 J=1,I
            L=L+1
            SUM1=0.D0
            DO 90 K=1,6
   90       SUM1=SUM1+VIB(K,I)*SHIFT(K)*VIB(K,J)
  100 FMAT(L)=FMAT(L)+SUM1
      END
