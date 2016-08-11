      SUBROUTINE DVFILL(NPPA,DIRVEC)
C	FUELLEN DES FELDES DIRVEC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DIRVEC(3,*)
      INTEGER FSET(3,20), KSET(2,30)
      DATA KSET/ 1, 2, 1, 3, 1, 4, 1, 5, 1, 6,
     1            12,11,12,10,12, 9,12, 8,12, 7,
     2             2, 3, 3, 4, 4, 5, 5, 6, 6, 2,
     3             7, 8, 8, 9, 9,10,10,11,11, 7,
     4             2,7,7,3,3,8,8,4,4,9,9,5,5,10,10,6,6,11,11,2/
      DATA FSET/ 1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2,
     1            12,11,10,12,10, 9,12, 9, 8,12, 8, 7,12, 7,11,
     2             2, 3, 7, 3, 4, 8, 4, 5, 9, 5, 6,10, 6, 2,11,
     3             7, 8, 3, 8, 9, 4, 9,10, 5,10,11, 6,11, 7, 2/
      DIRVEC (1,1) =  -1.D0
      DIRVEC (2,1) =   0.D0
      DIRVEC (3,1) =   0.D0
      ND=1
      R=SQRT(.8D0)
      H=SQRT(.2D0)
      DO 10 I= -1,1,2
         DO 10 J= 1,5
            ND=ND+1
            BETA=1.D0+ J*1.25663706D0 + (I+1)*0.3141593D0
            DIRVEC(2,ND)=R*COS(BETA)
            DIRVEC(3,ND)=R*SIN(BETA)
            DIRVEC(1,ND)=I*H
   10 CONTINUE
      DIRVEC (2,12) =  0.D0
      DIRVEC (3,12) =  0.D0
      DIRVEC (1,12) =  1.D0
      ND=12
C  NPPA=10*3**K*4**L+2
      M=(NPPA-2)/10
      DO 20 K=0,10
         IF ((M/3)*3 .NE. M) GO TO 30
   20 M=M/3
   30 DO 40 L=0,10
         IF ((M/4)*4 .NE. M) GO TO 50
   40 M=M/4
   50 IF (10*3**K*4**L+2 .NE. NPPA) STOP 'VALUE OF NPPA NOT ALLOWED:
     1  IT MUST BE 10*3**K*4**L+2'
      KH=K/2
      M=2**L*3**KH
C CREATE ON EACH EDGE 2**L*3**KH-1 NEW POINTS
      DO 70 I=1,30
         NA=KSET(1,I)
         NB=KSET(2,I)
         DO 70 J=1,M-1
            ND=ND+1
            DO 60 IX=1,3
   60       DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J)+DIRVEC(IX,NB)*J
   70 CONTINUE
C CREATE POINTS WITHIN EACH TRIANGLE
      DO 90 I=1,20
         NA=FSET(1,I)
         NB=FSET(2,I)
         NC=FSET(3,I)
         DO 90 J1=1,M-1
            DO 90 J2=1,M-J1-1
               ND=ND+1
               DO 80 IX=1,3
   80          DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J1-J2)
     1                     +DIRVEC(IX,NB)*J1+DIRVEC(IX,NC)*J2
   90 CONTINUE
      IF (K .EQ. 2*KH) GO TO 140
C CREATE TO ADDITIONAL SUBGRIDS
      T=1.D0/3.D0
      DO 110 I=1,20
         NA=FSET(1,I)
         NB=FSET(2,I)
         NC=FSET(3,I)
         DO 110 J1=0,M-1
            DO 110 J2=0,M-J1-1
               ND=ND+1
               DO 100 IX=1,3
  100          DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J1-J2-2*T)
     1                 +DIRVEC(IX,NB)*(J1+T)+DIRVEC(IX,NC)*(J2+T)
  110 CONTINUE
      T=2.D0/3.D0
      DO 130 I=1,20
         NA=FSET(1,I)
         NB=FSET(2,I)
         NC=FSET(3,I)
         DO 130 J1=0,M-2
            DO 130 J2=0,M-J1-2
               ND=ND+1
               DO 120 IX=1,3
  120          DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J1-J2-2*T)
     1                  +DIRVEC(IX,NB)*(J1+T)+DIRVEC(IX,NC)*(J2+T)
  130 CONTINUE
C NORMALIZE ALL VECTORS
  140 DO 170 I=1,NPPA
         DIST=0.D0
         DO 150 IX=1,3
  150    DIST=DIST+DIRVEC(IX,I)**2
         DIST=1.D0/SQRT(DIST)
         DO 160 IX=1,3
  160    DIRVEC(IX,I)=DIRVEC(IX,I)*DIST
  170 CONTINUE
      RETURN
      END
