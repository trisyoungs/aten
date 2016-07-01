      SUBROUTINE PATHK
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
************************************************************************
*
*  		Written by Manyin Yi, Aug 1989.
*       Restartable reaction_path calulation.
*	The number of path_step and step value are read in through
*       keyword POINT and STEP.
*	The reaction profile is archived.
*
************************************************************************
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /GRAVEC/ COSINE
      COMMON /PATH  / LATOM, LPARAM, REACT(200)
      COMMON /PPARAM/ CURRT
      COMMON /KLOOP / KLOOP
C ***** Modified by Jiro Toyoda at 1994-05-25 *****
C     COMMON /PROFIL/ PROFIL
      COMMON /PROFIC/ PROFIL
C ***************************** at 1994-05-25 *****
      COMMON /KEYWRD/ KEYWRD
      DIMENSION GD(MAXPAR),XLAST(MAXPAR),MDFP(20),XDFP(20)
      DIMENSION PROFIL(200)
      CHARACTER KEYWRD*241, GETNAM*80
      STEP=READA(KEYWRD,INDEX(KEYWRD,'STEP')+5)
      NPTS=READA(KEYWRD,INDEX(KEYWRD,'POINT')+6)
C
C  THE SMALLEST VALUE IN THE PATH IS
C      REACT(1) DEGREE OR GEO(LPARAM,LATOM) RADIANS
C
      DEGREE=180.D0/3.14159265359D0
      IF(LPARAM.NE.1)STEP=STEP/DEGREE
C
C  NOW TO SWEEP THROUGH THE PATH
C
      IF(LPARAM.NE.1) THEN
         C1=DEGREE
      ELSE
         C1=1.D0
      ENDIF
C
      KLOOP=1
      CPUTOT=0.0D0
      CURRT=GEO(LPARAM,LATOM)
      PROFIL(1)=0.D0
      IF (INDEX(KEYWRD,'RESTART').NE.0) THEN
         MDFP(9)=0
         CALL DFPSAV(CPUTOT,XPARAM,GD,XLAST,ESCF,MDFP,XDFP)
         WRITE(6,'(//10X,'' RESTARTING AT POINT '',I3)') KLOOP
      ENDIF
C
      GEO(LPARAM,LATOM)=CURRT
      DO 10 ILOOP=KLOOP,NPTS
         CPU1=SECOND()
         CURRT=GEO(LPARAM,LATOM)
         CALL FLEPO(XPARAM, NVAR, ESCF)
         KLOOP=KLOOP+1
         CPU2=SECOND()
         CPU3=CPU2-CPU1
         CPUTOT=CPUTOT+CPU3
         PROFIL(ILOOP)=ESCF
         WRITE(6,'(/''          VARIABLE        FUNCTION'')')
         WRITE(6,'('' :'',F16.5,F16.6)')GEO(LPARAM,LATOM)*C1,ESCF
         CALL GEOUT(6)
         GEO(LPARAM,LATOM)=GEO(LPARAM,LATOM)+STEP
   10 CONTINUE
      DO 20 I=2,NPTS
   20 REACT(I)=REACT(I-1)+STEP*C1
      WRITE(6,'(/16X,''POINTS ON REACTION PATH '',
     1          /16X,''AND CORRESPONDING HEATS'',//)')
!      OPEN(UNIT=12,FILE=GETNAM('FOR012'),STATUS='UNKNOWN')
      WRITE(12,30)
      CALL WRTTXT(12)
   30 FORMAT(' ARCHIVE FILE FOR PATH CALCULATION'/
     1 'A PROFIL OF COORDINATES - HEATS'/)
      WRITE(12,'(/'' TOTAL CPU TIME IN FLEPO : '',F10.3/)') CPUTOT
C
      L=NPTS/8
      M=NPTS - L*8
      IF (L.LT.1) GO TO 50
      DO 40 K=0,L-1
         WRITE(6,'(8F7.2)') (REACT(I),I=K*8+1,K*8+8)
         WRITE(6,'(8F7.2,/)') (PROFIL(I),I=K*8+1,K*8+8)
         WRITE(12,'(8F7.2)') (REACT(I),I=K*8+1,K*8+8)
   40 WRITE(12,'(8F7.2,/)') (PROFIL(I),I=K*8+1,K*8+8)
   50 IF (M.GT.0) THEN
         WRITE(6,'(8F7.2)') (REACT(I),I=L*8+1,L*8+M)
         WRITE(6,'(8F7.2,/)') (PROFIL(I),I=L*8+1,L*8+M)
         WRITE(12,'(8F7.2)') (REACT(I),I=L*8+1,L*8+M)
         WRITE(12,'(8F7.2,/)') (PROFIL(I),I=L*8+1,L*8+M)
      ENDIF
      RETURN
      END
