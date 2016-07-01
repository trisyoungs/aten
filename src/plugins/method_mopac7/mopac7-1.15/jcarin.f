      SUBROUTINE JCARIN (COORD,XPARAM,STEP,PRECI,B,NCOL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
C     JACOBIAN dCARTESIAN/dINTERNAL, WORKED OUT BY FINITE DIFFERENCE.
C  INPUT
C     XPARAM(*) : INTERNAL COORDINATES
C     STEP      : STEP SIZE FOR FINITE DIFFERENCE METHOD
C     PRECI     : .TRUE. IF 2-POINTS FINITE DIFFERENCES TO BE USED,
C                 .FALSE. OTHERWISE.
C  OUTPUT
C     B(NVAR,NCOL) : JACOBIAN, STEP TIME TOO LARGE.
C
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                      LOCDEP(MAXPAR)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
     3       /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, YPARAM(MAXPAR)
     4       /EULER / TVEC(3,3),ID
     5       /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
     6       /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      DIMENSION COORD(3,*),XPARAM(*),B(NVAR,*), COOLD(3,NUMATM*27)
      LOGICAL PRECI
C
      NCOL=3*NUMAT
      IF(ID.NE.0)
     1 NCOL=NCOL*(L1U-L1L+1)*(L2U-L2L+1)*(L3U-L3L+1)
C
C     INTERNAL OF CENTRAL POINT
      DO 10 IVAR=1,NVAR
   10 GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
C
      IF (ID.EQ.0) THEN
C
C        MOLECULAR SYSTEM
C        ----------------
         DO 30 IVAR=1,NVAR
C        STEP FORWARD
            GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)+STEP
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            DO 20 J=1,NCOL
   20       B(IVAR,J)=COORD(J,1)
   30    GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
         IF (PRECI) THEN
            DO 50 IVAR=1,NVAR
C           STEP BACKWARD
               GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)-STEP
               IF(NDEP.NE.0) CALL SYMTRY
               CALL GMETRY (GEO,COORD)
               DO 40 J=1,NCOL
   40          B(IVAR,J)=B(IVAR,J)-COORD(J,1)
   50       GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
         ELSE
C           CENTRAL POINT
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            DO 60 IVAR=1,NVAR
               DO 60 J=1,NCOL
   60       B(IVAR,J)=B(IVAR,J)-COORD(J,1)
         ENDIF
      ELSE
C
C        SOLID STATE
C        -----------
         DO 80 IVAR=1,NVAR
C        STEP FORWARD
            GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)+STEP
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            IJ=0
            DO 70 II=1,NUMAT
               DO 70 IL=L1L,L1U
                  DO 70 JL=L2L,L2U
                     DO 70 KL=L3L,L3U
                        DO 70 LL=1,3
                           IJ=IJ+1
   70       B(IVAR,IJ)=COORD(LL,II)
     1            +TVEC(LL,1)*IL+TVEC(LL,2)*JL+TVEC(LL,3)*KL
   80    GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
         IF (PRECI) THEN
            DO 100 IVAR=1,NVAR
C           STEP BACKWARD
               GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)-STEP
               IF(NDEP.NE.0) CALL SYMTRY
               CALL GMETRY (GEO,COORD)
               IJ=0
               DO 90 II=1,NUMAT
                  DO 90 IL=L1L,L1U
                     DO 90 JL=L2L,L2U
                        DO 90 KL=L3L,L3U
                           DO 90 LL=1,3
                              IJ=IJ+1
   90          B(IVAR,IJ)=B(IVAR,IJ)-COORD(LL,II)
     1                -TVEC(LL,1)*IL-TVEC(LL,2)*JL-TVEC(LL,3)*KL
  100       GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
         ELSE
C           CENTRAL POINT
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            IJ=0
            DO 110 II=1,NUMAT
               DO 110 IL=L1L,L1U
                  DO 110 JL=L2L,L2U
                     DO 110 KL=L3L,L3U
                        IJ=IJ+1
                        DO 110 LL=1,3
  110       COOLD(LL,IJ)=COORD(LL,II)
     1                  +TVEC(LL,1)*IL+TVEC(LL,2)*JL+TVEC(LL,3)*KL
            DO 120 IVAR=1,NVAR
               DO 120 IJ=1,NCOL
  120       B(IVAR,IJ)=B(IVAR,IJ)-COOLD(IJ,1)
         ENDIF
      ENDIF
      RETURN
      END
