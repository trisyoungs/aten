      SUBROUTINE DIJKL1 (C,N,NATI,W,CIJ,WCIJ,CKL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION C(N,*), W(*)
      DIMENSION CIJ(10*MAXORB), WCIJ(10*MAXORB), CKL(10*MAXORB)
************************************************************************
*
*   DIJKL1 IS SIMILAR TO IJKL.  THE MAIN DIFFERENCES ARE THAT
*   THE ARRAY W CONTAINS THE TWO ELECTRON INTEGRALS BETWEEN
*   ONE ATOM (NATI) AND ALL THE OTHER ATOMS IN THE SYSTEM.
*
*           ON EXIT
*
*   THE ARRAY XY IS FILLED WITH THE DIFFERENTIALS OF THE
*   TWO-ELECTRON INTEGRALS OVER ACTIVE-SPACE M.O.S W.R.T. MOTION
*   OF THE ATOM NATI.
************************************************************************
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /CIBITS/ NMOS,LAB,NELEC, NBO(3)
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      DIMENSION NB(0:8)
      DATA NB /1,0,0,10,0,0,0,0,45/
      NA=NMOS
      DO 110 I=1,NA
         DO 110 J=1,I
            IPQ=0
            DO 20 II=1,NUMAT
               IF(II.EQ.NATI) GOTO 20
               DO 10 IP=NFIRST(II),NLAST(II)
                  DO 10 IQ=NFIRST(II),IP
                     IPQ=IPQ+1
                     CIJ(IPQ)=C(IP,I)*C(IQ,J)+C(IP,J)*C(IQ,I)
   10          CONTINUE
   20       CONTINUE
            I77=IPQ+1
            DO 30 IP=NFIRST(NATI),NLAST(NATI)
               DO 30 IQ=NFIRST(NATI),IP
                  IPQ=IPQ+1
                  CIJ(IPQ)=C(IP,I)*C(IQ,J)+C(IP,J)*C(IQ,I)
   30       CONTINUE
            DO 40 II=1,IPQ
   40       WCIJ(II)=0.D0
            KR=1
            JS=1
            NBJ=NB(NLAST(NATI)-NFIRST(NATI))
            DO 50 II=1,NUMAT
               IF (II.EQ.NATI) GOTO 50
               NBI=NB(NLAST(II)-NFIRST(II))
               CALL FORMXY
     1(W(KR), KR, WCIJ(I77), WCIJ(JS), CIJ(I77), NBJ, CIJ(JS), NBI)
               JS=JS+NBI
   50       CONTINUE
            DO 100 K=1,I
               IF(K.EQ.I) THEN
                  LL=J
               ELSE
                  LL=K
               ENDIF
               DO 100 L=1,LL
                  IPQ=0
                  DO 70 II=1,NUMAT
                     IF(II.EQ.NATI) GOTO 70
                     DO 60 IP=NFIRST(II),NLAST(II)
                        DO 60 IQ=NFIRST(II),IP
                           IPQ=IPQ+1
                           CKL(IPQ)=C(IP,K)*C(IQ,L)+C(IP,L)*C(IQ,K)
   60                CONTINUE
   70             CONTINUE
                  DO 80 IP=NFIRST(NATI),NLAST(NATI)
                     DO 80 IQ=NFIRST(NATI),IP
                        IPQ=IPQ+1
                        CKL(IPQ)=C(IP,K)*C(IQ,L)+C(IP,L)*C(IQ,K)
   80             CONTINUE
                  SUM=0.D0
                  DO 90 II=1,IPQ
   90             SUM=SUM+CKL(II)*WCIJ(II)
                  XY(I,J,K,L)=SUM
                  XY(I,J,L,K)=SUM
                  XY(J,I,K,L)=SUM
                  XY(J,I,L,K)=SUM
                  XY(K,L,I,J)=SUM
                  XY(K,L,J,I)=SUM
                  XY(L,K,I,J)=SUM
                  XY(L,K,J,I)=SUM
  100       CONTINUE
  110 CONTINUE
      RETURN
      END
