      SUBROUTINE DIJKL2 (DC,NORBS,NMOS,DIJKL,WIJKL,NMECI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*     RELAXATION OF 2-ELECTRONS INTEGRALS IN M.O BASIS.
*
*   INPUT
*   DC(NORBS,NMOS) : C.I-ACTIVE M.O DERIVATIVES IN M.O BASIS, IN COLUMN.
*   NORBS          : TOTAL NUMBER OF M.O.
*   NMOS           : NUMBER OF C.I-ACTIVE M.O.
*   DIJKL(I,J,KL)  : <I(1),J(1)|K(2),L(2)> WITH
*                     I              OVER     ALL    M.O.
*                     J,KL CANONICAL OVER C.I-ACTIVE M.O.
*   NMECI          : MAX. SIZE OF WIJKL. (NMOS <= NMECI).
*   OUTPUT
*     WIJKL(I,J,K,L)= d< I(1),J(1) | K(2),L(2) >
*                   = <dI,J|K,L> + <I,dJ|K,L> + <I,J|dK,L> + <I,J|K,dL>
*                     WITH I,J,K,L OVER ALL C.I-ACTIVE M.O.
*     WRITTEN BY DANIEL LIOTARD
* (NOTE BY JJPS: AS THIS CODE IS HIGHLY EFFICIENT, NO CHANGES WERE MADE)
************************************************************************
      DIMENSION DC(NORBS,*),WIJKL(NMECI,NMECI,NMECI,NMECI)
      DIMENSION DIJKL(NORBS,NMOS,*)
      LOGICAL LIJ,LKL
C
      IJ=0
      DO 10 I=1,NMOS
         DO 10 J=1,I
            IJ=IJ+1
            LIJ=I.EQ.J
            KL=0
            DO 10 K=1,I
               IF(K.EQ.I) THEN
                  LL=J
               ELSE
                  LL=K
               ENDIF
               DO 10 L=1,LL
                  KL=KL+1
                  LKL=K.EQ.L
                  VAL=               DOT(DC(1,I),DIJKL(1,J,KL),NORBS)
                  IF(LIJ.AND.LKL.AND.J.EQ.K) THEN
                     VAL=VAL*4.D0
                  ELSE
                     IF(LIJ) THEN
                        VAL=VAL*2.D0
                     ELSE
                        VAL=VAL+     DOT(DC(1,J),DIJKL(1,I,KL),NORBS)
                     ENDIF
                     VAL2=           DOT(DC(1,K),DIJKL(1,L,IJ),NORBS)
                     IF(LKL) THEN
                        VAL=VAL+VAL2*2.D0
                     ELSE
                        VAL=VAL+VAL2+DOT(DC(1,L),DIJKL(1,K,IJ),NORBS)
                     ENDIF
                  ENDIF
                  WIJKL(I,J,K,L)=VAL
                  WIJKL(I,J,L,K)=VAL
                  WIJKL(J,I,K,L)=VAL
                  WIJKL(J,I,L,K)=VAL
                  WIJKL(K,L,I,J)=VAL
                  WIJKL(K,L,J,I)=VAL
                  WIJKL(L,K,I,J)=VAL
   10 WIJKL(L,K,J,I)=VAL
      RETURN
      END
