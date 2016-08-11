      FUNCTION CAPCOR(NAT,NFIRST,NLAST,NUMAT,P,H)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), H(*), NFIRST(NUMAT), NLAST(NUMAT), NAT(NUMAT)
******************************************************************
*
*    CORRECTION TO ELECTRONIC ENERGY DUE TO CAPPED BONDS
*
******************************************************************
      SUM=0.D0
      DO 40 I=1,NUMAT
         NI=NAT(I)
         IL=NFIRST(I)
         IU=NLAST(I)
         IF(NI.EQ.102) THEN
C
C   DO ENTIRE ROW - NO NEED TO CHECK FURTHER.
C
            J=(NLAST(I)*(NLAST(I)+1))/2
            II=IU-1
            DO 10 K=1,II
               J=J-1
   10       SUM=SUM+P(J)*H(J)
         ELSE
            DO 30 J=1,I
               JL=NFIRST(J)
               IF(NAT(J).EQ.102)THEN
                  DO  20 K=IL,IU
                     KK=(K*(K-1))/2+JL
   20             SUM=SUM+P(KK)*H(KK)
               ENDIF
   30       CONTINUE
         ENDIF
   40 CONTINUE
C
C   DOUBLE SUM SINCE WE ONLY CALCULATED LOWER HALF, AND CAPCOR
C   WILL APPEAR IN 1/2*P(H+F).  ONLY H PART OF F WILL BE USED.
      CAPCOR=-SUM*2.D0
      RETURN
      END
