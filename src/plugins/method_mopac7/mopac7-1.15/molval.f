      SUBROUTINE MOLVAL(C,P,NOCC,RHFUHF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION C(NORBS,NORBS), P(*)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      DIMENSION VAL(MAXORB)
      DO 40 I=1,NOCC
         SUM=0.D0
         DO 30 JJ=1,NUMAT
            JL=NFIRST(JJ)
            JU=NLAST(JJ)
            DO 30 J=JL,JU
               DO 30 KK=1,NUMAT
                  IF(KK.EQ.JJ) GOTO 20
                  KL=NFIRST(KK)
                  KU=NLAST(KK)
                  DO 10 K=KL,KU
                     L1=MAX(J,K)
                     L2=J+K-L1
                     L=(L1*(L1-1))/2+L2
                     SUM=SUM+C(J,I)*C(K,I)*P(L)
   10             CONTINUE
   20             CONTINUE
   30    CONTINUE
         VAL(I)=SUM*RHFUHF
   40 CONTINUE
      WRITE(6,'(10F8.4)')(VAL(I),I=1,NOCC)
      END
