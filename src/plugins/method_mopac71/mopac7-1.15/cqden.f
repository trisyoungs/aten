      SUBROUTINE CQDEN()
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                BH(LENABC)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /CORE  / CORE(107)
      IDEN=0
      DO 30 I=1,NUMAT
         IA=NFIRST(I)
         IDEL=NLAST(I)-IA
         IM=(IA*(IA+1))/2
         IDEN=IDEN+1
         QDEN(IDEN)=CORE(NAT(I))-P(IM)
         DO 20 IC=1,IDEL
            IM=IM+IA-1
            DO 10 ID=0,IC
               IM=IM+1
               IDEN=IDEN+1
               QDEN(IDEN)=-P(IM)
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
