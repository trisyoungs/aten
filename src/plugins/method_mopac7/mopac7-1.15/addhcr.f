      SUBROUTINE ADDHCR (H)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                BH(LENABC)
     4       /SOLVI/  IATSP(LENABC+1),NAR(LENABC), NN(2,NUMATM)
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /CORE  / CORE(107)
      DIMENSION H(*)
      IDEN=0
      I0=NPS2+NDEN*NPS
      DO 40 I=1,NUMAT
         IA=NFIRST(I)
         IDEL=NLAST(I)-IA
         IM=(IA*(IA+1))/2-1
         DO 30 IC=0,IDEL
            DO 20 ID=0,IC
               IM=IM+1
               IDEN=IDEN+1
               HIM=0.D0
               JDEN=1
               DO 10 J=1,NUMAT
                  JA=NFIRST(J)
                  JDEL=NLAST(J)-JA
C#              JDEN=JDEN+1
                  KDEN=MAX(IDEN,JDEN)
                  I1=(KDEN*(KDEN-3))/2+IDEN+JDEN+I0
                  HIM = HIM - ABCMAT(I1) * CORE(NAT(J))
                  JDEN=JDEN+JDEL**2+1
   10          CONTINUE
               H(IM) = H(IM) + HIM
   20       CONTINUE
            H(IM) = H(IM) + HIM
            IM=IM+IA-1
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
