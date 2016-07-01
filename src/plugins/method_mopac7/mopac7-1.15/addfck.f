      SUBROUTINE ADDFCK (F,P,NUMAT,NAT,NFIRST,NLAST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION NAT(*),NFIRST(*), NLAST(*)
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                BH(LENABC)
     4       /SOLVI/  IATSP(LENABC+1),NAR(LENABC), NNX(2,NUMATM)
      COMMON /DIRVEC/ DIRVEC(3,NPPA), NN(3,NUMATM)
C      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
C     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
C     2                NCLOSE,NOPEN,NDUMY,FRACT
      DIMENSION F(*),P(*)
      I0=NPS2+NDEN*NPS
      IDEN=0
      DO 60 I=1,NUMAT
         IA=NFIRST(I)
         IDEL=NLAST(I)-IA
         IM=(IA*(IA+1))/2-1
         DO 50 IC=0,IDEL
            DO 40 ID=0,IC
               IM=IM+1
               IDEN=IDEN+1
               FIM=0.D0
               JDEN=0
               DO 30 J=1,NUMAT
                  JA=NFIRST(J)
                  JDEL=NLAST(J)-JA
                  JM=(JA*(JA+1))/2-1
                  DO 20 JC=0,JDEL
                     DO 10 JD=0,JC
                        JM=JM+1
                        JDEN=JDEN+1
                        KDEN=MAX(IDEN,JDEN)
                        I1=(KDEN*(KDEN-3))/2+IDEN+JDEN+I0
                        FIM = FIM + ABCMAT(I1) * P(JM)
   10                CONTINUE
                     JM=JM+JA-1
   20             CONTINUE
   30          CONTINUE
               F(IM)=F(IM)+2*FIM
   40       CONTINUE
            F(IM)=F(IM)+2*FIM
            IM=IM+IA-1
   50    CONTINUE
   60 CONTINUE
      RETURN
      END
