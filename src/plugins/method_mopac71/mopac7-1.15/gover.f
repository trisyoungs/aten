      SUBROUTINE GOVER(NI,NJ,XI,XJ,R,SG)
************************************************************************
*                                                                      *
*   GOVER CALCULATES THE OVERLAP INTEGRALS USING A GAUSSIAN EXPANSION  *
*         STO-6G BY R.F. STEWART, J. CHEM. PHYS., 52 431-438, 1970     *
*                                                                      *
*         ON INPUT   NI   =  ATOMIC NUMBER OF FIRST ATOM               *
*                    NJ   =  ATOMIC NUMBER OF SECOND ATOM              *
*                    R    =  INTERATOMIC DISTANCE IN ANGSTROMS         *
*         ON EXIT    S    =  9X9 ARRAY OF OVERLAPS, IN ORDER S,PX,PY,  *
*                            PZ                                        *
*                                                                      *
************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /NATYPE/ NZTYPE(107), MTYPE(30),LTYPE
      COMMON /TEMP/  C(60,6), Z(60,6)
      COMMON /NATORB/ NATORB(107)
      DIMENSION S(6,6), XI(3),XJ(3), SG(9,9)
      SAVE NGAUSS
      DATA NGAUSS/6/
C
C    FIND START AND END OF GAUSSIAN
C
      IFA=NZTYPE(NI)*4-3
      IF(C(IFA+1,1).NE.0.D0)THEN
         ILA=IFA+3
      ELSE
         ILA=IFA
      ENDIF
      IFB=NZTYPE(NJ)*4-3
      IF(C(IFB+1,1).NE.0.D0)THEN
         ILB=IFB+3
      ELSE
         ILB=IFB
      ENDIF
C
C  CONVERT R INTO AU
C
      R=R/0.529167D0
      R = R**2
      KA=0
      DO 80 I=IFA,ILA
         KA=KA+1
         NAT=KA-1
         KB=0
         DO 80 J=IFB,ILB
            KB=KB+1
            NBT=KB-1
C
C         DECIDE IS IT AN S-S, S-P, P-S, OR P-P OVERLAP
C
            IF(NAT.GT.0.AND.NBT.GT.0) THEN
C    P-P
               IS=4
               TOMB=(XI(NAT)-XJ(NAT))*(XI(NBT)
     1-XJ(NBT))*3.5711928576D0
            ELSEIF(NAT.GT.0) THEN
C    P-S
               IS=3
               TOMB=(XI(NAT)-XJ(NAT))*1.88976D0
            ELSEIF(NBT.GT.0) THEN
C    S-P
               IS=2
               TOMB=(XI(NBT)-XJ(NBT))*1.88976D0
            ELSE
C    S-S
               IS=1
            ENDIF
            DO 60 K=1,NGAUSS
               DO 60 L=1,NGAUSS
                  S(K,L)=0.0D0
                  AMB=Z(I,K)+Z(J,L)
                  APB=Z(I,K)*Z(J,L)
                  ADB=APB/AMB
C
C           CHECK OF OVERLAP IS NON-ZERO BEFORE STARTING
C
                  IF((ADB*R).LT.90.D0) THEN
                     ABN=1.0D0
                     GO TO(50,10,20,30),IS
   10                ABN=2.D0*TOMB*Z(I,K)*SQRT(Z(J,L))/AMB
                     GO TO 50
   20                ABN=-2.D0*TOMB*Z(J,L)*SQRT(Z(I,K))/AMB
                     GO TO 50
   30                ABN=-ADB*TOMB
                     IF(NAT.EQ.NBT) ABN=ABN+0.5D0
   40                ABN=4.0D0*ABN*SQRT(APB)/AMB
   50                S(K,L)=SQRT((2.D0*SQRT(APB)/AMB)**3)*EXP(-ADB*R)*
     .                      ABN
                  ENDIF
   60       CONTINUE
            SG(KA,KB)=0.0D0
            DO 70 K=1,NGAUSS
               DO 70 L=1,NGAUSS
   70       SG(KA,KB)=SG(KA,KB)+S(K,L)*C(I,K)*C(J,L)
   80 CONTINUE
      RETURN
      END
