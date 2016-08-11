      SUBROUTINE DERS(M,N,RR,DEL1,DEL2,DEL3,IS,IOL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*                                                                      *
*    ON INPUT M    = INDEX OF FIRST ATOMIC ORBITAL                     *
*             N    = INDEX OF SECOND ATOMIC ORBITAL                    *
*             RR   = SQUARE IF INTERATOMIC DIATANCE (IN BOHR)          *
*             DEL1 = CATERSIAN DISTANCE IN DERIVATIVE DIRECTION        *
*             DEL2 = CARTESIAN DISTANCE IN M A.O.'S DIRECTION          *
*             DEL3 = CARTESIAN DISTANCE IN N A.O.'S DIRECTION          *
*             IS   = INDICATES TYPE OF A.O.-A.O. INTERACTION           *
*                  = 1 S/S, 2 S/P', 3 S/P, 4 P'/S, 5 P/S, 6 P/P',      *
*                    7 P'/P", 8 P'P', 9 P/P                            *
*             IOL  = INDEX FOR STORING DERIVATIVES IN DS               *
*                                                                      *
************************************************************************
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /TEMP/  CG(60,6),ZG(60,6)
      DIMENSION SS(6,6)
      A0=0.529167D0
      DO 110 I=1,6
         DO 110 J=1,6
            SS(I,J)=0.0D0
            APB=ZG(M,I)*ZG(N,J)
            AMB=ZG(M,I)+ZG(N,J)
            ADB=APB/AMB
            ADR=MIN(ADB*RR,35.D0)
            GO TO (10,20,30,40,50,60,70,80,90),IS
   10       ABN=-2.0D0*ADB*DEL1/(A0**2)
            GO TO 100
   20       ABN=-4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZG(N,J))*(A0**3))
            GO TO 100
   30       ABN=(2.0D0*ADB/(SQRT(ZG(N,J))*A0))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   40       ABN=4.0D0*(ADB**2)*DEL1*DEL2/(SQRT(ZG(M,I))*(A0**3))
            GO TO 100
   50       ABN=-(2.0D0*ADB/(SQRT(ZG(M,I))*A0))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   60       ABN=-(4.0D0*(ADB**2)*DEL2/(SQRT(APB)*(A0**2)))*
     1 (1.0D0-2.0D0*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   70       ABN=8.0D0*(ADB**3)*DEL1*DEL2*DEL3/(SQRT(APB)*(A0**4))
            GO TO 100
   80       ABN=-(8.0D0*(ADB**2)*DEL1/(SQRT(APB)*(A0**2)))*
     1 (0.5D0-ADB*(DEL2**2)/(A0**2))
            GO TO 100
   90       ABN=-(8.0D0*(ADB**2)*DEL1/(SQRT(APB)*(A0**2)))*
     1 (1.5D0-ADB*(DEL1**2)/(A0**2))
  100       SS(I,J)=SQRT((2.0D0*SQRT(APB)/AMB)**3)*EXP(-ADR)*ABN
  110 CONTINUE
      DO 120 I=1,6
         DO 120 J=1,6
            DS(IOL)=DS(IOL)+SS(I,J)*CG(M,I)*CG(N,J)
  120 CONTINUE
      RETURN
      END
