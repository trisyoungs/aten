      SUBROUTINE DELMOL(COORD,I,J,NI,NJ,IA,ID,JA,JD,IX,RIJ,TOMB,ISP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION COORD(3,25)
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22),TX(3),TY(3),TZ(3)
      IF(NI.GT.1.OR.NJ.GT.1) CALL ROTAT(COORD,I,J,IX,RIJ,TOMB,2)
      IB=MAX(IA,ID)
      JB=MAX(JA,JD)
      DO 10 K=IA,IB
         KK=K-IA
         DO 10 L=K,IB
            LL=L-IA
            DO 10 M=JA,JB
               MM=M-JA
               DO 10 N=M,JB
                  NN=N-JA
                  ISP=ISP+1
                  IF(NN.EQ.0)THEN
                     IF(LL.EQ.0) THEN
C   (SS/SS)
                        DR(ISP)=DG(1)
                     ELSEIF(KK.EQ.0) THEN
C   (SP/SS)
                        DR(ISP)=DG(2)*TX(LL)+G(2)*TDX(LL)
                     ELSE
C   (PP/SS)
                        DR(ISP)=DG(3)*TX(KK)*TX(LL)
     1       +G(3)*(TDX(KK)*TX(LL)+TX(KK)*TDX(LL))
     2       +DG(4)*(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))
     3       +G(4)*(TDY(KK)*TY(LL)+TY(KK)*TDY(LL)
     4             +TDZ(KK)*TZ(LL)+TZ(KK)*TDZ(LL))
                     ENDIF
                  ELSEIF(MM.EQ.0) THEN
                     IF(LL.EQ.0) THEN
C   (SS/SP)
                        DR(ISP)=DG(5)*TX(NN)+G(5)*TDX(NN)
                     ELSEIF(KK.EQ.0) THEN
C   (SP/SP)
                        DR(ISP)=DG(6)*TX(LL)*TX(NN)
     1       +G(6)*(TDX(LL)*TX(NN)+TX(LL)*TDX(NN))
     2       +DG(7)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     3       +G(7)*(TDY(LL)*TY(NN)+TY(LL)*TDY(NN)
     4             +TDZ(LL)*TZ(NN)+TZ(LL)*TDZ(NN))
                     ELSE
C   (PP/SP)
                        DR(ISP)=DG(8)*TX(KK)*TX(LL)*TX(NN)
     1       +G(8)*(TDX(KK)*TX(LL)*TX(NN)+TX(KK)*TDX(LL)*TX(NN)
     2             +TX(KK)*TX(LL)*TDX(NN))
     3       +DG(9)*(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))*TX(NN)
     4       +G(9)*((TDY(KK)*TY(LL)+TY(KK)*TDY(LL)
     5              +TDZ(KK)*TZ(LL)+TZ(KK)*TDZ(LL))*TX(NN)
     6             +(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))*TDX(NN))
     7       +DG(10)*(TX(KK)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     8               +TX(LL)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN)))
     9       +G(10)*(TDX(KK)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     1              +TDX(LL)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     2              +TX(KK)*(TDY(LL)*TY(NN)+TY(LL)*TDY(NN)
     3                      +TDZ(LL)*TZ(NN)+TZ(LL)*TDZ(NN))
     4              +TX(LL)*(TDY(KK)*TY(NN)+TY(KK)*TDY(NN)
     5                      +TDZ(KK)*TZ(NN)+TZ(KK)*TDZ(NN)))
                     ENDIF
                  ELSEIF(LL.EQ.0) THEN
C   (SS/PP)
                     DR(ISP)=DG(11)*TX(MM)*TX(NN)
     1       +G(11)*(TDX(MM)*TX(NN)+TX(MM)*TDX(NN))
     2       +DG(12)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     3       +G(12)*(TDY(MM)*TY(NN)+TY(MM)*TDY(NN)
     4              +TDZ(MM)*TZ(NN)+TZ(MM)*TDZ(NN))
                  ELSEIF(KK.EQ.0) THEN
C   (SP/PP)
                     DR(ISP)=DG(13)*TX(LL)*TX(MM)*TX(NN)
     1       +G(13)*(TDX(LL)*TX(MM)*TX(NN)+TX(LL)*TDX(MM)*TX(NN)
     2              +TX(LL)*TX(MM)*TDX(NN))
     3       +DG(14)*TX(LL)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     4       +G(14)*(TDX(LL)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     5              +TX(LL)*(TDY(MM)*TY(NN)+TY(MM)*TDY(NN)
     6                      +TDZ(MM)*TZ(NN)+TZ(MM)*TDZ(NN)))
     7       +DG(15)*(TY(LL)*(TY(MM)*TX(NN)+TY(NN)*TX(MM))
     8               +TZ(LL)*(TZ(MM)*TX(NN)+TZ(NN)*TX(MM)))
     9       +G(15)*(TDY(LL)*(TY(MM)*TX(NN)+TY(NN)*TX(MM))
     1              +TDZ(LL)*(TZ(MM)*TX(NN)+TZ(NN)*TX(MM))
     2              +TY(LL)*(TDY(MM)*TX(NN)+TY(MM)*TDX(NN)
     3                      +TDY(NN)*TX(MM)+TY(NN)*TDX(MM))
     4              +TZ(LL)*(TDZ(MM)*TX(NN)+TZ(MM)*TDX(NN)
     5                      +TDZ(NN)*TX(MM)+TZ(NN)*TDX(MM)))
                  ELSE
C   (PP/PP)
                     DR(ISP)=DG(16)*TX(KK)*TX(LL)*TX(MM)*TX(NN)
     1       +G(16)*(TDX(KK)*TX(LL)*TX(MM)*TX(NN)
     2              +TX(KK)*TDX(LL)*TX(MM)*TX(NN)
     3              +TX(KK)*TX(LL)*TDX(MM)*TX(NN)
     4              +TX(KK)*TX(LL)*TX(MM)*TDX(NN))
     5       +DG(17)*(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))*TX(MM)*TX(NN)
     6       +G(17)*((TDY(KK)*TY(LL)+TY(KK)*TDY(LL)
     7               +TDZ(KK)*TZ(LL)+TZ(KK)*TDZ(LL))*TX(MM)*TX(NN)
     8              +(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))
     9              *(TDX(MM)*TX(NN)+TX(MM)*TDX(NN)))
     1       +DG(18)*TX(KK)*TX(LL)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     2       +G(18)*((TDX(KK)*TX(LL)+TX(KK)*TDX(LL))
     3                 *(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     4              +TX(KK)*TX(LL)*(TDY(MM)*TY(NN)+TY(MM)*TDY(NN)
     5                             +TDZ(MM)*TZ(NN)+TZ(MM)*TDZ(NN)))
                     DR(ISP)=DR(ISP)
     1       +DG(19)*(TY(KK)*TY(LL)*TY(MM)*TY(NN)
     2                  +TZ(KK)*TZ(LL)*TZ(MM)*TZ(NN))
     3       +G(19)*(TDY(KK)*TY(LL)*TY(MM)*TY(NN)
     4                 +TY(KK)*TDY(LL)*TY(MM)*TY(NN)
     5                 +TY(KK)*TY(LL)*TDY(MM)*TY(NN)
     6                 +TY(KK)*TY(LL)*TY(MM)*TDY(NN)
     7                 +TDZ(KK)*TZ(LL)*TZ(MM)*TZ(NN)
     8                 +TZ(KK)*TDZ(LL)*TZ(MM)*TZ(NN)
     9                 +TZ(KK)*TZ(LL)*TDZ(MM)*TZ(NN)
     1                 +TZ(KK)*TZ(LL)*TZ(MM)*TDZ(NN))
     2       +DG(20)*(TX(KK)*(TX(MM)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     3                          +TX(NN)*(TY(LL)*TY(MM)+TZ(LL)*TZ(MM)))
     4                  +TX(LL)*(TX(MM)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     5                          +TX(NN)*(TY(KK)*TY(MM)+TZ(KK)*TZ(MM))))
C      TO AVOID COMPILER DIFFICULTIES THIS IS DIVIDED
                     TEMP1=         TDX(KK)*(TX(MM)*(TY(LL)*TY(NN)+TZ(LL
     1)*TZ(NN))                          +TX(NN)*(TY(LL)*TY(MM)+TZ(LL)*T
     2Z(MM)))                 +TDX(LL)*(TX(MM)*(TY(KK)*TY(NN)+TZ(KK)*TZ(
     3NN))                          +TX(NN)*(TY(KK)*TY(MM)+TZ(KK)*TZ(MM)
     4))                 +TX(KK)*(TDX(MM)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     5                         +TDX(NN)*(TY(LL)*TY(MM)+TZ(LL)*TZ(MM)))
     6                 +TX(LL)*(TDX(MM)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     7                         +TDX(NN)*(TY(KK)*TY(MM)+TZ(KK)*TZ(MM)))
                     TEMP2=            TX(KK)*(TX(MM)*(TDY(LL)*TY(NN)+TY
     1(LL)*TDY(NN)                                 +TDZ(LL)*TZ(NN)+TZ(LL
     2)*TDZ(NN))                         +TX(NN)*(TDY(LL)*TY(MM)+TY(LL)*
     3TDY(MM)                                 +TDZ(LL)*TZ(MM)+TZ(LL)*TDZ
     4(MM)))                 +TX(LL)*(TX(MM)*(TDY(KK)*TY(NN)+TY(KK)*TDY(
     5NN)                                 +TDZ(KK)*TZ(NN)+TZ(KK)*TDZ(NN)
     6)                         +TX(NN)*(TDY(KK)*TY(MM)+TY(KK)*TDY(MM)
     7                                 +TDZ(KK)*TZ(MM)+TZ(KK)*TDZ(MM)))
                     DR(ISP)=DR(ISP)+G(20)*(TEMP1+TEMP2)
                     DR(ISP)=DR(ISP)
     1       +DG(21)*(TY(KK)*TY(LL)*TZ(MM)*TZ(NN)
     2                 +TZ(KK)*TZ(LL)*TY(MM)*TY(NN))
     3       +G(21)*(TDY(KK)*TY(LL)*TZ(MM)*TZ(NN)
     4                 +TY(KK)*TDY(LL)*TZ(MM)*TZ(NN)
     5                 +TY(KK)*TY(LL)*TDZ(MM)*TZ(NN)
     6                 +TY(KK)*TY(LL)*TZ(MM)*TDZ(NN)
     7                 +TDZ(KK)*TZ(LL)*TY(MM)*TY(NN)
     8                 +TZ(KK)*TDZ(LL)*TY(MM)*TY(NN)
     9                 +TZ(KK)*TZ(LL)*TDY(MM)*TY(NN)
     1                 +TZ(KK)*TZ(LL)*TY(MM)*TDY(NN))
                     DR(ISP)=DR(ISP)
     1       +DG(22)*(TY(KK)*TZ(LL)+TZ(KK)*TY(LL))
     2                 *(TY(MM)*TZ(NN)+TZ(MM)*TY(NN))
     3       +G(22)*((TDY(KK)*TZ(LL)+TY(KK)*TDZ(LL)
     4                  +TDZ(KK)*TY(LL)+TZ(KK)*TDY(LL))
     5                 *(TY(MM)*TZ(NN)+TZ(MM)*TY(NN))
     6                 +(TY(KK)*TZ(LL)+TZ(KK)*TY(LL))
     7                 *(TDY(MM)*TZ(NN)+TY(MM)*TDZ(NN)
     8                  +TDZ(MM)*TY(NN)+TZ(MM)*TDY(NN)))
                  ENDIF
   10 CONTINUE
      RETURN
      END
