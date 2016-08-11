      SUBROUTINE ROTAT(COORD,I,J,IX,RIJ,DEL1,IDX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22),TX(3),TY(3),TZ(3)
      DIMENSION COORD(3,25)
      XD=COORD(1,I)-COORD(1,J)
      YD=COORD(2,I)-COORD(2,J)
      ZD=COORD(3,I)-COORD(3,J)
      RXY=SQRT(XD*XD+YD*YD)
      RYZ=SQRT(YD*YD+ZD*ZD)
      RZX=SQRT(ZD*ZD+XD*XD)
      DO 10 IJK=1,3
         TX(IJK)=0.0D0
         TY(IJK)=0.0D0
         TZ(IJK)=0.0D0
         TDX(IJK)=0.0D0
         TDY(IJK)=0.0D0
         TDZ(IJK)=0.0D0
   10 CONTINUE
      IF(RXY.LT.1.0D-4) THEN
C   MOLECULAR Z AXIS IS PARALLEL TO DIATOMIC Z AXIS
         TX(3)=1.0D0
         IF(ZD.LT.0.0D0) TX(3)=-1.0D0
         TY(2)=1.0D0
         TZ(1)=TX(3)
         IF(IDX.EQ.1) RETURN
         IF(IX.EQ.1) TDX(1)=1.0D0/RIJ
         IF(IX.EQ.2) TDX(2)=1.0D0/RIJ
         IF(IX.EQ.1) TDZ(3)=-1.0D0/RIJ
         IF(IX.EQ.2) TDY(3)=-TX(3)/RIJ
      ELSEIF(RYZ.LT.1.0D-4) THEN
C   MOLECULAR X AXIS IS PARALLEL TO DIATOMIC Z AXIS
         TX(1)=1.0D0
         IF(XD.LT.0.0D0) TX(1)=-1.0D0
         TY(2)=TX(1)
         TZ(3)=1.0D0
         IF(IDX.EQ.1) RETURN
         IF(IX.EQ.2) TDX(2)=1.0D0/RIJ
         IF(IX.EQ.3) TDX(3)=1.0D0/RIJ
         IF(IX.EQ.2) TDY(1)=-1.0D0/RIJ
         IF(IX.EQ.3) TDZ(1)=-TX(1)/RIJ
      ELSEIF(RZX.LT.1.0D-4) THEN
C   MOLECULAR Y AXIS IS PARALLEL TO DIATOMIC Z AXIS
         TX(2)=1.0D0
         IF(YD.LT.0.0D0) TX(2)=-1.0D0
         TY(1)=-TX(2)
         TZ(3)=1.0D0
         IF(IDX.EQ.1) RETURN
         IF(IX.EQ.1) TDX(1)=1.0D0/RIJ
         IF(IX.EQ.3) TDX(3)=1.0D0/RIJ
         IF(IX.EQ.1) TDY(2)=1.0D0/RIJ
         IF(IX.EQ.3) TDZ(2)=-TX(2)/RIJ
      ELSE
         TX(1)=XD/RIJ
         TX(2)=YD/RIJ
         TX(3)=ZD/RIJ
         TZ(3)=RXY/RIJ
         TY(1)=-TX(2)*SIGN(+1.0D0,TX(1))/TZ(3)
         TY(2)=ABS(TX(1)/TZ(3))
         TY(3)=0.0D0
         TZ(1)=-TX(1)*TX(3)/TZ(3)
         TZ(2)=-TX(2)*TX(3)/TZ(3)
         IF(IDX.EQ.1) RETURN
         TERM=DEL1/(RIJ*RIJ)
         IF(IX.EQ.1)THEN
            TDX(1)=1.0D0/RIJ-TX(1)*TERM
            TDX(2)=-TX(2)*TERM
            TDX(3)=-TX(3)*TERM
            TDZ(3)=TX(1)/RXY-TZ(3)*TERM
         ELSEIF(IX.EQ.2) THEN
            TDX(1)=-TX(1)*TERM
            TDX(2)=1.0D0/RIJ-TX(2)*TERM
            TDX(3)=-TX(3)*TERM
            TDZ(3)=TX(2)/RXY-TZ(3)*TERM
         ELSEIF(IX.EQ.3)THEN
            TDX(1)=-TX(1)*TERM
            TDX(2)=-TX(2)*TERM
            TDX(3)=1.0D0/RIJ-TX(3)*TERM
            TDZ(3)=-TZ(3)*TERM
         ENDIF
         TDY(1)=-TDX(2)/TZ(3)+TX(2)*TDZ(3)/TZ(3)**2
         IF(TX(1).LT.0.0D0) TDY(1)=-TDY(1)
         TDY(2)=TDX(1)/TZ(3)-TX(1)*TDZ(3)/TZ(3)**2
         IF(TX(1).LT.0.0D0) TDY(2)=-TDY(2)
         TDY(3)=0.0D0
         TDZ(1)=-TX(3)*TDX(1)/TZ(3)-TX(1)*TDX(3)/TZ(3)
     1 +TX(1)*TX(3)*TDZ(3)/TZ(3)**2
         TDZ(2)=-TX(3)*TDX(2)/TZ(3)-TX(2)*TDX(3)/TZ(3)
     1 +TX(2)*TX(3)*TDZ(3)/TZ(3)**2
      ENDIF
      RETURN
      END
