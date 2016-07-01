      SUBROUTINE CALPAR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /ONELEC/ USS(107),UPP(107),UDD(107)
     1       /ATOMIC/ EISOL(107),EHEAT(107)
     2       /ALPHA / ALP(107)
     3       /EXPONT/ ZS(107),ZP(107),ZD(107)
     4       /GAUSS / FN1(107),FN2(107)
     5       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     6       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107),
     7                GSD(107),GPD(107),GDD(107)
     8       /IDEAS / GUESS1(107,10), GUESS2(107,10), GUESS3(107,10)
      COMMON /MNDO/  USSM(107), UPPM(107), UDDM(107), ZSM(107),ZPM(107),
     1ZDM(107), BETASM(107), BETAPM(107), BETADM(107), ALPM(107),
     2EISOLM(107), DDM(107), QQM(107), AMM(107), ADM(107), AQM(107)
     3,GSSM(107),GSPM(107),GPPM(107),GP2M(107),HSPM(107), POLVOM(107)
      COMMON /KEYWRD/ KEYWRD
      COMMON/MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
      DIMENSION NSPQN(107)
      CHARACTER KEYWRD*241
      DIMENSION USSC(107), UPPC(107), GSSC(107), GSPC(107), HSPC(107),
     1GP2C(107), GPPC(107), UDDC(107), GSDC(107), GDDC(107)
      SAVE NSPQN, USSC, UPPC, UDDC, GSSC, GSPC, HSPC, GP2C, GSDC, GDDC
      DATA NSPQN/2*1,8*2,8*3,18*4,18*5,32*6,21*0/
C
C THE CONTINUATION LINES INDICATE THE PRINCIPAL QUANTUM NUMBER.
C
      DATA USSC/
     11.D0,                                                      0.D0,
     21.D0,                                               6*2.D0,0.D0,
     31.D0,                                               6*2.D0,0.D0,
     41.D0,4*2.D0,1.D0,4*2.D0,1.D0,                       6*2.D0,0.D0,
     51.D0,3*2.D0,2*1.D0,2.D0,2*1.D0,0.D0,1.D0,           6*2.D0,0.D0,
     61.D0,22*2.D0,1.D0,1.D0,                             6*2.D0,0.D0,
     721*0.D0/
      DATA  UPPC/
     1 2*0.D0,
     2 2*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     3 2*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     412*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     512*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     626*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     721*0.D0/
      DATA UDDC/18*0.D0,
     1 2*0.D0,1.D0,2.D0,3.D0,5.D0,5.D0,6.D0,7.D0,8.D0,1.D1,1.D1,6*0.D0,
     2 2*0.D0,1.D0,2.D0,4.D0,5.D0,5.D0,7.D0,8.D0,1.D1,1.D1,1.D1,6*0.D0,
     3 2*0.D0,1.D0,6*0.D0,1.D0,6*0.D0,1.D0,2.D0,3.D0,4.D0,
     4                           5.D0,6.D0,7.D0,9.D0,1.D1,1.D1,6*0.D0,
     5 21*0.D0/
      DATA GSSC/2*0.D0,
     1 0.D0,6*1.D0,0.D0,
     2 0.D0,6*1.D0,0.D0,
     3 0.D0,4*1.D0,0.D0,4*1.D0,0.D0,6*1.D0,0.D0,
     4 0.D0,3*1.D0,7*0.D0,6*1.D0,0.D0,
     5 0.D0,22*1.D0,2*1.D0,6*1.D0,0.D0,
     6 21*0.D0/
      DATA GSPC/2*0.D0,
     1 2*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     2 2*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     312*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     412*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     526*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     621*0.D0/
      DATA HSPC/2*0.D0,
     1 2*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     2 2*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     312*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     412*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     526*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     621*0.D0/
      DATA GP2C/2*0.D0,
     1 3*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     2 3*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     313*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     413*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     527*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     621*0.D0/
      DATA GPPC/2*0.D0,
     1 3*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     2 3*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     313*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     413*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     527*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     621*0.D0/
     7GSDC/18*0.D0,
     8 2*0.D0,2.D0,4.D0,6.D0,5.D0,10.D0,12.D0,14.D0,16.D0,10.D0,7*0.D0,
     9 2*0.D0,2.D0,4.D0,4.D0,5.D0,6.D0,7.D0,8.D0,0.D0,1.D1,7*0.D0,
     1 2*0.D0,2.D0,6*0.D0,2.D0,6*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,12.D0,
     2                                         14.D0,9.D0,10.D0,7*0.D0,
     321*0.D0/
      DATA GDDC/18*0.D0,
     1 3*0.D0,1.D0,3.D0,10.D0,10.D0,15.D0,21.D0,28.D0,8*0.D0,
     2 3*0.D0,1.D0,6.D0,10.D0,15.D0,21.D0,28.D0,45.D0,8*0.D0,
     317*0.D0,1.D0,3.D0, 6.D0,10.D0,15.D0,21.D0,36.D0,8*0.D0,
     421*0.D0/
C  The DATA block shown above is derived from the ground-state atomic
C  configuration of the elements.  In checking it, pay careful attention
C  to the actual ground-state configuration. Note also that there are no
C  configurations which have both p and d electrons in the valence shell
C
C     SET SCALING PARAMETER.
      P=2.D0
      P2=P*P
      P4=P**4
      DO 30 I=2,98
         IF(ZP(I).LT.1.D-4.AND.ZS(I).LT.1.D-4)GOTO 30
**********************************************************************
*
*   CONSTRAINTS ON THE POSSIBLE VALUES OF PARAMETERS
*
**********************************************************************
         IF(ZP(I).LT.0.3D0) ZP(I)=0.3D0
C  PUT IN ANY CONSTRAINTS AT THIS POINT
**********************************************************************
         HPP=0.5D0*(GPP(I)-GP2(I))
         HPP=MAX(0.1D0,HPP)
         HSP(I)=MAX(1.D-7,HSP(I))
         EISOL(I)=USS(I)*USSC(I)+UPP(I)*UPPC(I)+UDD(I)*UDDC(I)+
     1         GSS(I)*GSSC(I)+GPP(I)*GPPC(I)+GSP(I)*GSPC(I)+
     2         GP2(I)*GP2C(I)+HSP(I)*HSPC(I)+GSD(I)*GSDC(I)+
     3         GDD(I)*GDDC(I)
         QN=NSPQN(I)
         DD(I)=(2.D0*QN+1)*(4.D0*ZS(I)*ZP(I))**(QN+0.5D0)/(ZS(I)+ZP(I))
     1**(2.D0*QN+2)/SQRT(3.D0)
         DDM(I)=DD(I)
         QQ(I)=SQRT((4.D0*QN*QN+6.D0*QN+2.D0)/20.D0)/ZP(I)
         QQM(I)=QQ(I)
C     CALCULATE ADDITIVE TERMS, IN ATOMIC UNITS.
         JMAX=5
         GDD1= (P2*HSP(I)/(27.21D0* 4.D0*DD(I)**2))**(1.D0/3.D0)
         GQQ= (P4*HPP/(27.21D0*48.D0*QQ(I)**4))**0.2D0
         D1=GDD1
         D2=GDD1+0.04D0
         Q1=GQQ
         Q2=GQQ+0.04D0
         DO 10 J=1,JMAX
            DF=D2-D1
            HSP1= 2.D0*D1 - 2.D0/SQRT(4.D0*DD(I)**2+1.D0/D1**2)
            HSP2= 2.D0*D2 - 2.D0/SQRT(4.D0*DD(I)**2+1.D0/D2**2)
            HSP1= HSP1/P2
            HSP2= HSP2/P2
            D3= D1 + DF*(HSP(I)/27.21D0-HSP1)/(HSP2-HSP1)
            D1= D2
            D2= D3
   10    CONTINUE
         DO 20 J=1,JMAX
            QF=Q2-Q1
            HPP1= 4.D0*Q1 - 8.D0/SQRT(4.D0*QQ(I)**2+1.D0/Q1**2)
     1            + 4.D0/SQRT(8.D0*QQ(I)**2+1.D0/Q1**2)
            HPP2= 4.D0*Q2 - 8.D0/SQRT(4.D0*QQ(I)**2+1.D0/Q2**2)
     1            + 4.D0/SQRT(8.D0*QQ(I)**2+1.D0/Q2**2)
            HPP1= HPP1/P4
            HPP2= HPP2/P4
            Q3= Q1 + QF*(HPP/27.21D0-HPP1)/(HPP2-HPP1)
            Q1= Q2
            Q2= Q3
   20    CONTINUE
         AM(I)= GSS(I)/27.21D0
         AD(I)= D2
         AQ(I)= Q2
         AMM(I)=AM(I)
         ADM(I)=AD(I)
         AQM(I)=AQ(I)
   30 CONTINUE
      EISOL(1)=USS(1)
      AM(1)=GSS(1)/27.21D0
      AD(1)=AM(1)
      AQ(1)=AM(1)
      AMM(1)=AM(1)
      ADM(1)=AD(1)
      AQM(1)=AQ(1)
C
C     DEBUG PRINTING.
C     THIS IS FORMATTED FOR DIRECT INSERTION INTO 'PARAM'
C
      IF(INDEX(KEYWRD,'DEP').EQ.0) RETURN
      WRITE(6,50)
      DO 60 I=1,107
         IF(ZS(I).EQ.0) GOTO 60
         WRITE(6,'(''C'',20X,''DATA FOR ELEMENT'',I3)')I
         WRITE(6,'(6X,''DATA USSPM3('',I3,'')/'',F16.7,''D0/'')')
     1                    I,USS(I)
         IF(UPP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA UPPPM3('',I3,'')/'',F16.7,''D0/'')')I,UPP(I)
         IF(UDD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA UDDPM3('',I3,'')/'',F16.7,''D0/'')')I,UDD(I)
         IF(BETAS(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA BETASP('',I3,'')/'',F16.7,''D0/'')')
     2I,BETAS(I)
         IF(BETAP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA BETAPP('',I3,'')/'',F16.7,''D0/'')')
     2I,BETAP(I)
         IF(BETAD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA BETADP('',I3,'')/'',F16.7,''D0/'')')
     2I,BETAD(I)
         WRITE(6,'(6X,''DATA ZSPM3 ('',I3,'')/'',F16.7,''D0/'')')
     1I,ZS(I)
         IF(ZP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA ZPPM3 ('',I3,'')/'',F16.7,''D0/'')')I,ZP(I)
         IF(ZD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA ZDPM3 ('',I3,'')/'',F16.7,''D0/'')')I,ZD(I)
         WRITE(6,'(6X,''DATA ALPPM3('',I3,'')/'',F16.7,''D0/'')')
     1I,ALP(I)
         WRITE(6,'(6X,''DATA EISOLP('',I3,'')/'',F16.7,''D0/'')')
     1I,EISOL(I)
         IF(GSS(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GSSPM3('',I3,'')/'',F16.7,''D0/'')')
     2I,GSS(I)
         IF(GSP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GSPPM3('',I3,'')/'',F16.7,''D0/'')')
     2I,GSP(I)
         IF(GPP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GPPPM3('',I3,'')/'',F16.7,''D0/'')')
     2I,GPP(I)
         IF(GP2(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GP2PM3('',I3,'')/'',F16.7,''D0/'')')
     2I,GP2(I)
         IF(HSP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA HSPPM3('',I3,'')/'',F16.7,''D0/'')')
     2I,HSP(I)
         IF(DD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA DDPM3 ('',I3,'')/'',F16.7,''D0/'')')I,DD(I)
         IF(QQ(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA QQPM3 ('',I3,'')/'',F16.7,''D0/'')')I,QQ(I)
         WRITE(6,'(6X,''DATA AMPM3 ('',I3,'')/'',F16.7,''D0/'')')
     1I,AM(I)
         IF(AD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA ADPM3 ('',I3,'')/'',F16.7,''D0/'')')I,AD(I)
         IF(AQ(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA AQPM3 ('',I3,'')/'',F16.7,''D0/'')')I,AQ(I)
         IF(FN1(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA FN1PM3('',I3,'')/'',F16.7,''D0/'')')I,FN1(I)
         IF(FN2(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA FN2PM3('',I3,'')/'',F16.7,''D0/'')')I,FN2(I)
         DO 40 J=1,10
            IF(GUESS1(I,J) .EQ.0.D0) GOTO 40
            WRITE(6,'(6X,''DATA GUESP1('',I3,'','',I1,'')/'',
     1F16.7,''D0/'')')I,J,GUESS1(I,J)
            WRITE(6,'(6X,''DATA GUESP2('',I3,'','',I1,'')/'',
     1F16.7,''D0/'')')I,J,GUESS2(I,J)
            WRITE(6,'(6X,''DATA GUESP3('',I3,'','',I1,'')/'',
     1F16.7,''D0/'')')I,J,GUESS3(I,J)
   40    CONTINUE
   50    FORMAT(1H ,1X,'OUTPUT INCLUDES DEBUG INFORMATION',//)
   60 CONTINUE
      RETURN
      END
