      SUBROUTINE MOLDAT(MODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,20),NNHCO,ITYPE
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
     3       /KEYWRD/ KEYWRD
     4       /NATORB/ NATORB(107)
      COMMON /CORE  / CORE(107)
     1       /BETAS / BETAS(107),BETAP(107),BETAD(107)
     2       /MOLORB/ USPD(MAXORB),PSPD(MAXORB)
     3       /VSIPS / VS(107),VP(107),VD(107)
     4       /ONELEC/ USS(107),UPP(107),UDD(107)
      COMMON /ATHEAT/ ATHEAT
     1       /POLVOL/ POLVOL(107)
     2       /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
     3       /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107)
     4                ,GSD(107),GPD(107),GDD(107)
     5       /IDEAA / GUESA1(107,10),GUESA2(107,10),GUESA3(107,10)
     6       /IDEAS / GUESS1(107,10),GUESS2(107,10),GUESS3(107,10)
     7       /IDEAP / GUESP1(107,10),GUESP2(107,10),GUESP3(107,10)
      COMMON /ALPHA / ALP(107)
     1       /REFS/ ALLREF(107,4)
      COMMON /MNDO/  USSM(107), UPPM(107), UDDM(107), ZSM(107),
     1ZPM(107), ZDM(107), BETASM(107), BETAPM(107), BETADM(107),
     2ALPM(107), EISOLM(107), DDM(107), QQM(107), AMM(107),
     3ADM(107), AQM(107), GSSM(107), GSPM(107), GPPM(107),
     4GP2M(107), HSPM(107), POLVOM(107)
      COMMON /PM3 /  USSPM3(107), UPPPM3(107), UDDPM3(107), ZSPM3(107),
     1ZPPM3(107), ZDPM3(107), BETASP(107), BETAPP(107), BETADP(107),
     2ALPPM3(107), EISOLP(107), DDPM3(107), QQPM3(107), AMPM3(107),
     3ADPM3(107), AQPM3(107) ,GSSPM3(107), GSPPM3(107), GPPPM3(107),
     4GP2PM3(107), HSPPM3(107),POLVOP(107)
      COMMON /AM1BLO/USSAM1(107), UPPAM1(107), UDDAM1(107), ZSAM1(107),
     1ZPAM1(107), ZDAM1(107), BETASA(107), BETAPA(107), BETADA(107),
     2ALPAM1(107), EISOLA(107), DDAM1(107), QQAM1(107), AMAM1(107),
     3ADAM1(107), AQAM1(107) ,GSSAM1(107), GSPAM1(107), GPPAM1(107),
     4GP2AM1(107), HSPAM1(107),POLVOA(107)
      COMMON /GEOM  / GEO(3,NUMATM), XCOORD(3,NUMATM)
      PARAMETER (MDUMY=MAXPAR**2-MPACK)
      COMMON /SCRACH/ RXYZ(MPACK), XDUMY(MDUMY)
*
*  COMMON BLOCKS FOR MINDO/3
*
      COMMON /ONELE3 /  USS3(18),UPP3(18)
     1       /ATOMI3 /  EISOL3(18),EHEAT3(18)
     2       /EXPON3 /  ZS3(18),ZP3(18)
*
*  END OF MINDO/3 COMMON BLOCKS
*
      COMMON /EXPONT/ ZS(107),ZP(107),ZD(107)
      COMMON /ATOMIC/ EISOL(107),EHEAT(107)
      DIMENSION COORD(3,NUMATM), ISWAP(2,20), ESTORE(107)
      CHARACTER*241 KEYWRD, OLDE(20)*6, ALLREF*80
      LOGICAL DEBUG, UHF,EXCI, TRIP, MINDO3, BIRAD, AM1, LPM3,
     1LMNDO, HALFE, SLOW
      DATA ESTORE(1)/0.D0/
      IF(ESTORE(1).EQ.0.D0)THEN
      DO 9 I=1,107
   9  ESTORE(I)=EHEAT(I)
      ENDIF
      DO 8 I=1,107
  8   EHEAT(I)=ESTORE(I)
      DEBUG = (INDEX(KEYWRD,'MOLDAT').NE.0)
      LPM3  = (INDEX(KEYWRD,'PM3').NE.0)
      MINDO3= (INDEX(KEYWRD,'MINDO').NE.0)
      UHF=(INDEX(KEYWRD,'UHF') .NE. 0)
      AM1= (INDEX(KEYWRD,'AM1').NE.0)
      LMNDO=(.NOT.AM1.AND..NOT.LPM3)
      KHARGE=0
      I=INDEX(KEYWRD,'CHARGE')
      IF(I.NE.0) KHARGE=READA(KEYWRD,I)
      ELECS=-KHARGE
      NDORBS=0
      ATHEAT=0.D0
      EAT=0.D0
      NUMAT=0
      IF(MODE.EQ.1) GOTO 80
      IF ( LMNDO ) THEN
*
*    SWITCH IN MNDO PARAMETERS
*
C
C       ZERO OUT GAUSSIAN 1 FOR CARBON.  THIS WILL BE USED IN
C       ROTATE TO DECIDE WHETHER OR NOT TO USE AM1-TYPE GAUSSIANS
C
         GUESS1(6,1)=0.D0
         DO 10 I=1,107
            IF(.NOT.MINDO3) POLVOL(I)=POLVOM(I)
            ZS(I)=ZSM(I)
            ZP(I)=ZPM(I)
            ZD(I)=ZDM(I)
            USS(I)=USSM(I)
            UPP(I)=UPPM(I)
            UDD(I)=UDDM(I)
            BETAS(I)=BETASM(I)
            BETAP(I)=BETAPM(I)
            BETAD(I)=BETADM(I)
            ALP(I)=ALPM(I)
            EISOL(I)=EISOLM(I)
            DD(I)=DDM(I)
            QQ(I)=QQM(I)
            AM(I)=AMM(I)
            AD(I)=ADM(I)
            AQ(I)=AQM(I)
            GSS(I)=GSSM(I)
            GPP(I)=GPPM(I)
            GSP(I)=GSPM(I)
            GP2(I)=GP2M(I)
            HSP(I)=HSPM(I)
   10    CONTINUE
      ELSEIF( LPM3 ) THEN
*
*    SWITCH IN MNDO-PM3 PARAMETERS
*
         DO 30 I=1,107
            DO 20 J=1,10
               GUESS1(I,J)=GUESP1(I,J)
               GUESS2(I,J)=GUESP2(I,J)
   20       GUESS3(I,J)=GUESP3(I,J)
            POLVOL(I)=POLVOP(I)
            ZS(I)=ZSPM3(I)
            ZP(I)=ZPPM3(I)
            ZD(I)=ZDPM3(I)
            USS(I)=USSPM3(I)
            UPP(I)=UPPPM3(I)
            UDD(I)=UDDPM3(I)
            BETAS(I)=BETASP(I)
            BETAP(I)=BETAPP(I)
            BETAD(I)=BETADP(I)
            ALP(I)=ALPPM3(I)
            EISOL(I)=EISOLP(I)
            DD(I)=DDPM3(I)
            QQ(I)=QQPM3(I)
            AM(I)=AMPM3(I)
            AD(I)=ADPM3(I)
            AQ(I)=AQPM3(I)
            GSS(I)=GSSPM3(I)
            GPP(I)=GPPPM3(I)
            GSP(I)=GSPPM3(I)
            GP2(I)=GP2PM3(I)
            HSP(I)=HSPPM3(I)
   30    CONTINUE
      ELSE
*
*    SWITCH IN AM1 PARAMETERS
*
         DO 50 I=1,107
            DO 40 J=1,10
               GUESS1(I,J)=GUESA1(I,J)
               GUESS2(I,J)=GUESA2(I,J)
   40       GUESS3(I,J)=GUESA3(I,J)
            POLVOL(I)=POLVOA(I)
            ZS(I)=ZSAM1(I)
            ZP(I)=ZPAM1(I)
            ZD(I)=ZDAM1(I)
            USS(I)=USSAM1(I)
            UPP(I)=UPPAM1(I)
            UDD(I)=UDDAM1(I)
            BETAS(I)=BETASA(I)
            BETAP(I)=BETAPA(I)
            BETAD(I)=BETADA(I)
            ALP(I)=ALPAM1(I)
            EISOL(I)=EISOLA(I)
            DD(I)=DDAM1(I)
            QQ(I)=QQAM1(I)
            AM(I)=AMAM1(I)
            AD(I)=ADAM1(I)
            AQ(I)=AQAM1(I)
            GSS(I)=GSSAM1(I)
            GPP(I)=GPPAM1(I)
            GSP(I)=GSPAM1(I)
            GP2(I)=GP2AM1(I)
            HSP(I)=HSPAM1(I)
   50    CONTINUE
      ENDIF
C
C        SWAP IN OLD PARAMETERS FOR ELEMENTS.  OLDE CONTAINS THE
C        CHARACTER NAME OF THE ELEMENT, AND ISWAP(1,1:NEWELE) CONTAINS
C        THE ATOMIC NUMBER OF THE ELEMENT. ISWAP(2,1:NEWELE) CONTAINS
C        THE STORAGE ADDRESS OF THE OLD SET OF PARAMETERS.
C
      NEWELE=2
      OLDE(1)=' S1978'
      ISWAP(1,1)=16
      ISWAP(2,1)=91
      OLDE(2)='SI1978'
      ISWAP(1,2)=14
      ISWAP(2,2)=90
C$DOIT ASIS
      DO 60 K=1,NEWELE
         IF(INDEX(KEYWRD,OLDE(K)).NE.0)THEN
            I=ISWAP(1,K)
            J=ISWAP(2,K)
            ALLREF(I,3)=ALLREF(J,1)
            ALLREF(I,1)=ALLREF(J,1)
            ZS(I)=ZS(J)
            ZP(I)=ZP(J)
            ZD(I)=ZD(J)
            USS(I)=USS(J)
            UPP(I)=UPP(J)
            UDD(I)=UDD(J)
            BETAS(I)=BETAS(J)
            BETAP(I)=BETAP(J)
            BETAD(I)=BETAD(J)
            ALP(I)=ALP(J)
            EISOL(I)=EISOL(J)
            DD(I)=DD(J)
            QQ(I)=QQ(J)
            AM(I)=AM(J)
            AD(I)=AD(J)
            AQ(I)=AQ(J)
            IF(GSS(J).NE.0)GSS(I)=GSS(J)
            IF(GPP(J).NE.0)GPP(I)=GPP(J)
            IF(GSP(J).NE.0)GSP(I)=GSP(J)
            IF(GP2(J).NE.0)GP2(I)=GP2(J)
            IF(HSP(J).NE.0)HSP(I)=HSP(J)
         ENDIF
   60 CONTINUE
      IF( MINDO3 ) THEN
         DO 70 I=1,17
            IF(I.NE.2.AND.I.NE.10)THEN
               USS(I)=USS3(I)
               UPP(I)=UPP3(I)
               EISOL(I)=EISOL3(I)
               EHEAT(I)=EHEAT3(I)
               ZS(I)=ZS3(I)
               ZP(I)=ZP3(I)
               GSS(I)=GSSM(I)
               GPP(I)=GPPM(I)
               GSP(I)=GSPM(I)
               GP2(I)=GP2M(I)
               HSP(I)=HSPM(I)
            ENDIF
   70    CONTINUE
      ENDIF
   80 CONTINUE
      IF(USS(1) .GT. -1.D0) THEN
         WRITE(6,'(''  THE HAMILTONIAN REQUESTED IS NOT AVAILABLE IN''
     1,'' THIS PROGRAM'')')
         STOP
      ENDIF
      IA=1
      IB=0
      NHEAVY=0
      DO 130 II=1,NATOMS
         IF(LABELS(II).EQ.99.OR.LABELS(II).EQ.107) GOTO 130
         NUMAT=NUMAT+1
         NAT(NUMAT)=LABELS(II)
         NFIRST(NUMAT)=IA
         NI=NAT(NUMAT)
         ATHEAT=ATHEAT+EHEAT(NI)
         EAT   =EAT   +EISOL(NI)
         ELECS=ELECS+CORE(NI)
         IB=IA+NATORB(NI)-1
         NMIDLE(NUMAT)=IB
         IF(NATORB(NI).EQ.9)NDORBS=NDORBS+5
         IF(NATORB(NI).EQ.9)NMIDLE(NUMAT)=IA+3
         NLAST(NUMAT)=IB
         IF(IA.GT.MAXORB) GOTO 270
         USPD(IA)=USS(NI)
         IF(IA.EQ.IB) GOTO 120
         K=IA+1
         K1=IA+3
C$DOIT ASIS
         DO 90 J=K,K1
            IF(J.GT.MAXORB) GOTO 270
            USPD(J)=UPP(NI)
   90    CONTINUE
         NHEAVY=NHEAVY+1
  100    IF(K1.EQ.IB)GOTO 120
         K=K1+1
C$DOIT ASIS
         DO 110 J=K,IB
  110    USPD(J)=UDD(NI)
  120    CONTINUE
  130 IA=IB+1
      IF(NUMAT.EQ.1)THEN
         IF(INDEX(KEYWRD,'FORCE').NE.0)THEN
            WRITE(6,'(///,A)')'      A SINGLE ATOM HAS NO VIBRATIONAL MO
     1DES'
            STOP
         ENDIF
      ENDIF
      IF(MODE.NE.1)CALL REFER
      ATHEAT=ATHEAT-EAT*23.061D0
      NORBS=NLAST(NUMAT)
      IF(NORBS.GT.MAXORB)THEN
         WRITE(6,'(//10X,''**** MAX. NUMBER OF ORBITALS:'',I4,/
     1            10X,''NUMBER OF ORBITALS IN SYSTEM:'',I4)')
     2MAXORB,NORBS
         STOP
      ENDIF
      NLIGHT=NUMAT-NHEAVY
      N2EL=50*NHEAVY*(NHEAVY-1)+10*NHEAVY*NLIGHT+(NLIGHT*(NLIGHT-1))/2
      IF(N2EL.GT.N2ELEC)THEN
         WRITE(6,'(//10X,''**** MAX. NUMBER OF TWO-ELECTRON INTEGRALS:''
     1,I8,/
     2            10X,''NUMBER OF TWO ELECTRON INTEGRALS IN SYSTEM:'',
     3I8)')
     4N2ELEC,N2EL
         STOP
      ENDIF
C
C   NOW TO CALCULATE THE NUMBER OF LEVELS OCCUPIED
      TRIP=(INDEX(KEYWRD,'TRIP').NE.0)
      EXCI=(INDEX(KEYWRD,'EXCI').NE.0)
      BIRAD=(EXCI.OR.INDEX(KEYWRD,'BIRAD').NE.0)
      IF(INDEX(KEYWRD,'C.I.') .NE. 0 .AND. UHF ) THEN
         WRITE(6,'(//10X,''C.I. NOT ALLOWED WITH UHF '')')
         STOP
      ENDIF
C
C NOW TO WORK OUT HOW MANY ELECTRONS ARE IN EACH TYPE OF SHELL
C
      NALPHA=0
      NBETA=0
C
C      PROTECT DUMB USERS FROM DUMB ERRORS!
C
      NELECS=MAX(ELECS+0.5D0,0.D0)
      NELECS=MIN(2*NORBS,NELECS)
      NCLOSE=0
      NOPEN=0
      IF( UHF ) THEN
         FRACT=1.D0
         NBETA=NELECS/2
         IF( TRIP ) THEN
            IF(NBETA*2 .NE. NELECS) THEN
               WRITE(6,'(//10X,''TRIPLET SPECIFIED WITH ODD NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ELSE
               IF(MODE.NE.1)
     1WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
               NBETA=NBETA-1
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'QUAR').NE.0) THEN
            IF(NBETA*2 .EQ. NELECS) THEN
               WRITE(6,'(//10X,''QUARTET SPECIFIED WITH EVEN NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ELSE
               IF(MODE.NE.1)
     1WRITE(6,'(//'' QUARTET STATE CALCULATION'')')
               NBETA=NBETA-1
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'QUIN').NE.0) THEN
            IF(NBETA*2 .NE. NELECS) THEN
               WRITE(6,'(//10X,''QUINTET SPECIFIED WITH ODD NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ELSE
               IF(MODE.NE.1)
     1WRITE(6,'(//'' QUINTET STATE CALCULATION'')')
               NBETA=NBETA-2
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'SEXT').NE.0) THEN
            IF(NBETA*2 .EQ. NELECS) THEN
               WRITE(6,'(//10X,''SEXTET SPECIFIED WITH EVEN NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ELSE
               IF(MODE.NE.1)WRITE(6,'(//'' SEXTET STATE CALCULATION'')')
               NBETA=NBETA-2
            ENDIF
         ENDIF
         NALPHA=NELECS-NBETA
         IF(MODE.NE.1)
     1WRITE(6,'(//10X,''UHF CALCULATION, NO. OF ALPHA ELECTRONS ='',I
     23,/27X,''NO. OF BETA  ELECTRONS ='',I3)')NALPHA,NBETA
      ELSE
C
C   NOW TO DETERMINE OPEN AND CLOSED SHELLS
C
         IELEC=0
         ILEVEL=0
         IF( TRIP .OR. EXCI .OR. BIRAD ) THEN
            IF( (NELECS/2)*2 .NE. NELECS) THEN
               WRITE(6,'(//10X,''SYSTEM SPECIFIED WITH ODD NUMBER'',
     1            '' OF ELECTRONS, CORRECT FAULT '')')
               STOP
            ENDIF
            IF(MODE.NE.1) THEN
               IF(BIRAD)WRITE(6,'(//'' SYSTEM IS A BIRADICAL'')')
               IF(TRIP )WRITE(6,'(//'' TRIPLET STATE CALCULATION'')')
               IF(EXCI )WRITE(6,'(//'' EXCITED STATE CALCULATION'')')
            ENDIF
            IELEC=2
            ILEVEL=2
         ELSEIF((NELECS/2)*2.NE.NELECS) THEN
            IELEC=1
            ILEVEL=1
         ENDIF
         IF(INDEX(KEYWRD,'QUAR').NE.0) THEN
            IF(MODE.NE.1)WRITE(6,'(//'' QUARTET STATE CALCULATION'')')
            IELEC=3
            ILEVEL=3
         ENDIF
         IF(INDEX(KEYWRD,'QUIN').NE.0) THEN
            IF(MODE.NE.1)WRITE(6,'(//'' QUINTET STATE CALCULATION'')')
            IELEC=4
            ILEVEL=4
         ENDIF
         IF(INDEX(KEYWRD,'SEXT').NE.0) THEN
            IF(MODE.NE.1)WRITE(6,'(//'' SEXTET STATE CALCULATION'')')
            IELEC=5
            ILEVEL=5
         ENDIF
         I=INDEX(KEYWRD,'OPEN(')
         IF(I.NE.0)THEN
            IELEC=READA(KEYWRD,I)
            ILEVEL=READA(KEYWRD,I+7)
         ENDIF
         NCLOSE=NELECS/2
         NOPEN = NELECS-NCLOSE*2
         IF( IELEC.NE.0 )THEN
            IF((NELECS/2)*2.EQ.NELECS .NEQV.
     1                  (IELEC/2)*2.EQ.IELEC) THEN
               WRITE(6,'('' IMPOSSIBLE NUMBER OF OPEN SHELL ELECTR
     1ONS'')')
               STOP
            ENDIF
            NCLOSE=NCLOSE-IELEC/2
            NOPEN=ILEVEL
            IF(NCLOSE+NOPEN.GT.NORBS)THEN
               WRITE(6,'(A)')' NUMBER OF DOUBLY FILLED PLUS PARTLY FILLE
     1D LEVELS'
               WRITE(6,'(A)')' GREATER THAN TOTAL NUMBER OF ORBITALS.'
               STOP
            ENDIF
            FRACT=IELEC*1.D0/ILEVEL
            IF(MODE.NE.1)
     1WRITE(6,'('' THERE ARE'',I3,'' DOUBLY FILLED LEVELS'')')NCLOSE
         ENDIF
         IF(MODE.NE.1)WRITE(6,'(//10X,''RHF CALCULATION, NO. OF '',
     1''DOUBLY OCCUPIED LEVELS ='',I3)')NCLOSE
         IF(MODE.NE.1 .AND. NOPEN.NE.0.AND.ABS(FRACT-1.D0).LT.1.D-4)
     1WRITE(6,'(/27X,''NO. OF SINGLY OCCUPIED LEVELS ='',I3)')NOPEN
         IF(MODE.NE.1 .AND. NOPEN.NE.0.AND.ABS(FRACT-1.D0).GT.1.D-4)
     1WRITE(6,'(/27X,''NO. OF LEVELS WITH OCCUPANCY'',F6.3,''  ='',I3)')
     2FRACT,NOPEN
         IF(INDEX(KEYWRD,'C.I.=(').NE.0) THEN
            I=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+5)-
     1      READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+7)
            IF(NOPEN.GT.I)THEN
               WRITE(6,'(//,'' NUMBER OF OPEN-SHELLS ALLOWED IN C.I. IS
     1LESS ''            /''    THAN THAT SPECIFIED BY OTHER KEYWORDS'')
     2')
               STOP
            ENDIF
         ENDIF
         IF(INDEX(KEYWRD,'C.I.').NE.0.AND.NOPEN.EQ.0)THEN
            NOPEN=1
            NCLOSE=NCLOSE-1
            FRACT=2.D0
         ENDIF
         NOPEN=NOPEN+NCLOSE
      ENDIF
C
C  WORK OUT IF DEFINED SPIN-STATE ALLOWED
C
      MSDEL=INDEX(KEYWRD,' MS')
      IF(MSDEL.NE.0)THEN
         MSDEL=1.0001D0*READA(KEYWRD,INDEX(KEYWRD,' MS'))
      ELSE
         IF(INDEX(KEYWRD,'TRIP')+INDEX(KEYWRD,'QUAR').GT.0)MSDEL=1
         IF(INDEX(KEYWRD,'QUIN')+INDEX(KEYWRD,'SEXT').GT.0)MSDEL=2
      ENDIF
      IF(MSDEL.NE.0.AND..NOT.UHF)THEN
C
C   MSDEL = NUMBER OF ALPHA ELECTRONS - NUMBER OF BETA ELECTRONS
C
         NDOUBL=99
         IF(INDEX(KEYWRD,'C.I.=(').NE.0)THEN
            NDOUBL=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+7)
            NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=(')+5)
         ELSEIF (INDEX(KEYWRD,'C.I.=').NE.0)THEN
            NMOS=READA(KEYWRD,INDEX(KEYWRD,'C.I.=')+5)
         ELSE
            NMOS=NOPEN-NCLOSE
         ENDIF
         IF(NDOUBL.EQ.99)THEN
            J=MAX(MIN((NCLOSE+NOPEN+1)/2-(NMOS-1)/2,NORBS-NMOS+1),1)
         ELSE
            J=NCLOSE-NDOUBL+1
         ENDIF
         NE=MAX(0.D0,(NCLOSE-J+1.D0))*2.D0+
     1     MAX(0.D0,(NOPEN-NCLOSE)*FRACT) + 0.5D0
         NUPP=(NE+1)/2+MSDEL
         NDOWN=NE-NUPP
C
C  NUPP  = NUMBER OF ALPHA ELECTRONS IN ACTIVE SPACE
C  NDOWN = NUMBER OF BETA  ELECTRONS IN ACTIVE SPACE
C
         IF(NUPP*NDOWN.LT.0.OR.NUPP.GT.NMOS.OR.NDOWN.GT.NMOS)THEN
            WRITE(6,'(A)')
     1' SPECIFIED SPIN COMPONENT NOT SPANNED BY ACTIVE SPACE'
            STOP
         ENDIF
      ENDIF
C#      WRITE(6,'(''  NOPEN,NCLOSE,NALPHA,NBETA,FRACT'',4I4,F12.5)')
C#     1 NOPEN, NCLOSE, NLAPHA, NBETA, FRACT
C
C   MAKE SURE ANALYT IS NOT USED WITH ANALYTICAL C.I. DERIVATIVES
C
      HALFE = (NOPEN.GT.NCLOSE.AND.FRACT.NE.2.D0.AND.FRACT.NE.0.D0
     1 .OR. INDEX(KEYWRD,'C.I.').NE.0)
      SLOW=(INDEX(KEYWRD,'EXCI').NE.0.OR.
     1INDEX(KEYWRD,'ROOT').NE.0.AND.INDEX(KEYWRD,'ROOT=1').EQ.0)
      IF(HALFE)HALFE=(.NOT.SLOW)
      IF(INDEX(KEYWRD,'NOANCI').EQ.0.AND.
     1INDEX(KEYWRD,'ANALYT').NE.0.AND.HALFE)THEN
         WRITE(6,*)
         WRITE(6,'(A)')' KEYWORD ''ANALYT'' CANNOT BE USED HERE: ',
     1' ANALYICAL C.I. DERIVATIVES MUST USE FINITE DIFFERENCES',
     2' TO CORRECT, REMOVE KEYWORD ''ANALYT'' OR ADD ''NOANCI'''
         STOP
      ENDIF
      YY=FLOAT(KHARGE)/(NORBS+1.D-10)
      L=0
      DO 160 I=1,NUMAT
         NI=NAT(I)
         XX=1.D0/(NLAST(I)-NFIRST(I)+1+1.D-10)
         W=CORE(NI)*XX-YY
         IA=NFIRST(I)
         IC=NMIDLE(I)
         IB=NLAST(I)
C$DOIT ASIS
         DO 140 J=IA,IC
            L=L+1
  140    PSPD(L)=W
C$DOIT ASIS
         DO 150 J=IC+1,IB
            L=L+1
  150    PSPD(L)=0.D0
  160 CONTINUE
C
C   WRITE OUT THE INTERATOMIC DISTANCES
C
      CALL GMETRY(GEO,COORD)
      RMIN=100.D0
      L=0
      DO 170 I=1,NUMAT
         DO 170 J=1,I
            L=L+1
            RXYZ(L)=SQRT((COORD(1,I)-COORD(1,J))**2+
     1                     (COORD(2,I)-COORD(2,J))**2+
     2                     (COORD(3,I)-COORD(3,J))**2)
            IF(RMIN.GT.RXYZ(L) .AND. I .NE. J .AND.
     1 (NAT(I).LT.103 .OR. NAT(J).LT.103)) THEN
               IMINR=I
               JMINR=J
               RMIN=RXYZ(L)
            ENDIF
  170 CONTINUE
      NNHCO=0
C
C   SET UP MOLECULAR-MECHANICS CORRECTION TO -(C=O)-(NH)- LINKAGE
C   THIS WILL BE USED IF MMOK HAS BEEN SPECIFIED.
C
      ITYPE=1
      IF(INDEX(KEYWRD,'AM1').NE.0)ITYPE=2
      IF(INDEX(KEYWRD,'PM3').NE.0)ITYPE=3
      IF(INDEX(KEYWRD,'MINDO').NE.0)ITYPE=4
C
C   IDENTIFY O=C-N-H SYSTEMS VIA THE INTERATOMIC DISTANCES MATRIX
      DO 220 I=1,NUMAT
         IF(NAT(I).NE.8) GOTO 220
         DO 210 J=1,NUMAT
            IF(NAT(J).NE.6) GOTO 210
            IJ=MAX(I,J)
            JI=I+J-IJ
            IF(RXYZ((IJ*(IJ-1))/2+JI).GT.1.3)GOTO 210
            DO 200 K=1,NUMAT
               IF(NAT(K).NE.7) GOTO 200
               JK=MAX(J,K)
               KJ=J+K-JK
               IF(RXYZ((JK*(JK-1))/2+KJ).GT.1.6)GOTO 200
               DO 190 L=1,NUMAT
                  IF(NAT(L).NE.1) GOTO 190
                  KL=MAX(K,L)
                  LK=K+L-KL
                  IF(RXYZ((KL*(KL-1))/2+LK).GT.1.3)GOTO 190
C
C   WE HAVE A H-N-C=O SYSTEM.  THE ATOM NUMBERS ARE L-K-J-I
C   NOW SEARCH OUT ATOM ATTACHED TO NITROGEN, THIS SPECIFIES
C   THE SYSTEM X-N-C=O
C
                  DO 180 M=1,NUMAT
                     IF(M.EQ.K.OR.M.EQ.L.OR.M.EQ.J) GOTO 180
                     MK=MAX(M,K)
                     KM=M+K-MK
                     IF(RXYZ((MK*(MK-1))/2+KM).GT.1.7)GOTO 180
                     NNHCO=NNHCO+1
                     NHCO(1,NNHCO)=I
                     NHCO(2,NNHCO)=J
                     NHCO(3,NNHCO)=K
                     NHCO(4,NNHCO)=M
                     NNHCO=NNHCO+1
                     NHCO(1,NNHCO)=I
                     NHCO(2,NNHCO)=J
                     NHCO(3,NNHCO)=K
                     NHCO(4,NNHCO)=L
                     GOTO 190
  180             CONTINUE
  190          CONTINUE
  200       CONTINUE
  210    CONTINUE
  220 CONTINUE
      IF(MODE.NE.1.AND.NNHCO.NE.0)THEN
         IF(INDEX(KEYWRD,'MMOK').NE.0) THEN
            WRITE(6,'(A)')' MOLECULAR MECHANICS CORRECTION APPLIED TO PE
     1PTIDELINKAGE'
         ELSEIF(INDEX(KEYWRD,'NOMM').NE.0)THEN
            WRITE(6,'(A,I2,2A)')' THERE ARE ',NNHCO/2,' PEPTIDE LINKAGES
     1',' IDENTIFIED IN THIS SYSTEM'
            WRITE(6,'(A)')' IF YOU WANT MM CORRECTION TO THE CONH BARRIE
     1R, ADD THE KEY-WORD "MMOK"'
            NNHCO=0
         ELSE
            WRITE(6,'(A)')' THIS SYSTEM CONTAINS -HNCO- GROUPS.'
            WRITE(6,'(A)')' YOU MUST SPECIFY "NOMM" OR "MMOK" REGARDING
     1MOLECULAR MECHANICS CORRECTION'
            STOP
         ENDIF
      ENDIF
      IF(MODE.NE.1.AND.INDEX(KEYWRD,'NOINTER') .EQ. 0) THEN
         WRITE(6,'(//10X,''  INTERATOMIC DISTANCES'')')
         CALL VECPRT(RXYZ,NUMAT)
      ENDIF
      IF(RMIN.LT.0.8D0.AND.INDEX(KEYWRD,'GEO-OK') .EQ.0) THEN
         WRITE(6,230)IMINR,JMINR,RMIN
  230    FORMAT(//,'   ATOMS',I3,' AND',I3,' ARE SEPARATED BY',F8.4,
     1' ANGSTROMS.',/'   TO CONTINUE CALCULATION SPECIFY "GEO-OK"')
         STOP
      ENDIF
      IF(.NOT. DEBUG) RETURN
      WRITE(6,240)NUMAT,NORBS,NDORBS,NATOMS
  240 FORMAT('   NUMBER OF REAL ATOMS:',I4,/
     1      ,'   NUMBER OF ORBITALS:  ',I4,/
     2      ,'   NUMBER OF D ORBITALS:',I4,/
     3      ,'   TOTAL NO. OF ATOMS:  ',I4)
      WRITE(6,250)(USPD(I),I=1,NORBS)
  250 FORMAT('   ONE-ELECTRON DIAGONAL TERMS',/,10(/,10F8.3))
      WRITE(6,260)(PSPD(I),I=1,NORBS)
  260 FORMAT('   INITIAL P FOR ALL ATOMIC ORBITALS',/,10(/,10F8.3))
      RETURN
  270 WRITE(6,'(//10X,'' MAXIMUM NUMBER OF ATOMIC ORBITALS EXCEEDED'')')
      WRITE(6,'(  10X,'' MAXIMUM ALLOWED ='',I4)')MAXORB
      STOP
      END
