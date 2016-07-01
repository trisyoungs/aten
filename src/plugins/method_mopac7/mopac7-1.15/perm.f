      SUBROUTINE PERM(IPERM,NELS,NMOS,MAXMOS,NPERMS)
      DIMENSION IPERM(MAXMOS,60), IADD(20), NEL(20)
************************************************************************
*
*  PERM PERMUTES NELS ENTITIES AMONG NMOS LOCATIONS. THE ENTITIES AND
*       LOCATIONS ARE EACH INDISTINGUISHABLE. THE PAULI EXCLUSION
*       PRINCIPLE IS FOLLOWED. THE NUMBER OF STATES PRODUCED IS GIVEN
*       BY NMOS!/(NELS!*(NMOS-NELS)!).
* ON INPUT: NELS  = NUMBER OF INDISTINGUISHABLE ENTITIES
*           NMOS  = NUMBER OF INDISTINGUISHABLE LOCATIONS
*
* ON OUTPUT IPERM = ARRAY OF PERMUTATIONS, A 0 INDICATES NO ENTITY,
*                   A 1 INDICATES AN ENTITY.
*           NPERM = NUMBER OF PERMUTATIONS.
*
************************************************************************
      IF(NELS.GT.NMOS)THEN
         WRITE(6,'('' NUMBER OF PARTICLES,'',I3,'' GREATER THAN NO. '',
     1''OF STATES,'',I3)')NELS,NMOS
         NPERMS=0
         RETURN
      ENDIF
      NPERMS=1
      DO 10 I=1,20
   10 NEL(I)=1000
      DO 20 I=1,NELS
   20 NEL(I)=1
      DO 50 I12=1-12+NELS,NMOS,NEL(12)
         IADD(12)=I12
         DO 50 I11=I12+1,NMOS,NEL(11)
            IADD(11)=I11
            DO 50 I10=I11+1,NMOS,NEL(10)
               IADD(10)=I10
               DO 50 I9=I10+1,NMOS,NEL(9)
                  IADD(9)=I9
                  DO 50 I8=I9+1,NMOS,NEL(8)
                     IADD(8)=I8
                     DO 50 I7=I8+1,NMOS,NEL(7)
                        IADD(7)=I7
                        DO 50 I6=I7+1,NMOS,NEL(6)
                           IADD(6)=I6
                           DO 50 I5=I6+1,NMOS,NEL(5)
                              IADD(5)=I5
                              DO 50 I4=I5+1,NMOS,NEL(4)
                                 IADD(4)=I4
                                 DO 50 I3=I4+1,NMOS,NEL(3)
                                    IADD(3)=I3
                                    DO 50 I2=I3+1,NMOS,NEL(2)
                                       IADD(2)=I2
                                       DO 50 I1=I2+1,NMOS,NEL(1)
                                          IADD(1)=I1
                                          DO 30 J=1,NMOS
   30                                     IPERM(J,NPERMS)=0
                                          DO 40 J=1,NELS
   40                                     IPERM(IADD(J),NPERMS)=1
                                          NPERMS=NPERMS+1
                                          IF(NPERMS.GT.61)THEN
                                             WRITE(6,'('' NUMBER OF PERM
     1UTATIONS TOO GREAT, LIMIT 60'')')
                                             GOTO 60
                                          ENDIF
   50 CONTINUE
   60 NPERMS=NPERMS-1
      RETURN
      END
