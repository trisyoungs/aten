      SUBROUTINE DOFS(EREF,MONO3,N,DD,M,BOTTOM,TOP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EREF(MONO3,N), DD(M)
************************************************************************
*                                                                      *
* DOFS FORMS A NORMALISED, SLIGHTLY SMOOTHED DENSITY OF STATES FOR A   *
*      1-D DENSITY OF STATES                                           *
*  ON INPUT EREF = REFERENCE ENERGY LEVELS                             *
*           P    = POPULATION OF ENERGY LEVELS (=1 FOR ENERGY D.O.S.   *
*                  OR ATOMIC ORBITAL POPULATION FOR PARTIAL D.O.S.)    *
*           N    = NUMBER OF ENERGY LEVELS SUPPLIED                    *
*           M    = SIZE OF D.O.S. VECTOR                               *
*           D    = ARRAY TO HOLD D.O.S.                                *
*           BOTTOM = BOTTOM OF D.O.S. VECTOR                           *
*           TOP    = TOP OF D.O.S. VECTOR                              *
*                                                                      *
*   ON OUTPUT D = DENSITY OF STATES.  THIS ANALYSES IS INDEPENDENT OF N*
*                 ROUGHNESS WILL OCCUR IF N < CA. 30.                  *
*                                                                      *
************************************************************************
C
C  FIRST, EMPTY THE DENSITY-OF-STATES (DOS) BINS
C
      DO 10 I=1,500
   10 DD(I)=0.D0
C
C   SPREAD OUT THE ENERGIES OVER THE ENERGY SPECTRUM, TOP TO BOTTOM
C
      RANGE=(M+1)/(TOP-BOTTOM)
      DO 20 J=1,MONO3
         DO 20 I=1,N
            X=EREF(J,I)
            IF(X.LT.BOTTOM.OR.X.GT.TOP) X=-1.D7
   20 EREF(J,I)=(X-BOTTOM)*RANGE
      DO 40 II=1,MONO3
         DO 40 I=2,N
            B=EREF(II,I-1)
            IF(B.LT.1) GOTO 40
            A=EREF(II,I)
            IF(A.LT.1) GOTO 40
            IF(B .GT. A) THEN
               X=B
               B=A
               A=X
            ENDIF
            J=B
            K=A
C
C IF J EQUALS K THE INTERVAL FALLS WITHIN ONE BIN
C
            IF(J.EQ.K) THEN
               DD(K)=DD(K)+1.D0
            ELSE
               SPREAD=1.D0/(A-B+1.D-12)
               PARTJ=(J+1-B)*SPREAD
               PARTK=(A-K)*SPREAD
               DD(J)=DD(J)+PARTJ
               DD(K)=DD(K)+PARTK
C
C IF K EQUALS J+1 THE INTERVAL STRADDLES TWO BINS
C
               IF(K .NE. J+1) THEN
C
C IF K IS GREATER THAN J+1 THE INTERVAL COVERS MORE THAN TWO BINS
C
                  J=J+1
                  K=K-1
                  DO 30 L=J,K
   30             DD(L)=DD(L)+SPREAD
               ENDIF
            ENDIF
   40 CONTINUE
      X=M/((N-1)*(TOP-BOTTOM))
      DO 50  I=1,M
   50 DD(I)=DD(I)*X
      WRITE(6,'(A)')' NORMALIZED DENSITY OF STATES'
C
C  THE FIRST 'BIN' HAS LOWER BOUND AT BOTTOM AND UPPER BOUND
C  AT BOTTOM+RANGE, THEREFORE THE FIRST 'BIN' IS FOR BOTTOM+0.5*RANGE
C  THE LAST 'BIN' HAS BOUNDS TOP-RANGE AND TOP,
C  THEREFOR THE LAST 'BIN' IS FOR TOP-0.5*RANGE
      RANGE=M/(TOP-BOTTOM)
      DO 60 I=1,M
   60 WRITE(6,'(F9.2,F12.6)')BOTTOM+(I-0.5D0)/RANGE,DD(I)
      RETURN
      END
