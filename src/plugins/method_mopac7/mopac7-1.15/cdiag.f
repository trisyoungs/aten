CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     this subroutine is moved to the top of the
C     source file in order to resolve the mis-
C     matching parameter types later in code.
C     2005-10-11 Tommi Hassinen
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE EC08C(A,B,VALUE,VEC,N,IV,W)
C
C TO FIND THE EIGENVALUES AND VECTORS OF A TRI-DIAGONAL
C  HERMITIAN MATRIX.
      REAL VALUE(*),W(*),PCK(2),ONE,ZERO,VEC(*)
      COMPLEX A(*),B(*),DN,UPCK
      EQUIVALENCE (PCK(1),UPCK)
C  WE TREAT VEC AS IF IT IS DEFINED AS COMPLEX VEC(IV,N)
C  IN THE CALLING PROGRAM.
      DATA ONE, ZERO/1.0,0.0/
      IV2=IV+IV
      N2=N+N
      IL=IV2*(N-1)+1
      W(1)=A(1)
C
C  THE HERMITIAN FORM IS TRANSFORMED INTO A REAL FORM
      IF(N-1)80,80,10
   10 DO 20 I=2,N
         W(I)=A(I)
   20 W(I+N)=CABS(B(I))
C
C  FIND THE EIGENVALUES AND VECTORS OF THE REAL FORM
   30 CALL EA08C(W,W(N+1),VALUE,VEC,N,IV2,W(N2+1))
C
C THE VECTORS IN VEC AT THIS POINT ARE REAL,WE NOW EXPAND THEM
C INTO VEC AS THOUGH THEY WERE COMPLEX.
      DO 50 I=1,IL,IV2
         DO 40 J=1,N
            K=N-J
            L=K+K
   40    VEC(I+L)=VEC(I+K)
   50 VEC(I+1)=ZERO
      IF(N.LE.1)GO TO 80
      DN=ONE
      K=1
C
C TRANSFORM VECTORS OF REAL FORM TO THOSE OF COMPLEX FORM.
      DO 70 I=4,N2,2
         K=K+1
         UPCK=ONE
         IF(W(K+N).NE.ZERO)UPCK=DN*CONJG(B(K))/W(K+N)
         I1=IL-1+I
         DO 60 J=I,I1,IV2
            VEC(J)=VEC(J-1)*PCK(2)
   60    VEC(J-1)=VEC(J-1)*PCK(1)
   70 DN=UPCK
   80 RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     this subroutine is moved to the top of the
C     source file in order to resolve the mis-
C     matching parameter types later in code.
C     2005-10-11 Tommi Hassinen
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE ME08B (A,Q,B,N,IA)
      REAL A(IA,*),Q(2,*),B(IA,*)
      DO 10 I=1,N
         A(1,I)=A(1,I) -Q(1,1)*B(1,I)+Q(2,1)*B(2,I)
     1 -Q(1,I)*B(1,1)+Q(2,I)*B(2,1)
   10 A(2,I)=A(2,I)-Q(2,1)*B(1,I)-Q(1,1)*B(2,I)
     1 +Q(2,I)*B(1,1)+Q(1,I)*B(2,1)
      RETURN
      END
      SUBROUTINE CDIAG(A,VALUE,VEC,N, NEED)
C
C TO FIND THE EIGENVALUES AND EIGENVECTORS OF A HERMITIAN MATRIX.
      REAL VALUE(*),H
      REAL, DIMENSION(:,:), ALLOCATABLE :: RVEC
      real, dimension(:), allocatable :: rw
      COMPLEX W(6000)
      COMPLEX A(N,*),VEC(N,*)
      COMPLEX  FM06AS
      INTEGER NVEC1,NVEC2,NDIM
!Get the dimensions of vec
      nvec1=N
	  ndim=2
	  nvec2=size(vec,ndim)
      if(allocated(rvec)) deallocate(rvec)
      allocate(rvec(nvec1,nvec2))
      do i1=1,nvec1
         do i2=1,nvec2
            rvec(i1,i2)=real(vec(i1,i2))
         end do
      end do
      if(allocated(rw)) deallocate(rw)
      allocate(rw(size(w)))
      do i1=1,size(w)
         rw=real(w(i1))
      end do
      IA=N
      IV=N
C
C REDUCE MATRIX TO A TRI-DIAGONAL HERMITIAN MATRIX.
      CALL ME08A(A,W,W(N+1),N,IA,W(2*N+1))
C
C FIND THE EIGENVALUES AND EIGENVECTORS OF THE TRI-DIAGONAL MATRIX
      CALL EC08C(W,W(N+1),VALUE,RVEC,N,IV,RW(2*N+1))
      IF(NEED.EQ.0) GOTO 50
      IF(N.LT.2)RETURN
C
C THE EIGENVECTORS OF THE ORIGINAL MATRIX ARE NOW FOUND BY
C BACK TRANSFORMATION USING INFORMATION STORE IN THE UPPER
C TRIANGLE OF MATRIX A (BY ME08)
      DO 40 II=3,N
         I=N-II+1
         H=W(N+I+1)*CONJG(A(I,I+1))
         IF(H)10,40,10
   10    DO30L=1,N
            I1=I+1
            S=FM06AS(N-I,A(I,I+1),IA,VEC(I+1,L),1)
            S=S/H
            DO20K=I1,N
   20       VEC(K,L)=VEC(K,L)+CONJG(A(I,K))*S
   30    CONTINUE
   40 CONTINUE
   50 CALL SORT(VALUE,VEC,N)
      RETURN
      END
      SUBROUTINE EA08C(A,B,VALUE,VEC,M,IV,W)
C  STANDARD FORTRAN 66 (A VERIFIED PFORT SUBROUTINE)
      DIMENSION A(*),B(*),VALUE(*),VEC(*),W(*)
      DATA EPS/1.E-6/,A34/0.0/
C     THIS USES QR ITERATION TO FIND THE EIGENVALUES AND EIGENVECTORS
C  OF THE SYMMETRIC TRIDIAGONAL MATRIX WHOSE DIAGONAL ELEMENTS ARE
C  A(I),I=1,M AND OFF-DIAGONAL ELEMENTS ARE B(I),I=2,M.  THE ARRAY
C  W IS USED FOR WORKSPACE AND MUST HAVE DIMENSION AT LEAST 2*M.
C  WE TREAT VEC AS IF IT HAD DIMENSIONS (IV,M).
      SML=EPS*FLOAT(M)
      CALL EA09C(A,B,W(M+1),M,W)
C     SET VEC TO THE IDENTITY MATRIX.
      DO 20 I=1,M
         VALUE(I)=A(I)
         W(I)=B(I)
         K=(I-1)*IV+1
         L=K+M-1
         DO 10 J=K,L
   10    VEC(J)=0.
         KI=K+I-1
   20 VEC(KI)=1.
      K=0
      IF(M.EQ.1)RETURN
      DO 100 N3=2,M
         N2=M+2-N3
C     EACH QR ITERATION IS PERFORMED OF ROWS AND COLUMNS N1 TO N2
         MN2=M+N2
         ROOT=W(MN2)
         DO 80 ITER=1,20
            BB=(VALUE(N2)-VALUE(N2-1))*0.5
            CC=W(N2)*W(N2)
            A22=VALUE(N2)
            IF(CC.NE.0.0)A22=A22+CC/(BB+SIGN(1.0,BB)*SQRT(BB*BB+CC))
            DO 30 I=1,N2
               MI=M+I
               IF(ABS(ROOT-A22).LE.ABS(W(MI)-A22))GO TO 30
               ROOT=W(MI)
               MN=M+N2
               W(MI)=W(MN)
               W(MN)=ROOT
   30       CONTINUE
            DO 40 II=2,N2
               N1=2+N2-II
               IF(ABS(W(N1)).LE.(ABS(VALUE(N1-1))+ABS(VALUE(N1)))*SML)GO
     1 TO 50
   40       CONTINUE
            N1=1
   50       IF(N2.EQ.N1) GO TO 100
            N2M1=N2-1
            IF(ITER.GE.3)ROOT=A22
            K=K+1
            A22=VALUE(N1)
            A12=A22-ROOT
            A23=W(N1+1)
            A13=A23
            DO70 I=N1,N2M1
               A33=VALUE(I+1)
               IF(I.NE.N2M1)A34=W  (I+2)
               S=SIGN(SQRT(A12*A12+A13*A13),A12)
               SI=A13/S
               CO=A12/S
               JK=I*IV+1
               J1=JK-IV
               J2=J1+MIN0(M,I+K)-1
               DO 60 JI=J1,J2
                  V1=VEC(JI)
                  V2=VEC(JK)
                  VEC(JI)=V1*CO+V2*SI
                  VEC(JK)=V2*CO-V1*SI
   60          JK=JK+1
               IF(I.NE.N1)  W(I)=S
               A11=CO*A22+SI*A23
               A12=CO*A23+SI*A33
               A13=SI*A34
               A21=CO*A23-SI*A22
               A22=CO*A33-SI*A23
               A23=CO*A34
               VALUE(I)=A11*CO+A12*SI
               A12=-A11*SI+A12*CO
               W(I+1)=A12
   70       A22=A22*CO-A21*SI
   80    VALUE(N2)=A22
         WRITE(6,90)
   90    FORMAT(48H1CYCLE DETECTED IN SUBROUTINE EA08 -STOPPING NOW)
         STOP
  100 CONTINUE
C     RAYLEIGH QUOTIENT
      DO 120 J=1,M
         K=(J-1)*IV
         XX=VEC(K+1)**2
         XAX=XX*A(1)
         DO 110 I=2,M
            KI=K+I
            XX=XX+VEC(KI)**2
  110    XAX=XAX+VEC(KI)*(2.*B(I)*VEC(KI-1)+A(I)*VEC(KI))
  120 VALUE(J)=XAX/XX
      RETURN
      END
      SUBROUTINE EA09C(A,B,VALUE,M,OFF)
C  STANDARD FORTRAN 66 (A VERFIED PFORT SUBROUTINE)
      DIMENSION A(*),B(*),VALUE(*),OFF(*)
      DATA A34/0.0/,EPS/1.0E-6/
      SML=EPS*FLOAT(M)
      VALUE(1)=A(1)
      IF(M.EQ.1)RETURN
      DO 10 I=2,M
         VALUE(I)=A(I)
   10 OFF(I)=B(I)
C     EACH QR ITERATION IS PERFORMED OF ROWS AND COLUMNS N1 TO N2
      MAXIT=10*M
      DO 80 ITER=1,MAXIT
         DO 40 N3=2,M
            N2=M+2-N3
            DO 20 II=2,N2
               N1=2+N2-II
               IF(ABS(OFF(N1)).LE.(ABS(VALUE(N1-1))+ABS(VALUE(N1)))*SML)
     1GO TO 30
   20       CONTINUE
            N1=1
   30       IF(N2.NE.N1) GO TO 50
   40    CONTINUE
         RETURN
C     ROOT  IS THE EIGENVALUE OF THE BOTTOM 2*2 MATRIX THAT IS NEAREST
C     TO THE LAST MATRIX ELEMENT AND IS USED TO ACCELERATE THE
C     CONVERGENCE
   50    BB=(VALUE(N2)-VALUE(N2-1))*0.5
         CC=OFF(N2)*OFF(N2)
         SBB=1.
         IF(BB.LT.0.)SBB=-1.
         ROOT=VALUE(N2)+CC/(BB+SBB*SQRT(BB*BB+CC))
         N2M1=N2-1
   60    A22=VALUE(N1)
         A12=A22-ROOT
         A23=OFF(N1+1)
         A13=A23
         DO 70 I=N1,N2M1
            A33=VALUE(I+1)
            IF(I.NE.N2M1)A34=OFF(I+2)
            S=SQRT(A12*A12+A13*A13)
            SI=A13/S
            CO=A12/S
            IF(I.NE.N1)OFF(I)=S
            A11=CO*A22+SI*A23
            A12=CO*A23+SI*A33
            A13=SI*A34
            A21=CO*A23-SI*A22
            A22=CO*A33-SI*A23
            A23=CO*A34
            VALUE(I)=A11*CO+A12*SI
            A12=-A11*SI+A12*CO
            OFF(I+1)=A12
   70    A22=A22*CO-A21*SI
   80 VALUE(N2)=A22
      WRITE(6,90)
   90 FORMAT(39H1LOOPING DETECTED IN EA09-STOPPING NOW )
      STOP
      END
      COMPLEX FUNCTION FM06AS(N,A,IA,B,IB)
      IMPLICIT COMPLEX (A-H,O-Z)
      COMPLEX A(*), B(*)
*******************************************************
*
*    FM06AS - A FUNCTION ROUTINE TO COMPUTE THE VALUE OF THE
*      INNER PRODUCT, OR DOT PRODUCT, OF TWO SINGLE PRECISION
*      COMPLEX VECTORS, ACCUMULATING THE INTERMEDIATE PRODUCTS
*      DOUBLE PRECISION.  THE ELEMENTS OF EACH VECTOR CAN BE
*      STORED IN ANY FIXED DISPLACEMENT FROM NEIGHBOURING
*      ELEMENTS.
*
*    COMPUTES: SUM(J=1,N) A((J-1)*IA+1)*B((J-1)*IB+1)
*
*          W = FM06AS(N,A,IA,B,IB)
*
*    N   INTEGER SCALAR; (USER:*); LENGTH OF THE VECTORS A AND B.
*        IF N <= 0 THE INNER PRODUCT VALUE IS DEFINED TO BE ZERO.
*    A   COMPLEX*8 ARRAY((N-1)*IABS(IA)+1); (USER:*); THE ARRAY
*        CONTAINING THE 1ST VECTOR.  THE FORTRAN CONVENTION OF STORING
*        REAL AND IMAGINARY PARTS IN ADJACENT WORDS IS ASSUMED.
*    IA  INTEGER SCALAR; (USER:*); THE SUBSCRIPT DISPLACEMENT OF
*        AN ELEMENT IN THE ARRAY A TO ITS NEIGHBOUR, I.E. THE VECTOR
*        ELEMENTS ARE IN A(1), A(IA+1), A(2*IA+1),...
*        IF IA < 0 THE ELEMENTS ARE ASSUMED TO BE STORED IN
*        A(1-(N-1)*IA), A(1-(N-2)*IA),..., A(1-IA), A(1).
*    B   COMPLEX*8 ARRAY((N-1)*IABS(IA)+1); (USER:*); THE ARRAY
*        CONTAINING THE SECOND VECTOR.  TREAT LIKE A.
*    IB  INTEGER SCALAR; (USER:*); THE SUBSCRIPT DISPLACEMENT OF
*        AN ELEMENT IN B TO ITS NEIGHBOUR. TREAT LIKE IA.
*    FM06AS  COMPLEX FUNCTION; (*:FM06AS); THE INNER PRODUCT VALUE.
*        IT IS RETURNED DOUBLE PRECISION, THE REAL PART IN FLT PNT
*        REG 0 AND THE IMAGINARY PART IN FLT PNT REG 2.
*
*    THIS ROUTINE IS WRITTEN IN FORTRAN.
*
*--------------------------------------------------------*
      SUM=(0.0,0.0)
      DO 10 I=1,N
   10 SUM=SUM+A((I-1)*IA+1)*B((I-1)*IB+1)
      FM06AS=SUM
      RETURN
      END
      COMPLEX FUNCTION FM06BS(N,A,IA,B,IB)
      IMPLICIT COMPLEX (A-H,O-Z)
      COMPLEX A(*), B(*)
*******************************************************
*
*    FM06BS - A FUNCTION ROUTINE TO COMPUTE THE VALUE OF THE
*      INNER PRODUCT, OR DOT PRODUCT, OF A SIGLE    PRECISION
*      COMPLEX VECTORS, ACCUMULATING THE INTERMEDIATE PRODUCTS
*      DOUBLE PRECISION.  THE ELEMENTS OF EACH VECTOR CAN BE
*      STORED IN ANY FIXED DISPLACEMENT FROM NEIGHBOURING
*      ELEMENTS.
*
*    COMPUTES: SUM(J=1,N) A((J-1)*IA+1)*B((J-1)*IB+1)
*
*          W = FM06BS(N,A,IA,B,IB)
*
*    N   INTEGER SCALAR; (USER:*); LENGTH OF THE VECTORS A AND B.
*        IF N <= 0 THE INNER PRODUCT VALUE IS DEFINED TO BE ZERO.
*    A   COMPLEX*8 ARRAY((N-1)*IABS(IA)+1); (USER:*); THE ARRAY
*        CONTAINING THE 1ST VECTOR.  THE FORTRAN CONVENTION OF STORING
*        REAL AND IMAGINARY PARTS IN ADJACENT WORDS IS ASSUMED.
*    IA  INTEGER SCALAR; (USER:*); THE SUBSCRIPT DISPLACEMENT OF
*        AN ELEMENT IN THE ARRAY A TO ITS NEIGHBOUR, I.E. THE VECTOR
*        ELEMENTS ARE IN A(1), A(IA+1), A(2*IA+1),...
*        IF IA < 0 THE ELEMENTS ARE ASSUMED TO BE STORED IN
*        A(1-(N-1)*IA), A(1-(N-2)*IA),..., A(1-IA), A(1).
*    B   COMPLEX*8 ARRAY((N-1)*IABS(IA)+1); (USER:*); THE ARRAY
*        CONTAINING THE SECOND VECTOR.  TREAT LIKE A.
*    IB  INTEGER SCALAR; (USER:*); THE SUBSCRIPT DISPLACEMENT OF
*        AN ELEMENT IN B TO ITS NEIGHBOUR. TREAT LIKE IA.
*    FM06BS  COMPLEX FUNCTION; (*:FM06BS); THE INNER PRODUCT VALUE.
*        IT IS RETURNED DOUBLE PRECISION, THE REAL PART IN FLT PNT
*        REG 0 AND THE IMAGINARY PART IN FLT PNT REG 2.
*
*    THIS ROUTINE IS WRITTEN IN FORTRAN.
*
*--------------------------------------------------------*
      SUM=(0.0,0.0)
      DO 10 I=1,N
   10 SUM=SUM+A((I-1)*IA+1)*CONJG(B((I-1)*IB+1))
      FM06BS=SUM
      RETURN
      END
      SUBROUTINE ME08A(A,ALPHA,BETA,N,IA,Q)
      COMPLEX  A(IA,*),ALPHA(*),BETA(*),Q(*),CW,QJ
      COMPLEX  FM06AS,FM06BS
      REAL PP,ZERO,P5,H
      REAL S1,PP1
      real, dimension(:), allocatable :: rq
      real, dimension(:,:), allocatable :: ra
      integer ndim1,ndim2
      DATA ZERO/0.0/, P5 /0.50/
C**************************************************************
C THE REDUCTION OF FULL HERMITIAN MATRIX INTO TRI-DIAGONAL HERMITIAN
C FORM IS DONE IN N-2 STEPS.AT THE I TH STEP ZEROS ARE INTRODUCED IN
C THE I TH ROW AND COLUMNS WITHOUT DESTROYING PREVIOUSLY PRODUCED ZEROS
C
C                                                    H
C AT THE I TH STEP WE HAVE  A =P A   P  WITH P =I-U U  /K
C                            I  I I-1 I       I    I I   I
C
C WHERE U =(0,0,...,A     (1+S /T ),A     ,...,A   )
C        I           I,I+1    I  I   I,I+2      I,N
C        2   N          2        2                2
C       S = SUM  ] A   ]     K =S +T   AND  T = SQRT(]A     ] S )
C        I   J=I+1  I,J       I  I  I        I         I,I+1   I
C
C COMPUTATIONAL DETAILS AT THE I TH STAGE ARE (1) FORM S  ,K   THEN
C                                                       I   I
C
C                                     H                      H     H
C (2) Q =A   U  /K    (3) Q =Q -.5U (U Q /K ) (4) A =A   -U Q  -Q U
C      I  I-1 I   I        I  I    I  I I  I       I  I-1  I I   I I
C
C  THE VECTORS U BEING APPROPIATELY IN A.
      IF(N.LE.0)GO TO 90
      DO 10 J=1,N
         ALPHA(J)=A(J,J)
         DO 10 I=1,J
   10 A(I,J)=CONJG(A(J,I))
      IF(N-2)90,80,20
   20 N2=N-2
      ndim1=1
      ndim2=2
      if(allocated(rq)) deallocate(rq,ra)
      allocate(rq(size(q,ndim1)),ra(size(a,ndim1),size(a,ndim2)))
      DO 60 I=1,N2
         I1=I+1
C                       (1)
         CW=FM06BS(N-I,A(I,I+1),IA,A(I,I+1),IA)
         PP=CW
         PP1=SQRT(PP)
         BETA(I+1)=CMPLX(-PP1,ZERO)
         S1=CABS(A(I,I+1))
         IF(S1.GT.ZERO)BETA(I+1)=BETA(I+1)*A(I,I+1)/S1
         IF(PP.LE.1.D-15)GO TO 60
         H=PP+PP1*S1
         A(I,I+1)=A(I,I+1)-BETA(I+1)
C                       (2)
         DO 30 K=I1,N
            QJ=FM06AS(-(I-K),A(I+1,K),1,A(I,I+1),IA)
            QJ=FM06BS(N-K,A(K,K+1),IA,A(I,K+1),IA)+CONJG(QJ)
   30    Q(K)=QJ/H
C                       (3)
         CW=FM06AS(N-I,A(I,I+1),IA,Q(I+1),1)
         PP=CW*P5/H
         DO 40 K=I1,N
            Q(K)=Q(K)-PP*CONJG(A(I,K))
            rq(k)=real(q(k))
   40    continue
         do i1=1,N
            do i2=1,N
               ra(i1,i2)=real(a(i1,i2))
            end do
         end do
C                       (4)
         DO 50 K=I1,N
   50    CALL ME08B (rA(K,K),rQ(K),rA(I,K),N-K+1,IA*2)
   60 CONTINUE
      DO 70 I=2,N
         QJ=ALPHA(I)
         ALPHA(I)=A(I,I)
   70 A(I,I)=QJ
   80 BETA(N)=A(N-1,N)
   90 RETURN
      END
      SUBROUTINE SORT(VAL,VEC,N)
      COMPLEX VEC(N,*), SUM
      REAL    VAL(*)
      DO 30 I=1,N
         X=1.E9
         DO 10 J=I,N
            IF(VAL(J).LT.X) THEN
               K=J
               X=VAL(J)
            ENDIF
   10    CONTINUE
         DO 20 J=1,N
            SUM=VEC(J,K)
            VEC(J,K)=VEC(J,I)
   20    VEC(J,I)=SUM
         VAL(K)=VAL(I)
         VAL(I)=X
   30 CONTINUE
      RETURN
      END
