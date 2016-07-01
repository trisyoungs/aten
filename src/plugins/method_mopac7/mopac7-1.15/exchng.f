      SUBROUTINE EXCHNG (A,B,C,D,X,Y,T,Q,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*), Y(*)
C********************************************************************
C
C THE CONTENTS OF A, C, T, AND X ARE STORED IN B, D, Q, AND Y!
C
C   THIS IS A DEDICATED ROUTINE, IT IS CALLED BY LINMIN AND LOCMIN ONLY.
C
C********************************************************************
      B=A
      D=C
      Q=T
      DO 10 I=1,N
         Y(I)=X(I)
   10 CONTINUE
      RETURN
C
      END
