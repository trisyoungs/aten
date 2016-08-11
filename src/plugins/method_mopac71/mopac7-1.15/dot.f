      DOUBLE PRECISION FUNCTION DOT(X,Y,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*), Y(*)
************************************************************************
*
*   DOT FORMS THE SCALAR PRODUCT OF TWO VECTORS.
*
*   ON INPUT     X   =    FIRST VECTOR, OF LENGTH N.
*                Y   =    SECOND VECTOR, OF LENGTH N.
*
*   ON RETURN    DOT =    DOT PRODUCT OF X AND Y.
*
************************************************************************
      DOT = 0.0D0
      DO 10 I=1,N
   10 DOT = DOT + X(I)*Y(I)
      RETURN
      END
