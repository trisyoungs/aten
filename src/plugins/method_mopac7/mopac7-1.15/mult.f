      SUBROUTINE MULT(C,S,VECS,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C(N,*), S(N,*), VECS(N,*)
***********************************************************************
*
*   MULT IS USED IN THE MULLIKEN ANALYSIS ONLY. IT PERFORMS THE
*        OPERATION:-
*                                   VECS=BACK-TRANSFORMED EIGENVECTORS
*        VECS  =  C*S               C   =UN-BACK-TRANSFORMED VECTORS
*                                   S   =1/SQRT(OVERLAP MATRIX)
*
***********************************************************************
      DO 20 I=1,N
         DO 20 J=1,N
            SUM=0.D0
            DO 10 K=1,N
   10       SUM=SUM+C(K,I)*S(J,K)
   20 VECS(J,I)=SUM
      RETURN
      END
