      MODULE dopen_I   
      INTERFACE
!...Generated by Pacific-Sierra Research 77to90  4.4G  10:47:01  03/09/06  
      SUBROUTINE dopen (C, MDIM, NORBS, NDUBL, NSINGL, FRACT, SDM) 
      USE vast_kind_param,ONLY: DOUBLE       
      INTEGER, INTENT(IN) :: MDIM 
      INTEGER, INTENT(IN) :: NORBS 
      INTEGER, INTENT(IN) :: NDUBL 
      INTEGER, INTENT(IN) :: NSINGL 
      REAL(DOUBLE), INTENT(IN) :: FRACT 
      REAL(DOUBLE), DIMENSION(MDIM,MDIM), INTENT(IN) :: C 
      REAL(DOUBLE), DIMENSION((MDIM*(MDIM + 1))/2), INTENT(OUT) :: SDM 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE 
