      subroutine mult(c, s, vecs, n) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:35:11  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n 
      real(double) , intent(in) :: c(n,*) 
      real(double) , intent(in) :: s(n,*) 
      real(double) , intent(out) :: vecs(n,*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k 
      real(double) :: sum 
!-----------------------------------------------
!**********************************************************************
!
!   MULT IS USED IN THE MULLIKEN ANALYSIS ONLY. IT PERFORMS THE
!        OPERATION:-
!                                   VECS=BACK-TRANSFORMED EIGENVECTORS
!        VECS  =  C*S               C   =UN-BACK-TRANSFORMED VECTORS
!                                   S   =1/SQRT(OVERLAP MATRIX)
!
!**********************************************************************
      do i = 1, n 
        do j = 1, n 
          sum = 0.D0 
          do k = 1, n 
            sum = sum + c(k,i)*s(j,k) 
          end do 
          vecs(j,i) = sum 
        end do 
      end do 
      return  
      end subroutine mult 
