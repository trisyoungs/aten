      subroutine mxmt(a, nar, b, nbr, c, ncc) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:27:30  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dgemm_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer  :: nar 
      integer  :: nbr 
      integer  :: ncc 
      real(double)  :: a(nar,nbr) 
      real(double)  :: b(ncc,nbr) 
      real(double)  :: c(nar,ncc) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!     MATRIX PRODUCT C(NAR,NCC) = A(NAR,NBR) * (B(NCC,NBR))'
!     ALL MATRICES RECTANGULAR , PACKED.
      call dgemm ('N', 'T', nar, ncc, nbr, 1.0D0, a, nar, b, ncc, 0.0D0, c, nar&
        ) 
      return  
      end subroutine mxmt 
