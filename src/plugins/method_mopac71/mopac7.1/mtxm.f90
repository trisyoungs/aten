      subroutine mtxm(a, nar, b, nbr, c, ncc) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:12:28  03/09/06  
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
      real(double)  :: a(nbr,nar) 
      real(double)  :: b(nbr,ncc) 
      real(double)  :: c(nar,ncc) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!     MATRIX PRODUCT C(NAR,NCC) = (A(NBR,NAR))' * B(NBR,NCC)
!     ALL MATRICES RECTANGULAR , PACKED.
      call dgemm ('T', 'N', nar, ncc, nbr, 1.0D0, a, nbr, b, nbr, 0.0D0, c, nar&
        ) 
      return  
      end subroutine mtxm 
