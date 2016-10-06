      subroutine dijkl2(dc) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use meci_C, only : dijkl, xy, nmos
      use molkst_C, only : norbs
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:09  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dot_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: dc(norbs,nmos) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ij, i, j, kl, k, ll, l 
      real(double) :: val, val2 
      logical :: lij, lkl 
!-----------------------------------------------
!***********************************************************************
!     RELAXATION OF 2-ELECTRONS INTEGRALS IN M.O BASIS.
!
!   INPUT
!   DC(NORBS,NMOS) : C.I-ACTIVE M.O DERIVATIVES IN M.O BASIS, IN COLUMN.
!   NORBS          : TOTAL NUMBER OF M.O.
!   NMOS           : NUMBER OF C.I-ACTIVE M.O.
!   DIJKL(I,J,KL)  : <I(1),J(1)|K(2),L(2)> WITH
!                     I              OVER     ALL    M.O.
!                     J,KL CANONICAL OVER C.I-ACTIVE M.O.
!   OUTPUT
!     xy(I,J,K,L)= d< I(1),J(1) | K(2),L(2) >
!                   = <dI,J|K,L> + <I,dJ|K,L> + <I,J|dK,L> + <I,J|K,dL>
!                     WITH I,J,K,L OVER ALL C.I-ACTIVE M.O.
!     WRITTEN BY DANIEL LIOTARD
! (NOTE BY JJPS: AS THIS CODE IS HIGHLY EFFICIENT, NO CHANGES WERE MADE)
!***********************************************************************
!
      ij = 0 
      do i = 1, nmos 
        do j = 1, i 
          ij = ij + 1 
          lij = i == j 
          kl = 0 
          do k = 1, i 
            if (k == i) then 
              ll = j 
            else 
              ll = k 
            endif 
            do l = 1, ll 
              kl = kl + 1 
              lkl = k == l 
              val = dot(dc(1,i),dijkl(1,j,kl),norbs) 
              if (lij .and. lkl .and. j==k) then 
                val = val*4.D0 
              else 
                if (lij) then 
                  val = val*2.D0 
                else 
                  val = val + dot(dc(1,j),dijkl(1,i,kl),norbs) 
                endif 
                val2 = dot(dc(1,k),dijkl(1,l,ij),norbs) 
                if (lkl) then 
                  val = val + val2*2.D0 
                else 
                  val = val + val2 + dot(dc(1,l),dijkl(1,k,ij),norbs) 
                endif 
              endif 
              xy(i,j,k,l) = val 
              xy(i,j,l,k) = val 
              xy(j,i,k,l) = val 
              xy(j,i,l,k) = val 
              xy(k,l,i,j) = val 
              xy(k,l,j,i) = val 
              xy(l,k,i,j) = val 
              xy(l,k,j,i) = val 
            end do 
          end do 
        end do 
      end do 
      return  
      end subroutine dijkl2 
