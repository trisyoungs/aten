      subroutine deri23(f, fd, e, fci, cmo, emo) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : norbs, nopen, fract
      use meci_C, only : nelec, nmos, nbo
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:07  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use dcopy_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(in) :: f(*) 
      real(double) , intent(in) :: fd(*) 
      real(double) , intent(in) :: e(*) 
      real(double)  :: fci(*) 
      real(double) , intent(out) :: cmo(norbs,*) 
      real(double)  :: emo(*) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: l, nend, loop, ninit, n1, n2, i, j 
      real(double) :: const, diffe, com, scal 
!-----------------------------------------------
!**********************************************************************
!  1) UNPACK THE C.I-ACTIVE M.O. DERIVATIVES IN M.O. BASIS,
!     DIAGONAL BLOCKS INCLUDED.
!  2) EXTRACT THE FOCK EIGENVALUES RELAXATION OVER C.I-ACTIVE M.O.
!   INPUT
!     F           : UNSCALED SOLUTIONS VECTOR IN M.O. BASIS,
!                   OFF-DIAGONAL BLOCKS PACKED AS DEFINED IN 'DERI21'.
!     FD          : DIAGONAL BLOCKS OF NON-RELAXED FOCK MATRIX
!                   AS DEFINED IN 'DERI1'.
!     E(NORBS)    : FOCK EIGENVALUES.
!     FCI         : DIAGONAL BLOCKS OF RELAXATION OF THE FOCK MATRIX.
!     NORBS       : NUMBER OF M.O
!     NELEC,NMOS  : # OF LAST FROZEN CORE M.O , C.I-ACTIVE BAND LENGTH.
!   OUTPUT
!     CMO(N,NELEC+1,...,NELEC+NMOS): C.I-ACTIVE M.O DERIVATIVES
!                                  IN M.O BASIS.
!     EMO(  NELEC+1,...,NELEC+NMOS): C.I-ACTIVE FOCK EIGENVALUE RELAXATI
!
!**********************************************************************
!
      nopen = nbo(1) + nbo(2) 
      const = 1.D-3 
!
!     PART 1.
!     -------
!     COMPUTE AND UNPACK DIAGONAL BLOCKS, DIAGONAL TERMS INCLUDED,
!     ACCORDING TO CMO(I,J) = (FD(I,J)-FCI(I,J))/(E(I)-E(J))
!     AND TAKING   CMO(I,J)=0 IF E(I)=E(J) (THRESHOLD 1D-4 EV),
!                             I.E WHEN M.O. DEGENERACY OCCURS.
      l = 1 
      nend = 0 
      do loop = 1, 3 
        ninit = nend + 1 
        nend = nend + nbo(loop) 
        n1 = max(ninit,nelec + 1) 
        n2 = min(nend,nelec + nmos) 
        if (n2 < n1) cycle  
        do i = n1, n2 
          if (i > ninit) then 
            do j = ninit, i - 1 
              diffe = e(i) - e(j) 
              if (abs(diffe) > 1.D-4) then 
                com = (fd(l)-fci(l))/diffe 
              else 
                com = 0.D0 
              endif 
              cmo(i,j) = -com 
              cmo(j,i) = com 
              l = l + 1 
            end do 
          endif 
          cmo(i,i) = 0.D0 
        end do 
      end do 
!
!     C.I-ACTIVE EIGENVALUES RELAXATION.
      call dcopy (nmos, fci(l), 1, emo(nelec+1), 1) 
!
!     PART 2.
!     -------
!     UNPACK THE ANTISYMMETRIC MATRIX F IN CMO, (OFF-DIAGONAL BLOCKS).
!
      l = 1 
      if (nbo(2)>0 .and. nbo(1)>0) then 
!        OPEN-CLOSED
        scal = 1.D0/(2.D0 - fract + const) 
        do j = 1, nbo(1) 
          do i = nbo(1) + 1, nopen 
            com = f(l)*scal 
            cmo(i,j) = -com 
            cmo(j,i) = com 
            l = l + 1 
          end do 
        end do 
      endif 
      if (nbo(3)>0 .and. nbo(1)>0) then 
!       VIRTUAL-CLOSED
        scal = 0.5D0 
        do j = 1, nbo(1) 
          do i = nopen + 1, norbs 
            com = f(l)*scal 
            cmo(i,j) = -com 
            cmo(j,i) = com 
            l = l + 1 
          end do 
        end do 
      endif 
      if (nbo(3)/=0 .and. nbo(2)/=0) then 
!        VIRTUAL-OPEN
        scal = 1.D0/(fract + const) 
        do j = nbo(1) + 1, nopen 
          do i = nopen + 1, norbs 
            com = f(l)*scal 
            cmo(i,j) = -com 
            cmo(j,i) = com 
            l = l + 1 
          end do 
        end do 
      endif 
      return  
      end subroutine deri23 
