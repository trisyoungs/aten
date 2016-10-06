      subroutine ciosci(vects, oscil, conf) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      use molkst_C, only: numat,  norbs, keywrd
      use permanent_arrays, only : coord, nfirst, nlast
      use meci_C, only : nstate, lab, nmos, microa, microb
      use chanel_C, only: iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:04  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use matout_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   C o m m o n   B l o c k s
!-----------------------------------------------

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double) , intent(in) :: vects(norbs,norbs) 
      real(double) , intent(inout) :: oscil(3,*) 
      real(double), dimension(:,:), allocatable :: t4
      real(double) , intent(in) :: conf(lab*lab) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(8) :: ll 
      integer :: loop, iloop, iatom, i, j, k, l, m, ii, ij, jj, istate 
      real(double), dimension(norbs,nmos) :: vect1, vect2 
      real(double), dimension(nmos,nmos) :: t2 
      real(double), dimension(lab) :: work 
      real(double) :: sum 
      logical :: debug 
!-----------------------------------------------
!***********************************************************************
!
!    CIOSCI evaluates the expectation value <STATE|x or y or z|STATE>
!
!   Info:   VECTS  = Molecular Orbitals
!           CONF   = State Eigenvectors
!           NSTATE = State to be excited from
!
!
!***********************************************************************
      allocate(t4(lab, lab))
      debug = index(keywrd,'CIOSCI') /= 0 
      do loop = 1, 3 
!
!    Set up the M.O. unitary matrix, <psi | IOPER | psi>
!
        do iloop = 1, nmos 
!            SUM=0.D0
          do iatom = 1, numat 
            vect1(nfirst(iatom):nlast(iatom),iloop) = vects(nfirst(iatom):nlast&
              (iatom),iloop) 
            vect2(nfirst(iatom):nlast(iatom),iloop) = vects(nfirst(iatom):nlast&
              (iatom),iloop)*coord(loop,iatom) 
          end do 
        end do 
!
!    VECT1 holds the molecular orbitals in the coordinate system of
!          SYMTRZ.
!    VECT2 holds the same orbitals, after being operated on by the
!          dipole operators 'x', 'y', and 'z'.
!
!     T2 hold the unitary transform for the set of M.O.s
!        <Vect1|IOPER|VECT2>
!
        do i = 1, nmos 
          t2(i,i) = 0.D0 
          do j = 1, nmos 
            if (i == j) cycle  
            sum = 0.D0 
            do k = 1, norbs 
              sum = sum + vect1(k,i)*vect2(k,j) 
            end do 
            t2(i,j) = sum 
          end do 
        end do 
        if (debug) then 
          write (iw, '(A)') ' Effect of Dipole Operator on M.O.s' 
          do i = 1, nmos 
            write (iw, '(8F12.6)') (t2(j,i),j=1,nmos) 
          end do 
        endif 
!***********************************************************************
!
!   The big loop to fill T4
!
        do i = 1, lab 
          do j = 1, lab 
            l = 0 
            m = 0 
            do k = 1, nmos 
              if (microa(k,i) /= microa(k,j)) then 
                l = l + 1 
                ll(l) = k 
              endif 
              if (microb(k,i) == microb(k,j)) cycle  
              m = m + 1 
              ll(m) = k 
            end do 
            if (l==2 .and. m==0 .or. m==2 .and. l==0) then 
              t4(i,j) = t2(ll(1),ll(2)) 
              if (l == 2) then 
                do ii = 1, nmos 
                  if (microa(ii,i) == microa(ii,j)) cycle  
                  exit  
                end do 
                ij = microb(ii,i) 
                do jj = ii + 1, nmos 
                  if (microa(jj,i) /= microa(jj,j)) go to 160 
                  ij = ij + microa(jj,i) + microb(jj,i) 
                end do 
              else 
                do ii = 1, nmos 
                  if (microb(ii,i) == microb(ii,j)) cycle  
                  exit  
                end do 
                ij = 0 
                do jj = ii + 1, nmos 
                  if (microb(jj,i) /= microb(jj,j)) exit  
                  ij = ij + microa(jj,i) + microb(jj,i) 
                end do 
                ij = ij + microa(jj,i) 
              endif 
  160         continue 
              if (mod(ij,2) == 1) t4(i,j) = -t4(i,j) 
            else 
              t4(i,j) = 0.D0 
            endif 
          end do 
        end do 
        if (debug) then 
          write (iw, &
      '(                                                 '' Effect of dipole op&
      &erator on Microstates'')') 
          call matout (t4, t4, lab, lab, lab) 
        endif 
!
!    Now to perform <State(NSTATE)| x or y or z  | State(ISTATE)>
!
        do istate = 1, lab 
          do j = 1, lab 
            sum = 0.D0 
            do k = 1, lab 
              sum = sum + conf(k+(nstate-1)*lab)*t4(j,k) 
            end do 
            work(j) = sum 
          end do 
          sum = 0.D0 
          do k = 1, lab 
            sum = sum + work(k)*conf(k+(istate-1)*lab) 
          end do 
          oscil(loop,istate) = sum 
        end do 
      end do 
      if (debug) then 
        write (iw, '(A)') ' Effect of Dipole Operator on States' 
        write (iw, '(/,'' State    X           Y           Z'')') 
        do i = 1, lab 
          write (iw, '(I4,3F12.5)') i, (oscil(j,i),j=1,3) 
        end do 
      endif 
      return  
      end subroutine ciosci 
