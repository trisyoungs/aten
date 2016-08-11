      subroutine mullik() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat, nelecs, nclose, nopen, fract,  &
      & keywrd, norbs, mpack, jobnam
      use permanent_arrays, only : nfirst, nlast, nat, geo, coord, &
      & c, h, cb, pb
      use euler_C, only : tvec, id
      use parameters_C, only : zs, zp, zd, betas, betap
      use chanel_C, only : igpt
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:30  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r cb a c e   B l o c k s
!-----------------------------------------------
      use rsp_I 
      use gmetry_I 
      use mult_I 
      use densit_I 
      use vecprt_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(:), allocatable :: ifact 
      integer :: i, if, il, im1, k, ii, j, jf, jl, ij 
      real(double), dimension(norbs) :: eigs
      real(double), dimension(:), allocatable :: store, vecs 
      real(double) :: bi, bj, sum, summ 
      logical :: graph 
!-----------------------------------------------
!*********************************************************************
!
!  MULLIK DOES A MULLIKEN POPULATION ANALYSIS
! ON INPUT     C      =  SQUARE ARRAY OF EIGENVECTORS.
!              H      =  PACKED ARRAY OF ONE-ELECTRON MATRIX
!              cb      =  WORKSTORE OF SIZE AT LEAST NORBS*NORBS
!              VECS   =  WORKSTORE OF SIZE AT LEAST NORBS*NORBS
!              STORE  =  WORKSTORE OF SIZE AT LEAST (NORBS*(NORBS+1))/2
!
!*********************************************************************
     allocate(ifact(norbs + 1),  store(mpack), vecs(norbs**2))
!*********************************************************************
!
!  FIRST, RE-CALCULATE THE OVERLAP MATRIX
!
!*********************************************************************
      graph = index(keywrd,'GRAPH') /= 0 

      do i = 1, norbs 
        ifact(i) = (i*(i - 1))/2 
      end do 
      ifact(norbs+1) = (norbs*(norbs + 1))/2 
      do i = 1, numat 
        if = nfirst(i) 
        il = nlast(i) 
        im1 = i - 1 
        bi = betas(nat(i)) 
        do k = if, il 
          ii = (k*(k - 1))/2 
          do j = 1, im1 
            jf = nfirst(j) 
            jl = nlast(j) 
            bj = betas(nat(j)) 
!  THE  +1.D-14 IS TO PREVENT POSSIBLE ERRORS IN THE DIAGONALIZATION.
            ij = ii + jf 
            h(ij) = 2.D0*h(ij)/(bi + bj) + 1.D-14 
            store(ij) = h(ij) 
            bj = betap(nat(j)) 
            bj = betap(nat(j)) 
            h(ii+jf+1:jl+ii) = 2.D0*h(ii+jf+1:jl+ii)/(bi + bj) + 1.D-14 
!  THE  +1.D-14 IS TO PREVENT POSSIBLE ERRORS IN THE DIAGONALIZATION.
            store(ii+jf+1:jl+ii) = h(ii+jf+1:jl+ii) 
          end do 
          store(ii+if:k+ii) = 0.D0 
          h(ii+if:k+ii) = 0.D0 
          bi = betap(nat(i)) 
        end do 
      end do 
      store(ifact(2:norbs+1)) = 1.D0 
      h(ifact(2:norbs+1)) = 1.D0 
      call rsp (h, norbs, norbs, eigs, vecs) 
      do i = 1, norbs 
        eigs(i) = 1.D0/sqrt(abs(eigs(i))) 
      end do 
      ij = 0 
      do i = 1, norbs 
        do j = 1, i 
          ij = ij + 1 
          sum = 0.D0 
          do k = 1, norbs 
            sum = sum + vecs(i+(k-1)*norbs)*eigs(k)*vecs(j+(k-1)*norbs) 
          end do 
          cb(i,j) = sum 
          cb(j,i) = sum 
        end do 
      end do 
        if (graph) then 
          call gmetry (geo, coord) 
!
! WRITE TO DISK THE FOLLOWING DATA FOR GRAPHICS CALCULATION, IN ORDER:
!
!      NUMBER OF ATOMS, ORBITAL, ELECTRONS
!      ALL ATOMIC COORDINATES
!      ORBITAL COUNTERS
!      ORBITAL EXPONENTS, S, P, AND D, AND ATOMIC NUMBERS
!      EIGENVECTORS (M.O.S NOT RE-NORMALIZED)
!      INVERSE-SQUARE ROOT OF THE OVERLAP MATRIX.
!
          i = index(jobnam,' ') - 1 
          open(unit=igpt, file=jobnam(:i)//'.gpt', form='UNFORMATTED', status=&
            'NEW', err=100, position='asis') 
          go to 110 
  100     continue 
          open(unit=igpt, file=jobnam(:i)//'.gpt', status='OLD', form=&
            'UNFORMATTED', position='asis') 
  110     continue 
          rewind igpt 
          write (igpt) numat, norbs, nelecs, ((coord(i,j),j=1,numat),i=1,3) 
          write (igpt) (nlast(i),nfirst(i),i=1,numat) 
          write (igpt) (zs(nat(i)),i=1,numat), (zp(nat(i)),i=1,numat), (zd(nat(&
            i)),i=1,numat), (nat(i),i=1,numat) 
          write (igpt) cb
          write (igpt) id, tvec 
          close(igpt, status='KEEP') 
          if (index(keywrd,'MULLIK') == 0) return  
        endif 
      call mult (c, cb, vecs, norbs) 
      i = -1 
      call densit (vecs, norbs, norbs, nclose, nopen, fract, pb, 2) 
      pb = pb*store
      summ = 0.D0 
      do i = 1, norbs 
        sum = 0 
        do j = 1, i 
          sum = sum + pb(ifact(i)+j) 
        end do 
        do j = i + 1, norbs 
          sum = sum + pb(ifact(j)+i) 
        end do 
        summ = summ + sum 
        pb(ifact(i+1)) = sum 
      end do 
      call vecprt (pb, norbs) 
      return  
      end subroutine mullik 
