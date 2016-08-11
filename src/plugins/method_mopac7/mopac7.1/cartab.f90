      subroutine cartab 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use symmetry_C, only : nclass, igroup, group, allrep, &
      & nallop,ntbs, ntab, nallg, name, nirred, ielem, jx, jy
      use molkst_C, only : numcal, keywrd
      use chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:02  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use symdec_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: ngps = 57 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(ngps) :: nope, nrep 
      integer :: icalcn, i, j, k, l, nopers, nreprs, nstabl, kl, ku, istart, &
        igp, nzz, nz, prnt
      real(double) :: buff, fz, fn 
      logical :: debug, first, large 
      character, dimension(20) :: class*9 

      save nrep, class, debug, first, icalcn 
!-----------------------------------------------
!***********************************************************************
!
! CARTAB constructs Character Tables for Point Groups.  Each Group is
!        defined by an array having the same name as the Group, e.g. C2v
!        The format of each group is explained in BLOCK TABLES
!
!
!   The Magic Number is the decimal representation of a 20-digit binary
!   number, each digit of which is '1' if the associated operation is
!   present in the system, '0' otherwise.  The 20 operations are, in
!   order
!
!  1 C2(X)         6 Sigma(YZ)    11 C6     16 S8
!  2 C2(Y)         7 inversion    12 C7     17 S10
!  3 C2(Z)         8 C3           13 C8     18 S12
!  4 Sigma(XY)     9 C4           14 S4     19 Cubic group
!  5 Sigma(XZ)    10 C5           15 S6     20 Infinite group.
!
!***********************************************************************
      data first/ .TRUE./  
      data prnt/ 1/  
      data class/ 'C2(x)', 'C2(y)', 'C2(z)', 'Sigma(XY)', 'Sigma(v)', &
        'Sigma(d)', 'Inversion', ' C3', ' C4', ' C5', ' C6', ' C7', ' C8', &
        ' S4', ' S6', ' S8', ' S10', ' S12', ' ?? ', 'C(Inf)'/  
      data icalcn/ 0/  
!
      if (numcal /= icalcn) then 
        icalcn = numcal 
        debug = index(keywrd,'CARTAB') /= 0 
        large = index(keywrd,'LARGE')/=0 .and. debug 
      endif 
      large = large .and. prnt==1 
      if (large) prnt = 0 
      if (first) then 
        first = .FALSE. 
!
!   Generate array to hold addresses of groups
!
!  NOPE will apply to operations
!  NREP will apply to group name and irreducible representations
!
        nope(1) = 1 
        nrep(1) = 1 
        do i = 2, ngps 
          nope(i) = nope(i-1) + nallop(nope(i-1)) + 4 
          nrep(i) = nrep(i-1) + nallop(nope(i-1)+1) + 1 
        end do 
!
!   Generate array to hold addresses of character tables
!
        j = 1 
        do i = 1, ntbs 
          k = ntab(i) 
          ntab(i) = j 
          j = j + k 
        end do 
!
!  Use the following lines to check that the common blocks
!  have the correct sizes.
!
!#      WRITE(*,*)' Size of Groups:',NOPE(NGPS)+NALLOP(NOPE(NGPS))+3
!#      WRITE(*,*)' Size of Reps  :',NREP(NGPS)+NALLOP(NOPE(NGPS)+1)
!#      WRITE(*,*)' Size of TABLES:',J-1
      endif 
!
!  Debug code:  Print all the point groups, in order
!
      if (large) then 
        do i = 1, ngps 
          k = nope(i) 
          l = nrep(i) 
          nopers = nallop(k) 
          nreprs = nallop(k+1) 
          nstabl = ntab(nallop(k+2)) 
          write (iw, *) '         Point Group:', allrep(l) 
          write (iw, *) ' No. Ops:', nopers + 1, ' No. Reps:', nreprs, &
            ' Table:', nallop(k+2), ' "Magic" No.:', nallop(k+3) 
          write (iw, '(/10X,8(A,1X))') '   Identity    ', (class(nallop(j)),j=k&
             + 4,k + 3 + nallop(k)) 
          write (iw, '(A,10I10)') '    '//allrep(l+1), (1,j=1,nallop(k) + 1) 
          do j = 2, nreprs 
            kl = nstabl + (j - 2)*(nopers + 1) 
            ku = kl + nopers 
            write (iw, '(A,10I10)') '    '//allrep(l+j), (nallg(k),k=kl,ku) 
          end do 
        end do 
      endif 
!
!    Identify the Point Group of the molecule.
!
      do igroup = ngps, 1, -1 
        if (.not.symdec(nallop(nope(igroup)+3),ielem)) cycle  
        exit  
      end do 
      if (index(keywrd,' NOSYM') /= 0) igroup = 1 
      istart = nope(igroup) 
      igp = nrep(igroup) 
      name = allrep(igp) 
      nclass = nallop(istart) + 1 
      nirred = nallop(istart+1) 
!
!  IGROUP:  The number of the point-group of the system.
!  ISTART:  Starting address of the group.
!  IGP   :  Starting address of the names used in the group.
!  NAME  :  The name of the group.
!  NCLASS:  Number of operations used to represent the group, plus 1.
!  NIRRED:  Number of Irreducible Representations.
!  GROUP :  The full point-group character table for group IGROUP.
!
      group(1,:nclass) = 1.D0 
      jy(2:nclass) = nallop(istart+4:nclass+2+istart) 
      jx(:nirred) = allrep(igp+1:nirred+igp) 
      istart = ntab(nallop(istart+2)) - 1 
      do i = 2, nirred 
        do k = 1, nclass 
          istart = istart + 1 
          buff = nallg(istart) 
          if (buff>=10.D0 .and. (name/='R3' .or. name=='R3' .and. k/=1 .and. k&
            /=nclass)) then 
            nzz = nallg(istart) 
            nz = nzz/10 
            fz = nz 
            fn = nzz - 10*nz 
            buff = 2.D0*cos(6.283185307179D0*fn/fz) 
          endif 
          group(i,k) = buff 
        end do 
      end do 
      jy(1) = 0 
      if (debug) then 
        write (iw, '(2A)') ' Character Table for Group  ', name 
        write (iw, '(7X,10A10)') '   E    ', (class(jy(k)),k=2,nclass) 
        do i = 1, nirred 
          write (iw, '(A4,10F10.4)') jx(i), (group(i,k),k=1,nclass) 
        end do 
      endif 
      return  
      end subroutine cartab 
