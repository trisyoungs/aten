      subroutine gover(ni, nj, xj, r, sg, a0) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use analyt_C, only : nztype
      use overlaps_C, only : ccc, zzz
!***********************************************************************
!                                                                      *
!   GOVER CALCULATES THE OVERLAP INTEGRALS USING A GAUSSIAN EXPANSION  *
!         STO-6G BY R.F. STEWART, J. CHEM. PHYS., 52 431-438, 1970     *
!                                                                      *
!         ON INPUT   NI   =  ATOMIC NUMBER OF FIRST ATOM               *
!                    NJ   =  ATOMIC NUMBER OF SECOND ATOM              *
!                    R    =  INTERATOMIC DISTANCE IN ANGSTROMS         *
!         ON EXIT    S    =  ARRAY OF OVERLAPS, IN ORDER S,PX,PY,  *
!                            PZ                                        *
!                                                                      *
!***********************************************************************
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:18  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ni 
      integer , intent(in) :: nj 
      real(double) , intent(inout) :: r 
      real(double) , intent(in) :: a0 
      real(double) , intent(in) :: xj(3) 
      real(double) , intent(inout) :: sg(9,9) 
!-----------------------------------------------
!   L o ccc a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o ccc a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ngauss, ifa, ila, ifb, ilb, ka, i, nat, kb, j, nbt, is, k, l 
      real(double), dimension(6,6) :: s 
      real(double) :: tomb, amb, apb, adb, abn 

      save ngauss 
!-----------------------------------------------
      data ngauss/ 6/  
!
!    FIND START AND END OF GAUSSIAN
!
      ifa = nztype(ni)*4 - 3 
      if (ccc(ifa+1,1) /= 0.D0) then 
        ila = ifa + 3 
      else 
        ila = ifa 
      endif 
      ifb = nztype(nj)*4 - 3 
      if (ccc(ifb+1,1) /= 0.D0) then 
        ilb = ifb + 3 
      else 
        ilb = ifb 
      endif 
!
!  CONVERT R INTO AU
!
      r = r/a0 
      r = r**2 
      ka = 0 
      do i = ifa, ila 
        ka = ka + 1 
        nat = ka - 1 
        kb = 0 
        do j = ifb, ilb 
          kb = kb + 1 
          nbt = kb - 1 
!
!         DECIDE IS IT AN S-S, S-P, P-S, OR P-P OVERLAP
!
          if (nat>0 .and. nbt>0) then 
!    P-P
            is = 4 
            tomb = xj(nat)*xj(nbt)/a0**2 
          else if (nat > 0) then 
!    P-S
            is = 3 
            tomb = -xj(nat)/a0 
          else if (nbt > 0) then 
!    S-P
            is = 2 
            tomb = -xj(nbt)/a0 
          else 
!    S-S
            is = 1 
          endif 
          if (nat == nbt) then 
            do k = 1, ngauss 
              do l = 1, ngauss 
                s(k,l) = 0.0D0 
                amb = zzz(i,k) + zzz(j,l) 
                apb = zzz(i,k)*zzz(j,l) 
                adb = apb/amb 
!
!           CHECK OF OVERLAP IS NON-ZERO BEFORE STARTING
!
                if (adb*r >= 90.D0) cycle  
                abn = 1.0D0 
                go to (50,10,20,30) is 
   10           continue 
                abn = 2.D0*tomb*zzz(i,k)*sqrt(zzz(j,l))/amb 
                go to 50 
   20           continue 
                abn = -2.D0*tomb*zzz(j,l)*sqrt(zzz(i,k))/amb 
                go to 50 
   30           continue 
                abn = -adb*tomb 
                abn = abn + 0.5D0 
                abn = 4.0D0*abn*sqrt(apb)/amb 
   50           continue 
                s(k,l) = sqrt((2.D0*sqrt(apb)/amb)**3)*exp((-adb*r))*abn 
              end do 
            end do 
          else 
            do k = 1, ngauss 
!
!           CHECK OF OVERLAP IS NON-ZERO BEFORE STARTING
!
              do l = 1, ngauss 
                s(k,l) = 0.0D0 
                amb = zzz(i,k) + zzz(j,l) 
                apb = zzz(i,k)*zzz(j,l) 
                adb = apb/amb 
!
!           CHECK OF OVERLAP IS NON-ZERO BEFORE STARTING
!
                if (adb*r >= 90.D0) cycle  
                abn = 1.0D0 
                go to (1013,1010,1011,1012) is 
 1010           continue 
                abn = 2.D0*tomb*zzz(i,k)*sqrt(zzz(j,l))/amb 
                go to 1013 
 1011           continue 
                abn = -2.D0*tomb*zzz(j,l)*sqrt(zzz(i,k))/amb 
                go to 1013 
 1012           continue 
                abn = -adb*tomb 
                abn = 4.0D0*abn*sqrt(apb)/amb 
 1013           continue 
                s(k,l) = sqrt((2.D0*sqrt(apb)/amb)**3)*exp((-adb*r))*abn 
              end do 
            end do 
          endif 
          sg(ka,kb) = 0.0D0 
          do k = 1, ngauss 
            sg(ka,kb) = sg(ka,kb) + sum(s(k,:ngauss)*ccc(i,k)*ccc(j,:ngauss)) 
          end do 
        end do 
      end do 
      return  
      end subroutine gover 
