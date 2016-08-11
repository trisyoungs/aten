      subroutine perm(iperm, nels, nmos, nperms, limci) 
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:33  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      use molkst_C, only : 
      use chanel_C, only : iw
      use meci_C, only : maxci
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nels 
      integer , intent(in) :: nmos 
      integer , intent(out) :: nperms 
      integer , intent(in) :: limci  
      integer , intent(inout) :: iperm(nmos,maxci*4) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(20) :: iadd, nel 
      integer :: nmax, i12, i11, i10, i9, i8, i7, i6, i5, i4, i3, i2, i1&
        , j, k 
!-----------------------------------------------
!***********************************************************************
!
!  PERM PERMUTES NELS ENTITIES AMONG NMOS LOCATIONS. THE ENTITIES AND
!       LOCATIONS ARE EACH INDISTINGUISHABLE. THE PAULI EXCLUSION
!       PRINCIPLE IS FOLLOWED. THE NUMBER OF STATES PRODUCED IS GIVEN
!       BY NMOS!/(NELS!*(NMOS-NELS)!).
! ON INPUT: NELS  = NUMBER OF INDISTINGUISHABLE ENTITIES
!           NMOS  = NUMBER OF INDISTINGUISHABLE LOCATIONS
!
! ON OUTPUT IPERM = ARRAY OF PERMUTATIONS, A 0 INDICATES NO ENTITY,
!                   A 1 INDICATES AN ENTITY.
!           NPERM = NUMBER OF PERMUTATIONS.
!
!***********************************************************************
      if (nels > nmos) then 
        write (iw, &
      '('' NUMBER OF PARTICLES,'',I3,'' GREATER THAN NO. '',''OF STATES,'',I3)'&
          ) nels, nmos 
        nperms = 0 
        return  
      endif 
      nmax = maxci*4 
      nperms = 1 
      nel = 1000 
      nel(:nels) = 1 
      l60: do i12 = nels - 11, nmos, nel(12) 
        iadd(12) = i12 
        do i11 = i12 + 1, nmos, nel(11) 
          iadd(11) = i11 
          do i10 = i11 + 1, nmos, nel(10) 
            iadd(10) = i10 
            do i9 = i10 + 1, nmos, nel(9) 
              iadd(9) = i9 
              do i8 = i9 + 1, nmos, nel(8) 
                iadd(8) = i8 
                do i7 = i8 + 1, nmos, nel(7) 
                  iadd(7) = i7 
                  do i6 = i7 + 1, nmos, nel(6) 
                    iadd(6) = i6 
                    do i5 = i6 + 1, nmos, nel(5) 
                      iadd(5) = i5 
                      do i4 = i5 + 1, nmos, nel(4) 
                        iadd(4) = i4 
                        do i3 = i4 + 1, nmos, nel(3) 
                          iadd(3) = i3 
                          do i2 = i3 + 1, nmos, nel(2) 
                            iadd(2) = i2 
                            do i1 = i2 + 1, nmos, nel(1) 
                              iadd(1) = i1 
                              iperm(:,nperms) = 0 
                              iperm(iadd(:nels),nperms) = 1 
!
!   ELIMINATE MICROSTATES WHICH ARE NOT ALLOWED BY LIMCI
!
                              if (limci/=0 .and. nperms>1) then 
                                k = 0 
                                do j = 1, nmos 
                                  k = k + abs(iperm(j,nperms)-iperm(j,1)) 
                                end do 
                                if (k > limci) nperms = nperms - 1 
                              endif 
                              nperms = nperms + 1 
                              if (nperms <= nmax) cycle  
                              write (iw, &
      '(                '' NUMBER OF PERMUTATIONS TOO GREAT, LIMIT'',I4)') nmax 
                              exit  l60 
                            end do 
                          end do 
                        end do 
                      end do 
                    end do 
                  end do 
                end do 
              end do 
            end do 
          end do 
        end do 
      end do l60 
      nperms = nperms - 1 
      return  
      end subroutine perm 
