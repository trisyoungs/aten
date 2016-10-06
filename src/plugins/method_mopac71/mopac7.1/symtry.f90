      subroutine symtry () 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : ndep
      USE symmetry_C, ONLY: locpar, idepfn, locdep, depmul
      use permanent_arrays, only : geo
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:04  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use haddon_I  
      implicit none
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: n, i, locn, j 
      real(double) :: value 
!-----------------------------------------------
!**********************************************************************
!
!  SYMTRY COMPUTES THE BOND LENGTHS AND ANGLES THAT ARE FUNCTIONS OF
!         OTHER BOND LENGTHS AND ANGLES.
!
! ON INPUT GEO     = KNOWN INTERNAL COORDINATES
!          NDEP    = NUMBER OF DEPENDENCY FUNCTIONS.
!          IDEPFN  = ARRAY OF DEPENDENCY FUNCTIONS.
!          LOCDEP  = ARRAY OF LABELS OF DEPENDENT ATOMS.
!          LOCPAR  = ARRAY OF LABELS OF REFERENCE ATOMS.
!
!  ON OUTPUT THE ARRAY "GEO" IS FILLED
!***********************************************************************
!
!     NOW COMPUTE THE DEPENDENT PARAMETERS.
!
      n = 0 
      do i = 1, ndep 
        if (idepfn(i)==18 .and. depmul(n+1)>1.D-3) then 
          n = n + 1 
          call haddon (value, locn, idepfn(i), locpar(i), geo, depmul(n)) 
        else 
          call haddon (value, locn, idepfn(i), locpar(i), geo, depmul(1)) 
        endif 
        j = locdep(i) 
        geo(locn,j) = value 
      end do 
      return  
      end subroutine symtry
