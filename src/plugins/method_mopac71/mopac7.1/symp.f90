      subroutine symp 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use symmetry_C, only : nsym, r, ipo, name
      USE molkst_C, ONLY: numat 
      USE chanel_C, only : iw
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  15:34:19  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: maxfun = 120 
      real(double), parameter :: tol = 1D-2 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, n, m 
      real(double) :: res 
!-----------------------------------------------
!****************************************************************
!
!   ON INPUT   R    = SYMMETRY OPERATIONS (7 MAX)
!              IPO  = PERM OPR FOR ABOVE OPERATIONS
!              NSYM = CURRENT NUMBER OF SYMMETRY OPERATIONS
!              NENT = NUMBER OF USER SUPPLIED OPERATIONS
!
!   ON OUTPUT  R    = SYMMETRY OPERATIONS (120 MAX)
!              IPO  = PERMUTATION OPERATOR FOR SYMMETRY OPERATIONS
!              NSYM = NUMBER OF SYMMETRY OPERATIONS
!
!****************************************************************
!
!  A SUBROUTINE THAT WILL EXPAND THE SYMMETRY OPERATIONS READ IN INTO
!     THE COMPLETE SET.  NOTE: VERY FEW OPERATIONS ARE REQUIRED TO
!     GENERATE EVEN VERY LARGE GROUPS OF OPERATIONS.
!
!
!
!
!  Variables used:  (n represents the number of atomic centers)
!
!    For the next two items, the last index represents the symmetry
!        operation number.
!     R(9,*):   The 9 elements of each record are a packed 3 by 3
!          array of a given symmetry operation.
!    IPO(n,*):  A vector that contains the symmetry mapping of atomic ce
!
!  NSYM IS ALWAYS THE UPPER BOUND OF THE VALID FUNCTIONS.  QUIT IF IT
!     REACHES 120.
!  I IS THE SLOW INDEX OF FUNCTIONS TO MULTIPLY
!  J IS THE FAST INDEX OF FUNCTIONS TO MULTIPLY
!  ALWAYS DO R(I)*R(J) AND TAKE I,J FROM 2 TO NSYM
!
      i = 2 
      j = 1 
!
!  DETERMINE IF IT IS TIME TO STOP
!
   10 continue 
      j = j + 1 
      if (j > nsym) then 
        j = 2 
        i = i + 1 
        if (i > nsym) go to 50 
      endif 
      if (nsym == maxfun) go to 50 
!
!  NOW TO START THE MULTIPLICATION
!
      r(1,nsym+1) = r(1,i)*r(1,j) + r(2,i)*r(4,j) + r(3,i)*r(7,j) 
      r(2,nsym+1) = r(1,i)*r(2,j) + r(2,i)*r(5,j) + r(3,i)*r(8,j) 
      r(3,nsym+1) = r(1,i)*r(3,j) + r(2,i)*r(6,j) + r(3,i)*r(9,j) 
      r(4,nsym+1) = r(4,i)*r(1,j) + r(5,i)*r(4,j) + r(6,i)*r(7,j) 
      r(5,nsym+1) = r(4,i)*r(2,j) + r(5,i)*r(5,j) + r(6,i)*r(8,j) 
      r(6,nsym+1) = r(4,i)*r(3,j) + r(5,i)*r(6,j) + r(6,i)*r(9,j) 
      r(7,nsym+1) = r(7,i)*r(1,j) + r(8,i)*r(4,j) + r(9,i)*r(7,j) 
      r(8,nsym+1) = r(7,i)*r(2,j) + r(8,i)*r(5,j) + r(9,i)*r(8,j) 
      r(9,nsym+1) = r(7,i)*r(3,j) + r(8,i)*r(6,j) + r(9,i)*r(9,j) 
!
!  IS IT UNIQUE?
!
      do n = 1, nsym 
        res = 0.D0 
        do m = 1, 9 
          res = res + abs(r(m,n)-r(m,nsym+1)) 
        end do 
        if (res < tol) go to 10 
      end do 
!
!  YES, IT IS UNIQUE.  NOW, GENERATE THE NEW IPO(,NSYM)
!
      nsym = nsym + 1 
      do n = 1, numat 
        ipo(n,nsym) = ipo(ipo(n,j),i) 
      end do 
!
!     ALL DONE ADDING THE NEW FUNCTION.  GO TRY TO FIND A NEW ONE.
!
      go to 10 
!
!
   50 continue 
      write (iw, 60) name, nsym 
   60 format(/,'    FOR POINT-GROUP ',a4,' THERE ARE ',i3,&
        ' UNIQUE SYMMETRY FUNCTIONS.',/) 
!#       WRITE(IW,*)' Symmetry Operations'
!#       DO 14 I=1,NSYM
!#       WRITE(IW,*)
!#       WRITE(IW,'(3f12.6)')(R(J,I),J=1,9)
!#  14   continue
!
      return  
      end subroutine symp 
