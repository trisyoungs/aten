      logical function symdec (n1, ielem) 
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:50:03  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n1 
      integer , intent(in) :: ielem(20) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: in1, i, ibin 
!-----------------------------------------------
!
!*********************************************************************
!
!   SYMDEC matches up a set of symmetry operations for the system
!          with a point-group.  The set of symmetry operations are
!          stored in IELEM.  Point-groups are represented by a
!          number, N1, which is expanded into binary.
!
!          A system has the symmetry of a specific point-group if
!          every operation of the point-group is present in the
!          system.  Extra operations may also be present, but are
!          ignored.  This allows a controlled descent in symmetry.
!          The symmetry groups in the calling routine, CARTAB, are
!          stored in order of symmetry - C1 to R3.
!  SYMDEC  returns a .TRUE. if the system matches point-group N1.
!
!*********************************************************************

      in1 = n1 
      do i = 1, 20 
        ibin = mod(in1,2) 
        if (ielem(i)/=1 .and. ibin==1) go to 20 
        in1 = in1/2 
      end do 
      symdec = .TRUE. 
      return  
   20 continue 
      symdec = .FALSE. 
      return  
      end function symdec 
