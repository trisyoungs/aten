    subroutine getsym (locpar, idepfn, locdep, depmul)
    use molkst_C, only: natoms, ndep, keywrd
    USE vast_kind_param, ONLY:  double 
    use chanel_C, only : iw, ir
   !
   !.. Implicit Declarations ..
    implicit none
   !
   !.. Formal Arguments ..
    integer, dimension (3*natoms), intent (inout) :: idepfn, locpar
    integer, dimension (3*natoms), intent (out) :: locdep
    double precision, dimension (natoms), intent (out) :: depmul
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:17  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------


!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(40) :: ivalue 
      integer :: mode, n, nvalue, i, ll, j 
      real(double), dimension(40) :: value 
      character , dimension(18) :: texti*60, textx*60 
      character :: line*80 
      character, dimension(18,2) :: text*60 

      save text 
!-----------------------------------------------
      
      equivalence (text(1,1), texti), (text(1,2), textx) 
      data texti/ &
        ' BOND LENGTH    IS SET EQUAL TO THE REFERENCE BOND LENGTH   ', &
        ' BOND ANGLE     IS SET EQUAL TO THE REFERENCE BOND ANGLE    ', &
        ' DIHEDRAL ANGLE IS SET EQUAL TO THE REFERENCE DIHEDRAL ANGLE', &
        ' DIHEDRAL ANGLE VARIES AS  90 DEGREES - REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS  90 DEGREES + REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 120 DEGREES - REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 120 DEGREES + REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 180 DEGREES - REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 180 DEGREES + REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 240 DEGREES - REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 240 DEGREES + REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 270 DEGREES - REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS 270 DEGREES + REFERENCE DIHEDRAL  ', &
        ' DIHEDRAL ANGLE VARIES AS - REFERENCE DIHEDRAL              ', &
        ' BOND LENGTH VARIES AS HALF THE REFERENCE BOND LENGTH       ', &
        ' BOND ANGLE VARIES AS HALF THE REFERENCE BOND ANGLE         ', &
        ' BOND ANGLE VARIES AS 180 DEGREES - REFERENCE BOND ANGLE    ', &
        ' BOND LENGTH IS A MULTIPLE OF THE REFERENCE BOND LENGTH     '/  
      data textx/ &
        ' X COORDINATE IS SET EQUAL TO   THE REFERENCE X COORDINATE  ', &
        ' Y COORDINATE IS SET EQUAL TO   THE REFERENCE Y COORDINATE  ', &
        ' Z COORDINATE IS SET EQUAL TO   THE REFERENCE Z COORDINATE  ', &
        ' X COORDINATE IS SET EQUAL TO - THE REFERENCE X COORDINATE  ', &
        ' Y COORDINATE IS SET EQUAL TO - THE REFERENCE Y COORDINATE  ', &
        ' Z COORDINATE IS SET EQUAL TO - THE REFERENCE Z COORDINATE  ', &
        ' X COORDINATE IS SET EQUAL TO   THE REFERENCE Y COORDINATE  ', &
        ' Y COORDINATE IS SET EQUAL TO   THE REFERENCE Z COORDINATE  ', &
        ' Z COORDINATE IS SET EQUAL TO   THE REFERENCE X COORDINATE  ', &
        ' X COORDINATE IS SET EQUAL TO - THE REFERENCE Y COORDINATE  ', &
        ' Y COORDINATE IS SET EQUAL TO - THE REFERENCE Z COORDINATE  ', &
        ' Z COORDINATE IS SET EQUAL TO - THE REFERENCE X COORDINATE  ', &
        ' X COORDINATE IS SET EQUAL TO   THE REFERENCE Z COORDINATE  ', &
        ' Y COORDINATE IS SET EQUAL TO   THE REFERENCE X COORDINATE  ', &
        ' Z COORDINATE IS SET EQUAL TO   THE REFERENCE Y COORDINATE  ', &
        ' X COORDINATE IS SET EQUAL TO - THE REFERENCE Z COORDINATE  ', &
        ' Y COORDINATE IS SET EQUAL TO - THE REFERENCE X COORDINATE  ', &
        ' Z COORDINATE IS SET EQUAL TO - THE REFERENCE Y COORDINATE  '/  
      if (index(keywrd,' XYZ') /= 0) then 
        mode = 2 
      else 
        mode = 1 
      endif 
!
! TITLE OUTPUT
!
      write (iw, 10) 
   10 format(/,/,/,5x,'PARAMETER DEPENDENCE DATA'/,/,&
        '        REFERENCE ATOM      FUNCTION NO.    DEPENDENT ATOM(S)') 
!
! INPUT SYMMETRY : FUNCTION, REFERANCE PARAMETER, AND DEPENDENT ATOMS
!
      n = 0 
      ndep = 0 
      depmul(1) = 0.D0 
   20 continue 
      read (ir, '(A)', end=90) line 
      call nuchar (line, value, nvalue) 
!   INTEGER VALUES
      do i = 1, nvalue 
        ivalue(i) = nint(value(i)) 
      end do 
!   FILL THE LOCDEP ARRAY
      if (nvalue==0 .or. ivalue(3)==0) go to 90 
      if (ivalue(2)==18 .and. mode==1) then 
        do i = 4, nvalue 
          if (ivalue(i) == 0) go to 60 
          ndep = ndep + 1 
          locdep(ndep) = ivalue(i) 
          locpar(ndep) = ivalue(1) 
          idepfn(ndep) = 18 
          n = n + 1 
          depmul(n) = value(3) 
        end do 
      else 
        do i = 3, nvalue 
          if (ivalue(i) == 0) exit  
          ndep = ndep + 1 
          locdep(ndep) = ivalue(i) 
          locpar(ndep) = ivalue(1) 
          idepfn(ndep) = ivalue(2) 
        end do 
      endif 
   60 continue 
      ll = i - 1 
      if (ivalue(2)==18 .and. mode==1) then 
        write (iw, 70) ivalue(1), ivalue(2), value(3), (&
          ivalue(j),j=4,ll) 
   70   format(i13,i13,f13.8,i7,11i3,10(/,43x,12i3)) 
      else 
         write (iw, 80) ivalue(1), ivalue(2), (ivalue(j),j=3&
          ,ll) 
   80   format(i13,i19,i14,11i3,10(/,43x,12i3)) 
      endif 
      go to 20 
!
! CLEAN UP
   90 continue 
      write (iw, 100) 
  100 format(/,10x,'   DESCRIPTIONS OF THE FUNCTIONS USED',/) 
      do j = 1, 18 
        do i = 1, ndep 
          if (idepfn(i) == j) go to 120 
        end do 
        cycle  
  120   continue 
         write (iw, 130) j, text(j,mode) 
  130   format(i4,5x,a) 
      end do 
      return  
      end subroutine getsym 
