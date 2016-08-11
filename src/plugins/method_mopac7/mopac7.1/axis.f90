      subroutine axis(coord, numat, a, b, c, sumw, mass, evec) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      use molkst_C, only : numcal, keywrd
      use permanent_arrays, only : atmass
      USE chanel_C, only : iw
      USE funcon_C, only : fpc_6, fpc_8, fpc_9, fpc_10
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:18:11  03/15/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rsp_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: numat 
      integer , intent(in) :: mass 
      real(double) , intent(out) :: a 
      real(double) , intent(out) :: b 
      real(double) , intent(out) :: c 
      real(double) , intent(out) :: sumw 
      real(double) , intent(inout) :: coord(3,numat) 
      real(double)  :: evec(3,3) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, j 
      real(double), dimension(6) :: t 
      real(double), dimension(numat) :: x, y, z 
      real(double), dimension(3) :: rot, xyzmom, eig 
      real(double) :: const1, const2, sumwx, sumwy, sumwz, sum 
      logical :: first 

      save t, rot, xyzmom, eig, first, icalcn 
!-----------------------------------------------
!***********************************************************************
!
!  AXIS CALCULATES THE THREE MOMENTS OF INERTIA AND THE MOLECULAR
!       WEIGHT.  THE MOMENTS OF INERTIA ARE RETURNED IN A, B, AND C.
!       THE MOLECULAR WEIGHT IN SUMW.
!       THE UNITS OF INERTIA ARE 10**(-40)GRAM-CM**2,
!       AND MOL.WEIGHT IN ATOMIC-MASS-UNITS. (AMU'S)
!***********************************************************************
      data t/ 6*0.D0/  
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
        first = .TRUE. 
      endif 
!***********************************************************************
!     CONST1 =  10**40/(N*A*A)
!               N = AVERGADRO'S NUMBER
!               A = CM IN AN ANGSTROM
!               10**40 IS TO ALLOW UNITS TO BE 10**(-40)GRAM-CM**2
!
!***********************************************************************
      const1 = 10.d0**24/fpc_10
!***********************************************************************
!
!     CONST2 = CONVERSION FACTOR FROM ANGSTROM-AMU TO CM**(-1)
!
!            = (PLANCK'S CONSTANT*N*10**16)/(8*PI*PI*C)
!            = H[ERG-SEC]*N*10**16/
!              (8*PI**2*C[CM/SEC])
!
!***********************************************************************
      const2 = fpc_6*fpc_10*1.D16/(8.D0*3.14159265358979D0**2*fpc_8) 
!    FIRST WE CENTRE THE MOLECULE ABOUT THE CENTRE OF GRAVITY,
!    THIS DEPENDS ON THE ISOTOPIC MASSES, AND THE CARTESIAN GEOMETRY.
!
      sumw = 1.D-20 
      sumwx = 0.D0 
      sumwy = 0.D0 
      sumwz = 0.D0 
!
      if (mass > 0) then 
        do i = 1, numat 
          sumw = sumw + atmass(i) 
          sumwx = sumwx + atmass(i)*coord(1,i) 
          sumwy = sumwy + atmass(i)*coord(2,i) 
          sumwz = sumwz + atmass(i)*coord(3,i) 
        end do 
      else 
        sumw = sumw + dble(numat) 
        do i = 1, numat 
          sumwx = sumwx + coord(1,i) 
          sumwy = sumwy + coord(2,i) 
          sumwz = sumwz + coord(3,i) 
        end do 
      endif 
!
      if (mass>0 .and. first) write (iw, '(/10X,''MOLECULAR WEIGHT ='',F8.2,/)'&
        ) min(99999.99D0,sumw) 
      sumwx = sumwx/sumw 
      sumwy = sumwy/sumw 
      sumwz = sumwz/sumw 
      x(:numat) = coord(1,:) - sumwx 
      y(:numat) = coord(2,:) - sumwy 
      z(:numat) = coord(3,:) - sumwz 
!***********************************************************************
!
!    MATRIX FOR MOMENTS OF INERTIA IS OF FORM
!
!           |   Y**2+Z**2                         |
!           |    -Y*X       Z**2+X**2             | -I =0
!           |    -Z*X        -Z*Y       X**2+Y**2 |
!
!***********************************************************************
      do i = 1, 6 
        t(i) = dble(i)*1.0D-10 
      end do 
!
      if (mass > 0) then 
        t(1) = t(1) + dot_product(atmass(:numat),y(:numat)**2+z(:numat)**2) 
        t(2) = t(2) - dot_product(atmass(:numat)*x(:numat),y(:numat)) 
        t(3) = t(3) + dot_product(atmass(:numat),z(:numat)**2+x(:numat)**2) 
        t(4) = t(4) - dot_product(atmass(:numat)*z(:numat),x(:numat)) 
        t(5) = t(5) - dot_product(atmass(:numat)*y(:numat),z(:numat)) 
        t(6) = t(6) + dot_product(atmass(:numat),x(:numat)**2+y(:numat)**2) 
      else 
        do i = 1, numat 
          t(1) = t(1) + (y(i)**2+z(i)**2) 
          t(2) = t(2) - x(i)*y(i) 
          t(3) = t(3) + (z(i)**2+x(i)**2) 
          t(4) = t(4) - z(i)*x(i) 
          t(5) = t(5) - y(i)*z(i) 
          t(6) = t(6) + (x(i)**2+y(i)**2) 
        end do 
      endif 
!
      call rsp (t, 3, 3, eig, evec) 
      if (mass>0 .and. first .and. index(keywrd,'RC=')==0) then 
        write (iw, &
      '(2/9X,'' PRINCIPAL MOMENTS OF INERTIA ''             ,''IN CM(-1)'',/)') 
        where (eig < 3.D-4)  
          eig = 0.D0 
          rot = 0.D0 
        elsewhere 
          rot = const2/eig 
        end where 
        xyzmom = eig*const1 
        write (iw, &
      '(10X,''A ='',F12.6,''   B ='',F12.6,                 ''   C ='',F12.6,/)&
      ') (rot(i),i=1,3) 
        if (index(keywrd,'RC=') == 0) write (iw, &
      '(2/10X,'' PRINCIPAL MOMENTS OF INERTIA IN '',           ''UNITS OF 10**(&
      &-40)*GRAM-CM**2'',/)') 
        write (iw, &
      '(10X,''A ='',F12.6,''   B ='',F12.6,                 ''   C ='',F12.6,/)&
      ') (xyzmom(i),i=1,3) 
        c = rot(1) 
        b = rot(2) 
        a = rot(3) 
      endif 
!
!     MAKE DIAGONAL TERMS OBLIGATE POSITIVE
!
      do i = 1, 3 
        if (evec(i,i) >= 0.D0) cycle  
        evec(:,i) = -evec(:,i) 
      end do 
!
!   NOW TO ORIENT THE MOLECULE SO THE CHIRALITY IS PRESERVED
!   CHIRALITY CAN ONLY BE LOST IF ONE OR MORE EVEC(I,I) ARE ZERO
!
      sum = evec(1,1)*(evec(2,2)*evec(3,3)-evec(3,2)*evec(2,3)) + evec(1,2)*(&
        evec(2,3)*evec(3,1)-evec(2,1)*evec(3,3)) + evec(1,3)*(evec(2,1)*evec(3,&
        2)-evec(2,2)*evec(3,1)) 
      if (sum < 0) then 
        sum = 1.D0 
        do j = 1, 3 
          if (evec(j,j) >= sum) cycle  
          sum = evec(j,j) 
          i = j 
        end do 
        evec(:,i) = -evec(:,i) 
      endif 
      if (index(keywrd,' NOREOR') == 0) then 
        coord(1,:) = x(:numat) 
        coord(2,:) = y(:numat) 
        coord(3,:) = z(:numat) 
      endif 
      if (mass > 0) first = .FALSE. 
      return  
      end subroutine axis 
