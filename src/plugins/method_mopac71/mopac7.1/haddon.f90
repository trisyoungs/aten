      subroutine haddon(w, l, m, loc, a,  fact) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double
      use chanel_C, only : iw 
      use molkst_C, only : numcal, keywrd
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:20  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use mopend_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: l 
      integer , intent(in) :: m 
      integer , intent(in) :: loc 
      real(double) , intent(out) :: w 
      real(double) , intent(in) :: fact 
      real(double) , intent(in) :: a(3,*) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i 
      real(double) :: pi 
      logical :: xyz 

      save xyz, icalcn 
!-----------------------------------------------
      data icalcn/ 0/  
      if (numcal /= icalcn) then 
        icalcn = numcal 
        xyz = index(keywrd,' XYZ') /= 0 
      endif 
      pi = 3.1415926536D00 
      if (m>18 .or. m<1) then 
        write (iw, '(3/10X,''UNDEFINED SYMMETRY FUNCTION USED'')') 
        call mopend ('UNDEFINED SYMMETRY FUNCTION USED') 
      endif 
      i = loc 
      if (xyz) then 
        go to (210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360&
          ,370,380) m 
      else 
        go to (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180) &
          m 
      endif 
!
!  Set bond-lengths equal
!
   10 continue 
      l = 1 
      w = a(1,i) 
      return  
!
!  Set bond-angles equal
!
   20 continue 
      l = 2 
      w = a(2,i) 
      return  
!
!  Set dihedrals equal
!
   30 continue 
      w = a(3,i) 
      go to 190 
   40 continue 
      w = pi/2.0D00 - a(3,i) 
      go to 190 
   50 continue 
      w = pi/2.0D00 + a(3,i) 
      go to 190 
   60 continue 
      w = 2.0D00*pi/3.0D00 - a(3,i) 
      go to 190 
   70 continue 
      w = 2.0D00*pi/3.0D00 + a(3,i) 
      go to 190 
   80 continue 
      w = pi - a(3,i) 
      go to 190 
   90 continue 
      w = pi + a(3,i) 
      go to 190 
  100 continue 
      w = 4.0D00*pi/3.0D00 - a(3,i) 
      go to 190 
  110 continue 
      w = 4.0D00*pi/3.0D00 + a(3,i) 
      go to 190 
  120 continue 
      w = 3.0D00*pi/2.0D00 - a(3,i) 
      go to 190 
  130 continue 
      w = 3.0D00*pi/2.0D00 + a(3,i) 
      go to 190 
  140 continue 
      w = -a(3,i) 
      go to 190 
  150 continue 
      l = 1 
      w = a(1,i)/2.0D00 
      return  
  160 continue 
      l = 2 
      w = a(2,i)/2.0D00 
      return  
  170 continue 
      l = 2 
      w = pi - a(2,i) 
      return  
  180 continue 
      l = 1 
      w = a(1,i)*fact 
      return  
  190 continue 
      l = 3 
      return  
  210 continue 
      l = 1 
      w = a(1,i) 
      return  
!                          Y = Y
  220 continue 
      l = 2 
      w = a(2,i) 
      return  
!                          Z = Z
  230 continue 
      l = 3 
      w = a(3,i) 
      return  
!                          X = -X
  240 continue 
      l = 1 
      w = -a(1,i) 
      return  
!                          Y = -Y
  250 continue 
      l = 2 
      w = -a(2,i) 
      return  
!                          Z = -Z
  260 continue 
      l = 3 
      w = -a(3,i) 
      return  
!                          X = Y
  270 continue 
      l = 1 
      w = a(2,i) 
      return  
!                          Y = Z
  280 continue 
      l = 2 
      w = a(3,i) 
      return  
!                          Z = X
  290 continue 
      l = 3 
      w = a(1,i) 
      return  
!                          X = -Y
  300 continue 
      l = 1 
      w = -a(2,i) 
      return  
!                          Y = -Z
  310 continue 
      l = 2 
      w = -a(3,i) 
      return  
!                          Z = -X
  320 continue 
      l = 3 
      w = -a(1,i) 
      return  
!                          X = Z
  330 continue 
      l = 1 
      w = a(3,i) 
      return  
!                          Y = X
  340 continue 
      l = 2 
      w = a(1,i) 
      return  
!                          Z = Y
  350 continue 
      l = 3 
      w = a(2,i) 
      return  
!                          X = -Z
  360 continue 
      l = 1 
      w = -a(3,i) 
      return  
!                          Y = -X
  370 continue 
      l = 2 
      w = -a(1,i) 
      return  
!                          Z = -Y
  380 continue 
      l = 3 
      w = -a(2,i) 
      return  
      end subroutine haddon 
