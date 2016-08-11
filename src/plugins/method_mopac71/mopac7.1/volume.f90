      real(kind(0.0d0)) function volume (vecs, ndim) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  17:49:42  03/20/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ndim 
      real(double) , intent(in) :: vecs(3,ndim) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(double) :: a, b, gamma, sing, c, alpha, sina, beta, sinb 
!-----------------------------------------------
!**********************************************************************
!
! VOLUME RETURNS (A) THE VOLUME OF A UNIT CELL IF NDIM=3
!                (B) THE AREA   OF A UNIT CELL IF NDIM=2
!                (C) THE LENGTH OF A UNIT CELL IF NDIM=1
!
!  ON INPUT VECS = ARRAY OF POINTS MARKING THE ENDS OF UNIT CELL VECTORS
!                  THE ORIGIN BEING (0.0, 0.0, 0.0)
!
!**********************************************************************
      a = sqrt(vecs(1,1)**2+vecs(2,1)**2+vecs(3,1)**2) 
!
!    CASE 1: SYSTEM IS A POLYMER
!
      if (ndim == 1) then 
        volume = a 
        return  
      endif 
      b = sqrt(vecs(1,2)**2+vecs(2,2)**2+vecs(3,2)**2) 
      gamma = sqrt((vecs(1,1)-vecs(1,2))**2+(vecs(2,1)-vecs(2,2))**2+(vecs(3,1)&
        -vecs(3,2))**2) 
!
!     SING = SIN OF ANGLE BETWEEN FIRST AND SECOND VECTORS
!            OBTAINED FROM COSINE RULE C**2=A**2+B**2-2*A*B*COS(C)
!
      sing = sqrt(1.D0 - ((a*a + b*b - gamma*gamma)/(2.D0*a*b))**2) 
!
!    CASE 2: SYSTEM IS A LAYER STRUCTURE
!
      if (ndim == 2) then 
!
!   AREA OF A PARALLELOGRAM = BASE * HEIGHT
!
        volume = a*b*sing 
        return  
      endif 
!
!    CASE 3: SYSTEM IS A SOLID
!
      c = sqrt(vecs(1,3)**2+vecs(2,3)**2+vecs(3,3)**2) 
      alpha = sqrt((vecs(1,2)-vecs(1,3))**2+(vecs(2,2)-vecs(2,3))**2+(vecs(3,2)&
        -vecs(3,3))**2) 
      sina = sqrt(1.D0 - ((b*b + c*c - alpha*alpha)/(2.D0*b*c))**2) 
      beta = sqrt((vecs(1,1)-vecs(1,3))**2+(vecs(2,1)-vecs(2,3))**2+(vecs(3,1)-&
        vecs(3,3))**2) 
      sinb = sqrt(1.D0 - ((a*a + c*c - beta*beta)/(2.D0*a*c))**2) 
!
!     SINA = SIN OF ANGLE BETWEEN SECOND AND THIRD VECTORS
!     SINB = SIN OF ANGLE BETWEEN FIRST AND THIRD VECTORS
!
!     VOLUME OF A PARALLELEPIPED =A*SIN(ALPHA)*B*SIN(BETA)*C*SIN(GAMMA)
!
      volume = a*b*c*sina*sinb*sing 
      return  
      end function volume 
