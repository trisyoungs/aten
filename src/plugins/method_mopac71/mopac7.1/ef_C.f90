      module ef_C 
      USE vast_kind_param, ONLY:  double 
      integer :: mode, nstep, negreq, iprnt, iloop
      real(double) :: ddd, rmin, rmax, omin, xlamd, xlamd0, skal, x0, x1, x2
      real(double), dimension(:), allocatable :: oldf, d, vmode, pmat, uc, hessc
      real(double), dimension(:,:), allocatable :: hess, bmat, u, oldhss, oldu, alparm
      end module ef_C 
