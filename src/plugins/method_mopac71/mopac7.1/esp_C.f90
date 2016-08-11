      module esp_C 
      USE vast_kind_param, ONLY:  double 
      integer  :: nesp, idip, iz, ipx, isc, is_esp, icd, ipe, npr, ic, ip, ncc
      real(double) :: dens, scale, cf, &
      & rms, rrms, dx, dy, dz, den
      real(double), dimension(0:8,821) :: fv 
      real(double), dimension(-1:96) :: dex 
      real(double), dimension(0:2) :: tf 

      integer, dimension(:), allocatable :: ind, itemp, ird, indc

      integer, dimension(:,:), allocatable :: iam

      real(double), dimension(:), allocatable :: cc, ex, temp, rad, es, &
      & b_esp, esp_array, cesp, cespml, al, qesp, td, dx_array, dy_array, dz_array, &
      & ptd, pexs, pce, pexpn, pewcx, pewcy, pewcz, pf0, pf1, pf2, pmtd, fc, &
      & qsc

      real(double), dimension(:,:), allocatable :: cespm, cen, potpc, co, &
      & potpt, ovl, cespm2, a2, exs, ce, expn, ewcx, ewcy, ewcz, f0, f1, u_esp, &
      & rnai, rnai1, rnai2, espi, exsr
      logical, dimension(:,:), allocatable :: cequiv
      data tf/33.d0, 37.d0, 41.d0/
      end module esp_C 
