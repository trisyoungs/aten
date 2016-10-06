module cosmo_C
  USE vast_kind_param, ONLY:  double
  implicit none
  logical :: iseps, noeps, useps 
  integer :: nspa, nps, nps2, nden, lenabc = 1000, lenab2, nppa = 1082
  integer, dimension(:), allocatable :: &
  & iatsp,   & !
  & nar_csm, & !
  & nsetf,i, & !  Watch out for the "i"
  & ipiden,  & !
  & idenat,  & !
  & nset,    & !
  & nar
  integer, dimension(:,:), allocatable :: &
  & isude,   & !
  & nn
  real(double) :: fepsi, rds, disex2, area1, cif1, cif2, cdiagi, area
  real(double), dimension(3,1082) :: dirvec, dirtm 
  double precision, dimension(3,3,1082) :: tm
  double precision, dimension(:), allocatable :: &
  & amat,    & !
  & cmat,    & !
  & gden,    & !
  & qscat,   & !
  & arat,    & !
  & srad,    & !
  & abcmat,  & !
  & qden,    & !
  & bh,      & ! 
  & cdiag
  double precision, dimension(:,:), allocatable :: &
  & bmat,    & !
  & phinet,  & !
  & qscnet,  & !
  & qdenet,  & !
  & cosurf,  & !
  & sude,    & !
  & xsp,     & !
  & cxy     
 
end module cosmo_C
