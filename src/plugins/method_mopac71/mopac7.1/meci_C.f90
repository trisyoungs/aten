module meci_C
  USE vast_kind_param, ONLY:  double 
  integer, parameter :: mmci = 70
  integer ::       &
  &  nmos,         & !  Number of M.O.s in active space in C.I.
  &  lab,          & !  Number of microstates used in the C.I.
  &  labsiz,       & !  Number of states to be calculated (a subset of lab)
  &  nstate,       & !  Number of States used in defining the system
  &  nelec =-8,        & !  Number of electrons in the active space
  &  nmeci = 22,   & !  Maximum number of M.O.s in active space
  &  maxci = 1200, & !  Largest number of configurations allowed
  &  is,           &
  &  iiloop,       &
  &  jloop,        &
  &  k,            &
  &  dummy
  
  real(double) :: &
  &  cif1,   & !
  &  cif2,   & !
  &  cdiagi

  
  integer :: nbo(3)

  integer, dimension(:), allocatable :: &
  &  nalmat, & !
  &  ispin     ! Spins of each state (Singlet = 1) 

  integer, dimension(:,:), allocatable :: ispqr 

  real(double), dimension(:), allocatable :: &
  &  cdiag,  & !
  &  occa,   & !
  &  occb,   & !
  &  conf,   & !  State vectors
  &  eig,    & !  State eigenvalues
  &  vectci    !  State vector of interest

  real(double), dimension(:,:), allocatable :: &
  &  rjkaa, & !
  &  rjkab, & !
  &  cxy    !

  real(double), dimension(:,:,:), allocatable :: &
  &  dijkl
  real(double), dimension(:,:,:,:), allocatable :: &
  &  xy

  integer, dimension(:,:), allocatable :: &
  &  microa,& !
  &  microb
  

end module meci_C
