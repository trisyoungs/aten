module permanent_arrays
  USE vast_kind_param, ONLY:  double 
  implicit none
  integer, dimension (:), allocatable :: &
  &  labels,     & !  Atomic numbers of all atoms, real, dummy, Tv
  &  nat,        & !  Atomic numbers of real atoms
  &  na, nb, nc, & !  Connectivities used in Z-matrix
  &  nfirst,     & !  Starting index of atomic orbitals for each atom
  &  nlast,      & !  Ending index of atomic orbitals for each atom
  &  nw,         & !
  &  dummy
  integer, dimension (:,:), allocatable :: &
  &  loc,        & !  Indices of atoms and coordinates marked for optimization
  &  dummy2
  real(double), dimension (:), allocatable :: &
  &  xparam,     & !  Values of geometric parameters marked for optimization
  &  xparef,     & !  Reference copy of xparam
  &  uspd,       & !  Initial values of one-electron diagonal (Uss, Upp, and Udd)
  &  pdiag,      & !  initial values of diagonal of density matrix
  &  h,          & !  One electron Hamiltonian matrix
  &  hb,         & !  Beta one electron Hamiltonian matrix
  &  f,          & !  Fock matrix
  &  fb,         & !  Beta Fock matrix
  &  w,          & !  Two-electron integrals
  &  wj,         & !  "J"-type two-electron integrals (used in solid state only)
  &  wk,         & !  "K"-type two-electron integrals (used in solid state only)
  &  p,          & !  Density matrix
  &  pa, pb,     & !  Alpha and beta density matrices
  &  q,          & !  Net charge on atoms
  &  eigs,       & !  M.O. eigenvalues (eV)
  &  eigb,       & !  Beta M.O. eigenvalues (eV)
  &  dxyz,       & !  Cartesian gradient vector in kcal/mol/Angstrom
  &  grad,       & !  Gradient vector (kcal/mol/A, or kcal/mol/radian)
  &  atmass,     & !  Atomic masses (amu)
  &  ch,         & !
  &  hesinv,     & !  inverse hessian
  &  gnext1,     & !
  &  gmin1,      & !
  &  errfn,      & !
  &  profil,     & !
  &  aicorr
  real(double), dimension (:,:), allocatable :: &
  &  geo,        & !  Coordinates of all atoms in units defined by input
  &  geoa,       & !  second set of coordinates, if needed
  &  coord,      & !  Atom Cartesian coordinates (Angstroms)
  &  c,          & !  Eigenvectors
  &  cb,         & !  Beta eigenvectors
  &  dxyz2,      & !  dxyz, but 2-D
  & rdummy2

  character, dimension (:), allocatable :: simbol*4, txtatm*8

  
end module permanent_arrays
