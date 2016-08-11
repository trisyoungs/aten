module molkst_C
  USE vast_kind_param, ONLY:  double 
  integer :: &
  &  maxatoms, & ! Maximum number of atoms allowed = number of lines in the data set
  &  natoms,   & !  Number of atoms, real plus dummy, etc., in the system
  &  numat = 0,& !  Number of real atoms in the system
  &  norbs,    & !  Number of atomic orbitals in the system
  &  nelecs,   & !  Number of electrons
  &  ndep = 0, & !  Number of dependent coordinates, related by symmetry
  &  nvar = 0, & !  Number of coordinate that are to be optimized
  &  nclose,   & !  Number of doubly-occupied M.O.s
  &  nalpha,   & !  Number of alpha electrons
  &  nbeta,    & !  Number of beta electrons
  &  nopen,    & !  Number of M.O.s in the active space.
  &  numcal,   & !  Number of the calculation
  &  mpack,    & !  Number of elements in a lower-half-triangle = (norbs*(norbs+1))/2
  &  n2elec,   & !  Number of two-electron integrals
  &  nscf,     & !  Number of SCF calculations done
  &  iscf,     & !  Index of message of how the SCF ended
  &  iflepo,   & !  Index of message of how the geometry operation ended
  &  last,     & !  
  &  na1,      & !  99 if coordinates must be Cartesian, 0 otherwise
  &  lm61,     & !
  &  lm6,      & !

  &  dummy
  real(double) ::  &
  &  escf,         & !  Heat of formation (kcal/mol)
  &  emin,         & !  Lowest heat of formation calculated.
  &  elect,        & !  Electronic energy (eV)
  &  enuclr,       & !  Nuclear energy (eV)
  &  fract,        & !  Fractional occupancy of M.O.s in active space
  &  gnorm,        & !  Scalar of gradient vector
  &  time0 = 0.d0, & !  The start of time, or when the job began (seconds)
  &  tleft,        & !  Number of seconds the job has left before running out of time
  &  tdump = 0.d0, & !  Time between checkpoint dumps (seconds)
  &  verson=7.1d0, & !  Version number for this copy of MOPAC
  &  cosine,       & !  Angle between previous and current gradient vectors
  &  ux, uy, uz,   & !  Dipole components (?)
  &  step,         & !  "step" in SADDLE calculation
  &  rjkab1,       & !  "J" and "K" integrals for correction of doublet I.P.
  &  atheat,       & !
  &  efield(3),    & !  Electric field 
  &  rdummy

  logical ::       &
  &  moperr,       & !
  &  uhf,          & !  True if the calculation is Unrestricted Hartree-Fock
  &  isok,         & !
  &  limscf,       & !  Convergence criterion for SCF: if TRUE, then exit the SCF
                     !  if the energy changes a lot (useful in geometry optimization)
                     !  if FALSE, then converge the SCF to the default criterion
  & is_PARAM=.false. !  This will be set "TRUE" in a PARAM run
  character :: ltxt
  character :: jobnam*80 
  character (len=241) :: keywrd, koment, title
  character :: errtxt*200 
  logical :: &
  & method_mndo,     &
  & method_am1,      &
  & method_pm3,      &
  & method_mndod,    &
  & method_dorbs,    &
  & method             !  Default method = mndo

end module molkst_C
