add_library(ff
  combine.h
  energystore.h
  forcefield.h
  forms.h
  angle.cpp 
  bond.cpp 
  combine.cpp
  coulomb.cpp 
  energystore.cpp
  ewald.cpp 
  expression.cpp
  forcefield.cpp
  forms.cpp
  loadforcefield.cpp
  rules.cpp 
  saveforcefield.cpp
  torsion.cpp 
  vdw.cpp
)

add_library(fourier
fourier.h
fourier.cpp 
)

include_directories(
../
${QT_INCLUDE_DIR}
${CMAKE_BINARY_DIR}
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}/src
)

