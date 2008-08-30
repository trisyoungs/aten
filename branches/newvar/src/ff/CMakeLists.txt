add_library(ff
forcefield.h
angle.cpp 
bond.cpp 
coulomb.cpp 
ewald.cpp 
forcefield.cpp
loadforcefield.cpp
rules.cpp 
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

