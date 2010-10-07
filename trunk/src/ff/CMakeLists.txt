add_library(ff
energystore.h
forcefield.h
forms.h
angle.cpp 
bond.cpp 
coulomb.cpp 
energystore.cpp
ewald.cpp 
expression.cpp
forcefield.cpp
forms.cpp
loadforcefield.cpp
rules.cpp 
torsion.cpp 
ureybradley.cpp
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

