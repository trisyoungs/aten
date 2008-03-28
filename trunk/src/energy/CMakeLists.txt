add_library(energy
forms.h
angle.cpp 
bond.cpp 
coulomb.cpp 
ewald.cpp 
forms.cpp 
rules.cpp 
torsion.cpp 
vdw.cpp
)

include_directories(
../
${QT_INCLUDE_DIR}
${CMAKE_BINARY_DIR}
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}/src
)

