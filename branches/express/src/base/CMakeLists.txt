add_library(base
cli.h 
constants.h 
debug.h 
elements.h 
master.h 
prefs.h 
sysfunc.h
cli.cpp 
debug.cpp 
elements.cpp 
master.cpp 
prefs.cpp 
sysfunc.cpp
)

add_library(math
mathfunc.h
mathfunc.cpp
)

add_library(sg
generator.h
spacegroup.h
generator.cpp 
spacegroup.cpp
)

include_directories(
./
../
${QT_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}/src/gui 
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src/)
