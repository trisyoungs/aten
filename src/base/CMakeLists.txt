add_library(base
cli.h 
constants.h 
elements.h 
master.h 
messenger.h
prefs.h 
sysfunc.h
cli.cpp 
elements.cpp 
master.cpp 
messenger.cpp
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
