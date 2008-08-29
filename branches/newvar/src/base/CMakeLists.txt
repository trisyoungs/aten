add_library(base
aten.h
cli.h 
constants.h 
elements.h 
messenger.h
prefs.h 
sysfunc.h
aten.cpp
atenaccess.cpp
cli.cpp 
elements.cpp 
messenger.cpp
modes.cpp
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
