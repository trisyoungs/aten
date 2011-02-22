add_library(base
dnchar.cpp
mathfunc.cpp
sysfunc.cpp
dnchar.h
mathfunc.h
sysfunc.h
)

include_directories(
../
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src
)
