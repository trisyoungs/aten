add_library(main
aten.h
cli.h 
version.h 
aten.cpp
aten_filters.cpp
cli.cpp 
modes.cpp
probe.cpp
)

include_directories(
./
../
${QT_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}/src/gui 
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src/)
