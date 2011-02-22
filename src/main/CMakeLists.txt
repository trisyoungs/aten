add_library(cli
cli.h 
cli.cpp 
)

include_directories(
./
../
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src/)
