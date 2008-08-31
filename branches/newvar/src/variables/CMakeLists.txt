add_library(variables
character.h
expression.h
expressionnode.h
integer.h
pointer.h
real.h
variable.h
variablelist.h
character.cpp
expression.cpp
expressionnode.cpp
integer.cpp
pointer.cpp
real.cpp
variable.cpp
variablelist.cpp
)

include_directories(
../
${QT_INCLUDE_DIR}
${CMAKE_BINARY_DIR}
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}/src
)

