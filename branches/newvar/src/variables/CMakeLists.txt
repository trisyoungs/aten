add_library(variables
expression.h
expressionnode.h
variable.h
variablelist.h
expression.cpp
expressionnode.cpp
variable.cpp
variablelist.cpp
)

add_library(types
character.h
double.h
integer.h
character.cpp
double.cpp
integer.cpp
)

include_directories(
../
${QT_INCLUDE_DIR}
${CMAKE_BINARY_DIR}
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}/src
)

