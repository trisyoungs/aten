add_library(model
model.h
atom.cpp 
bond.cpp 
build.cpp 
cell.cpp 
energy.cpp 
expression.cpp 
glyph.cpp
measure.cpp 
model.cpp 
molecule.cpp 
pattern.cpp 
select.cpp 
selection.cpp
site.cpp 
trajectory.cpp 
transform.cpp 
typing.cpp 
undo.cpp 
view.cpp
)



include_directories(
../
${QT_INCLUDE_DIR}
${CMAKE_SOURCE_DIR}
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src
)
