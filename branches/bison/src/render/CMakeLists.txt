add_library(render
atoms.cpp 
colourscale.cpp
extra.cpp 
glyph.cpp 
model.cpp 
objects.cpp 
render.cpp 
surface.cpp
)

include_directories(
../
${QT_INCLUDE_DIR}
${CMAKE_SOURCE_DIR} 
${CMAKE_BINARY_DIR}
${CMAKE_BINARY_DIR}/src
)
