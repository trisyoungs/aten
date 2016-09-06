BISON_TARGET(TextPrimitiveParser textprimitive_grammar.yy ${CMAKE_CURRENT_BINARY_DIR}/textprimitive_grammar.cc)

add_library(render STATIC
  ${BISON_TextPrimitiveParser_OUTPUTS}
  fontinstance.h
  primitive.h
  primitiveinstance.h
  primitiveset.h
  rendergroup.h
  renderlist.h
  renderoccurrence.h
  renderoccurrencechunk.h
  textformat.h
  textfragment.h
  textprimitive.h
  textprimitivelist.h
  fontinstance.cpp
  primitive.cpp
  primitive_surface.cpp
  primitiveinstance.cpp
  primitiveset.cpp
  rendergroup.cpp
  rendergroup_glyph.cpp
  rendergroup_model.cpp
  rendergroup_overlays.cpp
  renderlist.cpp
  renderoccurrence.cpp
  renderoccurrencechunk.cpp
  textformat.cpp
  textfragment.cpp
  textprimitive.cpp
  textprimitivelist.cpp
)

target_include_directories(render PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${FREETYPE_INCLUDE_DIRS}
)
