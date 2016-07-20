# Meta-Objects
set(mol2_MOC_HDRS
  mol2.hui
)
QT5_WRAP_CPP(mol2_MOC_SRCS ${mol2_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(sybylmol2 MODULE
  mol2_funcs.cpp
  ${mol2_MOC_SRCS}
)
target_link_libraries(sybylmol2
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(sybylmol2 PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS sybylmol2
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(sybylmol2 PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

