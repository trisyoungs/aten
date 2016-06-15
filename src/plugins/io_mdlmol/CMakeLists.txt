# Meta-Objects
set(mdlmol_MOC_HDRS
  mdlmol.hui
)
QT5_WRAP_CPP(mdlmol_MOC_SRCS ${mdlmol_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(mdlmol MODULE
  mdlmol_funcs.cpp
  ${mdlmol_MOC_SRCS}
)
set_target_properties(mdlmol PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS mdlmol
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(mdlmol PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

