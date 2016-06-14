# Meta-Objects
set(pdens_MOC_HDRS
  pdens.hui
)
QT5_WRAP_CPP(pdens_MOC_SRCS ${pdens_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(pdens MODULE
  pdens_funcs.cpp
  ${pdens_MOC_SRCS}
)
set_target_properties(pdens PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS pdens
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(pdens PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

