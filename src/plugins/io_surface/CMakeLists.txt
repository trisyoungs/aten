# Meta-Objects
set(surface_MOC_HDRS
  surface.hui
)
QT5_WRAP_CPP(surface_MOC_SRCS ${surface_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(surface MODULE
  surface_funcs.cpp
  ${surface_MOC_SRCS}
)
set_target_properties(surface PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS surface
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(surface PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

