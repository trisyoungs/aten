# Meta-Objects
set(xyz_MOC_HDRS
  xyz.h
)
QT5_WRAP_CPP(xyz_MOC_SRCS ${xyz_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(xyz MODULE
  xyz_funcs.cpp
  ${xyz_MOC_SRCS}
)

if(APPLE OR WIN32)
set_target_properties(xyz PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)
else(APPLE OR WIN32)
install(TARGETS xyz
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT
RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT
RuntimePlugins
)
endif(APPLE OR WIN32)

target_include_directories(xyz PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

