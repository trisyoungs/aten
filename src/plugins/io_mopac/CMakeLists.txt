# Meta-Objects
set(mopac_MOC_HDRS
  mopac.hui
)
QT5_WRAP_CPP(mopac_MOC_SRCS ${mopac_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(mopac MODULE
  mopac_funcs.cpp
  ${mopac_MOC_SRCS}
)
set_target_properties(mopac PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS mopac
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(mopac PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

