if(MOPACPLUGIN)

# Meta-Objects
set(mopac71_MOC_HDRS
  mopac71.hui
)
QT5_WRAP_CPP(mopac71_MOC_SRCS ${mopac71_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(mopac71method MODULE
  mopac71_funcs.cpp
  ${mopac71_MOC_SRCS}
)
set_target_properties(mopac71method PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS mopac71method
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(mopac71method PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

add_subdirectory(mopac7.1)

endif(MOPACPLUGIN)
