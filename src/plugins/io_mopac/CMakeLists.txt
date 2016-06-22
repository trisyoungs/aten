# MOPAC Control file
set(mopaccontrol_MOC_HDRS
  mopaccontrol.hui
)
QT5_WRAP_CPP(mopaccontrol_MOC_SRCS ${mopaccontrol_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(mopaccontrol_UIS
  controlexportoptions.ui 
)
QT5_WRAP_UI(mopaccontrol_UIS_H ${mopaccontrol_UIS})

add_library(mopaccontrol MODULE
  mopaccontrol_funcs.cpp
  controlexportoptions_funcs.cpp
  common.cpp
  ${mopaccontrol_MOC_SRCS}
  ${mopaccontrol_UIS_H}
)
set_target_properties(mopaccontrol PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# MOPAC Arc file
set(mopacarc_MOC_HDRS
  mopacarc.hui
)
QT5_WRAP_CPP(mopacarc_MOC_SRCS ${mopacarc_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(mopacarc MODULE
  mopacarc_funcs.cpp
  common.cpp
  ${mopacarc_MOC_SRCS}
)
set_target_properties(mopacarc PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)
if(UNIX AND NOT APPLE)
install(TARGETS mopacarc mopaccontrol
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(mopacarc PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

target_include_directories(mopaccontrol PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

