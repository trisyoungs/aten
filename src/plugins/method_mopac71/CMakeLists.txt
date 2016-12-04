if(BUILD_MOPACPLUGIN)

#
# Main MOPAC 7.1 Method Plugin
#

# Meta-Objects
set(mopac71_MOC_HDRS
  mopac71.hui
)
QT5_WRAP_CPP(mopac71_MOC_SRCS ${mopac71_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(mopac71method MODULE
  common.cpp
  mopac71_funcs.cpp
  ${mopac71_MOC_SRCS}
)
set_target_properties(mopac71method PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

target_include_directories(mopac71method PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

#
# MOPAC 7.1  Control (File) Plugin
#

# Meta-Objects
set(mopac71control_MOC_HDRS
  mopac71control.hui
  controlexportoptions.h
)
QT5_WRAP_CPP(mopac71control_MOC_SRCS ${mopac71control_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(mopac71control_UIS
  controlexportoptions.ui
)
QT5_WRAP_UI(mopac71control_UIS_H ${mopac71control_UIS})

add_library(mopac71control MODULE
  common.cpp
  mopac71control_funcs.cpp
  controlexportoptions_funcs.cpp
  ${mopac71control_MOC_SRCS}
  ${mopac71control_UIS_H}
)

set_target_properties(mopac71control PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

target_include_directories(mopac71control PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

#
# MOPAC 7.1  Geometry Optimisation Plugin
#

# Meta-Objects
set(mopac71opt_MOC_HDRS
  mopac71opt.hui
  optoptions.h
)
QT5_WRAP_CPP(mopac71opt_MOC_SRCS ${mopac71opt_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(mopac71opt_UIS
  optoptions.ui
)
QT5_WRAP_UI(mopac71opt_UIS_H ${mopac71opt_UIS})

add_library(mopac71opt MODULE
  common.cpp
  mopac71opt_funcs.cpp
  optoptions_funcs.cpp
  ${mopac71opt_MOC_SRCS}
  ${mopac71opt_UIS_H}
)

set_target_properties(mopac71opt PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)
target_link_libraries(mopac71opt mopac71f90)
target_include_directories(mopac71opt PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

#
# Install Targets
#

if(UNIX AND NOT APPLE)
install(TARGETS mopac71method mopac71control mopac71opt
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

#
# MOPAC 7.1 Fortran Source
#
add_subdirectory(mopac7.1)

endif()
