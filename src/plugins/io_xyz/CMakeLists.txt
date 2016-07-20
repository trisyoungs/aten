# Meta-Objects
set(xyz_MOC_HDRS
  xyz.hui
  xyzimportoptions.h
  xyzexportoptions.h
)
QT5_WRAP_CPP(xyz_MOC_SRCS ${xyz_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(xyz_UIS
 xyzimportoptions.ui 
 xyzexportoptions.ui 
)
QT5_WRAP_UI(xyz_UIS_H ${xyz_UIS})

add_library(xyz MODULE
  xyz_funcs.cpp
  xyzimportoptions_funcs.cpp
  xyzexportoptions_funcs.cpp
  common.cpp
  ${xyz_MOC_SRCS}
  ${xyz_UIS_H}
)
target_link_libraries(xyz
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(xyz PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Meta-Objects
set(xyztraj_MOC_HDRS
  xyztraj.hui
)
QT5_WRAP_CPP(xyztraj_MOC_SRCS ${xyztraj_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(xyztraj MODULE
  xyztraj_funcs.cpp
  common.cpp
  ${xyztraj_MOC_SRCS}
)
target_link_libraries(xyztraj
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(xyztraj PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS xyz xyztraj
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(xyz PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

target_include_directories(xyztraj PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

