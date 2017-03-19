# Meta-Objects
set(ringstool_MOC_HDRS
  ringstool.hui
  ringstooldialog.h
)
QT5_WRAP_CPP(ringstool_MOC_SRCS ${ringstool_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(ringstool_UIS
 ringstooldialog.ui
)
QT5_WRAP_UI(ringstool_UIS_H ${ringstool_UIS})

# Resources
set(ringstool_RES_QRC
  ringstool_icons.qrc 
)
QT5_ADD_RESOURCES(ringstool_RES ${ringstool_RES_QRC})

add_library(ringstool MODULE
  ringstool_funcs.cpp
  ringstooldialog_funcs.cpp
  ${ringstool_RES}
  ${ringstool_MOC_SRCS}
  ${ringstool_UIS_H}
)
target_link_libraries(ringstool
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(ringstool PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS ringstool
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(ringstool PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

