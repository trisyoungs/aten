# Meta-Objects
set(testtool_MOC_HDRS
  testtool.hui
  testtooldialog.h
)
QT5_WRAP_CPP(testtool_MOC_SRCS ${testtool_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(testtool_UIS
 testtooldialog.ui
)
QT5_WRAP_UI(testtool_UIS_H ${testtool_UIS})

# Resources
set(testtool_RES_QRC
  testtool_icons.qrc 
)
QT5_ADD_RESOURCES(testtool_RES ${testtool_RES_QRC})

add_library(testtool MODULE
  testtool_funcs.cpp
  testtooldialog_funcs.cpp
  ${testtool_RES}
  ${testtool_MOC_SRCS}
  ${testtool_UIS_H}
)
target_link_libraries(testtool
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(testtool PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS testtool
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(testtool PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

