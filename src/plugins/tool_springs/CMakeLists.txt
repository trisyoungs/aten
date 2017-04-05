# Meta-Objects
set(springstool_MOC_HDRS
  springstool.hui
  springstooldialog.h
)
QT5_WRAP_CPP(springstool_MOC_SRCS ${springstool_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(springstool_UIS
 springstooldialog.ui
)
QT5_WRAP_UI(springstool_UIS_H ${springstool_UIS})

# Resources
set(springstool_RES_QRC
  springstool_icons.qrc 
)
QT5_ADD_RESOURCES(springstool_RES ${springstool_RES_QRC})

add_library(springstool MODULE
  springstool_funcs.cpp
  springstooldialog_funcs.cpp
  ${springstool_RES}
  ${springstool_MOC_SRCS}
  ${springstool_UIS_H}
)
target_link_libraries(springstool
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(springstool PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS springstool
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(springstool PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${Qt5Widgets_INCLUDE_DIRS}
)

