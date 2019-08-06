# Meta-Objects
set(chemshelltool_MOC_HDRS
  chemshelltool.hui
  chemshelltooldialog.h
)
QT5_WRAP_CPP(chemshelltool_MOC_SRCS ${chemshelltool_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(chemshelltool_UIS
 chemshelltooldialog.ui
)
QT5_WRAP_UI(chemshelltool_UIS_H ${chemshelltool_UIS})

# Resources
set(chemshelltool_RES_QRC
  chemshelltool_icons.qrc 
)
QT5_ADD_RESOURCES(chemshelltool_RES ${chemshelltool_RES_QRC})

add_library(chemshelltool MODULE
  chemshelltool_funcs.cpp
  chemshelltooldialog_funcs.cpp
  ${chemshelltool_RES}
  ${chemshelltool_MOC_SRCS}
  ${chemshelltool_UIS_H}
)
target_link_libraries(chemshelltool
  plugins
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(chemshelltool PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS chemshelltool
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(chemshelltool PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${Qt5Widgets_INCLUDE_DIRS}
)

