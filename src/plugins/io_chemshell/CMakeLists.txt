# Meta-Objects
set(chemshell_MOC_HDRS
  chemshell.hui
  chemshellexportoptions.h
)
QT5_WRAP_CPP(chemshell_MOC_SRCS ${chemshell_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(chemshell_UIS
 chemshellexportoptions.ui 
)
QT5_WRAP_UI(chemshell_UIS_H ${chemshell_UIS})

add_library(chemshell MODULE
  chemshell_funcs.cpp
  chemshellexportoptions_funcs.cpp
  ${chemshell_MOC_SRCS}
  ${chemshell_UIS_H}
)
target_link_libraries(chemshell
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(chemshell PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS chemshell
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(chemshell PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

