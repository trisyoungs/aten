# Meta-Objects
set(gamessus_MOC_HDRS
  gamessus.hui
)
QT5_WRAP_CPP(gamessus_MOC_SRCS ${gamessus_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(gamessus MODULE
  gamessus_funcs.cpp
  ${gamessus_MOC_SRCS}
)
target_link_libraries(gamessus
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(gamessus PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS gamessus
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(gamessus PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

