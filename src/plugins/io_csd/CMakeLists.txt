# Meta-Objects
set(csd_MOC_HDRS
  csd.hui
)
QT5_WRAP_CPP(csd_MOC_SRCS ${csd_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(csd MODULE
  csd_funcs.cpp
  ${csd_MOC_SRCS}
)
target_link_libraries(csd
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(csd PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS csd
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(csd PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

