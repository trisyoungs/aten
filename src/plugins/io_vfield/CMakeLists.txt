# Meta-Objects
set(vfield_MOC_HDRS
  vfield.hui
)
QT5_WRAP_CPP(vfield_MOC_SRCS ${vfield_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(vfield MODULE
  vfield_funcs.cpp
  ${vfield_MOC_SRCS}
)
target_link_libraries(vfield
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(vfield PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS vfield
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(vfield PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

