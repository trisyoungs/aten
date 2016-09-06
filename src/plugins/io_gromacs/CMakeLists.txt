# Meta-Objects
set(gromacs_MOC_HDRS
  gromacs.hui
)
QT5_WRAP_CPP(gromacs_MOC_SRCS ${gromacs_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(gromacs MODULE
  gromacs_funcs.cpp
  ${gromacs_MOC_SRCS}
)
target_link_libraries(gromacs
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(gromacs PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS gromacs
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(gromacs PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

