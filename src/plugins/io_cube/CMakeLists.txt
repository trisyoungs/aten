# Meta-Objects
set(cube_MOC_HDRS
  cube.hui
)
QT5_WRAP_CPP(cube_MOC_SRCS ${cube_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(cube MODULE
  cube_funcs.cpp
  ${cube_MOC_SRCS}
)
target_link_libraries(cube
  mraten
  Qt5::Widgets
  Qt5::Core
)
set_target_properties(cube PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS cube
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(cube PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

