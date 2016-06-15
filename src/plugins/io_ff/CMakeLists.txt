# Meta-Objects
set(ff_MOC_HDRS
  ff.hui
)
QT5_WRAP_CPP(ff_MOC_SRCS ${ff_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(ff MODULE
  ff_funcs.cpp
  ${ff_MOC_SRCS}
)
set_target_properties(ff PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS ff
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(ff PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

