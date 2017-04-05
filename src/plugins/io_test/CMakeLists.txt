# Meta-Objects
set(xyztest_MOC_HDRS
  xyztest.hui
)
QT5_WRAP_CPP(xyztest_MOC_SRCS ${xyztest_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(xyztest MODULE
  xyztest_funcs.cpp
  ${xyztest_MOC_SRCS}
)
set_target_properties(xyztest PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS xyztest
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(xyztest PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

