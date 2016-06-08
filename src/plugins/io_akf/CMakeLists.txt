# Meta-Objects
set(akf_MOC_HDRS
  akf.h
)
QT5_WRAP_CPP(akf_MOC_SRCS ${akf_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(akf MODULE
  akf_funcs.cpp
  ${akf_MOC_SRCS}
)
set_target_properties(akf PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS akf
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(akf PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

