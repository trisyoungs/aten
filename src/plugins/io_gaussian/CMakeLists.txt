# Meta-Objects
set(gaussian_MOC_HDRS
  gaussian.hui
)
QT5_WRAP_CPP(gaussian_MOC_SRCS ${gaussian_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(gaussian MODULE
  gaussian_funcs.cpp
  ${gaussian_MOC_SRCS}
)
target_link_libraries(gaussian
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(gaussian PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS gaussian
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(gaussian PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

