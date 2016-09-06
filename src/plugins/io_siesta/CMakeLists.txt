# Meta-Objects
set(siesta_MOC_HDRS
  siesta.hui
)
QT5_WRAP_CPP(siesta_MOC_SRCS ${siesta_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(siesta MODULE
  siesta_funcs.cpp
  ${siesta_MOC_SRCS}
)
target_link_libraries(siesta
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(siesta PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS siesta
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(siesta PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

