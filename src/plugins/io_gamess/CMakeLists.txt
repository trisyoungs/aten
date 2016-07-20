# Meta-Objects
set(gamess_MOC_HDRS
  gamess.hui
)
QT5_WRAP_CPP(gamess_MOC_SRCS ${gamess_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(gamess MODULE
  gamess_funcs.cpp
  ${gamess_MOC_SRCS}
)
target_link_libraries(gamess
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(gamess PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS gamess
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(gamess PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

