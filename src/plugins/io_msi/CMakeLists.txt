# Meta-Objects
set(msi_MOC_HDRS
  msi.hui
)
QT5_WRAP_CPP(msi_MOC_SRCS ${msi_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(msi MODULE
  msi_funcs.cpp
  ${msi_MOC_SRCS}
)
target_link_libraries(msi
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(msi PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS msi
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(msi PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

