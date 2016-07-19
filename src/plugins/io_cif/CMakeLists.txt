# Meta-Objects
set(cif_MOC_HDRS
  cif.hui
)
QT5_WRAP_CPP(cif_MOC_SRCS ${cif_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(cif MODULE
  cif_funcs.cpp
  ${cif_MOC_SRCS}
)
target_link_libraries(cif
  mraten
)
set_target_properties(cif PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS cif
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(cif PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

