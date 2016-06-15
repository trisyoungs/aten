# Meta-Objects
set(mol_MOC_HDRS
  mol.hui
)
QT5_WRAP_CPP(mol_MOC_SRCS ${mol_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(mol MODULE
  mol_funcs.cpp
  ${mol_MOC_SRCS}
)
set_target_properties(mol PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS mol
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(mol PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

