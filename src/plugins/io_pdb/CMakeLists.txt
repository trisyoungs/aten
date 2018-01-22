# Meta-Objects
set(pdb_MOC_HDRS
  pdb.hui
  pdbexportoptions.h
  pdbimportoptions.h
)
QT5_WRAP_CPP(pdb_MOC_SRCS ${pdb_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(pdb_UIS
 pdbexportoptions.ui 
 pdbimportoptions.ui 
)
QT5_WRAP_UI(pdb_UIS_H ${pdb_UIS})

add_library(pdb MODULE
  pdb_funcs.cpp
  pdbexportoptions_funcs.cpp
  pdbimportoptions_funcs.cpp
  ${pdb_MOC_SRCS}
  ${pdb_UIS_H}
)
target_link_libraries(pdb
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(pdb PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

if(UNIX AND NOT APPLE)
install(TARGETS pdb
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(pdb PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

