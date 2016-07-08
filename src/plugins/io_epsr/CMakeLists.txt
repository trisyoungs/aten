# EPSR ATO file
set(epsrato_MOC_HDRS
  ato.hui
  #atoimportoptions.h
)
QT5_WRAP_CPP(epsrato_MOC_SRCS ${epsrato_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(epsrato_UIS
 #atoimportoptions.ui 
)
QT5_WRAP_UI(epsrato_UIS_H ${epsrato_UIS})

add_library(epsrato MODULE
  ato_funcs.cpp
  ${epsrato_MOC_SRCS}
  ${epsrato_UIS_H}
)
set_target_properties(epsrato PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# EPSR Molfile
set(epsrmol_MOC_HDRS
  mol.hui
)
QT5_WRAP_CPP(epsrmol_MOC_SRCS ${epsrmol_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

SET(epsrmol_UIS
  molexportoptions.ui 
)
QT5_WRAP_UI(epsrmol_UIS_H ${epsrmol_UIS})

add_library(epsrmol MODULE
  mol_funcs.cpp
  molexportoptions_funcs.cpp
  ${epsrmol_MOC_SRCS}
  ${epsrmol_UIS_H}
)
set_target_properties(epsrmol PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS epsrato epsrmol
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(epsrato PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

target_include_directories(epsrmol PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

