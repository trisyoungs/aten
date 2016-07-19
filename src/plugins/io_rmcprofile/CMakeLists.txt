#----------------------------
# RMC Profile V3 Model Plugin
#----------------------------

# Meta-objects (main interface hui)
set(rmcp3_MOC_HDRS
  rmcp3.hui
)
QT5_WRAP_CPP(rmcp3_MOC_SRCS ${rmcp3_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

# Main plugin library
add_library(rmcp3 MODULE
  rmcp3_funcs.cpp
  common.cpp
  ${rmcp3_MOC_SRCS}
  ${rmcp3_UIS_H}
)
target_link_libraries(rmcp3
  mraten
)
set_target_properties(rmcp3 PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Includes for target
target_include_directories(rmcp3 PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

#----------------------------
# RMC Profile V6 Model Plugin
#----------------------------

# Meta-Objects
set(rmcp6_MOC_HDRS
  rmcp6.hui
#  rmcp6importoptions.h
#  rmcp6exportoptions.h
)
QT5_WRAP_CPP(rmcp6_MOC_SRCS ${rmcp6_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

# User interface files
SET(rmcp6_UIS
# rmcp6importoptions.ui 
# rmcp6exportoptions.ui 
)
QT5_WRAP_UI(rmcp6_UIS_H ${rmcp6_UIS})

# Main plugin library
add_library(rmcp6 MODULE
  rmcp6_funcs.cpp
#  rmcp6importoptions_funcs.cpp
#  rmcp6exportoptions_funcs.cpp
  common.cpp
  ${rmcp6_MOC_SRCS}
)
target_link_libraries(rmcp6
  mraten
)
set_target_properties(rmcp6 PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Includes for target
target_include_directories(rmcp6 PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

#----------------------------
# RMC Profile V6 Bonds Plugin
#----------------------------

# Meta-Objects
set(rmcp6bonds_MOC_HDRS
  rmcp6bonds.hui
)
QT5_WRAP_CPP(rmcp6bonds_MOC_SRCS ${rmcp6bonds_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

# Main plugin library
add_library(rmcp6bonds MODULE
  rmcp6bonds_funcs.cpp
  common.cpp
  ${rmcp6bonds_MOC_SRCS}
)
target_link_libraries(rmcp6bonds
  mraten
)
set_target_properties(rmcp6bonds PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS rmcp6 rmcp6bonds rmcp3
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes for target
target_include_directories(rmcp6bonds PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

