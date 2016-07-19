# Meta-Objects
set(pdens_MOC_HDRS
  pdens.hui
)
QT5_WRAP_CPP(pdens_MOC_SRCS ${pdens_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(dlputilspdens MODULE
  pdens_funcs.cpp
  ${pdens_MOC_SRCS}
)
target_link_libraries(dlputilspdens
  mraten
  Qt5::Widgets
  Qt5::Core
)
set_target_properties(dlputilspdens PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Meta-Objects
set(surface_MOC_HDRS
  surface.hui
)
QT5_WRAP_CPP(surface_MOC_SRCS ${surface_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(dlputilssurface MODULE
  surface_funcs.cpp
  ${surface_MOC_SRCS}
)
target_link_libraries(dlputilssurface
  mraten
  Qt5::Widgets
  Qt5::Core
)
set_target_properties(dlputilssurface PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS dlputilspdens dlputilssurface
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

# Includes
target_include_directories(dlputilspdens PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

target_include_directories(dlputilssurface PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

