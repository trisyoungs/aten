### Cube Model

# Meta-Objects
set(cubemodel_MOC_HDRS
  cubemodel.hui
)
QT5_WRAP_CPP(cubemodel_MOC_SRCS ${cubemodel_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(cubemodel MODULE
  cubemodel_funcs.cpp
  ${cubemodel_MOC_SRCS}
)
target_link_libraries(cubemodel
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(cubemodel PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

target_include_directories(cubemodel PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

#### Cube

# Meta-Objects
set(cube_MOC_HDRS
  cube.hui
)
QT5_WRAP_CPP(cube_MOC_SRCS ${cube_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(cube MODULE
  cube_funcs.cpp
  ${cube_MOC_SRCS}
)
target_link_libraries(cube
  ${PLUGIN_LINK_LIBS}
)
set_target_properties(cube PROPERTIES
   LIBRARY_OUTPUT_DIRECTORY ${Aten_BINARY_DIR}/data/plugins
   COMPILE_DEFINITIONS "QT_PLUGIN"
   PREFIX ""
)

# Install Targets

# Install Targets
if(UNIX AND NOT APPLE)
install(TARGETS cube
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/aten/plugins COMPONENT RuntimePlugins
)
endif(UNIX AND NOT APPLE)

target_include_directories(cube PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

