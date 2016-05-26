add_library(plugins
  interfaces.cpp
  interfaces.h
  pluginstore.h
  plugintypes.h
)
target_include_directories(plugins PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

target_link_libraries(plugins)

add_subdirectory(io_xyz)
