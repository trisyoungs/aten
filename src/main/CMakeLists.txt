add_library(main STATIC
  aten.h
  cli.h 
  version.h 
  aten.cpp
  cli.cpp
  clipboards.cpp
  commands.cpp
  current.cpp
  encoders.cpp
  ff.cpp
  fragments.cpp
  importexport.cpp
  includes.cpp
  locations.cpp
  models.cpp
  modes.cpp
  partitions.cpp
  plugins.cpp
  prefs.cpp
  scripts.cpp
  session.cpp
)
target_include_directories(main PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${Qt5Widgets_INCLUDE_DIRS}
  ${READLINE_INCLUDE_DIRS}
  ${PROJECT_SOURCE_DIR}/src/gui
)
target_link_libraries(main gui) 
