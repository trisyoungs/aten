add_library(math
  cuboid.cpp
  doubleexp.cpp
  mathfunc.cpp
  matrix.cpp
  constants.h
  cuboid.h
  doubleexp.h
  mathfunc.h
  matrix.h
)
target_include_directories(math PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)
