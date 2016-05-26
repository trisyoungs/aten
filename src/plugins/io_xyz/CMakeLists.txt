# Meta-Objects
set(xyz_MOC_HDRS
  xyz.h
)
QT5_WRAP_CPP(xyz_MOC_SRCS ${xyz_MOC_HDRS} OPTIONS -I${PROJECT_SOURCE_DIR}/src)

add_library(xyz MODULE
  xyz_funcs.cpp
  ${xyz_MOC_SRCS}
)

target_include_directories(xyz PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_BINARY_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)
