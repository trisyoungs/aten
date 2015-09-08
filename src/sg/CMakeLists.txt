add_library(sg
  generator.h
  spacegroup.h
  sginfo.h
  generator.cpp 
  sgclib.c
  sgfind.c
  sghkl.c
  sgio.c
  sgsi.c
  spacegroup.cpp
)
target_include_directories(sg PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

