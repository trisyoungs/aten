add_library(methods STATIC
  calculable.h 
  cg.h 
  delaunay.h
  disorderdata.h
  geometry.h 
  linemin.h 
  mc.h 
  partitioncelldata.h
  partitiondata.h
  partitioningscheme.h
  pdens.h 
  rdf.h 
  sd.h
  calculable.cpp 
  cg.cpp 
  delaunay.cpp
  disorder.cpp
  disorderdata.cpp
  geometry.cpp 
  linemin.cpp 
  mc.cpp 
  partitioncelldata.cpp
  partitiondata.cpp
  partitioningscheme.cpp
  pdens.cpp 
  rdf.cpp 
  sd.cpp
)
target_include_directories(methods PRIVATE
  ${PROJECT_SOURCE_DIR}/src
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
)

