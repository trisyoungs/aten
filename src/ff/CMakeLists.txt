add_library(ff
  combine.h
  energystore.h
  forcefield.h
  forms.h
  angle.cpp 
  bond.cpp 
  combine.cpp
  coulomb.cpp 
  energystore.cpp
  ewald.cpp 
  expression.cpp
  forcefield.cpp
  forms.cpp
  loadforcefield.cpp
  rules.cpp 
  saveforcefield.cpp
  torsion.cpp 
  vdw.cpp
)
target_include_directories(ff PRIVATE
	${PROJECT_SOURCE_DIR}/src
  ${Qt5Gui_INCLUDE_DIRS}
)

