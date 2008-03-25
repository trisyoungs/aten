add_library(base
cli.h 
constants.h 
debug.h 
elements.h 
master.h 
prefs.h 
sysfunc.h
cli.cpp 
debug.cpp 
elements.cpp 
master.cpp 
prefs.cpp 
sysfunc.cpp
)

add_library(math
mathfunc.h
mathfunc.cpp
)

add_library(sg
generator.h
spacegroup.h
generator.cpp 
spacegroup.cpp
)

include_directories(
../
)
