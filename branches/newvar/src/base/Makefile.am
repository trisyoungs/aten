noinst_LIBRARIES = libbase.a libmath.a libsg.a

libmath_a_SOURCES = mathfunc.cpp

libbase_a_SOURCES = aten.cpp cli.cpp elements.cpp messenger.cpp modes.cpp prefs.cpp sysfunc.cpp 

libsg_a_SOURCES = generator.cpp spacegroup.cpp

noinst_HEADERS = cli.h constants.h elements.h generator.h aten.h mathfunc.h messenger.h prefs.h spacegroup.h sysfunc.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
