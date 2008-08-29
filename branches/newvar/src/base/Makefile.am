noinst_LIBRARIES = libbase.a libmath.a libsg.cpp

libmath_a_SOURCES = mathfunc.cpp

libbase_a_SOURCES = atomaddress.cpp atom.cpp bond.cpp bundle.cpp cell.cpp colourscalepoint.cpp dnchar.cpp elements.cpp forcefieldparams.cpp generator.cpp glyph.cpp log.cpp mathfunc.cpp measurement.cpp pattern.cpp region.cpp spacegroup.cpp sysfunc.cpp vaccess.cpp vobject.cpp

libsg_a_SOURCES = generator.cpp spacegroup.cpp

noinst_HEADERS = atomaddress.h atom.h bond.h bundle.h cell.h colourscalepoint.h constants.h dnchar.h elements.h forcefieldparams.h generator.h glyph.h log.h mathfunc.h measurement.h pattern.h region.h spacegroup.h sysfunc.h vaccess.h vobject.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
