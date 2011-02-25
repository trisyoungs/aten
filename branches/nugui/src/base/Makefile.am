noinst_LIBRARIES = libbase.a libmath.a libsg.a

libmath_a_SOURCES = mathfunc.cpp

libbase_a_SOURCES = atomaddress.cpp atom.cpp atom_geometry.cpp bond.cpp bundle.cpp cell.cpp colourscalepoint.cpp dnchar.cpp doubleexp.cpp eigenvector.cpp elements.cpp glyph.cpp kvmap.cpp lineparser.cpp log.cpp mathfunc.cpp matrix.cpp measurement.cpp messenger.cpp pattern.cpp region.cpp sysfunc.cpp vibration.cpp wrapint.cpp

libsg_a_SOURCES = generator.cpp sgclib.c sgfind.c sghkl.c sgio.c sgsi.c spacegroup.cpp

noinst_HEADERS = atomaddress.h atom.h bond.h bundle.h cell.h colourscalepoint.h constants.h dnchar.h doubleexp.h eigenvector.h elements.h generator.h glyph.h kvmap.h lineparser.h log.h mathfunc.h matrix.h measurement.h messenger.h pattern.h region.h sginfo.h spacegroup.h sysfunc.h vibration.h wrapint.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
