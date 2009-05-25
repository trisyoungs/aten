noinst_LIBRARIES = libbase.a libmath.a libsg.a

libmath_a_SOURCES = mathfunc.cpp

libbase_a_SOURCES = atomaddress.cpp atom.cpp bond.cpp bundle.cpp cell.cpp colourscalepoint.cpp dnchar.cpp elements.cpp forms.cpp glyph.cpp log.cpp mathfunc.cpp measurement.cpp messenger.cpp lineparser.cpp pattern.cpp region.cpp sysfunc.cpp

libsg_a_SOURCES = generator.cpp sgclib.c sgfind.c sghkl.c sgio.c sgsi.c spacegroup.cpp

noinst_HEADERS = atomaddress.h atom.h bond.h bundle.h cell.h colourscalepoint.h constants.h dnchar.h elements.h forms.h generator.h glyph.h log.h mathfunc.h measurement.h messenger.h lineparser.h pattern.h region.h sginfo.h spacegroup.h sysfunc.h

INCLUDES = -I$(top_srcdir)/s.cpp -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
