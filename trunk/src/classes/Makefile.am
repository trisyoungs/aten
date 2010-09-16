noinst_LIBRARIES = libclasses.a

BUILT_SOURCES = neta_grammar.h
AM_YFLAGS = -d

libclasses_a_SOURCES = basisfunction.cpp colourscale.cpp forcefieldatom.cpp forcefieldbound.cpp grid.cpp neta.cpp neta_grammar.yy neta_lexer.cpp neta_parser.cpp prefs.cpp restraint.cpp ring.cpp site.cpp zmatrix.cpp

noinst_HEADERS = basisfunction.h colourscale.h forcefieldatom.h forcefieldbound.h grid.h neta.h neta_parser.h prefs.h restraint.h ring.h site.h zmatrix.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
