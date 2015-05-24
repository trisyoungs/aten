noinst_LIBRARIES = libbase.a libsg.a libfourierdata.a

SUFFIXES: .h

.h.hh:
	ln -s $< $@

AM_YFLAGS = -d

libbase_a_SOURCES = atomaddress.cpp atom.cpp atom_geometry.cpp basisshell.cpp bond.cpp cell.cpp choice.cpp colourscale.cpp colourscalepoint.cpp datastore.cpp eigenvector.cpp element.cpp elementmap.cpp forcefieldatom.cpp forcefieldbound.cpp glyph.cpp grid.cpp gridpoint.cpp kvmap.cpp lineparser.cpp log.cpp measurement.cpp message.cpp messenger.cpp neta.cpp neta_grammar.yy neta_grammar.hh neta_lexer.cpp neta_parser.cpp pattern.cpp prefs.cpp progress.cpp ring.cpp site.cpp sysfunc.cpp vibration.cpp wrapint.cpp zmatrix.cpp zmatrixelement.cpp

libsg_a_SOURCES = generator.cpp sgclib.c sgfind.c sghkl.c sgio.c sgsi.c spacegroup.cpp

libfourierdata_a_SOURCES = fourierdata.cpp

noinst_HEADERS = atomaddress.h atom.h basisshell.h bond.h cell.h choice.h colourscale.h colourscalepoint.h datastore.h eigenvector.h element.h elementmap.h forcefieldatom.h forcefieldbound.h fourierdata.h generator.h glyph.h grid.h gridpoint.h kvmap.h lineparser.h log.h measurement.h message.h messenger.h namespace.h neta.h neta_parser.h pattern.h prefs.h progress.h ring.h sginfo.h site.h spacegroup.h sysfunc.h vibration.h wrapint.h zmatrix.h zmatrixelement.h

CLEANFILES = neta_grammar.h neta_grammar.cc neta_grammar.hh

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @ATEN_CFLAGS@
