noinst_LTLIBRARIES = libmessenger.la libbase.la libfourierdata.la

SUFFIXES: .h .hui

.h.hh:
	ln -s $< $@

.hui.lo:
	$(QTMOC) -o $*.cpp -f$< $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

clean-local:
	-rm -f ui_* *.o task.cpp moc_*.cpp

AM_YFLAGS = -d

libbase_la_SOURCES = atomaddress.cpp atom.cpp atom_geometry.cpp basisshell.cpp bond.cpp cell.cpp choice.cpp colourscale.cpp colourscalepoint.cpp datastore.cpp eigenvector.cpp element.cpp elementmap.cpp encoderdefinition.cpp externalcommand.cpp fileparser.cpp forcefieldatom.cpp forcefieldbound.cpp glyph.cpp grid.cpp gridpoint.cpp kvmap.cpp lineparser.cpp log.cpp measurement.cpp neta.cpp neta_grammar.yy neta_grammar.hh neta_lexer.cpp neta_parser.cpp pattern.cpp prefs.cpp ring.cpp site.cpp sysfunc.cpp vibration.cpp wrapint.cpp zmatrix.cpp zmatrixelement.cpp

libfourierdata_la_SOURCES = fourierdata.cpp

libmessenger_la_SOURCES = message.cpp messenger.h messenger.cpp task.hui task_funcs.cpp

noinst_HEADERS = atomaddress.h atom.h basisshell.h bond.h cell.h choice.h colourscale.h colourscalepoint.h datastore.h eigenvector.h element.h elementmap.h encoderdefinition.h externalcommand.h fileparser.h forcefieldatom.h forcefieldbound.h fourierdata.h glyph.h grid.h gridpoint.h kvmap.h lineparser.h log.h measurement.h message.h messenger.h namespace.h neta.h neta_parser.h pattern.h prefs.h ring.h site.h sysfunc.h vibration.h wrapint.h zmatrix.h zmatrixelement.h

CLEANFILES = neta_grammar.h neta_grammar.cc neta_grammar.hh

AM_CPPFLAGS = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @ATEN_INCLUDES@ @ATEN_CFLAGS@
