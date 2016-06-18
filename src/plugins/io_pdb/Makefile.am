# Set plugin installation dir and define plugin targets
pluginexecdir = ${pkglibdir}/plugins
pluginexec_LTLIBRARIES = pdb.la

# Rules
.ui.lo:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $*.h
	${LIBTOOL} --tag=CXX --mode=compile ${CXX} -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c moc_$*.cpp -o $@
	rm moc_$*.cpp

.hui.lo:
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

# Local clean (temporary files generated from rules)
clean-local:
	-rm -f pdb.cpp

# AKF Model plugin
pdb_la_SOURCES = pdbimportoptions.ui pdbimportoptions_funcs.cpp pdb_funcs.cpp pdb.hui
pdb_la_LDFLAGS = -module -shared -avoid-version

noinst_HEADERS = pdbimportoptions.h

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
