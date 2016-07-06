# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = epsrato.la epsrmol.la

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
	-rm -f ato.cpp mol.cpp

# Ato Model Plugin
#epsrato_la_SOURCES = atoimportoptions.ui atoimportoptions_funcs.cpp
epsrato_la_SOURCES = ato_funcs.cpp ato.hui 
epsrato_la_LDFLAGS = -module -shared -avoid-version

# Molfile Model Plugin
epsrmol_la_SOURCES = molexportoptions.ui molexportoptions_funcs.cpp
epsrmol_la_SOURCES += mol_funcs.cpp mol.hui
epsrmol_la_LDFLAGS = -module -shared -avoid-version

noinst_HEADERS = molexportoptions.h

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
