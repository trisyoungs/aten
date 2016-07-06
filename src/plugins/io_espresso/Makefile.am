# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = espressoin.la espressoout.la

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
	-rm -f espressoin.cpp espressoout.cpp

# Quantum Espresso Input file exporter
espressoin_la_SOURCES = espressoin_funcs.cpp espressoin.hui
espressoin_la_LDFLAGS = -module -shared -avoid-version

# Quantum Espresso Output file importer
espressoout_la_SOURCES = espressoout_funcs.cpp espressoout.hui
espressoout_la_LDFLAGS = -module -shared -avoid-version


AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
