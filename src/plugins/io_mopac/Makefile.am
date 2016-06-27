# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = mopacarc.la mopaccontrol.la

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
	-rm -f mopacarc.cpp mopaccontrol.cpp

# Common functions between plugins
noinst_LTLIBRARIES = libcommon.la
libcommon_la_SOURCES = common.cpp

# MOPAC Control File Model plugin
mopaccontrol_la_SOURCES = controlexportoptions.ui controlexportoptions_funcs.cpp mopaccontrol_funcs.cpp mopaccontrol.hui
mopaccontrol_la_LIBADD = libcommon.la
mopaccontrol_la_LDFLAGS = -module -shared -avoid-version

# MOPAC Archive File Model plugin
mopacarc_la_SOURCES = mopacarc_funcs.cpp mopacarc.hui
mopacarc_la_LIBADD = libcommon.la
mopacarc_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

noinst_HEADERS = common.h controlexportoptions.h
