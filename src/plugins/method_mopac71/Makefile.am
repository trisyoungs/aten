if MOPACPLUGIN

SUBDIRS = mopac7.1

# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = mopac71method.la mopac71control.la mopac71opt.la

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
	-rm -f mopac71.cpp mopac71control.cpp mopac71opt.cpp

# Mopac71 General Plugin
mopac71method_la_SOURCES = mopac71_funcs.cpp mopac71.hui
mopac71method_la_LIBADD = mopac7.1/libmopac71f90.la
mopac71method_la_LDFLAGS = -module -shared -avoid-version -lgfortran

# Mopac71 Control (File/IO) Plugin
mopac71control_la_SOURCES = controlexportoptions.ui controlexportoptions_funcs.cpp
mopac71control_la_SOURCES += mopac71control_funcs.cpp mopac71control.hui 
mopac71control_la_LDFLAGS = -module -shared -avoid-version

# Mopac71 Geometry Optimisation Plugin
mopac71opt_la_SOURCES = optoptions.ui optoptions_funcs.cpp
mopac71opt_la_SOURCES += mopac71opt_funcs.cpp mopac71opt.hui
mopac71opt_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

endif
