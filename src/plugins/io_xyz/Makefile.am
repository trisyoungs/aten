# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = xyz.la xyztraj.la

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
	-rm -f xyz.cpp xyztraj.cpp

# Common functions to both plugins
noinst_LTLIBRARIES = libcommon.la
libcommon_la_SOURCES = common.cpp

# XYZ Model Plugin
xyz_la_SOURCES = xyzimportoptions.ui xyzimportoptions_funcs.cpp xyzexportoptions.ui xyzexportoptions_funcs.cpp xyz_funcs.cpp xyz.hui 
xyz_la_LIBADD = libcommon.la
xyz_la_LDFLAGS = -module -shared -avoid-version

# XYZ Trajectory Plugin
xyztraj_la_SOURCES = xyztraj_funcs.cpp xyztraj.hui
xyztraj_la_LIBADD = libcommon.la
xyztraj_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

noinst_HEADERS = common.h xyzimportoptions.h xyzexportoptions.h

