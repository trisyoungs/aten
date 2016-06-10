pluginexecdir = ${pkglibdir}/plugins
noinst_LTLIBRARIES = libcommon.la
pluginexec_LTLIBRARIES = xyz.la xyztraj.la

.hui.lo:
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

libcommon_la_SOURCES = common.cpp
xyz_la_SOURCES = xyz_funcs.cpp xyz.hui
xyz_la_LDFLAGS = -module -shared -avoid-version
xyztraj_la_SOURCES = xyztraj_funcs.cpp xyztraj.hui
xyztraj_la_LDFLAGS = -module -shared -avoid-version

noinst_HEADERS = common.h

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
