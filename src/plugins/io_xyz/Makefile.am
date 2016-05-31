pluginexecdir = ${pkglibdir}/plugins
pluginexec_LTLIBRARIES = xyz.la

%.cpp: %.h
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<

clean-local:
	-rm -f xyz.cpp

xyz_la_SOURCES = xyz_funcs.cpp xyz.cpp
xyz_la_LDFLAGS = -module -shared -avoid-version

noinst_HEADERS = xyz.h 

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
