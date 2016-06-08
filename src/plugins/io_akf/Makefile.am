pluginexecdir = ${pkglibdir}/plugins
pluginexec_LTLIBRARIES = akf.la

%.cpp: %.h
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<

clean-local:
	-rm -f akf.cpp

akf_la_SOURCES = akf_funcs.cpp akf.cpp
akf_la_LDFLAGS = -module -shared -avoid-version

noinst_HEADERS = akf.h 

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
