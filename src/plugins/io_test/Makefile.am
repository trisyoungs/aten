pluginexecdir = ${pkglibdir}/plugins
pluginexec_LTLIBRARIES = xyztest.la

# Rules
.hui.lo:
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

clean-local:
	-rm -f xyztest.cpp

xyztest_la_SOURCES = xyztest.hui xyztest_funcs.cpp
xyztest_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
