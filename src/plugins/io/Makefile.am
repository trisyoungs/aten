lib_LTLIBRARIES = xyz.la 

.h.cpp:
	${QTMOC} -o $*.cpp ${AM_CPPFLAGS} $<

xyz_la_SOURCES = xyz_funcs.cpp xyz.cpp
xyz_la_LDFLAGS = -module

noinst_HEADERS = xyz.h 

AM_CPPFLAGS = -I$(top_srcdir)/src -I../../ -I$(top_srcdir)/src/gui @ATEN_CFLAGS@

