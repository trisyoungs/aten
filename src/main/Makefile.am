noinst_LTLIBRARIES = libmain.la

libmain_la_SOURCES = aten.cpp cli.cpp clipboards.cpp commands.cpp current.cpp encoders.cpp ff.cpp fragments.cpp importexport.cpp includes.cpp locations.cpp models.cpp modes.cpp partitions.cpp plugins.cpp prefs.cpp scripts.cpp session.cpp tools.cpp

noinst_HEADERS = aten.h cli.h version.h

AM_CPPFLAGS = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @ATEN_INCLUDES@ @ATEN_CFLAGS@
