bin_PROGRAMS = aten

aten_SOURCES = main.cpp

SUBDIRS = gui templates base sg math model ff methods render command parser plugins main

noinst_LTLIBRARIES = libaten.la
libaten_la_SOURCES =
libaten_la_LIBADD = base/libmessenger.la gui/libgui.la parser/libparser.la gui/libtreegui.la command/libcommand.la methods/libmethods.la model/libmodel.la render/librender.la math/libmath.la main/libmain.la plugins/libplugins.la ff/libff.la base/libbase.la sg/libsg.la base/libfourierdata.la
libaten_la_LDFLAGS = -static

aten_LDADD = libaten.la @ATEN_LDLIBS@ @ATEN_LDFLAGS@

AM_CPPFLAGS = @ATEN_INCLUDES@ @ATEN_CFLAGS@
