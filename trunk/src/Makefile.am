bin_PROGRAMS = aten

aten_SOURCES = main.cpp

noinst_LTLIBRARIES = libaten.la

libaten_la_SOURCES = 
libaten_la_LIBADD = main/libmain.la gui/libgui.la render/librender.la ff/libff.la ff/libfourier.la command/libcommand.la methods/libmethods.la model/libmodel.la parser/libparser.la base/libbase.la math/libmath.la base/libsg.la

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = gui templates base math model ff methods render command parser main

# aten_LDADD =  @MAC_LIBS@ @GUI_LDLIBS@

aten_LDADD =  libaten.la @MAC_LIBS@ @GUI_LDLIBS@