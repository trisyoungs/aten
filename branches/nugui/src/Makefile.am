bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = gui templates base classes model ff methods render command parser main

aten_LDADD = main/libaten.a parser/libparser.a command/libcommand.a gui/libgui.a render/librender.a methods/libmethods.a model/libmodel.a ff/libff.a ff/libfourier.a base/libbase.a classes/libclasses.a base/libmath.a base/libsg.a @MAC_LIBS@ @GUI_LDLIBS@
