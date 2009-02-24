bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = templates base classes model ff gui methods render parser variables command main

#aten_LDADD = main/libaten.a base/libbase.a command/libcommand.a gui/libgui.a render/librender.a methods/libmethods.a model/libmodel.a base/libsg.a classes/libclasses.a ff/libff.a ff/libfourier.a base/libmath.a @MAC_LIBS@ @GUI_LDLIBS@
aten_LDADD = main/libaten.a gui/libgui.a command/libcommand.a parser/libparser.a variables/libvariables.a render/librender.a methods/libmethods.a model/libmodel.a ff/libff.a ff/libfourier.a base/libbase.a classes/libclasses.a base/libmath.a base/libsg.a @MAC_LIBS@ @GUI_LDLIBS@
