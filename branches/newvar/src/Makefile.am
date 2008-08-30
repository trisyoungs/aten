bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = templates base classes model ff methods parse render command aten gui

aten_LDADD = aten/libaten.a base/libbase.a command/libcommand.a gui/libgui.a render/librender.a methods/libmethods.a model/libmodel.a base/libsg.a parse/libparse.a classes/libclasses.a ff/libff.a ff/libfourier.a base/libmath.a @MAC_LIBS@ @GUI_LDLIBS@
