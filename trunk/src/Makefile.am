bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = templates gui model base classes energy methods parse render command 

aten_LDADD = base/libbase.a command/libcommand.a gui/libgui.a render/librender.a methods/libmethods.a model/libmodel.a base/libsg.a parse/libparse.a classes/libclasses.a energy/libenergy.a classes/libfourier.a base/libmath.a @MAC_LIBS@ @GUI_LDLIBS@
