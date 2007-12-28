bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = templates base command classes energy gui methods model gui-gtk gui-qt parse render

aten_LDADD = base/libbase.a command/libcommand.a @GUI_OBJECTS@ methods/libmethods.a model/libmodel.a parse/libparse.a classes/libclasses.a energy/libenergy.a base/libmath.a @MAC_LIBS@ @GUI_LDLIBS@
