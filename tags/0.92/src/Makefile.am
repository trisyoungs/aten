bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = templates base script classes energy file gui methods model gui-gtk gui-qt render

aten_LDADD = base/libbase.a script/libscript.a @GUI_OBJECTS@ methods/libmethods.a model/libmodel.a file/libfile.a classes/libclasses.a energy/libenergy.a base/libmath.a templates/libcommand.a @MAC_LIBS@ @GUI_LDLIBS@
