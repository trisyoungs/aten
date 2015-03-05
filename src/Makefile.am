bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @GUI_CFLAGS@

SUBDIRS = gui templates base math model ff methods render command parser main

aten_LDADD = gui/libgui.a parser/libparser.a gui/libtreegui.a render/librender.a command/libcommand.a methods/libmethods.a model/libmodel.a math/libmath.a base/libsg.a main/libmain.a ff/libfourier.a ff/libff.a base/libbase.a @MAC_LIBS@ @GUI_LDLIBS@
