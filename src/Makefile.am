bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @ATEN_CFLAGS@

SUBDIRS = gui templates base math model ff methods render command parser main

aten_LDADD = gui/libgui.a parser/libparser.a gui/libtreegui.a command/libcommand.a methods/libmethods.a render/librender.a model/libmodel.a math/libmath.a main/libmain.a ff/libfourier.a ff/libff.a base/libbase.a base/libsg.a @ATEN_LDLIBS@
