bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @ATEN_CFLAGS@

SUBDIRS = gui templates base sg math model ff methods render command parser main

aten_LDADD = gui/libgui.a parser/libparser.a gui/libtreegui.a command/libcommand.a methods/libmethods.a model/libmodel.a render/librender.a math/libmath.a main/libmain.a ff/libff.a base/libbase.a sg/libsg.a base/libfourierdata.a @ATEN_LDLIBS@
