bin_PROGRAMS = aten

aten_SOURCES = main.cpp

INCLUDES = -I../ @ATEN_CFLAGS@

SUBDIRS = gui templates base sg math model ff methods render command parser plugins main

aten_LDADD = base/libmessenger.a gui/libgui.a parser/libparser.a gui/libtreegui.a command/libcommand.a methods/libmethods.a model/libmodel.a render/librender.a math/libmath.a main/libmain.a ff/libff.a base/libbase.a sg/libsg.a base/libfourierdata.a plugins/interfaces/libinterfaces.a @ATEN_LDLIBS@
