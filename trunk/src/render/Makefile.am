noinst_LIBRARIES = librender.a

AM_YFLAGS = -d

clean-local:
	-rm -f textprimitive_grammar.cc

librender_a_SOURCES = textprimitive_grammar.yy
librender_a_SOURCES += fontinstance.cpp glextensions.cpp linestipple.cpp linestyle.cpp primitive.cpp primitiveinfo.cpp primitiveinstance.cpp primitivelist.cpp renderprimitives.cpp textformat.cpp textfragment.cpp textprimitive.cpp textprimitivelist.cpp vertexchunk.cpp

noinst_HEADERS = fontinstance.h glextensions.h linestipple.h linestyle.h primitive.h primitiveinfo.h primitiveinstance.h primitivelist.h renderprimitives.h textformat.h textfragment.h textprimitive.h textprimitivelist.h vertexchunk.h

librender_a_CPPFLAGS = -I$(top_srcdir)/src -I$(top_srcdir)/src/gui @GUI_CFLAGS@

