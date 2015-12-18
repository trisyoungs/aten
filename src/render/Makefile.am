noinst_LIBRARIES = librender.a

AM_YFLAGS = -d

clean-local:
	-rm -f textprimitive_grammar.cc textprimitive_grammar.hh

librender_a_SOURCES = textprimitive_grammar.yy

librender_a_SOURCES += fontinstance.cpp linestipple.cpp linestyle.cpp primitive.cpp primitive_surface.cpp primitiveinstance.cpp primitiveset.cpp rendergroup.cpp rendergroup_glyph.cpp rendergroup_model.cpp rendergroup_overlays.cpp renderlist.cpp renderoccurrence.cpp renderoccurrencechunk.cpp textformat.cpp textfragment.cpp textprimitive.cpp textprimitivelist.cpp

noinst_HEADERS = fontinstance.h linestipple.h linestyle.h primitive.h primitiveinstance.h primitiveset.h rendergroup.h renderlist.h renderoccurrence.h renderoccurrencechunk.h textformat.h textfragment.h textprimitive.h textprimitivelist.h

librender_a_CPPFLAGS = -I$(top_srcdir)/src -I$(top_srcdir)/src/gui @ATEN_CFLAGS@

