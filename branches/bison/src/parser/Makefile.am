noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = commandnode.cpp commands.cpp functions.cpp grammar.yy integer.cpp lexer.cpp returnvalue.cpp tree.cpp treenode.cpp variable.cpp

noinst_HEADERS = commandnode.h commands.h integer.h returnvalue.h tree.h treenode.h variable.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
