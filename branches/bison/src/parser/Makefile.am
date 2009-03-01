noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = character.cpp commandnode.cpp commands.cpp functions.cpp grammar.yy integer.cpp lexer.cpp real.cpp returnvalue.cpp scopenode.cpp testfuncs.cpp tree.cpp treenode.cpp variable.cpp variablelist.cpp variablenode.cpp vtypes.cpp

noinst_HEADERS = character.h commandnode.h commands.h integer.h real.h returnvalue.h scopenode.h tree.h treenode.h variable.h variablelist.h variablenode.h vtypes.h 

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
