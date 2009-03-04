noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = accessnode.cpp atom.cpp character.cpp commandnode.cpp grammar.yy integer.cpp lexer.cpp model.cpp pathnode.cpp real.cpp returnvalue.cpp scopenode.cpp tree.cpp treenode.cpp variable.cpp variablelist.cpp variablenode.cpp vector.cpp vtypes.cpp

noinst_HEADERS = accessnode.h accessor.h atom.h character.h commandnode.h integer.h model.h pathnode.h real.h returnvalue.h scopenode.h tree.h treenode.h variable.h variablelist.h variablenode.h vector.h vtypes.h 

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
