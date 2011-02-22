noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = commandnode.cpp double.cpp grammar.yy integer.cpp parser.cpp parser_lexer.cpp parser_tree.cpp returnvalue.cpp scopenode.cpp stepnode.cpp tree.cpp tree_opcheck.cpp treenode.cpp usercommandnode.cpp variable.cpp variablelist.cpp variablenode.cpp vector.cpp vtypes.cpp

noinst_HEADERS = accessor.h commandnode.h double.h integer.h parser.h returnvalue.h scopenode.h stepnode.h tree.h treenode.h usercommandnode.h variable.h variablelist.h variablenode.h vector.h vtypes.h 

INCLUDES = -I$(top_srcdir)/src -I../ 
