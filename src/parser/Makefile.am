noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = aten.cpp atom.cpp bond.cpp cell.cpp character.cpp commandnode.cpp elements.cpp forcefield.cpp forcefieldatom.cpp forcefieldbound.cpp grammar.yy integer.cpp lexer.cpp model.cpp real.cpp returnvalue.cpp scopenode.cpp stepnode.cpp tree.cpp treenode.cpp variable.cpp variablelist.cpp variablenode.cpp vector.cpp vtypes.cpp

noinst_HEADERS = accessor.h aten.h atom.h bond.h cell.h character.h commandnode.h elements.h forcefield.h forcefieldatom.h forcefieldbound.h integer.h model.h real.h returnvalue.h scopenode.h stepnode.h tree.h treenode.h variable.h variablelist.h variablenode.h vector.h vtypes.h 

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
