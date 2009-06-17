noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = aten.cpp atom.cpp bond.cpp cell.cpp character.cpp commandnode.cpp double.cpp element.cpp filterdata.cpp forest.cpp forcefield.cpp forcefieldatom.cpp forcefieldbound.cpp format.cpp grammar.yy glyph.cpp grid.cpp integer.cpp measurement.cpp model.cpp parser.cpp parser_lexer.cpp parser_tree.cpp pattern.cpp patternbound.cpp prefs.cpp pvariable.cpp region.cpp returnvalue.cpp scopenode.cpp stepnode.cpp tree.cpp tree_opcheck.cpp treenode.cpp usercommandnode.cpp variable.cpp variablelist.cpp variablenode.cpp vector.cpp vtypes.cpp

noinst_HEADERS = accessor.h aten.h atom.h bond.h cell.h character.h commandnode.h double.h element.h filterdata.h forest.h forcefield.h forcefieldatom.h forcefieldbound.h format.h glyph.h grid.h integer.h measurement.h model.h parser.h pattern.h patternbound.h prefs.h pvariable.h region.h returnvalue.h scopenode.h stepnode.h tree.h treenode.h usercommandnode.h variable.h variablelist.h variablenode.h vector.h vtypes.h 

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
