noinst_LIBRARIES = libparser.a

BUILT_SOURCES = grammar.h
AM_YFLAGS = -d

libparser_a_SOURCES = aten.cpp atom.cpp basisprimitive.cpp basisshell.cpp bond.cpp cell.cpp character.cpp colourscale.cpp colourscalepoint.cpp commandnode.cpp double.cpp eigenvector.cpp element.cpp energystore.cpp filterdata.cpp forcefield.cpp forcefieldatom.cpp forcefieldbound.cpp format.cpp grammar.yy glyph.cpp glyphdata.cpp grid.cpp integer.cpp mc.cpp measurement.cpp model.cpp parser.cpp parser_lexer.cpp pattern.cpp patternbound.cpp prefs.cpp program.cpp pvariable.cpp returnvalue.cpp scopenode.cpp site.cpp stepnode.cpp tree.cpp tree_opcheck.cpp treenode.cpp usercommandnode.cpp variable.cpp variablelist.cpp variablenode.cpp vector.cpp vibration.cpp vtypes.cpp widgetnode.cpp zmatrix.cpp zmatrixelement.cpp

noinst_HEADERS = accessor.h aten.h atom.h basisprimitive.h basisshell.h bond.h cell.h character.h colourscale.h colourscalepoint.h  commandnode.h double.h eigenvector.h element.h energystore.h filterdata.h forcefield.h forcefieldatom.h forcefieldbound.h format.h glyph.h glyphdata.h grid.h integer.h mc.h measurement.h model.h parser.h pattern.h patternbound.h prefs.h program.h pvariable.h returnvalue.h scopenode.h site.h stepnode.h tree.h treenode.h usercommandnode.h variable.h variablelist.h variablenode.h vector.h vibration.h vtypes.h widgetnode.h zmatrix.h zmatrixelement.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
