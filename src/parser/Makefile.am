noinst_LIBRARIES = libparser.a

AM_YFLAGS = -d

libparser_a_SOURCES = aten.cpp atom.cpp basisprimitive.cpp basisshell.cpp bond.cpp cell.cpp character.cpp colourscale.cpp colourscalepoint.cpp commandnode.cpp dialog.cpp double.cpp eigenvector.cpp element.cpp energystore.cpp filterdata.cpp forcefield.cpp forcefieldatom.cpp forcefieldbound.cpp format.cpp grammar.yy glyph.cpp glyphdata.cpp grid.cpp integer.cpp matrix.cpp mc.cpp measurement.cpp model.cpp newnode.cpp parser.cpp parser_lexer.cpp pattern.cpp patternbound.cpp prefs.cpp program.cpp pvariable.cpp returnvalue.cpp scopenode.cpp site.cpp stepnode.cpp tree.cpp tree_opcheck.cpp treegui.cpp treenode.cpp usercommandnode.cpp variable.cpp variablelist.cpp variablenode.cpp vector.cpp vibration.cpp vtypes.cpp widget.cpp zmatrix.cpp zmatrixelement.cpp

noinst_HEADERS = accessor.h aten.h atom.h basisprimitive.h basisshell.h bond.h cell.h character.h colourscale.h colourscalepoint.h commandnode.h dialog.h double.h eigenvector.h element.h energystore.h filterdata.h forcefield.h forcefieldatom.h forcefieldbound.h format.h glyph.h glyphdata.h grid.h integer.h matrix.h mc.h measurement.h model.h newnode.h parser.h pattern.h patternbound.h prefs.h program.h pvariable.h returnvalue.h scopenode.h site.h stepnode.h tree.h treegui.h treenode.h usercommandnode.h variable.h variablelist.h variablenode.h vector.h vibration.h vtypes.h widget.h zmatrix.h zmatrixelement.h

CLEANFILES = grammar.hh grammar.cc

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @ATEN_CFLAGS@
