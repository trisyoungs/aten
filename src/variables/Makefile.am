noinst_LIBRARIES = libvariables.a libtypes.a

libvariables_a_SOURCES = expression.cpp expressionnode.cpp variable.cpp variablelist.cpp

libtypes_a_SOURCES = character.cpp double.cpp integer.cpp

noinst_HEADERS = expression.h expressionnode.h variable.h variablelist.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
