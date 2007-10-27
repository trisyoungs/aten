#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.60)

# Set program name, version, bug-address and source directory
m4_define([ATEN_VERSION],[0.91])
AC_INIT(aten,ATEN_VERSION,t.youngs@qub.ac.uk)
AC_CONFIG_SRCDIR([src/main.cpp])

# Check system type and set defines
# Specify a single file in the source tree so we can check that --srcdir points to the right place
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADER(config.h)
AC_CANONICAL_TARGET
MAC_LIBS=""
if test "$target_vendor" = "apple"; then
  AC_DEFINE([IS_MAC], [], [Whether operating system is an Apple])
  MAC_LIBS="-framework OpenGL -framework GLUT"
fi
AC_SUBST(MAC_LIBS)

AM_INIT_AUTOMAKE(aten,ATEN_VERSION)

# Checks for programs.
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LIBTOOL


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([string.h])
#  --- doesn't work on the Mac since they're in a 'nonstandard' path
if test "$target_vendor" != "apple"; then
  AC_CHECK_HEADERS([GL/glut.h], ,[AC_MSG_ERROR([Could not find GLUT header. Is freeglut/Mesa installed?])])
else
  AC_CHECK_HEADERS([GLUT/glut.h], ,[AC_MSG_ERROR([Could not find GLUT header. Is the OpenGL/GLUT framework installed?])])
fi


# Checks for GL, GLU, and GLUT headers
# Provide --with options to specify the include locations
GL_CFLAGS=""
AC_ARG_WITH([gl-includedir],[AC_HELP_STRING([--with-gl-includedir=DIR],
    [Set location of OpenGL and GLUT headers to DIR [default=auto]])], ,
    [with_gl_includedir=auto])
if test "x$with_gl_includedir" != "xauto"; then
  GL_CFLAGS="-I$with_gl_includedir"
fi


# Check for GUI to compile and set up link / compile flags
GUI_DIRS=""
GUI_OBJECTS=""
AC_ARG_WITH([gui],[AC_HELP_STRING([--with-gui=gtk|qt|none],
    [Sets GUI to compile with program (GTK+, Qt4, or none) [default=qt]])], ,
    [with_gui=qt])


# Allow Qt bin directory to be specified (to avoid version conflicts for moc, rcc, and uic)
QTDIR=""
AC_ARG_WITH([qtdir],[AC_HELP_STRING([--with-qtdir=<dir>],
    [Specify location of Qt4 developer tools (moc, uic, and rcc) [default=""]])], ,
    [with_qtdir=])
if test "x$with_qtdir" != "x"; then
  QTDIR="$with_qtdir/"
fi


# Add in Apple configuration arguments
AC_ARG_WITH([qt],[AC_HELP_STRING([--with-qt=framework|<blank>],
    [Configures the build to use OS X Qt frameworks instead of pkg-config[default=]])], ,
    [with_qt=])


# Pre-initialise pkg-config since we don't know where the first PKG_CHECK_MODULES will occur
PKG_PROG_PKG_CONFIG


# Set automake conditionals to false by default
AM_CONDITIONAL(MAKEGTKGUI, false)
AM_CONDITIONAL(MAKEQTGUI, false)
AM_CONDITIONAL(MAKERENDER, false)

if test "$with_gui" = "gtk"; then
  AM_CONDITIONAL(MAKEGTKGUI, true)
  AM_CONDITIONAL(MAKERENDER, true)
  GUI_OBJECTS="gui-gtk/libguigtk.a gui/libguimaster.a render/librender.a"
  AC_DEFINE([WITH_GTKGUI], [], [Compile with the GTK+ GUI interface])
  AC_DEFINE([HAS_GUI], [], [Whether a GUI has been specified])
  PKG_CHECK_MODULES(GTK24, gtk+-2.0 >= 2.4.0)
  PKG_CHECK_MODULES(GTK28, gtk+-2.0 >= 2.8.0,[AC_DEFINE([HAVE_GTK_NEW],[],[Version of gtk+-2.0 >= 2.8])],[])
  PKG_CHECK_MODULES(GDK, gdk-2.0 >= 2.0.0)
  PKG_CHECK_MODULES(PANGO, pango >= 1.0.0)
  PKG_CHECK_MODULES(GTKGLEXT, gtkglext-1.0 >= 1.0.0)
  PKG_CHECK_MODULES(GTHREAD, gthread-2.0 >= 2.0.0)
  GUI_LDLIBS="$GTKGLEXT_LIBS $GTHREAD_LIBS -lglut"
  GUI_CFLAGS="$GTKGLEXT_CFLAGS"
fi

if test "$with_gui" = "qt"; then
  AM_CONDITIONAL(MAKEQTGUI, true)
  AM_CONDITIONAL(MAKERENDER, true)
  GUI_OBJECTS="gui-qt/libguiqt.a gui/libguimaster.a render/librender.a"
  AC_DEFINE([WITH_QTGUI], [], [Compile with the Qt4 GUI interface])
  AC_DEFINE([HAS_GUI], [], [Whether a GUI has been specified])
  if test "x$with_qt" = "xframework"; then
    QTGUI_LIBS="-framework QtGui -framework QtOpenGL -framework QtCore"
    QTGUI_CFLAGS="-F QtGui -F QtOpenGl -F QtCore"
  else
    PKG_CHECK_MODULES(QTGUI, QtGui >= 4.1.0)
    PKG_CHECK_MODULES(QTOPENGL, QtOpenGL >= 4.1.0)
    QTGUI_LIBS="$QTGUI_LIBS -lglut"
  fi
  GUI_LDLIBS="$QTGUI_LIBS $QTOPENGL_LIBS"
  GUI_CFLAGS="$QTGUI_CFLAGS"
fi

if test "$with_gui" = "none"; then
  GUI_OBJECTS="gui/libguimaster.a"
fi


AC_SUBST(GUI_OBJECTS)
AC_SUBST(GUI_LDLIBS)
AC_SUBST(GUI_CFLAGS)
AC_SUBST(QTDIR)


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_SELECT_ARGTYPES
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([floor pow select sqrt strchr strrchr strstr])

# Define AC_OUTPUT 
AC_OUTPUT(Makefile
src/Makefile
src/base/Makefile
src/classes/Makefile
src/energy/Makefile
src/file/Makefile
src/gui/Makefile
src/gui-gtk/Makefile
src/gui-qt/Makefile
src/methods/Makefile
src/model/Makefile
src/render/Makefile
src/script/Makefile
src/templates/Makefile
)
