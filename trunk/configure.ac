#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.60)

# Set program name, version, bug-address and source directory
m4_define([ATEN_VERSION],[0.95])
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
  MAC_LIBS="-framework OpenGL"
fi
AC_SUBST(MAC_LIBS)

AM_INIT_AUTOMAKE(aten,ATEN_VERSION)

# Checks for programs.
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LIBTOOL


#
# Additional Configuration Options
#

# MEMDEBUG
AC_ARG_WITH([memdebug],[AC_HELP_STRING([--with-memdebug],
    [Enable manual constructor / destructor counts.])], ,
    [memdebug=])
if test "x$with_memdebug" != "x"; then
  AC_DEFINE([MEMDEBUG], [], [Enable manual constructor / destructor counts.])
fi

# SPEEDTEST
AC_ARG_WITH([speedtest],[AC_HELP_STRING([--with-speedtest],
    [Enable rendering speedtest (warning - makes compiled code otherwise unusable).])], ,
    [speedtest=])
if test "x$with_speedtest" != "x"; then
  AC_DEFINE([SPEEDTEST], [], [Enable rendering speedtest (warning - makes compiled code otherwise unusable).])
fi


# QTDIR
QTDIR=""
AC_ARG_WITH([qtdir],[AC_HELP_STRING([--with-qtdir=<dir>],
    [Specify location of Qt4 developer tools (moc, uic, and rcc) [default=""]])], ,
    [with_qtdir=])
if test "x$with_qtdir" != "x"; then
  QTDIR="$with_qtdir/"
fi

# QT-FRAMEWORK  (Mac only)
AC_ARG_WITH([qt],[AC_HELP_STRING([--with-qt=framework|fink|<none>],
    [Determines which installation of Qt4 to use on OS X (should not be set for Linux)[default=<none>]])], ,
    [with_qt=])


# Pre-initialise pkg-config  (unless using Qt framework on the Mac)
if test "$with_qt" != "framework"; then
  PKG_PROG_PKG_CONFIG
fi


# Set up compilation for Qt GUI
#AC_DEFINE([WITH_QTGUI], [], [Compile with the Qt4 GUI interface])
AC_DEFINE([HAS_GUI], [], [Whether a GUI has been specified])
if test "$with_qt" = "framework"; then
  QTGUI_LIBS="-framework QtGui -framework QtOpenGL -framework QtCore"
  QTGUI_CFLAGS="-F QtGui -F QtOpenGl -F QtCore"
fi
if test "$with_qt" = "fink"; then
  QTGUI_LIBS="-L/sw/lib/qt4-x11/lib -lQtGui -lQtCore -lQtOpenGL"
  QTGUI_CFLAGS="-I/sw/lib/qt4-x11/include -I/usr/X11R6/include"
  QTOPENGL_CFLAGS="-I/sw/lib/qt4-x11/include"
  QTOPENGL_LIBS="-I/sw/lib/qt4-x11/lib"
  PKG_CHECK_MODULES(QTGUI, QtGui >= 4.1.0)
  PKG_CHECK_MODULES(QTOPENGL, QtOpenGL >= 4.1.0)
fi
if test "x$with_qt" = "x"; then
  PKG_CHECK_MODULES(QTGUI, QtGui >= 4.1.0)
  PKG_CHECK_MODULES(QTOPENGL, QtOpenGL >= 4.1.0)
fi
GUI_LDLIBS="$QTGUI_LIBS $QTOPENGL_LIBS"
GUI_CFLAGS="$QTGUI_CFLAGS"


# Perform Makefile substitutions
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


# Define Makefiles in AC_OUTPUT 
AC_OUTPUT(Makefile
src/Makefile
src/base/Makefile
src/classes/Makefile
src/command/Makefile
src/energy/Makefile
src/gui/Makefile
src/methods/Makefile
src/model/Makefile
src/parse/Makefile
src/render/Makefile
src/templates/Makefile
)
