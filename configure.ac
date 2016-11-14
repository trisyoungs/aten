#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.60)


# Set program name, version and source directory
m4_define([ATEN_VERSION],[2.1.5])
AC_INIT([aten], ATEN_VERSION)
AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_TARGET
AC_CONFIG_SRCDIR([src/main.cpp])
AC_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE


# Checks for programs.
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LIBTOOL
AC_PROG_YACC


#
# Additional Configuration Options
#

# QT Tools
AC_ARG_WITH([qtmoc],[AC_HELP_STRING([--with-qtmoc=<executable>],
    [Specify Qt5 Meta-Object Compiler tool (moc) [default="moc-qt5"]])], ,
    [with_qtmoc="moc-qt5"])
AC_ARG_WITH([qtuic],[AC_HELP_STRING([--with-qtuic=<executable>],
    [Specify Qt5 User Interface Compiler tool (uic) [default="uic-qt5"]])], ,
    [with_qtuic="uic-qt5"])
AC_ARG_WITH([qtrcc],[AC_HELP_STRING([--with-qtrcc=<executable],
    [Specify Qt5 Resource Compiler tool (rcc) [default="rcc-qt5"]])], ,
    [with_qtrcc="rcc-qt5"])
QTMOC="$with_qtmoc"
QTRCC="$with_qtrcc"
QTUIC="$with_qtuic"

# Checks?
CHECK_CFLAGS=""
AC_ARG_WITH([checks], [AC_HELP_STRING([--with-checks], [Enable internal code array/pointer checking [default="no"]])], [with_checks="yes"], [with_checks="no"])
if test "$with_checks" = "yes"; then
  CHECK_CFLAGS="-DCHECKS"
fi

# Local deployment of plugins (to data/plugins)
AC_ARG_WITH([localdeploy], [AC_HELP_STRING([--with-localdeploy], [Enable local deployment of plugins [default="no"]])], [with_localdeploy="yes"], [with_localdeploy="no"])
AM_CONDITIONAL([LOCALDEPLOY], [test "$with_localdeploy" = "yes"])

# Turn off use of -rdynamic at link stage (occasionally required on OSX?)
AC_ARG_WITH([nordynamic], [AC_HELP_STRING([--with-nordynamic], [Disable passing of -rdynamic at link time [default="no"]])], [with_nordynamic="yes"], [with_nordynamic="no"])
AM_CONDITIONAL([NORDYNAMIC], [test "$with_nordynamic" = "yes"])

# Turn on compilation of MOPAC7 plugin (which requires a Fortran compiler)
AC_ARG_WITH([mopacplugin], [AC_HELP_STRING([--with-mopacplugin], [Enable compilation of MOPAC7 plugin [default="no"]])], [with_mopacplugin="yes"], [with_mopacplugin="no"])
AM_CONDITIONAL([MOPACPLUGIN], [test "$with_mopacplugin" = "yes"])
if test "$with_mopacplugin" = "yes"; then
  AC_PROG_FC
fi

# Readline include dir
RLINCDIR=""
AC_ARG_WITH([readline-includes],[AC_HELP_STRING([--with-readline-includes=<dir>],
    [Specify location of readline includes [default="/usr/include/readline"]])], ,
    [with_readline_includes="/usr/include/readline"])
RLINCDIR="$with_readline_includes"

# Build Directory (used when packaging)
BUILDDIR=""
AC_ARG_WITH([build-dir],[AC_HELP_STRING([--with-build-dir=<dir>],
    [Specify location of build environment when packaging [default=""]])], ,
    [with_build_dir=""])
BUILDDIR="$with_build_dir"

# Install Directory (used when packaging - should be '$PREFIX - $BUILDDIR')
INSTALLDIR="/usr/local"
AC_ARG_WITH([install-dir],[AC_HELP_STRING([--with-install-dir=<dir>],
    [Specify 'clean' install dir prefix, i.e. without any BUILDDIR [default="/usr/local"]])], ,
    [with_install_dir="/usr/local"])
# If --prefix has been give, use this value by default
if test "$prefix" != "NONE"; then
  INSTALLDIR="$prefix"
fi

# Plugin Install Directory (libdir)
ATEN_PLUGINLIBDIR="${libdir}/aten/plugins"

# Freetype and FTGL
PKG_CHECK_MODULES(FREETYPE, freetype2 >= 2.0)
PKG_CHECK_MODULES(FTGL, ftgl >= 2.0)

# QT-FRAMEWORK  (Mac only)
AC_ARG_WITH([qt],[AC_HELP_STRING([--with-qt=framework|fink|<none>],
    [Determines which installation of Qt5 to use on OS X (should not be set for Linux)[default=<none>]])], ,
    [with_qt=])

# Pre-initialise pkg-config  (unless using Qt framework on the Mac)
if test "$with_qt" != "framework"; then
  PKG_PROG_PKG_CONFIG
fi

# Check for other dependencies
AC_CHECK_HEADER([$RLINCDIR/readline.h],,[AC_MSG_ERROR([Cannot find headers for readline.])])

# Set up compilation for Qt GUI
if test "$with_qt" = "framework"; then
  QTGUI_LIBS="-framework QtGui -framework QtOpenGL -framework QtCore -framework OpenGL"
  QTGUI_CFLAGS="-F QtGui -F QtOpenGl -F QtCore"
fi
if test "x$with_qt" = "x"; then
  PKG_CHECK_MODULES(QTGUI, Qt5Gui >= 5.0.0)
  PKG_CHECK_MODULES(QTOPENGL, Qt5OpenGL >= 5.0.0)
  PKG_CHECK_MODULES(QTWIDGETS, Qt5Widgets >= 5.0.0)
fi

# Finalise substitution strings
ATEN_LDLIBS="$FTGL_LIBS $FREETYPE_LIBS $QTGUI_LIBS $QTOPENGL_LIBS -lreadline -lGL -lfreetype"
ATEN_CFLAGS="-fPIC $CHECK_CFLAGS $CXXFLAGS"
ATEN_LDFLAGS="$LDFLAGS"
if test "$with_nordynamic" = "no"; then
  ATEN_LDFLAGS+=" -rdynamic "
fi
ATEN_INCLUDES=" $FREETYPE_CFLAGS $QTGUI_CFLAGS $QTWIDGETS_CFLAGS -I$RLINCDIR"

# Perform Makefile substitutions
AC_SUBST(ATEN_LDLIBS)
AC_SUBST(ATEN_LDFLAGS)
AC_SUBST(ATEN_PLUGINLIBDIR)
AC_SUBST(ATEN_CFLAGS)
AC_SUBST(ATEN_INCLUDES)
AC_SUBST(QTMOC)
AC_SUBST(QTUIC)
AC_SUBST(QTRCC)
AC_SUBST(BUILDDIR)
AC_SUBST(INSTALLDIR)


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
src/command/Makefile
src/ff/Makefile
src/gui/Makefile
src/main/Makefile
src/math/Makefile
src/methods/Makefile
src/parser/Makefile
src/model/Makefile
src/render/Makefile
src/undo/Makefile
src/plugins/Makefile
src/plugins/interfaces/Makefile
src/plugins/io_akf/Makefile
src/plugins/io_cif/Makefile
src/plugins/io_cube/Makefile
src/plugins/io_dlpoly/Makefile
src/plugins/io_dlputils/Makefile
src/plugins/io_epsr/Makefile
src/plugins/io_ff/Makefile
src/plugins/io_gamessus/Makefile
src/plugins/io_gromacs/Makefile
src/plugins/io_mdlmol/Makefile
src/plugins/io_mopac/Makefile
src/plugins/io_msi/Makefile
src/plugins/io_pdb/Makefile
src/plugins/io_rmcprofile/Makefile
src/plugins/io_sybylmol2/Makefile
src/plugins/io_vfield/Makefile
src/plugins/io_xyz/Makefile
src/plugins/method_mopac71/Makefile
src/plugins/method_mopac71/mopac7.1/Makefile
src/sg/Makefile
src/templates/Makefile
)
#src/plugins/io_csd/Makefile
#src/plugins/io_espresso/Makefile
#src/plugins/io_gaussian/Makefile
#src/plugins/io_siesta/Makefile
