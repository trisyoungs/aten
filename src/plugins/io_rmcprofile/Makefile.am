# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = rmcp3.la rmcp6.la rmcp6bonds.la

# Rules
.ui.lo:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $*.h
	${LIBTOOL} --tag=CXX --mode=compile ${CXX} -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c moc_$*.cpp -o $@
	rm moc_$*.cpp

.hui.lo:
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

# Local clean (temporary files generated from rules)
clean-local:
	-rm -f rmcp3.cpp rmcp6.cpp

# Common functions to all RMCProfile plugins in this dir
noinst_LTLIBRARIES = libcommon.la
libcommon_la_SOURCES = common.cpp

# RMCProfile V3 Model Plugin
rmcp3_la_SOURCES = rmcp3_funcs.cpp rmcp3.hui
rmcp3_la_LIBADD = libcommon.la
rmcp3_la_LDFLAGS = -module -shared -avoid-version

# RMCProfile V6 Model Plugin
rmcp6_la_SOURCES = rmcp6_funcs.cpp rmcp6.hui # rmcp6importoptions.ui rmcp6importoptions_funcs.cpp rmcp6exportoptions.ui rmcp6exportoptions_funcs.cpp
rmcp6_la_LIBADD = libcommon.la
rmcp6_la_LDFLAGS = -module -shared -avoid-version

# RMCProfile V6 Bonds File Plugin
rmcp6bonds_la_SOURCES = rmcp6bonds_funcs.cpp rmcp6bonds.hui
rmcp6bonds_la_LIBADD = libcommon.la
rmcp6bonds_la_LDFLAGS = -module -shared -avoid-version

# Headers not for installation, but which should be included when running 'make dist'
noinst_HEADERS = common.h # rmcp6importoptions.h rmcp6exportoptions.h

# General CPPFLAGS for compilation
AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
