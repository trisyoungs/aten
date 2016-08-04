# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = dlp2.la dlp4.la dlpfield.la dlp2history.la dlp4history.la

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
	-rm -f dlp2.cpp dlp4.cpp field.cpp history.cpp

# Common functions to plugins
noinst_LTLIBRARIES = libcommon.la
libcommon_la_SOURCES = common.cpp

# DL_POLY 2 CONFIG Plugin
dlp2_la_SOURCES = dlp2exportoptions.ui dlp2exportoptions_funcs.cpp dlp2importoptions.ui dlp2importoptions_funcs.cpp
dlp2_la_SOURCES += dlp2_funcs.cpp dlp2.hui
dlp2_la_LIBADD = libcommon.la
dlp2_la_LDFLAGS = -module -shared -avoid-version

# DL_POLY 4 CONFIG Plugin
dlp4_la_SOURCES = dlp4exportoptions.ui dlp4exportoptions_funcs.cpp dlp4importoptions.ui dlp4importoptions_funcs.cpp
dlp4_la_SOURCES += dlp4_funcs.cpp dlp4.hui
dlp4_la_LIBADD = libcommon.la
dlp4_la_LDFLAGS = -module -shared -avoid-version

# DL_POLY FIELD Plugin
#dlpfield_la_SOURCES = fieldimportoptions_funcs.cpp fieldimportoptions.ui
dlpfield_la_SOURCES = field_funcs.cpp field.hui
dlpfield_la_LIBADD = libcommon.la
dlpfield_la_LDFLAGS = -module -shared -avoid-version

# DL_POLY 2 History Plugin
dlp2history_la_SOURCES = dlp2importoptions.ui dlp2importoptions_funcs.cpp
dlp2history_la_SOURCES += history2_funcs.cpp history2.hui
dlp2history_la_LIBADD = libcommon.la
dlp2history_la_LDFLAGS = -module -shared -avoid-version

# DL_POLY 4 History Plugin
dlp4history_la_SOURCES = dlp4importoptions.ui dlp4importoptions_funcs.cpp
dlp4history_la_SOURCES += history4_funcs.cpp history4.hui
dlp4history_la_LIBADD = libcommon.la
dlp4history_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

noinst_HEADERS = common.h dlp2exportoptions.h dlp2importoptions.h dlp4exportoptions.h dlp4importoptions.h

