# Set plugin installation dir and define plugin targets
pluginexecdir = @ATEN_PLUGINLIBDIR@
pluginexec_LTLIBRARIES = ringstool.la

SUFFIXES: .ui .hui .qrc

.ui.lo:
	$(QTUIC) -o ui_$*.h $<
	$(QTMOC) -o moc_$*.cpp $*.h
	${LIBTOOL} --tag=CXX --mode=compile ${CXX} -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c moc_$*.cpp -o $@
	rm moc_$*.cpp

.hui.lo:
	${QTMOC} -o $*.cpp -I../../ @ATEN_INCLUDES@ $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c $*.cpp -o $@
	rm $*.cpp

.qrc.lo:
	$(QTRCC) -o $*.cpp -name $* $<
	${LIBTOOL} --tag=CXX --mode=compile $(CXX) -I$(top_srcdir)/src -I../ -I./ ${AM_CPPFLAGS} -c -o $@ $*.cpp
	rm $*.cpp

# Local clean (temporary files generated from rules)
clean-local:
	-rm -f ringstool.cpp ringstool_icons.cpp

# XYZ Model Plugin
ringstool_la_SOURCES = ringstool_icons.qrc ringstooldialog.ui ringstooldialog_funcs.cpp ringstool_funcs.cpp ringstool.hui
#ringstool_la_LIBADD = ../../gui/qcustomplot/libqcustomplot.la
ringstool_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@

noinst_HEADERS = ringstooldialog.h

