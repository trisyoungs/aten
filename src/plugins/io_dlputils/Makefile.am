# Set plugin installation dir and define plugin targets
pluginexecdir = ${pkglibdir}/plugins
pluginexec_LTLIBRARIES = dlputilspdens.la dlputilssurface.la

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
	-rm -f pdens.cpp surface.cpp

# PDens Grid Plugin
dlputilspdens_la_SOURCES = pdens_funcs.cpp pdens.hui 
dlputilspdens_la_LDFLAGS = -module -shared -avoid-version

# Surface Grid Plugin
dlputilssurface_la_SOURCES = surface_funcs.cpp surface.hui
dlputilssurface_la_LDFLAGS = -module -shared -avoid-version

AM_CPPFLAGS = -I${top_srcdir}/src @ATEN_INCLUDES@ @ATEN_CFLAGS@
