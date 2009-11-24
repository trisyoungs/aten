SUBDIRS = src

SUFFIXES: .dt

.dt.desktop:
	sed -e "s:PREFIX:@INSTALLDIR@:" -e "s:VERSION:@VERSION@:" $< > $@

EXTRA_DIST = scripts/*.txt src/gui/icons/*.* data/ff/* data/filters/* data/test/* TODO aten.spec CMakeLists.txt cmake_uninstall.cmake.in NSIS.template.in FindREADLINE.cmake src/CMakeLists.txt src/*/CMakeLists.txt desktop.cmake Aten.dt extra/*

atendir = $(datadir)/aten
aten_DATA = src/gui/icons/aten.png

filterdir = $(atendir)/filters
filter_DATA = data/filters/*

testdir = $(atendir)/test
test_DATA = data/test/*

ffdir = $(atendir)/ff
ff_DATA = data/ff/*

desktopdir = @BUILDDIR@/usr/share/applications
desktop_DATA = Aten.desktop

