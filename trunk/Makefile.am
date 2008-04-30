SUBDIRS = src

SUFFIXES: .dt

.dt.desktop:
	sed -e "s:PREFIX:${prefix}:" -e "s:VERSION:${ATEN_VERSION}:" $< > $@

EXTRA_DIST = scripts/*.txt doc/manual.pdf src/gui/icons/*.* data/filters/* data/ff/* data/test/* TODO aten.spec CMakeLists.txt cmake_uninstall.cmake.in NSIS.template.in FindREADLINE.cmake src/CMakeLists.txt src/*/CMakeLists.txt desktop.cmake Aten.dt

atendir = $(datadir)/aten
aten_DATA = src/gui/icons/aten.png

filterdir = $(atendir)/filters
filter_DATA = data/filters/*

ffdir = $(atendir)/ff
ff_DATA = data/ff/*

testdir = $(atendir)/test
test_DATA = data/test/*

desktopdir = /usr/share/applications
desktop_DATA = Aten.desktop

