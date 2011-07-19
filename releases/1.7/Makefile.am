SUBDIRS = src

SUFFIXES: .dt

.dt.desktop:
	sed -e "s:PREFIX:@INSTALLDIR@:" -e "s:VERSION:@VERSION@:" $< > $@

EXTRA_DIST = src/gui/icons/*.* data/ff/* data/fftesting/* data/filters/* data/includes/* data/partitions/* data/test/* data/scripts/* TODO aten.spec CMakeLists.txt cmake_uninstall.cmake.in NSIS.template.in FindREADLINE.cmake src/CMakeLists.txt src/*/CMakeLists.txt desktop.cmake Aten.dt extra/* data/fragments/*/*.akf

atendir = $(datadir)/aten
aten_DATA = src/gui/icons/aten.png

filterdir = $(atendir)/filters
filter_DATA = data/filters/*

includesdir = $(atendir)/includes
includes_DATA = data/includes/*

testdir = $(atendir)/test
test_DATA = data/test/*

ffdir = $(atendir)/ff
ff_DATA = data/ff/*

fftestingdir = $(atendir)/fftesting
fftesting_DATA = data/fftesting/*

scriptsdir = $(atendir)/scripts
scripts_DATA = data/scripts/*

partitionsdir = $(atendir)/partitions
partitions_DATA = data/partitions/*

desktopdir = @BUILDDIR@/usr/share/applications
desktop_DATA = Aten.desktop

install-data-hook:
	mkdir $(DESTDIR)$(atendir)/fragments
	cp -r data/fragments/* $(DESTDIR)$(atendir)/fragments/

uninstall-hook:
	rm -rf $(atendir)/fragments
