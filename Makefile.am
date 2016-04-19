SUBDIRS = src

SUFFIXES: .dt

.dt.desktop:
	sed -e "s:PREFIX:@INSTALLDIR@:" -e "s:VERSION:@VERSION@:" $< > $@

EXTRA_DIST = src/gui/icons/*.* src/gui/freefont/*
EXTRA_DIST += data/ff/* data/fftesting/* data/filters/* data/fragments/*/* data/includes/* data/partitions/* data/test/* data/scripts/* data/external/*
EXTRA_DIST += TODO TODO2
EXTRA_DIST += CMakeLists.txt cmake/* src/CMakeLists.txt src/*/CMakeLists.txt
EXTRA_DIST += desktop.cmake
EXTRA_DIST += extra/* 

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

fragmentsroot = $(atendir)/fragments
fragments1dir = $(fragmentsroot)/alkyl
fragments1_DATA = data/fragments/alkyl/*
fragments2dir = $(fragmentsroot)/aromatics
fragments2_DATA = data/fragments/aromatics/*
fragments3dir = $(fragmentsroot)/il-anions
fragments3_DATA = data/fragments/il-anions/*
fragments4dir = $(fragmentsroot)/il-cations
fragments4_DATA = data/fragments/il-cations/*
fragments5dir = $(fragmentsroot)/rings
fragments5_DATA = data/fragments/rings/*

desktopdir = @BUILDDIR@/usr/share/applications
desktop_DATA = extra/Aten.desktop

icondir = @BUILDDIR@/usr/share/pixmaps
icon_DATA = extra/aten.png

