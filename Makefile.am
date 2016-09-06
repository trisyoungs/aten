SUBDIRS = src

SUFFIXES: .dt

.dt.desktop:
	sed -e "s:PREFIX:@INSTALLDIR@:" -e "s:VERSION:@VERSION@:" $< > $@

EXTRA_DIST = src/gui/icons/*.* src/gui/freefont/*
EXTRA_DIST += data/ff/* data/fftesting/* data/fragments/*/* data/partitions/* data/test/*/* data/scripts/* data/external/*
EXTRA_DIST += TODO TODO2
EXTRA_DIST += CMakeLists.txt cmake/* src/CMakeLists.txt src/*/CMakeLists.txt
EXTRA_DIST += desktop.cmake
EXTRA_DIST += extra/* 
#EXTRA_DIST += data/includes/*

atendir = $(datadir)/aten
aten_DATA = src/gui/icons/aten.png

#includesdir = $(atendir)/includes
#includes_DATA = data/includes/*

testroot = $(atendir)/test
test01dir = $(testroot)/build
test01_DATA = data/test/build/*
test02dir = $(testroot)/ff
test02_DATA = data/test/ff/*
test03dir = $(testroot)/forces
test03_DATA = data/test/forces/*
test04dir = $(testroot)/io_cif
test04_DATA = data/test/io_cif/*
test05dir = $(testroot)/io_csd
test05_DATA = data/test/io_csd/*
test06dir = $(testroot)/io_dlpoly
test06_DATA = data/test/io_dlpoly/*
test07dir = $(testroot)/io_dlputils
test07_DATA = data/test/io_dlputils/*
test08dir = $(testroot)/io_epsr
test08_DATA = data/test/io_epsr/*
test09dir = $(testroot)/io_gamess
test09_DATA = data/test/io_gamess/*
test10dir = $(testroot)/io_msi
test10_DATA = data/test/io_msi/*
test11dir = $(testroot)/io_pdb
test11_DATA = data/test/io_pdb/*
test12dir = $(testroot)/io_sybylmol2
test12_DATA = data/test/io_sybylmol2/*
test13dir = $(testroot)/io_xyz
test13_DATA = data/test/io_xyz/*
test14dir = $(testroot)/render
test14_DATA = data/test/render/*
test15dir = $(testroot)/typing
test15_DATA = data/test/typing/*

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

