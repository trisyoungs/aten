noinst_LTLIBRARIES = libff.la

libff_la_SOURCES = angle.cpp bond.cpp combine.cpp coulomb.cpp energystore.cpp ewald.cpp expression.cpp forcefield.cpp forms.cpp loadforcefield.cpp rules.cpp saveforcefield.cpp torsion.cpp vdw.cpp

noinst_HEADERS = combine.h energystore.h forcefield.h forms.h

AM_CPPFLAGS = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @ATEN_INCLUDES@ @ATEN_CFLAGS@
