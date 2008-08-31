noinst_LIBRARIES = libff.a libfourier.a

libff_a_SOURCES = angle.cpp bond.cpp coulomb.cpp energystore.cpp ewald.cpp expression.cpp forcefield.cpp loadforcefield.cpp rules.cpp torsion.cpp vdw.cpp
libfourier_a_SOURCES = fourier.cpp

noinst_HEADERS = energystore.h forcefield.h fourier.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
