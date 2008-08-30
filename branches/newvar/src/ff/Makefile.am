noinst_LIBRARIES = libff.a libfourier.a

libff_a_SOURCES = angle.cpp bond.cpp coulomb.cpp ewald.cpp forcefield.cpp forms.cpp loadforcefield.cpp rules.cpp torsion.cpp vdw.cpp
libfourier_a_SOURCES = fourier.cpp

noinst_HEADERS = forms.h forcefield.h fourier.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
