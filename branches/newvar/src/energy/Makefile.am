noinst_LIBRARIES = libff.a libfourier.a

libenergy_a_SOURCES = angle.cpp bond.cpp coulomb.cpp forcefield.cpp ewald.cpp forms.cpp rules.cpp torsion.cpp vdw.cpp
libff_a_SOURCES = fourier.cpp

noinst_HEADERS = forms.h forcefield.h fourier.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
