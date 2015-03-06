noinst_LIBRARIES = libff.a libfourier.a

libff_a_SOURCES = angle.cpp bond.cpp combine.cpp coulomb.cpp energystore.cpp ewald.cpp expression.cpp forcefield.cpp forms.cpp loadforcefield.cpp rules.cpp saveforcefield.cpp torsion.cpp vdw.cpp

libfourier_a_SOURCES = fourier.cpp

noinst_HEADERS = combine.h energystore.h forcefield.h forms.h fourier.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
