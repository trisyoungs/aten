noinst_LIBRARIES = libenergy.a libfourier.a

libenergy_a_SOURCES = angle.cpp bond.cpp coulomb.cpp ewald.cpp forms.cpp rules.cpp torsion.cpp vdw.cpp

libfourier_a_SOURCES = fourier.cpp

noinst_HEADERS = forms.h fourier.h

INCLUDES = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @GUI_CFLAGS@
