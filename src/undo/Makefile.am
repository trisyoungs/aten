noinst_LTLIBRARIES = libundo.la

libundo_la_SOURCES = atom_charge.cpp atom_colour.cpp atom_creation.cpp atom_fix.cpp atom_hide.cpp atom_label.cpp atom_select.cpp atom_shift.cpp atom_style.cpp atom_swap.cpp atom_translate.cpp atom_transmute.cpp bond_change.cpp bond_creation.cpp cell_set.cpp glyph_creation.cpp measurement_creation.cpp model_rename.cpp undoevent.cpp undostate.cpp

noinst_HEADERS = atom_charge.h atom_colour.h atom_creation.h atom_fix.h atom_hide.h atom_label.h atom_select.h atom_shift.h atom_style.h atom_swap.h atom_translate.h atom_transmute.h bond_change.h bond_creation.h cell_set.h glyph_creation.h measurement_creation.h model_rename.h undoevent.h undostate.h

AM_CPPFLAGS = -I$(top_srcdir)/src -I../ -I$(top_srcdir)/src/gui @ATEN_INCLUDES@ @ATEN_CFLAGS@

