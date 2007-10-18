/*
	*** GTK icons
	*** src/gui-gtk/icons.h

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef H_ICONS_H
#define H_ICONS_H

#include <gdk/gdkpixbuf.h>
#include "classes/atom.h"

typedef struct pixbuf_data
{
	GdkPixbuf *xpm_add;
	GdkPixbuf *xpm_caesious;
	GdkPixbuf *xpm_cell;
	GdkPixbuf *xpm_ds[DS_NITEMS];
	GdkPixbuf *xpm_exclaim;
	GdkPixbuf *xpm_pat_cross;
	GdkPixbuf *xpm_pat_tick;
	GdkPixbuf *xpm_remove;
	GdkPixbuf *xpm_togglelist;
	GdkPixbuf *xpm_traj_end;
	GdkPixbuf *xpm_traj_ff;
	GdkPixbuf *xpm_traj_playpause;
	GdkPixbuf *xpm_traj_rewind;
	GdkPixbuf *xpm_traj_start;
	GdkPixbuf *xpm_traj_stop;
	GdkPixbuf *xpm_translate_down;
	GdkPixbuf *xpm_translate_in;
	GdkPixbuf *xpm_translate_left;
	GdkPixbuf *xpm_translate_out;
	GdkPixbuf *xpm_translate_right;
	GdkPixbuf *xpm_translate_up;
	GdkPixbuf *xpm_bond_augment;
	GdkPixbuf *xpm_bond_augmentsel;
	GdkPixbuf *xpm_bond_calculate;
	GdkPixbuf *xpm_bond_calculatesel;
	GdkPixbuf *xpm_bond_clear;
	GdkPixbuf *xpm_bond_clearsel;
	GdkPixbuf *xpm_bond_delete;
	GdkPixbuf *xpm_bond_makedouble;
	GdkPixbuf *xpm_bond_makesingle;
	GdkPixbuf *xpm_bond_maketriple;
	GdkPixbuf *xpm_draw_hadd;
	GdkPixbuf *xpm_draw_atomchain;
	GdkPixbuf *xpm_draw_atoms;
	GdkPixbuf *xpm_draw_delatom;
	GdkPixbuf *xpm_draw_fragment;
	GdkPixbuf *xpm_draw_singleatom;
	GdkPixbuf *xpm_draw_transmute;
	GdkPixbuf *xpm_epsr_restraint;
	GdkPixbuf *xpm_epsr_rotation;
	GdkPixbuf *xpm_geometry_angle;
	GdkPixbuf *xpm_geometry_distance;
	GdkPixbuf *xpm_geometry_torsion;
	GdkPixbuf *xpm_select_element;
	GdkPixbuf *xpm_select_expand;
	GdkPixbuf *xpm_select_fragment;
	GdkPixbuf *xpm_select_invert;
	GdkPixbuf *xpm_select_pick;
	GdkPixbuf *xpm_select_sphere;
	// Functions
	pixbuf_data();
	~pixbuf_data();
};

extern pixbuf_data pixbufs;

#endif
