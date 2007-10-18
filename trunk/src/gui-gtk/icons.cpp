/*
	*** GTK icons
	*** src/gui-gtk/icons.cpp

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

#include "gui-gtk/icons.h"
#include "gui-gtk/16x16.xpms"
#include "gui-gtk/atompos.xpms"
#include "gui-gtk/build_bond.xpms"
#include "gui-gtk/build_sketch.xpms"
#include "gui-gtk/epsr.xpms"
#include "gui-gtk/geometry.xpms"
#include "gui-gtk/select.xpms"

pixbuf_data pixbufs;

// Constructor
pixbuf_data::pixbuf_data()
{
	// Create pixbufs from xpm image data
	g_type_init();
	pixbufs.xpm_add = gdk_pixbuf_new_from_xpm_data(icon_add);
	pixbufs.xpm_caesious = gdk_pixbuf_new_from_xpm_data(icon_caesious);
	pixbufs.xpm_cell = gdk_pixbuf_new_from_xpm_data(icon_cell);
	pixbufs.xpm_ds[DS_INDIVIDUAL] = gdk_pixbuf_new_from_xpm_data(icon_ds_individual);
	pixbufs.xpm_ds[DS_SCALED] = gdk_pixbuf_new_from_xpm_data(icon_ds_scaled);
	pixbufs.xpm_ds[DS_SPHERE] = gdk_pixbuf_new_from_xpm_data(icon_ds_sphere);
	pixbufs.xpm_ds[DS_STICK] = gdk_pixbuf_new_from_xpm_data(icon_ds_stick);
	pixbufs.xpm_ds[DS_TUBE] = gdk_pixbuf_new_from_xpm_data(icon_ds_tube);
	pixbufs.xpm_exclaim = gdk_pixbuf_new_from_xpm_data(icon_exclaim);
	pixbufs.xpm_pat_cross = gdk_pixbuf_new_from_xpm_data(icon_pat_cross);
	pixbufs.xpm_pat_tick = gdk_pixbuf_new_from_xpm_data(icon_pat_tick);
	pixbufs.xpm_remove = gdk_pixbuf_new_from_xpm_data(icon_remove);
	pixbufs.xpm_togglelist = gdk_pixbuf_new_from_xpm_data(icon_togglelist);
	pixbufs.xpm_traj_end = gdk_pixbuf_new_from_xpm_data(icon_traj_end);
	pixbufs.xpm_traj_ff = gdk_pixbuf_new_from_xpm_data(icon_traj_ff);
	pixbufs.xpm_traj_playpause = gdk_pixbuf_new_from_xpm_data(icon_traj_playpause);
	pixbufs.xpm_traj_rewind = gdk_pixbuf_new_from_xpm_data(icon_traj_rewind);
	pixbufs.xpm_traj_start = gdk_pixbuf_new_from_xpm_data(icon_traj_start);
	pixbufs.xpm_traj_stop = gdk_pixbuf_new_from_xpm_data(icon_traj_stop);
	pixbufs.xpm_translate_down = gdk_pixbuf_new_from_xpm_data(icon_translate_down);
	pixbufs.xpm_translate_in = gdk_pixbuf_new_from_xpm_data(icon_translate_in);
	pixbufs.xpm_translate_left = gdk_pixbuf_new_from_xpm_data(icon_translate_left);
	pixbufs.xpm_translate_out = gdk_pixbuf_new_from_xpm_data(icon_translate_out);
	pixbufs.xpm_translate_right = gdk_pixbuf_new_from_xpm_data(icon_translate_right);
	pixbufs.xpm_translate_up = gdk_pixbuf_new_from_xpm_data(icon_translate_up);
	pixbufs.xpm_bond_augment = gdk_pixbuf_new_from_xpm_data(icon_bond_augment);
	pixbufs.xpm_bond_augmentsel = gdk_pixbuf_new_from_xpm_data(icon_bond_augmentsel);
	pixbufs.xpm_bond_calculate = gdk_pixbuf_new_from_xpm_data(icon_bond_calculate);
	pixbufs.xpm_bond_calculatesel = gdk_pixbuf_new_from_xpm_data(icon_bond_calculatesel);
	pixbufs.xpm_bond_clear = gdk_pixbuf_new_from_xpm_data(icon_bond_clear);
	pixbufs.xpm_bond_clearsel = gdk_pixbuf_new_from_xpm_data(icon_bond_clearsel);
	pixbufs.xpm_bond_delete = gdk_pixbuf_new_from_xpm_data(icon_bond_delete);
	pixbufs.xpm_bond_makedouble = gdk_pixbuf_new_from_xpm_data(icon_bond_makedouble);
	pixbufs.xpm_bond_makesingle = gdk_pixbuf_new_from_xpm_data(icon_bond_makesingle);
	pixbufs.xpm_bond_maketriple = gdk_pixbuf_new_from_xpm_data(icon_bond_maketriple);
	pixbufs.xpm_draw_hadd = gdk_pixbuf_new_from_xpm_data(icon_draw_hadd);
	pixbufs.xpm_draw_atomchain = gdk_pixbuf_new_from_xpm_data(icon_draw_atomchain);
	pixbufs.xpm_draw_atoms = gdk_pixbuf_new_from_xpm_data(icon_draw_atoms);
	pixbufs.xpm_draw_delatom = gdk_pixbuf_new_from_xpm_data(icon_draw_delatom);
	pixbufs.xpm_draw_fragment = gdk_pixbuf_new_from_xpm_data(icon_draw_fragment);
	pixbufs.xpm_draw_singleatom = gdk_pixbuf_new_from_xpm_data(icon_draw_singleatom);
	pixbufs.xpm_draw_transmute = gdk_pixbuf_new_from_xpm_data(icon_draw_transmute);
	pixbufs.xpm_epsr_restraint = gdk_pixbuf_new_from_xpm_data(icon_epsr_restraint);
	pixbufs.xpm_epsr_rotation = gdk_pixbuf_new_from_xpm_data(icon_epsr_rotation);
	pixbufs.xpm_geometry_angle = gdk_pixbuf_new_from_xpm_data(icon_geometry_angle);
	pixbufs.xpm_geometry_distance = gdk_pixbuf_new_from_xpm_data(icon_geometry_distance);
	pixbufs.xpm_geometry_torsion = gdk_pixbuf_new_from_xpm_data(icon_geometry_torsion);
	pixbufs.xpm_select_element = gdk_pixbuf_new_from_xpm_data(icon_select_element);
	pixbufs.xpm_select_expand = gdk_pixbuf_new_from_xpm_data(icon_select_expand);
	pixbufs.xpm_select_fragment = gdk_pixbuf_new_from_xpm_data(icon_select_fragment);
	pixbufs.xpm_select_invert = gdk_pixbuf_new_from_xpm_data(icon_select_invert);
	pixbufs.xpm_select_pick = gdk_pixbuf_new_from_xpm_data(icon_select_pick);
	pixbufs.xpm_select_sphere = gdk_pixbuf_new_from_xpm_data(icon_select_sphere);
}

pixbuf_data::~pixbuf_data()
{
	// Free the pixbuf memory
	g_object_unref(xpm_add);
	g_object_unref(xpm_caesious);
	g_object_unref(xpm_cell);
	g_object_unref(xpm_ds[DS_INDIVIDUAL]);
	g_object_unref(xpm_ds[DS_SCALED]);
	g_object_unref(xpm_ds[DS_SPHERE]);
	g_object_unref(xpm_ds[DS_STICK]);
	g_object_unref(xpm_ds[DS_TUBE]);
	g_object_unref(xpm_exclaim);
	g_object_unref(xpm_pat_cross);
	g_object_unref(xpm_pat_tick);
	g_object_unref(xpm_remove);
	g_object_unref(xpm_togglelist);
	g_object_unref(xpm_traj_end);
	g_object_unref(xpm_traj_ff);
	g_object_unref(xpm_traj_playpause);
	g_object_unref(xpm_traj_rewind);
	g_object_unref(xpm_traj_start);
	g_object_unref(xpm_traj_stop);
	g_object_unref(xpm_translate_down);
	g_object_unref(xpm_translate_in);
	g_object_unref(xpm_translate_left);
	g_object_unref(xpm_translate_out);
	g_object_unref(xpm_translate_right);
	g_object_unref(xpm_translate_up);
	g_object_unref(xpm_bond_augment);
	g_object_unref(xpm_bond_augmentsel);
	g_object_unref(xpm_bond_calculate);
	g_object_unref(xpm_bond_calculatesel);
	g_object_unref(xpm_bond_clear);
	g_object_unref(xpm_bond_clearsel);
	g_object_unref(xpm_bond_delete);
	g_object_unref(xpm_bond_makedouble);
	g_object_unref(xpm_bond_makesingle);
	g_object_unref(xpm_bond_maketriple);
	g_object_unref(xpm_draw_hadd);
	g_object_unref(xpm_draw_atomchain);
	g_object_unref(xpm_draw_atoms);
	g_object_unref(xpm_draw_delatom);
	g_object_unref(xpm_draw_fragment);
	g_object_unref(xpm_draw_singleatom);
	g_object_unref(xpm_draw_transmute);
	g_object_unref(xpm_epsr_restraint);
	g_object_unref(xpm_epsr_rotation);
	g_object_unref(xpm_geometry_angle);
	g_object_unref(xpm_geometry_distance);
	g_object_unref(xpm_geometry_torsion);
	g_object_unref(xpm_select_element);
	g_object_unref(xpm_select_expand);
	g_object_unref(xpm_select_fragment);
	g_object_unref(xpm_select_invert);
	g_object_unref(xpm_select_pick);
	g_object_unref(xpm_select_sphere);
}
