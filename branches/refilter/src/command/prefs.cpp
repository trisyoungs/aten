/*
	*** Script prefs functions
	*** src/command/prefs.cpp
	Copyright T. Youngs 2007

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

#include "command/commands.h"
#include "base/debug.h"
#include "gui/gui.h"

// Preference-related commands
bool script::command_prefs(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_prefs");
	bool result = TRUE;
	// Huge list of temporary enum variables
	mouse_button mb;
	mouse_action ma;
	modifier_key mk;
	view_object vo;
	key_action ka;
	draw_style ds;
	gl_option go;
	energy_unit eu;
	density_unit du;
	colour c;
	static vec3<double> colvec;
	switch (cmd->get_command())
	{
		// Set energy unit to use in output ('energyunits <unit>')
		case (SC_ENERGYUNITS):
			if (EU_from_text(cmd->argc(0)) == EU_NITEMS) result = FALSE;
			else prefs.set_internal_units(EU_from_text(cmd->argc(0)));
			break;
		// Set density unit to use in output ('densityunits <unit>')
		case (SC_DENSITYUNITS):
			if (DU_from_text(cmd->argc(0)) == DU_NITEMS) result = FALSE;
			else prefs.set_density_units(DU_from_text(cmd->argc(0)));
			break;
		// Mouse bindings
		case (SC_MOUSE):
			mb = MB_from_text(cmd->argc(0));
			ma = MA_from_text(cmd->argc(1));
			if ((ma != MA_NITEMS) && (mb != MB_NITEMS)) prefs.set_mb_action(mb,ma);
			break;
		// Key bindings
		case (SC_KEY):
			mk = MK_from_text(cmd->argc(0));
			ka = KA_from_text(cmd->argc(1));
			if ((mk != MK_NITEMS) && (ka != KA_NITEMS)) prefs.set_keymod_action(mk, ka);
			break;
		// Atom and bond quadric detail
		case (SC_ATOMDETAIL):
			prefs.set_atom_detail(cmd->argi(0));
			break;
		case (SC_BONDDETAIL):
			prefs.set_bond_detail(cmd->argi(0));
			break;
		// OpenGL
		case (SC_SHININESS):
			prefs.set_shininess(cmd->argi(0));
			break;
		case (SC_RADIUS):
			ds = DS_from_text(cmd->argc(0));
			if (ds != DS_NITEMS) prefs.set_atom_size(ds, cmd->argd(1));
			break;
		case (SC_GL):
			go = GO_from_text(cmd->argc(0));
			if (go != GO_NITEMS)
			{
				if (cmd->datavar[1]->get_as_bool()) prefs.add_gl_option(go);
				else prefs.remove_gl_option(go);
				gui.mainview.init_gl();
			}
			break;
		// Render Objects
		case (SC_SHOW):
			vo = VO_from_text(cmd->argc(0));
			prefs.set_visible(vo, cmd->datavar[1]->get_as_bool());
			break;
		// View Styles
		case (SC_STYLE):
			ds = DS_from_text(cmd->argc(0));
			if (ds != DS_NITEMS) prefs.set_render_style(ds);
			break;
		// Colours
		case (SC_COLOUR):
			colvec = cmd->get_vector3d(1);
			c = COL_from_text(cmd->argc(0));
			if (c != COL_NITEMS) prefs.set_colour(c, colvec.x, colvec.y, colvec.z, 1.0);
			break;
		default:
			printf("Error - missed prefs command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_prefs");
	return result;
}
