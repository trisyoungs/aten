/*
	*** Prefs command functions
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

#include "command/commandlist.h"
#include "base/debug.h"
#include "base/elements.h"
#include "gui/gui.h"

// Atom quadric detail
int commanddata::function_CA_ATOMDETAIL(command *&c, bundle &obj)
{
	prefs.set_atom_detail(c->argi(0));
	return CR_SUCCESS;
}

// Bond quadric detail
int commanddata::function_CA_BONDDETAIL(command *&c, bundle &obj)
{
	prefs.set_bond_detail(c->argi(0));
	return CR_SUCCESS;
}

// Colours
int commanddata::function_CA_COLOUR(command *&c, bundle &obj)
{
	colour col = COL_from_text(c->argc(0));
	if (col == COL_NITEMS) return CR_FAIL;
	vec3<double> colvec = c->arg3d(1);
	prefs.set_colour(col, colvec.x, colvec.y, colvec.z, 1.0);
	return CR_SUCCESS;
}

// Set density unit to use in output ('densityunits <unit>')
int commanddata::function_CA_DENSITYUNITS(command *&c, bundle &obj)
{
	density_unit du = DU_from_text(c->argc(0));
	if (du == DU_NITEMS) return CR_FAIL;
	else prefs.set_density_units(du);
	return CR_SUCCESS;
}

// Set element's ambient colour
int commanddata::function_CA_ELEMENTAMBIENT(command *&c, bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.set_ambient(el,0,c->argi(1));
	elements.set_ambient(el,1,c->argi(2));
	elements.set_ambient(el,2,c->argi(3));
	return CR_SUCCESS;
}

// Set element's diffuse colour
int commanddata::function_CA_ELEMENTDIFFUSE(command *&c, bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.set_diffuse(el,0,c->argi(1));
	elements.set_diffuse(el,1,c->argi(2));
	elements.set_diffuse(el,2,c->argi(3));
	return CR_SUCCESS;
}

// Set element's radius
int commanddata::function_CA_ELEMENTRADIUS(command *&c, bundle &obj)
{
	int el = elements.find(c->argc(0));
	if (el == 0) return CR_FAIL;
	elements.set_radius(el, c->argd(1));
	return CR_SUCCESS;
}

// Set energy unit to use in output ('energyunits <unit>')
int commanddata::function_CA_ENERGYUNITS(command *&c, bundle &obj)
{
	energy_unit eu = EU_from_text(c->argc(0));
	if (eu == EU_NITEMS) return CR_FAIL;
	else prefs.set_internal_units(eu);
	return CR_SUCCESS;
}

// GL Options
int commanddata::function_CA_GL(command *&c, bundle &obj)
{
	gl_option go = GO_from_text(c->argc(0));
	if (go == GO_NITEMS) return CR_FAIL;
	if (c->argb(1)) prefs.add_gl_option(go);
	else prefs.remove_gl_option(go);
	gui.mainview.init_gl();
	return CR_SUCCESS;
}

// Key bindings
int commanddata::function_CA_KEY(command *&c, bundle &obj)
{
	modifier_key mk = MK_from_text(c->argc(0));
	key_action ka = KA_from_text(c->argc(1));
	if ((mk != MK_NITEMS) && (ka != KA_NITEMS)) prefs.set_keymod_action(mk,ka);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Mouse bindings
int commanddata::function_CA_MOUSE(command *&c, bundle &obj)
{
	mouse_button mb = MB_from_text(c->argc(0));
	mouse_action ma = MA_from_text(c->argc(1));
	if ((ma != MA_NITEMS) && (mb != MB_NITEMS)) prefs.set_mb_action(mb,ma);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Atom screen radii
int commanddata::function_CA_RADIUS(command *&c, bundle &obj)
{
	draw_style ds = DS_from_text(c->argc(0));
	if (ds != DS_NITEMS) prefs.set_atom_size(ds, c->argd(1));
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Atom shininess
int commanddata::function_CA_SHININESS(command *&c, bundle &obj)
{
	prefs.set_shininess(c->argi(0));
	return CR_SUCCESS;
}

// Render Objects
int commanddata::function_CA_SHOW(command *&c, bundle &obj)
{
	view_object vo = VO_from_text(c->argc(0));
	if (vo != VO_NITEMS) prefs.set_visible(vo, c->argb(1));
	else return CR_FAIL;
	return CR_SUCCESS;
}

// View Styles
int commanddata::function_CA_STYLE(command *&c, bundle &obj)
{
	draw_style ds = DS_from_text(c->argc(0));
	if (ds != DS_NITEMS) prefs.set_render_style(ds);
	else return CR_FAIL;
	return CR_SUCCESS;
}
