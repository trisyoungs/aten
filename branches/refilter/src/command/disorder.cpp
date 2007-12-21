/*
	*** Disorder command functions
	*** src/command/disorder.cpp
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
#include "base/master.h"
#include "base/debug.h"

// Adds component to list ('addcomponent <name> <model> <nmols>')
int command_functions::function_CA_ADDCOMPONENT(command *&c, bundle &obj)
{
	model *compm = master.find_model(c->argc(1));
	if (compm != NULL)
	{
		component *newcomp = master.mc.components.add();
		newcomp->set_model(compm);
		newcomp->set_nrequested(c->argi(2));
		newcomp->set_name(c->argc(0));
	}
	else
	{
		msg(DM_NONE,"Couldn't find model '%s' specified in 'addcomponent'\n", c->argc(1));
		return CR_FAIL;
	}
	return CR_SUCCESS;
}

// Performs MC insertion ('disorder <ncycles>')
int command_functions::function_CA_DISORDER(command *&c, bundle &obj)
{
	if (master.mc.components.size() == 0)
	{
		msg(DM_NONE,"Disordered builder requires a list of components.\n");
		return CR_FAIL;
	}
	msg(DM_NONE,"Performing disordered build for model '%s'\n", obj.m->get_name());
	master.mc.set_ncycles(c->argi(0));
	master.mc.disorder(obj.m);
	return CR_SUCCESS;
}

// Print current component list ('printcomponents')
int command_functions::function_CA_PRINTCOMPONENTS(command *&c, bundle &obj)
{
	msg(DM_NONE,"Current component list:\n");
	vec3<double> v1, v2;
	component *comp = master.mc.components.first();
	comp != NULL ? printf("Name         Nmols  I D T R Z  Model         Region       cent.x  cent.y  cent.z  size.x  size.y  size.z  Overlap\n")
		: printf("None.\n");
	while (comp != NULL)
	{
		v1 = comp->area.get_centre();
		v2 = comp->area.get_size();
		printf("%-10s  %5i  %s %s %s %s %s  %-12s  %-12s %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %3s\n",
			comp->get_name(),comp->get_nrequested(),
			(comp->get_allowed(MT_INSERT) ? "+" : " "),
			(comp->get_allowed(MT_DELETE) ? "+" : " "),
			(comp->get_allowed(MT_TRANSLATE) ? "+" : " "),
			(comp->get_allowed(MT_ROTATE) ? "+" : " "),
			(comp->get_allowed(MT_ZMATRIX) ? "+" : " "),
			comp->get_model()->get_name(),
			text_from_RS(comp->area.get_shape()),
			v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
			(comp->area.get_allowoverlap() ? "Yes" : "No"));
		comp = comp->next;
	}
	return CR_SUCCESS;
}

// Set region centre to position ('setcentre <name> <x y z>')
int command_functions::function_CA_SETCENTRE(command *&c, bundle &obj)
{
	component *comp = master.mc.get_component_by_name(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	comp->area.set_centre(c->get_vector3d(1));
	return CR_SUCCESS;
}

// Set geometry of region ('setgeometry <name> <x y z> [l]')
int command_functions::function_CA_SETGEOMETRY(command *&c, bundle &obj)
{
	component *comp = master.mc.get_component_by_name(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	comp->area.set_size(c->get_vector3d(1));
	if (!c->has_arg(4)) comp->area.set_length(c->argd(4));
	return CR_SUCCESS;
}

// Set overlap flag ('setoverlap <name> true|false')
int command_functions::function_CA_SETOVERLAP(command *&c, bundle &obj)
{
	component *comp = master.mc.get_component_by_name(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	comp->area.set_allowoverlap(c->argb(1));
	return CR_SUCCESS;
}

// Set shape for region ('setshape <name> <shape>')
int command_functions::function_CA_SETSHAPE(command *&c, bundle &obj)
{
	component *comp = master.mc.get_component_by_name(c->argc(0));
	if (comp == NULL)
	{
		msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n", c->argc(0));
		return CR_FAIL;
	}
	region_shape rs = RS_from_text(c->argc(1));
	if (rs != RS_NITEMS) comp->area.set_shape(rs);
	return CR_SUCCESS;
}

// Set vdw radius scaling for method ('vdwscale <scale>')
int command_functions::function_CA_VDWSCALE(command *&c, bundle &obj)
{
	master.mc.set_vdw_radius_scale(c->argd(0));
	return CR_SUCCESS;
}
