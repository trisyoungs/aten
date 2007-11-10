/*
	*** Script disorder functions
	*** src/script/disorder.cpp
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

#include "script/script.h"
#include "base/master.h"
#include "base/debug.h"

// Disorder builder-related script commands (root=SR_DISORDER)
bool script::command_disorder(command_node<script_command>  *cmd)
{
	dbg_begin(DM_CALLS,"script::command_disorder");
	bool result = TRUE;
	atom *i;
	model *m;
	vec3<double> v1,v2;
	region_shape rs;
	component *c;
	model *m2 = check_activemodel(text_from_SC(cmd->get_command()));
	if (m2 == NULL)
	{
		dbg_end(DM_CALLS,"script::command_disorder");
		return FALSE;
	}
	int cmdi = cmd->get_command();
	switch (cmdi)
	{
		case (SC_ADDCOMPONENT):		// Adds component to list ('addcomponent <name> <model> <nmols>')
			m = master.find_model(cmd->datavar[1]->get_as_char());
			if (m != NULL)
			{
				c = master.mc.components.add();
				c->set_model(m);
				c->set_nrequested(cmd->datavar[2]->get_as_int());
				c->set_name(cmd->datavar[0]->get_as_char());
			}
			else
			{
				msg(DM_NONE,"script : Couldn't find model specified in 'addcomponent'\n");
				result = FALSE;
			}
			break;
		case (SC_PRINTCOMPONENTS):	// Print current component list ('printcomponents')
			msg(DM_NONE,"Current component list:\n");
			c = master.mc.components.first();
			c != NULL ? printf("Name         Nmols  I D T R Z  Model         Region       cent.x  cent.y  cent.z  size.x  size.y  size.z  Overlap\n")
				: printf("None.\n");
			while (c != NULL)
			{
				v1 = c->area.get_centre();
				v2 = c->area.get_size();
				printf("%-10s  %5i  %s %s %s %s %s  %-12s  %-12s %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %3s\n",
					c->get_name(),c->get_nrequested(),
					(c->get_allowed(MT_INSERT) ? "+" : " "),
					(c->get_allowed(MT_DELETE) ? "+" : " "),
					(c->get_allowed(MT_TRANSLATE) ? "+" : " "),
					(c->get_allowed(MT_ROTATE) ? "+" : " "),
					(c->get_allowed(MT_ZMATRIX) ? "+" : " "),
					c->get_model()->get_name(),
					text_from_RS(c->area.get_shape()),
					v1.x, v1.y, v1.z, v2.x, v2.y, v2.z,
					(c->area.get_allowoverlap() ? "Yes" : "No"));
				c = c->next;
			}
			break;
		case (SC_DISORDER):		// Performs MC insertion ('disorder <ncycles>')
			if (master.mc.components.size() == 0)
			{
				msg(DM_NONE,"MC insertion requires a list of components.\n");
				break;
			}
			msg(DM_NONE,"Performing disordered build for model '%s'\n",m2->get_name());
			master.mc.set_ncycles(cmd->datavar[0]->get_as_int());
			master.mc.disorder(m2);
			break;
		case (SC_SETCENTRE):
		case (SC_SETOVERLAP):
		case (SC_SETSHAPE):
		case (SC_SETGEOMETRY):
			// Commands that set region data - all need a valid component
			c = master.mc.get_component_by_name(cmd->datavar[0]->get_as_char());
			if (c == NULL)
			{
				msg(DM_NONE,"ERROR: '%s' is not a valid component name.\n",cmd->datavar[0]->get_as_char());
				dbg_end(DM_CALLS,"script::command_region");
				return FALSE;
			}
			switch (cmdi)
			{
				case (SC_SETSHAPE):	// Set shape for region ('setshape <name> <shape>')
					rs = RS_from_text(cmd->datavar[1]->get_as_char());
					if (rs != RS_NITEMS) c->area.set_shape(rs);
					break;
				case (SC_SETCENTRE):	// Set region centre to position ('setcentre <name> <x y z>')
					c->area.set_centre(cmd->get_vector3d(1));
					break;
				case (SC_SETGEOMETRY):	// Set geometry of region ('setgeometry <name> <x y z> [l]')
					c->area.set_size(cmd->get_vector3d(1));
					if (cmd->datavar[4] != NULL) c->area.set_length(cmd->datavar[4]->get_as_double());
					break;
				case (SC_SETOVERLAP):	// Set overlap flag ('setoverlap <name> true|false')
					printf("Setting overlap flag..\n");
					c->area.set_allowoverlap(cmd->datavar[1]->get_as_bool());
					break;
			}
			break;
		case (SC_VDWSCALE):	// Set vdw radius scaling for method ('vdwscale <scale>')
			master.mc.set_vdw_radius_scale(cmd->datavar[0]->get_as_double());
			break;
		default:
			printf("Error - missed expr command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_disorder");
	return result;
}

