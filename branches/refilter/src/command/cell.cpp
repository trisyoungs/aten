/*
	*** Script cell functions
	*** src/command/cell.cpp
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
#include "classes/cell.h"

// Cell-related script commands (root=SR_CELL)
bool script::command_cell(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_cell");
	bool result = TRUE;
	static vec3<double> v1, v2;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_cell");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		// Print cell information ('printcell')
		case (SC_PRINTCELL):
			msg(DM_NONE,"Unit cell type for model '%s' is %s\n",m->get_name(),text_from_CT(m->get_celltype()));
			if (m->get_celltype() != 0) m->get_cell()->print();
			break;
		// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
		case (SC_REPLICATECELL):
			v1 = cmd->get_vector3d(0);
			v2 = cmd->get_vector3d(3);
			m->replicate_cell(v1, v2);
			break;
		// Set/create unit cell ('setcell <a b c> <alpha beta gamma>')
		case (SC_SETCELL):
			m->set_cell(cmd->get_vector3d(0),cmd->get_vector3d(3));
			m->log_change(LOG_VISUAL);
			m->calculate_density();
			break;
		// Scale cell and molecule COGs ('scalecell <x y z>')
		case (SC_SCALECELL):
			m->scale_cell(cmd->get_vector3d(0));
			break;
		default:
			printf("Error - missed cell command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_cell");
	return result;
}
