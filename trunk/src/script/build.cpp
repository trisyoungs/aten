/*
	*** Script build functions
	*** src/script/build.cpp
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
#include "base/prefs.h"
#include "base/elements.h"
#include "base/debug.h"

// Build-related script commands (root=SR_BUILD)
bool script::command_build(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_build");
	bool result = TRUE;
	double theta;
	static mat3<double> rotmat;
	int el;
	atom *i;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_build");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		// Draw unbound atom ('addatom <el>')
		case (SC_ADDATOM):
			i = m->add_atom(elements.find(cmd->datavar[0]->get_as_char(),ZM_ALPHA), penpos);
			activeatom = i;
			break;
		// Draw atom with bond to 'activeatom' ('addchain <el>')
		case (SC_ADDCHAIN):
			i = m->add_atom(elements.find(cmd->datavar[0]->get_as_char(),ZM_ALPHA), penpos);
			if (check_activeatom("addchain"))
			{
				m->bond_atoms(activeatom,i,BT_SINGLE);
				activebond = activeatom->find_bond(i);
			}
			activeatom = i;
			break;
		// Add hydrogens to model ('addhydrogen')
		case (SC_ADDHYDROGEN):
			m->hydrogen_satisfy();
			break;
		// Delete current selection ('delete')
		case (SC_DELETE):
			m->selection_delete();
			break;
		// Terminate chain ('endchain')
		case (SC_ENDCHAIN):
			activeatom = NULL;
			break;
		// Move pen along pen axes ('move <dx dy dz>')
		case (SC_MOVE):
			penpos += penorient.rows[0] * cmd->datavar[0]->get_as_double();
			penpos += penorient.rows[1] * cmd->datavar[1]->get_as_double();
			penpos += penorient.rows[2] * cmd->datavar[2]->get_as_double();
			break;
		// Rotate pen orientation about x axis ('rotx <theta>')
		case (SC_ROTX):
			theta = cmd->datavar[0]->get_as_double() / DEGRAD;
			rotmat.set(0,1.0,0.0,0.0);
			rotmat.set(1,0.0,cos(theta),sin(theta));
			rotmat.set(2,0.0,-sin(theta),cos(theta));
			penorient *= rotmat;
			break;
		// Rotate pen orientation about y axis ('roty <theta>')
		case (SC_ROTY):
			theta = cmd->datavar[0]->get_as_double() / DEGRAD;
			rotmat.set(0,cos(theta),0.0,-sin(theta));
			rotmat.set(1,0.0,1.0,0.0);
			rotmat.set(2,sin(theta),0.0,cos(theta));
			penorient *= rotmat;
			break;
		// Rotate pen orientation about z axis ('rotz <theta>')
		case (SC_ROTZ):
			theta = cmd->datavar[0]->get_as_double() / DEGRAD;
			rotmat.set(0,cos(theta),sin(theta),0.0);
			rotmat.set(1,-sin(theta),cos(theta),0.0);
			rotmat.set(2,0.0,0.0,1.0);
			penorient *= rotmat;
			break;
		// Set pen coordinates ('locate <dx dy dz>')
		case (SC_LOCATE):
			penpos.x = cmd->datavar[0]->get_as_double();
			penpos.y = cmd->datavar[1]->get_as_double();
			penpos.z = cmd->datavar[2]->get_as_double();
			break;
		// Transmute the current selection ('transmute <el>')
		case (SC_TRANSMUTE):
			el = elements.find(cmd->datavar[0]->get_as_char());
			for (i = m->get_first_selected(); i != NULL; i = i->get_next_selected()) m->transmute_atom(i,el);
			break;
		default:
			printf("Error - missed build command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_build");
	return result;
}
