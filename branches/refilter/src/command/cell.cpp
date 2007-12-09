/*
	*** Cell command functions
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

#include "command/commandlist.h"
#include "base/debug.h"
#include "model/model.h"

// Print cell information ('printcell')
int command_functions::function_CA_PRINTCELL(command *&c, objects &obj)
{
	msg(DM_NONE,"Unit cell type for model '%s' is %s\n", obj.m->get_name(), text_from_CT(obj.m->get_celltype()));
	if (obj.m->get_celltype() != 0) obj.m->get_cell()->print();
	return CR_SUCCESS;
}

// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
int command_functions::function_CA_REPLICATECELL(command *&c, objects &obj)
{
	vec3<double> v1, v2;
	v1 = c->get_vector3d(0);
	v2 = c->get_vector3d(3);
	obj.m->replicate_cell(v1, v2);
	return CR_SUCCESS;
}

// Scale cell and molecule COGs ('scalecell <x y z>')
int command_functions::function_CA_SCALECELL(command *&c, objects &obj)
{
	obj.m->scale_cell(c->get_vector3d(0));
	return CR_SUCCESS;
}

// Set/create unit cell ('setcell <a b c> <alpha beta gamma>')
int command_functions::function_CA_SETCELL(command *&c, objects &obj)
{
	obj.m->set_cell(c->get_vector3d(0), c->get_vector3d(3));
	obj.m->log_change(LOG_VISUAL);
	obj.m->calculate_density();
	return CR_SUCCESS;
}
