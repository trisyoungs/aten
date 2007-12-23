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

// Fold atoms into unit cell
int command_functions::function_CA_FOLD(command *&c, bundle &obj)
{
	if (c->parent->get_infile() == NULL) obj.m->fold_all_atoms();
	else if (prefs.get_fold_on_load() != PS_NO) obj.m->fold_all_atoms();
	return CR_SUCCESS;
}

// Convert fractional coordinates to real coordinates
int command_functions::function_CA_FRACTOREAL(command *&c, bundle &obj)
{
	obj.m->frac_to_real();
	return CR_SUCCESS;
}

// Do crystal packing in model
int command_functions::function_CA_PACK(command *&c, bundle &obj)
{
	if (c->parent->get_infile() == NULL) obj.m->apply_spacegroup_symmops(NULL);
	else if (prefs.get_pack_on_load() != PS_NO) obj.m->apply_spacegroup_symmops(NULL);
	return CR_SUCCESS;
}

// Print cell information ('printcell')
int command_functions::function_CA_PRINTCELL(command *&c, bundle &obj)
{
	msg(DM_NONE,"Unit cell type for model '%s' is %s\n", obj.m->get_name(), text_from_CT(obj.m->get_celltype()));
	if (obj.m->get_celltype() != 0) obj.m->get_cell()->print();
	return CR_SUCCESS;
}

// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
int command_functions::function_CA_REPLICATECELL(command *&c, bundle &obj)
{
	obj.m->replicate_cell(c->arg3d(0), c->arg3d(3));
	return CR_SUCCESS;
}

// Scale cell and molecule COGs ('scalecell <x y z>')
int command_functions::function_CA_SCALECELL(command *&c, bundle &obj)
{
	obj.m->scale_cell(c->arg3d(0));
	return CR_SUCCESS;
}

// Set/create unit cell ('setcell <a b c> <alpha beta gamma>')
int command_functions::function_CA_SETCELL(command *&c, bundle &obj)
{
	obj.m->set_cell(c->arg3d(0), c->arg3d(3));
	obj.m->log_change(LOG_VISUAL);
	obj.m->calculate_density();
	return CR_SUCCESS;
}

// Set/create unit cell ('setcell <ax ay az> <bx by bz> <cx cy cz>')
int command_functions::function_CA_SETCELLAXES(command *&c, bundle &obj)
{
	mat3<double> mat;
	mat.rows[0] = c->arg3d(0);
	mat.rows[1] = c->arg3d(3);
	mat.rows[2] = c->arg3d(6);
	obj.m->set_cell(mat);
	obj.m->log_change(LOG_VISUAL);
	obj.m->calculate_density();
	return CR_SUCCESS;
}

// Set spacegroup
int command_functions::function_CA_SETSPACEGROUP(command *&c, bundle &obj)
{
	obj.m->set_spacegroup(c->argi(0));
	return CR_SUCCESS;
}
