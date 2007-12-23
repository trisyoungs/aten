/*
	*** Build command functions
	*** src/command/build.cpp
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
#include "base/elements.h"
#include "model/model.h"

// Add hydrogens to model ('addhydrogen')
int command_functions::function_CA_ADDHYDROGEN(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->hydrogen_satisfy();
	return CR_SUCCESS;
}

// Draw unbound atom ('addatom <el> [x y z]')
int command_functions::function_CA_ADDATOM(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	int el = elements.find(c->argc(0));
	if (c->has_arg(3)) obj.i = obj.m->add_atom(el, c->parent->penpos);
	else obj.i = obj.m->add_atom(el, c->arg3d(1));
	// Set other variables for this atom...
	c->parent->variables.get_atom_variables(obj.i);
	// Reset variables...
	return CR_SUCCESS;
}

// Draw atom with bond to 'activeatom' ('addchain <el>')
int command_functions::function_CA_ADDCHAIN(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	atom *i = obj.m->add_atom(elements.find(c->argc(0),ZM_ALPHA), c->parent->penpos);
	if (obj.i != NULL) obj.m->bond_atoms(obj.i,i,BT_SINGLE);
	obj.i = i;
	return CR_SUCCESS;
}

// Delete current selection ('delete')
int command_functions::function_CA_DELETE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->selection_delete();
	return CR_SUCCESS;
}

// Terminate chain ('endchain')
int command_functions::function_CA_ENDCHAIN(command *&c, bundle &obj)
{
	// TODO end chain with atom id (optional argument)
	obj.i = NULL;
	return CR_FAIL;
}

// Set pen coordinates ('locate <dx dy dz>')
int command_functions::function_CA_LOCATE(command *&c, bundle &obj)
{
	c->parent->penpos.x = c->argd(0);
	c->parent->penpos.y = c->argd(1);
	c->parent->penpos.z = c->argd(2);
	return CR_SUCCESS;
}

// Move pen along pen axes ('move <dx dy dz>')
int command_functions::function_CA_MOVE(command *&c, bundle &obj)
{
	c->parent->penpos += c->parent->penorient.rows[0] * c->argd(0);
	c->parent->penpos += c->parent->penorient.rows[1] * c->argd(1);
	c->parent->penpos += c->parent->penorient.rows[2] * c->argd(2);
	return CR_SUCCESS;
}

// Rotate pen orientation about x axis ('rotx <theta>')
int command_functions::function_CA_ROTX(command *&c, bundle &obj)
{
	mat3<double> rotmat;
	double theta = c->argd(0) / DEGRAD;
	rotmat.set(0,1.0,0.0,0.0);
	rotmat.set(1,0.0,cos(theta),sin(theta));
	rotmat.set(2,0.0,-sin(theta),cos(theta));
	c->parent->penorient *= rotmat;
	return CR_SUCCESS;
}

// Rotate pen orientation about y axis ('roty <theta>')
int command_functions::function_CA_ROTY(command *&c, bundle &obj)
{
	mat3<double> rotmat;
	double theta = c->argd(0) / DEGRAD;
	rotmat.set(0,cos(theta),0.0,-sin(theta));
	rotmat.set(1,0.0,1.0,0.0);
	rotmat.set(2,sin(theta),0.0,cos(theta));
	c->parent->penorient *= rotmat;
	return CR_SUCCESS;
}

// Rotate pen orientation about z axis ('rotz <theta>')
int command_functions::function_CA_ROTZ(command *&c, bundle &obj)
{
	mat3<double> rotmat;
	double theta = c->argd(0) / DEGRAD;
	rotmat.set(0,cos(theta),sin(theta),0.0);
	rotmat.set(1,-sin(theta),cos(theta),0.0);
	rotmat.set(2,0.0,0.0,1.0);
	c->parent->penorient *= rotmat;
	return CR_SUCCESS;
}

// Transmute the current selection ('transmute <el>')
int command_functions::function_CA_TRANSMUTE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	int el = elements.find(c->argc(0));
	for (atom *i = obj.m->get_first_selected(); i != NULL; i = i->get_next_selected()) obj.m->transmute_atom(i,el);
	return CR_SUCCESS;
}
