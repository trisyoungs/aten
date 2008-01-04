/*
	*** Transformation command functions
	*** src/command/transform.cpp
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
#include "model/model.h"
#include "classes/atom.h"

// Centre model at given coordinates
int command_functions::function_CA_CENTRE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->get_parent()->get_infile() == NULL) obj.m->centre(c->arg3d(0));
	else if (prefs.get_centre_on_load() != PS_NO) obj.m->centre(c->arg3d(0));
	return CR_SUCCESS;
}

int command_functions::function_CA_CENTRESELECTION(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	return CR_FAIL;
}

int command_functions::function_CA_TRANSLATE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	return CR_FAIL;
}


// Translate activeatom ('translateatom <dx dy dz>')
int command_functions::function_CA_TRANSLATEATOM(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r() += c->arg3d(0);
	return CR_SUCCESS;
}

// Translate selection ('translate <dx dy dz>')
int command_functions::function_CA_TRANSLATESELECTION(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->translate_selection_local(c->arg3d(0));
	return CR_SUCCESS;
}

// Mirror selection along specified axis
int command_functions::function_CA_MIRRORSELECTION(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->mirror_selection_local(c->argi(0));
	return CR_SUCCESS;
}
