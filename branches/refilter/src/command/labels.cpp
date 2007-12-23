/*
	*** Labelling command functions
	*** src/command/labels.cpp
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

// Clear labels in selection
int command_functions::function_CA_CLEARLABELS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->selection_clear_labels();
	return CR_SUCCESS;
}

// Add label to current selection
int command_functions::function_CA_ADDLABEL(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	atom_label al = AL_from_text(c->argc(0));
	if (al != AL_NITEMS) obj.m->selection_add_labels(al);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Remove label from current selection
int command_functions::function_CA_REMOVELABEL(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	atom_label al = AL_from_text(c->argc(0));
	if (al != AL_NITEMS) obj.m->selection_remove_labels(al);
	else return CR_FAIL;
	return CR_SUCCESS;
}
