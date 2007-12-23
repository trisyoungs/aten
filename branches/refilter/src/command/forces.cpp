/*
	*** Forces command functions
	*** src/command/energy.cpp
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
#include "base/prefs.h"
#include "base/debug.h"

// Calculate forces at trajectory configuration ('frameforces')
int command_functions::function_CA_FRAMEFORCES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	model *frame = obj.m->get_currentframe();
	if (obj.m->create_expression()) obj.m->calculate_forces(frame);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Calculate atomic forces of model ('modelforces')
int command_functions::function_CA_MODELFORCES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (obj.m->create_expression()) obj.m->calculate_forces(obj.m);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Print forces of model ('printforces')
int command_functions::function_CA_PRINTFORCES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->print_forces();
	return CR_SUCCESS;
}
