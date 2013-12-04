/*
	*** Forces Commands
	*** src/command/forces.cpp
	Copyright T. Youngs 2007-2013

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
#include "parser/commandnode.h"
#include "model/model.h"

// Calculate forces at trajectory configuration ('frameforces')
bool Command::function_FrameForces(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!obj.m->createExpression()) return FALSE;
	bool success = obj.m->calculateForces(obj.rs());
	rv.reset();
	return success;
}

// Calculate atomic forces of model ('modelforces')
bool Command::function_ModelForces(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!obj.m->createExpression()) return FALSE;
	bool success = obj.m->calculateForces(obj.m);
	rv.reset();
	return success;
}

// Print forces of model ('printforces')
bool Command::function_PrintForces(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->printForces();
	rv.reset();
	return TRUE;
}

