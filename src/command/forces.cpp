/*
	*** Forces Commands
	*** src/command/forces.cpp
	Copyright T. Youngs 2007-2017

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
#include "model/bundle.h"
#include "model/model.h"
#include <main/aten.h>

ATEN_USING_NAMESPACE

// Calculate forces at trajectory configuration ('frameforces')
bool Commands::function_FrameForces(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (!obj.m->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield())) return false;
	bool success = obj.m->calculateForces(obj.rs());
	rv.reset();
	return success;
}

// Calculate atomic forces of model ('modelforces')
bool Commands::function_ModelForces(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (!obj.m->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield())) return false;
	bool success = obj.m->calculateForces(obj.m);
	rv.reset();
	return success;
}

// Print forces of model ('printforces')
bool Commands::function_PrintForces(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->printForces();
	rv.reset();
	return true;
}

