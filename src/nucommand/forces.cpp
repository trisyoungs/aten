/*
	*** Forces functions
	*** src/parser/forces.cpp
	Copyright T. Youngs 2007-2009

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

#include "nucommand/commands.h"
#include "model/model.h"

// Calculate forces at trajectory configuration ('frameforces')
bool NuCommand::function_Frameforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->createExpression()) obj.m->calculateForces(obj.rs);
	else return FALSE;
	return TRUE;
}

// Calculate atomic forces of model ('modelforces')
bool NuCommand::function_Modelforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->createExpression()) obj.m->calculateForces(obj.m);
	else return FALSE;
	return TRUE;
}

// Print forces of model ('printforces')
bool NuCommand::function_Printforces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->printForces();
	return TRUE;
}
