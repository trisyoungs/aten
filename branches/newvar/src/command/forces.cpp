/*
	*** Forces command functions
	*** src/command/forces.cpp
	Copyright T. Youngs 2007,2008

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
#include "base/messenger.h"

// Calculate forces at trajectory configuration ('frameforces')
int CommandData::function_CA_FRAMEFORCES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	if (obj.m->createExpression()) obj.m->calculateForces(obj.rs);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Calculate atomic forces of model ('modelforces')
int CommandData::function_CA_MODELFORCES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	if (obj.m->createExpression()) obj.m->calculateForces(obj.m);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Print forces of model ('printforces')
int CommandData::function_CA_PRINTFORCES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.m->printForces();
	return CR_SUCCESS;
}
