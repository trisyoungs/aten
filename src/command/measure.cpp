/*
	*** Measurement command functions
	*** src/command/measure.cpp
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
#include "base/messenger.h"
#include "base/aten.h"
#include "model/model.h"

// Clear all measurements in current model
int CommandData::function_CA_CLEARMEASUREMENTS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	aten.currentModel()->clearMeasurements();
	return CR_SUCCESS;
}

// List all measurements in current model
int CommandData::function_CA_LISTMEASUREMENTS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	aten.currentModel()->listMeasurements();
	return CR_SUCCESS;
}

// Make a measurement within the current model
int CommandData::function_CA_MEASURE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3))
	{
		obj.rs->beginUndoState("Measure torsion");
		aten.currentModel()->measureTorsion(c->argi(0), c->argi(1), c->argi(2), c->argi(3));
	}
	else if (c->hasArg(2))
	{
		obj.rs->beginUndoState("Measure angle");
		aten.currentModel()->measureAngle(c->argi(0), c->argi(1), c->argi(2));
	}
	else
	{
		obj.rs->beginUndoState("Measure distance");
		aten.currentModel()->measureDistance(c->argi(0), c->argi(1));
	}
	obj.rs->endUndoState();
	return CR_SUCCESS;
}
