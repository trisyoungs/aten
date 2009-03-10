/*
	*** Measurement functions
	*** src/parser/measure.cpp
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

// Measure angle between supplied atoms
bool NuCommand::function_Angle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Measure angle");
	double a = obj.rs->measureAngle(c->argi(0)-1, c->argi(1)-1, c->argi(2)-1);
	obj.rs->endUndoState();
	if (c->hasArg(3)) c->arg(3)->set(a);
	return TRUE;
}

// Measure all bond angles in selection
bool NuCommand::function_Angles(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Measure bond angles in selection");
	obj.rs->addMeasurementsInSelection(Measurement::Angle);
	obj.rs->endUndoState();
	return TRUE;
}

// Clear all measurements in current model
bool NuCommand::function_Clearmeasurements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Remove all measurements");
	obj.rs->clearMeasurements();
	obj.rs->endUndoState();
	return TRUE;
}

// Measure distance between supplied atoms
bool NuCommand::function_Distance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Measure distance");
	double d = obj.rs->measureDistance(c->argi(0)-1, c->argi(1)-1);
	obj.rs->endUndoState();
	if (c->hasArg(2)) c->arg(2)->set(d);
	return TRUE;
}

// Measure all bond distances in selection
bool NuCommand::function_Distances(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Measure bond distances in selection");
	obj.rs->addMeasurementsInSelection(Measurement::Distance);
	obj.rs->endUndoState();
	return TRUE;
}

// List all measurements in current model
bool NuCommand::function_Listmeasurements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->listMeasurements();
	return TRUE;
}

// Make a measurement within the current model
bool NuCommand::function_Measure(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (c->hasArg(3))
	{
		
		obj.rs->beginUndoState("Measure torsion");
		obj.rs->measureTorsion(c->argi(0)-1, c->argi(1)-1, c->argi(2)-1, c->argi(3)-1);
	}
	else if (c->hasArg(2))
	{
		obj.rs->beginUndoState("Measure angle");
		obj.rs->measureAngle(c->argi(0)-1, c->argi(1)-1, c->argi(2)-1);
	}
	else
	{
		obj.rs->beginUndoState("Measure distance");
		obj.rs->measureDistance(c->argi(0)-1, c->argi(1)-1);
	}
	obj.rs->endUndoState();
	return TRUE;
}

// Measure torsion angle between supplied atoms
bool NuCommand::function_Torsion(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Measure torsion");
	double t = obj.rs->measureTorsion(c->argi(0)-1, c->argi(1)-1, c->argi(2)-1, c->argi(3)-1);
	obj.rs->endUndoState();
	if (c->hasArg(4)) c->arg(4)->set(t);
	return TRUE;
}

// Measure all torsion angles in selection
bool NuCommand::function_Torsions(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Measure torsions in selection");
	obj.rs->addMeasurementsInSelection(Measurement::Torsion);
	obj.rs->endUndoState();
	return TRUE;
}
