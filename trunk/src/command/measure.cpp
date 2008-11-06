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
#include "model/model.h"

// Measure angle between supplied atoms
int Command::function_CA_ANGLE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Measure angle");
	double a = obj.rs->measureAngle(c->argi(0)-1, c->argi(1)-1, c->argi(2)-1);
	obj.rs->endUndoState();
	if (c->hasArg(3)) c->arg(3)->set(a);
	return Command::Success;
}

// Measure all bond angles in selection
int Command::function_CA_ANGLES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Measure bond angles in selection");
	obj.rs->addMeasurementsInSelection(Measurement::Angle);
	obj.rs->endUndoState();
	return Command::Success;
}

// Clear all measurements in current model
int Command::function_CA_CLEARMEASUREMENTS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Remove all measurements");
	obj.rs->clearMeasurements();
	obj.rs->endUndoState();
	return Command::Success;
}

// Measure distance between supplied atoms
int Command::function_CA_DISTANCE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Measure distance");
	double d = obj.rs->measureDistance(c->argi(0)-1, c->argi(1)-1);
	obj.rs->endUndoState();
	if (c->hasArg(2)) c->arg(2)->set(d);
	return Command::Success;
}

// Measure all bond distances in selection
int Command::function_CA_DISTANCES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Measure bond distances in selection");
	obj.rs->addMeasurementsInSelection(Measurement::Distance);
	obj.rs->endUndoState();
	return Command::Success;
}

// List all measurements in current model
int Command::function_CA_LISTMEASUREMENTS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->listMeasurements();
	return Command::Success;
}

// Make a measurement within the current model
int Command::function_CA_MEASURE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
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
	return Command::Success;
}

// Measure torsion angle between supplied atoms
int Command::function_CA_TORSION(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Measure torsion");
	double t = obj.rs->measureTorsion(c->argi(0)-1, c->argi(1)-1, c->argi(2)-1, c->argi(3)-1);
	obj.rs->endUndoState();
	if (c->hasArg(4)) c->arg(4)->set(t);
	return Command::Success;
}

// Measure all torsion angles in selection
int Command::function_CA_TORSIONS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Measure torsion angles in selection");
	obj.rs->addMeasurementsInSelection(Measurement::Torsion);
	obj.rs->endUndoState();
	return Command::Success;
}
