/*
	*** Measurement Commands
	*** src/command/measure.cpp
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

ATEN_USING_NAMESPACE

// Clear all measurements in current model
bool Commands::function_ClearMeasurements(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Remove all measurements");
	obj.rs()->clearMeasurements();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Calculate a measurement within the current model, but don't display it
bool Commands::function_GeometryCalc(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Atom* atoms[4];
	if (c->hasArg(3))
	{
		for (int n=0; n<4; ++n) atoms[n] = c->argType(n) == VTypes::IntegerData ? obj.rs()->atom(c->argi(n)-1) : (Atom*) c->argp(n, VTypes::AtomData);
		rv.set(obj.rs()->torsion(atoms[0], atoms[1], atoms[2], atoms[3]));
	}
	else if (c->hasArg(2))
	{
		for (int n=0; n<3; ++n) atoms[n] = c->argType(n) == VTypes::IntegerData ? obj.rs()->atom(c->argi(n)-1) : (Atom*) c->argp(n, VTypes::AtomData);
		rv.set(obj.rs()->angle(atoms[0], atoms[1], atoms[2]));
	}
	else
	{
		for (int n=0; n<2; ++n) atoms[n] = c->argType(n) == VTypes::IntegerData ? obj.rs()->atom(c->argi(n)-1) : (Atom*) c->argp(n, VTypes::AtomData);
		rv.set(obj.rs()->distance(atoms[0], atoms[1]));
	}
	return true;
}

// List all measurements in current model
bool Commands::function_ListMeasurements(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->listMeasurements();
	rv.reset();
	return true;
}

// Make a measurement within the current model
bool Commands::function_Measure(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Atom* atoms[4];
	if (c->hasArg(3))
	{
		obj.rs()->beginUndoState("Measure torsion");
		for (int n=0; n<4; ++n) atoms[n] = c->argType(n) == VTypes::IntegerData ? obj.rs()->atom(c->argi(n)-1) : (Atom*) c->argp(n, VTypes::AtomData);
		rv.set(obj.rs()->addTorsionMeasurement(atoms[0], atoms[1], atoms[2], atoms[3]));
	}
	else if (c->hasArg(2))
	{
		obj.rs()->beginUndoState("Measure angle");
		for (int n=0; n<3; ++n) atoms[n] = c->argType(n) == VTypes::IntegerData ? obj.rs()->atom(c->argi(n)-1) : (Atom*) c->argp(n, VTypes::AtomData);
		rv.set(obj.rs()->addAngleMeasurement(atoms[0], atoms[1], atoms[2]));
	}
	else
	{
		obj.rs()->beginUndoState("Measure distance");
		for (int n=0; n<2; ++n) atoms[n] = c->argType(n) == VTypes::IntegerData ? obj.rs()->atom(c->argi(n)-1) : (Atom*) c->argp(n, VTypes::AtomData);
		rv.set(obj.rs()->addDistanceMeasurement(atoms[0], atoms[1]));
	}
	obj.rs()->endUndoState();
	return true;
}

// Make a series measurements of one type within the current atom selection
bool Commands::function_MeasureSelected(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	switch (c->argi(0))
	{
		case (2):
			obj.rs()->beginUndoState("Measure distances in selection");
			obj.rs()->addMeasurementsInSelection(Measurement::DistanceMeasurement);
			obj.rs()->endUndoState();
			break;
		case (3):
			obj.rs()->beginUndoState("Measure angles in selection");
			obj.rs()->addMeasurementsInSelection(Measurement::AngleMeasurement);
			obj.rs()->endUndoState();
			break;
		case (4):
			obj.rs()->beginUndoState("Measure torsions in selection");
			obj.rs()->addMeasurementsInSelection(Measurement::TorsionMeasurement);
			obj.rs()->endUndoState();
			break;
		default:
			Messenger::print("%i does not represent a geometry type (number of atoms involved).", c->argi(0));
			return false;
			break;
	}
	return true;
}
