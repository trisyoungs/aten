/*
	*** Undo Event - Measurement Creation
	*** src/undo/measurement_creation.cpp
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

#include "undo/measurement_creation.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
MeasurementCreationEvent::MeasurementCreationEvent() : UndoEvent()
{
}

// Destructor
MeasurementCreationEvent::~MeasurementCreationEvent()
{
}

// Set change 
void MeasurementCreationEvent::set(bool creation, Measurement::MeasurementType mt, int id1, int id2, int id3, int id4)
{
	Messenger::enter("MeasurementCreationEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	type_ = mt;
	targetId_[0] = id1;
	targetId_[1] = id2;
	targetId_[2] = id3;
	targetId_[3] = id4;
	Messenger::exit("MeasurementCreationEvent::set");
}

// Undo stored change
void MeasurementCreationEvent::undo(Model* m)
{
	Messenger::enter("MeasurementCreationEvent::undo");
	Measurement* me = NULL;
	Atom* i, *j, *k,*l, **modelatoms = m->atomArray();
	// Measurement creation (UndoEvent::Undo) and deletion (UndoEvent::Redo)
	i = modelatoms[targetId_[0]];
	j = modelatoms[targetId_[1]];
	k = modelatoms[targetId_[2]];
	l = modelatoms[targetId_[3]];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing measurement - type = %i", type_);
		if (type_ == Measurement::DistanceMeasurement) me = m->findDistanceMeasurement(i, j);
		if (type_ == Measurement::AngleMeasurement) me = m->findAngleMeasurement(i, j, k);
		if (type_ == Measurement::TorsionMeasurement) me = m->findTorsionMeasurement(i, j, k, l);
		if (me != NULL) m->removeMeasurement(me);
		else printf("Couldn't find measurement in UndoEvent.\n");
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying measurement - type = %i", type_);
		m->addMeasurement(type_, i, j, k, l);
	}
	Messenger::exit("MeasurementCreationEvent::undo");
}

// Print event info
void MeasurementCreationEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Measurement deletion, type = %i, atoms = %i %i %i %i\n", type_, targetId_[0], targetId_[1], targetId_[2], targetId_[3]);
	else printf("       Measurement creation - type = %i, atoms = %i %i %i %i\n", type_, targetId_[0], targetId_[1], targetId_[2], targetId_[3]);
}

