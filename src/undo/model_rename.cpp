/*
	*** Undo Event - Model Rename
	*** src/undo/model_rename.cpp
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

#include "undo/model_rename.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
ModelRenameEvent::ModelRenameEvent() : UndoEvent()
{
}

// Destructor
ModelRenameEvent::~ModelRenameEvent()
{
}

// Set change 
void ModelRenameEvent::set(QString oldName, QString newName)
{
	Messenger::enter("ModelRenameEvent::set");
	oldName_ = oldName;
	newName_ = newName;
	Messenger::exit("ModelRenameEvent::set");
}

// Undo stored change
void ModelRenameEvent::undo(Model* m)
{
	Messenger::enter("ModelRenameEvent::undo");
	// Model Rename, to oldName_ (UndoEvent::Undo) or newName_ (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing model rename - to %i", qPrintable(oldName_));
		m->setName(oldName_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying model rename - to %i", qPrintable(newName_));
		m->setName(newName_);
	}
	Messenger::exit("ModelRenameEvent::undo");
}

// Print event info
void ModelRenameEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Model rename from %s to %s\n", qPrintable(newName_), qPrintable(oldName_));
	else printf("       Model rename from %s to %s\n",  qPrintable(oldName_), qPrintable(newName_));
}

