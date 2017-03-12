/*
	*** Undo Event - Model Rename
	*** src/undo/model_rename.h
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

#ifndef ATEN_UNDOEVENT_MODELRENAME_H
#define ATEN_UNDOEVENT_MODELRENAME_H

#include "undo/undoevent.h"
#include <QString>

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// Model Rename Event
class ModelRenameEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	ModelRenameEvent();
	~ModelRenameEvent();
	
	private:
	// Change data
	QString oldName_, newName_;

	public:
	// Set change data
	void set(QString oldName, QString newName);
	// Undo stored change
	void undo(Model* m);
	// Print change information
	void print();
};

ATEN_END_NAMESPACE

#endif
