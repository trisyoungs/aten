/*
	*** Labelling command functions
	*** src/command/labels.cpp
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

// Clear labels in selection
int CommandData::function_CA_CLEARLABELS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Clear all labels in selection");
	obj.rs->selectionClearLabels();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Add label to current selection
int CommandData::function_CA_LABEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Atom::AtomLabel al = Atom::atomLabel(c->argc(0));
	if (al != Atom::nLabelItems)
	{
		obj.rs->beginUndoState("Add labels to selection");
		obj.rs->selectionAddLabels(al);
		obj.rs->endUndoState();
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Remove label from current selection
int CommandData::function_CA_REMOVELABEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Atom::AtomLabel al = Atom::atomLabel(c->argc(0));
	if (al != Atom::nLabelItems)
	{
		obj.rs->beginUndoState("Remove labels from selection");
		obj.rs->selectionRemoveLabels(al);
		obj.rs->endUndoState();
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}
