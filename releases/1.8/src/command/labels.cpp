/*
	*** Labelling Commands
	*** src/command/labels.cpp
	Copyright T. Youngs 2007-2013

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
#include "model/model.h"

// Clear labels in selection
bool Command::function_ClearLabels(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Clear all labels in selection");
	obj.rs()->selectionClearLabels();
	obj.rs()->endUndoState();
	return TRUE;
}

// Add label to current selection or specified atom
bool Command::function_Label(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Atom::AtomLabel al = Atom::atomLabel(c->argc(0), TRUE);
	if (al == Atom::nLabelTypes) return FALSE;
	if (c->hasArg(1))
	{
		Atom *i = obj.rs()->atom(c->argi(1)-1);
		if (i == NULL) return FALSE;
		obj.rs()->beginUndoState("Label atom");
		obj.rs()->addLabel(i, al);
		obj.rs()->endUndoState();
	}
	else
	{
		obj.rs()->beginUndoState("Label selection");
		obj.rs()->selectionAddLabels(al);
		obj.rs()->endUndoState();
	}
	rv.reset();
	return TRUE;
}

// Remove label from current selection or specified atom
bool Command::function_RemoveLabel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Atom::AtomLabel al = Atom::atomLabel(c->argc(0), TRUE);
	if (al == Atom::nLabelTypes) return FALSE;
	if (c->hasArg(1))
	{
		Atom *i = obj.rs()->atom(c->argi(1)-1);
		if (i == NULL) return FALSE;
		obj.rs()->beginUndoState("Remove label from atom");
		obj.rs()->removeLabel(i, al);
		obj.rs()->endUndoState();
	}
	else
	{
		obj.rs()->beginUndoState("Remove labels from selection");
		obj.rs()->selectionRemoveLabels(al);
		obj.rs()->endUndoState();
	}
	rv.reset();
	return TRUE;
}

// Remove all labels from current selection or specified atom
bool Command::function_RemoveLabels(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (c->hasArg(0))
	{
		Atom *i = obj.rs()->atom(c->argi(0)-1);
		if (i == NULL) return FALSE;
		obj.rs()->beginUndoState("Remove all labels from atom");
		obj.rs()->clearLabels(i);
		obj.rs()->endUndoState();
	}
	else
	{
		obj.rs()->beginUndoState("Remove all labels from selection");
		obj.rs()->selectionClearLabels();
		obj.rs()->endUndoState();
	}
	rv.reset();
	return TRUE;
}

