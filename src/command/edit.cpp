/*
	*** Edit Commands
	*** src/command/edit.cpp
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
#include "main/aten.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Copy current selection ('copy')
bool Commands::function_Copy(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	aten_.userClipboard->copySelection(obj.rs());
	Messenger::print("%i atoms copied to clipboard.", aten_.userClipboard->nAtoms());
	Messenger::print(Messenger::Verbose, "Copied selection (%i atoms) from model %s", aten_.userClipboard->nAtoms(), qPrintable(obj.m->name()));
	return true;
}

// Cut current selection ('cut')
bool Commands::function_Cut(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Cut %i atom%s",obj.rs()->nSelected(),(obj.rs()->nSelected() == 1 ? "" : "s"));
	aten_.userClipboard->cutSelection(obj.rs());
	obj.rs()->endUndoState();
	Messenger::print("%i atoms cut to clipboard.", aten_.userClipboard->nAtoms());
	return true;
}

// Delete current selection ('delete')
bool Commands::function_Delete(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int n = obj.rs()->nSelected();
	obj.rs()->beginUndoState("Delete %i atom%s", n, (n == 1 ? "" : "s"));
	obj.rs()->selectionDelete();
	obj.rs()->endUndoState();
	Messenger::print("%i atom%s deleted from model.", n, (n == 1 ? "" : "s"));
	return true;
}

// Paste copied selection ('paste')
bool Commands::function_Paste(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int n = aten_.userClipboard->nAtoms();
	obj.rs()->beginUndoState("Paste %i atom%s", n, (n == 1 ? "" : "s"));
	if (!c->hasArg(2)) aten_.userClipboard->pasteToModel(obj.rs());
	else
	{
		Vec3<double> shift = c->arg3d(0);
		aten_.userClipboard->pasteToModel(obj.rs(), shift);
	}
	obj.rs()->endUndoState();
	Messenger::print("%i atom%s pasted to model.", n, (n == 1 ? "" : "s"));
	return true;
}

// Redo most recent change
bool Commands::function_Redo(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->redo();
	return true;
}

// Undo most recent change
bool Commands::function_Undo(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->undo();
	return true;
}

