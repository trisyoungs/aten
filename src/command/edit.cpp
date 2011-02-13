/*
	*** Edit Commands
	*** src/command/edit.cpp
	Copyright T. Youngs 2007-2011

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
#include "model/clipboard.h"

// Copy current selection ('copy')
bool Command::function_Copy(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	aten.userClipboard->copySelection(obj.rs);
	msg.print("%i atoms copied to clipboard.\n", aten.userClipboard->nAtoms());
	msg.print(Messenger::Verbose, "Copied selection (%i atoms) from model %s\n", aten.userClipboard->nAtoms(), obj.rs->name());
	return TRUE;
}

// Cut current selection ('cut')
bool Command::function_Cut(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Cut %i atom%s\n",obj.rs->nSelected(),(obj.rs->nSelected() == 1 ? "" : "s"));
	aten.userClipboard->cutSelection(obj.rs);
	obj.rs->endUndoState();
	msg.print("%i atoms cut to clipboard.\n", aten.userClipboard->nAtoms());
	return TRUE;
}

// Delete current selection ('delete')
bool Command::function_Delete(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	int n = obj.rs->nSelected();
	obj.rs->beginUndoState("Delete %i atom%s\n", n, (n == 1 ? "" : "s"));
	obj.rs->selectionDelete();
	obj.rs->endUndoState();
	msg.print("%i atom%s deleted from model.\n", n, (n == 1 ? "" : "s"));
	return TRUE;
}

// Paste copied selection ('paste')
bool Command::function_Paste(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	int n = aten.userClipboard->nAtoms();
	obj.rs->beginUndoState("Paste %i atom%s\n", n, (n == 1 ? "" : "s"));
	if (!c->hasArg(2)) aten.userClipboard->pasteToModel(obj.rs);
	else
	{
		Vec3<double> shift = c->arg3d(0);
		aten.userClipboard->pasteToModel(obj.rs, shift);
	}
	obj.rs->endUndoState();
	msg.print("%i atom%s pasted to model.\n", n, (n == 1 ? "" : "s"));
	return TRUE;
}

// Redo most recent change
bool Command::function_Redo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->redo();
	return TRUE;
}

// Undo most recent change
bool Command::function_Undo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->undo();
	return TRUE;
}

