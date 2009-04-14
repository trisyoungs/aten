/*
	*** Edit Commands
	*** src/nucommand/edit.cpp
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
	char s[128];
	sprintf(s,"Cut %i atom%s\n",obj.rs->nSelected(),(obj.rs->nSelected() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	aten.userClipboard->cutSelection(obj.rs);
	obj.rs->endUndoState();
	return TRUE;
}

// Delete current selection ('delete')
bool Command::function_Delete(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	char s[128];
	sprintf(s,"Delete %i atom%s\n", obj.rs->nSelected(), (obj.rs->nSelected() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	obj.rs->selectionDelete();
	obj.rs->endUndoState();
	return TRUE;
}

// Paste copied selection ('paste')
bool Command::function_Paste(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	char s[128];
	sprintf(s,"Paste %i atom%s\n", aten.userClipboard->nAtoms(), (aten.userClipboard->nAtoms() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	if (!c->hasArg(2)) aten.userClipboard->pasteToModel(obj.rs);
	else
	{
		Vec3<double> shift = c->arg3d(0);
		aten.userClipboard->pasteToModel(obj.rs, shift);
	}
	obj.rs->endUndoState();
	return TRUE;
}

// Redo most recent change
bool Command::function_Redo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	aten.currentModel()->redo();
	return TRUE;
}

// Undo most recent change
bool Command::function_Undo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	aten.currentModel()->undo();
	return TRUE;
}
