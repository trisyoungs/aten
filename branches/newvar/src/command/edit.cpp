/*
	*** Edit command functions
	*** src/command/edit.cpp
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
#include "main/aten.h"
#include "model/model.h"
#include "model/clipboard.h"

// Copy current selection ('copy')
int Command::function_CA_COPY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	aten.userClipboard->copySelection(obj.rs);
	return Command::Success;
}

// Cut current selection ('cut')
int Command::function_CA_CUT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	char s[128];
	sprintf(s,"Cut %i atom%s\n",obj.rs->nSelected(),(obj.rs->nSelected() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	aten.userClipboard->cutSelection(obj.rs);
	obj.rs->endUndoState();
	return Command::Success;
}

// Delete current selection ('delete')
int Command::function_CA_DELETE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	char s[128];
	sprintf(s,"Delete %i atom%s\n", obj.rs->nSelected(), (obj.rs->nSelected() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	obj.rs->selectionDelete();
	obj.rs->endUndoState();
	return Command::Success;
}

// Paste copied selection ('paste')
int Command::function_CA_PASTE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
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
	return Command::Success;
}

// Redo most recent change
int Command::function_CA_REDO(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	aten.currentModel()->redo();
	return Command::Success;
}

// Undo most recent change
int Command::function_CA_UNDO(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	aten.currentModel()->undo();
	return Command::Success;
}
