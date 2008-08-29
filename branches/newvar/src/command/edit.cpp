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
#include "base/elements.h"
#include "base/aten.h"
#include "model/model.h"
#include "classes/clipboard.h"

// Copy current selection ('copy')
int CommandData::function_CA_COPY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	aten.userClipboard->copySelection(obj.rs);
	return CR_SUCCESS;
}

// Cut current selection ('cut')
int CommandData::function_CA_CUT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	char s[128];
	sprintf(s,"Cut %i atom%s\n",obj.rs->nSelected(),(obj.rs->nSelected() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	aten.userClipboard->cutSelection(obj.rs);
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Delete current selection ('delete')
int CommandData::function_CA_DELETE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	char s[128];
	sprintf(s,"Delete %i atom%s\n", obj.rs->nSelected(), (obj.rs->nSelected() == 1 ? "" : "s"));
	obj.rs->beginUndoState(s);
	obj.rs->selectionDelete();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Paste copied selection ('paste')
int CommandData::function_CA_PASTE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
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
	return CR_SUCCESS;
}

// Redo most recent change
int CommandData::function_CA_REDO(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	aten.currentModel()->redo();
	return CR_SUCCESS;
}

// Undo most recent change
int CommandData::function_CA_UNDO(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	aten.currentModel()->undo();
	return CR_SUCCESS;
}
