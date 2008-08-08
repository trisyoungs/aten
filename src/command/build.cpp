/*
	*** Build command functions
	*** src/command/build.cpp
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

// Add hydrogens to model ('addhydrogen')
int CommandData::function_CA_ADDHYDROGEN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Optional argument specifies an atom, either by id or pointer
	if (c->hasArg(0))
	{
		Atom *i;
		if (c->argt(0) == Variable::IntegerVariable) i = obj.rs->atom(c->argi(0)-1);
		else if (c->argt(0) == Variable::AtomVariable) i = c->arga(0);
		else
		{
			msg.print("Optional argument to 'addhydrogen' must be a variable of Integer or Atom type.\n");
			return CR_FAIL;
		}
		obj.rs->hydrogenSatisfy(i);
	}
	else obj.rs->hydrogenSatisfy();
	return CR_SUCCESS;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
int CommandData::function_CA_CHAIN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// In the first form, draw element at current pen position. In the second, add at the specified coordinates
	Atom *i;
	if (c->hasArg(3))
	{
		Vec3<double> pos = c->arg3d(1);
		i = obj.rs->addAtom(elements.find(c->argc(0),Prefs::AlphaZmap), pos);
		if (obj.i != NULL) obj.rs->bondAtoms(obj.i,i,c->hasArg(4) ? Bond::bondType(c->argc(4)) : Bond::Single);
	}
	else
	{
		i = obj.rs->addAtomAtPen(elements.find(c->argc(0),Prefs::AlphaZmap));
		if (obj.i != NULL) obj.rs->bondAtoms(obj.i,i,c->hasArg(1) ? Bond::bondType(c->argc(1)) : Bond::Single);
	}
	aten.current.i = i;
	return CR_SUCCESS;
}

// Copy current selection ('copy')
int CommandData::function_CA_COPY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	aten.userClipboard->copySelection(obj.rs);
	return CR_SUCCESS;
}

// Cut current selection ('cut')
int CommandData::function_CA_CUT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	aten.userClipboard->cutSelection(obj.rs);
	return CR_SUCCESS;
}

// Terminate chain ('endchain')
int CommandData::function_CA_ENDCHAIN(Command *&c, Bundle &obj)
{
	// TODO end chain with atom id (optional argument)
	obj.i = NULL;
	return CR_SUCCESS;
}

// Delete current selection ('delete')
int CommandData::function_CA_DELETE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectionDelete();
	return CR_SUCCESS;
}

// Set pen coordinates ('locate <dx dy dz>')
int CommandData::function_CA_LOCATE(Command *&c, Bundle &obj)
{
	obj.rs->setPenPosition(c->arg3d(0));
	return CR_SUCCESS;
}

// Move pen along pen axes ('move <dx dy dz>')
int CommandData::function_CA_MOVE(Command *&c, Bundle &obj)
{
	obj.rs->movePenPosition(c->arg3d(0));
	return CR_SUCCESS;
}

// Paste copied selection ('paste')
int CommandData::function_CA_PASTE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (!c->hasArg(2)) aten.userClipboard->pasteToModel(obj.rs);
	else
	{
		Vec3<double> shift = c->arg3d(0);
		aten.userClipboard->pasteToModel(obj.rs, shift);
	}
	return CR_SUCCESS;
}

// Reset pen orientation
int CommandData::function_CA_RESETPEN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->resetPenOrientation();
	return CR_SUCCESS;
}

// Rotate pen orientation about x axis ('rotx <theta>')
int CommandData::function_CA_ROTX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->rotatePenAxis(0, c->argd(0));
	return CR_SUCCESS;
}

// Rotate pen orientation about y axis ('roty <theta>')
int CommandData::function_CA_ROTY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->rotatePenAxis(1, c->argd(0));
	return CR_SUCCESS;
}

// Rotate pen orientation about z axis ('rotz <theta>')
int CommandData::function_CA_ROTZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->rotatePenAxis(2, c->argd(0));
	return CR_SUCCESS;
}

// Shift the current selection down ('shiftdown [n]')
int CommandData::function_CA_SHIFTDOWN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs->shiftSelectionDown();
	return CR_SUCCESS;
}

// Shift the current selection up ('shiftup [n]')
int CommandData::function_CA_SHIFTUP(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs->shiftSelectionUp();
	return CR_SUCCESS;
}

// Move current selection to end of list ('toend')
int CommandData::function_CA_TOEND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->moveSelectionToEnd();
	return CR_SUCCESS;
}

// Move current selection to start of list ('tostart')
int CommandData::function_CA_TOSTART(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->moveSelectionToStart();
	return CR_SUCCESS;
}

// Transmute the current selection ('transmute <el>')
int CommandData::function_CA_TRANSMUTE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	int el = elements.find(c->argc(0));
	for (Atom *i = obj.rs->firstSelected(); i != NULL; i = i->nextSelected()) obj.rs->transmuteAtom(i,el);
	return CR_SUCCESS;
}
