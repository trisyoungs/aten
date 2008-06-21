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
#include "base/master.h"
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
			msg(Debug::None,"Optional argument to 'addhydrogen' must be a variable of Integer or Atom type.\n");
			return CR_FAIL;
		}
		obj.rs->hydrogenSatisfy(i);
	}
	else obj.rs->hydrogenSatisfy();
	return CR_SUCCESS;
}

// Copy current selection ('copy')
int CommandData::function_CA_COPY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	master.userClipboard->copySelection(obj.rs);
	return CR_SUCCESS;
}

// Cut current selection ('cut')
int CommandData::function_CA_CUT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	master.userClipboard->cutSelection(obj.rs);
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
	c->parent()->penPosition.x = c->argd(0);
	c->parent()->penPosition.y = c->argd(1);
	c->parent()->penPosition.z = c->argd(2);
	return CR_SUCCESS;
}

// Move pen along pen axes ('move <dx dy dz>')
int CommandData::function_CA_MOVE(Command *&c, Bundle &obj)
{
	c->parent()->penPosition += c->parent()->penOrientation.rows[0] * c->argd(0);
	c->parent()->penPosition += c->parent()->penOrientation.rows[1] * c->argd(1);
	c->parent()->penPosition += c->parent()->penOrientation.rows[2] * c->argd(2);
	return CR_SUCCESS;
}

// Paste copied selection ('paste')
int CommandData::function_CA_PASTE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Model *m = master.currentModel();
	master.userClipboard->pasteToModel(m);
	return CR_SUCCESS;
}

// Rotate pen orientation about x axis ('rotx <theta>')
int CommandData::function_CA_ROTX(Command *&c, Bundle &obj)
{
	Mat3<double> rotmat;
	double theta = c->argd(0) / DEGRAD;
	rotmat.set(0,1.0,0.0,0.0);
	rotmat.set(1,0.0,cos(theta),sin(theta));
	rotmat.set(2,0.0,-sin(theta),cos(theta));
	c->parent()->penOrientation *= rotmat;
	return CR_SUCCESS;
}

// Rotate pen orientation about y axis ('roty <theta>')
int CommandData::function_CA_ROTY(Command *&c, Bundle &obj)
{
	Mat3<double> rotmat;
	double theta = c->argd(0) / DEGRAD;
	rotmat.set(0,cos(theta),0.0,-sin(theta));
	rotmat.set(1,0.0,1.0,0.0);
	rotmat.set(2,sin(theta),0.0,cos(theta));
	c->parent()->penOrientation *= rotmat;
	return CR_SUCCESS;
}

// Rotate pen orientation about z axis ('rotz <theta>')
int CommandData::function_CA_ROTZ(Command *&c, Bundle &obj)
{
	Mat3<double> rotmat;
	double theta = c->argd(0) / DEGRAD;
	rotmat.set(0,cos(theta),sin(theta),0.0);
	rotmat.set(1,-sin(theta),cos(theta),0.0);
	rotmat.set(2,0.0,0.0,1.0);
	c->parent()->penOrientation *= rotmat;
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
