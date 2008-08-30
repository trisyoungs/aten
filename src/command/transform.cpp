/*
	*** Transformation command functions
	*** src/command/transform.cpp
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
#include "classes/prefs.h"

// Centre selection at given coordinates
int CommandData::function_CA_CENTRE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	if (c->parent()->inputFile() == NULL)
	{
		char s[128];
		Vec3<double> centre = c->arg3d(0);
		sprintf(s,"Centre %i atom(s) at %f %f %f\n", obj.rs->nSelected(), centre.x, centre.y, centre.z);
		obj.rs->beginUndoState(s);
		obj.rs->centre(centre);
		obj.rs->endUndoState();
	}
	else if (prefs.centreOnLoad() != Prefs::SwitchOff) obj.rs->centre(c->arg3d(0));
	return CR_SUCCESS;
}

// Translate current selection in local coordinates ('translate dx dy dz')
int CommandData::function_CA_TRANSLATE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	char s[128];
	Vec3<double> tvec = c->arg3d(0);
	sprintf(s,"Translate Cartesian (%i atom(s), %f %f %f)\n", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->beginUndoState(s);
	obj.rs->translateSelectionLocal(tvec);
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Translate activeatom ('translateatom <dx dy dz>')
int CommandData::function_CA_TRANSLATEATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::AtomPointer)) return CR_FAIL;
	char s[128];
	Vec3<double> tvec = c->arg3d(0);
	sprintf(s,"Translate Cartesian (atom %i, %f %f %f)\n", obj.i->id()+1, tvec.x, tvec.y, tvec.z);
	obj.rs->beginUndoState(s);
	obj.rs->translateAtom(obj.i, tvec);
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Translate current selection in fractional cell coordinates ('translatecell dx dy dz')
int CommandData::function_CA_TRANSLATECELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Vec3<double> tvec;
	tvec = obj.rs->cell()->axes() * c->arg3d(0);
	char s[128];
	sprintf(s,"Translate Cell (%i atom(s), %f %f %f)\n", obj.rs->nSelected(), tvec.x, tvec.y, tvec.z);
	obj.rs->beginUndoState(s);
	obj.rs->translateSelectionLocal(tvec);
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Mirror selection along specified axis
int CommandData::function_CA_MIRROR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	char s[128];
	sprintf(s,"Mirror %i atoms along %c\n", obj.rs->nSelected(), 88+c->argi(0));
	obj.rs->beginUndoState(s);
	obj.rs->mirrorSelectionLocal(c->argi(0));
	obj.rs->endUndoState();
	return CR_SUCCESS;
}
