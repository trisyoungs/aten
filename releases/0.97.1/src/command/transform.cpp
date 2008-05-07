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
#include "base/master.h"
#include "classes/atom.h"

// Centre selection at given coordinates
int CommandData::function_CA_CENTRE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->parent()->inputFile() == NULL) obj.m->centre(c->arg3d(0));
	else if (prefs.centreOnLoad() != Prefs::SwitchOff) obj.m->centre(c->arg3d(0));
	return CR_SUCCESS;
}

// Translate current selection in local coordinates ('translate dx dy dz')
int CommandData::function_CA_TRANSLATE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->translateSelectionLocal(c->arg3d(0));
	return CR_SUCCESS;
}

// Translate activeatom ('translateatom <dx dy dz>')
int CommandData::function_CA_TRANSLATEATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r() += c->arg3d(0);
	return CR_SUCCESS;
}

// Translate current selection in fractional cell coordinates ('translatecell dx dy dz')
int CommandData::function_CA_TRANSLATECELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Vec3<double> tvec;
	tvec = master.currentModel()->cell()->axes() * c->arg3d(0);
	obj.m->translateSelectionLocal(tvec);
	return CR_SUCCESS;
}

// Mirror selection along specified axis
int CommandData::function_CA_MIRROR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->mirrorSelectionLocal(c->argi(0));
	return CR_SUCCESS;
}
