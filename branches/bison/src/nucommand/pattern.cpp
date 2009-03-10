/*
	*** Pattern functions
	*** src/parser/pattern.cpp
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
#include "model/model.h"
#include "base/pattern.h"

// Add manual pattern definition ('newpattern <name> <nmols> <natoms>')
bool NuCommand::function_Newpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->addPattern(c->argi(1), c->argi(2), c->argc(0));
	// TODO Add 'check_pattern(pattern*) method to model*
	return TRUE;
}

// Clear current pattern definition ('clearpatterns')
bool NuCommand::function_Clearpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->clearPatterns();
	return TRUE;
}

// Autocreate pattern definition ('createpatterns')
bool NuCommand::function_Createpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->autocreatePatterns();
	return TRUE;
}

// Select working pattern from model ('getpattern <name>')
bool NuCommand::function_Getpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Pattern *p = (c->argt(0) == VTypes::IntegerData ? obj.m->pattern(c->argi(0)-1) : obj.m->findPattern(c->argc(0)));
	if (p != NULL)
	{
 		obj.p = p;
 		c->arg(1)->set(p, VTypes::PatternData);
	}
	else return FALSE;
	return TRUE;
}

// Print pattern definition for current model ('listpatterns')
bool NuCommand::function_Listpatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->printPatterns();
	return TRUE;
}
