/*
	*** Pattern command functions
	*** src/command/pattern.cpp
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
#include "base/pattern.h"

// Add manual pattern definition ('newpattern <name> <nmols> <natoms>')
int Command::function_CA_NEWPATTERN(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->addPattern(c->argi(1), c->argi(2), c->argc(0));
	// TODO Add 'check_pattern(pattern*) method to model*
	return Command::Success;
}

// Clear current pattern definition ('clearpatterns')
int Command::function_CA_CLEARPATTERNS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->clearPatterns();
	return Command::Success;
}

// Autocreate pattern definition ('createpatterns')
int Command::function_CA_CREATEPATTERNS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->autocreatePatterns();
	return Command::Success;
}

// Select working pattern from model ('getpattern <name>')
int Command::function_CA_GETPATTERN(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Pattern *p = (c->argt(0) == VTypes::IntegerData ? obj.m->pattern(c->argi(0)-1) : obj.m->findPattern(c->argc(0)));
	if (p != NULL)
	{
 		obj.p = p;
 		c->arg(1)->set(p, VTypes::PatternData);
	}
	else return Command::Fail;
	return Command::Success;
}

// Print pattern definition for current model ('listpatterns')
int Command::function_CA_LISTPATTERNS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->printPatterns();
	return Command::Success;
}
