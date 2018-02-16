/*
	*** Pattern Commands
	*** src/command/pattern.cpp
	Copyright T. Youngs 2007-2018

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
#include "model/bundle.h"
#include "model/model.h"
#include "base/pattern.h"

ATEN_USING_NAMESPACE

// Clear current pattern definition ('clearpatterns')
bool Commands::function_ClearPatterns(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->clearPatterns();
	rv.reset();
	return true;
}

// Autocreate pattern definition ('createpatterns')
bool Commands::function_CreatePatterns(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.set( obj.m->createPatterns() );
	return true;
}

// Set working pattern from model ('currentpattern <name>')
bool Commands::function_CurrentPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Pattern* p = (c->argType(0) == VTypes::IntegerData ? obj.m->pattern(c->argi(0)-1) : obj.m->findPattern(c->argc(0)));
	if (p != NULL) obj.p = p;
	rv.set(VTypes::PatternData, p);
	return true;
}

// Fix positions of atoms in pattern
bool Commands::function_FixPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.p->setAtomsFixed( c->argb(0) );
	rv.reset();
	return true;
}

// Select working pattern from model ('getpattern <name>')
bool Commands::function_GetPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Pattern* p = (c->argType(0) == VTypes::IntegerData ? obj.m->pattern(c->argi(0)-1) : obj.m->findPattern(c->argc(0)));
	rv.set(VTypes::PatternData, p);
	return true;
}

// Print pattern definition for current model ('listpatterns')
bool Commands::function_ListPatterns(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->printPatterns();
	rv.reset();
	return true;
}

// Add manual pattern definition ('newpattern <name> <nmols> <natoms>')
bool Commands::function_NewPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.set(VTypes::PatternData, obj.m->addPattern(c->argc(0), c->argi(1), c->argi(2)));
	return true;
}
