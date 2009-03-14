/*
	*** Script Commands
	*** src/nucommand/script.cpp
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

// List available scripts
bool NuCommand::function_ListScripts(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (aten.scripts.nItems() == 0) msg.print("No scripts loaded.\n");
	else msg.print("Currently loaded scripts:\n");
	for (Forest *f = aten.scripts.first(); f != NULL; f = f->next)
		msg.print("  %s (%s)\n", f->filename(), f->name());
	rv.reset();
	return TRUE;
}

// Load script from disk
bool NuCommand::function_LoadScript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Forest *f = aten.scripts.add();
	ifstream inputfile(c->argc(0), ios::in);
	if (!f->generate(&inputfile))
	{
		aten.scripts.remove(f);
		return FALSE;
	}
	if (c->hasArg(1)) f->setName(c->argc(1));
	else f->setName(c->argc(0));
	rv.reset();
	return TRUE;
}

// Run specified script
bool NuCommand::function_RunScript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Find the script...
	Forest *f;
	for (f = aten.scripts.first(); f != NULL; f = f->next) if (strcmp(c->argc(0), f->name()) == 0) break;
	if (f != NULL)
	{
		msg.print("Executing script '%s':\n",c->argc(0));
		NuReturnValue result;
		f->executeAll(result);
	}
	else msg.print("Couldn't find script '%s'.\n",c->argc(0));
	rv.reset();
	return TRUE;
}
