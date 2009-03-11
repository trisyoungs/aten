/*
	*** Script functions
	*** src/parser/script.cpp
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
bool NuCommand::function_Listscripts(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (aten.scripts.nItems() == 0) msg.print("No scripts loaded.\n");
	else msg.print("Currently loaded scripts:\n");
	for (CommandList *cl = aten.scripts.first(); cl != NULL; cl = cl->next)
		msg.print("  %s (%s)\n", cl->scriptFilename(), cl->name());
	return TRUE;
}

// Load script from disk
bool NuCommand::function_Loadscript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	CommandList *cl = aten.scripts.add();
	if (!cl->load(c->argc(0)))
	{
		aten.scripts.remove(cl);
		return FALSE;
	}
	if (c->hasArg(1)) cl->setName(c->argc(1));
	else cl->setName(c->argc(0));
	return TRUE;
}

// Run specified script
bool NuCommand::function_Runscript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Find the script...
	CommandList *cl;
	for (cl = aten.scripts.first(); cl != NULL; cl = cl->next)
		if (strcmp(c->argc(0), cl->name()) == 0) break;
	if (cl != NULL)
	{
		msg.print("Executing script '%s':\n",c->argc(0));
		cl->execute();
	}
	else msg.print("Couldn't find script '%s'.\n",c->argc(0));
	return TRUE;
}
