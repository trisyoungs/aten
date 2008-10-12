/*
	*** Script command functions
	*** src/command/script.cpp
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
#include "main/aten.h"

// List available scripts
int Command::function_CA_LISTSCRIPTS(CommandNode *&c, Bundle &obj)
{
	if (aten.scripts.nItems() == 0) msg.print("No scripts loaded.\n");
	else msg.print("Currently loaded scripts:\n");
	for (CommandList *cl = aten.scripts.first(); cl != NULL; cl = cl->next)
		msg.print("  %s (%s)\n", cl->scriptFilename(), cl->name());
	return Command::Success;
}

// Load script from disk
int Command::function_CA_LOADSCRIPT(CommandNode *&c, Bundle &obj)
{
	CommandList *cl = aten.scripts.add();
	if (!cl->load(c->argc(0)))
	{
		aten.scripts.remove(cl);
		return Command::Fail;
	}
	if (c->hasArg(1)) cl->setName(c->argc(1));
	else cl->setName(c->argc(0));
	return Command::Success;
}

// Run specified script
int Command::function_CA_RUNSCRIPT(CommandNode *&c, Bundle &obj)
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
	return Command::Success;
}
