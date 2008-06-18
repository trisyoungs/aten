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
#include "base/master.h"

// List available scripts
int CommandData::function_CA_LISTSCRIPTS(Command *&c, Bundle &obj)
{
	if (master.scripts.nItems() == 0) msg(Debug::None,"No scripts loaded.\n");
	else msg(Debug::None,"Currently loaded scripts:\n");
	for (CommandList *cl = master.scripts.first(); cl != NULL; cl = cl->next)
		msg(Debug::None,"  %s (%s)\n", cl->scriptFilename(), cl->name());
	return CR_SUCCESS;
}

// Load script from disk
int CommandData::function_CA_LOADSCRIPT(Command *&c, Bundle &obj)
{
	CommandList *cl = master.scripts.add();
	if (!cl->load(c->argc(0)))
	{
		master.scripts.remove(cl);
		return CR_FAIL;
	}
	if (c->hasArg(1)) cl->setName(c->argc(1));
	else cl->setName(c->argc(0));
	return CR_SUCCESS;
}

// Run specified script
int CommandData::function_CA_RUNSCRIPT(Command *&c, Bundle &obj)
{
	// Find the script...
	CommandList *cl;
	for (cl = master.scripts.first(); cl != NULL; cl = cl->next)
		if (strcmp(c->argc(0), cl->name()) == 0) break;
	if (cl != NULL)
	{
		msg(Debug::None,"Executing script '%s':\n",c->argc(0));
		cl->execute();
	}
	else msg(Debug::None,"Couldn't find script '%s'.\n",c->argc(0));
	return CR_SUCCESS;
}
