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
int commanddata::function_CA_LISTSCRIPTS(command *&c, bundle &obj)
{
	if (master.scripts.size() == 0) msg(DM_NONE,"No scripts loaded.\n");
	else msg(DM_NONE,"Currently loaded scripts:\n");
	for (commandlist *cl = master.scripts.first(); cl != NULL; cl = cl->next)
		msg(DM_NONE,"  %s (%s)\n", cl->get_scriptfilename(), cl->get_name());
	return CR_SUCCESS;
}

// Load script from disk
int commanddata::function_CA_LOADSCRIPT(command *&c, bundle &obj)
{
	commandlist *cl = master.scripts.add();
	if (!cl->load(c->argc(0)))
	{
		master.scripts.remove(cl);
		return CR_FAIL;
	}
	if (c->has_arg(1)) cl->set_name(c->argc(1));
	else cl->set_name(c->argc(0));
	return CR_SUCCESS;
}

// Run specified script
int commanddata::function_CA_RUNSCRIPT(command *&c, bundle &obj)
{
	// Find the script...
	commandlist *cl;
	for (cl = master.scripts.first(); cl != NULL; cl = cl->next)
		if (strcmp(c->argc(0), cl->get_name()) == 0) break;
	if (cl != NULL)
	{
		msg(DM_NONE,"Executing script '%s':\n",c->argc(0));
		cl->execute();
	}
	else msg(DM_NONE,"Couldn't find script '%s'.\n",c->argc(0));
	return CR_SUCCESS;
}
