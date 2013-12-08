/*
	*** Script Commands
	*** src/command/script.cpp
	Copyright T. Youngs 2007-2013

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
#include "main/aten.h"
#include "gui/gui.h"
#include "gui/command.h"

// List available scripts
bool Command::function_ListScripts(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (aten.nScripts() == 0) msg.print("No scripts loaded.\n");
	else msg.print("Currently loaded scripts:\n");
	for (Program *prog = aten.scripts(); prog != NULL; prog = prog->next) msg.print("  %s (%s)\n", prog->filename(), prog->name());
	rv.reset();
	return TRUE;
}

// Load script from disk
bool Command::function_LoadScript(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Program *prog = aten.addScript();
	if (!prog->generateFromFile(c->argc(0), "ScriptFile"))
	{
		aten.removeScript(prog);
		return FALSE;
	}
	if (c->hasArg(1)) prog->setName(c->argc(1));
	else prog->setName(c->argc(0));
	rv.reset();
	// Update GUI
	if (gui.exists()) gui.commandWidget->refreshScripts();
	return TRUE;
}

// Run specified script
bool Command::function_RunScript(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Find the script...
	Program *prog;
	for (prog = aten.scripts(); prog != NULL; prog = prog->next) if (strcmp(c->argc(0), prog->name()) == 0) break;
	if (prog != NULL)
	{
		msg.print("Executing script '%s':\n",c->argc(0));
		ReturnValue result;
		prog->execute(result);
	}
	else msg.print("Couldn't find script '%s'.\n",c->argc(0));
	rv.reset();
	return TRUE;
}
