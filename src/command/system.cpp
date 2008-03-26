/*
	*** System control functions
	*** src/command/system.cpp
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
#include "base/debug.h"
#include "base/master.h"
#include "gui/gui.h"

// Start GUI
int CommandData::function_CA_GUI(Command *&c, Bundle &obj)
{
	// If we're in interactive mode, just set program mode and let main.cpp handle it.
	if (master.programMode() == PM_INTERACTIVE) master.setProgramMode(PM_GUI);
	else if (!gui.exists())
	{
		// Set program mode and start gui
		master.setProgramMode(PM_GUI);
		// Add empty model if none were specified on the command line
		if (master.nModels() == 0) Model *m = master.addModel();
		gui.run(0, NULL);
	}
	return CR_SUCCESS;
}

// Help function
int CommandData::function_CA_HELP(Command *&c, Bundle &obj)
{
	CommandAction ca = CA_from_text(c->argc(0));
	if (ca == CA_NITEMS) msg(Debug::None,"help: Unrecognised command '%s'.\n",c->argc(0));
	else if (CA_data[ca].hasArguments()) msg(Debug::None,"help:  %s  --  %s\n", CA_data[ca].keyword, CA_data[ca].syntax);
	else msg(Debug::None,"help:  %s %s  --  %s\n", CA_data[ca].keyword, CA_data[ca].argText, CA_data[ca].syntax);
	return CR_SUCCESS;
}

// Set random seed
int CommandData::function_CA_SEED(Command *&c, Bundle &obj)
{
	srand( (unsigned) c->argi(0) );
	return CR_SUCCESS;
}

// Quit main program
int CommandData::function_CA_QUIT(Command *&c, Bundle &obj)
{
	// Set program mode here, in case we are running in PM_COMMAND
	master.setProgramMode(PM_NONE);
	// If the GUI is active, close it...
	if (gui.exists()) gui.saveBeforeClose();
	return CR_EXIT;
}
