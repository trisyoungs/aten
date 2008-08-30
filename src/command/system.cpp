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
#include "aten/aten.h"
#include "gui/gui.h"

// Toggle debug modes
int CommandData::function_CA_DEBUG(Command *&c, Bundle &obj)
{
	Messenger::OutputType ot = Messenger::outputType(c->argc(0));
	if (ot != Messenger::nOutputTypes)
	{
		// Check to see if level is already active
		msg.isOutputActive(ot) ? msg.removeOutputType(ot) : msg.addOutputType(ot);
	}
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Start GUI
int CommandData::function_CA_GUI(Command *&c, Bundle &obj)
{
	// If we're in interactive mode, just set program mode and let main.cpp handle it.
	if (aten.programMode() == Aten::InteractiveMode) aten.setProgramMode(Aten::GuiMode);
	else if (!gui.exists())
	{
		// Set program mode and start gui
		aten.setProgramMode(Aten::GuiMode);
		// Add empty model if none were specified on the command line
		if (aten.nModels() == 0) Model *m = aten.addModel();
		gui.run();
	}
	return CR_SUCCESS;
}

// Help function
int CommandData::function_CA_HELP(Command *&c, Bundle &obj)
{
	CommandAction ca = CA_from_text(c->argc(0));
	if (ca == CA_NITEMS) msg.print("help: Unrecognised command '%s'.\n",c->argc(0));
	else if (CA_data[ca].hasArguments()) msg.print("help:  %s  --  %s\n", CA_data[ca].keyword, CA_data[ca].syntax);
	else msg.print("help:  %s %s  --  %s\n", CA_data[ca].keyword, CA_data[ca].argText, CA_data[ca].syntax);
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
	aten.setProgramMode(Aten::NoMode);
	// If the GUI is active, close it...
	if (gui.exists()) gui.saveBeforeClose();
	return CR_EXIT;
}
