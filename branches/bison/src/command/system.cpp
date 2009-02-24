/*
	*** System control functions
	*** src/command/system.cpp
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

#include "command/commandlist.h"
#include "main/aten.h"
#include "gui/gui.h"

// Toggle debug modes
int Command::function_CA_DEBUG(CommandNode *&c, Bundle &obj)
{
	Messenger::OutputType ot = Messenger::outputType(c->argc(0));
	if (ot != Messenger::nOutputTypes)
	{
		// Check to see if level is already active
		msg.isOutputActive(ot) ? msg.removeOutputType(ot) : msg.addOutputType(ot);
	}
	else return Command::Fail;
	return Command::Success;
}

// Start GUI
int Command::function_CA_GUI(CommandNode *&c, Bundle &obj)
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
	return Command::Success;
}

// Help function
int Command::function_CA_HELP(CommandNode *&c, Bundle &obj)
{
	Command::Function cf = commands.command(c->argc(0));
	if (cf == CA_NITEMS) msg.print("help: Unrecognised command '%s'.\n", c->argc(0));
	else if (commands.data[cf].hasArguments()) msg.print("help:  %s %s\n       %s\n", commands.data[cf].keyword, commands.data[cf].argText, commands.data[cf].syntax);
	else msg.print("help:  %s\n       %s\n", commands.data[cf].keyword, commands.data[cf].syntax);
	return Command::Success;
}

// Set random seed
int Command::function_CA_SEED(CommandNode *&c, Bundle &obj)
{
	srand( (unsigned) c->argi(0) );
	return Command::Success;
}

// Quit main program
int Command::function_CA_QUIT(CommandNode *&c, Bundle &obj)
{
	// Set program mode here, in case we are running in PM_COMMAND
	aten.setProgramMode(Aten::NoMode);
	// If the GUI is active, close it...
	if (gui.exists()) gui.saveBeforeClose();
	return Command::Exit;
}

// Print version information
int Command::function_CA_VERSION(CommandNode *&c, Bundle &obj)
{
	printf("Aten version %s, built from %s@%s.\n", ATENVERSION, ATENURL, ATENREVISION);
	return Command::Exit;
}
