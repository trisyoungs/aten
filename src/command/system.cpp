/*
	*** System Commands
	*** src/command/system.cpp
	Copyright T. Youngs 2007-2015

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
#include "main/version.h"
#include "gui/gui.h"
#include "base/sysfunc.h"

// Toggle debug modes
bool Command::function_Debug(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Messenger::OutputType ot = Messenger::outputType(c->argc(0), TRUE);
	if (ot != Messenger::nOutputTypes)
	{
		// Check to see if level is already active
		msg.isOutputActive(ot) ? msg.removeOutputType(ot) : msg.addOutputType(ot);
	}
	else return FALSE;
	return TRUE;
}

// Retrieve environment variable
bool Command::function_Getenv(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (getenv(c->argc(0)) != '\0') rv.set(getenv(c->argc(0)));
	else rv.set("");
	return TRUE;
}

// Retrieve environment variable as a floating point value
bool Command::function_Getenvf(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (getenv(c->argc(0)) != '\0') rv.set( atof(getenv(c->argc(0))) );
	else rv.set(0.0);
	return TRUE;
}

// Retrieve environment variable as an integer value
bool Command::function_Getenvi(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (getenv(c->argc(0)) != '\0') rv.set( atoi(getenv(c->argc(0))) );
	else rv.set(0);
	return TRUE;
}

// Start GUI
bool Command::function_Gui(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// If we're in interactive mode, just set program mode and let main.cpp handle it.
	if (aten.programMode() == Aten::InteractiveMode) aten.setProgramMode(Aten::GuiMode);
	else if (!gui.exists())
	{
		// Set program mode and start gui
		aten.setProgramMode(Aten::GuiMode);
		gui.run();
	}
	return TRUE;
}

// Help function
bool Command::function_Help(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
// 	Command::Function cf = commands.command(c->argc(0));
// 	if (cf == Command::nCommands) msg.print("help: Unrecognised command '%s'.\n", c->argc(0));
	int cf = c->argi(0);
	if ((cf < 0) || (cf >= Command::nCommands)) msg.print("Unrecognised command passed to 'help'.\n");
	else if (commands.data[cf].hasArguments()) msg.print("%s(%s)\n       %s\n", commands.data[cf].keyword, commands.data[cf].argText, commands.data[cf].syntax);
	else msg.print("%s\n       %s\n", commands.data[cf].keyword, commands.data[cf].syntax);
	return TRUE;
}

// Nullify specified pointers
bool Command::function_Null(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	bool result = TRUE;
	ReturnValue temprv;
	for (int n=0; n<c->nArgs(); ++n)
	{
		temprv.set(c->argType(n), NULL);
		c->setArg(n,temprv);
	}
	return result;
}

// Quit main program
bool Command::function_Quit(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Set program mode here
	aten.setProgramMode(Aten::NoMode);
	// If the GUI is active, close it...
	if (gui.exists()) gui.saveBeforeClose();
	c->parent()->setAcceptedFail(Command::Quit);
	return FALSE;
}

// Search available commands
bool Command::function_SearchCommands(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Dnchar lcase = lowerCase(c->argc(0));
	for (int cf = 0; cf < Command::nCommands; ++cf) if (strstr(commands.data[cf].keyword, lcase.get()) != NULL)
		msg.print("  %-15s : %s\n", commands.data[cf].keyword, commands.data[cf].syntax);
	return TRUE;
}

// Set random seed
bool Command::function_Seed(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	srand( (unsigned) c->argi(0) );
	return TRUE;
}

// Print version information
bool Command::function_Version(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	printf("Aten version %s, built from %s@%s.\n", ATENVERSION, ATENURL, ATENREVISION);
	return TRUE;
}

