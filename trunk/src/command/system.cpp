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
#include "gui/mainwindow.h"
#include "main/version.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Toggle debug modes
bool Commands::function_Debug(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Messenger::OutputType ot = Messenger::outputType(c->argc(0), TRUE);
	if (ot != Messenger::nOutputTypes)
	{
		// Check to see if level is already active
		Messenger::isOutputActive(ot) ? Messenger::removeOutputType(ot) : Messenger::addOutputType(ot);
	}
	else return FALSE;
	return TRUE;
}

// Retrieve environment variable
bool Commands::function_Getenv(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (getenv(qPrintable(c->argc(0))) != '\0') rv.set(getenv(qPrintable(c->argc(0))));
	else rv.set("");
	return TRUE;
}

// Retrieve environment variable as a floating point value
bool Commands::function_Getenvf(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (getenv(qPrintable(c->argc(0))) != '\0')
	{
		QString s = getenv(qPrintable(c->argc(0)));
		rv.set(s.toDouble());
	}
	else rv.set(0.0);
	return TRUE;
}

// Retrieve environment variable as an integer value
bool Commands::function_Getenvi(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (getenv(qPrintable(c->argc(0))) != '\0')
	{
		QString s = getenv(qPrintable(c->argc(0)));
		rv.set(s.toInt());
	}
	else rv.set(0);
	return TRUE;
}

// Help function
bool Commands::function_Help(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
// 	Commands::Function cf = commands.command(c->argc(0));
// 	if (cf == Commands::nCommands) Messenger::print("help: Unrecognised command '%s'.", c->argc(0));
	int i = c->argi(0);
	if ((i < 0) || (i >= Commands::nCommands))
	{
		Messenger::print("Unrecognised command passed to 'help'.");
		return false;
	}

	Commands::Function cf = (Commands::Function) i;
	if (Commands::data(cf).hasArguments()) Messenger::print("%s(%s)\n       %s", Commands::data(cf).keyword, Commands::data(cf).argText, Commands::data(cf).syntax);
	else Messenger::print("%s\n       %s", Commands::data(cf).keyword, Commands::data(cf).syntax);
	return TRUE;
}

// Nullify specified pointers
bool Commands::function_Null(CommandNode* c, Bundle& obj, ReturnValue& rv)
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
bool Commands::function_Quit(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	aten_.atenWindow()->saveBeforeClose();

	c->parent()->setAcceptedFail(Commands::Quit);

	return FALSE;
}

// Search available commands
bool Commands::function_SearchCommands(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	QString lcase = c->argc(0).toLower();
	for (int i = 0; i < Commands::nCommands; ++i)
	{
		Commands::Function cf = (Commands::Function) i;
		if (QString(Commands::data(cf).keyword).contains(lcase)) Messenger::print("  %-15s : %s", Commands::data(cf).keyword, Commands::data(cf).syntax);
	}
	return TRUE;
}

// Set random seed
bool Commands::function_Seed(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	srand( (unsigned) c->argi(0) );
	return TRUE;
}

// Print version information
bool Commands::function_Version(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	printf("Aten version %s, built from %s@%s.\n", ATENVERSION, ATENURL, ATENREVISION);
	return TRUE;
}

