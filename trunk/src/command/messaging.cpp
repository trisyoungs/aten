/*
	*** Messaging and GUI Commands
	*** src/command/messaging.cpp
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
#include "parser/tree.h"
#include "base/prefs.h"
#include <QtWidgets/QMessageBox>

ATEN_USING_NAMESPACE

// Create and return TreeGui dialog
bool Commands::function_CreateDialog(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set(VTypes::DialogData, c->parent()->createDialog(c->hasArg(0) ? c->argc(0) : "New Dialog"));
	return true;
}

// Return this Tree's default TreeGui dialog
bool Commands::function_DefaultDialog(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (c->hasArg(0)) c->parent()->defaultDialog().setInitialProperties(c->argc(0));
	rv.set(VTypes::DialogData, &c->parent()->defaultDialog());
	return true;
}

// Write line to msg output and stop
bool Commands::function_Error(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'error' command.\n");
		return false;
	}
	if (fmt->writeToString())
	{
		Messenger::print(fmt->string());
		QMessageBox::critical(NULL, "Aten", fmt->string(), QMessageBox::Ok, QMessageBox::Ok);
	}
	c->parent()->setAcceptedFail(Commands::Error);
	return false;
}

// Display message dialog and continue
bool Commands::function_Message(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* fmt = c->createFormat(1,2);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'message' command.\n");
		return false;
	}
	if (fmt->writeToString())
	{
		Messenger::print("[%s] %s", qPrintable(c->argc(0)), qPrintable(fmt->string()));
		QMessageBox::information(NULL, c->argc(0), fmt->string(), QMessageBox::Ok, QMessageBox::Ok);
	}
	else return false;
	return true;
}

// Print formatted string
bool Commands::function_Printf(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'printf' command.\n");
		return false;
	}
	if (fmt->writeToString()) Messenger::print(fmt->string());
	else return false;
	return true;
}

// Show this Tree's default TreeGui dialog
bool Commands::function_ShowDefaultDialog(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (c->hasArg(0)) c->parent()->defaultDialog().setInitialProperties(c->argc(0));
	if (prefs.allowDialogs()) rv.set(c->parent()->defaultDialog().execute());
	else rv.set(true);
	return true;
}

// Print formatted string (in verbose output only)
bool Commands::function_Verbose(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'verbose' command.\n");
		return false;
	}
	if (!Messenger::isOutputActive(Messenger::Verbose)) return true;
	if (fmt->writeToString()) Messenger::print(Messenger::Verbose, "%s", qPrintable(fmt->string()));
	else return false;
	return true;
}

