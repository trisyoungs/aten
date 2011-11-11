/*
	*** Messaging and GUI Commands
	*** src/command/messaging.cpp
	Copyright T. Youngs 2007-2011

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
#include "gui/gui.h"

// Create and return TreeGui dialog
bool Command::function_CreateDialog(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set(VTypes::DialogData, c->parent()->createDialog(c->hasArg(0) ? c->argc(0) : "New Dialog"));
	return TRUE;
}

// Return this Tree's default TreeGui dialog
bool Command::function_DefaultDialog(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) c->parent()->defaultDialog().setInitialProperties(c->argc(0));
	rv.set(VTypes::DialogData, &c->parent()->defaultDialog());
	return TRUE;
}

// Write line to msg output and stop
bool Command::function_Error(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'error' command.\n");
		return FALSE;
	}
	if (fmt->writeToString())
	{
		msg.print("%s\n",fmt->string());
		if (gui.exists()) QMessageBox::critical(NULL, "Aten", fmt->string(), QMessageBox::Ok, QMessageBox::Ok);
	}
	c->parent()->setAcceptedFail(Command::Error);
	return FALSE;
}

// Display message dialog and continue
bool Command::function_Message(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *fmt = c->createFormat(1,2);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'message' command.\n");
		return FALSE;
	}
	if (fmt->writeToString())
	{
		msg.print("[%s] %s\n",c->argc(0), fmt->string());
		if (gui.exists()) QMessageBox::information(NULL, c->argc(0), fmt->string(), QMessageBox::Ok, QMessageBox::Ok);
	}
	else return FALSE;
	return TRUE;
}

// Print formatted string
bool Command::function_Printf(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'printf' command.\n");
		return FALSE;
	}
	if (fmt->writeToString()) msg.print(Messenger::Always, "%s", fmt->string());
	else return FALSE;
	return TRUE;
}

// Show this Tree's default TreeGui dialog
bool Command::function_ShowDefaultDialog(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) c->parent()->defaultDialog().setInitialProperties(c->argc(0));
	if (gui.exists() || prefs.allowDialogs()) rv.set(c->parent()->defaultDialog().execute());
	else rv.set(TRUE);
	return TRUE;
}

// Print formatted string (in verbose output only)
bool Command::function_Verbose(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Format *fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'verbose' command.\n");
		return FALSE;
	}
	if (!msg.isOutputActive(Messenger::Verbose)) return TRUE;
	if (fmt->writeToString()) msg.print(Messenger::Verbose, "%s\n",fmt->string());
	else return FALSE;
	return TRUE;
}

