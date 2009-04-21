/*
	*** Flow Commands
	*** src/nucommand/flow..cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "parser/tree.h"
#include "base/mathfunc.h"
#include <stdio.h>
#include <string.h>

// Dummy Node
bool Command::function_NoFunction(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	return TRUE;
}

// Joiner
bool Command::function_Joiner(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Execute both commands
	bool result = TRUE;
	if (c->hasArg(0)) result = c->arg(0, rv);
	if (result && c->hasArg(1)) result = c->arg(1, rv);
	return result;
}

// Declarations
bool Command::function_Declarations(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Reset each variable argument
	for (int n=0; n<c->nArgs(); ++n) if (!c->argNode(n)->initialise()) return FALSE;
	return TRUE;
}

// Break out of current for loop
bool Command::function_Break(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	c->parent()->setAcceptedFail(Command::Break);
	return FALSE;
}

// Continue for loop at next iteration
bool Command::function_Continue(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	c->parent()->setAcceptedFail(Command::Continue);
	return FALSE;
}

// Do-While loop
bool Command::function_DoWhile(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Argument 0 - Blockment
	// Argument 1 - Test condition
	ReturnValue test;
	bool result;
	Command::Function af;
	do
	{
		// Run blockment- catch break and continue calls which return FALSE
		result = c->arg(0, rv);
		if (!result)
		{
			af = c->parent()->acceptedFail();
			c->parent()->setAcceptedFail(Command::NoFunction);
			if (af == Command::Break) break;
			else if (af != Command::Continue) return FALSE;
		}
		// Perform test of condition
		if (!c->arg(1, test)) return FALSE;
	} while (test.asBool());
	return TRUE;
}

// For loop
bool Command::function_For(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Argument 0 - Initial value expression
	// Argument 1 - Loop condition
	// Argument 2 - Action on loop cycle
	// Argument 3 - Statementlist
	 // Get initial variable value
	if (!c->arg(0, rv)) return FALSE;
	ReturnValue ifval;
	bool result;
	Command::Function af;
	while (TRUE)
	{
		// Termination condition
		if (!c->arg(1, ifval)) return FALSE;
		if (!ifval.asBool()) break;
		// Loop body - catch break and continue calls which return FALSE
		result = c->arg(3, rv);
		if (!result)
		{
			af = c->parent()->acceptedFail();
			c->parent()->setAcceptedFail(Command::NoFunction);
			if (af == Command::Break) break;
			else if (af != Command::Continue) return FALSE;
		}
		// Loop 'increment' statement
		if (!c->arg(2, rv)) return FALSE;
	}
	return TRUE;
}

// If test
bool Command::function_If(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	ReturnValue ifval;
	if (!c->arg(0, ifval)) return FALSE;
	if (ifval.asBool()) return (c->arg(1, rv));
	else if (c->hasArg(2)) return (c->arg(2, rv));
	return TRUE;
}

// Return from function/filter/program
bool Command::function_Return(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	c->parent()->setAcceptedFail(Command::Return);
	if (c->hasArg(0)) c->arg(0, rv);
	return FALSE;
}

// While loop
bool Command::function_While(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Argument 0 - Test condition
	// Argument 1 - Blockment
	ReturnValue test;
	bool result;
	Command::Function af;
	// Perform initial test of condition
	if (!c->arg(0, test)) return FALSE;
	while (test.asBool())
	{
		// Run blockment- catch break and continue calls which return FALSE
		result = c->arg(1, rv);
		if (!result)
		{
			af = c->parent()->acceptedFail();
			c->parent()->setAcceptedFail(Command::NoFunction);
			if (af == Command::Break) break;
			else if (af != Command::Continue) return FALSE;
		}
		// Perform test of condition
		if (!c->arg(0, test)) return FALSE;
	}
	return TRUE;
}
