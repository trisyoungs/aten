/*
	*** Flow Commands
	*** src/command/flow.cpp
	Copyright T. Youngs 2007-2017

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

ATEN_USING_NAMESPACE

// Dummy Node
bool Commands::function_NoFunction(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	return true;
}

// Joiner
bool Commands::function_Joiner(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Execute both commands
	bool result = true;
	if (c->hasArg(0)) result = c->arg(0, rv);
	if (result && c->hasArg(1)) result = c->arg(1, rv);
	return result;
}

// Declarations
bool Commands::function_Declarations(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Reset each variable argument
	for (int n=0; n<c->nArgs(); ++n) if (!c->argNode(n)->initialise()) return false;
	return true;
}

// Break out of current 'for' loop or 'switch' structure
bool Commands::function_Break(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	c->parent()->setAcceptedFail(Commands::Break);
	return false;
}

// Case statement within 'switch' structure
bool Commands::function_Case(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (!c->arg(0, rv)) return false;
	return true;
}

// Continue for loop at next iteration
bool Commands::function_Continue(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	c->parent()->setAcceptedFail(Commands::Continue);
	return false;
}

// Default case statement within 'switch' structure
bool Commands::function_Default(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	return true;
}

// Do-While loop
bool Commands::function_DoWhile(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Argument 0 - Blockment
	// Argument 1 - Test condition
	ReturnValue test;
	bool result;
	Commands::Function af;
	do
	{
		// Run blockment- catch break and continue calls which return false
		result = c->arg(0, rv);
		if (!result)
		{
			// Check acceptedfail flag - if Break or Continue, reset flag and quit/continue
			af = c->parent()->acceptedFail();
			if (af == Commands::Break)
			{
				c->parent()->setAcceptedFail(Commands::NoFunction);
				return true;
			}
			else if (af == Commands::Continue) c->parent()->setAcceptedFail(Commands::NoFunction);
			else if (af != Commands::NoFunction) return false;
		}
		// Perform test of condition
		if (!c->arg(1, test)) return false;
	} while (test.asBool());
	return true;
}

// For loop
bool Commands::function_For(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Argument 0 - Initial value expression
	// Argument 1 - Loop condition
	// Argument 2 - Action on loop cycle
	// Argument 3 - Statementlist
	 // Get initial variable value
	if (!c->arg(0, rv)) return false;
	ReturnValue ifval;
	bool result;
	Commands::Function af;
	while (true)
	{
		// Termination condition
		if (!c->arg(1, ifval)) return false;
		if (!ifval.asBool()) break;
		// Loop body - catch break and continue calls which return false
		result = c->arg(3, rv);
		if (!result)
		{
			// Check acceptedfail flag - if Break or Continue, reset flag and quit/continue
			af = c->parent()->acceptedFail();
			if (af == Commands::Break)
			{
				c->parent()->setAcceptedFail(Commands::NoFunction);
				return true;
			}
			else if (af == Commands::Continue) c->parent()->setAcceptedFail(Commands::NoFunction);
			else return false;
		}
		// Loop 'increment' statement
		if (!c->arg(2, rv)) return false;
	}
	return true;
}

// For x in y loop
bool Commands::function_ForIn(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Argument 0 - Initial value expression
	// Argument 1 - Loop condition
	// Argument 2 - Statementlist
	// Set initial value
	ReturnValue varval;
	if (!c->arg(1, varval)) return false;
	c->setArg(0, varval);
	Commands::Function af;
	bool result;
	while (c->argp(0,c->argType(0)) != NULL)
	{
		// Loop body - catch break and continue calls which return false
		result = c->arg(2, rv);
		if (!result)
		{
			// Check acceptedfail flag - if Break or Continue, reset flag and quit/continue
			af = c->parent()->acceptedFail();
			if (af == Commands::Break)
			{
				c->parent()->setAcceptedFail(Commands::NoFunction);
				return true;
			}
			else if (af == Commands::Continue) c->parent()->setAcceptedFail(Commands::NoFunction);
			else return false;
		}
		// Skip to next linked item...
		if (!varval.increase()) return false;
		c->setArg(0, varval);
	}
	return true;
}

// If test
bool Commands::function_If(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	ReturnValue ifval;
	if (!c->arg(0, ifval)) return false;
	if (ifval.asBool()) return (c->arg(1, rv));
	else if (c->hasArg(2)) return (c->arg(2, rv));
	return true;
}

// Return from function/filter/program
bool Commands::function_Return(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	c->parent()->setAcceptedFail(Commands::Return);
	if (c->hasArg(0)) c->arg(0, rv);
	return false;
}

// Switch statement
bool Commands::function_Switch(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	ReturnValue switchVal, caseVal;
	if (!c->arg(0, switchVal)) return false;
	int index = 1;
	CommandNode* node;
	bool result, execute = false;
	Commands::Function af;
	while (c->hasArg(index))
	{
		// Do nothing if its not a CommandNode (but it always should be...)
		if (c->argNode(index)->nodeType() == TreeNode::CmdNode)
		{
			node = (CommandNode*) c->argNode(index);
			// Are we executing or checking case values?
			if (execute)
			{
				if ((node->function() != Commands::Case) && (node->function() != Commands::Default))
				{
					result = c->arg(index, rv);
					if (!result)
					{
						// Did we break out?
						af = c->parent()->acceptedFail();
						if (af == Commands::Break)
						{
// 							printf("Broken.\n");
							c->parent()->setAcceptedFail(Commands::NoFunction);
							return true;
						}
						else return false;
					}
				}
			}
			else
			{
				// Is this a 'case' or a 'default' node
				if (node->function() == Commands::Default)
				{
					++index;
					if (c->hasArg(index))
					{
						if (!c->arg(index, rv)) return false;
						break;
					}
				}
				else if ((node->function() == Commands::Case) && (!execute))
				{
					if (!c->arg(index, caseVal)) return false;
// 					printf("Index %i is a case node whose value is %s..\n", index, caseval.info());
					// Do comparison...
					if ((switchVal.type() == VTypes::IntegerData) && (caseVal.type() == VTypes::IntegerData)) result = (switchVal.asInteger() == caseVal.asInteger());
					else result = switchVal.asString() == caseVal.asString();
					// Flag to enter into execution state if values matched
					if (result) execute = true;
				}
			}
		}
		++index;
	}
	return true;
}

// While loop
bool Commands::function_While(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Argument 0 - Test condition
	// Argument 1 - Blockment
	ReturnValue test;
	bool result;
	Commands::Function af;
	// Perform initial test of condition
	if (!c->arg(0, test)) return false;
	while (test.asBool())
	{
		// Run blockment- catch break and continue calls which return false
		result = c->arg(1, rv);
		if (!result)
		{
			// Check acceptedfail flag - if Break or Continue, reset flag and quit/continue
			af = c->parent()->acceptedFail();
			if (af == Commands::Break)
			{
				c->parent()->setAcceptedFail(Commands::NoFunction);
				return true;
			}
			else if (af == Commands::Continue) c->parent()->setAcceptedFail(Commands::NoFunction);
			else if (af != Commands::NoFunction) return false;
		}
		// Perform test of condition
		if (!c->arg(0, test)) return false;
	}
	return true;
}

