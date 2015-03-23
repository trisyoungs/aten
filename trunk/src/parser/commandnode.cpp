/*
	*** Command Node
	*** src/parser/commandnode.cpp
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

#include "parser/commandnode.h"
#include "parser/tree.h"
#include "main/aten.h"
#include <string.h>

ATEN_USING_NAMESPACE

// Static members
Aten* CommandNode::aten_ = NULL;

// Constructor
CommandNode::CommandNode(Commands::Function func) : TreeNode()
{
	// Private variables
	function_ = func;
	nodeType_ = TreeNode::CmdNode;
	format_ = NULL;
}

CommandNode::CommandNode(TreeNode* source)
{
	format_ = NULL;
	copy(source);	
}

// Destructor
CommandNode::~CommandNode()
{
	if (format_ != NULL) delete format_;
}

/*
 * Link to Aten
 */

// Return reference to Aten
Aten* CommandNode::aten()
{
	return aten_;
}

// Set pointer to Aten
void CommandNode::setAten(Aten* aten)
{
	aten_ = aten;
}

/*
 * Command Data
 */

// Prepare function (if possible)
bool CommandNode::prepFunction()
{
	bool result = TRUE;
	switch (function_)
	{
		// For functions that use formats, attempt to create the Format* if* the format string is a character constant
		case (Commands::Error):
		case (Commands::Printf):
		case (Commands::Verbose):
		case (Commands::ReadLineFormatted):
		case (Commands::WriteLineFormatted):
		case (Commands::ToA):
			if (!args_.first()->item->readOnly()) break;
			result = createFormat(0,1);
			break;
		case (Commands::WriteVariable):
		case (Commands::ReadVariable):
			result = createFormat(-1,1);
			break;
		case (Commands::WriteVariableFormatted):
		case (Commands::ReadVariableFormatted):
			if (!args_[1]->item->readOnly()) break;
			result = createFormat(1,2);
			break;
		case (Commands::DeSelectFormatted):
		case (Commands::SelectFormatted):
			if (!args_.first()->item->readOnly()) break;
			result = createFormat(0,1);
			break;
		// For the 'return' function, the return type must match the return type of the parent tree...
		case (Commands::Return):
			if (parent_->returnType() == VTypes::NoData)
			{
				if (!hasArg(0)) break;
				Messenger::print("Error: Return value provided when none is required.");
				result = FALSE;
			}
			else
			{
				if ((!hasArg(0)) && (parent_->returnType() != VTypes::NoData))
				{
					Messenger::print("Error: No return value provided.");
					result = FALSE;
				}
				else if (argType(0) != parent_->returnType())
				{
					Messenger::print("Error: Return value of type '%s' provided for function that returns %s.", VTypes::dataType(argType(0)), VTypes::aDataType(parent_->returnType()));
					result = FALSE;
				}
			}
			break;
		default:
			break;
	}
	return result;
}

// Get function
Commands::Function CommandNode::function()
{
	return function_;
}

// Create format node (if necessary) from supplied argument id
Format* CommandNode::createFormat(int fmtargid, int firstargid)
{
	Messenger::enter("CommandNode::createFormat");

	// fmtargid = id of argument which contains the formatting string, or -1 for no formatting string (free-form format)
	// firstargid = id of first data argument
	// If we do not currently have a format associated to the node, create it regardless
	bool result = FALSE;
	Refitem<TreeNode,int>* firstarg = firstargid >= args_.nItems() ? NULL : args_[firstargid];
	if (format_ == NULL)
	{
		result = TRUE;
		format_ = fmtargid == -1 ? new Format(firstarg) : new Format(argc(fmtargid), firstarg);
		if (!format_->isValid())
		{
			result = FALSE;
			delete format_;
			format_ = NULL;
		}
	}
	else
	{
		// So a format already exists. If the source argument is a constant (or there is no source argument) don't recreate it
		if ((fmtargid == -1) || (argNode(fmtargid)->readOnly())) result = TRUE;
		else
		{
			// Delete old format
			delete format_;
			// Create new format
			format_ = fmtargid == -1 ? new Format(firstarg) : new Format(argc(fmtargid), firstarg);
			if (!format_->isValid())
			{
				result = FALSE;
				delete format_;
				format_ = NULL;
			}
		}
	}
	Messenger::exit("CommandNode::createFormat");
	return (result == FALSE ? NULL : format_);
}

// Execute command
bool CommandNode::execute(ReturnValue& rv)
{
	// Execute the command
	return aten_->callCommand(function_, this, rv);
}

// Print node contents
void CommandNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
// 	printf("Function id = %p\n", function_);
	printf("[CN]%s%s (Command) (%i arguments)\n", tab.get(), aten_->commandKeyword(function_), args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
}

// Set from returnvalue node
bool CommandNode::set(ReturnValue& rv)
{
	printf("Internal Error: Trying to 'set' a CommandNode.\n");
	return FALSE;
}

// Initialise node
bool CommandNode::initialise()
{
	printf("Internal Error: A CommandNode cannot be initialised.\n");
	return FALSE;
}

// Create, run, and free a single command with simple arguments
bool CommandNode::run(Commands::Function func, const char* argList, ...)
{
	Messenger::enter("CommandNode::run");
	// Local tree to contain commandnode and its arguments
	Tree tree;

	// Create our temporary node
	CommandNode node(func);
	node.parent_ = &tree;

	// Set arguments from supplied list
	const char* c;
	va_list vars;
	va_start(vars, argList);
	TreeNode* var = NULL;
	for (c = &argList[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = tree.addConstant(va_arg(vars, int));
				break;
			case ('d'):
				var = tree.addConstant(va_arg(vars, double));
				break;
			case ('c'):
			case ('s'):
				var = tree.addConstant(va_arg(vars, const char* ));
				break;
			default:
				printf("Invalid argument specifier '%c' in CommandNode::run.\n", *c);
				var = NULL;
				break;
		}
		node.addArgument(var);
	}
	va_end(vars);
	// Now, run the command...
	ReturnValue rv;
	bool result = node.execute(rv);
	Messenger::exit("CommandNode::run");
	return result;
}

// Create, run, and free a single command with simple arguments and specified bundle
bool CommandNode::run(Commands::Function func, Bundle& bundle, const char* argList, ...)
{
	Messenger::enter("CommandNode::run[bundle]");
	// Local tree to contain commandnode and its arguments
	Tree tree;

	// Create our temporary node
	CommandNode node(func);
	node.parent_ = &tree;

	// Set arguments from supplied list
	const char* c;
	va_list vars;
	va_start(vars, argList);
	TreeNode* var = NULL;
	for (c = &argList[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = tree.addConstant(va_arg(vars, int));
				break;
			case ('d'):
				var = tree.addConstant(va_arg(vars, double));
				break;
			case ('c'):
			case ('s'):
				var = tree.addConstant(va_arg(vars, const char* ));
				break;
			default:
				printf("Invalid argument specifier '%c' in CommandNode::run.\n", *c);
				var = NULL;
				break;
		}
		node.addArgument(var);
	}
	va_end(vars);
	// Now, run the command...
	ReturnValue rv;
	bool result = node.execute(bundle, rv);
	Messenger::exit("CommandNode::run[bundle]");
	return result;
}

// Execute command with specified bundle
bool CommandNode::execute(Bundle& bundle, ReturnValue& rv)
{
	// Execute the command
	return aten_->callCommand(function_, this, rv, bundle);
}
